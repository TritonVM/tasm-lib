use triton_vm::prelude::*;
use triton_vm::proof_item::ProofItemVariant;
use triton_vm::table::extension_table::Quotientable;
use triton_vm::table::master_table::MasterExtTable;
use triton_vm::table::NUM_BASE_COLUMNS;
use triton_vm::table::NUM_EXT_COLUMNS;
use triton_vm::table::NUM_QUOTIENT_SEGMENTS;
use triton_vm::twenty_first::math::x_field_element::EXTENSION_DEGREE;

use crate::arithmetic::bfe::primitive_root_of_unity::PrimitiveRootOfUnity;
use crate::array::horner_evaluation::HornerEvaluation;
use crate::array::inner_product_of_three_rows_with_weights::BaseElementType;
use crate::array::inner_product_of_three_rows_with_weights::InnerProductOfThreeRowsWithWeights;
use crate::array::inner_product_of_xfes::InnerProductOfXfes;
use crate::data_type::DataType;
use crate::field;
use crate::hashing::algebraic_hasher::sample_scalar_one::SampleScalarOne;
use crate::hashing::algebraic_hasher::sample_scalars_static_length_dyn_malloc::SampleScalarsStaticLengthDynMalloc;
use crate::library::Library;
use crate::traits::basic_snippet::BasicSnippet;
use crate::verifier::challenges;
use crate::verifier::challenges::shared::conventional_challenges_pointer;
use crate::verifier::claim::instantiate_fiat_shamir_with_claim::InstantiateFiatShamirWithClaim;
use crate::verifier::claim::shared::claim_type;
use crate::verifier::fri;
use crate::verifier::fri::verify::FriSnippet;
use crate::verifier::fri::verify::FriVerify;
use crate::verifier::master_ext_table::quotient_summands::QuotientSummands;
use crate::verifier::master_ext_table::verify_table_rows::ColumnType;
use crate::verifier::master_ext_table::verify_table_rows::VerifyTableRows;
use crate::verifier::out_of_domain_points::OodPoint;
use crate::verifier::out_of_domain_points::OutOfDomainPoints;
use crate::verifier::vm_proof_iter::dequeue_next_as::DequeueNextAs;
use crate::verifier::vm_proof_iter::shared::vm_proof_iter_type;

#[derive(Debug, Clone, Default)]
pub struct StarkVerify {
    stark: Stark,
}

impl BasicSnippet for StarkVerify {
    fn inputs(&self) -> Vec<(DataType, String)> {
        let claim_type = DataType::StructRef(claim_type());
        let proof_iter_type = DataType::StructRef(vm_proof_iter_type());
        vec![
            (claim_type, "claim".to_string()),
            (proof_iter_type, "vm_proof_iter".to_string()),
        ]
    }

    fn outputs(&self) -> Vec<(DataType, String)> {
        vec![]
    }

    fn entrypoint(&self) -> String {
        "tasmlib_verifier_stark_verify".to_string()
    }

    fn code(&self, library: &mut Library) -> Vec<LabelledInstruction> {
        let entrypoint = self.entrypoint();

        let ood_curr_row_base_and_ext_value_pointer_write =
            library.kmalloc(EXTENSION_DEGREE.try_into().unwrap());
        let ood_curr_row_base_and_ext_value_pointer_read =
            ood_curr_row_base_and_ext_value_pointer_write
                + BFieldElement::new(EXTENSION_DEGREE as u64 - 1);

        let ood_next_row_base_and_ext_value_pointer_write =
            library.kmalloc(EXTENSION_DEGREE.try_into().unwrap());
        let ood_next_row_base_and_ext_value_pointer_read =
            ood_next_row_base_and_ext_value_pointer_write
                + BFieldElement::new(EXTENSION_DEGREE as u64 - 1);

        let ood_curr_row_quotient_segment_value_pointer_write =
            library.kmalloc(EXTENSION_DEGREE.try_into().unwrap());
        let ood_curr_row_quotient_segment_value_pointer_read =
            ood_curr_row_quotient_segment_value_pointer_write
                + BFieldElement::new(EXTENSION_DEGREE as u64 - 1);

        let out_of_domain_curr_row_quot_segments_pointer_pointer = library.kmalloc(1);

        let instantiate_fiat_shamir_with_claim =
            library.import(Box::new(InstantiateFiatShamirWithClaim));
        let next_as_log_2_padded_height = library.import(Box::new(DequeueNextAs {
            proof_item: ProofItemVariant::Log2PaddedHeight,
        }));
        let next_as_merkleroot = library.import(Box::new(DequeueNextAs {
            proof_item: ProofItemVariant::MerkleRoot,
        }));
        let next_as_outofdomainbaserow = library.import(Box::new(DequeueNextAs {
            proof_item: ProofItemVariant::OutOfDomainBaseRow,
        }));
        let next_as_outofdomainextrow = library.import(Box::new(DequeueNextAs {
            proof_item: ProofItemVariant::OutOfDomainExtRow,
        }));
        let next_as_outofdomainquotientsegments = library.import(Box::new(DequeueNextAs {
            proof_item: ProofItemVariant::OutOfDomainQuotientSegments,
        }));
        let next_as_basetablerows = library.import(Box::new(DequeueNextAs {
            proof_item: ProofItemVariant::MasterBaseTableRows,
        }));
        let next_as_authentication_path = library.import(Box::new(DequeueNextAs {
            proof_item: ProofItemVariant::AuthenticationStructure,
        }));
        let next_as_exttablerows = library.import(Box::new(DequeueNextAs {
            proof_item: ProofItemVariant::MasterExtTableRows,
        }));
        let next_as_quotient_segment_elements = library.import(Box::new(DequeueNextAs {
            proof_item: ProofItemVariant::QuotientSegmentsElements,
        }));
        let derive_fri_parameters =
            library.import(Box::new(fri::derive_from_stark::DeriveFriFromStark {
                stark: self.stark,
            }));
        let num_collinearity_checks_field = field!(FriVerify::num_collinearity_checks);
        let domain_length_field = field!(FriVerify::domain_length);
        let domain_offset_field = field!(FriVerify::domain_offset);
        let domain_generator_field = field!(FriVerify::domain_generator);

        fn fri_snippet() -> FriSnippet {
            #[cfg(not(test))]
            {
                FriSnippet {}
            }
            #[cfg(test)]
            {
                FriSnippet {
                    test_instance: FriVerify::dummy(),
                }
            }
        }

        let fri_verify = library.import(Box::new(fri_snippet()));

        let get_challenges = library.import(Box::new(
            challenges::new_generic_dyn_claim::NewGenericDynClaim::conventional_with_tvm_parameters(
            ),
        ));
        let sample_quotient_codeword_weights =
            library.import(Box::new(SampleScalarsStaticLengthDynMalloc {
                num_elements: MasterExtTable::NUM_CONSTRAINTS,
            }));
        let domain_generator = library.import(Box::new(PrimitiveRootOfUnity));
        let sample_scalar_one = library.import(Box::new(SampleScalarOne));
        let calculate_out_of_domain_points = library.import(Box::new(OutOfDomainPoints));
        let quotient_summands =
            library.import(Box::new(QuotientSummands::with_conventional_memory_layout()));
        let inner_product_quotient_summands = library.import(Box::new(InnerProductOfXfes {
            length: MasterExtTable::NUM_CONSTRAINTS,
        }));
        let horner_evaluation_of_ood_curr_row_quot_segments =
            library.import(Box::new(HornerEvaluation {
                num_coefficients: NUM_QUOTIENT_SEGMENTS,
            }));
        const NUM_DEEP_CODEWORD_COMPONENTS: usize = 3;
        let sample_beqd_weights = library.import(Box::new(SampleScalarsStaticLengthDynMalloc {
            num_elements: NUM_BASE_COLUMNS
                + NUM_EXT_COLUMNS
                + NUM_QUOTIENT_SEGMENTS
                + NUM_DEEP_CODEWORD_COMPONENTS,
        }));
        let verify_base_table_rows = library.import(Box::new(VerifyTableRows {
            column_type: ColumnType::Base,
        }));
        let verify_extension_table_rows = library.import(Box::new(VerifyTableRows {
            column_type: ColumnType::Extension,
        }));
        let verify_quotient_segments = library.import(Box::new(VerifyTableRows {
            column_type: ColumnType::Quotient,
        }));
        let inner_product_three_rows_with_weights_bfe_base = library.import(Box::new(
            InnerProductOfThreeRowsWithWeights::triton_vm_parameters(BaseElementType::Bfe),
        ));
        let inner_product_three_rows_with_weights_xfe_base = library.import(Box::new(
            InnerProductOfThreeRowsWithWeights::triton_vm_parameters(BaseElementType::Xfe),
        ));
        let inner_product_4_xfes = library.import(Box::new(InnerProductOfXfes { length: 4 }));
        let quotient_segment_codeword_weights_from_be_weights = triton_asm!(
            // _ *beqd_ws

            push {(NUM_BASE_COLUMNS + NUM_EXT_COLUMNS) * EXTENSION_DEGREE}
            add
            // _ *quotient_segment_weights
        );
        let deep_codeword_weights_read_address = |n: usize| {
            assert!(n < NUM_DEEP_CODEWORD_COMPONENTS);
            triton_asm!(
                // _ *beqd_ws

                push {(NUM_BASE_COLUMNS + NUM_EXT_COLUMNS + NUM_QUOTIENT_SEGMENTS + n) * EXTENSION_DEGREE + {EXTENSION_DEGREE - 1}}
                add
                // _ *deep_codeword_weight[n]_last_word
            )
        };

        let verify_challenges_pointer = triton_asm!(
            push {conventional_challenges_pointer()}
            eq
            assert
        );

        let assert_top_two_xfes_eq = triton_asm!(
            // _ y2 y1 y0 x2 x1 x0

            swap 4
            eq
            assert
            // _ y2 x0 y0 x2

            swap 2
            eq
            assert
            // _ y2 x2

            eq
            assert
            // _
        );

        let swap_top_two_xfes = triton_asm!(
            // _ y2 y1 y0 x2 x1 x0
            swap 3
            swap 1
            swap 4
            swap 1
            swap 2
            swap 5
            swap 2

            // _ x2 x1 x0 y2 y1 y0
        );

        let main_loop_label = format!("{entrypoint}_main_loop");
        let main_loop_body = triton_asm!(
            //                                                        (u32, XFieldElement)
            // _ remaining_rounds fri_gen fri_offset *etrow *btrow *qseg_elem *fri_revealed_idx *beqd_ws *oodpnts
            // Calculate `current_fri_domain_value`
            dup 2
            read_mem 1
            swap 4
            pop 1
            // _ remaining_rounds fri_gen fri_offset *etrow *btrow *qseg_elem *fri_revealed_xfe *beqd_ws *oodpnts fri_idx

            dup 8
            pow
            dup 7
            mul
            push -1
            mul
            hint neg_fri_domain_point = stack[0]
            // _ remaining_rounds fri_gen fri_offset *etrow *btrow *qseg_elem *fri_revealed_xfe *beqd_ws *oodpnts (-fdom_pnt)


            dup 6
            dup 6
            dup 4
            call {inner_product_three_rows_with_weights_bfe_base} // expect arguments: *ext *base *ws
            hint base_and_ext_opened_row_element: Xfe = stack[0..3]
            // _ remaining_rounds fri_gen fri_offset *etrow *btrow *qseg_elem *fri_revealed_xfe *beqd_ws *oodpnts (-fdom_pnt) [be_opnd_elem]

            // Update `*btrow` and `*etrow` pointer values to point to previous element
            swap 9
            push {-((EXTENSION_DEGREE * NUM_EXT_COLUMNS) as i32)}
            add
            swap 9
            swap 8
            push {-(NUM_BASE_COLUMNS as i32)}
            add
            swap 8
            // _ remaining_rounds fri_gen fri_offset *etrow_prev *btrow_prev *qseg_elem *fri_revealed_xfe *beqd_ws *oodpnts (-fdom_pnt) [be_opnd_elem]

            push -1
            xb_mul
            hint neg_base_and_ext_opened_row_element: Xfe = stack[0..3]

            // Calculate `cuotient_curr_row_deep_value`
            dup 4
            {&OutOfDomainPoints::read_ood_point(OodPoint::CurrentRowPowNumSegments)}
            // _ remaining_rounds fri_gen fri_offset *etrow_prev *btrow_prev *qseg_elem *fri_revealed_xfe *beqd_ws *oodpnts (-fdom_pnt) [-be_opnd_elem] [oodp_pow_nsegs]

            dup 6
            add
            // _ remaining_rounds fri_gen fri_offset *etrow_prev *btrow_prev *qseg_elem *fri_revealed_xfe *beqd_ws *oodpnts (-fdom_pnt) [-be_opnd_elem] [oodp_pow_nsegs - fdom_pnt]

            x_invert
            // _ remaining_rounds fri_gen fri_offset *etrow_prev *btrow_prev *qseg_elem *fri_revealed_xfe *beqd_ws *oodpnts (-fdom_pnt) [-be_opnd_elem] [1/(oodp_pow_nsegs - fdom_pnt)]

            dup 8
            {&quotient_segment_codeword_weights_from_be_weights}
            dup 11
            call {inner_product_4_xfes}
            // _ remaining_rounds fri_gen fri_offset *etrow_prev *btrow_prev *qseg_elem *fri_revealed_xfe *beqd_ws *oodpnts (-fdom_pnt) [-be_opnd_elem] [1/(oodp_pow_nsegs - fdom_pnt)] [inner_prod]

            swap 13
            push {-((NUM_QUOTIENT_SEGMENTS * EXTENSION_DEGREE) as i32)}
            add
            swap 13
            // _ remaining_rounds fri_gen fri_offset *etrow_prev *btrow_prev *qseg_elem_prev *fri_revealed_xfe *beqd_ws *oodpnts (-fdom_pnt) [-be_opnd_elem] [1/(oodp_pow_nsegs - fdom_pnt)] [inner_prod]

            push -1
            xb_mul
            // _ remaining_rounds fri_gen fri_offset *etrow_prev *btrow_prev *qseg_elem_prev *fri_revealed_xfe *beqd_ws *oodpnts (-fdom_pnt) [-be_opnd_elem] [1/(oodp_pow_nsegs - fdom_pnt)] [-inner_prod]

            push {ood_curr_row_quotient_segment_value_pointer_read}
            read_mem {EXTENSION_DEGREE}
            pop 1
            // _ remaining_rounds fri_gen fri_offset *etrow_prev *btrow_prev *qseg_elem_prev *fri_revealed_xfe *beqd_ws *oodpnts (-fdom_pnt) [-be_opnd_elem] [1/(oodp_pow_nsegs - fdom_pnt)] [-inner_prod] [out_of_domain_curr_row_quotient_segment_value]

            xx_add
            xx_mul
            hint quot_curr_row_deep_value: XFieldElement = stack[0..3]
            // _ remaining_rounds fri_gen fri_offset *etrow_prev *btrow_prev *qseg_elem_prev *fri_revealed_xfe *beqd_ws *oodpnts (-fdom_pnt) [-be_opnd_elem] [(out_of_domain_curr_row_quotient_segment_value - inner_prod) / (oodp_pow_nsegs - fdom_pnt)]
            // _ remaining_rounds fri_gen fri_offset *etrow_prev *btrow_prev *qseg_elem_prev *fri_revealed_xfe *beqd_ws *oodpnts (-fdom_pnt) [-be_opnd_elem] [quot_curr_row_deep_value]


            /* Calculate $dv2 = quot_curr_row_deep_value * deep_codeword_weights[2]$ */
            dup 8
            {&deep_codeword_weights_read_address(2)}
            read_mem {EXTENSION_DEGREE}
            pop 1
            xx_mul
            // _ remaining_rounds fri_gen fri_offset *etrow_prev *btrow_prev *qseg_elem_prev *fri_revealed_xfe *beqd_ws *oodpnts (-fdom_pnt) [-be_opnd_elem] [quot_curr_row_deep_value * deep_codeword_weights[2]]
            // _ remaining_rounds fri_gen fri_offset *etrow_prev *btrow_prev *qseg_elem_prev *fri_revealed_xfe *beqd_ws *oodpnts (-fdom_pnt) [-be_opnd_elem] [dv2]

            dup 7
            {&OutOfDomainPoints::read_ood_point(OodPoint::CurrentRow)}
            // _ remaining_rounds fri_gen fri_offset *etrow_prev *btrow_prev *qseg_elem_prev *fri_revealed_xfe *beqd_ws *oodpnts (-fdom_pnt) [-be_opnd_elem] [dv2] [ood_point_curr_row]

            dup 9
            add
            x_invert
            // _ remaining_rounds fri_gen fri_offset *etrow_prev *btrow_prev *qseg_elem_prev *fri_revealed_xfe *beqd_ws *oodpnts (-fdom_pnt) [-be_opnd_elem] [dv2] [1/(ood_point_curr_row - fdom_pnt)]

            push {ood_curr_row_base_and_ext_value_pointer_read}
            read_mem {EXTENSION_DEGREE}
            pop 1
            // _ remaining_rounds fri_gen fri_offset *etrow_prev *btrow_prev *qseg_elem_prev *fri_revealed_xfe *beqd_ws *oodpnts (-fdom_pnt) [-be_opnd_elem] [dv2] [1/(ood_point_curr_row - fdom_pnt)] [out_of_domain_curr_row_base_and_ext_value]

            dup 11
            dup 11
            dup 11
            xx_add
            // _ remaining_rounds fri_gen fri_offset *etrow_prev *btrow_prev *qseg_elem_prev *fri_revealed_xfe *beqd_ws *oodpnts (-fdom_pnt) [-be_opnd_elem] [dv2] [1/(ood_point_curr_row - fdom_pnt)] [out_of_domain_curr_row_base_and_ext_value - be_opnd_elem]

            xx_mul
            // _ remaining_rounds fri_gen fri_offset *etrow_prev *btrow_prev *qseg_elem_prev *fri_revealed_xfe *beqd_ws *oodpnts (-fdom_pnt) [-be_opnd_elem] [dv2] [(out_of_domain_curr_row_base_and_ext_value - be_opnd_elem)/(ood_point_curr_row - fdom_pnt)]

            dup 11
            {&deep_codeword_weights_read_address(0)}
            read_mem {EXTENSION_DEGREE}              // read deep_codeword_weights[0]
            pop 1
            xx_mul
            // _ remaining_rounds fri_gen fri_offset *etrow_prev *btrow_prev *qseg_elem_prev *fri_revealed_xfe *beqd_ws *oodpnts (-fdom_pnt) [-be_opnd_elem] [dv2] [deep_codeword_weights[0] * (out_of_domain_curr_row_base_and_ext_value - be_opnd_elem)/(ood_point_curr_row - fdom_pnt)]
            // _ remaining_rounds fri_gen fri_offset *etrow_prev *btrow_prev *qseg_elem_prev *fri_revealed_xfe *beqd_ws *oodpnts (-fdom_pnt) [-be_opnd_elem] [dv2] [dv0]

            xx_add
            // _ remaining_rounds fri_gen fri_offset *etrow_prev *btrow_prev *qseg_elem_prev *fri_revealed_xfe *beqd_ws *oodpnts (-fdom_pnt) [-be_opnd_elem] [dv2 + dv0]
            hint dv2_plus_dv0: XFieldElement = stack[0..3]

            {&swap_top_two_xfes}
            // _ remaining_rounds fri_gen fri_offset *etrow_prev *btrow_prev *qseg_elem_prev *fri_revealed_xfe *beqd_ws *oodpnts (-fdom_pnt) [dv2 + dv0] [-be_opnd_elem]

            push {ood_next_row_base_and_ext_value_pointer_read}
            read_mem {EXTENSION_DEGREE}
            pop 1
            xx_add
            // _ remaining_rounds fri_gen fri_offset *etrow_prev *btrow_prev *qseg_elem_prev *fri_revealed_xfe *beqd_ws *oodpnts (-fdom_pnt) [dv2 + dv0] [ood_next_row_be_value - be_opnd_elem]

            dup 7
            {&OutOfDomainPoints::read_ood_point(OodPoint::NextRow)}
            // _ remaining_rounds fri_gen fri_offset *etrow_prev *btrow_prev *qseg_elem_prev *fri_revealed_xfe *beqd_ws *oodpnts (-fdom_pnt) [dv2 + dv0] [ood_next_row_be_value - be_opnd_elem] [out_of_domain_point_next_row]

            dup 9
            add
            // _ remaining_rounds fri_gen fri_offset *etrow_prev *btrow_prev *qseg_elem_prev *fri_revealed_xfe *beqd_ws *oodpnts (-fdom_pnt) [dv2 + dv0] [ood_next_row_be_value - be_opnd_elem] [out_of_domain_point_next_row - fdom_pnt]

            x_invert
            xx_mul
            // _ remaining_rounds fri_gen fri_offset *etrow_prev *btrow_prev *qseg_elem_prev *fri_revealed_xfe *beqd_ws *oodpnts (-fdom_pnt) [dv2 + dv0] [(ood_next_row_be_value - be_opnd_elem)/(out_of_domain_point_next_row - fdom_pnt)]

            dup 8
            {&deep_codeword_weights_read_address(1)}
            read_mem {EXTENSION_DEGREE}              // read deep_codeword_weights[1]
            pop 1
            xx_mul
            // _ remaining_rounds fri_gen fri_offset *etrow_prev *btrow_prev *qseg_elem_prev *fri_revealed_xfe *beqd_ws *oodpnts (-fdom_pnt) [dv2 + dv0] [deep_codeword_weights[1] * (ood_next_row_be_value - be_opnd_elem)/(out_of_domain_point_next_row - fdom_pnt)]
            // _ remaining_rounds fri_gen fri_offset *etrow_prev *btrow_prev *qseg_elem_prev *fri_revealed_xfe *beqd_ws *oodpnts (-fdom_pnt) [dv2 + dv0] [dv1]

            xx_add
            hint deep_value: XFieldElement = stack[0..3]
            // _ remaining_rounds fri_gen fri_offset *etrow_prev *btrow_prev *qseg_elem_prev *fri_revealed_xfe *beqd_ws *oodpnts (-fdom_pnt) [dv2 + dv0 + dv1]
            // _ remaining_rounds fri_gen fri_offset *etrow_prev *btrow_prev *qseg_elem_prev *fri_revealed_xfe *beqd_ws *oodpnts (-fdom_pnt) [deep_value]

            swap 1
            swap 2
            swap 3
            // _ remaining_rounds fri_gen fri_offset *etrow_prev *btrow_prev *qseg_elem_prev *fri_revealed_xfe *beqd_ws *oodpnts [deep_value] (-fdom_pnt)

            swap 6
            read_mem {EXTENSION_DEGREE}
            swap 9
            pop 1
            // _ remaining_rounds fri_gen fri_offset *etrow_prev *btrow_prev *qseg_elem_prev *fri_revealed_idx_prev *beqd_ws *oodpnts [deep_value] [fri_revealed_value]

            {&assert_top_two_xfes_eq}

            // _ remaining_rounds fri_gen fri_offset *etrow_prev *btrow_prev *qseg_elem_prev *fri_revealed_idx_prev *beqd_ws *oodpnts
        );
        let main_loop = triton_asm!(
            // The loop goes from last index to 1st index
            // Invariant: _ num_colli fri_gen fri_offset *etrow *btrow *qseg_elem *fri_revealed_elem *beqd_ws *oodpnts
            {main_loop_label}:
                // test end-condition
                dup 8
                push 0
                eq
                skiz
                    return

                {&main_loop_body}

                // Update counter
                swap 8
                push -1
                add
                swap 8
                recurse
        );

        triton_asm!(
            {entrypoint}:
                sponge_init
                // _ *clm *proof_iter


                /* Fiat-Shamir: Claim */
                dup 1
                call {instantiate_fiat_shamir_with_claim}
                // _ *clm *p_iter


                /* derive additional parameters */
                dup 0
                call {next_as_log_2_padded_height}
                // _ *clm *p_iter *log_2_padded_height

                read_mem 1
                pop 1
                // _ *clm *p_iter log_2_padded_height

                push 2
                pow
                hint padded_height = stack[0]
                // _ *clm *p_iter padded_height

                dup 0
                call {derive_fri_parameters}
                hint fri = stack[0]
                // _ *clm *p_iter padded_height *fri

                /* Fiat-Shamir 1 */
                dup 2
                call {next_as_merkleroot}
                hint b_mr = stack[0]
                // _ *clm *p_iter padded_height *fri *b_mr

                swap 4
                // _ *b_mr *p_iter padded_height *fri *clm

                call {get_challenges}
                // _ *b_mr *p_iter padded_height *fri *challenges

                {&verify_challenges_pointer}
                // _ *b_mr *p_iter padded_height *fri

                dup 2
                call {next_as_merkleroot}
                hint e_mr = stack[0]
                // _ *b_mr *p_iter padded_height *fri *e_mr

                call {sample_quotient_codeword_weights}
                // _ *b_mr *p_iter padded_height *fri *e_mr *quot_cw_ws
                hint quot_codeword_weights = stack[0]

                dup 4
                call {next_as_merkleroot}
                hint quot_mr = stack[0]
                // _ *b_mr *p_iter padded_height *fri *e_mr *quot_cw_ws *quot_mr


                /* sample and calculate OOD points (not rows) */
                push 0
                dup 5
                call {domain_generator}
                hint trace_domain_generator = stack[0]
                // _ *b_mr *p_iter padded_height *fri *e_mr *quot_cw_ws *quot_mr dom_gen

                dup 0
                // _ *b_mr *p_iter padded_height *fri *e_mr *quot_cw_ws *quot_mr dom_gen dom_gen

                call {sample_scalar_one}
                // _ *b_mr *p_iter padded_height *fri *e_mr *quot_cw_ws *quot_mr dom_gen dom_gen [ood_curr_row]

                call {calculate_out_of_domain_points}
                hint out_of_domain_points = stack[0]
                // _ *b_mr *p_iter padded_height *fri *e_mr *quot_cw_ws *quot_mr dom_gen *oodpnts


                /* out-of-domain quotient summands */
                push 2
                add
                read_mem {EXTENSION_DEGREE}
                push 1
                add
                // _ *b_mr *p_iter padded_height *fri *e_mr *quot_cw_ws *quot_mr dom_gen [out_of_domain_curr_row] *oodpnts

                swap 9
                // _ *b_mr *p_iter *oodpnts *fri *e_mr *quot_cw_ws *quot_mr dom_gen [out_of_domain_curr_row] padded_height

                swap 1
                swap 2
                swap 3
                swap 4
                // _ *b_mr *p_iter *oodpnts *fri *e_mr *quot_cw_ws *quot_mr [out_of_domain_curr_row] padded_height dom_gen

                call {quotient_summands}
                // _ *b_mr *p_iter *oodpnts *fri *e_mr *quot_cw_ws *quot_mr *quotient_summands


                /* Dequeue out-of-domain row */
                dup 6
                call {next_as_outofdomainbaserow}
                hint out_of_domain_curr_base_row: Pointer = stack[0]

                dup 7
                call {next_as_outofdomainextrow}
                hint out_of_domain_curr_ext_row: Pointer = stack[0]

                dup 8
                call {next_as_outofdomainbaserow}
                hint out_of_domain_next_base_row: Pointer = stack[0]

                dup 9
                call {next_as_outofdomainextrow}
                hint out_of_domain_next_ext_row: Pointer = stack[0]

                dup 10
                call {next_as_outofdomainquotientsegments}
                hint out_of_domain_quotient_segments: Pointer = stack[0]
                // _ *b_mr *p_iter *oodpnts *fri *e_mr *quot_cw_ws *quot_mr *quotient_summands *ood_brow_curr *ood_erow_curr *odd_brow_nxt *ood_erow_nxt *ood_quotient_segments

                dup 0
                push {out_of_domain_curr_row_quot_segments_pointer_pointer}
                write_mem 1
                pop 1


                /* Calculate `sum_of_evaluated_out_of_domain_quotient_segments` */
                dup 10
                {&OutOfDomainPoints::read_ood_point(OodPoint::CurrentRow)}
                // _ *b_mr *p_iter *oodpnts *fri *e_mr *quot_cw_ws *quot_mr *quotient_summands *ood_brow_curr *ood_erow_curr *odd_brow_nxt *ood_erow_nxt *ood_quotient_segments [ood_curr_row]

                call {horner_evaluation_of_ood_curr_row_quot_segments}
                // _ *b_mr *p_iter *oodpnts *fri *e_mr *quot_cw_ws *quot_mr *quotient_summands *ood_brow_curr *ood_erow_curr *odd_brow_nxt *ood_erow_nxt [sum_of_evaluated_out_of_domain_quotient_segments]


                /* Calculate inner product `out_of_domain_quotient_value` */
                swap 2
                swap 4
                swap 9
                swap 1
                swap 3
                swap 7
                // _ *b_mr *p_iter *oodpnts *fri *e_mr *odd_brow_nxt *quot_mr *ood_erow_nxt *ood_brow_curr *ood_erow_curr [sum_of_evaluated_out_of_domain_quotient_segments] *quot_cw_ws *quotient_summands

                call {inner_product_quotient_summands}
                // _ *b_mr *p_iter *oodpnts *fri *e_mr *odd_brow_nxt *quot_mr *ood_erow_nxt *ood_brow_curr *ood_erow_curr [sum_of_evaluated_out_of_domain_quotient_segments] [out_of_domain_quotient_value]


                /* Verify quotient's segments */
                {&assert_top_two_xfes_eq}
                // _ *b_mr *p_iter *oodpnts *fri *e_mr *odd_brow_nxt *quot_mr *ood_erow_nxt *ood_brow_curr *ood_erow_curr

                /* Fiat-shamir 2 */
                call {sample_beqd_weights}
                hint beqd_weights = stack[0]
                // _ *b_mr *p_iter *oodpnts *fri *e_mr *odd_brow_nxt *quot_mr *ood_erow_nxt *ood_brow_curr *ood_erow_curr *beqd_ws

                swap 10
                // _ *beqd_ws *p_iter *oodpnts *fri *e_mr *odd_brow_nxt *quot_mr *ood_erow_nxt *ood_brow_curr *ood_erow_curr *b_mr


                /* FRI */
                // We need the `fri` data structure for field values later, so we preserve its pointer on the stack
                dup 9
                dup 8
                call {fri_verify}
                hint fri_revealed = stack[0]
                // _ *beqd_ws *p_iter *oodpnts *fri *e_mr *odd_brow_nxt *quot_mr *ood_erow_nxt *ood_brow_curr *ood_erow_curr *b_mr *fri_revealed


                /* Dequeue base-table rows and verify against its Merkle root */
                dup 10
                call {next_as_basetablerows}
                // _ *beqd_ws *p_iter *oodpnts *fri *e_mr *odd_brow_nxt *quot_mr *ood_erow_nxt *ood_brow_curr *ood_erow_curr *b_mr *fri_revealed *btrows


                dup 9
                {&num_collinearity_checks_field}
                read_mem 1
                pop 1
                // _ *beqd_ws *p_iter *oodpnts *fri *e_mr *odd_brow_nxt *quot_mr *ood_erow_nxt *ood_brow_curr *ood_erow_curr *b_mr *fri_revealed *btrows num_colli

                dup 10
                {&domain_length_field}
                read_mem 1
                pop 1
                // _ *beqd_ws *p_iter *oodpnts *fri *e_mr *odd_brow_nxt *quot_mr *ood_erow_nxt *ood_brow_curr *ood_erow_curr *b_mr *fri_revealed *btrows num_colli dom_len

                log_2_floor
                // _ *beqd_ws *p_iter *oodpnts *fri *e_mr *odd_brow_nxt *quot_mr *ood_erow_nxt *ood_brow_curr *ood_erow_curr *b_mr *fri_revealed *btrows num_colli mt_height

                swap 1
                swap 2
                swap 3
                swap 4
                // _ *beqd_ws *p_iter *oodpnts *fri *e_mr *odd_brow_nxt *quot_mr *ood_erow_nxt *ood_brow_curr *ood_erow_curr *fri_revealed *btrows num_colli mt_height *b_mr

                dup 4
                // _ *beqd_ws *p_iter *oodpnts *fri *e_mr *odd_brow_nxt *quot_mr *ood_erow_nxt *ood_brow_curr *ood_erow_curr *fri_revealed *btrows num_colli mt_height *b_mr *fri_revealed

                dup 4
                // _ *beqd_ws *p_iter *oodpnts *fri *e_mr *odd_brow_nxt *quot_mr *ood_erow_nxt *ood_brow_curr *ood_erow_curr *fri_revealed *btrows num_colli mt_height *b_mr *fri_revealed *btrows

                call {verify_base_table_rows}
                // _ *beqd_ws *p_iter *oodpnts *fri *e_mr *odd_brow_nxt *quot_mr *ood_erow_nxt *ood_brow_curr *ood_erow_curr *fri_revealed *btrows


                /* Dequeue and ignore base-table's authentication path */
                dup 10
                call {next_as_authentication_path}
                pop 1
                // _ *beqd_ws *p_iter *oodpnts *fri *e_mr *odd_brow_nxt *quot_mr *ood_erow_nxt *ood_brow_curr *ood_erow_curr *fri_revealed *btrows

                swap 7
                // _ *beqd_ws *p_iter *oodpnts *fri *btrows *odd_brow_nxt *quot_mr *ood_erow_nxt *ood_brow_curr *ood_erow_curr *fri_revealed *e_mr


                /* Dequeue ext-table rows and verify against its Merkle root */
                dup 10
                call {next_as_exttablerows}
                // _ *beqd_ws *p_iter *oodpnts *fri *btrows *odd_brow_nxt *quot_mr *ood_erow_nxt *ood_brow_curr *ood_erow_curr *fri_revealed *e_mr *etrows

                swap 1
                // _ *beqd_ws *p_iter *oodpnts *fri *btrows *odd_brow_nxt *quot_mr *ood_erow_nxt *ood_brow_curr *ood_erow_curr *fri_revealed *etrows *e_mr

                dup 9
                {&num_collinearity_checks_field}
                read_mem 1
                pop 1
                dup 10
                {&domain_length_field}
                read_mem 1
                pop 1
                log_2_floor
                // _ *beqd_ws *p_iter *oodpnts *fri *btrows *odd_brow_next *quot_mr *ood_erow_nxt *ood_brow_curr *ood_erow_curr *fri_revealed *etrows *e_mr num_colli mt_height

                swap 1
                swap 2
                dup 4
                dup 4
                // _ *beqd_ws *p_iter *oodpnts *fri *btrows *odd_brow_next *quot_mr *ood_erow_nxt *ood_brow_curr *ood_erow_curr *fri_revealed *etrows num_colli mt_height *e_mr *fri_revealed *etrows

                call {verify_extension_table_rows}
                // _ *beqd_ws *p_iter *oodpnts *fri *btrows *odd_brow_next *quot_mr *ood_erow_nxt *ood_brow_curr *ood_erow_curr *fri_revealed *etrows


                /* Dequeue and ignore ext-table's authentication path */
                dup 10
                call {next_as_authentication_path}
                pop 1

                swap 5
                // _ *beqd_ws *p_iter *oodpnts *fri *btrows *odd_brow_next *etrows *ood_erow_nxt *ood_brow_curr *ood_erow_curr *fri_revealed *quot_mr


                /* Dequeue quotient-table rows and verify against its Merkle root */
                dup 10
                call {next_as_quotient_segment_elements}
                swap 1
                dup 9
                {&num_collinearity_checks_field}
                read_mem 1
                pop 1
                dup 10
                {&domain_length_field}
                read_mem 1
                pop 1
                log_2_floor
                // _ *beqd_ws *p_iter *oodpnts *fri *btrows *odd_brow_next *etrows *ood_erow_nxt *ood_brow_curr *ood_erow_curr *fri_revealed *qseg_elems *quot_mr num_colli mt_height

                swap 1
                swap 2
                // _ *beqd_ws *p_iter *oodpnts *fri *btrows *odd_brow_next *etrows *ood_erow_nxt *ood_brow_curr *ood_erow_curr *fri_revealed *qseg_elems num_colli mt_height *quot_mr

                dup 4
                dup 4
                // _ *beqd_ws *p_iter *oodpnts *fri *btrows *odd_brow_next *etrows *ood_erow_nxt *ood_brow_curr *ood_erow_curr *fri_revealed *qseg_elems num_colli mt_height *quot_mr *fri_revealed *qseg_elems

                call {verify_quotient_segments}
                // _ *beqd_ws *p_iter *oodpnts *fri *btrows *odd_brow_next *etrows *ood_erow_nxt *ood_brow_curr *ood_erow_curr *fri_revealed *qseg_elems

                /* Various length asserts */
                // assert!(num_combination_codeword_checks == quotient_segment_elements.len());
                dup 8
                {&num_collinearity_checks_field}
                read_mem 1
                pop 1
                hint num_combination_codeword_checks = stack[0]
                // _ *beqd_ws *p_iter *oodpnts *fri *btrows *odd_brow_next *etrows *ood_erow_nxt *ood_brow_curr *ood_erow_curr *fri_revealed *qseg_elems num_colli

                // assert!(num_combination_codeword_checks == revealed_fri_indices_and_elements.len())
                dup 2
                read_mem 1
                pop 1
                dup 1
                eq
                assert
                // _ *beqd_ws *p_iter *oodpnts *fri *btrows *odd_brow_next *etrows *ood_erow_nxt *ood_brow_curr *ood_erow_curr *fri_revealed *qseg_elems num_colli

                // assert!(num_combination_codeword_checks == base_table_rows.len());
                dup 8
                read_mem 1
                pop 1
                dup 1
                eq
                assert

                // assert!(num_combination_codeword_checks == ext_table_rows.len())
                dup 6
                read_mem 1
                pop 1
                dup 1
                eq
                assert

                // assert!(num_combination_codeword_checks == quotient_segment_elements.len());
                dup 1
                read_mem 1
                pop 1
                dup 1
                eq
                assert
                // _ *beqd_ws *p_iter *oodpnts *fri *btrows *odd_brow_next *etrows *ood_erow_nxt *ood_brow_curr *ood_erow_curr *fri_revealed *qseg_elems num_colli

                /* Clean up stack */
                swap 12
                swap 11
                pop 1
                // _ num_colli *beqd_ws *oodpnts *fri *btrows *odd_brow_next *etrows *ood_erow_nxt *ood_brow_curr *ood_erow_curr *fri_revealed *qseg_elems

                /* Sum out-of-domain values */
                // Goal for stack: `_ *ood_erow_curr *ood_brow_curr *beqd_ws`, preserving `*beqd_ws`.

                dup 10
                swap 2
                // _ num_colli *beqd_ws *oodpnts *fri *btrows *odd_brow_next *etrows *ood_erow_nxt *ood_brow_curr *ood_erow_curr *beqd_ws *qseg_elems *fri_revealed

                swap 4
                swap 1
                swap 3
                swap 2
                // _ num_colli *beqd_ws *oodpnts *fri *btrows *odd_brow_next *etrows *ood_erow_nxt *fri_revealed *qseg_elems *ood_erow_curr *ood_brow_curr *beqd_ws

                call {inner_product_three_rows_with_weights_xfe_base} // expects arguments: *ext *base *ws
                hint out_of_domain_curr_row_base_and_ext_value: XFieldElement = stack[0..3]
                // _ num_colli *beqd_ws *oodpnts *fri *btrows *odd_brow_next *etrows *ood_erow_nxt *fri_revealed *qseg_elems [ood_curr_beval]

                push {ood_curr_row_base_and_ext_value_pointer_write}
                write_mem {EXTENSION_DEGREE}
                pop 1
                // _ num_colli *beqd_ws *oodpnts *fri *btrows *odd_brow_next *etrows *ood_erow_nxt *fri_revealed *qseg_elems

                // Goal: `_ *ood_erow_nxt *odd_brow_next *beqd_ws`, preserving `*beqd_ws`.

                swap 2
                swap 1
                swap 4
                dup 8
                // _ num_colli *beqd_ws *oodpnts *fri *btrows *fri_revealed *etrows *qseg_elems *ood_erow_nxt *odd_brow_next *beqd_ws

                call {inner_product_three_rows_with_weights_xfe_base}  // expects arguments: *ext *base *ws
                hint out_of_domain_next_row_base_and_ext_value: XFieldElement = stack[0..3]
                // _ num_colli *beqd_ws *oodpnts *fri *btrows *fri_revealed *etrows *qseg_elems [ood_next_value]

                push {ood_next_row_base_and_ext_value_pointer_write}
                write_mem {EXTENSION_DEGREE}
                pop 1
                // _ num_colli *beqd_ws *oodpnts *fri *btrows *fri_revealed *etrows *qseg_elems

                // Goal: `_ *quotient_segment_codeword_weights *ood_curr_row_quot_segments`
                dup 6
                {&quotient_segment_codeword_weights_from_be_weights}
                // _ num_colli *beqd_ws *oodpnts *fri *btrows *fri_revealed *etrows *qseg_elems *quotient_segment_codeword_weights

                push {out_of_domain_curr_row_quot_segments_pointer_pointer}
                read_mem 1
                pop 1
                // _ num_colli *beqd_ws *oodpnts *fri *btrows *fri_revealed *etrows *qseg_elems *quotient_segment_codeword_weights *ood_curr_row_quot_segments

                call {inner_product_4_xfes}
                hint out_of_domain_curr_row_quotient_segment_value: XFieldElement = stack[0..3]
                // _ num_colli *beqd_ws *oodpnts *fri *btrows *fri_revealed *etrows *qseg_elems [out_of_domain_curr_row_quotient_segment_value]

                push {ood_curr_row_quotient_segment_value_pointer_write}
                write_mem {EXTENSION_DEGREE}
                pop 1
                // _ num_colli *beqd_ws *oodpnts *fri *btrows *fri_revealed *etrows *qseg_elems

                // Put fri domain generator and domain offset on stack
                swap 4
                dup 0
                {&domain_offset_field}
                read_mem 1
                pop 1
                hint fri_domain_offset = stack[0]
                // _ num_colli *beqd_ws *oodpnts *qseg_elems *btrows *fri_revealed *etrows *fri fri_offset

                swap 1
                {&domain_generator_field}
                read_mem 1
                pop 1
                hint fri_domain_gen = stack[0]
                // _ num_colli *beqd_ws *oodpnts *qseg_elems *btrows *fri_revealed *etrows fri_offset fri_gen

                // adjust relevant pointers to point to last word in sequence, as they are traversed
                // high-to-low in the main loop

                // Adjust *fri_revealed (list) to point to last word
                swap 3
                dup 8
                push {EXTENSION_DEGREE + 1} // size of element in `fri_revealed` list
                mul
                add
                hint fri_revealed_elem = stack[0]
                swap 3

                // Adjust *btrows (list) to point to last element
                swap 4
                push 1
                add
                push {NUM_BASE_COLUMNS} // size of element of base row list
                dup 9
                push -1
                add
                mul
                add
                hint base_table_row = stack[0]
                swap 4

                // Adjust *etrows (list) to point to last element
                swap 2
                push 1
                add
                push {NUM_EXT_COLUMNS * EXTENSION_DEGREE} // size of element of ext row list
                dup 9
                push -1
                add
                mul
                add
                hint ext_table_row = stack[0]
                swap 2

                // Adjust *qseg_elems to point to last element
                swap 5
                push 1
                add
                push {NUM_QUOTIENT_SEGMENTS * EXTENSION_DEGREE} // size of element of quot row list
                dup 9
                push -1
                add
                mul
                add
                hint quotient_segment_elem = stack[0]
                swap 5

                // _ num_colli *beqd_ws *oodpnts *qseg_elems *btrows *fri_revealed *etrows fri_offset fri_gen

                /* Reorganize stack for main-loop, to keep all necessary words accessible without spilling */
                swap 7
                swap 1
                swap 6
                swap 2
                swap 5
                // _ num_colli fri_gen fri_offset *etrows_last_elem *btrows *fri_revealed *oodpnts *beqd_ws *qseg_elems

                swap 3
                swap 2
                // _ num_colli fri_gen fri_offset *etrows_last_elem *btrows_last_elem *qseg_elems_last_elem *fri_revealed_last_elem *beqd_ws *oodpnts

                call {main_loop_label}
                // _ 0 fri_gen fri_offset *etrow_elem *btrows_elem *qseg_elem *fri_revealed_elem *beqd_ws *oodpnts

                /* Cleanup stack */
                pop 5 pop 4

                return

                {&main_loop}
        )
    }
}

#[cfg(test)]
pub mod tests {
    use std::collections::HashMap;
    use std::io::Write;

    use crate::execute_test;
    use crate::test_helpers::maybe_write_tvm_output_to_disk;
    use crate::verifier::claim::shared::insert_claim_into_static_memory;
    use crate::verifier::vm_proof_iter::shared::insert_default_proof_iter_into_memory;
    use itertools::Itertools;
    use tests::fri::test_helpers::extract_fri_proof;
    use triton_vm::example_programs::FIBONACCI_SEQUENCE;
    use triton_vm::instruction::Instruction;
    use triton_vm::proof_stream::ProofStream;
    use triton_vm::table::table_column::{MasterBaseTableColumn, ProcessorBaseTableColumn};

    use super::*;

    #[test]
    fn dump_stark_verifier() {
        let code = StarkVerify::default().link_for_isolated_run();
        let program = triton_program!({ &code });
        let mut program_file = std::fs::File::create("stark_verifier.tasm").unwrap();
        write!(program_file, "{program}").unwrap();
    }

    #[test]
    fn list_all_executed_instructions_of_stark_verifier() {
        // first iteration – just any proof
        let (stark, claim, proof) = triton_vm::prove_program(
            &FIBONACCI_SEQUENCE,
            PublicInput::new(bfe_vec![10]),
            NonDeterminism::default(),
        )
        .unwrap();

        let (mut non_determinism, _) = nd_from_proof(&stark, &claim, proof);
        let (claim_pointer, claim_size) =
            insert_claim_into_static_memory(&mut non_determinism.ram, claim);

        let proof_iter_pointer = bfe!(1_u32 << 31);
        insert_default_proof_iter_into_memory(&mut non_determinism.ram, proof_iter_pointer);

        let stark_verify_snippet = StarkVerify::default();
        let stark_verifier = triton_program! {
            push {claim_pointer}
            push {proof_iter_pointer}
            {&stark_verify_snippet.link_for_isolated_run_populated_static_memory(claim_size)}
        };

        // next iteration – first recurisve proof
        let (stark, claim, proof) =
            triton_vm::prove_program(&stark_verifier, PublicInput::default(), non_determinism)
                .unwrap();

        let (mut non_determinism, _) = nd_from_proof(&stark, &claim, proof);
        let (claim_pointer, claim_size) =
            insert_claim_into_static_memory(&mut non_determinism.ram, claim);

        let proof_iter_pointer = bfe!(1_u32 << 31);
        insert_default_proof_iter_into_memory(&mut non_determinism.ram, proof_iter_pointer);

        let stark_verify_snippet = StarkVerify::default();
        let stark_verifier = triton_program! {
            push {claim_pointer}
            push {proof_iter_pointer}
            {&stark_verify_snippet.link_for_isolated_run_populated_static_memory(claim_size)}
        };

        // now just run, don't prove
        let (aet, _) = stark_verifier
            .trace_execution(PublicInput::default(), non_determinism)
            .unwrap();

        let mut executed_instructions = vec![];
        for row in aet.processor_trace.rows() {
            let ci = row[ProcessorBaseTableColumn::CI.base_table_index()];
            let instruction = Instruction::try_from(ci).unwrap();
            let instruction = if instruction.arg().is_some() {
                let arg = row[ProcessorBaseTableColumn::NIA.base_table_index()];
                instruction.change_arg(arg).unwrap()
            } else {
                instruction
            };
            executed_instructions.push(instruction);
        }

        let instructions_str = executed_instructions.iter().join("\n");
        let mut the_file = std::fs::File::create("stark_verifier.instructions").unwrap();
        write!(the_file, "{instructions_str}").unwrap();
    }

    #[ignore = "Used for debugging when comparing two versions of the verifier"]
    #[test]
    fn verify_from_stored_proof_output() {
        use std::fs::File;
        let stark = File::open("stark.json").expect("stark file should open read only");
        let stark: Stark = serde_json::from_reader(stark).unwrap();
        let claim = File::open("claim.json").expect("claim file should open read only");
        let claim_for_proof: Claim = serde_json::from_reader(claim).unwrap();
        let proof = File::open("proof.json").expect("proof file should open read only");
        let proof: Proof = serde_json::from_reader(proof).unwrap();

        let (mut non_determinism, _inner_padded_height) =
            nd_from_proof(&stark, &claim_for_proof, proof);

        let (claim_pointer, claim_size) =
            insert_claim_into_static_memory(&mut non_determinism.ram, claim_for_proof);

        let proof_iter_pointer = BFieldElement::new(1 << 31);
        insert_default_proof_iter_into_memory(&mut non_determinism.ram, proof_iter_pointer);

        let snippet = StarkVerify {
            stark: Stark::default(),
        };

        let mut init_stack = [
            snippet.init_stack_for_isolated_run(),
            vec![claim_pointer, proof_iter_pointer],
        ]
        .concat();
        let code = snippet.link_for_isolated_run_populated_static_memory(claim_size);
        let _final_tasm_state = execute_test(
            &code,
            &mut init_stack,
            snippet.stack_diff(),
            vec![],
            non_determinism,
            None,
        );
    }

    /// Run the verifier, and return the inner padded height for (extremely) crude benchmarking.
    fn prop(
        inner_nondeterminism: NonDeterminism,
        inner_program: &Program,
        inner_public_input: &[BFieldElement],
        stark: Stark,
    ) -> (VMState, usize) {
        let (mut non_determinism, claim_for_proof, inner_padded_height) =
            non_determinism_claim_and_padded_height(
                inner_program,
                inner_public_input,
                inner_nondeterminism,
                &stark,
            );

        let (claim_pointer, claim_size) =
            insert_claim_into_static_memory(&mut non_determinism.ram, claim_for_proof);

        let proof_iter_pointer = BFieldElement::new(1 << 31);
        insert_default_proof_iter_into_memory(&mut non_determinism.ram, proof_iter_pointer);

        let snippet = StarkVerify { stark };
        let mut init_stack = [
            snippet.init_stack_for_isolated_run(),
            vec![claim_pointer, proof_iter_pointer],
        ]
        .concat();
        let code = snippet.link_for_isolated_run_populated_static_memory(claim_size);

        let final_tasm_state = execute_test(
            &code,
            &mut init_stack,
            snippet.stack_diff(),
            vec![],
            non_determinism,
            None,
        );

        (final_tasm_state, inner_padded_height)
    }

    #[test]
    fn different_fri_expansion_factors() {
        const FACTORIAL_ARGUMENT: u32 = 3;

        for log2_of_fri_expansion_factor in 2..=5 {
            println!("log2_of_fri_expansion_factor: {log2_of_fri_expansion_factor}");
            let factorial_program = factorial_program_with_io();
            let stark = Stark::new(160, log2_of_fri_expansion_factor);
            let (final_vm_state, inner_padded_height) = prop(
                NonDeterminism::default(),
                &factorial_program,
                &[FACTORIAL_ARGUMENT.into()],
                stark,
            );
            println!(
                "TASM-verifier of factorial({FACTORIAL_ARGUMENT}):\n
                Fri expansion factor: {}\n
                clock cycle count: {}.\n
                Inner padded height was: {}",
                1 << log2_of_fri_expansion_factor,
                final_vm_state.cycle_count,
                inner_padded_height,
            );
        }
    }

    #[test]
    fn verify_tvm_proof_factorial_program() {
        const FACTORIAL_ARGUMENT: u32 = 3;
        let factorial_program = factorial_program_with_io();
        let stark = Stark::default();
        let (final_vm_state, inner_padded_height) = prop(
            NonDeterminism::default(),
            &factorial_program,
            &[FACTORIAL_ARGUMENT.into()],
            stark,
        );

        println!(
            "TASM-verifier of factorial({FACTORIAL_ARGUMENT}):\n
            clock cycle count: {}.\n
            Inner padded height was: {}",
            final_vm_state.cycle_count, inner_padded_height,
        );
    }

    pub(super) fn factorial_program_with_io() -> Program {
        triton_program!(
            read_io 1
            push 1               // n accumulator
            call factorial       // 0 accumulator!
            write_io 1
            halt

            factorial:           // n acc
                // if n == 0: return
                dup 1            // n acc n
                push 0 eq        // n acc n==0
                skiz             // n acc
                return           // 0 acc
                // else: multiply accumulator with n and recurse
                dup 1            // n acc n
                mul              // n acc·n
                swap 1           // acc·n n
                push -1 add      // acc·n n-1
                swap 1           // n-1 acc·n

                recurse
        )
    }

    pub(super) fn nd_from_proof(
        stark: &Stark,
        claim: &Claim,
        proof: Proof,
    ) -> (NonDeterminism, usize) {
        let fri = stark.derive_fri(proof.padded_height().unwrap()).unwrap();
        let proof_stream = ProofStream::try_from(&proof).unwrap();
        let proof_extraction = extract_fri_proof(&proof_stream, claim, stark);
        let tasm_lib_fri: fri::verify::FriVerify = fri.into();
        let fri_proof_digests =
            tasm_lib_fri.extract_digests_required_for_proving(&proof_extraction.fri_proof_stream);
        let padded_height = proof.padded_height().unwrap();
        let Proof(raw_proof) = proof;
        let ram: HashMap<_, _> = raw_proof
            .into_iter()
            .enumerate()
            .map(|(k, v)| (BFieldElement::new(k as u64), v))
            .collect();

        let nd_digests = [
            fri_proof_digests,
            proof_extraction
                .base_tree_authentication_paths
                .into_iter()
                .flatten()
                .collect_vec(),
            proof_extraction
                .ext_tree_authentication_paths
                .into_iter()
                .flatten()
                .collect_vec(),
            proof_extraction
                .quot_tree_authentication_paths
                .into_iter()
                .flatten()
                .collect_vec(),
        ]
        .concat();

        (
            NonDeterminism::default()
                .with_ram(ram)
                .with_digests(nd_digests),
            padded_height,
        )
    }

    /// Generate the required data for the verifier, when verifying
    /// a given program, input, nondeterminism, and STARK.
    pub fn non_determinism_claim_and_padded_height(
        inner_program: &Program,
        inner_public_input: &[BFieldElement],
        inner_nondeterminism: NonDeterminism,
        stark: &Stark,
    ) -> (NonDeterminism, triton_vm::proof::Claim, usize) {
        println!("Generating proof for non-determinism");

        let (aet, inner_output) = inner_program
            .trace_execution(inner_public_input.into(), inner_nondeterminism.clone())
            .unwrap();
        let claim = Claim {
            program_digest: inner_program.hash::<Tip5>(),
            input: inner_public_input.to_vec(),
            output: inner_output,
        };

        triton_vm::profiler::start("inner program");
        let proof = stark.prove(&claim, &aet).unwrap();
        let profile = triton_vm::profiler::finish();
        let padded_height = proof.padded_height().unwrap();
        let report = profile
            .with_cycle_count(aet.processor_trace.nrows())
            .with_padded_height(padded_height)
            .with_fri_domain_len(stark.derive_fri(padded_height).unwrap().domain.length);
        println!("Done generating proof for non-determinism");
        println!("{report}");

        assert!(
            stark.verify(&claim, &proof).is_ok(),
            "Proof from TVM must verify through TVM"
        );

        maybe_write_tvm_output_to_disk(stark, &claim, &proof);

        let (non_determinism, padded_height) = nd_from_proof(stark, &claim, proof);

        (non_determinism, claim, padded_height)
    }
}

#[cfg(test)]
mod benches {
    use std::cell::RefCell;
    use std::rc::Rc;

    use benches::tests::factorial_program_with_io;
    use benches::tests::nd_from_proof;
    use benches::tests::non_determinism_claim_and_padded_height;

    use crate::generate_full_profile;
    use crate::linker::execute_bench;
    use crate::linker::link_for_isolated_run;
    use crate::memory::encode_to_memory;
    use crate::snippet_bencher::write_benchmarks;
    use crate::snippet_bencher::BenchmarkCase;
    use crate::snippet_bencher::NamedBenchmarkResult;
    use crate::test_helpers::prepend_program_with_stack_setup;
    use crate::verifier::claim::shared::insert_claim_into_static_memory;
    use crate::verifier::vm_proof_iter::shared::insert_default_proof_iter_into_memory;

    use super::*;

    #[ignore = "Used for profiling the verification of a proof stored on disk."]
    #[test]
    fn profile_from_stored_proof_output() {
        use std::fs::File;
        let stark = File::open("stark.json").expect("stark file should open read only");
        let stark: Stark = serde_json::from_reader(stark).unwrap();
        let claim_file = File::open("claim.json").expect("claim file should open read only");
        let claim_for_proof: Claim = serde_json::from_reader(claim_file).unwrap();
        let proof = File::open("proof.json").expect("proof file should open read only");
        let proof: Proof = serde_json::from_reader(proof).unwrap();

        let (mut non_determinism, _inner_padded_height) =
            nd_from_proof(&stark, &claim_for_proof, proof);

        let (claim_pointer, claim_size) =
            insert_claim_into_static_memory(&mut non_determinism.ram, claim_for_proof.clone());

        let proof_iter_pointer = BFieldElement::new(1 << 31);
        insert_default_proof_iter_into_memory(&mut non_determinism.ram, proof_iter_pointer);

        let snippet = StarkVerify { stark };

        let init_stack = [
            snippet.init_stack_for_isolated_run(),
            vec![claim_pointer, proof_iter_pointer],
        ]
        .concat();
        let code = snippet.link_for_isolated_run_populated_static_memory(claim_size);
        let program = prepend_program_with_stack_setup(&init_stack, &Program::new(&code));

        let name = snippet.entrypoint();
        let profile = generate_full_profile(
            &name,
            program,
            &PublicInput::new(claim_for_proof.input),
            &non_determinism,
        );
        println!("{}", profile);
    }

    #[test]
    fn benchmark_small_default_stark() {
        benchmark_verifier(3, 1 << 8, Stark::default());
        benchmark_verifier(40, 1 << 9, Stark::default());
    }

    #[ignore = "Takes a fairly long time. Intended to find optimal FRI expansion factor."]
    #[test]
    fn small_benchmark_different_fri_expansion_factors() {
        for log2_of_fri_expansion_factor in 1..=5 {
            let stark = Stark::new(160, log2_of_fri_expansion_factor);
            benchmark_verifier(10, 1 << 8, stark);
            benchmark_verifier(40, 1 << 9, stark);
            benchmark_verifier(80, 1 << 10, stark);
        }
    }

    #[ignore = "Takes a very long time. Intended to find optimal FRI expansion factor. Make sure to run
       with `RUSTFLAGS=\"-C opt-level=3 -C debug-assertions=no\"`"]
    #[test]
    fn big_benchmark_different_fri_expansion_factors() {
        for log2_of_fri_expansion_factor in 1..=5 {
            let stark = Stark::new(160, log2_of_fri_expansion_factor);
            benchmark_verifier(25600, 1 << 19, stark);
            benchmark_verifier(51200, 1 << 20, stark);
            benchmark_verifier(102400, 1 << 21, stark);
        }
    }

    #[ignore = "Intended to generate data about verifier table heights as a function of inner padded
       height. Make sure to run with `RUSTFLAGS=\"-C opt-level=3 -C debug-assertions=no\"`"]
    #[test]
    fn benchmark_verification_as_a_function_of_inner_padded_height() {
        for (fact_arg, expected_inner_padded_height) in [
            (10, 1 << 8),
            (40, 1 << 9),
            (80, 1 << 10),
            (100, 1 << 11),
            (200, 1 << 12),
            (400, 1 << 13),
            (800, 1 << 14),
            (1600, 1 << 15),
            (3200, 1 << 16),
            (6400, 1 << 17),
            (12800, 1 << 18),
            (25600, 1 << 19),
            (51200, 1 << 20),
            (102400, 1 << 21),
        ] {
            benchmark_verifier(fact_arg, expected_inner_padded_height, Stark::default());
        }
    }

    fn benchmark_verifier(
        factorial_argument: u32,
        expected_inner_padded_height: usize,
        stark: Stark,
    ) {
        let factorial_program = factorial_program_with_io();
        let (mut non_determinism, claim_for_proof, inner_padded_height) =
            non_determinism_claim_and_padded_height(
                &factorial_program,
                &[bfe!(factorial_argument)],
                NonDeterminism::default(),
                &stark,
            );

        let claim_pointer = BFieldElement::new(1 << 30);
        encode_to_memory(
            &mut non_determinism.ram,
            claim_pointer,
            claim_for_proof.clone(),
        );

        let proof_iter_pointer = BFieldElement::new(1 << 31);
        insert_default_proof_iter_into_memory(&mut non_determinism.ram, proof_iter_pointer);

        assert_eq!(expected_inner_padded_height, inner_padded_height);

        let snippet = StarkVerify { stark };

        let init_stack = [
            snippet.init_stack_for_isolated_run(),
            vec![claim_pointer, proof_iter_pointer],
        ]
        .concat();
        let code = link_for_isolated_run(Rc::new(RefCell::new(snippet.clone())));
        let benchmark = execute_bench(&code, &init_stack, vec![], non_determinism.clone(), None);
        let benchmark = NamedBenchmarkResult {
            name: format!(
                "{}_inner_padded_height_{}_fri_exp_{}",
                snippet.entrypoint(),
                inner_padded_height,
                stark.fri_expansion_factor
            ),
            benchmark_result: benchmark,
            case: BenchmarkCase::CommonCase,
        };

        write_benchmarks(vec![benchmark]);

        let program = prepend_program_with_stack_setup(&init_stack, &Program::new(&code));
        let name = snippet.entrypoint();
        let profile = generate_full_profile(
            &name,
            program,
            &PublicInput::new(claim_for_proof.input),
            &non_determinism,
        );
        println!("{}", profile);
    }
}
