use triton_vm::prelude::*;
use triton_vm::proof_item::ProofItemVariant;
use triton_vm::table::extension_table::Quotientable;
use triton_vm::table::master_table::MasterExtTable;
use triton_vm::table::NUM_BASE_COLUMNS;
use triton_vm::table::NUM_EXT_COLUMNS;
use triton_vm::table::NUM_QUOTIENT_SEGMENTS;
use triton_vm::twenty_first::shared_math::x_field_element::EXTENSION_DEGREE;

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
use crate::recufier::challenges;
use crate::recufier::challenges::shared::conventional_challenges_pointer;
use crate::recufier::claim::instantiate_fiat_shamir_with_claim::InstantiateFiatShamirWithClaim;
use crate::recufier::claim::shared::claim_type;
use crate::recufier::fri;
use crate::recufier::fri::verify::FriSnippet;
use crate::recufier::fri::verify::FriVerify;
use crate::recufier::master_ext_table::quotient_summands::QuotientSummands;
use crate::recufier::master_ext_table::verify_table_rows::ColumnType;
use crate::recufier::master_ext_table::verify_table_rows::VerifyTableRows;
use crate::recufier::out_of_domain_points::OodPoint;
use crate::recufier::out_of_domain_points::OutOfDomainPoints;
use crate::recufier::vm_proof_iter::dequeue_next_as::DequeueNextAs;
use crate::recufier::vm_proof_iter::shared::vm_proof_iter_type;
use crate::traits::basic_snippet::BasicSnippet;

#[derive(Debug, Clone)]
pub struct StarkVerify {
    stark_parameters: Stark,
    log_2_padded_height: Option<u32>,
}

impl BasicSnippet for StarkVerify {
    fn inputs(&self) -> Vec<(DataType, String)> {
        vec![
            (DataType::StructRef(claim_type()), "claim".to_owned()),
            (
                DataType::StructRef(vm_proof_iter_type()),
                "vm_proof_iter".to_owned(),
            ),
        ]
    }

    fn outputs(&self) -> Vec<(DataType, String)> {
        vec![]
    }

    fn entrypoint(&self) -> String {
        "tasmlib_recufier_stark_verify".to_owned()
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
        let derive_fri_parameters = library.import(Box::new(
            fri::derive_from_stark_params::DeriveFriFromStarkParams {
                stark_parameters: self.stark_parameters,
            },
        ));
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
                num_elements: triton_vm::table::master_table::num_quotients(),
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
        let sample_base_ext_and_quotient_weights =
            library.import(Box::new(SampleScalarsStaticLengthDynMalloc {
                num_elements: NUM_BASE_COLUMNS + NUM_EXT_COLUMNS + NUM_QUOTIENT_SEGMENTS,
            }));
        const NUM_DEEP_CODEWORD_COMPONENTS: usize = 3;
        let deep_codeword_weights = library.import(Box::new(SampleScalarsStaticLengthDynMalloc {
            num_elements: NUM_DEEP_CODEWORD_COMPONENTS,
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
            InnerProductOfThreeRowsWithWeights::recufier_parameters(BaseElementType::Bfe),
        ));
        let inner_product_three_rows_with_weights_xfe_base = library.import(Box::new(
            InnerProductOfThreeRowsWithWeights::recufier_parameters(BaseElementType::Xfe),
        ));
        let inner_product_4_xfes = library.import(Box::new(InnerProductOfXfes { length: 4 }));
        let quotient_segment_codeword_weights_from_be_weights = triton_asm!(
            // _ *b_and_ext_cw_ws

            push {(NUM_BASE_COLUMNS + NUM_EXT_COLUMNS) * EXTENSION_DEGREE}
            add
            // _ *quotient_segment_weights
        );

        let verify_log_2_padded_height =
            if let Some(expected_log_2_padded_height) = self.log_2_padded_height {
                triton_asm!(
                    dup 0
                    push {expected_log_2_padded_height}
                    eq
                    assert
                )
            } else {
                triton_asm!()
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
            // _ remaining_rounds fri_gen fdoffset *etrow *btrow *qseg_elem *fri_revealed_idx *b_and_ext_cw_ws *deepws *oodpnts
            // Calculate `current_fri_domain_value`
            dup 3
            read_mem 1
            swap 5
            pop 1
            // _ remaining_rounds fri_gen fdoffset *etrow *btrow *qseg_elem *fri_revealed_xfe *b_and_ext_cw_ws *deepws *oodpnts fri_idx

            dup 9
            pow
            dup 8
            mul
            push -1
            mul
            hint neg_fri_domain_point = stack[0]
            // _ remaining_rounds fri_gen fdoffset *etrow *btrow *qseg_elem *fri_revealed_xfe *b_and_ext_cw_ws *deepws *oodpnts (-fdom_pnt)


            dup 3
            dup 7
            dup 9
            call {inner_product_three_rows_with_weights_bfe_base}
            hint base_and_ext_opened_row_element: Xfe = stack[0..3]
            // _ remaining_rounds fri_gen fdoffset *etrow *btrow *qseg_elem *fri_revealed_xfe *b_and_ext_cw_ws *deepws *oodpnts (-fdom_pnt) [be_opnd_elem]

            // Update `*btrow` and `*etrow` pointer values to point to previous element
            swap 10
            push {-((EXTENSION_DEGREE * NUM_EXT_COLUMNS) as i32)}
            add
            swap 10
            swap 9
            push {-(NUM_BASE_COLUMNS as i32)}
            add
            swap 9
            // _ remaining_rounds fri_gen fdoffset *etrow_prev *btrow_prev *qseg_elem *fri_revealed_xfe *b_and_ext_cw_ws *deepws *oodpnts (-fdom_pnt) [-be_opnd_elem]

            push -1
            xbmul
            hint neg_base_and_ext_opened_row_element: Xfe = stack[0..3]

            // Calculate `cuotient_curr_row_deep_value`
            dup 4
            {&OutOfDomainPoints::read_ood_point(OodPoint::CurrentRowPowNumSegments)}
            // _ remaining_rounds fri_gen fdoffset *etrow_prev *btrow_prev *qseg_elem *fri_revealed_xfe *b_and_ext_cw_ws *deepws *oodpnts (-fdom_pnt) [-be_opnd_elem] [oodp_pow_nsegs]

            dup 6
            add
            // _ remaining_rounds fri_gen fdoffset *etrow_prev *btrow_prev *qseg_elem *fri_revealed_xfe *b_and_ext_cw_ws *deepws *oodpnts (-fdom_pnt) [-be_opnd_elem] [oodp_pow_nsegs - fdom_pnt]

            xinvert
            // _ remaining_rounds fri_gen fdoffset *etrow_prev *btrow_prev *qseg_elem *fri_revealed_xfe *b_and_ext_cw_ws *deepws *oodpnts (-fdom_pnt) [-be_opnd_elem] [1/(oodp_pow_nsegs - fdom_pnt)]

            dup 9
            {&quotient_segment_codeword_weights_from_be_weights}
            dup 12
            call {inner_product_4_xfes}
            // _ remaining_rounds fri_gen fdoffset *etrow_prev *btrow_prev *qseg_elem *fri_revealed_xfe *b_and_ext_cw_ws *deepws *oodpnts (-fdom_pnt) [-be_opnd_elem] [1/(oodp_pow_nsegs - fdom_pnt)] [inner_prod]

            swap 14
            push {-((NUM_QUOTIENT_SEGMENTS * EXTENSION_DEGREE) as i32)}
            add
            swap 14
            // _ remaining_rounds fri_gen fdoffset *etrow_prev *btrow_prev *qseg_elem_prev *fri_revealed_xfe *b_and_ext_cw_ws *deepws *oodpnts (-fdom_pnt) [-be_opnd_elem] [1/(oodp_pow_nsegs - fdom_pnt)] [inner_prod]

            push -1
            xbmul
            // _ remaining_rounds fri_gen fdoffset *etrow_prev *btrow_prev *qseg_elem_prev *fri_revealed_xfe *b_and_ext_cw_ws *deepws *oodpnts (-fdom_pnt) [-be_opnd_elem] [1/(oodp_pow_nsegs - fdom_pnt)] [-inner_prod]

            push {ood_curr_row_quotient_segment_value_pointer_read}
            read_mem {EXTENSION_DEGREE}
            pop 1
            // _ remaining_rounds fri_gen fdoffset *etrow_prev *btrow_prev *qseg_elem_prev *fri_revealed_xfe *b_and_ext_cw_ws *deepws *oodpnts (-fdom_pnt) [-be_opnd_elem] [1/(oodp_pow_nsegs - fdom_pnt)] [-inner_prod] [out_of_domain_curr_row_quotient_segment_value]

            xxadd
            xxmul
            hint quot_curr_row_deep_value: XFieldElement = stack[0..3]
            // _ remaining_rounds fri_gen fdoffset *etrow_prev *btrow_prev *qseg_elem_prev *fri_revealed_xfe *b_and_ext_cw_ws *deepws *oodpnts (-fdom_pnt) [-be_opnd_elem] [(out_of_domain_curr_row_quotient_segment_value - inner_prod) / (oodp_pow_nsegs - fdom_pnt)]
            // _ remaining_rounds fri_gen fdoffset *etrow_prev *btrow_prev *qseg_elem_prev *fri_revealed_xfe *b_and_ext_cw_ws *deepws *oodpnts (-fdom_pnt) [-be_opnd_elem] [quot_curr_row_deep_value]


            /* Calculate $dv2 = quot_curr_row_deep_value * deep_codeword_weights[2]$ */
            dup 8
            push {EXTENSION_DEGREE * 2 + {EXTENSION_DEGREE - 1}} // read deep_codeword_weights[2]
            add
            read_mem {EXTENSION_DEGREE}
            pop 1
            xxmul
            // _ remaining_rounds fri_gen fdoffset *etrow_prev *btrow_prev *qseg_elem_prev *fri_revealed_xfe *b_and_ext_cw_ws *deepws *oodpnts (-fdom_pnt) [-be_opnd_elem] [quot_curr_row_deep_value * deep_codeword_weights[2]]
            // _ remaining_rounds fri_gen fdoffset *etrow_prev *btrow_prev *qseg_elem_prev *fri_revealed_xfe *b_and_ext_cw_ws *deepws *oodpnts (-fdom_pnt) [-be_opnd_elem] [dv2]

            dup 7
            {&OutOfDomainPoints::read_ood_point(OodPoint::CurrentRow)}
            // _ remaining_rounds fri_gen fdoffset *etrow_prev *btrow_prev *qseg_elem_prev *fri_revealed_xfe *b_and_ext_cw_ws *deepws *oodpnts (-fdom_pnt) [-be_opnd_elem] [dv2] [ood_point_curr_row]

            dup 9
            add
            xinvert
            // _ remaining_rounds fri_gen fdoffset *etrow_prev *btrow_prev *qseg_elem_prev *fri_revealed_xfe *b_and_ext_cw_ws *deepws *oodpnts (-fdom_pnt) [-be_opnd_elem] [dv2] [1/(ood_point_curr_row - fdom_pnt)]

            push {ood_curr_row_base_and_ext_value_pointer_read}
            read_mem {EXTENSION_DEGREE}
            pop 1
            // _ remaining_rounds fri_gen fdoffset *etrow_prev *btrow_prev *qseg_elem_prev *fri_revealed_xfe *b_and_ext_cw_ws *deepws *oodpnts (-fdom_pnt) [-be_opnd_elem] [dv2] [1/(ood_point_curr_row - fdom_pnt)] [out_of_domain_curr_row_base_and_ext_value]

            dup 11
            dup 11
            dup 11
            xxadd
            // _ remaining_rounds fri_gen fdoffset *etrow_prev *btrow_prev *qseg_elem_prev *fri_revealed_xfe *b_and_ext_cw_ws *deepws *oodpnts (-fdom_pnt) [-be_opnd_elem] [dv2] [1/(ood_point_curr_row - fdom_pnt)] [out_of_domain_curr_row_base_and_ext_value - be_opnd_elem]

            xxmul
            // _ remaining_rounds fri_gen fdoffset *etrow_prev *btrow_prev *qseg_elem_prev *fri_revealed_xfe *b_and_ext_cw_ws *deepws *oodpnts (-fdom_pnt) [-be_opnd_elem] [dv2] [(out_of_domain_curr_row_base_and_ext_value - be_opnd_elem)/(ood_point_curr_row - fdom_pnt)]

            dup 11
            push {EXTENSION_DEGREE - 1} // read deep_codeword_weights[0]
            add
            read_mem {EXTENSION_DEGREE}
            pop 1
            xxmul
            // _ remaining_rounds fri_gen fdoffset *etrow_prev *btrow_prev *qseg_elem_prev *fri_revealed_xfe *b_and_ext_cw_ws *deepws *oodpnts (-fdom_pnt) [-be_opnd_elem] [dv2] [deep_codeword_weights[0] * (out_of_domain_curr_row_base_and_ext_value - be_opnd_elem)/(ood_point_curr_row - fdom_pnt)]
            // _ remaining_rounds fri_gen fdoffset *etrow_prev *btrow_prev *qseg_elem_prev *fri_revealed_xfe *b_and_ext_cw_ws *deepws *oodpnts (-fdom_pnt) [-be_opnd_elem] [dv2] [dv0]

            xxadd
            // _ remaining_rounds fri_gen fdoffset *etrow_prev *btrow_prev *qseg_elem_prev *fri_revealed_xfe *b_and_ext_cw_ws *deepws *oodpnts (-fdom_pnt) [-be_opnd_elem] [dv2 + dv0]
            hint dv2_plus_dv0: XFieldElement = stack[0..3]

            {&swap_top_two_xfes}
            // _ remaining_rounds fri_gen fdoffset *etrow_prev *btrow_prev *qseg_elem_prev *fri_revealed_xfe *b_and_ext_cw_ws *deepws *oodpnts (-fdom_pnt) [dv2 + dv0] [-be_opnd_elem]

            push {ood_next_row_base_and_ext_value_pointer_read}
            read_mem {EXTENSION_DEGREE}
            pop 1
            xxadd
            // _ remaining_rounds fri_gen fdoffset *etrow_prev *btrow_prev *qseg_elem_prev *fri_revealed_xfe *b_and_ext_cw_ws *deepws *oodpnts (-fdom_pnt) [dv2 + dv0] [ood_next_row_be_value - be_opnd_elem]

            dup 7
            {&OutOfDomainPoints::read_ood_point(OodPoint::NextRow)}
            // _ remaining_rounds fri_gen fdoffset *etrow_prev *btrow_prev *qseg_elem_prev *fri_revealed_xfe *b_and_ext_cw_ws *deepws *oodpnts (-fdom_pnt) [dv2 + dv0] [ood_next_row_be_value - be_opnd_elem] [out_of_domain_point_next_row]

            dup 9
            add
            // _ remaining_rounds fri_gen fdoffset *etrow_prev *btrow_prev *qseg_elem_prev *fri_revealed_xfe *b_and_ext_cw_ws *deepws *oodpnts (-fdom_pnt) [dv2 + dv0] [ood_next_row_be_value - be_opnd_elem] [out_of_domain_point_next_row - fdom_pnt]

            xinvert
            xxmul
            // _ remaining_rounds fri_gen fdoffset *etrow_prev *btrow_prev *qseg_elem_prev *fri_revealed_xfe *b_and_ext_cw_ws *deepws *oodpnts (-fdom_pnt) [dv2 + dv0] [(ood_next_row_be_value - be_opnd_elem)/(out_of_domain_point_next_row - fdom_pnt)]

            dup 8
            push {EXTENSION_DEGREE + EXTENSION_DEGREE - 1} // read deep_codeword_weights[1]
            add
            read_mem {EXTENSION_DEGREE}
            pop 1
            xxmul
            // _ remaining_rounds fri_gen fdoffset *etrow_prev *btrow_prev *qseg_elem_prev *fri_revealed_xfe *b_and_ext_cw_ws *deepws *oodpnts (-fdom_pnt) [dv2 + dv0] [deep_codeword_weights[1] * (ood_next_row_be_value - be_opnd_elem)/(out_of_domain_point_next_row - fdom_pnt)]
            // _ remaining_rounds fri_gen fdoffset *etrow_prev *btrow_prev *qseg_elem_prev *fri_revealed_xfe *b_and_ext_cw_ws *deepws *oodpnts (-fdom_pnt) [dv2 + dv0] [dv1]

            xxadd
            hint deep_value: XFieldElement = stack[0..3]
            // _ remaining_rounds fri_gen fdoffset *etrow_prev *btrow_prev *qseg_elem_prev *fri_revealed_xfe *b_and_ext_cw_ws *deepws *oodpnts (-fdom_pnt) [dv2 + dv0 + dv1]
            // _ remaining_rounds fri_gen fdoffset *etrow_prev *btrow_prev *qseg_elem_prev *fri_revealed_xfe *b_and_ext_cw_ws *deepws *oodpnts (-fdom_pnt) [deep_value]

            swap 1
            swap 2
            swap 3
            // _ remaining_rounds fri_gen fdoffset *etrow_prev *btrow_prev *qseg_elem_prev *fri_revealed_xfe *b_and_ext_cw_ws *deepws *oodpnts [deep_value] (-fdom_pnt)

            swap 7
            read_mem {EXTENSION_DEGREE}
            swap 10
            pop 1
            // _ remaining_rounds fri_gen fdoffset *etrow_prev *btrow_prev *qseg_elem_prev *fri_revealed_idx_prev *b_and_ext_cw_ws *deepws *oodpnts [deep_value] [fri_revealed_value]

            {&assert_top_two_xfes_eq}

            // _ remaining_rounds fri_gen fdoffset *etrow_prev *btrow_prev *qseg_elem_prev *fri_revealed_idx_prev *b_and_ext_cw_ws *deepws *oodpnts
        );
        let main_loop = triton_asm!(
            // The loop goes from last index to 1st index
            // Invariant: _ num_cw_chks fri_gen fdoffset *etrow *btrow *qseg_elem *fri_revealed_elem *b_and_ext_cw_ws *deepws *oodpnts
            {main_loop_label}:
                // test end-condition
                dup 9
                push 0
                eq
                skiz
                    return

                {&main_loop_body}

                // Update counter
                swap 9
                push -1
                add
                swap 9
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

                {&verify_log_2_padded_height}
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
                call {sample_base_ext_and_quotient_weights}
                hint base_ext_and_quot_codeword_weights = stack[0]
                // _ *b_mr *p_iter *oodpnts *fri *e_mr *odd_brow_nxt *quot_mr *ood_erow_nxt *ood_brow_curr *ood_erow_curr *b_and_ext_cw_ws

                call {deep_codeword_weights}
                hint deep_codeword_weights = stack[0]
                // _ *b_mr *p_iter *oodpnts *fri *e_mr *odd_brow_nxt *quot_mr *ood_erow_nxt *ood_brow_curr *ood_erow_curr *b_and_ext_cw_ws *deepws


                swap 11
                // _ *deepws *p_iter *oodpnts *fri *e_mr *odd_brow_nxt *quot_mr *ood_erow_nxt *ood_brow_curr *ood_erow_curr *b_and_ext_cw_ws *b_mr


                /* FRI */
                // We need the `fri` data structure for field values later, so we preserve its pointer on the stack
                dup 10
                dup 9
                call {fri_verify}
                // _ *deepws *p_iter *oodpnts *fri *e_mr *odd_brow_nxt *quot_mr *ood_erow_nxt *ood_brow_curr *ood_erow_curr *b_and_ext_cw_ws *b_mr *fri_revealed


                /* Dequeue base-table rows and verify against its Merkle root */
                dup 11
                call {next_as_basetablerows}
                // _ *deepws *p_iter *oodpnts *fri *e_mr *odd_brow_nxt *quot_mr *ood_erow_nxt *ood_brow_curr *ood_erow_curr *b_and_ext_cw_ws *b_mr *fri_revealed *btrows


                dup 10
                {&num_collinearity_checks_field}
                read_mem 1
                pop 1
                // _ *deepws *p_iter *oodpnts *fri *e_mr *odd_brow_nxt *quot_mr *ood_erow_nxt *ood_brow_curr *ood_erow_curr *b_and_ext_cw_ws *b_mr *fri_revealed *btrows num_colli

                push 2
                mul
                // _ *deepws *p_iter *oodpnts *fri *e_mr *odd_brow_nxt *quot_mr *ood_erow_nxt *ood_brow_curr *ood_erow_curr *b_and_ext_cw_ws *b_mr *fri_revealed *btrows num_cw_chks

                dup 11
                {&domain_length_field}
                read_mem 1
                pop 1
                // _ *deepws *p_iter *oodpnts *fri *e_mr *odd_brow_nxt *quot_mr *ood_erow_nxt *ood_brow_curr *ood_erow_curr *b_and_ext_cw_ws *b_mr *fri_revealed *btrows num_cw_chks dom_len

                log_2_floor
                // _ *deepws *p_iter *oodpnts *fri *e_mr *odd_brow_nxt *quot_mr *ood_erow_nxt *ood_brow_curr *ood_erow_curr *b_and_ext_cw_ws *b_mr *fri_revealed *btrows num_cw_chks mt_height

                swap 1
                swap 2
                swap 3
                swap 4
                // _ *deepws *p_iter *oodpnts *fri *e_mr *odd_brow_nxt *quot_mr *ood_erow_nxt *ood_brow_curr *ood_erow_curr *b_and_ext_cw_ws *fri_revealed *btrows num_cw_chks mt_height *b_mr

                dup 4
                // _ *deepws *p_iter *oodpnts *fri *e_mr *odd_brow_nxt *quot_mr *ood_erow_nxt *ood_brow_curr *ood_erow_curr *b_and_ext_cw_ws *fri_revealed *btrows num_cw_chks mt_height *b_mr *fri_revealed

                dup 4
                // _ *deepws *p_iter *oodpnts *fri *e_mr *odd_brow_nxt *quot_mr *ood_erow_nxt *ood_brow_curr *ood_erow_curr *b_and_ext_cw_ws *fri_revealed *btrows num_cw_chks mt_height *b_mr *fri_revealed *btrows

                call {verify_base_table_rows}
                // _ *deepws *p_iter *oodpnts *fri *e_mr *odd_brow_nxt *quot_mr *ood_erow_nxt *ood_brow_curr *ood_erow_curr *b_and_ext_cw_ws *fri_revealed *btrows


                /* Dequeue and ignore base-table's authentication path */
                dup 11
                call {next_as_authentication_path}
                pop 1
                // _ *deepws *p_iter *oodpnts *fri *e_mr *odd_brow_nxt *quot_mr *ood_erow_nxt *ood_brow_curr *ood_erow_curr *b_and_ext_cw_ws *fri_revealed *btrows

                swap 8
                // _ *deepws *p_iter *oodpnts *fri *btrows *odd_brow_nxt *quot_mr *ood_erow_nxt *ood_brow_curr *ood_erow_curr *b_and_ext_cw_ws *fri_revealed *e_mr


                /* Dequeue ext-table rows and verify against its Merkle root */
                dup 11
                call {next_as_exttablerows}
                // _ *deepws *p_iter *oodpnts *fri *btrows *odd_brow_nxt *quot_mr *ood_erow_nxt *ood_brow_curr *ood_erow_curr *b_and_ext_cw_ws *fri_revealed *e_mr *etrows

                swap 1
                // _ *deepws *p_iter *oodpnts *fri *btrows *odd_brow_nxt *quot_mr *ood_erow_nxt *ood_brow_curr *ood_erow_curr *b_and_ext_cw_ws *fri_revealed *etrows *e_mr

                dup 10
                {&num_collinearity_checks_field}
                read_mem 1
                pop 1
                push 2
                mul
                dup 11
                {&domain_length_field}
                read_mem 1
                pop 1
                log_2_floor
                // _ *deepws *p_iter *oodpnts *fri *btrows *odd_brow_next *quot_mr *ood_erow_nxt *ood_brow_curr *ood_erow_curr *b_and_ext_cw_ws *fri_revealed *etrows *e_mr num_cw_chks mt_height

                swap 1
                swap 2
                dup 4
                dup 4
                // _ *deepws *p_iter *oodpnts *fri *btrows *odd_brow_next *quot_mr *ood_erow_nxt *ood_brow_curr *ood_erow_curr *b_and_ext_cw_ws *fri_revealed *etrows num_cw_chks mt_height *e_mr *fri_revealed *etrows

                call {verify_extension_table_rows}
                // _ *deepws *p_iter *oodpnts *fri *btrows *odd_brow_next *quot_mr *ood_erow_nxt *ood_brow_curr *ood_erow_curr *b_and_ext_cw_ws *fri_revealed *etrows


                /* Dequeue and ignore ext-table's authentication path */
                dup 11
                call {next_as_authentication_path}
                pop 1

                swap 6
                // _ *deepws *p_iter *oodpnts *fri *btrows *odd_brow_next *etrows *ood_erow_nxt *ood_brow_curr *ood_erow_curr *b_and_ext_cw_ws *fri_revealed *quot_mr


                /* Dequeue quotient-table rows and verify against its Merkle root */
                dup 11
                call {next_as_quotient_segment_elements}
                swap 1
                dup 10
                {&num_collinearity_checks_field}
                read_mem 1
                pop 1
                push 2
                mul
                dup 11
                {&domain_length_field}
                read_mem 1
                pop 1
                log_2_floor
                // _ *deepws *p_iter *oodpnts *fri *btrows *odd_brow_next *etrows *ood_erow_nxt *ood_brow_curr *ood_erow_curr *b_and_ext_cw_ws *fri_revealed *qseg_elems *quot_mr num_cw_chks mt_height

                swap 1
                swap 2
                // _ *deepws *p_iter *oodpnts *fri *btrows *odd_brow_next *etrows *ood_erow_nxt *ood_brow_curr *ood_erow_curr *b_and_ext_cw_ws *fri_revealed *qseg_elems num_cw_chks mt_height *quot_mr

                dup 4
                dup 4
                // _ *deepws *p_iter *oodpnts *fri *btrows *odd_brow_next *etrows *ood_erow_nxt *ood_brow_curr *ood_erow_curr *b_and_ext_cw_ws *fri_revealed *qseg_elems num_cw_chks mt_height *quot_mr *fri_revealed *qseg_elems

                call {verify_quotient_segments}
                // _ *deepws *p_iter *oodpnts *fri *btrows *odd_brow_next *etrows *ood_erow_nxt *ood_brow_curr *ood_erow_curr *b_and_ext_cw_ws *fri_revealed *qseg_elems

                /* Various length asserts */
                // assert!(num_combination_codeword_checks == quotient_segment_elements.len());
                dup 9
                {&num_collinearity_checks_field}
                read_mem 1
                pop 1
                push 2
                mul
                hint num_combination_codeword_checks = stack[0]
                // _ *deepws *p_iter *oodpnts *fri *btrows *odd_brow_next *etrows *ood_erow_nxt *ood_brow_curr *ood_erow_curr *b_and_ext_cw_ws *fri_revealed *qseg_elems num_cw_chks

                // assert!(num_combination_codeword_checks == revealed_fri_indices_and_elements.len())
                dup 2
                read_mem 1
                pop 1
                dup 1
                eq
                assert
                // _ *deepws *p_iter *oodpnts *fri *btrows *odd_brow_next *etrows *ood_erow_nxt *ood_brow_curr *ood_erow_curr *b_and_ext_cw_ws *fri_revealed *qseg_elems num_cw_chks

                // assert!(num_combination_codeword_checks == base_table_rows.len());
                dup 9
                read_mem 1
                pop 1
                dup 1
                eq
                assert

                // assert!(num_combination_codeword_checks == ext_table_rows.len())
                dup 7
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
                // _ *deepws *p_iter *oodpnts *fri *btrows *odd_brow_next *etrows *ood_erow_nxt *ood_brow_curr *ood_erow_curr *b_and_ext_cw_ws *fri_revealed *qseg_elems num_cw_chks

                /* Clean up stack */
                swap 13
                swap 12
                pop 1
                // _ num_cw_chks *deepws *oodpnts *fri *btrows *odd_brow_next *etrows *ood_erow_nxt *ood_brow_curr *ood_erow_curr *b_and_ext_cw_ws *fri_revealed *qseg_elems

                /* Sum out-of-domain values */
                // Goal for stack: _ *b_and_ext_cw_ws *ood_brow_curr *ood_erow_curr

                dup 2
                swap 2
                // _ num_cw_chks *deepws *oodpnts *fri *btrows *odd_brow_next *etrows *ood_erow_nxt *ood_brow_curr *ood_erow_curr *b_and_ext_cw_ws *b_and_ext_cw_ws *qseg_elems *fri_revealed

                swap 5
                swap 1
                swap 4
                // _ num_cw_chks *deepws *oodpnts *fri *btrows *odd_brow_next *etrows *ood_erow_nxt *fri_revealed *qseg_elems *b_and_ext_cw_ws *b_and_ext_cw_ws *ood_brow_curr *ood_erow_curr

                call {inner_product_three_rows_with_weights_xfe_base}
                hint out_of_domain_curr_row_base_and_ext_value: XFieldElement = stack[0..3]
                // _ num_cw_chks *deepws *oodpnts *fri *btrows *odd_brow_next *etrows *ood_erow_nxt *fri_revealed *qseg_elems *b_and_ext_cw_ws [ood_curr_beval]

                push {ood_curr_row_base_and_ext_value_pointer_write}
                write_mem {EXTENSION_DEGREE}
                pop 1
                // _ num_cw_chks *deepws *oodpnts *fri *btrows *odd_brow_next *etrows *ood_erow_nxt *fri_revealed *qseg_elems *b_and_ext_cw_ws

                // Goal: _ *b_and_ext_cw_ws *odd_brow_next *ood_erow_nxt
                dup 0
                swap 2
                swap 6
                swap 1
                swap 4
                // _ num_cw_chks *deepws *oodpnts *fri *btrows *qseg_elems *etrows *b_and_ext_cw_ws *fri_revealed *b_and_ext_cw_ws *odd_brow_next *ood_erow_nxt

                call {inner_product_three_rows_with_weights_xfe_base}
                hint out_of_domain_next_row_base_and_ext_value: XFieldElement = stack[0..3]
                // _ num_cw_chks *deepws *oodpnts *fri *btrows *qseg_elems *etrows *b_and_ext_cw_ws *fri_revealed [ood_next_value]

                push {ood_next_row_base_and_ext_value_pointer_write}
                write_mem {EXTENSION_DEGREE}
                pop 1
                // _ num_cw_chks *deepws *oodpnts *fri *btrows *qseg_elems *etrows *b_and_ext_cw_ws *fri_revealed

                // Goal: _ *quotient_segment_codeword_weights *ood_curr_row_quot_segments
                dup 1
                {&quotient_segment_codeword_weights_from_be_weights}
                // _ num_cw_chks *deepws *oodpnts *fri *btrows *qseg_elems *etrows *b_and_ext_cw_ws *fri_revealed *quotient_segment_codeword_weights

                push {out_of_domain_curr_row_quot_segments_pointer_pointer}
                read_mem 1
                pop 1
                // _ num_cw_chks *deepws *oodpnts *fri *btrows *qseg_elems *etrows *b_and_ext_cw_ws *fri_revealed *quotient_segment_codeword_weights *ood_curr_row_quot_segments

                call {inner_product_4_xfes}
                hint out_of_domain_curr_row_quotient_segment_value: XFieldElement = stack[0..3]
                // _ num_cw_chks *deepws *oodpnts *fri *btrows *qseg_elems *etrows *b_and_ext_cw_ws *fri_revealed [out_of_domain_curr_row_quotient_segment_value]

                push {ood_curr_row_quotient_segment_value_pointer_write}
                write_mem {EXTENSION_DEGREE}
                pop 1
                // _ num_cw_chks *deepws *oodpnts *fri *btrows *qseg_elems *etrows *b_and_ext_cw_ws *fri_revealed

                // Put fri domain generator and domain offset on stack
                swap 5
                dup 0
                {&domain_offset_field}
                read_mem 1
                pop 1
                hint fri_domain_offset = stack[0]
                // _ num_cw_chks *deepws *oodpnts *fri_revealed *btrows *qseg_elems *etrows *b_and_ext_cw_ws *fri fdoffset

                swap 1
                {&domain_generator_field}
                read_mem 1
                pop 1
                hint fri_domain_gen = stack[0]
                // _ num_cw_chks *deepws *oodpnts *fri_revealed *btrows *qseg_elems *etrows *b_and_ext_cw_ws fdoffset fri_gen

                // adjust relevant pointers to point to last word in sequence, as they are traversed
                // high-to-low in the main loop

                // Adjust *fri_revealed (list) to point to last word
                swap 6
                dup 9
                push {EXTENSION_DEGREE + 1} // size of element in `fri_revealed` list
                mul
                add
                hint fri_revealed_elem = stack[0]
                swap 6

                // Adjust *btrows (list) to point to last element
                swap 5
                push 1
                add
                push {NUM_BASE_COLUMNS} // size of element of base row list
                dup 10
                push -1
                add
                mul
                add
                hint base_table_row = stack[0]
                swap 5

                // Adjust *etrows (list) to point to last element
                swap 3
                push 1
                add
                push {NUM_EXT_COLUMNS * EXTENSION_DEGREE} // size of element of ext row list
                dup 10
                push -1
                add
                mul
                add
                hint ext_table_row = stack[0]
                swap 3
                // _ num_cw_chks *deepws *oodpnts *fri_revealed *btrows *qseg_elems *etrows *b_and_ext_cw_ws fdoffset fri_gen

                // Adjust *qseg_elems to point to last element
                swap 4
                push 1
                add
                push {NUM_QUOTIENT_SEGMENTS * EXTENSION_DEGREE} // size of element of quot row list
                dup 10
                push -1
                add
                mul
                add
                hint quotient_segment_elem = stack[0]
                swap 4

                // _ num_cw_chks *deepws *oodpnts *fri_revealed_last_elem *btrows_last_elem *qseg_elems_last_elem *etrows_last_elem *b_and_ext_cw_ws fdoffset fri_gen

                /* Reorganize stack for main-loop, to keep all necessary words accessible without spilling */
                swap 8
                swap 1
                swap 7
                swap 3
                swap 6
                swap 3
                // _ num_cw_chks fri_gen fdoffset *etrows_last_elem *btrows_last_elem *qseg_elems_last_elem *fri_revealed_last_elem *b_and_ext_cw_ws *deepws *oodpnts

                call {main_loop_label}
                // _ num_cw_chks fri_gen fdoffset *etrows_last_elem *btrows_last_elem *qseg_elems_last_elem *fri_revealed_last_elem *b_and_ext_cw_ws *deepws *oodpnts

                /* Cleanup stack */
                pop 5 pop 5

                return

                {&main_loop}
        )
    }
}

#[cfg(test)]
pub mod tests {
    use itertools::Itertools;
    use tests::fri::test_helpers::extract_fri_proof;
    use triton_vm::stark::StarkProofStream;

    use crate::execute_test;
    use crate::recufier::claim::shared::insert_claim_into_static_memory;
    use crate::recufier::vm_proof_iter::shared::insert_default_proof_iter_into_memory;

    use super::*;

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
            stark_parameters: Stark::default(),
            log_2_padded_height: None,
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

    #[test]
    fn verify_tvm_proof_factorial_program() {
        const FACTORIAL_ARGUMENT: u32 = 3;
        let factorial_program = factorial_program_with_io();
        let stark = Stark::default();
        let (mut non_determinism, claim_for_proof, inner_padded_height) =
            non_determinism_claim_and_padded_height(
                &factorial_program,
                &[FACTORIAL_ARGUMENT.into()],
                NonDeterminism::default(),
                &stark,
            );

        // Insert `claim` into standard memory, since that's how the interface is defined.
        // In any real setting, you probably want to use the above snippet as an inner function,
        // and instead call an entrypoint that puts the claim into memory and passes it as a pointer
        // to the above snippet. Same goes for the proof-iterator.
        let (claim_pointer, claim_size) =
            insert_claim_into_static_memory(&mut non_determinism.ram, claim_for_proof);

        let proof_iter_pointer = BFieldElement::new(1 << 31);
        insert_default_proof_iter_into_memory(&mut non_determinism.ram, proof_iter_pointer);

        let snippet = StarkVerify {
            stark_parameters: stark,
            log_2_padded_height: None,
        };
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

        println!(
            "TASM-verifier of factorial({FACTORIAL_ARGUMENT}):\n
            clock cycle count: {}.\n
             Inner padded height was: {}",
            final_tasm_state.cycle_count, inner_padded_height,
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

    fn nd_from_proof(
        stark: &Stark,
        claim: &Claim,
        proof: Proof,
    ) -> (NonDeterminism<BFieldElement>, usize) {
        let fri = stark.derive_fri(proof.padded_height().unwrap()).unwrap();
        let proof_stream = StarkProofStream::try_from(&proof).unwrap();
        let proof_extraction = extract_fri_proof(&proof_stream, claim, stark);
        let tasm_lib_fri: fri::verify::FriVerify = fri.into();
        let fri_proof_digests =
            tasm_lib_fri.extract_digests_required_for_proving(&proof_extraction.fri_proof_stream);
        let padded_height = proof.padded_height().unwrap();
        let Proof(raw_proof) = proof;
        let ram = raw_proof
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

    pub fn non_determinism_claim_and_padded_height(
        inner_program: &Program,
        inner_public_input: &[BFieldElement],
        inner_nondeterminism: NonDeterminism<BFieldElement>,
        stark: &Stark,
    ) -> (
        NonDeterminism<BFieldElement>,
        triton_vm::proof::Claim,
        usize,
    ) {
        println!("Generating proof for non-determinism");

        let (aet, inner_output) = inner_program
            .trace_execution(inner_public_input.into(), inner_nondeterminism.clone())
            .unwrap();
        let claim = Claim {
            program_digest: inner_program.hash::<Tip5>(),
            input: inner_public_input.to_vec(),
            output: inner_output,
        };

        let proof = stark.prove(&claim, &aet, &mut None).unwrap();
        println!("Done generating proof for non-determinism");

        assert!(
            stark.verify(&claim, &proof, &mut None).is_ok(),
            "Proof from TVM must verify through TVM"
        );

        let (non_determinism, padded_height) = nd_from_proof(stark, &claim, proof);

        (non_determinism, claim, padded_height)
    }
}

#[cfg(test)]
mod benches {
    use std::cell::RefCell;
    use std::rc::Rc;

    use benches::tests::{factorial_program_with_io, non_determinism_claim_and_padded_height};

    use crate::linker::execute_bench;
    use crate::memory::encode_to_memory;
    use crate::snippet_bencher::{write_benchmarks, BenchmarkCase, NamedBenchmarkResult};
    use crate::{
        linker::link_for_isolated_run,
        recufier::vm_proof_iter::shared::insert_default_proof_iter_into_memory,
    };

    use super::*;

    #[test]
    fn benchmark_small() {
        benchmark_verifier(3, 1 << 8);
        benchmark_verifier(40, 1 << 9);
    }

    #[ignore = "Intended to generate data about verifier table heights as a function of inner padded
       height Make sure to run with `RUSTFLAGS=\"-C opt-level=3 -C debug-assertions=no`"]
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
            benchmark_verifier(fact_arg, expected_inner_padded_height);
        }
    }

    fn benchmark_verifier(factorial_argument: u32, expected_inner_padded_height: usize) {
        let factorial_program = factorial_program_with_io();
        let stark = Stark::default();
        let (mut non_determinism, claim_for_proof, inner_padded_height) =
            non_determinism_claim_and_padded_height(
                &factorial_program,
                &[bfe!(factorial_argument)],
                NonDeterminism::default(),
                &stark,
            );

        let claim_pointer = BFieldElement::new(1 << 30);
        encode_to_memory(&mut non_determinism.ram, claim_pointer, claim_for_proof);

        let proof_iter_pointer = BFieldElement::new(1 << 31);
        insert_default_proof_iter_into_memory(&mut non_determinism.ram, proof_iter_pointer);

        assert_eq!(expected_inner_padded_height, inner_padded_height);

        let snippet = StarkVerify {
            stark_parameters: stark,
            log_2_padded_height: None,
        };

        let stack = [
            snippet.init_stack_for_isolated_run(),
            vec![claim_pointer, proof_iter_pointer],
        ]
        .concat();
        let code = link_for_isolated_run(Rc::new(RefCell::new(snippet.clone())));
        let benchmark = execute_bench(&code, &stack, vec![], non_determinism, None);
        let benchmark = NamedBenchmarkResult {
            name: format!("{}_{}", snippet.entrypoint(), inner_padded_height),
            benchmark_result: benchmark,
            case: BenchmarkCase::CommonCase,
        };

        write_benchmarks(vec![benchmark]);
    }
}
