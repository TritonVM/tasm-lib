use itertools::Itertools;
use triton_vm::challenges::Challenges;
use triton_vm::prelude::*;
use triton_vm::proof_item::ProofItemVariant;
use triton_vm::proof_stream::ProofStream;
use triton_vm::table::NUM_QUOTIENT_SEGMENTS;
use triton_vm::table::master_table::MasterAuxTable;
use triton_vm::table::master_table::MasterMainTable;
use twenty_first::math::x_field_element::EXTENSION_DEGREE;
use twenty_first::prelude::MerkleTreeInclusionProof;

use super::master_table::air_constraint_evaluation::AirConstraintEvaluation;
use super::master_table::air_constraint_evaluation::MemoryLayout;
use crate::arithmetic::bfe::primitive_root_of_unity::PrimitiveRootOfUnity;
use crate::array::horner_evaluation::HornerEvaluation;
use crate::array::inner_product_of_three_rows_with_weights::InnerProductOfThreeRowsWithWeights;
use crate::array::inner_product_of_three_rows_with_weights::MainElementType;
use crate::array::inner_product_of_xfes::InnerProductOfXfes;
use crate::field;
use crate::hashing::algebraic_hasher::sample_scalar_one::SampleScalarOne;
use crate::hashing::algebraic_hasher::sample_scalars_static_length_dyn_malloc::SampleScalarsStaticLengthDynMalloc;
use crate::prelude::*;
use crate::verifier::challenges;
use crate::verifier::claim::instantiate_fiat_shamir_with_claim::InstantiateFiatShamirWithClaim;
use crate::verifier::claim::shared::claim_type;
use crate::verifier::fri;
use crate::verifier::fri::verify::FriSnippet;
use crate::verifier::fri::verify::FriVerify;
use crate::verifier::master_table::divide_out_zerofiers::DivideOutZerofiers;
use crate::verifier::master_table::verify_table_rows::ColumnType;
use crate::verifier::master_table::verify_table_rows::VerifyTableRows;
use crate::verifier::out_of_domain_points::OodPoint;
use crate::verifier::out_of_domain_points::OutOfDomainPoints;
use crate::verifier::vm_proof_iter::dequeue_next_as::DequeueNextAs;
use crate::verifier::vm_proof_iter::drop::Drop;
use crate::verifier::vm_proof_iter::new::New;

pub(crate) const NUM_PROOF_ITEMS_PER_FRI_ROUND: usize = 2;
pub(crate) const NUM_PROOF_ITEMS_EXCLUDING_FRI: usize = 15;

/// Verify a STARK proof.
///
/// Verify a STARK proof located in memory. Assumes the nondeterministic digests
/// stream has been updated with the digests extracted from the proof using
/// [`update_nondeterminism`](Self::update_nondeterminism). Crashes the VM if the
/// proof is invalid.
///
/// Stack signature:
///  - BEFORE: _ *claim *proof
///  - AFTER:  _
#[derive(Debug, Copy, Clone)]
pub struct StarkVerify {
    stark: Stark,
    memory_layout: MemoryLayout,
}

impl StarkVerify {
    const LOG2_PADDED_HEIGHT_TOO_LARGE: i128 = 238;

    pub fn new_with_static_layout(stark: Stark) -> Self {
        Self {
            stark,
            memory_layout: MemoryLayout::conventional_static(),
        }
    }

    pub fn new_with_dynamic_layout(stark: Stark) -> Self {
        Self {
            stark,
            memory_layout: MemoryLayout::conventional_dynamic(),
        }
    }

    /// The number of nondeterministic digests that will be
    /// consumed when this snippet verifies the given proof.
    pub fn number_of_nondeterministic_digests_consumed(&self, proof: &Proof) -> usize {
        const NUM_FULL_DOMAIN_AUTH_PATHS: usize = 4;

        let padded_height = proof.padded_height().unwrap();
        let fri_params = self.stark.fri(padded_height).unwrap();
        let num_fri_rounds = fri_params.num_rounds();

        let mut j = 0;
        let mut tree_height: usize = fri_params.domain.len().ilog2().try_into().unwrap();

        let mut acc = NUM_FULL_DOMAIN_AUTH_PATHS * tree_height * fri_params.num_collinearity_checks;
        while j < num_fri_rounds {
            acc += fri_params.num_collinearity_checks * tree_height;
            j += 1;
            tree_height -= 1;
        }

        acc
    }

    /// The number of nondeterministic individual tokens that will be consumed when
    /// this snippet verifies the given (claim, proof) pair.
    // Right now this number is zero, but that might change in the future.
    pub fn number_of_nondeterministic_tokens_consumed(
        &self,
        _proof: &Proof,
        _claim: &Claim,
    ) -> usize {
        0
    }

    /// Prepares the non-determinism for verifying a STARK proof. Specifically,
    /// extracts the digests for traversing authentication paths and appends them
    /// to nondeterministic digests. Leaves memory and individual tokens intact.
    pub fn update_nondeterminism(
        &self,
        nondeterminism: &mut NonDeterminism,
        proof: &Proof,
        claim: &Claim,
    ) {
        nondeterminism
            .digests
            .append(&mut self.extract_nondeterministic_digests(proof, claim));
    }

    fn extract_nondeterministic_digests(&self, proof: &Proof, claim: &Claim) -> Vec<Digest> {
        const NUM_DEEP_CODEWORD_COMPONENTS: usize = 3;

        fn extract_paths<R: BFieldCodec>(
            indices: Vec<usize>,
            leaf_preimages: Vec<R>,
            tree_height: u32,
            authentication_structure: Vec<Digest>,
        ) -> Vec<Vec<Digest>> {
            let indexed_leafs = indices
                .into_iter()
                .zip(leaf_preimages.iter().map(Tip5::hash))
                .collect();
            MerkleTreeInclusionProof {
                tree_height,
                indexed_leafs,
                authentication_structure,
            }
            .into_authentication_paths()
            .unwrap()
        }

        // We do need to carefully update the sponge state because otherwise
        // we end up sampling indices that generate different authentication
        // paths.
        let mut proof_stream = ProofStream::try_from(proof).unwrap();
        proof_stream.alter_fiat_shamir_state_with(claim);
        let log2_padded_height = proof_stream
            .dequeue()
            .unwrap()
            .try_into_log2_padded_height()
            .unwrap();

        // Main-table Merkle root
        let _main_table_root = proof_stream
            .dequeue()
            .unwrap()
            .try_into_merkle_root()
            .unwrap();

        // Auxiliary challenge weights
        let _challenges = proof_stream.sample_scalars(Challenges::SAMPLE_COUNT);

        // Auxiliary-table Merkle root
        let _aux_mt_root = proof_stream
            .dequeue()
            .unwrap()
            .try_into_merkle_root()
            .unwrap();

        // Quotient codeword weights
        proof_stream.sample_scalars(MasterAuxTable::NUM_CONSTRAINTS);

        // Quotient codeword Merkle root
        let _quotient_root = proof_stream
            .dequeue()
            .unwrap()
            .try_into_merkle_root()
            .unwrap();

        // Out-of-domain point current row
        let _out_of_domain_point_curr_row = proof_stream.sample_scalars(1);

        // Five out-of-domain values
        proof_stream
            .dequeue()
            .unwrap()
            .try_into_out_of_domain_main_row()
            .unwrap();
        proof_stream
            .dequeue()
            .unwrap()
            .try_into_out_of_domain_aux_row()
            .unwrap();
        proof_stream
            .dequeue()
            .unwrap()
            .try_into_out_of_domain_main_row()
            .unwrap();
        proof_stream
            .dequeue()
            .unwrap()
            .try_into_out_of_domain_aux_row()
            .unwrap();
        proof_stream
            .dequeue()
            .unwrap()
            .try_into_out_of_domain_quot_segments()
            .unwrap();

        // `beqd_weights`
        proof_stream.sample_scalars(
            MasterMainTable::NUM_COLUMNS
                + MasterAuxTable::NUM_COLUMNS
                + NUM_QUOTIENT_SEGMENTS
                + NUM_DEEP_CODEWORD_COMPONENTS,
        );

        // FRI digests
        let padded_height = 1 << log2_padded_height;
        let fri = self.stark.fri(padded_height).unwrap();
        let fri_proof_stream = proof_stream.clone();
        let fri_verify_result = fri.verify(&mut proof_stream).unwrap();
        let indices = fri_verify_result.iter().map(|(i, _)| *i).collect_vec();
        let tree_height = fri.domain.len().ilog2();
        let fri_digests =
            FriVerify::from(fri).extract_digests_required_for_proving(&fri_proof_stream);

        // main
        let main_table_rows = proof_stream
            .dequeue()
            .unwrap()
            .try_into_master_main_table_rows()
            .unwrap();
        let main_authentication_structure = proof_stream
            .dequeue()
            .unwrap()
            .try_into_authentication_structure()
            .unwrap();
        let main_tree_auth_paths = extract_paths(
            indices.clone(),
            main_table_rows,
            tree_height,
            main_authentication_structure,
        );

        // aux
        let aux_table_rows = proof_stream
            .dequeue()
            .unwrap()
            .try_into_master_aux_table_rows()
            .unwrap();
        let aux_authentication_structure = proof_stream
            .dequeue()
            .unwrap()
            .try_into_authentication_structure()
            .unwrap();
        let aux_tree_auth_paths = extract_paths(
            indices.clone(),
            aux_table_rows,
            tree_height,
            aux_authentication_structure,
        );

        // quotient
        let quot_table_rows = proof_stream
            .dequeue()
            .unwrap()
            .try_into_quot_segments_elements()
            .unwrap();
        let quot_authentication_structure = proof_stream
            .dequeue()
            .unwrap()
            .try_into_authentication_structure()
            .unwrap();
        let quot_tree_auth_paths = extract_paths(
            indices,
            quot_table_rows,
            tree_height,
            quot_authentication_structure,
        );

        let stark_digests = [
            main_tree_auth_paths,
            aux_tree_auth_paths,
            quot_tree_auth_paths,
        ]
        .concat()
        .concat();

        [fri_digests, stark_digests].concat()
    }
}

impl BasicSnippet for StarkVerify {
    fn parameters(&self) -> Vec<(DataType, String)> {
        let claim_type = DataType::StructRef(claim_type());
        vec![
            (claim_type, "claim".to_string()),
            (DataType::VoidPointer, "*proof".to_string()),
        ]
    }

    fn return_values(&self) -> Vec<(DataType, String)> {
        vec![]
    }

    fn entrypoint(&self) -> String {
        let memory_layout_category = self.memory_layout.label_friendly_name();
        format!("tasmlib_verifier_stark_verify_{memory_layout_category}")
    }

    fn code(&self, library: &mut Library) -> Vec<LabelledInstruction> {
        const NUM_DEEP_CODEWORD_COMPONENTS: usize = 3;
        const NUM_OOD_ROWS_WO_QUOTIENT: u32 = 4;

        fn fri_snippet() -> FriSnippet {
            FriSnippet {
                #[cfg(test)]
                test_instance: FriVerify::dummy(),
            }
        }

        let entrypoint = self.entrypoint();

        let proof_to_vm_proof_iter = library.import(Box::new(New));
        let drop_vm_proof_iter = library.import(Box::new(Drop));

        let ood_curr_row_main_and_aux_value_pointer_alloc =
            library.kmalloc(EXTENSION_DEGREE.try_into().unwrap());
        let ood_next_row_main_and_aux_value_pointer_alloc =
            library.kmalloc(EXTENSION_DEGREE.try_into().unwrap());
        let ood_curr_row_quotient_segment_value_pointer_alloc =
            library.kmalloc(EXTENSION_DEGREE.try_into().unwrap());

        let out_of_domain_curr_row_quot_segments_pointer_alloc = library.kmalloc(1);

        let instantiate_fiat_shamir_with_claim =
            library.import(Box::new(InstantiateFiatShamirWithClaim));
        let next_as_log_2_padded_height = library.import(Box::new(DequeueNextAs {
            proof_item: ProofItemVariant::Log2PaddedHeight,
        }));
        let next_as_merkleroot = library.import(Box::new(DequeueNextAs {
            proof_item: ProofItemVariant::MerkleRoot,
        }));
        let next_as_outofdomainmainrow = library.import(Box::new(DequeueNextAs {
            proof_item: ProofItemVariant::OutOfDomainMainRow,
        }));
        let next_as_outofdomainauxrow = library.import(Box::new(DequeueNextAs {
            proof_item: ProofItemVariant::OutOfDomainAuxRow,
        }));
        let next_as_outofdomainquotientsegments = library.import(Box::new(DequeueNextAs {
            proof_item: ProofItemVariant::OutOfDomainQuotientSegments,
        }));
        let next_as_maintablerows = library.import(Box::new(DequeueNextAs {
            proof_item: ProofItemVariant::MasterMainTableRows,
        }));
        let next_as_authentication_path = library.import(Box::new(DequeueNextAs {
            proof_item: ProofItemVariant::AuthenticationStructure,
        }));
        let next_as_auxtablerows = library.import(Box::new(DequeueNextAs {
            proof_item: ProofItemVariant::MasterAuxTableRows,
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

        let fri_verify = library.import(Box::new(fri_snippet()));

        let get_challenges = library.import(Box::new(
            challenges::new_generic_dyn_claim::NewGenericDynClaim::tvm_challenges(
                self.memory_layout.challenges_pointer(),
            ),
        ));
        let sample_quotient_codeword_weights =
            library.import(Box::new(SampleScalarsStaticLengthDynMalloc {
                num_elements: MasterAuxTable::NUM_CONSTRAINTS,
            }));
        let domain_generator = library.import(Box::new(PrimitiveRootOfUnity));
        let sample_scalar_one = library.import(Box::new(SampleScalarOne));
        let calculate_out_of_domain_points = library.import(Box::new(OutOfDomainPoints));
        let divide_out_zerofiers = library.import(Box::new(DivideOutZerofiers));
        let inner_product_quotient_summands = library.import(Box::new(InnerProductOfXfes {
            length: MasterAuxTable::NUM_CONSTRAINTS,
        }));
        let horner_evaluation_of_ood_curr_row_quot_segments =
            library.import(Box::new(HornerEvaluation {
                num_coefficients: NUM_QUOTIENT_SEGMENTS,
            }));
        let sample_beqd_weights = library.import(Box::new(SampleScalarsStaticLengthDynMalloc {
            num_elements: MasterMainTable::NUM_COLUMNS
                + MasterAuxTable::NUM_COLUMNS
                + NUM_QUOTIENT_SEGMENTS
                + NUM_DEEP_CODEWORD_COMPONENTS,
        }));
        let verify_main_table_rows = library.import(Box::new(VerifyTableRows {
            column_type: ColumnType::Main,
        }));
        let verify_aux_table_rows = library.import(Box::new(VerifyTableRows {
            column_type: ColumnType::Aux,
        }));
        let verify_quotient_segments = library.import(Box::new(VerifyTableRows {
            column_type: ColumnType::Quotient,
        }));
        let inner_product_three_rows_with_weights_bfe_main = library.import(Box::new(
            InnerProductOfThreeRowsWithWeights::triton_vm_parameters(MainElementType::Bfe),
        ));
        let inner_product_three_rows_with_weights_xfe_main = library.import(Box::new(
            InnerProductOfThreeRowsWithWeights::triton_vm_parameters(MainElementType::Xfe),
        ));
        let inner_product_4_xfes = library.import(Box::new(InnerProductOfXfes { length: 4 }));
        let quotient_segment_codeword_weights_from_be_weights = triton_asm!(
            // _ *beqd_ws

            push {(MasterMainTable::NUM_COLUMNS + MasterAuxTable::NUM_COLUMNS) * EXTENSION_DEGREE}
            add
            // _ *quotient_segment_weights
        );
        let deep_codeword_weights_read_address = |n: usize| {
            assert!(n < NUM_DEEP_CODEWORD_COMPONENTS);
            triton_asm!(
                // _ *beqd_ws

                push {(MasterMainTable::NUM_COLUMNS + MasterAuxTable::NUM_COLUMNS + NUM_QUOTIENT_SEGMENTS + n) * EXTENSION_DEGREE + {EXTENSION_DEGREE - 1}}
                add
                // _ *deep_codeword_weight[n]_last_word
            )
        };

        let dequeue_four_ood_rows = triton_asm! {
            // _ *proof_iter
            dup 0
            call {next_as_outofdomainmainrow}
            hint out_of_domain_curr_main_row: Pointer = stack[0]

            dup 1
            call {next_as_outofdomainauxrow}
            hint out_of_domain_curr_aux_row: Pointer = stack[0]

            dup 2
            call {next_as_outofdomainmainrow}
            hint out_of_domain_next_main_row: Pointer = stack[0]

            dup 3
            call {next_as_outofdomainauxrow}
            hint out_of_domain_next_aux_row: Pointer = stack[0]
            // _ *proof_iter *curr_main *curr_aux *next_main *next_aux
        };

        // BEFORE:
        // _ *p_iter - - - *quot_cw_ws - dom_gen [out_of_domain_curr_row] padded_height *proof_iter *curr_main *curr_aux *next_main *next_aux
        // AFTER:
        // _ *p_iter - - - *quot_cw_ws - dom_gen [out_of_domain_curr_row] padded_height *air_evaluation_result
        let ood_pointers_alloc = library.kmalloc(NUM_OOD_ROWS_WO_QUOTIENT);
        let evaluate_air_and_store_ood_pointers = match self.memory_layout {
            MemoryLayout::Static(static_layout) => {
                let static_eval =
                    library.import(Box::new(AirConstraintEvaluation::new_static(static_layout)));
                triton_asm! {
                    push {ood_pointers_alloc.write_address()}
                    write_mem {ood_pointers_alloc.num_words()}

                    pop 2
                    // _ ... padded_height

                    call {static_eval}
                    // _ ... padded_height *air_evaluation_result
                }
            }
            MemoryLayout::Dynamic(dynamic_layout) => {
                let dynamic_eval = library.import(Box::new(AirConstraintEvaluation::new_dynamic(
                    dynamic_layout,
                )));
                triton_asm! {
                    // store pointers to static memory
                    dup 3
                    dup 3
                    dup 3
                    dup 3
                    push {ood_pointers_alloc.write_address()}
                    write_mem {ood_pointers_alloc.num_words()}
                    pop 1
                    // _ ... padded_height *proof_iter *curr_main *curr_aux *next_main *next_aux

                    call {dynamic_eval}
                    // _ ... padded_height *proof_iter *air_evaluation_result

                    pick 1 pop 1
                    // _ ... padded_height *air_evaluation_result
                }
            }
        };

        let put_ood_row_pointers_back_on_stack = triton_asm! {
            // _
            push {ood_pointers_alloc.read_address()}
            read_mem {ood_pointers_alloc.num_words()}
            pop 1

            // _ *curr_main *curr_aux *next_main *next_aux
        };

        let challenges_ptr = self.memory_layout.challenges_pointer();

        let assert_top_two_xfes_eq = triton_asm!(
            // _ y2 y1 y0 x2 x1 x0
            pick 3
            eq
            assert error_id 230

            // _ y2 y1 x2 x1
            pick 2
            eq
            assert error_id 231

            // _ y2 x2
            eq
            assert error_id 232

            // _
        );

        let main_loop_label = format!("{entrypoint}_main_loop");
        let main_loop_body = triton_asm!(
            //                                                        (u32, XFieldElement)
            // _ remaining_rounds fri_gen fri_offset *etrow *btrow *qseg_elem *fri_revealed_idx *beqd_ws *oodpnts
            // Calculate `current_fri_domain_value`
            pick 2
            read_mem 1
            place 3
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
            call {inner_product_three_rows_with_weights_bfe_main} // expect arguments: *aux *main *ws
            hint main_and_aux_opened_row_element: Xfe = stack[0..3]
            // _ remaining_rounds fri_gen fri_offset *etrow *btrow *qseg_elem *fri_revealed_xfe *beqd_ws *oodpnts (-fdom_pnt) [be_opnd_elem]

            // Update `*btrow` and `*etrow` pointer values to point to previous element
            pick 9
            addi {-bfe!(EXTENSION_DEGREE * MasterAuxTable::NUM_COLUMNS)}
            place 9
            pick 8
            addi {-bfe!(MasterMainTable::NUM_COLUMNS)}
            place 8
            // _ remaining_rounds fri_gen fri_offset *etrow_prev *btrow_prev *qseg_elem *fri_revealed_xfe *beqd_ws *oodpnts (-fdom_pnt) [be_opnd_elem]

            push -1
            xb_mul
            hint neg_main_and_aux_opened_row_element: Xfe = stack[0..3]

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

            pick 13
            addi {-bfe!(NUM_QUOTIENT_SEGMENTS * EXTENSION_DEGREE)}
            place 13
            // _ remaining_rounds fri_gen fri_offset *etrow_prev *btrow_prev *qseg_elem_prev *fri_revealed_xfe *beqd_ws *oodpnts (-fdom_pnt) [-be_opnd_elem] [1/(oodp_pow_nsegs - fdom_pnt)] [inner_prod]

            push -1
            xb_mul
            // _ remaining_rounds fri_gen fri_offset *etrow_prev *btrow_prev *qseg_elem_prev *fri_revealed_xfe *beqd_ws *oodpnts (-fdom_pnt) [-be_opnd_elem] [1/(oodp_pow_nsegs - fdom_pnt)] [-inner_prod]

            push {ood_curr_row_quotient_segment_value_pointer_alloc.read_address()}
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

            push {ood_curr_row_main_and_aux_value_pointer_alloc.read_address()}
            read_mem {EXTENSION_DEGREE}
            pop 1
            // _ remaining_rounds fri_gen fri_offset *etrow_prev *btrow_prev *qseg_elem_prev *fri_revealed_xfe *beqd_ws *oodpnts (-fdom_pnt) [-be_opnd_elem] [dv2] [1/(ood_point_curr_row - fdom_pnt)] [out_of_domain_curr_row_main_and_aux_value]

            dup 11
            dup 11
            dup 11
            xx_add
            // _ remaining_rounds fri_gen fri_offset *etrow_prev *btrow_prev *qseg_elem_prev *fri_revealed_xfe *beqd_ws *oodpnts (-fdom_pnt) [-be_opnd_elem] [dv2] [1/(ood_point_curr_row - fdom_pnt)] [out_of_domain_curr_row_main_and_aux_value - be_opnd_elem]

            xx_mul
            // _ remaining_rounds fri_gen fri_offset *etrow_prev *btrow_prev *qseg_elem_prev *fri_revealed_xfe *beqd_ws *oodpnts (-fdom_pnt) [-be_opnd_elem] [dv2] [(out_of_domain_curr_row_main_and_aux_value - be_opnd_elem)/(ood_point_curr_row - fdom_pnt)]

            dup 11
            {&deep_codeword_weights_read_address(0)}
            read_mem {EXTENSION_DEGREE}              // read deep_codeword_weights[0]
            pop 1
            xx_mul
            // _ remaining_rounds fri_gen fri_offset *etrow_prev *btrow_prev *qseg_elem_prev *fri_revealed_xfe *beqd_ws *oodpnts (-fdom_pnt) [-be_opnd_elem] [dv2] [deep_codeword_weights[0] * (out_of_domain_curr_row_main_and_aux_value - be_opnd_elem)/(ood_point_curr_row - fdom_pnt)]
            // _ remaining_rounds fri_gen fri_offset *etrow_prev *btrow_prev *qseg_elem_prev *fri_revealed_xfe *beqd_ws *oodpnts (-fdom_pnt) [-be_opnd_elem] [dv2] [dv0]

            xx_add
            // _ remaining_rounds fri_gen fri_offset *etrow_prev *btrow_prev *qseg_elem_prev *fri_revealed_xfe *beqd_ws *oodpnts (-fdom_pnt) [-be_opnd_elem] [dv2 + dv0]
            hint dv2_plus_dv0: XFieldElement = stack[0..3]

            pick 5
            pick 5
            pick 5
            // _ remaining_rounds fri_gen fri_offset *etrow_prev *btrow_prev *qseg_elem_prev *fri_revealed_xfe *beqd_ws *oodpnts (-fdom_pnt) [dv2 + dv0] [-be_opnd_elem]

            push {ood_next_row_main_and_aux_value_pointer_alloc.read_address()}
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

            pick 3
            pop 1
            // _ remaining_rounds fri_gen fri_offset *etrow_prev *btrow_prev *qseg_elem_prev *fri_revealed_xfe *beqd_ws *oodpnts [deep_value]

            pick 5
            read_mem {EXTENSION_DEGREE}
            place 8
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
                pick 8
                addi -1
                place 8
                recurse
        );

        triton_asm!(
            {entrypoint}:
                sponge_init
                // _ *claim *proof

                call {proof_to_vm_proof_iter}
                hint proof_iter = stack[0]

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

                /* Verify log_2_padded_height <= 31 */
                push 32
                dup 1
                lt
                // _ *clm *p_iter log_2_padded_height (32 > log_2_padded_height)

                assert error_id {Self::LOG2_PADDED_HEIGHT_TOO_LARGE}
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

                // verify that the challenges are stored at the right place
                push {challenges_ptr}
                eq
                assert error_id 233
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

                dup 10
                {&dequeue_four_ood_rows}
                // _ *b_mr *p_iter *oodpnts *fri *e_mr *quot_cw_ws *quot_mr dom_gen [out_of_domain_curr_row] padded_height *proof_iter *curr_main *curr_aux *next_main *next_aux

                {&evaluate_air_and_store_ood_pointers}
                // _ *b_mr *p_iter *oodpnts *fri *e_mr *quot_cw_ws *quot_mr dom_gen [out_of_domain_curr_row] padded_height *air_evaluation_result

                swap 5
                // _ *b_mr *p_iter *oodpnts *fri *e_mr *quot_cw_ws *quot_mr *air_evaluation_result [out_of_domain_curr_row] padded_height dom_gen

                call {divide_out_zerofiers}
                // _ *b_mr *p_iter *oodpnts *fri *e_mr *quot_cw_ws *quot_mr *quotient_summands

                {&put_ood_row_pointers_back_on_stack}
                // _ *b_mr *p_iter *oodpnts *fri *e_mr *quot_cw_ws *quot_mr *quotient_summands *ood_brow_curr *ood_erow_curr *odd_brow_nxt *ood_erow_nxt

                dup 10
                call {next_as_outofdomainquotientsegments}
                hint out_of_domain_quotient_segments: Pointer = stack[0]
                // _ *b_mr *p_iter *oodpnts *fri *e_mr *quot_cw_ws *quot_mr *quotient_summands *ood_brow_curr *ood_erow_curr *odd_brow_nxt *ood_erow_nxt *ood_quotient_segments

                dup 0
                push {out_of_domain_curr_row_quot_segments_pointer_alloc.write_address()}
                write_mem {out_of_domain_curr_row_quot_segments_pointer_alloc.num_words()}
                pop 1


                /* Calculate `sum_of_evaluated_out_of_domain_quotient_segments` */
                dup 10
                {&OutOfDomainPoints::read_ood_point(OodPoint::CurrentRow)}
                // _ *b_mr *p_iter *oodpnts *fri *e_mr *quot_cw_ws *quot_mr *quotient_summands *ood_brow_curr *ood_erow_curr *odd_brow_nxt *ood_erow_nxt *ood_quotient_segments [ood_curr_row]

                call {horner_evaluation_of_ood_curr_row_quot_segments}
                // _ *b_mr *p_iter *oodpnts *fri *e_mr *quot_cw_ws *quot_mr *quotient_summands *ood_brow_curr *ood_erow_curr *odd_brow_nxt *ood_erow_nxt [sum_of_evaluated_out_of_domain_quotient_segments]


                /* Calculate inner product `out_of_domain_quotient_value` */
                pick 4 place 9
                pick 3 place 6
                pick 8 pick 6
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


                /* Dequeue main-table rows and verify against its Merkle root */
                dup 10
                call {next_as_maintablerows}
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

                pick 4
                // _ *beqd_ws *p_iter *oodpnts *fri *e_mr *odd_brow_nxt *quot_mr *ood_erow_nxt *ood_brow_curr *ood_erow_curr *fri_revealed *btrows num_colli mt_height *b_mr

                dup 4
                // _ *beqd_ws *p_iter *oodpnts *fri *e_mr *odd_brow_nxt *quot_mr *ood_erow_nxt *ood_brow_curr *ood_erow_curr *fri_revealed *btrows num_colli mt_height *b_mr *fri_revealed

                dup 4
                // _ *beqd_ws *p_iter *oodpnts *fri *e_mr *odd_brow_nxt *quot_mr *ood_erow_nxt *ood_brow_curr *ood_erow_curr *fri_revealed *btrows num_colli mt_height *b_mr *fri_revealed *btrows

                call {verify_main_table_rows}
                // _ *beqd_ws *p_iter *oodpnts *fri *e_mr *odd_brow_nxt *quot_mr *ood_erow_nxt *ood_brow_curr *ood_erow_curr *fri_revealed *btrows


                /* Dequeue and ignore main-table's authentication path */
                dup 10
                call {next_as_authentication_path}
                pop 1
                // _ *beqd_ws *p_iter *oodpnts *fri *e_mr *odd_brow_nxt *quot_mr *ood_erow_nxt *ood_brow_curr *ood_erow_curr *fri_revealed *btrows

                swap 7
                // _ *beqd_ws *p_iter *oodpnts *fri *btrows *odd_brow_nxt *quot_mr *ood_erow_nxt *ood_brow_curr *ood_erow_curr *fri_revealed *e_mr


                /* Dequeue aux-table rows and verify against its Merkle root */
                dup 10
                call {next_as_auxtablerows}
                // _ *beqd_ws *p_iter *oodpnts *fri *btrows *odd_brow_nxt *quot_mr *ood_erow_nxt *ood_brow_curr *ood_erow_curr *fri_revealed *e_mr *etrows

                pick 1
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

                pick 2
                dup 4
                dup 4
                // _ *beqd_ws *p_iter *oodpnts *fri *btrows *odd_brow_next *quot_mr *ood_erow_nxt *ood_brow_curr *ood_erow_curr *fri_revealed *etrows num_colli mt_height *e_mr *fri_revealed *etrows

                call {verify_aux_table_rows}
                // _ *beqd_ws *p_iter *oodpnts *fri *btrows *odd_brow_next *quot_mr *ood_erow_nxt *ood_brow_curr *ood_erow_curr *fri_revealed *etrows


                /* Dequeue and ignore aux-table's authentication path */
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

                pick 2
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
                assert error_id 234
                // _ *beqd_ws *p_iter *oodpnts *fri *btrows *odd_brow_next *etrows *ood_erow_nxt *ood_brow_curr *ood_erow_curr *fri_revealed *qseg_elems num_colli

                // assert!(num_combination_codeword_checks == main_table_rows.len());
                dup 8
                read_mem 1
                pop 1
                dup 1
                eq
                assert error_id 235

                // assert!(num_combination_codeword_checks == aux_table_rows.len())
                dup 6
                read_mem 1
                pop 1
                dup 1
                eq
                assert error_id 236

                // assert!(num_combination_codeword_checks == quotient_segment_elements.len());
                dup 1
                read_mem 1
                pop 1
                dup 1
                eq
                assert error_id 237
                // _ *beqd_ws *p_iter *oodpnts *fri *btrows *odd_brow_next *etrows *ood_erow_nxt *ood_brow_curr *ood_erow_curr *fri_revealed *qseg_elems num_colli


                /* Dequeue last authentication path, and verify that p_iter ends up in consistent state */
                swap 12
                swap 11
                dup 0
                call {next_as_authentication_path}
                pop 1

                call {drop_vm_proof_iter}
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

                call {inner_product_three_rows_with_weights_xfe_main} // expects arguments: *aux *main *ws
                hint out_of_domain_curr_row_main_and_aux_value: XFieldElement = stack[0..3]
                // _ num_colli *beqd_ws *oodpnts *fri *btrows *odd_brow_next *etrows *ood_erow_nxt *fri_revealed *qseg_elems [ood_curr_beval]

                push {ood_curr_row_main_and_aux_value_pointer_alloc.write_address()}
                write_mem {ood_curr_row_main_and_aux_value_pointer_alloc.num_words()}
                pop 1
                // _ num_colli *beqd_ws *oodpnts *fri *btrows *odd_brow_next *etrows *ood_erow_nxt *fri_revealed *qseg_elems

                // Goal: `_ *ood_erow_nxt *odd_brow_next *beqd_ws`, preserving `*beqd_ws`.

                swap 2
                swap 1
                swap 4
                dup 8
                // _ num_colli *beqd_ws *oodpnts *fri *btrows *fri_revealed *etrows *qseg_elems *ood_erow_nxt *odd_brow_next *beqd_ws

                call {inner_product_three_rows_with_weights_xfe_main}  // expects arguments: *aux *main *ws
                hint out_of_domain_next_row_main_and_aux_value: XFieldElement = stack[0..3]
                // _ num_colli *beqd_ws *oodpnts *fri *btrows *fri_revealed *etrows *qseg_elems [ood_next_value]

                push {ood_next_row_main_and_aux_value_pointer_alloc.write_address()}
                write_mem {ood_next_row_main_and_aux_value_pointer_alloc.num_words()}
                pop 1
                // _ num_colli *beqd_ws *oodpnts *fri *btrows *fri_revealed *etrows *qseg_elems

                // Goal: `_ *quotient_segment_codeword_weights *ood_curr_row_quot_segments`
                dup 6
                {&quotient_segment_codeword_weights_from_be_weights}
                // _ num_colli *beqd_ws *oodpnts *fri *btrows *fri_revealed *etrows *qseg_elems *quotient_segment_codeword_weights

                push {out_of_domain_curr_row_quot_segments_pointer_alloc.read_address()}
                read_mem {out_of_domain_curr_row_quot_segments_pointer_alloc.num_words()}
                pop 1
                // _ num_colli *beqd_ws *oodpnts *fri *btrows *fri_revealed *etrows *qseg_elems *quotient_segment_codeword_weights *ood_curr_row_quot_segments

                call {inner_product_4_xfes}
                hint out_of_domain_curr_row_quotient_segment_value: XFieldElement = stack[0..3]
                // _ num_colli *beqd_ws *oodpnts *fri *btrows *fri_revealed *etrows *qseg_elems [out_of_domain_curr_row_quotient_segment_value]

                push {ood_curr_row_quotient_segment_value_pointer_alloc.write_address()}
                write_mem {ood_curr_row_quotient_segment_value_pointer_alloc.num_words()}
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
                pick 3
                dup 8
                push {EXTENSION_DEGREE + 1} // size of element in `fri_revealed` list
                mul
                add
                hint fri_revealed_elem = stack[0]
                place 3

                // Adjust *btrows (list) to point to last element
                pick 4
                addi 1
                dup 8
                addi -1
                push {MasterMainTable::NUM_COLUMNS} // size of element of main row list
                mul
                add
                hint main_table_row = stack[0]
                place 4

                // Adjust *etrows (list) to point to last element
                pick 2
                addi 1
                dup 8
                addi -1
                push {MasterAuxTable::NUM_COLUMNS * EXTENSION_DEGREE} // size of element of aux row list
                mul
                add
                hint aux_table_row = stack[0]
                place 2

                // Adjust *qseg_elems to point to last element
                pick 5
                addi 1
                dup 8
                addi -1
                push {NUM_QUOTIENT_SEGMENTS * EXTENSION_DEGREE} // size of element of quot row list
                mul
                add
                hint quotient_segment_elem = stack[0]
                place 5

                // _ num_colli *beqd_ws *oodpnts *qseg_elems *btrows *fri_revealed *etrows fri_offset fri_gen

                /* reorganize stack for main-loop */
                place 7
                place 6
                place 5
                place 4
                place 4
                place 3
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

    use num_traits::ConstZero;
    use tasm_object_derive::TasmObject;
    use triton_vm::proof_item::ProofItem;

    use super::*;
    use crate::execute_test;
    use crate::maybe_write_debuggable_vm_state_to_disk;
    use crate::memory::FIRST_NON_DETERMINISTICALLY_INITIALIZED_MEMORY_ADDRESS;
    use crate::memory::encode_to_memory;
    use crate::test_helpers::maybe_write_tvm_output_to_disk;
    use crate::verifier::claim::shared::insert_claim_into_static_memory;
    use crate::verifier::master_table::air_constraint_evaluation::an_integral_but_profane_dynamic_memory_layout;

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

        let snippet = StarkVerify {
            stark,
            memory_layout: MemoryLayout::conventional_dynamic(),
        };
        let mut nondeterminism = NonDeterminism::new(vec![]);
        snippet.update_nondeterminism(&mut nondeterminism, &proof, &claim_for_proof);

        let (claim_pointer, claim_size) =
            insert_claim_into_static_memory(&mut nondeterminism.ram, &claim_for_proof);

        let default_proof_pointer = BFieldElement::ZERO;

        let mut init_stack = [
            snippet.init_stack_for_isolated_run(),
            vec![claim_pointer, default_proof_pointer],
        ]
        .concat();
        let code = snippet.link_for_isolated_run_populated_static_memory(claim_size);
        let _final_tasm_state = execute_test(
            &code,
            &mut init_stack,
            snippet.stack_diff(),
            vec![],
            nondeterminism,
            None,
        );
    }

    #[test]
    fn fail_on_too_big_log2_padded_height() {
        let mut proof_stream = ProofStream::new();
        proof_stream.enqueue(ProofItem::Log2PaddedHeight(32));
        let proof: Proof = proof_stream.into();

        let mut nondeterminism = NonDeterminism::new(vec![]);
        encode_to_memory(
            &mut nondeterminism.ram,
            FIRST_NON_DETERMINISTICALLY_INITIALIZED_MEMORY_ADDRESS,
            &proof,
        );
        let (claim_pointer, claim_size) = insert_claim_into_static_memory(
            &mut nondeterminism.ram,
            &Claim::new(Digest::default()),
        );

        let snippet = StarkVerify {
            stark: Stark::default(),
            memory_layout: MemoryLayout::conventional_static(),
        };
        let default_proof_pointer = bfe!(0);
        let init_stack = [
            snippet.init_stack_for_isolated_run(),
            vec![claim_pointer, default_proof_pointer],
        ]
        .concat();

        let program =
            Program::new(&snippet.link_for_isolated_run_populated_static_memory(claim_size));
        let mut vm_state = VMState::new(program, [].into(), nondeterminism.clone());
        vm_state.op_stack.stack = init_stack.clone();
        let error = vm_state.run().unwrap_err();
        match error {
            InstructionError::AssertionFailed(assertion_error) => {
                assert_eq!(
                    StarkVerify::LOG2_PADDED_HEIGHT_TOO_LARGE,
                    assertion_error.id.unwrap()
                );
            }
            _ => panic!(),
        }
    }

    /// Run the verifier, and return the cycle count and inner padded
    /// height for crude benchmarking.
    fn test_verify_and_report_basic_features(
        inner_nondeterminism: NonDeterminism,
        inner_program: Program,
        inner_public_input: &[BFieldElement],
        stark: Stark,
        layout: MemoryLayout,
    ) -> (usize, usize) {
        let (mut non_determinism, claim_for_proof) = prove_and_get_non_determinism_and_claim(
            inner_program.clone(),
            inner_public_input,
            inner_nondeterminism.clone(),
            &stark,
        );

        let (claim_pointer, claim_size) =
            insert_claim_into_static_memory(&mut non_determinism.ram, &claim_for_proof);

        let default_proof_pointer = bfe!(0);

        let snippet = StarkVerify {
            stark,
            memory_layout: layout,
        };
        let mut init_stack = [
            snippet.init_stack_for_isolated_run(),
            vec![claim_pointer, default_proof_pointer],
        ]
        .concat();
        let code = snippet.link_for_isolated_run_populated_static_memory(claim_size);

        let program = Program::new(&code);
        let mut vm_state = VMState::new(program, [].into(), non_determinism.clone());
        vm_state.op_stack.stack = init_stack.clone();
        maybe_write_debuggable_vm_state_to_disk(&vm_state);

        let final_tasm_state = execute_test(
            &code,
            &mut init_stack,
            snippet.stack_diff(),
            vec![],
            non_determinism,
            None,
        );

        let (aet, _public_output) = VM::trace_execution(
            inner_program,
            (&claim_for_proof.input).into(),
            inner_nondeterminism,
        )
        .unwrap();
        let inner_padded_height = aet.padded_height();

        (final_tasm_state.cycle_count as usize, inner_padded_height)
    }

    #[test]
    fn different_fri_expansion_factors() {
        const FACTORIAL_ARGUMENT: u32 = 3;

        for log2_of_fri_expansion_factor in 2..=5 {
            println!("log2_of_fri_expansion_factor: {log2_of_fri_expansion_factor}");
            let factorial_program = factorial_program_with_io();
            let stark = Stark::new(160, log2_of_fri_expansion_factor);
            let (cycle_count, inner_padded_height) = test_verify_and_report_basic_features(
                NonDeterminism::default(),
                factorial_program,
                &[FACTORIAL_ARGUMENT.into()],
                stark,
                MemoryLayout::conventional_static(),
            );
            println!(
                "TASM-verifier of factorial({FACTORIAL_ARGUMENT}):\n
                Fri expansion factor: {}\n
                clock cycle count: {}.\n
                Inner padded height was: {}",
                1 << log2_of_fri_expansion_factor,
                cycle_count,
                inner_padded_height,
            );
        }
    }

    fn verify_tvm_proof_factorial_program_basic_properties(mem_layout: MemoryLayout) {
        const FACTORIAL_ARGUMENT: u32 = 3;

        let factorial_program = factorial_program_with_io();
        let stark = Stark::default();
        let (cycle_count, inner_padded_height) = test_verify_and_report_basic_features(
            NonDeterminism::default(),
            factorial_program,
            &[FACTORIAL_ARGUMENT.into()],
            stark,
            mem_layout,
        );

        println!(
            "TASM-verifier of factorial({FACTORIAL_ARGUMENT}):\n
            clock cycle count: {cycle_count}.\n
            Inner padded height was: {inner_padded_height}",
        );
    }

    #[test]
    fn verify_tvm_proof_factorial_program_conventional_static_memlayout() {
        verify_tvm_proof_factorial_program_basic_properties(MemoryLayout::conventional_static());
    }

    #[test]
    fn verify_tvm_proof_factorial_program_conventional_dynamic_memlayout() {
        verify_tvm_proof_factorial_program_basic_properties(MemoryLayout::conventional_dynamic());
    }

    #[test]
    fn verify_tvm_proof_factorial_program_profane_dynamic_memlayout() {
        verify_tvm_proof_factorial_program_basic_properties(MemoryLayout::Dynamic(
            an_integral_but_profane_dynamic_memory_layout(),
        ));
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
                pick 1           // acc·n n
                addi -1          // acc·n n-1
                place 1          // n-1 acc·n

                recurse
        )
    }

    /// Return data needed to verify a program execution's proof.
    ///
    /// Prepares the caller so that the caller can call verify on a simple program
    /// execution. Specifically, given an inner program, inner public input, inner
    /// nondeterminism, and stark parameters; produce the proof, and use it to
    /// populate non-determism (both memory and streams). Returns the claim that
    /// the caller will then have to put into memory. Proof is stored on first
    /// address of the ND-memory region.
    pub fn prove_and_get_non_determinism_and_claim(
        inner_program: Program,
        inner_public_input: &[BFieldElement],
        inner_nondeterminism: NonDeterminism,
        stark: &Stark,
    ) -> (NonDeterminism, Claim) {
        println!("Generating proof for non-determinism");

        let inner_input = inner_public_input.to_vec();
        let claim = Claim::about_program(&inner_program).with_input(inner_input.clone());
        let (aet, inner_output) =
            VM::trace_execution(inner_program, inner_input.into(), inner_nondeterminism).unwrap();
        let claim = claim.with_output(inner_output);

        triton_vm::profiler::start("inner program");
        let seed = [
            227, 232, 115, 183, 84, 194, 68, 59, 166, 60, 140, 218, 88, 117, 227, 129, 10, 121,
            108, 40, 65, 125, 143, 31, 155, 128, 202, 75, 218, 44, 120, 170,
        ];
        let prover = Prover::new(*stark).set_randomness_seed_which_may_break_zero_knowledge(seed);
        let proof = prover.prove(&claim, &aet).unwrap();
        let profile = triton_vm::profiler::finish();
        let padded_height = proof.padded_height().unwrap();
        let report = profile
            .with_cycle_count(aet.processor_trace.nrows())
            .with_padded_height(padded_height)
            .with_fri_domain_len(stark.fri(padded_height).unwrap().domain.len());
        println!("Done generating proof for non-determinism");
        println!("{report}");

        assert!(
            stark.verify(&claim, &proof).is_ok(),
            "Proof from TVM must verify through TVM"
        );

        maybe_write_tvm_output_to_disk(stark, &claim, &proof);

        let mut nondeterminism = NonDeterminism::new(vec![]);
        let stark_verify = StarkVerify {
            stark: *stark,
            memory_layout: MemoryLayout::conventional_static(),
        };

        // Verify nd-digest count
        let actual_num_extracted_digests = stark_verify
            .extract_nondeterministic_digests(&proof, &claim)
            .len();
        let expected_num_extracted_digests =
            stark_verify.number_of_nondeterministic_digests_consumed(&proof);
        assert_eq!(
            actual_num_extracted_digests, expected_num_extracted_digests,
            "Number of extracted digests must match expected value"
        );

        stark_verify.update_nondeterminism(&mut nondeterminism, &proof, &claim);
        encode_to_memory(
            &mut nondeterminism.ram,
            FIRST_NON_DETERMINISTICALLY_INITIALIZED_MEMORY_ADDRESS,
            &proof,
        );

        (nondeterminism, claim)
    }

    #[test]
    fn verify_two_proofs() {
        #[derive(Debug, Clone, BFieldCodec, TasmObject)]
        struct TwoProofs {
            proof1: Proof,
            claim1: Claim,
            proof2: Proof,
            claim2: Claim,
        }

        let stark = Stark::default();
        let stark_snippet = StarkVerify::new_with_dynamic_layout(stark);

        let mut library = Library::new();
        let stark_verify = library.import(Box::new(stark_snippet));
        let proof1 = field!(TwoProofs::proof1);
        let proof2 = field!(TwoProofs::proof2);
        let claim1 = field!(TwoProofs::claim1);
        let claim2 = field!(TwoProofs::claim2);
        let verify_two_proofs_program = triton_asm! {


            push {FIRST_NON_DETERMINISTICALLY_INITIALIZED_MEMORY_ADDRESS}
            // *two_proofs

            dup 0
            {&claim1}

            dup 1
            {&proof1}
            // *two_proofs *claim1 *proof1

            call {stark_verify}

            dup 0
            {&claim2}

            dup 1
            {&proof2}
            // *two_proofs *claim2 *proof2

            call {stark_verify}

            halt

            {&library.all_imports()}
        };

        let inner_input_1 = bfe_vec![3];
        let factorial_program = factorial_program_with_io();
        let (aet_1, inner_output_1) = VM::trace_execution(
            factorial_program.clone(),
            inner_input_1.clone().into(),
            [].into(),
        )
        .unwrap();
        let claim_1 = Claim::about_program(&factorial_program)
            .with_input(inner_input_1.clone())
            .with_output(inner_output_1.clone());
        let proof_1 = stark.prove(&claim_1, &aet_1).unwrap();
        let padded_height_1 = proof_1.padded_height().unwrap();
        println!("padded_height_1: {padded_height_1}");

        let inner_input_2 = bfe_vec![25];
        let (aet_2, inner_output_2) = VM::trace_execution(
            factorial_program.clone(),
            inner_input_2.clone().into(),
            [].into(),
        )
        .unwrap();
        let claim_2 = Claim::about_program(&factorial_program)
            .with_input(inner_input_2.clone())
            .with_output(inner_output_2.clone());
        let proof_2 = stark.prove(&claim_2, &aet_2).unwrap();
        let padded_height_2 = proof_2.padded_height().unwrap();
        println!("padded_height_2: {padded_height_2}");

        let two_proofs = TwoProofs {
            proof1: proof_1.clone(),
            claim1: claim_1.clone(),
            proof2: proof_2.clone(),
            claim2: claim_2.clone(),
        };

        let mut memory = HashMap::<BFieldElement, BFieldElement>::new();
        let outer_input = vec![];
        encode_to_memory(
            &mut memory,
            FIRST_NON_DETERMINISTICALLY_INITIALIZED_MEMORY_ADDRESS,
            &two_proofs,
        );

        let mut outer_nondeterminism = NonDeterminism::new(vec![]).with_ram(memory);

        let num_nd_digests_before = outer_nondeterminism.digests.len();

        stark_snippet.update_nondeterminism(&mut outer_nondeterminism, &proof_1, &claim_1);
        stark_snippet.update_nondeterminism(&mut outer_nondeterminism, &proof_2, &claim_2);

        let num_nd_digests_after = outer_nondeterminism.digests.len();

        assert_eq!(
            num_nd_digests_after - num_nd_digests_before,
            stark_snippet.number_of_nondeterministic_digests_consumed(&proof_1)
                + stark_snippet.number_of_nondeterministic_digests_consumed(&proof_2)
        );

        let program = Program::new(&verify_two_proofs_program);
        let vm_state = VMState::new(
            program.clone(),
            outer_input.clone().into(),
            outer_nondeterminism.clone(),
        );
        maybe_write_debuggable_vm_state_to_disk(&vm_state);
        VM::run(program, outer_input.into(), outer_nondeterminism)
            .expect("could not verify two STARK proofs");

        println!(
            "fact({}) == {} ∧ fact({}) == {}",
            inner_input_1[0], inner_output_1[0], inner_input_2[0], inner_output_2[0]
        );

        assert_ne!(
            padded_height_1, padded_height_2,
            "proofs do not have different padded heights"
        );
    }
}

#[cfg(test)]
mod benches {
    use benches::tests::factorial_program_with_io;
    use benches::tests::prove_and_get_non_determinism_and_claim;
    use num_traits::ConstZero;

    use super::*;
    use crate::generate_full_profile;
    use crate::linker::execute_bench;
    use crate::memory::encode_to_memory;
    use crate::snippet_bencher::BenchmarkCase;
    use crate::snippet_bencher::NamedBenchmarkResult;
    use crate::snippet_bencher::write_benchmarks;
    use crate::test_helpers::prepend_program_with_stack_setup;
    use crate::verifier::claim::shared::insert_claim_into_static_memory;

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

        let snippet = StarkVerify {
            stark,
            memory_layout: MemoryLayout::conventional_static(),
        };
        let mut nondeterminism = NonDeterminism::new(vec![]);
        snippet.update_nondeterminism(&mut nondeterminism, &proof, &claim_for_proof);

        let (claim_pointer, claim_size) =
            insert_claim_into_static_memory(&mut nondeterminism.ram, &claim_for_proof);

        let default_proof_pointer = BFieldElement::ZERO;

        let init_stack = [
            snippet.init_stack_for_isolated_run(),
            vec![claim_pointer, default_proof_pointer],
        ]
        .concat();
        let code = snippet.link_for_isolated_run_populated_static_memory(claim_size);
        let program = prepend_program_with_stack_setup(&init_stack, &Program::new(&code));

        let name = snippet.entrypoint();
        let profile = generate_full_profile(
            &name,
            program,
            &PublicInput::new(claim_for_proof.input),
            &nondeterminism,
        );
        println!("{profile}");
    }

    #[test]
    fn benchmark_small_default_stark_static_memory() {
        benchmark_verifier(
            3,
            1 << 8,
            Stark::default(),
            MemoryLayout::conventional_static(),
        );
        benchmark_verifier(
            40,
            1 << 9,
            Stark::default(),
            MemoryLayout::conventional_static(),
        );
    }

    #[test]
    fn benchmark_small_default_stark_dynamic_memory() {
        benchmark_verifier(
            3,
            1 << 8,
            Stark::default(),
            MemoryLayout::conventional_dynamic(),
        );
        benchmark_verifier(
            40,
            1 << 9,
            Stark::default(),
            MemoryLayout::conventional_dynamic(),
        );
    }

    #[ignore = "Takes a fairly long time. Intended to find optimal FRI expansion factor."]
    #[test]
    fn small_benchmark_different_fri_expansion_factors() {
        for log2_of_fri_expansion_factor in 1..=5 {
            let stark = Stark::new(160, log2_of_fri_expansion_factor);
            benchmark_verifier(10, 1 << 8, stark, MemoryLayout::conventional_static());
            benchmark_verifier(40, 1 << 9, stark, MemoryLayout::conventional_static());
            benchmark_verifier(80, 1 << 10, stark, MemoryLayout::conventional_static());
        }
    }

    #[ignore = "Takes a very long time. Intended to find optimal FRI expansion factor. Make sure to run
       with `RUSTFLAGS=\"-C opt-level=3 -C debug-assertions=no\"`"]
    #[test]
    fn big_benchmark_different_fri_expansion_factors() {
        let mem_layout = MemoryLayout::conventional_static();
        for log2_of_fri_expansion_factor in 2..=3 {
            let stark = Stark::new(160, log2_of_fri_expansion_factor);
            benchmark_verifier(25600, 1 << 19, stark, mem_layout);
            benchmark_verifier(51200, 1 << 20, stark, mem_layout);
            benchmark_verifier(102400, 1 << 21, stark, mem_layout);
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
            benchmark_verifier(
                fact_arg,
                expected_inner_padded_height,
                Stark::default(),
                MemoryLayout::conventional_static(),
            );
        }
    }

    fn benchmark_verifier(
        factorial_argument: u32,
        inner_padded_height: usize,
        stark: Stark,
        mem_layout: MemoryLayout,
    ) {
        let (mut non_determinism, claim_for_proof) = prove_and_get_non_determinism_and_claim(
            factorial_program_with_io(),
            &[bfe!(factorial_argument)],
            NonDeterminism::default(),
            &stark,
        );

        let claim_pointer = BFieldElement::new(1 << 30);
        encode_to_memory(&mut non_determinism.ram, claim_pointer, &claim_for_proof);

        let default_proof_pointer = BFieldElement::ZERO;

        let snippet = StarkVerify {
            stark,
            memory_layout: mem_layout,
        };

        let init_stack = [
            snippet.init_stack_for_isolated_run(),
            vec![claim_pointer, default_proof_pointer],
        ]
        .concat();
        let code = snippet.link_for_isolated_run();
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
        println!("{profile}");
    }
}
