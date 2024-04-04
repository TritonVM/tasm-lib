use triton_vm::prelude::*;
use triton_vm::proof_item::ProofItemVariant;
use triton_vm::table::extension_table::Quotientable;
use triton_vm::table::master_table::MasterExtTable;
use triton_vm::table::NUM_QUOTIENT_SEGMENTS;
use triton_vm::twenty_first::shared_math::x_field_element::EXTENSION_DEGREE;

use crate::arithmetic::bfe::primitive_root_of_unity::PrimitiveRootOfUnity;
use crate::array::horner_evaluation::HornerEvaluation;
use crate::array::inner_product_of_xfes::InnerProductOfXfes;
use crate::data_type::DataType;
use crate::hashing::algebraic_hasher::sample_scalar_one::SampleScalarOne;
use crate::hashing::algebraic_hasher::sample_scalars_static_length_dyn_malloc::SampleScalarsStaticLengthDynMalloc;
use crate::library::Library;
use crate::recufier::challenges::shared::conventional_challenges_pointer;
use crate::recufier::claim::instantiate_fiat_shamir_with_claim::InstantiateFiatShamirWithClaim;
use crate::recufier::claim::shared::claim_type;
use crate::recufier::master_ext_table::quotient_summands::QuotientSummands;
use crate::recufier::out_of_domain_points::OodPoint;
use crate::recufier::out_of_domain_points::OutOfDomainPoints;
use crate::recufier::vm_proof_iter::dequeue_next_as::DequeueNextAs;
use crate::recufier::{challenges, fri, vm_proof_iter};
use crate::traits::basic_snippet::BasicSnippet;

pub struct StarkVerify {
    stark_parameters: Stark,
    log_2_padded_height: Option<u32>,
}

impl BasicSnippet for StarkVerify {
    fn inputs(&self) -> Vec<(DataType, String)> {
        vec![(DataType::StructRef(claim_type()), "claim".to_owned())]
    }

    fn outputs(&self) -> Vec<(DataType, String)> {
        vec![]
    }

    fn entrypoint(&self) -> String {
        "tasm_recufier_stark_verify".to_owned()
    }

    fn code(&self, library: &mut Library) -> Vec<LabelledInstruction> {
        let entrypoint = self.entrypoint();

        let instantiate_fiat_shamir_with_claim =
            library.import(Box::new(InstantiateFiatShamirWithClaim));
        let new_proof_iter = library.import(Box::new(vm_proof_iter::new::New));
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
        let derive_fri_parameters = library.import(Box::new(
            fri::derive_from_stark_params::DeriveFriFromStarkParams {
                stark_parameters: self.stark_parameters,
            },
        ));
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

        triton_asm!(
            {entrypoint}:
                sponge_init

                // _ *clm


                /* Fiat-Shamir: Claim */
                dup 0
                call {instantiate_fiat_shamir_with_claim}
                // _ *clm


                /* derive additional parameters */
                call {new_proof_iter}
                hint p_iter = stack[0]
                // _ *clm *p_iter

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


                /* Fiat-Shamir 1 */
                dup 1
                call {next_as_merkleroot}
                hint base_mr = stack[0]
                // _ *clm *p_iter padded_height *base_mr

                swap 3
                // _  *base_mr *p_iter padded_height *clm

                call {get_challenges}
                // _ *base_mr *p_iter padded_height *challenges

                {&verify_challenges_pointer}
                // _ *base_mr *p_iter padded_height

                dup 1
                call {next_as_merkleroot}
                hint ext_mr = stack[0]
                // _ *base_mr *p_iter padded_height *ext_mr

                call {sample_quotient_codeword_weights}
                // _ *base_mr *p_iter padded_height *ext_mr *quot_cw_ws
                hint quot_codeword_weights = stack[0]

                dup 3
                call {next_as_merkleroot}
                hint quot_mr = stack[0]
                // _ *base_mr *p_iter padded_height *ext_mr *quot_cw_ws *quot_mr


                /* sample and calculate OOD points (not rows) */
                push 0
                dup 4
                call {domain_generator}
                hint trace_domain_generator = stack[0]
                // _ *base_mr *p_iter padded_height *ext_mr *quot_cw_ws *quot_mr dom_gen

                dup 0
                // _ *base_mr *p_iter padded_height *ext_mr *quot_cw_ws *quot_mr dom_gen dom_gen

                call {sample_scalar_one}
                // _ *base_mr *p_iter padded_height *ext_mr *quot_cw_ws *quot_mr dom_gen dom_gen [ood_curr_row]

                call {calculate_out_of_domain_points}
                // _ *base_mr *p_iter padded_height *ext_mr *quot_cw_ws *quot_mr dom_gen *ood_points


                /* out-of-domain quotient summands */
                push 2
                add
                read_mem {EXTENSION_DEGREE}
                push 1
                add
                // _ *base_mr *p_iter padded_height *ext_mr *quot_cw_ws *quot_mr dom_gen [out_of_domain_curr_row] *ood_points

                swap 8
                // _ *base_mr *p_iter *ood_points *ext_mr *quot_cw_ws *quot_mr dom_gen [out_of_domain_curr_row] padded_height

                swap 1
                swap 2
                swap 3
                swap 4
                // _ *base_mr *p_iter *ood_points *ext_mr *quot_cw_ws *quot_mr [out_of_domain_curr_row] padded_height dom_gen

                call {quotient_summands}
                // _ *base_mr *p_iter *ood_points *ext_mr *quot_cw_ws *quot_mr *quotient_summands


                /* Dequeue out-of-domain row */
                dup 5
                call {next_as_outofdomainbaserow}

                dup 6
                call {next_as_outofdomainextrow}

                dup 7
                call {next_as_outofdomainbaserow}

                dup 8
                call {next_as_outofdomainextrow}

                dup 9
                call {next_as_outofdomainquotientsegments}
                // _ *base_mr *p_iter *ood_points *ext_mr *quot_cw_ws *quot_mr *quotient_summands *ood_base_row_curr *ood_ext_row_curr *odd_base_row_next *ood_ext_row_next *ood_quotient_segments


                /* Calculate `sum_of_evaluated_out_of_domain_quotient_segments` */
                dup 9
                {&OutOfDomainPoints::read_ood_point(OodPoint::CurrentRow)}
                // _ *base_mr *p_iter *ood_points *ext_mr *quot_cw_ws *quot_mr *quotient_summands *ood_base_row_curr *ood_ext_row_curr *odd_base_row_next *ood_ext_row_next *ood_quotient_segments [ood_curr_row]

                call {horner_evaluation_of_ood_curr_row_quot_segments}
                // _ *base_mr *p_iter *ood_points *ext_mr *quot_cw_ws *quot_mr *quotient_summands *ood_base_row_curr *ood_ext_row_curr *odd_base_row_next *ood_ext_row_next [sum_of_evaluated_out_of_domain_quotient_segments]


                /* Calculate inner product `out_of_domain_quotient_value` */
                swap 2
                swap 4
                swap 9
                swap 1
                swap 3
                swap 7
                // _ *base_mr *p_iter *ood_points *ext_mr *odd_base_row_next *quot_mr *ood_ext_row_next *ood_base_row_curr *ood_ext_row_curr [sum_of_evaluated_out_of_domain_quotient_segments] *quot_cw_ws *quotient_summands

                call {inner_product_quotient_summands}
                // _ *base_mr *p_iter *ood_points *ext_mr *odd_base_row_next *quot_mr *ood_ext_row_next *ood_base_row_curr *ood_ext_row_curr [sum_of_evaluated_out_of_domain_quotient_segments] [out_of_domain_quotient_value]


                /* Verify quotient's segments */
                {&assert_top_two_xfes_eq}
                // _ *base_mr *p_iter *ood_points *ext_mr *odd_base_row_next *quot_mr *ood_ext_row_next *ood_base_row_curr *ood_ext_row_curr

                return
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

    use super::*;

    #[test]
    fn verify_tvm_proof_factorial_program() {
        const FACTORIAL_ARGUMENT: u32 = 3;
        let factorial_program = factorial_program_with_io();
        let (mut non_determinism, claim_for_proof, inner_padded_height) =
            non_determinism_claim_and_padded_height(
                &factorial_program,
                &[FACTORIAL_ARGUMENT.into()],
                NonDeterminism::default(),
            );

        // Insert `claim` into standard memory, since that's how the interface is defined
        // In any real setting, you probably want to use the above snippet as an inner function,
        // and instead call an entrypoint that puts the claim into memory and passes a pointer to
        // the above snippet.
        let (claim_pointer, claim_size) =
            insert_claim_into_static_memory(&mut non_determinism.ram, claim_for_proof);

        let snippet = StarkVerify {
            stark_parameters: Stark::default(),
            log_2_padded_height: None,
        };
        let mut init_stack = [snippet.init_stack_for_isolated_run(), vec![claim_pointer]].concat();
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
            "Clock cycle count of TASM-verifier of factorial({FACTORIAL_ARGUMENT}): {}.\nInner padded height was: {}",
            final_tasm_state.cycle_count,
            inner_padded_height,
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

    pub fn non_determinism_claim_and_padded_height(
        inner_program: &Program,
        inner_public_input: &[BFieldElement],
        inner_nondeterminism: NonDeterminism<BFieldElement>,
    ) -> (
        NonDeterminism<BFieldElement>,
        triton_vm::proof::Claim,
        usize,
    ) {
        // TODO: Delete this function once `u64` types are removed from TVM interface
        fn nd_bf_to_u64(nd: NonDeterminism<BFieldElement>) -> NonDeterminism<u64> {
            let individual_tokens = nd
                .individual_tokens
                .iter()
                .map(|&element| element.into())
                .collect();
            let ram = nd
                .ram
                .iter()
                .map(|(&key, &value)| (key.into(), value.into()))
                .collect();
            NonDeterminism {
                individual_tokens,
                digests: nd.digests.clone(),
                ram,
            }
        }

        println!("Generating proof for non-determinism");
        let (stark, claim, proof) = triton_vm::prove_program(
            inner_program,
            &inner_public_input.iter().map(|x| x.value()).collect_vec(),
            &nd_bf_to_u64(inner_nondeterminism),
        )
        .unwrap();
        println!("Done generating proof for non-determinism");

        assert!(
            triton_vm::verify(stark, &claim, &proof),
            "Proof from TVM must verify through TVM"
        );

        let fri = stark.derive_fri(proof.padded_height().unwrap()).unwrap();
        let proof_stream = StarkProofStream::try_from(&proof).unwrap();
        let proof_extraction = extract_fri_proof(&proof_stream, &claim, stark);
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
            claim,
            padded_height,
        )
    }
}
