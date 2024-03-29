use triton_vm::prelude::*;
use triton_vm::proof_item::ProofItemVariant;

use crate::data_type::DataType;
use crate::library::Library;
use crate::recufier::challenges::shared::conventional_challenges_pointer;
use crate::recufier::claim::instantiate_fiat_shamir_with_claim::InstantiateFiatShamirWithClaim;
use crate::recufier::claim::shared::claim_type;
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
        let derive_fri_parameters = library.import(Box::new(
            fri::derive_from_stark_params::DeriveFriFromStarkParams {
                stark_parameters: self.stark_parameters,
            },
        ));
        let get_challenges = library.import(Box::new(
            challenges::new_generic_dyn_claim::NewGenericDynClaim::conventional_with_tvm_parameters(
            ),
        ));

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

        triton_asm!(
            {entrypoint}:
                sponge_init

                // _ *claim

                dup 0
                call {instantiate_fiat_shamir_with_claim}
                // _ *claim

                call {new_proof_iter}
                // _ *claim *proof_iter

                call {next_as_log_2_padded_height}
                // _ *claim *proof_iter log_2_padded_height

                {&verify_log_2_padded_height}
                // _ *claim *proof_iter log_2_padded_height

                push 2
                pow
                // _ *claim *proof_iter padded_height

                call {next_as_merkleroot}
                // _ *claim *proof_iter padded_height *base_merkle_root

                swap 3
                call {get_challenges}
                // _ *base_merkle_root *proof_iter padded_height *challenges

                {&verify_challenges_pointer}
                // _ *base_merkle_root *proof_iter padded_height

                // dup 0
                // call {derive_fri_parameters}
                // // _ *claim *proof_iter padded_height *fri



                return
        )
    }
}

#[cfg(test)]
pub mod tests {
    use itertools::Itertools;
    use tests::fri::verify::test::extract_fri_proof;
    use triton_vm::stark::StarkProofStream;

    use crate::traits::procedure::Procedure;

    use super::*;

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
        todo!()
    }
}
