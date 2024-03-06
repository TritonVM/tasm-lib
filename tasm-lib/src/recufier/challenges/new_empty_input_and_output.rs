use num::One;
use triton_vm::prelude::*;
use triton_vm::twenty_first::shared_math::tip5::DIGEST_LENGTH;
use triton_vm::twenty_first::shared_math::x_field_element::EXTENSION_DEGREE;

use crate::data_type::ArrayType;
use crate::data_type::DataType;
use crate::data_type::StructType;
use crate::hashing;
use crate::library::Library;
use crate::recufier::eval_arg::compute_terminal_const_sized_static_symbols::ComputeTerminalConstSizedStaticSymbols;
use crate::recufier::eval_arg::compute_terminal_from_digest::ComputeTerminalFromDigestInitialIsOne;
use crate::traits::basic_snippet::BasicSnippet;
use triton_vm::table::challenges::ChallengeId::*;

pub struct NewEmptyInputAndOutput {
    // claim_input: Vec<BFieldElement>,
    // claim_output: Vec<BFieldElement>,
    num_of_fiat_shamir_challenges: usize,
    num_of_claim_derived_challenges: usize,
}

impl NewEmptyInputAndOutput {
    fn total_number_of_challenges_returned(&self) -> usize {
        self.num_of_fiat_shamir_challenges + self.num_of_claim_derived_challenges
    }

    fn return_type(&self) -> DataType {
        DataType::StructRef(StructType {
            name: "Challenges".to_owned(),
            fields: vec![(
                "challenges".to_owned(),
                DataType::Array(Box::new(ArrayType {
                    element_type: DataType::Xfe,
                    length: self.total_number_of_challenges_returned(),
                })),
            )],
        })
    }
}

impl BasicSnippet for NewEmptyInputAndOutput {
    fn inputs(&self) -> Vec<(DataType, String)> {
        vec![(DataType::Digest, "program_digest".to_owned())]
    }

    fn outputs(&self) -> Vec<(DataType, String)> {
        vec![(self.return_type(), "Challenges".to_owned())]
    }

    fn entrypoint(&self) -> String {
        // let all_static_parameters = [
        //     // self.claim_input.clone(),
        //     // self.claim_output.clone(),
        //     vec![
        //         BFieldElement::new(self.num_of_fiat_shamir_challenges as u64),
        //         BFieldElement::new(self.num_of_claim_derived_challenges as u64),
        //     ],
        // ]
        // .concat();
        // let parameters_as_digest = Tip5::hash_varlen(&all_static_parameters);
        // let digest_first_word = parameters_as_digest.encode()[0];
        format!(
            "tasm_recufier_challenges_new_empty_input_and_output_{}_{}",
            self.num_of_fiat_shamir_challenges, self.num_of_claim_derived_challenges
        )
    }

    fn code(&self, library: &mut Library) -> Vec<LabelledInstruction> {
        let entrypoint = self.entrypoint();
        let sample_scalars = library.import(Box::new(
            hashing::algebraic_hasher::sample_scalars_static_length_dyn_malloc::SampleScalarsStaticLengthDynMalloc {
                num_elements: self.num_of_fiat_shamir_challenges,
            },
        ));
        let compute_compressed_digest =
            library.import(Box::new(ComputeTerminalFromDigestInitialIsOne));
        let program_digest_pointer = library.kmalloc(DIGEST_LENGTH as u32);
        let scalars_pointer_pointer = library.kmalloc(1);
        let digest_length_minus_one = BFieldElement::new(DIGEST_LENGTH as u64 - 1u64);
        let calculate_lookup_terminal =
            library.import(Box::new(ComputeTerminalConstSizedStaticSymbols {
                symbols: tip5::LOOKUP_TABLE.map(|i| BFieldElement::new(i as u64)),
                initial: XFieldElement::one(),
            }));

        triton_asm!(
            {entrypoint}:
                // _ [program_digest]

                push {program_digest_pointer}
                write_mem {DIGEST_LENGTH}
                pop 1
                // _

                // Sample from Fiat-shamir heuristic
                call {sample_scalars}
                // _ *scalars

                // Calculate `compressed_digest`
                push {EXTENSION_DEGREE - 1}
                add
                read_mem {EXTENSION_DEGREE}
                push 1
                add
                // _ [compress_program_digest_indeterminate] *scalars

                push {scalars_pointer_pointer}
                write_mem 1
                pop 1
                // _ [compress_program_digest_indeterminate]

                push {program_digest_pointer + digest_length_minus_one}
                read_mem {DIGEST_LENGTH}
                pop 1
                // _ [compress_program_digest_indeterminate] [program_digest]

                call {compute_compressed_digest}
                // _ [compressed_digest; 3]

                // **** Write compressed_digest to challenges array ****
                push {scalars_pointer_pointer}
                read_mem 1
                pop 1
                // _ [compressed_digest; 3] *scalars

                push {CompressedProgramDigest.index() * EXTENSION_DEGREE}
                add
                // _ [compressed_digest; 3] *scalars[CompressedProgramDigest.index()]

                write_mem {EXTENSION_DEGREE}
                pop 1
                // _

                // **** Calculate lookup_terminal ****
                push {scalars_pointer_pointer}
                read_mem 1
                pop 1
                // _ *scalars

                push {LookupTablePublicIndeterminate.index() * EXTENSION_DEGREE + EXTENSION_DEGREE - 1}
                add
                read_mem {EXTENSION_DEGREE}
                pop 1
                // _ [lookup_table_public_indeterminate]

                call {calculate_lookup_terminal}
                // _ [lookup_terminal]

                // **** Write lookup_terminal to challenges array ****
                push {scalars_pointer_pointer}
                read_mem 1
                pop 1
                push {LookupTablePublicTerminal.index() * EXTENSION_DEGREE}
                add
                write_mem {EXTENSION_DEGREE}
                pop 1

                // Store `input_terminal = 1`
                push 0
                push 0
                push 1
                push {scalars_pointer_pointer}
                read_mem 1
                pop 1
                push {StandardInputTerminal.index() * EXTENSION_DEGREE}
                add
                write_mem {EXTENSION_DEGREE}
                pop 1

                // Store `output_terminal = 1`
                push 0
                push 0
                push 1
                push {scalars_pointer_pointer}
                read_mem 1
                pop 1
                push {StandardOutputTerminal.index() * EXTENSION_DEGREE}
                add
                write_mem {EXTENSION_DEGREE}
                pop 1
                // _

                push {scalars_pointer_pointer}
                read_mem 1
                pop 1
                // _ *scalars

                return
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use itertools::Itertools;
    use rand::rngs::StdRng;
    use rand::{Rng, SeedableRng};
    use triton_vm::table::challenges::Challenges;
    use triton_vm::twenty_first::util_types::algebraic_hasher::AlgebraicHasher;

    use crate::memory::dyn_malloc::DYN_MALLOC_FIRST_ADDRESS;
    use crate::rust_shadowing_helper_functions::array::insert_as_array;
    use crate::traits::basic_snippet::BasicSnippet;
    use crate::traits::procedure::{Procedure, ProcedureInitialState, ShadowedProcedure};
    use crate::traits::rust_shadow::RustShadow;
    use crate::VmHasher;

    impl Procedure for NewEmptyInputAndOutput {
        fn rust_shadow(
            &self,
            stack: &mut Vec<BFieldElement>,
            memory: &mut std::collections::HashMap<BFieldElement, BFieldElement>,
            _nondeterminism: &NonDeterminism<BFieldElement>,
            _public_input: &[BFieldElement],
            sponge: &mut Option<VmHasher>,
        ) -> Vec<BFieldElement> {
            let sponge = sponge.as_mut().expect("sponge must be initialized");
            let program_digest = Digest::new([
                stack.pop().unwrap(),
                stack.pop().unwrap(),
                stack.pop().unwrap(),
                stack.pop().unwrap(),
                stack.pop().unwrap(),
            ]);

            let claim = Claim {
                program_digest,
                input: vec![],
                output: vec![],
            };
            let challenges = sponge.sample_scalars(self.num_of_fiat_shamir_challenges);
            let challenges = Challenges::new(challenges, &claim);
            println!(
                "Rust-shadowing challenges: {}",
                challenges.challenges.iter().join("\n")
            );

            let scalars_pointer = DYN_MALLOC_FIRST_ADDRESS;
            stack.push(scalars_pointer);

            insert_as_array(scalars_pointer, memory, challenges.challenges.to_vec());

            // Emulate behavior of static memory usage
            memory.insert(
                BFieldElement::new(BFieldElement::P - 2),
                program_digest.0[4],
            );
            memory.insert(
                BFieldElement::new(BFieldElement::P - 3),
                program_digest.0[3],
            );
            memory.insert(
                BFieldElement::new(BFieldElement::P - 4),
                program_digest.0[2],
            );
            memory.insert(
                BFieldElement::new(BFieldElement::P - 5),
                program_digest.0[1],
            );
            memory.insert(
                BFieldElement::new(BFieldElement::P - 6),
                program_digest.0[0],
            );
            memory.insert(
                BFieldElement::new(BFieldElement::P - 7),
                DYN_MALLOC_FIRST_ADDRESS,
            );

            vec![]
        }

        fn pseudorandom_initial_state(
            &self,
            seed: [u8; 32],
            _bench_case: Option<crate::snippet_bencher::BenchmarkCase>,
        ) -> ProcedureInitialState {
            let mut rng: StdRng = SeedableRng::from_seed(seed);
            let program_digest: Digest = rng.gen();
            let challenge = program_digest.0.into_iter().rev().collect_vec();
            let stack = [self.init_stack_for_isolated_run(), challenge].concat();
            let sponge = Tip5 { state: rng.gen() };

            ProcedureInitialState {
                stack,
                sponge: Some(sponge),
                ..Default::default()
            }
        }
    }

    #[test]
    fn new_challenge_empty_input_output_pbt() {
        const NUM_OF_CLAIM_DERIVED_CHALLENGES: usize = 4;
        ShadowedProcedure::new(NewEmptyInputAndOutput {
            num_of_claim_derived_challenges: NUM_OF_CLAIM_DERIVED_CHALLENGES,
            num_of_fiat_shamir_challenges: Challenges::count() - NUM_OF_CLAIM_DERIVED_CHALLENGES,
        })
        .test();
    }
}

#[cfg(test)]
mod benches {
    use triton_vm::table::challenges::Challenges;

    use crate::traits::procedure::ShadowedProcedure;
    use crate::traits::rust_shadow::RustShadow;

    use super::*;

    #[test]
    fn bench_for_challenges_calc_for_recufier() {
        const NUM_OF_CLAIM_DERIVED_CHALLENGES: usize = 4;
        ShadowedProcedure::new(NewEmptyInputAndOutput {
            num_of_claim_derived_challenges: 4,
            num_of_fiat_shamir_challenges: Challenges::count() - NUM_OF_CLAIM_DERIVED_CHALLENGES,
        })
        .bench();
    }
}
