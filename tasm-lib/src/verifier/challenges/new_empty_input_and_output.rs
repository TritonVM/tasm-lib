use num::One;
use triton_vm::air::challenge_id::ChallengeId;
use triton_vm::air::cross_table_argument::CrossTableArg;
use triton_vm::air::cross_table_argument::EvalArg;
use triton_vm::prelude::*;
use twenty_first::math::x_field_element::EXTENSION_DEGREE;

use crate::hashing::algebraic_hasher::sample_scalars_static_length_static_pointer::SampleScalarsStaticLengthStaticPointer;
use crate::prelude::*;
use crate::verifier::challenges::shared::challenges_data_type;
use crate::verifier::eval_arg::compute_terminal_const_sized_static_symbols::ComputeTerminalConstSizedStaticSymbols;
use crate::verifier::eval_arg::compute_terminal_from_digest::ComputeTerminalFromDigestInitialIsOne;

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub struct NewEmptyInputAndOutput {
    // claim_input: Vec<BFieldElement>,
    // claim_output: Vec<BFieldElement>,
    num_of_fiat_shamir_challenges: usize,
    num_of_claim_derived_challenges: usize,

    /// Memory address to store the challenges
    pub challenges_pointer: BFieldElement,
}

impl NewEmptyInputAndOutput {
    pub fn new(
        num_challenges_to_sample: usize,
        num_challenges_to_compute: usize,
        challenges_pointer: BFieldElement,
    ) -> Self {
        Self {
            num_of_fiat_shamir_challenges: num_challenges_to_sample,
            num_of_claim_derived_challenges: num_challenges_to_compute,
            challenges_pointer,
        }
    }

    fn total_number_of_challenges_returned(&self) -> usize {
        self.num_of_fiat_shamir_challenges + self.num_of_claim_derived_challenges
    }

    fn sample_scalars_snippet(&self) -> SampleScalarsStaticLengthStaticPointer {
        SampleScalarsStaticLengthStaticPointer {
            num_elements_to_sample: self.num_of_fiat_shamir_challenges,
            extra_capacity: self.num_of_claim_derived_challenges,
            scalars_pointer: self.challenges_pointer,
        }
    }
}

impl BasicSnippet for NewEmptyInputAndOutput {
    fn parameters(&self) -> Vec<(DataType, String)> {
        vec![(DataType::Digest, "program_digest".to_owned())]
    }

    fn return_values(&self) -> Vec<(DataType, String)> {
        vec![(
            challenges_data_type(self.total_number_of_challenges_returned()),
            "challenges".to_owned(),
        )]
    }

    fn entrypoint(&self) -> String {
        format!(
            "tasmlib_verifier_challenges_new_empty_input_and_output_{}_{}",
            self.num_of_fiat_shamir_challenges, self.num_of_claim_derived_challenges,
        )
    }

    fn code(&self, library: &mut Library) -> Vec<LabelledInstruction> {
        let entrypoint = self.entrypoint();
        let sample_scalars_snippet = self.sample_scalars_snippet();
        let sample_scalars = library.import(Box::new(sample_scalars_snippet));
        let compute_compressed_digest =
            library.import(Box::new(ComputeTerminalFromDigestInitialIsOne));
        let calculate_lookup_terminal =
            library.import(Box::new(ComputeTerminalConstSizedStaticSymbols {
                symbols: tip5::LOOKUP_TABLE.map(|i| BFieldElement::new(i as u64)),
                initial: XFieldElement::one(),
            }));

        let scalars_pointer = self.challenges_pointer;
        let scalar_write_index = |i: ChallengeId| {
            scalars_pointer.value() + u64::try_from(i.index() * EXTENSION_DEGREE).unwrap()
        };
        let scalar_read_index =
            |i: ChallengeId| scalar_write_index(i) + u64::try_from(EXTENSION_DEGREE - 1).unwrap();

        triton_asm!(
            {entrypoint}:
                // _ [program_digest]
                call {sample_scalars}
                // _ [program_digest]

                push {scalar_read_index(ChallengeId::CompressProgramDigestIndeterminate)}
                read_mem {EXTENSION_DEGREE}
                pop 1
                // _ [program_digest; 5] [compress_program_digest_indeterminate; 3]
                dup 7
                dup 7
                dup 7
                dup 7
                dup 7
                // _ [program_digest; 5] [compress_program_digest_indeterminate; 3] [program_digest; 5]

                call {compute_compressed_digest}
                // _ [program_digest; 5] [compressed_digest; 3]

                // **** Write compressed_digest to challenges array ****
                push {scalar_write_index(ChallengeId::CompressedProgramDigest)}
                write_mem {EXTENSION_DEGREE}
                pop 1
                // _ [program_digest; 5]
                pop 5
                // _

                // **** Calculate lookup_terminal ****
                push {scalar_read_index(ChallengeId::LookupTablePublicIndeterminate)}
                read_mem {EXTENSION_DEGREE}
                pop 1
                // _ [lookup_table_public_indeterminate]

                call {calculate_lookup_terminal}
                // _ [lookup_terminal]

                // **** Write lookup_terminal to challenges array ****
                push {scalar_write_index(ChallengeId::LookupTablePublicTerminal)}
                write_mem {EXTENSION_DEGREE}
                pop 1

                // Store `input_terminal`
                push {EvalArg::default_initial().coefficients[2]}
                push {EvalArg::default_initial().coefficients[1]}
                push {EvalArg::default_initial().coefficients[0]}
                push {scalar_write_index(ChallengeId::StandardInputTerminal)}
                write_mem {EXTENSION_DEGREE}
                pop 1

                // Store `output_terminal`
                push {EvalArg::default_initial().coefficients[2]}
                push {EvalArg::default_initial().coefficients[1]}
                push {EvalArg::default_initial().coefficients[0]}
                push {scalar_write_index(ChallengeId::StandardOutputTerminal)}
                write_mem {EXTENSION_DEGREE}
                pop 1
                // _

                push {scalars_pointer}
                // _ *scalars

                return
        )
    }
}

#[cfg(test)]
mod tests {
    use triton_vm::challenges::Challenges;

    use super::*;
    use crate::rust_shadowing_helper_functions::array::insert_as_array;
    use crate::test_prelude::*;
    use crate::verifier::challenges::shared;

    impl Procedure for NewEmptyInputAndOutput {
        fn rust_shadow(
            &self,
            stack: &mut Vec<BFieldElement>,
            memory: &mut std::collections::HashMap<BFieldElement, BFieldElement>,
            _: &NonDeterminism,
            _: &[BFieldElement],
            sponge: &mut Option<Tip5>,
        ) -> Vec<BFieldElement> {
            let sponge = sponge.as_mut().expect("sponge must be initialized");
            let program_digest = pop_encodable(stack);

            let claim = Claim::new(program_digest);
            let challenges = sponge.sample_scalars(self.num_of_fiat_shamir_challenges);
            let challenges = Challenges::new(challenges, &claim);
            dbg!(
                "Rust-shadowing challenges: {}",
                challenges.challenges.iter().join("\n")
            );

            stack.push(self.challenges_pointer);

            insert_as_array(
                self.challenges_pointer,
                memory,
                challenges.challenges.to_vec(),
            );

            vec![]
        }

        fn pseudorandom_initial_state(
            &self,
            seed: [u8; 32],
            _: Option<BenchmarkCase>,
        ) -> ProcedureInitialState {
            let mut rng = StdRng::from_seed(seed);
            let program_digest: Digest = rng.random();
            let challenge = program_digest.0.into_iter().rev().collect_vec();
            let stack = [self.init_stack_for_isolated_run(), challenge].concat();
            let sponge = Tip5 {
                state: rng.random(),
            };

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
            num_of_fiat_shamir_challenges: Challenges::COUNT - NUM_OF_CLAIM_DERIVED_CHALLENGES,
            challenges_pointer: shared::conventional_challenges_pointer(),
        })
        .test();
    }
}

#[cfg(test)]
mod benches {
    use triton_vm::challenges::Challenges;

    use super::*;
    use crate::test_prelude::*;
    use crate::verifier::challenges::shared;

    #[test]
    fn benchmark() {
        const NUM_OF_CLAIM_DERIVED_CHALLENGES: usize = 4;
        ShadowedProcedure::new(NewEmptyInputAndOutput {
            num_of_claim_derived_challenges: 4,
            num_of_fiat_shamir_challenges: Challenges::COUNT - NUM_OF_CLAIM_DERIVED_CHALLENGES,
            challenges_pointer: shared::conventional_challenges_pointer(),
        })
        .bench();
    }
}
