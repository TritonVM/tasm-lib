use num::One;
use triton_vm::air::challenge_id::ChallengeId;
use triton_vm::air::cross_table_argument::CrossTableArg;
use triton_vm::air::cross_table_argument::EvalArg;
use triton_vm::challenges::Challenges;
use triton_vm::prelude::*;
use twenty_first::math::x_field_element::EXTENSION_DEGREE;

use crate::hashing::algebraic_hasher::sample_scalars_static_length_static_pointer::SampleScalarsStaticLengthStaticPointer;
use crate::prelude::*;
use crate::verifier::challenges::shared::challenges_data_type;
use crate::verifier::claim::shared::claim_type;
use crate::verifier::eval_arg::compute_terminal_const_sized_static_symbols::ComputeTerminalConstSizedStaticSymbols;
use crate::verifier::eval_arg::compute_terminal_dyn_sized_dynamic_symbols::ComputeTerminalDynSizedDynamicSymbols;
use crate::verifier::eval_arg::compute_terminal_from_digest::ComputeTerminalFromDigestInitialIsOne;

/// Calculate a `Challenges` structure from a claim that is only known at runtime
#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub struct NewGenericDynClaim {
    num_of_fiat_shamir_challenges: usize,
    num_of_claim_derived_challenges: usize,

    /// Memory address to store the challenges
    pub challenges_pointer: BFieldElement,
}

impl NewGenericDynClaim {
    /// An instantiation of this snippet that contains the same number of
    /// challenges that TVM uses in its STARK engine
    pub fn tvm_challenges(challenges_pointer: BFieldElement) -> Self {
        Self {
            num_of_fiat_shamir_challenges: Challenges::SAMPLE_COUNT,
            num_of_claim_derived_challenges: ChallengeId::NUM_DERIVED_CHALLENGES,
            challenges_pointer,
        }
    }

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

impl BasicSnippet for NewGenericDynClaim {
    fn parameters(&self) -> Vec<(DataType, String)> {
        vec![(DataType::StructRef(claim_type()), "*claim".to_owned())]
    }

    fn return_values(&self) -> Vec<(DataType, String)> {
        vec![(
            challenges_data_type(self.total_number_of_challenges_returned()),
            "challenges".to_owned(),
        )]
    }

    fn entrypoint(&self) -> String {
        format!(
            "tasmlib_verifier_challenges_new_generic_dyn_claim_{}_{}",
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
        let calculate_io_terminal = library.import(Box::new(ComputeTerminalDynSizedDynamicSymbols));

        let scalars_pointer = self.challenges_pointer;
        let scalar_write_index = |i: ChallengeId| {
            scalars_pointer.value() + u64::try_from(i.index() * EXTENSION_DEGREE).unwrap()
        };
        let scalar_read_index =
            |i: ChallengeId| scalar_write_index(i) + u64::try_from(EXTENSION_DEGREE - 1).unwrap();

        let default_initial = EvalArg::default_initial();

        triton_asm!(
            {entrypoint}:
                // _ *claim
                call {sample_scalars}
                // _ *claim

                // Calculate and store `output_terminal`:
                push {scalar_read_index(ChallengeId::StandardOutputIndeterminate)}
                read_mem {EXTENSION_DEGREE}
                pop 1
                // _ *claim [standard_output_indeterminate; 3]

                push {default_initial.coefficients[2]}
                push {default_initial.coefficients[1]}
                push {default_initial.coefficients[0]}
                // _ *claim [standard_output_indeterminate; 3] [initial; 3]

                dup 6
                push 1
                add
                // _ *claim [standard_output_indeterminate; 3] [initial; 3] *outputs

                call {calculate_io_terminal}
                // _ *claim [standard_output_terminal; 3]

                push {scalar_write_index(ChallengeId::StandardOutputTerminal)}
                write_mem {EXTENSION_DEGREE}
                pop 1
                // _ *claim

                // Calculate and store `input_terminal`:
                push {scalar_read_index(ChallengeId::StandardInputIndeterminate)}
                read_mem {EXTENSION_DEGREE}
                pop 1
                // _ *claim [standard_input_indeterminate; 3]

                push {default_initial.coefficients[2]}
                push {default_initial.coefficients[1]}
                push {default_initial.coefficients[0]}
                // _ *claim [standard_input_indeterminate; 3] [initial; 3]

                dup 6
                read_mem 1
                push 3
                add
                add
                // _ *claim [standard_input_indeterminate; 3] [initial; 3] *inputs

                dup 0
                swap 8
                pop 1
                // _ *inputs [standard_input_indeterminate; 3] [initial; 3] *inputs

                call {calculate_io_terminal}
                // _ *inputs [standard_input_terminal; 3]

                push {scalar_write_index(ChallengeId::StandardInputTerminal)}
                write_mem {EXTENSION_DEGREE}
                pop 1
                // _ *inputs

                read_mem 1
                // _ inputs_len (*inputs-1)

                addi {1 + 1 + Digest::LEN} // skip field `version` (of length 1)
                add
                // _ *digest_last_word

                push {scalar_read_index(ChallengeId::CompressProgramDigestIndeterminate)}
                read_mem {EXTENSION_DEGREE}
                pop 1
                // _ *digest_last_word [compress_program_digest_indeterminate; 3]

                dup 3
                read_mem {Digest::LEN}
                pop 1
                // _ *digest_last_word [compress_program_digest_indeterminate; 3] [program_digest; 5]

                call {compute_compressed_digest}
                // _ *digest_last_word [compressed_digest; 3]

                // **** Write compressed_digest to challenges array ****
                push {scalar_write_index(ChallengeId::CompressedProgramDigest)}
                write_mem {EXTENSION_DEGREE}
                pop 2
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
    use twenty_first::math::other::random_elements;

    use super::*;
    use crate::rust_shadowing_helper_functions::array::insert_as_array;
    use crate::rust_shadowing_helper_functions::claim::load_claim_from_memory;
    use crate::test_prelude::*;
    use crate::verifier::challenges::shared::conventional_challenges_pointer;

    impl Procedure for NewGenericDynClaim {
        fn rust_shadow(
            &self,
            stack: &mut Vec<BFieldElement>,
            memory: &mut HashMap<BFieldElement, BFieldElement>,
            _nondeterminism: &NonDeterminism,
            _public_input: &[BFieldElement],
            sponge: &mut Option<Tip5>,
        ) -> Vec<BFieldElement> {
            let sponge = sponge.as_mut().expect("sponge must be initialized");
            let claim_pointer = stack.pop().unwrap();
            let claim = load_claim_from_memory(claim_pointer, memory);
            let challenges = sponge.sample_scalars(self.num_of_fiat_shamir_challenges);
            let challenges = Challenges::new(challenges, &claim);

            let challenges_pointer = self.challenges_pointer;
            stack.push(challenges_pointer);

            insert_as_array(challenges_pointer, memory, challenges.challenges.to_vec());

            vec![]
        }

        fn pseudorandom_initial_state(
            &self,
            seed: [u8; 32],
            bench_case: Option<BenchmarkCase>,
        ) -> ProcedureInitialState {
            let mut rng = StdRng::from_seed(seed);
            let (input_length, output_length) = match bench_case {
                Some(BenchmarkCase::CommonCase) => (0, 0),
                Some(BenchmarkCase::WorstCase) => (100, 100),
                None => (rng.random_range(0..1000), rng.random_range(0..1000)),
            };

            let claim = Claim::new(rng.random())
                .with_input(random_elements(input_length))
                .with_output(random_elements(output_length));

            let mut memory = HashMap::default();

            let claim_pointer = rng.random();
            encode_to_memory(&mut memory, claim_pointer, &claim);

            let stack = [self.init_stack_for_isolated_run(), vec![claim_pointer]].concat();
            let sponge = Tip5 {
                state: rng.random(),
            };

            ProcedureInitialState {
                stack,
                sponge: Some(sponge),
                nondeterminism: NonDeterminism::default().with_ram(memory),
                public_input: vec![],
            }
        }
    }

    #[test]
    fn new_challenges_pbt() {
        ShadowedProcedure::new(NewGenericDynClaim::tvm_challenges(
            conventional_challenges_pointer(),
        ))
        .test();
    }
}

#[cfg(test)]
mod benches {
    use super::*;
    use crate::test_prelude::*;
    use crate::verifier::challenges::shared::conventional_challenges_pointer;

    #[test]
    fn bench_for_challenges_calc_for_recufier() {
        ShadowedProcedure::new(NewGenericDynClaim::tvm_challenges(
            conventional_challenges_pointer(),
        ))
        .bench();
    }
}
