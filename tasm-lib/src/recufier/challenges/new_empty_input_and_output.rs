use num::One;
use triton_vm::prelude::*;
use triton_vm::table::challenges::ChallengeId;
use triton_vm::table::cross_table_argument::CrossTableArg;
use triton_vm::table::cross_table_argument::EvalArg;
use triton_vm::twenty_first::shared_math::x_field_element::EXTENSION_DEGREE;

use tasm_lib::hashing::algebraic_hasher::sample_scalars_static_length_kmalloc::SampleScalarsStaticLengthKMalloc;

use crate::data_type::ArrayType;
use crate::data_type::DataType;
use crate::data_type::StructType;
use crate::library::Library;
use crate::recufier::eval_arg::compute_terminal_const_sized_static_symbols::ComputeTerminalConstSizedStaticSymbols;
use crate::recufier::eval_arg::compute_terminal_from_digest::ComputeTerminalFromDigestInitialIsOne;
use crate::traits::basic_snippet::BasicSnippet;

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

    fn sample_scalars_snippet(&self) -> SampleScalarsStaticLengthKMalloc {
        SampleScalarsStaticLengthKMalloc {
            num_elements_to_sample: self.num_of_fiat_shamir_challenges,
            extra_capacity: self.num_of_claim_derived_challenges,
        }
    }

    pub fn kmalloc_name(&self) -> String {
        self.sample_scalars_snippet().scalars_kmalloc_name()
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
        let sample_scalars_snippet = self.sample_scalars_snippet();
        let sample_scalars = library.import(Box::new(sample_scalars_snippet));
        let compute_compressed_digest =
            library.import(Box::new(ComputeTerminalFromDigestInitialIsOne));
        let (scalars_pointer, scalars_len) =
            library.get_pub_allocation(&sample_scalars_snippet.scalars_kmalloc_name());
        let calculate_lookup_terminal =
            library.import(Box::new(ComputeTerminalConstSizedStaticSymbols {
                symbols: tip5::LOOKUP_TABLE.map(|i| BFieldElement::new(i as u64)),
                initial: XFieldElement::one(),
            }));

        let scalars_len = usize::try_from(scalars_len).unwrap();
        assert!(scalars_len >= self.total_number_of_challenges_returned() * EXTENSION_DEGREE);

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
    use itertools::Itertools;
    use rand::rngs::StdRng;
    use rand::Rng;
    use rand::SeedableRng;
    use triton_vm::table::challenges::Challenges;
    use triton_vm::twenty_first::util_types::algebraic_hasher::AlgebraicHasher;

    use crate::rust_shadowing_helper_functions::array::insert_as_array;
    use crate::traits::basic_snippet::BasicSnippet;
    use crate::traits::procedure::Procedure;
    use crate::traits::procedure::ProcedureInitialState;
    use crate::traits::procedure::ShadowedProcedure;
    use crate::traits::rust_shadow::RustShadow;
    use crate::VmHasher;

    use super::*;

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

            let scalars_pointer = self
                .sample_scalars_snippet()
                .k_malloc_address_isolated_run();
            stack.push(scalars_pointer);

            insert_as_array(scalars_pointer, memory, challenges.challenges.to_vec());

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
