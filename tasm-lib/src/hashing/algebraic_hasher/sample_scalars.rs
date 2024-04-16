use triton_vm::prelude::*;
use triton_vm::twenty_first::shared_math::x_field_element::EXTENSION_DEGREE;

use crate::data_type::DataType;
use crate::hashing::squeeze_repeatedly::SqueezeRepeatedly;
use crate::list::new::New;
use crate::list::set_length::SetLength;
use crate::traits::basic_snippet::BasicSnippet;

/// Squeeze the sponge to sample a given number of `XFieldElement`s.
#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
pub struct SampleScalars;

impl BasicSnippet for SampleScalars {
    fn inputs(&self) -> Vec<(DataType, String)> {
        vec![(DataType::U32, "num_scalars".to_string())]
    }

    fn outputs(&self) -> Vec<(DataType, String)> {
        vec![(
            DataType::List(Box::new(DataType::Xfe)),
            "*scalars".to_string(),
        )]
    }

    fn entrypoint(&self) -> String {
        "tasmlib_hashing_algebraic_hasher_sample_scalars".to_string()
    }

    fn code(&self, library: &mut crate::library::Library) -> Vec<LabelledInstruction> {
        assert_eq!(10, tip5::RATE, "Code assumes Tip5's RATE is 10");
        assert_eq!(3, EXTENSION_DEGREE, "Code assumes extension degree 3");

        let entrypoint = self.entrypoint();
        let set_length = library.import(Box::new(SetLength::new(DataType::Xfe)));
        let new_list_of_xfes = library.import(Box::new(New::new(DataType::Xfe)));
        let safety_offset = 1;
        let squeeze_repeatedly = library.import(Box::new(SqueezeRepeatedly));
        triton_asm! {
            // BEFORE: _ num_scalars
            // AFTER:  _ *scalars
            {entrypoint}:
                call {new_list_of_xfes}
                                // _ num_scalars *scalars

                // set length
                dup 1           // _ num_scalars *scalars num_scalars
                call {set_length}
                                // _ num_scalars *scalars

                // calculate number of squeezes
                dup 1           // _ num_scalars *scalars num_scalars
                push {EXTENSION_DEGREE} mul

                // _ num_scalars *scalars num_bfes
                push 9 add      // _ num_scalars *scalars (num_bfes+9)
                push {tip5::RATE} swap 1
                                // _ num_scalars *scalars rate (num_bfes+9)
                div_mod pop 1   // _ num_scalars *scalars floor((num_bfes+9)/rate)
                                // _ num_scalars *scalars num_squeezes

                // prepare stack for call to squeeze_repeatedly
                dup 1
                push {safety_offset}
                add
                swap 1          // _ num_scalars *scalars (*scalars+so) num_squeezes

                // squeeze
                call {squeeze_repeatedly}
                                // _ num_scalars *scalars *scalars' 0

                // clean up stack
                pop 2
                swap 1
                pop 1           // _ *scalars
                return

        }
    }
}

#[cfg(test)]
mod test {
    use std::collections::HashMap;

    use itertools::Itertools;
    use rand::rngs::StdRng;
    use rand::{Rng, SeedableRng};
    use triton_vm::program::NonDeterminism;
    use triton_vm::twenty_first::shared_math::b_field_element::BFieldElement;
    use triton_vm::twenty_first::shared_math::tip5::{self, Tip5};
    use triton_vm::twenty_first::shared_math::x_field_element::{XFieldElement, EXTENSION_DEGREE};
    use triton_vm::twenty_first::util_types::algebraic_hasher::AlgebraicHasher;

    use crate::memory::dyn_malloc::DYN_MALLOC_FIRST_ADDRESS;
    use crate::memory::encode_to_memory;
    use crate::snippet_bencher::BenchmarkCase;
    use crate::test_helpers::tasm_final_state;
    use crate::traits::basic_snippet::BasicSnippet;
    use crate::traits::procedure::{Procedure, ProcedureInitialState, ShadowedProcedure};
    use crate::traits::rust_shadow::RustShadow;
    use crate::twenty_first::prelude::Sponge;
    use crate::{empty_stack, rust_shadowing_helper_functions, VmHasher};

    use super::SampleScalars;

    impl Procedure for SampleScalars {
        fn rust_shadow(
            &self,
            stack: &mut Vec<BFieldElement>,
            memory: &mut HashMap<BFieldElement, BFieldElement>,
            _nondeterminism: &NonDeterminism<BFieldElement>,
            _public_input: &[BFieldElement],
            sponge: &mut Option<VmHasher>,
        ) -> Vec<BFieldElement> {
            let sponge = sponge.as_mut().expect("sponge must be initialized");
            let num_scalars = stack.pop().unwrap().value() as usize;
            let num_squeezes = (num_scalars * 3 + 9) / tip5::RATE;
            let pseudorandomness = (0..num_squeezes)
                .flat_map(|_| sponge.squeeze().to_vec())
                .collect_vec();
            let scalars = pseudorandomness
                .chunks(3)
                .take(num_scalars)
                .map(|ch| XFieldElement::new(ch.try_into().unwrap()))
                .collect_vec();
            let scalars_pointer = DYN_MALLOC_FIRST_ADDRESS;

            encode_to_memory(memory, scalars_pointer, scalars);

            // store all pseudorandomness (not just sampled scalars) to memory
            let safety_offset = BFieldElement::new(1);
            for (i, pr) in pseudorandomness.iter().enumerate() {
                memory.insert(
                    BFieldElement::new(i as u64) + scalars_pointer + safety_offset,
                    *pr,
                );
            }

            // the list of scalars was allocated properly; reflect that fact
            memory.insert(scalars_pointer, BFieldElement::new(num_scalars as u64));

            stack.push(scalars_pointer);
            vec![]
        }

        fn pseudorandom_initial_state(
            &self,
            seed: [u8; 32],
            bench_case: Option<crate::snippet_bencher::BenchmarkCase>,
        ) -> ProcedureInitialState {
            let mut rng: StdRng = SeedableRng::from_seed(seed);
            let num_scalars = match bench_case {
                Some(BenchmarkCase::CommonCase) => 10,
                Some(BenchmarkCase::WorstCase) => 100,
                None => rng.gen_range(0..40),
            };
            let mut stack = empty_stack();
            stack.push(BFieldElement::new(num_scalars as u64));
            let sponge = Tip5 { state: rng.gen() };

            ProcedureInitialState {
                stack,
                sponge: Some(sponge),
                ..Default::default()
            }
        }

        fn corner_case_initial_states(&self) -> Vec<ProcedureInitialState> {
            let zero_to_twenty_scalars_empty_sponge = (0..20)
                .map(|num_scalars| {
                    let mut stack = empty_stack();
                    stack.push(BFieldElement::new(num_scalars as u64));
                    let sponge = Tip5::init();

                    ProcedureInitialState {
                        stack,
                        sponge: Some(sponge),
                        ..Default::default()
                    }
                })
                .collect_vec();
            let zero_to_twenty_scalars_non_empty_sponge = (0..20)
                .map(|num_scalars| {
                    let mut stack = empty_stack();
                    stack.push(BFieldElement::new(num_scalars as u64));
                    let mut sponge = Tip5::init();
                    sponge.absorb([BFieldElement::new(42); Tip5::RATE]);

                    ProcedureInitialState {
                        stack,
                        sponge: Some(sponge),
                        ..Default::default()
                    }
                })
                .collect_vec();

            [
                zero_to_twenty_scalars_empty_sponge,
                zero_to_twenty_scalars_non_empty_sponge,
            ]
            .concat()
        }
    }

    #[test]
    fn test() {
        ShadowedProcedure::new(SampleScalars).test();
    }

    /// This is a regression test that verifies that this implementation of `sample_scalars`
    /// agrees with Tip5's version from twenty-first. For the bugfix, see:
    /// <https://github.com/Neptune-Crypto/twenty-first/commit/e708b305>
    #[test]
    fn verify_agreement_with_tip5_sample_scalars() {
        let empty_sponge = Tip5::init();
        let mut non_empty_sponge = Tip5::init();
        non_empty_sponge.absorb([BFieldElement::new(100); Tip5::RATE]);

        for init_sponge in [empty_sponge, non_empty_sponge] {
            for num_scalars in 0..30 {
                let init_stack = [
                    SampleScalars.init_stack_for_isolated_run(),
                    vec![BFieldElement::new(num_scalars as u64)],
                ]
                .concat();
                let tasm = tasm_final_state(
                    &ShadowedProcedure::new(SampleScalars),
                    &init_stack,
                    &[],
                    NonDeterminism::default(),
                    &Some(init_sponge.clone()),
                );

                let final_ram = tasm.ram;
                let snippet_output_scalar_pointer =
                    tasm.op_stack.stack[tasm.op_stack.stack.len() - 1];

                let scalars_from_tip5 = Tip5::sample_scalars(&mut init_sponge.clone(), num_scalars);

                for (i, expected_scalar) in scalars_from_tip5.into_iter().enumerate() {
                    assert_eq!(
                        expected_scalar.coefficients.to_vec(),
                        rust_shadowing_helper_functions::list::list_get(
                            snippet_output_scalar_pointer,
                            i,
                            &final_ram,
                            EXTENSION_DEGREE
                        )
                    );
                }
            }
        }
    }
}

#[cfg(test)]
mod bench {
    use crate::traits::procedure::ShadowedProcedure;
    use crate::traits::rust_shadow::RustShadow;

    use super::*;

    #[test]
    fn bench() {
        ShadowedProcedure::new(SampleScalars).bench();
    }
}
