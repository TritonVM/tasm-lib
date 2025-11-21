use triton_vm::prelude::*;
use twenty_first::math::x_field_element::EXTENSION_DEGREE;
use twenty_first::tip5::RATE;

use crate::hashing::algebraic_hasher::sample_scalars_static_length_dyn_malloc::SampleScalarsStaticLengthDynMalloc;
use crate::hashing::squeeze_repeatedly_static_number::SqueezeRepeatedlyStaticNumber;
use crate::prelude::*;

/// Squeeze the sponge to sample a given number of [`XFieldElement`]s. Puts the scalars into
/// statically allocated memory.
///
/// # Panics
///
/// Panics if both fields are 0 because the static allocator will be unhappy. :)
#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub struct SampleScalarsStaticLengthKMalloc {
    pub num_elements_to_sample: usize,

    /// Number of additional elements to statically allocate, in number of
    /// [extension field element][xfe]s.
    /// Necessary for [`Challenges`][chall].
    ///
    /// [chall]: crate::verifier::challenges::new_empty_input_and_output::NewEmptyInputAndOutput
    /// [xfe]: XFieldElement
    pub extra_capacity: usize,
}

impl SampleScalarsStaticLengthKMalloc {
    fn num_words_to_allocate(&self) -> u32 {
        ((self.num_elements_to_sample + self.extra_capacity) * EXTENSION_DEGREE)
            .try_into()
            .unwrap()
    }
}

impl BasicSnippet for SampleScalarsStaticLengthKMalloc {
    fn parameters(&self) -> Vec<(DataType, String)> {
        vec![]
    }

    fn return_values(&self) -> Vec<(DataType, String)> {
        vec![]
    }

    fn entrypoint(&self) -> String {
        format!(
            "tasmlib_hashing_algebraic_hasher_sample_scalars_static_length_k_malloc{}",
            self.num_elements_to_sample
        )
    }

    fn code(&self, library: &mut Library) -> Vec<LabelledInstruction> {
        assert_eq!(10, RATE, "Code assumes Tip5's RATE is 10");
        assert_eq!(3, EXTENSION_DEGREE, "Code assumes extension degree 3");
        assert!(
            self.extra_capacity + self.num_elements_to_sample > 0,
            "Must allocate positive number of words"
        );
        let num_squeezes =
            SampleScalarsStaticLengthDynMalloc::num_squeezes(self.num_elements_to_sample);

        let num_squeezed_words = num_squeezes * RATE;
        debug_assert!(
            self.num_elements_to_sample * EXTENSION_DEGREE <= num_squeezed_words,
            "need {} elements but getting {num_squeezed_words}",
            self.num_elements_to_sample * EXTENSION_DEGREE,
        );

        let entrypoint = self.entrypoint();
        let squeeze_repeatedly_static_number =
            library.import(Box::new(SqueezeRepeatedlyStaticNumber { num_squeezes }));
        let scalars_pointer_alloc = library.kmalloc(self.num_words_to_allocate());

        triton_asm!(
            {entrypoint}:
                push {scalars_pointer_alloc.write_address()}
                call {squeeze_repeatedly_static_number}
                return
        )
    }
}

#[cfg(test)]
pub(crate) mod tests {
    use std::ops::Neg;

    use twenty_first::util_types::sponge::Sponge;

    use super::*;
    use crate::rust_shadowing_helper_functions::array::array_get;
    use crate::rust_shadowing_helper_functions::array::insert_as_array;
    use crate::test_helpers::tasm_final_state;
    use crate::test_prelude::*;

    impl SampleScalarsStaticLengthKMalloc {
        /// For testing purposes only.
        pub fn k_malloc_address_isolated_run(&self) -> BFieldElement {
            let dyn_allocator_state_size = 1;
            BFieldElement::from(self.num_words_to_allocate() + dyn_allocator_state_size).neg()
        }
    }

    impl Procedure for SampleScalarsStaticLengthKMalloc {
        fn rust_shadow(
            &self,
            _stack: &mut Vec<BFieldElement>,
            memory: &mut std::collections::HashMap<BFieldElement, BFieldElement>,
            _nondeterminism: &NonDeterminism,
            _public_input: &[BFieldElement],
            sponge: &mut Option<Tip5>,
        ) -> Vec<BFieldElement> {
            let sponge = sponge.as_mut().expect("sponge must be initialized");
            let num_squeezes =
                SampleScalarsStaticLengthDynMalloc::num_squeezes(self.num_elements_to_sample);
            let pseudorandomness = (0..num_squeezes)
                .flat_map(|_| sponge.squeeze().to_vec())
                .collect_vec();
            let scalars_pointer = self.k_malloc_address_isolated_run();
            insert_as_array(scalars_pointer, memory, pseudorandomness);

            vec![]
        }

        fn pseudorandom_initial_state(
            &self,
            seed: [u8; 32],
            _: Option<BenchmarkCase>,
        ) -> ProcedureInitialState {
            let mut rng = StdRng::from_seed(seed);
            let stack = self.init_stack_for_isolated_run();
            let sponge = Tip5 {
                state: rng.random(),
            };

            ProcedureInitialState {
                stack,
                sponge: Some(sponge),
                ..Default::default()
            }
        }

        fn corner_case_initial_states(&self) -> Vec<ProcedureInitialState> {
            let freshly_initialized_sponge = ProcedureInitialState {
                stack: self.init_stack_for_isolated_run(),
                sponge: Some(Tip5::init()),
                ..Default::default()
            };

            vec![freshly_initialized_sponge]
        }
    }

    #[test]
    fn sample_scalars_static_length_pbt() {
        for num_elements_to_sample in 0..11 {
            for extra_capacity in 0..11 {
                if num_elements_to_sample + extra_capacity == 0 {
                    continue;
                }
                ShadowedProcedure::new(SampleScalarsStaticLengthKMalloc {
                    num_elements_to_sample,
                    extra_capacity,
                })
                .test();
            }
        }
    }

    #[proptest]
    fn verify_agreement_with_tip5_sample_scalars(
        #[strategy(1_usize..500)] num_elements_to_sample: usize,
        #[strategy(1_usize..500)] extra_capacity: usize,
        #[strategy(arb())] mut sponge: Tip5,
    ) {
        let snippet = SampleScalarsStaticLengthKMalloc {
            num_elements_to_sample,
            extra_capacity,
        };
        let init_stack = snippet.init_stack_for_isolated_run();
        let tasm = tasm_final_state(
            &ShadowedProcedure::new(snippet),
            &init_stack,
            &[],
            NonDeterminism::default(),
            &Some(sponge.clone()),
        );

        let scalar_pointer = snippet.k_malloc_address_isolated_run();
        let read_scalar = |i| array_get(scalar_pointer, i, &tasm.ram, EXTENSION_DEGREE);

        let scalars_from_tip5 = sponge.sample_scalars(num_elements_to_sample);
        for (i, expected_scalar) in scalars_from_tip5.into_iter().enumerate() {
            assert_eq!(expected_scalar.coefficients.to_vec(), read_scalar(i));
        }
    }
}

#[cfg(test)]
mod bench {
    use super::*;
    use crate::test_prelude::*;

    #[test]
    fn bench_10() {
        ShadowedProcedure::new(SampleScalarsStaticLengthKMalloc {
            num_elements_to_sample: 10,
            extra_capacity: 4,
        })
        .bench();
    }

    #[test]
    fn bench_100() {
        ShadowedProcedure::new(SampleScalarsStaticLengthKMalloc {
            num_elements_to_sample: 100,
            extra_capacity: 4,
        })
        .bench();
    }

    #[test]
    fn bench_63() {
        ShadowedProcedure::new(SampleScalarsStaticLengthKMalloc {
            num_elements_to_sample: 63,
            extra_capacity: 4,
        })
        .bench();
    }
}
