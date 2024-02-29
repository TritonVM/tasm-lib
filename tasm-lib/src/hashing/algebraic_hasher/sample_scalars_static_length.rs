use triton_vm::prelude::*;
use triton_vm::twenty_first::shared_math::tip5::RATE;
use triton_vm::twenty_first::shared_math::x_field_element::EXTENSION_DEGREE;

use crate::data_type::ArrayType;
use crate::data_type::DataType;
use crate::hashing::squeeze_repeatedly_static_number::SqueezeRepeatedlyStaticNumber;
use crate::library::Library;
use crate::memory::dyn_malloc::DynMalloc;
use crate::traits::basic_snippet::BasicSnippet;

/// Squeeze the sponge to sample a given number of `XFieldElement`s.
#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
pub struct SampleScalarsStaticLength {
    num_elements: usize,
}

impl SampleScalarsStaticLength {
    fn num_squeezes(&self) -> usize {
        (self.num_elements * EXTENSION_DEGREE + RATE - 1) / RATE
    }
}

impl BasicSnippet for SampleScalarsStaticLength {
    fn inputs(&self) -> Vec<(DataType, String)> {
        vec![]
    }

    fn outputs(&self) -> Vec<(DataType, String)> {
        vec![(
            DataType::Array(Box::new(ArrayType {
                element_type: DataType::Xfe,
                length: self.num_elements,
            })),
            "*scalars".to_owned(),
        )]
    }

    fn entrypoint(&self) -> String {
        format!(
            "tasm_hashing_algebraic_hasher_sample_scalars_static_length_{}",
            self.num_elements
        )
    }

    fn code(&self, library: &mut Library) -> Vec<LabelledInstruction> {
        assert_eq!(10, tip5::RATE, "Code assumes Tip5's RATE is 10");
        assert_eq!(3, EXTENSION_DEGREE, "Code assumes extension degree 3");
        let num_squeezes = self.num_squeezes();

        debug_assert!(
            self.num_elements * EXTENSION_DEGREE <= num_squeezes * RATE,
            "need {} elements but getting {}",
            self.num_elements * EXTENSION_DEGREE,
            num_squeezes * RATE
        );

        let entrypoint = self.entrypoint();
        let squeeze_repeatedly_static_number =
            library.import(Box::new(SqueezeRepeatedlyStaticNumber { num_squeezes }));
        let dyn_malloc = library.import(Box::new(DynMalloc));

        triton_asm!(
            {entrypoint}:
                // _

                // Allocate memory for return-array
                call {dyn_malloc}
                // _ *array

                dup 0
                // _ *array *array

                // squeeze
                call {squeeze_repeatedly_static_number}
                // _ *array

                return
        )
    }
}

#[cfg(test)]
mod tests {
    use itertools::Itertools;
    use rand::rngs::StdRng;
    use rand::{Rng, SeedableRng};
    use triton_vm::prelude::*;
    use triton_vm::twenty_first::shared_math::x_field_element::EXTENSION_DEGREE;
    use triton_vm::twenty_first::util_types::algebraic_hasher::{AlgebraicHasher, Sponge};

    use crate::memory::dyn_malloc::DYN_MALLOC_FIRST_ADDRESS;
    use crate::test_helpers::tasm_final_state;
    use crate::traits::basic_snippet::BasicSnippet;
    use crate::traits::procedure::{Procedure, ProcedureInitialState, ShadowedProcedure};
    use crate::traits::rust_shadow::RustShadow;
    use crate::{rust_shadowing_helper_functions, VmHasher};

    use super::SampleScalarsStaticLength;

    impl Procedure for SampleScalarsStaticLength {
        fn rust_shadow(
            &self,
            stack: &mut Vec<BFieldElement>,
            memory: &mut std::collections::HashMap<BFieldElement, BFieldElement>,
            _nondeterminism: &NonDeterminism<BFieldElement>,
            _public_input: &[BFieldElement],
            sponge: &mut Option<VmHasher>,
        ) -> Vec<BFieldElement> {
            let sponge = sponge.as_mut().expect("sponge must be initialized");
            let num_squeezes = self.num_squeezes();
            let pseudorandomness = (0..num_squeezes)
                .flat_map(|_| sponge.squeeze().to_vec())
                .collect_vec();
            let scalars_pointer = DYN_MALLOC_FIRST_ADDRESS;
            stack.push(scalars_pointer);

            // store all pseudorandomness (not just sampled scalars) to memory
            for (i, pr) in pseudorandomness.iter().enumerate() {
                memory.insert(BFieldElement::new(i as u64) + scalars_pointer, *pr);
            }

            vec![]
        }

        fn pseudorandom_initial_state(
            &self,
            seed: [u8; 32],
            _bench_case: Option<crate::snippet_bencher::BenchmarkCase>,
        ) -> crate::traits::procedure::ProcedureInitialState {
            let mut rng: StdRng = SeedableRng::from_seed(seed);
            let stack = self.init_stack_for_isolated_run();
            let sponge = Tip5 { state: rng.gen() };

            ProcedureInitialState {
                stack,
                sponge: Some(sponge),
                ..Default::default()
            }
        }
    }

    #[test]
    fn sample_scalars_static_length_pbt() {
        for i in 0..100 {
            ShadowedProcedure::new(SampleScalarsStaticLength { num_elements: i }).test();
        }
    }

    #[test]
    fn verify_agreement_with_tip5_sample_scalars() {
        let empty_sponge = Tip5::init();
        let mut non_empty_sponge = Tip5::init();
        non_empty_sponge.absorb([BFieldElement::new(100); Tip5::RATE]);

        for init_sponge in [empty_sponge, non_empty_sponge] {
            for num_elements in 0..30 {
                let snippet = SampleScalarsStaticLength { num_elements };
                let init_stack = snippet.init_stack_for_isolated_run();
                let tasm = tasm_final_state(
                    &ShadowedProcedure::new(snippet),
                    &init_stack,
                    &[],
                    NonDeterminism::default(),
                    &Some(init_sponge.clone()),
                );

                let final_ram = tasm.final_ram;
                let snippet_output_scalar_pointer = tasm.final_stack[tasm.final_stack.len() - 1];

                let scalars_from_tip5 =
                    Tip5::sample_scalars(&mut init_sponge.clone(), num_elements);

                for (i, expected_scalar) in scalars_from_tip5.into_iter().enumerate() {
                    assert_eq!(
                        expected_scalar.coefficients.to_vec(),
                        rust_shadowing_helper_functions::array::array_get(
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
    fn bench_10() {
        ShadowedProcedure::new(SampleScalarsStaticLength { num_elements: 10 }).bench();
    }

    #[test]
    fn bench_100() {
        ShadowedProcedure::new(SampleScalarsStaticLength { num_elements: 100 }).bench();
    }

    #[test]
    fn bench_63() {
        ShadowedProcedure::new(SampleScalarsStaticLength { num_elements: 63 }).bench();
    }
}
