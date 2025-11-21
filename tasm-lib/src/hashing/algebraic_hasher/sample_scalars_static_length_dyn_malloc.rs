use triton_vm::prelude::*;
use twenty_first::math::x_field_element::EXTENSION_DEGREE;
use twenty_first::tip5::RATE;

use crate::data_type::ArrayType;
use crate::hashing::squeeze_repeatedly_static_number::SqueezeRepeatedlyStaticNumber;
use crate::prelude::*;

/// Squeeze the sponge to sample a given number of `XFieldElement`s.
#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub struct SampleScalarsStaticLengthDynMalloc {
    pub num_elements: usize,
}

impl SampleScalarsStaticLengthDynMalloc {
    pub(super) fn num_squeezes(num_elements: usize) -> usize {
        (num_elements * EXTENSION_DEGREE).div_ceil(RATE)
    }
}

impl BasicSnippet for SampleScalarsStaticLengthDynMalloc {
    fn parameters(&self) -> Vec<(DataType, String)> {
        vec![]
    }

    fn return_values(&self) -> Vec<(DataType, String)> {
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
            "tasmlib_hashing_algebraic_hasher_sample_scalars_static_length_dyn_malloc_{}",
            self.num_elements
        )
    }

    fn code(&self, library: &mut Library) -> Vec<LabelledInstruction> {
        assert_eq!(10, RATE, "Code assumes Tip5's RATE is 10");
        assert_eq!(3, EXTENSION_DEGREE, "Code assumes extension degree 3");
        let num_squeezes = Self::num_squeezes(self.num_elements);

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
    use twenty_first::prelude::*;

    use super::*;
    use crate::memory::dyn_malloc::DYN_MALLOC_FIRST_ADDRESS;
    use crate::rust_shadowing_helper_functions;
    use crate::test_helpers::tasm_final_state;
    use crate::test_prelude::*;

    impl Procedure for SampleScalarsStaticLengthDynMalloc {
        fn rust_shadow(
            &self,
            stack: &mut Vec<BFieldElement>,
            memory: &mut std::collections::HashMap<BFieldElement, BFieldElement>,
            _nondeterminism: &NonDeterminism,
            _public_input: &[BFieldElement],
            sponge: &mut Option<Tip5>,
        ) -> Vec<BFieldElement> {
            let sponge = sponge.as_mut().expect("sponge must be initialized");
            let num_squeezes = Self::num_squeezes(self.num_elements);
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
    }

    #[test]
    fn sample_scalars_static_length_pbt() {
        for i in 0..100 {
            ShadowedProcedure::new(SampleScalarsStaticLengthDynMalloc { num_elements: i }).test();
        }
    }

    #[test]
    fn verify_agreement_with_tip5_sample_scalars() {
        let empty_sponge = Tip5::init();
        let mut non_empty_sponge = Tip5::init();
        non_empty_sponge.absorb([BFieldElement::new(100); Tip5::RATE]);

        for init_sponge in [empty_sponge, non_empty_sponge] {
            for num_elements in 0..30 {
                let snippet = SampleScalarsStaticLengthDynMalloc { num_elements };
                let init_stack = snippet.init_stack_for_isolated_run();
                let tasm = tasm_final_state(
                    &ShadowedProcedure::new(snippet),
                    &init_stack,
                    &[],
                    NonDeterminism::default(),
                    &Some(init_sponge.clone()),
                );

                let final_ram = tasm.ram;
                let snippet_output_scalar_pointer =
                    tasm.op_stack.stack[tasm.op_stack.stack.len() - 1];

                let scalars_from_tip5 =
                    Tip5::sample_scalars(&mut init_sponge.clone(), num_elements);

                for (i, expected_scalar) in scalars_from_tip5.into_iter().enumerate() {
                    assert_eq!(
                        expected_scalar.coefficients.to_vec(),
                        rust_shadowing_helper_functions::array::array_get(
                            snippet_output_scalar_pointer,
                            i,
                            &final_ram,
                            EXTENSION_DEGREE,
                        )
                    );
                }
            }
        }
    }
}

#[cfg(test)]
mod bench {
    use super::*;
    use crate::test_prelude::*;

    #[test]
    fn bench_10() {
        ShadowedProcedure::new(SampleScalarsStaticLengthDynMalloc { num_elements: 10 }).bench();
    }

    #[test]
    fn bench_100() {
        ShadowedProcedure::new(SampleScalarsStaticLengthDynMalloc { num_elements: 100 }).bench();
    }

    #[test]
    fn bench_63() {
        ShadowedProcedure::new(SampleScalarsStaticLengthDynMalloc { num_elements: 63 }).bench();
    }
}
