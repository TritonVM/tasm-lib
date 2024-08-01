use num::Zero;
use triton_vm::prelude::*;

use crate::data_type::ArrayType;
use crate::data_type::DataType;
use crate::library::Library;
use crate::memory::load_words_from_memory_pop_pointer;
use crate::traits::basic_snippet::BasicSnippet;

#[allow(dead_code)]
pub struct SumOfBfes {
    length: usize,
}

impl BasicSnippet for SumOfBfes {
    fn inputs(&self) -> Vec<(DataType, String)> {
        vec![(
            DataType::Array(Box::new(ArrayType {
                element_type: DataType::Bfe,
                length: self.length,
            })),
            "*array".to_owned(),
        )]
    }

    fn outputs(&self) -> Vec<(DataType, String)> {
        vec![(DataType::Bfe, "sum".to_owned())]
    }

    fn entrypoint(&self) -> String {
        format!("tasmlib_array_sum_of_{}_bfes", self.length)
    }

    fn code(&self, _library: &mut Library) -> Vec<LabelledInstruction> {
        let entrypoint = self.entrypoint();

        let move_pointer_to_last_word = match self.length {
            0 | 1 => triton_asm!(),
            n => {
                let length_minus_one = n - 1;
                triton_asm!(
                    push {length_minus_one}
                    add
                )
            }
        };

        let load_all_elements_to_stack = load_words_from_memory_pop_pointer(self.length);

        let sum = if self.length.is_zero() {
            triton_asm!(
                push 0
            )
        } else {
            vec![triton_asm!(add); self.length - 1].concat()
        };

        triton_asm!(
            {entrypoint}:
                // _ *array

                {&move_pointer_to_last_word}
                // _ *last_word

                {&load_all_elements_to_stack}
                // _ [elements]

                {&sum}
                // _ sum

                return
        )
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use num::Zero;
    use rand::prelude::*;
    use triton_vm::twenty_first::math::b_field_element::BFieldElement;

    use crate::rust_shadowing_helper_functions::array::insert_random_array;
    use crate::snippet_bencher::BenchmarkCase;
    use crate::traits::function::Function;
    use crate::traits::function::FunctionInitialState;
    use crate::traits::function::ShadowedFunction;
    use crate::traits::rust_shadow::RustShadow;
    use num_traits::ConstZero;

    use super::*;

    impl Function for SumOfBfes {
        fn rust_shadow(
            &self,
            stack: &mut Vec<BFieldElement>,
            memory: &mut HashMap<BFieldElement, BFieldElement>,
        ) {
            let mut array_pointer = stack.pop().unwrap();
            let mut array_quote_unquote = vec![BFieldElement::zero(); self.length];
            for array_elem in array_quote_unquote.iter_mut() {
                memory
                    .get(&array_pointer)
                    .unwrap_or(&BFieldElement::ZERO)
                    .clone_into(array_elem);
                array_pointer.increment();
            }

            let sum = array_quote_unquote.into_iter().sum();

            stack.push(sum);
        }

        fn pseudorandom_initial_state(
            &self,
            seed: [u8; 32],
            _bench_case: Option<BenchmarkCase>,
        ) -> crate::traits::function::FunctionInitialState {
            let mut rng: StdRng = SeedableRng::from_seed(seed);
            let list_pointer = BFieldElement::new(rng.gen());
            self.prepare_state(list_pointer)
        }

        fn corner_case_initial_states(&self) -> Vec<FunctionInitialState> {
            let all_zeros = {
                let mut init_stack = self.init_stack_for_isolated_run();
                init_stack.push(BFieldElement::new(500));
                FunctionInitialState {
                    stack: init_stack,
                    memory: HashMap::default(),
                }
            };

            vec![all_zeros]
        }
    }

    impl SumOfBfes {
        fn prepare_state(&self, array_pointer: BFieldElement) -> FunctionInitialState {
            let mut memory = HashMap::default();
            insert_random_array(&DataType::Bfe, array_pointer, self.length, &mut memory);

            let mut init_stack = self.init_stack_for_isolated_run();
            init_stack.push(array_pointer);
            FunctionInitialState {
                stack: init_stack,
                memory,
            }
        }
    }

    #[test]
    fn sum_bfes_pbt() {
        let snippets = (0..20).chain(100..110).map(|x| SumOfBfes { length: x });
        for test_case in snippets {
            ShadowedFunction::new(test_case).test()
        }
    }
}

#[cfg(test)]
mod benches {
    use crate::traits::function::ShadowedFunction;
    use crate::traits::rust_shadow::RustShadow;

    use super::*;

    #[test]
    fn sum_bfes_bench_100() {
        ShadowedFunction::new(SumOfBfes { length: 100 }).bench();
    }

    #[test]
    fn sum_bfes_bench_200() {
        ShadowedFunction::new(SumOfBfes { length: 200 }).bench();
    }
}
