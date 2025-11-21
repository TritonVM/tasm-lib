use num::Zero;
use triton_vm::prelude::*;

use crate::data_type::ArrayType;
use crate::memory::load_words_from_memory_pop_pointer;
use crate::prelude::*;

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub struct SumOfBfes {
    length: usize,
}

impl BasicSnippet for SumOfBfes {
    fn parameters(&self) -> Vec<(DataType, String)> {
        vec![(
            DataType::Array(Box::new(ArrayType {
                element_type: DataType::Bfe,
                length: self.length,
            })),
            "*array".to_owned(),
        )]
    }

    fn return_values(&self) -> Vec<(DataType, String)> {
        vec![(DataType::Bfe, "sum".to_owned())]
    }

    fn entrypoint(&self) -> String {
        format!("tasmlib_array_sum_of_{}_bfes", self.length)
    }

    fn code(&self, _: &mut Library) -> Vec<LabelledInstruction> {
        let move_pointer_to_last_word = match self.length {
            0 | 1 => triton_asm!(),
            n => triton_asm!( addi {n - 1} ),
        };

        let load_all_elements_to_stack = load_words_from_memory_pop_pointer(self.length);

        let sum = if self.length.is_zero() {
            triton_asm!(push 0)
        } else {
            triton_asm![add; self.length - 1]
        };

        triton_asm!(
            {self.entrypoint()}:
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
    use num_traits::ConstZero;

    use super::*;
    use crate::rust_shadowing_helper_functions::array::insert_random_array;
    use crate::test_prelude::*;

    impl Function for SumOfBfes {
        fn rust_shadow(
            &self,
            stack: &mut Vec<BFieldElement>,
            memory: &mut HashMap<BFieldElement, BFieldElement>,
        ) {
            let mut array_pointer = stack.pop().unwrap();
            let mut array_quote_unquote = bfe_vec![0; self.length];
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
            _: Option<BenchmarkCase>,
        ) -> FunctionInitialState {
            self.prepare_state(StdRng::from_seed(seed).random())
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

            let mut stack = self.init_stack_for_isolated_run();
            stack.push(array_pointer);

            FunctionInitialState { stack, memory }
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
    use super::*;
    use crate::test_prelude::*;

    #[test]
    fn sum_bfes_bench_100() {
        ShadowedFunction::new(SumOfBfes { length: 100 }).bench();
    }

    #[test]
    fn sum_bfes_bench_200() {
        ShadowedFunction::new(SumOfBfes { length: 200 }).bench();
    }
}
