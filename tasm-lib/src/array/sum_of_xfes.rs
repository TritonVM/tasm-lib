use num::Zero;
use triton_vm::prelude::*;
use twenty_first::math::x_field_element::EXTENSION_DEGREE;

use crate::data_type::ArrayType;
use crate::memory::load_words_from_memory_pop_pointer;
use crate::prelude::*;

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub struct SumOfXfes {
    length: usize,
}

impl SumOfXfes {
    fn input_type(&self) -> DataType {
        DataType::Array(Box::new(ArrayType {
            element_type: DataType::Xfe,
            length: self.length,
        }))
    }
}

impl BasicSnippet for SumOfXfes {
    fn parameters(&self) -> Vec<(DataType, String)> {
        vec![(self.input_type(), "*array".to_owned())]
    }

    fn return_values(&self) -> Vec<(DataType, String)> {
        vec![(DataType::Xfe, "sum".to_owned())]
    }

    fn entrypoint(&self) -> String {
        format!("tasmlib_array_sum_of_{}_xfes", self.length)
    }

    fn code(&self, _library: &mut Library) -> Vec<LabelledInstruction> {
        let entrypoint = self.entrypoint();

        let move_pointer_to_last_word = match self.length {
            0 => triton_asm!(),
            n => {
                let word_length_minus_one = EXTENSION_DEGREE * n - 1;
                triton_asm!(
                    push {word_length_minus_one}
                    add
                )
            }
        };

        let load_all_elements_to_stack =
            load_words_from_memory_pop_pointer(EXTENSION_DEGREE * self.length);

        let sum = if self.length.is_zero() {
            triton_asm!(
                push 0
                push 0
                push 0
            )
        } else {
            vec![triton_asm!(xx_add); self.length - 1].concat()
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
    use num::One;
    use num::Zero;
    use num_traits::ConstZero;

    use super::*;
    use crate::rust_shadowing_helper_functions::array::insert_as_array;
    use crate::rust_shadowing_helper_functions::array::insert_random_array;
    use crate::test_prelude::*;

    impl Function for SumOfXfes {
        fn rust_shadow(
            &self,
            stack: &mut Vec<BFieldElement>,
            memory: &mut HashMap<BFieldElement, BFieldElement>,
        ) {
            let mut array_pointer = stack.pop().unwrap();
            let mut array_quote_unquote = vec![XFieldElement::zero(); self.length];
            for array_elem in array_quote_unquote.iter_mut() {
                for word in array_elem.coefficients.iter_mut() {
                    memory
                        .get(&array_pointer)
                        .unwrap_or(&BFieldElement::ZERO)
                        .clone_into(word);
                    array_pointer.increment();
                }
            }

            let sum: XFieldElement = array_quote_unquote.into_iter().sum();

            for word in sum.coefficients.into_iter().rev() {
                stack.push(word);
            }
        }

        fn pseudorandom_initial_state(
            &self,
            seed: [u8; 32],
            _bench_case: Option<BenchmarkCase>,
        ) -> crate::traits::function::FunctionInitialState {
            let mut rng = StdRng::from_seed(seed);
            let list_pointer = BFieldElement::new(rng.random());
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
            let all_ones = {
                let mut init_stack = self.init_stack_for_isolated_run();
                let array_pointer = BFieldElement::new(500);
                init_stack.push(array_pointer);
                let all_ones = vec![XFieldElement::one(); self.length];
                let mut memory = HashMap::default();
                insert_as_array(array_pointer, &mut memory, all_ones);

                FunctionInitialState {
                    stack: init_stack,
                    memory,
                }
            };

            vec![all_zeros, all_ones]
        }
    }

    impl SumOfXfes {
        fn prepare_state(&self, array_pointer: BFieldElement) -> FunctionInitialState {
            let mut memory = HashMap::default();
            insert_random_array(&DataType::Xfe, array_pointer, self.length, &mut memory);

            let mut init_stack = self.init_stack_for_isolated_run();
            init_stack.push(array_pointer);
            FunctionInitialState {
                stack: init_stack,
                memory,
            }
        }
    }

    #[test]
    fn sum_xfes_pbt() {
        let snippets = (0..20).chain(100..110).map(|x| SumOfXfes { length: x });
        for test_case in snippets {
            ShadowedFunction::new(test_case).test()
        }
    }

    #[test]
    fn xfe_array_sum_unit_test() {
        let xfes = vec![
            XFieldElement::new([
                BFieldElement::new(100),
                BFieldElement::zero(),
                BFieldElement::new(10),
            ]),
            XFieldElement::new([
                BFieldElement::new(200),
                BFieldElement::zero(),
                BFieldElement::new(4),
            ]),
        ];

        let expected_sum = XFieldElement::new([
            BFieldElement::new(300),
            BFieldElement::zero(),
            BFieldElement::new(14),
        ]);
        assert_eq!(xfes.iter().cloned().sum::<XFieldElement>(), expected_sum);

        let mut memory = HashMap::default();
        let array_pointer = BFieldElement::new(1u64 << 44);
        insert_as_array(array_pointer, &mut memory, xfes);

        let snippet = SumOfXfes { length: 2 };
        let expected_final_stack = [
            snippet.init_stack_for_isolated_run(),
            expected_sum.coefficients.into_iter().rev().collect_vec(),
        ]
        .concat();
        let init_stack = [snippet.init_stack_for_isolated_run(), vec![array_pointer]].concat();
        test_rust_equivalence_given_complete_state(
            &ShadowedFunction::new(snippet),
            &init_stack,
            &[],
            &NonDeterminism::default().with_ram(memory),
            &None,
            Some(&expected_final_stack),
        );
    }
}

#[cfg(test)]
mod benches {
    use super::*;
    use crate::test_prelude::*;

    #[test]
    fn sum_xfes_bench_100() {
        ShadowedFunction::new(SumOfXfes { length: 100 }).bench();
    }

    #[test]
    fn sum_xfes_bench_200() {
        ShadowedFunction::new(SumOfXfes { length: 200 }).bench();
    }
}
