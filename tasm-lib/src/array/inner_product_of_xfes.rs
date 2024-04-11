use crate::data_type::{ArrayType, DataType};
use crate::library::Library;
use crate::traits::basic_snippet::BasicSnippet;
use triton_vm::prelude::*;
use triton_vm::twenty_first::shared_math::x_field_element::EXTENSION_DEGREE;

pub struct InnerProductOfXfes {
    pub length: usize,
}

impl InnerProductOfXfes {
    pub fn new(length: usize) -> Self {
        Self { length }
    }

    fn argument_type(&self) -> DataType {
        DataType::Array(Box::new(ArrayType {
            element_type: DataType::Xfe,
            length: self.length,
        }))
    }
}

impl BasicSnippet for InnerProductOfXfes {
    fn inputs(&self) -> Vec<(DataType, String)> {
        vec![
            (self.argument_type(), "*a".to_owned()),
            (self.argument_type(), "*b".to_owned()),
        ]
    }

    fn outputs(&self) -> Vec<(DataType, String)> {
        vec![(DataType::Xfe, "inner_product".to_owned())]
    }

    fn entrypoint(&self) -> String {
        format!("tasm_array_inner_product_of_{}_xfes", self.length)
    }

    fn code(&self, _library: &mut Library) -> Vec<LabelledInstruction> {
        let entrypoint = self.entrypoint();

        let move_pointers_to_last_word = match self.length {
            0 => triton_asm!(),
            n => {
                let word_length_minus_one = EXTENSION_DEGREE * n - 1;
                triton_asm!(
                    // _ *a *b

                    push {word_length_minus_one}
                    add
                    // _ *a *b_last_word

                    swap 1
                    push {word_length_minus_one}
                    add
                    // _ *b_last_word *a_last_word
                )
            }
        };

        let acc_one_index = triton_asm!(
            // _ *a *b [acc]

            dup 3
            // _ *a *b [acc] *b

            read_mem 3
            // _ *a *b [acc] [b_elem] *b_prev

            swap 8
            // _ *b_prev *b [acc] [b_elem] *a

            read_mem 3
            // _ *b_prev *b [acc] [b_elem] [a_elem] *a_prev

            swap 10
            pop 1
            // _ *b_prev *a_prev [acc] [b_elem] [a_elem]

            xxmul
            xxadd
            // _ *b_prev *a_prev [acc']
        );

        let accumulate_all_indices = vec![acc_one_index; self.length].concat();

        triton_asm!(
            {entrypoint}:
                // _ *a *b

                {&move_pointers_to_last_word}
                // _ *b_last_word *a_last_word

                push 0
                push 0
                push 0
                // _ *b_last_word *a_last_word [acc]

                {&accumulate_all_indices}
                // _ pointer0 pointer1 [acc]
                // _ pointer0 pointer1 acc2 acc1 acc0

                swap 2
                swap 4
                pop 1
                // _ acc2 pointer1 acc0 acc1

                swap 2
                pop 1
                // _ [acc]

                return
        )
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use itertools::Itertools;
    use num::Zero;
    use rand::rngs::StdRng;
    use rand::Rng;
    use rand::SeedableRng;
    use triton_vm::twenty_first::shared_math::b_field_element::BFIELD_ZERO;

    use crate::rust_shadowing_helper_functions::array::insert_as_array;
    use crate::rust_shadowing_helper_functions::array::insert_random_array;
    use crate::snippet_bencher::BenchmarkCase;
    use crate::test_helpers::test_rust_equivalence_given_complete_state;
    use crate::traits::function::Function;
    use crate::traits::function::FunctionInitialState;
    use crate::traits::function::ShadowedFunction;
    use crate::traits::rust_shadow::RustShadow;

    use super::*;

    impl Function for InnerProductOfXfes {
        fn rust_shadow(
            &self,
            stack: &mut Vec<BFieldElement>,
            memory: &mut HashMap<BFieldElement, BFieldElement>,
        ) {
            let one = BFieldElement::new(1);
            let two = BFieldElement::new(2);
            let three = BFieldElement::new(3);
            let mut array_pointer_b = stack.pop().unwrap();
            let mut array_pointer_a = stack.pop().unwrap();
            let mut acc = XFieldElement::zero();
            for _ in 0..self.length {
                let element_a = XFieldElement::new([
                    memory
                        .get(&array_pointer_a)
                        .unwrap_or(&BFIELD_ZERO)
                        .to_owned(),
                    memory
                        .get(&(array_pointer_a + one))
                        .unwrap_or(&BFIELD_ZERO)
                        .to_owned(),
                    memory
                        .get(&(array_pointer_a + two))
                        .unwrap_or(&BFIELD_ZERO)
                        .to_owned(),
                ]);
                let element_b = XFieldElement::new([
                    memory
                        .get(&array_pointer_b)
                        .unwrap_or(&BFIELD_ZERO)
                        .to_owned(),
                    memory
                        .get(&(array_pointer_b + one))
                        .unwrap_or(&BFIELD_ZERO)
                        .to_owned(),
                    memory
                        .get(&(array_pointer_b + two))
                        .unwrap_or(&BFIELD_ZERO)
                        .to_owned(),
                ]);

                acc += element_a * element_b;
                array_pointer_b += three;
                array_pointer_a += three;
            }

            for word in acc.coefficients.into_iter().rev() {
                stack.push(word);
            }
        }

        fn pseudorandom_initial_state(
            &self,
            seed: [u8; 32],
            _bench_case: Option<BenchmarkCase>,
        ) -> FunctionInitialState {
            let mut rng: StdRng = SeedableRng::from_seed(seed);
            let array_pointer_a = BFieldElement::new(rng.gen());
            let mut array_pointer_b = BFieldElement::new(rng.gen());
            while array_pointer_a.value().abs_diff(array_pointer_b.value())
                < EXTENSION_DEGREE as u64 * self.length as u64
            {
                array_pointer_b = BFieldElement::new(rng.gen());
            }

            self.prepare_state(array_pointer_a, array_pointer_b)
        }

        fn corner_case_initial_states(&self) -> Vec<FunctionInitialState> {
            let all_zeros = {
                let init_stack = [
                    self.init_stack_for_isolated_run(),
                    vec![BFieldElement::new(0), BFieldElement::new(1 << 40)],
                ]
                .concat();
                FunctionInitialState {
                    stack: init_stack,
                    memory: HashMap::default(),
                }
            };

            vec![all_zeros]
        }
    }

    impl InnerProductOfXfes {
        fn prepare_state(
            &self,
            array_pointer_a: BFieldElement,
            array_pointer_b: BFieldElement,
        ) -> FunctionInitialState {
            let mut memory = HashMap::default();
            insert_random_array(&DataType::Xfe, array_pointer_a, self.length, &mut memory);
            insert_random_array(&DataType::Xfe, array_pointer_b, self.length, &mut memory);

            let mut init_stack = self.init_stack_for_isolated_run();
            init_stack.push(array_pointer_a);
            init_stack.push(array_pointer_b);
            FunctionInitialState {
                stack: init_stack,
                memory,
            }
        }
    }

    #[test]
    fn inner_product_of_xfes_pbt() {
        let snippets = (0..20)
            .chain(100..110)
            .map(|x| InnerProductOfXfes { length: x });
        for test_case in snippets {
            ShadowedFunction::new(test_case).test()
        }
    }

    #[test]
    fn inner_product_unit_test() {
        let a = vec![
            XFieldElement::new([
                BFieldElement::new(3),
                BFieldElement::zero(),
                BFieldElement::zero(),
            ]),
            XFieldElement::new([
                BFieldElement::new(5),
                BFieldElement::zero(),
                BFieldElement::zero(),
            ]),
        ];
        let b = vec![
            XFieldElement::new([
                BFieldElement::new(501),
                BFieldElement::zero(),
                BFieldElement::zero(),
            ]),
            XFieldElement::new([
                BFieldElement::new(1003),
                BFieldElement::zero(),
                BFieldElement::zero(),
            ]),
        ];

        let expected_inner_product = XFieldElement::new([
            BFieldElement::new(3 * 501 + 5 * 1003),
            BFieldElement::zero(),
            BFieldElement::zero(),
        ]);
        assert_eq!(
            expected_inner_product,
            a.iter()
                .cloned()
                .zip(b.iter().cloned())
                .map(|(a, b)| a * b)
                .sum::<XFieldElement>()
        );

        let mut memory = HashMap::default();
        let array_pointer_a = BFieldElement::new(1u64 << 44);
        insert_as_array(array_pointer_a, &mut memory, a);
        let array_pointer_b = BFieldElement::new(1u64 << 45);
        insert_as_array(array_pointer_b, &mut memory, b);

        let snippet = InnerProductOfXfes { length: 2 };
        let expected_final_stack = [
            snippet.init_stack_for_isolated_run(),
            expected_inner_product
                .coefficients
                .into_iter()
                .rev()
                .collect_vec(),
        ]
        .concat();
        let init_stack = [
            snippet.init_stack_for_isolated_run(),
            vec![array_pointer_a, array_pointer_b],
        ]
        .concat();
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
    use triton_vm::table::extension_table::Quotientable;
    use triton_vm::table::master_table::{MasterExtTable, NUM_COLUMNS};

    use crate::traits::function::ShadowedFunction;
    use crate::traits::rust_shadow::RustShadow;

    use super::*;

    #[test]
    fn inner_product_xfes_bench_100() {
        ShadowedFunction::new(InnerProductOfXfes { length: 100 }).bench();
    }

    #[test]
    fn inner_product_xfes_bench_200() {
        ShadowedFunction::new(InnerProductOfXfes { length: 200 }).bench();
    }

    #[test]
    fn inner_product_xfes_bench_num_columns_current_tvm() {
        ShadowedFunction::new(InnerProductOfXfes {
            length: NUM_COLUMNS,
        })
        .bench();
    }

    #[test]
    fn inner_product_xfes_bench_num_constraints_current_tvm() {
        ShadowedFunction::new(InnerProductOfXfes {
            length: MasterExtTable::NUM_CONSTRAINTS,
        })
        .bench();
    }
}
