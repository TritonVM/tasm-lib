use triton_vm::prelude::*;

use crate::data_type::ArrayType;
use crate::prelude::*;

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
        format!("tasmlib_array_inner_product_of_{}_xfes", self.length)
    }

    fn code(&self, _library: &mut Library) -> Vec<LabelledInstruction> {
        let entrypoint = self.entrypoint();

        let accumulate_all_indices = triton_asm![xx_dot_step; self.length];

        triton_asm!(
            {entrypoint}:
                // _ *a *b

                push 0
                push 0
                push 0
                // _ *a *b 0 0 0

                pick 4
                pick 4
                // _ 0 0 0 *a *b

                {&accumulate_all_indices}
                // _ acc2 acc1 acc0 *garbage0 *garbage1

                pop 2
                // _ acc2 acc1 acc0

                return
        )
    }
}

#[cfg(test)]
mod tests {
    use num::Zero;
    use num_traits::ConstZero;
    use twenty_first::math::x_field_element::EXTENSION_DEGREE;

    use super::*;
    use crate::rust_shadowing_helper_functions::array::insert_as_array;
    use crate::rust_shadowing_helper_functions::array::insert_random_array;
    use crate::test_prelude::*;

    impl Function for InnerProductOfXfes {
        fn rust_shadow(
            &self,
            stack: &mut Vec<BFieldElement>,
            memory: &mut HashMap<BFieldElement, BFieldElement>,
        ) {
            fn read_xfe(
                memory: &HashMap<BFieldElement, BFieldElement>,
                address: BFieldElement,
            ) -> XFieldElement {
                let coefficients = [
                    memory.get(&address),
                    memory.get(&(address + bfe!(1))),
                    memory.get(&(address + bfe!(2))),
                ]
                .map(|b| b.copied().unwrap_or(BFieldElement::ZERO));
                xfe!(coefficients)
            }

            let mut array_pointer_b = stack.pop().unwrap();
            let mut array_pointer_a = stack.pop().unwrap();
            let mut acc = XFieldElement::zero();
            for _ in 0..self.length {
                let element_a = read_xfe(memory, array_pointer_a);
                let element_b = read_xfe(memory, array_pointer_b);
                acc += element_a * element_b;
                array_pointer_b += bfe!(3);
                array_pointer_a += bfe!(3);
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
            let mut rng = StdRng::from_seed(seed);
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
    use triton_vm::table::master_table::MasterAuxTable;
    use triton_vm::table::master_table::MasterMainTable;
    use triton_vm::table::master_table::MasterTable;

    use super::*;
    use crate::test_prelude::*;

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
            length: MasterMainTable::NUM_COLUMNS + MasterAuxTable::NUM_COLUMNS,
        })
        .bench();
    }

    #[test]
    fn inner_product_xfes_bench_num_constraints_current_tvm() {
        ShadowedFunction::new(InnerProductOfXfes {
            length: MasterAuxTable::NUM_CONSTRAINTS,
        })
        .bench();
    }
}
