use arbitrary::Arbitrary;
use strum::Display;
use triton_vm::prelude::triton_asm;
use triton_vm::table::master_table::MasterAuxTable;
use triton_vm::table::master_table::MasterMainTable;
use triton_vm::table::master_table::MasterTable;

use crate::data_type::ArrayType;
use crate::data_type::DataType;
use crate::traits::basic_snippet::BasicSnippet;

#[derive(Debug, Clone, Copy, Display, Arbitrary)]
pub enum MainElementType {
    Bfe,
    Xfe,
}

impl From<MainElementType> for DataType {
    fn from(value: MainElementType) -> Self {
        match value {
            MainElementType::Bfe => DataType::Bfe,
            MainElementType::Xfe => DataType::Xfe,
        }
    }
}

/// Calculate inner products of with weights.
///
/// Calculate inner product of both main columns and auxiliary columns with weights. Returns one
/// scalar in the form of an auxiliary-field element. Main column can be either a base field
/// element, or an auxiliary-field element.
pub struct InnerProductOfThreeRowsWithWeights {
    main_length: usize,
    main_element_type: MainElementType,
    aux_length: usize,
    randomizer_length: usize,
}

impl InnerProductOfThreeRowsWithWeights {
    pub fn new(
        main_length: usize,
        main_element_type: MainElementType,
        aux_length: usize,
        randomizer_length: usize,
    ) -> Self {
        Self {
            main_length,
            main_element_type,
            aux_length,
            randomizer_length,
        }
    }

    pub fn triton_vm_parameters(main_element_type: MainElementType) -> Self {
        Self {
            main_length: MasterMainTable::NUM_COLUMNS,
            aux_length: MasterAuxTable::NUM_COLUMNS,
            main_element_type,
            // TODO: Use NUM_RANDOMIZERS from TVM here instead, once randomizers
            // are handled correctly in TVM.
            randomizer_length: 0,
        }
    }
}

impl BasicSnippet for InnerProductOfThreeRowsWithWeights {
    fn inputs(&self) -> Vec<(DataType, String)> {
        vec![
            (
                DataType::Array(Box::new(ArrayType {
                    element_type: DataType::Xfe,
                    length: self.aux_length,
                })),
                "*aux_row".to_string(),
            ),
            (
                DataType::Array(Box::new(ArrayType {
                    element_type: self.main_element_type.into(),
                    length: self.main_length,
                })),
                "*main_row".to_string(),
            ),
            (
                DataType::Array(Box::new(ArrayType {
                    element_type: DataType::Xfe,
                    length: self.main_length + self.aux_length - self.randomizer_length,
                })),
                "*weights".to_string(),
            ),
        ]
    }

    fn outputs(&self) -> Vec<(DataType, String)> {
        vec![(DataType::Xfe, "inner_product".to_owned())]
    }

    fn entrypoint(&self) -> String {
        format!(
            "tasmlib_array_inner_product_of_three_rows_with_weights_{}_mainrowelem",
            self.main_element_type
        )
    }

    fn code(
        &self,
        _library: &mut crate::library::Library,
    ) -> Vec<triton_vm::prelude::LabelledInstruction> {
        let entrypoint = self.entrypoint();
        let acc_all_main_rows = match self.main_element_type {
            MainElementType::Bfe => triton_asm![xb_dot_step; self.main_length],
            MainElementType::Xfe => triton_asm![xx_dot_step; self.main_length],
        };
        let acc_all_aux_rows = triton_asm![xx_dot_step; self.aux_length - self.randomizer_length];

        triton_asm! {
            // BEFORE: _ *aux_row *main_row *weights
            // AFTER: _ [inner_product]
            {entrypoint}:


                push 0
                push 0
                push 0
                // _ *aux_row *main_row *weights 0 0 0

                swap 3
                swap 1
                swap 4
                // _ *aux_row 0 0 0 *weights *main_row

                {&acc_all_main_rows}
                // _ *aux_row acc2 acc1 acc0 *weights_next *garbage0

                swap 5
                // _ *garbage0 acc2 acc1 acc0 *weights_next *aux_row

                {&acc_all_aux_rows}
                // _ *garbage0 acc2 acc1 acc0 *garbage1 garbage2
                // _ *garbage0 result2 result1 result0 *garbage1 garbage2 <-- rename

                pop 2
                swap 1
                swap 2
                swap 3
                pop 1
                // _ result2 result1 result0
                // _ [result]

                return
        }
    }
}

#[cfg(test)]
mod test {
    use std::collections::HashMap;

    use itertools::Itertools;
    use proptest_arbitrary_interop::arb;
    use rand::prelude::*;
    use test_strategy::proptest;
    use triton_vm::prelude::*;

    use super::*;
    use crate::rust_shadowing_helper_functions::array::array_from_memory;
    use crate::rust_shadowing_helper_functions::array::insert_random_array;
    use crate::snippet_bencher::BenchmarkCase;
    use crate::traits::function::Function;
    use crate::traits::function::FunctionInitialState;
    use crate::traits::function::ShadowedFunction;
    use crate::traits::rust_shadow::RustShadow;

    #[test]
    fn three_rows_tvm_parameters_xfe_main_test() {
        ShadowedFunction::new(InnerProductOfThreeRowsWithWeights::triton_vm_parameters(
            MainElementType::Xfe,
        ))
        .test()
    }

    #[test]
    fn three_rows_tvm_parameters_bfe_main_test() {
        ShadowedFunction::new(InnerProductOfThreeRowsWithWeights::triton_vm_parameters(
            MainElementType::Bfe,
        ))
        .test()
    }

    #[test]
    fn works_with_main_or_aux_column_count_of_zero() {
        let num_main_columns_is_zero_bfe = InnerProductOfThreeRowsWithWeights {
            main_length: 0,
            main_element_type: MainElementType::Bfe,
            aux_length: 8,
            randomizer_length: 0,
        };
        let num_main_columns_is_zero_xfe = InnerProductOfThreeRowsWithWeights {
            main_length: 0,
            main_element_type: MainElementType::Xfe,
            aux_length: 14,
            randomizer_length: 0,
        };
        let num_aux_columns_is_zero = InnerProductOfThreeRowsWithWeights {
            main_length: 12,
            main_element_type: MainElementType::Xfe,
            aux_length: 0,
            randomizer_length: 0,
        };
        let num_aux_columns_is_num_randomizers = InnerProductOfThreeRowsWithWeights {
            main_length: 12,
            main_element_type: MainElementType::Xfe,
            aux_length: 2,
            randomizer_length: 2,
        };
        for snippet in [
            num_main_columns_is_zero_bfe,
            num_main_columns_is_zero_xfe,
            num_aux_columns_is_zero,
            num_aux_columns_is_num_randomizers,
        ] {
            ShadowedFunction::new(snippet).test()
        }
    }

    #[proptest(cases = 6)]
    fn three_rows_pbt_pbt(
        #[strategy(arb())] main_element_type: MainElementType,
        #[strategy(0_usize..500)] main_length: usize,
        #[strategy(0_usize..500)] aux_length: usize,
        #[filter(#randomizer_length<#aux_length)]
        #[strategy(0_usize..10)]
        randomizer_length: usize,
    ) {
        let snippet = InnerProductOfThreeRowsWithWeights {
            main_length,
            main_element_type,
            aux_length,
            randomizer_length,
        };
        ShadowedFunction::new(snippet).test()
    }

    impl Function for InnerProductOfThreeRowsWithWeights {
        fn rust_shadow(
            &self,
            stack: &mut Vec<BFieldElement>,
            memory: &mut HashMap<BFieldElement, BFieldElement>,
        ) {
            // read stack: _ *e *b *w
            let weights_address = stack.pop().unwrap();
            let main_row_address = stack.pop().unwrap();
            let auxiliary_row_address = stack.pop().unwrap();

            // read arrays
            let weights_len = self.main_length + self.aux_length - self.randomizer_length;
            let weights: Vec<XFieldElement> =
                array_from_memory(weights_address, weights_len, memory);
            let aux_row: Vec<XFieldElement> = array_from_memory(
                auxiliary_row_address,
                self.aux_length - self.randomizer_length,
                memory,
            );

            let ip = match self.main_element_type {
                MainElementType::Bfe => {
                    let main_row: Vec<BFieldElement> =
                        array_from_memory(main_row_address, self.main_length, memory);

                    // compute inner product
                    main_row
                        .iter()
                        .map(|b| b.lift())
                        .chain(aux_row)
                        .zip_eq(weights)
                        .map(|(l, r)| l * r)
                        .sum::<XFieldElement>()
                }
                MainElementType::Xfe => {
                    let main_row: Vec<XFieldElement> =
                        array_from_memory(main_row_address, self.main_length, memory);

                    // compute inner product
                    main_row
                        .into_iter()
                        .chain(aux_row)
                        .zip_eq(weights)
                        .map(|(l, r)| l * r)
                        .sum::<XFieldElement>()
                }
            };

            // write inner product back to stack
            let mut res = ip.coefficients.to_vec();
            stack.push(res.pop().unwrap());
            stack.push(res.pop().unwrap());
            stack.push(res.pop().unwrap());
        }

        fn pseudorandom_initial_state(
            &self,
            seed: [u8; 32],
            _bench_case: Option<BenchmarkCase>,
        ) -> FunctionInitialState {
            let mut rng: StdRng = SeedableRng::from_seed(seed);
            let main_address = rng.gen();
            let aux_address = rng.gen();
            let weights_address = rng.gen();

            let mut memory: HashMap<BFieldElement, BFieldElement> = HashMap::default();

            match self.main_element_type {
                MainElementType::Bfe => {
                    insert_random_array(&DataType::Bfe, main_address, self.main_length, &mut memory)
                }
                MainElementType::Xfe => {
                    insert_random_array(&DataType::Xfe, main_address, self.main_length, &mut memory)
                }
            }
            insert_random_array(&DataType::Xfe, aux_address, self.aux_length, &mut memory);
            insert_random_array(
                &DataType::Xfe,
                weights_address,
                self.main_length + self.aux_length,
                &mut memory,
            );

            let stack = [
                self.init_stack_for_isolated_run(),
                vec![aux_address, main_address, weights_address],
            ]
            .concat();

            FunctionInitialState { stack, memory }
        }
    }
}

#[cfg(test)]
mod benches {
    use triton_vm::stark::NUM_RANDOMIZER_POLYNOMIALS;

    use super::*;
    use crate::traits::function::ShadowedFunction;
    use crate::traits::rust_shadow::RustShadow;

    #[test]
    fn inner_product_of_three_rows_bench_current_tvm_main_is_bfe() {
        // This benchmark is for the calculation of the (inside-of-domain) current row element that
        // takes place inside the main-loop where all revealed FRI values are verified.
        ShadowedFunction::new(InnerProductOfThreeRowsWithWeights {
            main_length: MasterMainTable::NUM_COLUMNS,
            aux_length: MasterAuxTable::NUM_COLUMNS,
            randomizer_length: NUM_RANDOMIZER_POLYNOMIALS,
            main_element_type: MainElementType::Bfe,
        })
        .bench();
    }

    #[test]
    fn inner_product_of_three_rows_bench_current_tvm_main_is_xfe() {
        // This benchmark is for the calculation of the out-of-domain current and next row values.
        ShadowedFunction::new(InnerProductOfThreeRowsWithWeights {
            main_length: MasterMainTable::NUM_COLUMNS,
            aux_length: MasterAuxTable::NUM_COLUMNS,
            randomizer_length: NUM_RANDOMIZER_POLYNOMIALS,
            main_element_type: MainElementType::Xfe,
        })
        .bench();
    }
}
