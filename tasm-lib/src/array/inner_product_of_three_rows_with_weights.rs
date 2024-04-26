use arbitrary::Arbitrary;
use strum::Display;
use triton_vm::table::{NUM_BASE_COLUMNS, NUM_EXT_COLUMNS};
use triton_vm::triton_asm;

use crate::data_type::ArrayType;
use crate::data_type::DataType;
use crate::traits::basic_snippet::BasicSnippet;

#[derive(Debug, Clone, Copy, Display, Arbitrary)]
pub enum BaseElementType {
    Bfe,
    Xfe,
}

impl From<BaseElementType> for DataType {
    fn from(value: BaseElementType) -> Self {
        match value {
            BaseElementType::Bfe => DataType::Bfe,
            BaseElementType::Xfe => DataType::Xfe,
        }
    }
}

/// Calculate inner product of both base columns and extension columns with weights. Returns one
/// scalar in the form of an extension-field element. Base column can be either a base-field
// element, or an extension-field element.
pub struct InnerProductOfThreeRowsWithWeights {
    base_length: usize,
    base_element_type: BaseElementType,
    ext_length: usize,
    randomizer_length: usize,
}

impl InnerProductOfThreeRowsWithWeights {
    pub fn new(
        base_length: usize,
        base_element_type: BaseElementType,
        ext_length: usize,
        randomizer_length: usize,
    ) -> Self {
        Self {
            base_length,
            base_element_type,
            ext_length,
            randomizer_length,
        }
    }

    pub fn triton_vm_parameters(base_element_type: BaseElementType) -> Self {
        Self {
            base_length: NUM_BASE_COLUMNS,
            ext_length: NUM_EXT_COLUMNS,
            base_element_type,
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
                    length: self.ext_length,
                })),
                "*ext_row".to_string(),
            ),
            (
                DataType::Array(Box::new(ArrayType {
                    element_type: self.base_element_type.into(),
                    length: self.base_length,
                })),
                "*base_row".to_string(),
            ),
            (
                DataType::Array(Box::new(ArrayType {
                    element_type: DataType::Xfe,
                    length: self.base_length + self.ext_length - self.randomizer_length,
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
            "tasmlib_array_inner_product_of_three_rows_with_weights_{}_baserowelem",
            self.base_element_type
        )
    }

    fn code(
        &self,
        _library: &mut crate::library::Library,
    ) -> Vec<triton_vm::prelude::LabelledInstruction> {
        let entrypoint = self.entrypoint();
        let acc_all_base_rows = match self.base_element_type {
            BaseElementType::Bfe => triton_asm![xbdotstep; self.base_length],
            BaseElementType::Xfe => triton_asm![xxdotstep; self.base_length],
        };
        let acc_all_ext_rows = triton_asm![xxdotstep; self.ext_length - self.randomizer_length];

        triton_asm! {
            // BEFORE: _ *ext_row *base_row *weights
            // AFTER: _ [inner_product]
            {entrypoint}:


                push 0
                push 0
                push 0
                // _ *ext_row *base_row *weights 0 0 0

                swap 3
                swap 1
                swap 4
                // _ *ext_row 0 0 0 *weights *base_row

                {&acc_all_base_rows}
                // _ *ext_row acc2 acc1 acc0 *weights_next *garbage0

                swap 5
                // _ *garbage0 acc2 acc1 acc0 *weights_next *ext_row

                {&acc_all_ext_rows}
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
    use rand::rngs::StdRng;
    use rand::Rng;
    use rand::SeedableRng;
    use test_strategy::proptest;
    use triton_vm::twenty_first::math::b_field_element::BFieldElement;
    use triton_vm::twenty_first::math::x_field_element::XFieldElement;

    use crate::rust_shadowing_helper_functions::array::array_from_memory;
    use crate::rust_shadowing_helper_functions::array::insert_random_array;
    use crate::snippet_bencher::BenchmarkCase;
    use crate::traits::function::{Function, FunctionInitialState, ShadowedFunction};
    use crate::traits::rust_shadow::RustShadow;

    use super::*;

    #[test]
    fn three_rows_tvm_parameters_xfe_base_test() {
        ShadowedFunction::new(InnerProductOfThreeRowsWithWeights::triton_vm_parameters(
            BaseElementType::Xfe,
        ))
        .test()
    }

    #[test]
    fn three_rows_tvm_parameters_bfe_base_test() {
        ShadowedFunction::new(InnerProductOfThreeRowsWithWeights::triton_vm_parameters(
            BaseElementType::Bfe,
        ))
        .test()
    }

    #[test]
    fn works_with_base_or_ext_column_count_of_zero() {
        let num_base_columns_is_zero_bfe = InnerProductOfThreeRowsWithWeights {
            base_length: 0,
            base_element_type: BaseElementType::Bfe,
            ext_length: 8,
            randomizer_length: 0,
        };
        let num_base_columns_is_zero_xfe = InnerProductOfThreeRowsWithWeights {
            base_length: 0,
            base_element_type: BaseElementType::Xfe,
            ext_length: 14,
            randomizer_length: 0,
        };
        let num_ext_columns_is_zero = InnerProductOfThreeRowsWithWeights {
            base_length: 12,
            base_element_type: BaseElementType::Xfe,
            ext_length: 0,
            randomizer_length: 0,
        };
        let num_ext_columns_is_num_randomizers = InnerProductOfThreeRowsWithWeights {
            base_length: 12,
            base_element_type: BaseElementType::Xfe,
            ext_length: 2,
            randomizer_length: 2,
        };
        for snippet in [
            num_base_columns_is_zero_bfe,
            num_base_columns_is_zero_xfe,
            num_ext_columns_is_zero,
            num_ext_columns_is_num_randomizers,
        ] {
            ShadowedFunction::new(snippet).test()
        }
    }

    #[proptest(cases = 6)]
    fn three_rows_pbt_pbt(
        #[strategy(arb())] base_element_type: BaseElementType,
        #[strategy(0_usize..500)] base_length: usize,
        #[strategy(0_usize..500)] ext_length: usize,
        #[filter(#randomizer_length<#ext_length)]
        #[strategy(0_usize..10)]
        randomizer_length: usize,
    ) {
        let snippet = InnerProductOfThreeRowsWithWeights {
            base_length,
            base_element_type,
            ext_length,
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
            let base_row_address = stack.pop().unwrap();
            let extension_row_address = stack.pop().unwrap();

            // read arrays
            let weights_len = self.base_length + self.ext_length - self.randomizer_length;
            let weights: Vec<XFieldElement> =
                array_from_memory(weights_address, weights_len, memory);
            let ext_row: Vec<XFieldElement> = array_from_memory(
                extension_row_address,
                self.ext_length - self.randomizer_length,
                memory,
            );

            let ip = match self.base_element_type {
                BaseElementType::Bfe => {
                    let base_row: Vec<BFieldElement> =
                        array_from_memory(base_row_address, self.base_length, memory);

                    // compute inner product
                    base_row
                        .iter()
                        .map(|b| b.lift())
                        .chain(ext_row)
                        .zip_eq(weights)
                        .map(|(l, r)| l * r)
                        .sum::<XFieldElement>()
                }
                BaseElementType::Xfe => {
                    let base_row: Vec<XFieldElement> =
                        array_from_memory(base_row_address, self.base_length, memory);

                    // compute inner product
                    base_row
                        .into_iter()
                        .chain(ext_row)
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
            let base_address = rng.gen();
            let ext_address = rng.gen();
            let weights_address = rng.gen();

            let mut memory: HashMap<BFieldElement, BFieldElement> = HashMap::default();

            match self.base_element_type {
                BaseElementType::Bfe => {
                    insert_random_array(&DataType::Bfe, base_address, self.base_length, &mut memory)
                }
                BaseElementType::Xfe => {
                    insert_random_array(&DataType::Xfe, base_address, self.base_length, &mut memory)
                }
            }
            insert_random_array(&DataType::Xfe, ext_address, self.ext_length, &mut memory);
            insert_random_array(
                &DataType::Xfe,
                weights_address,
                self.base_length + self.ext_length,
                &mut memory,
            );

            let stack = [
                self.init_stack_for_isolated_run(),
                vec![ext_address, base_address, weights_address],
            ]
            .concat();

            FunctionInitialState { stack, memory }
        }
    }
}

#[cfg(test)]
mod benches {
    use triton_vm::stark::NUM_RANDOMIZER_POLYNOMIALS;
    use triton_vm::table::NUM_BASE_COLUMNS;
    use triton_vm::table::NUM_EXT_COLUMNS;

    use crate::traits::function::ShadowedFunction;
    use crate::traits::rust_shadow::RustShadow;

    use super::*;

    #[test]
    fn inner_product_of_three_rows_bench_current_tvm_base_is_bfe() {
        // This benchmark is for the calculation of the (inside-of-domain) current row element that
        // takes place inside the main-loop where all revealed FRI values are verified.
        ShadowedFunction::new(InnerProductOfThreeRowsWithWeights {
            base_length: NUM_BASE_COLUMNS,
            ext_length: NUM_EXT_COLUMNS,
            randomizer_length: NUM_RANDOMIZER_POLYNOMIALS,
            base_element_type: BaseElementType::Bfe,
        })
        .bench();
    }

    #[test]
    fn inner_product_of_three_rows_bench_current_tvm_base_is_xfe() {
        // This benchmark is for the calculation of the out-of-domain current and next row values.
        ShadowedFunction::new(InnerProductOfThreeRowsWithWeights {
            base_length: NUM_BASE_COLUMNS,
            ext_length: NUM_EXT_COLUMNS,
            randomizer_length: NUM_RANDOMIZER_POLYNOMIALS,
            base_element_type: BaseElementType::Xfe,
        })
        .bench();
    }
}
