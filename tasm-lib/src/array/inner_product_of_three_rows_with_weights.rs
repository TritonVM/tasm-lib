use arbitrary::Arbitrary;
use strum::Display;
use triton_vm::prelude::triton_asm;
use triton_vm::table::master_table::MasterAuxTable;
use triton_vm::table::master_table::MasterMainTable;
use triton_vm::table::master_table::MasterTable;

use crate::data_type::ArrayType;
use crate::prelude::*;

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash, Display, Arbitrary)]
pub enum MainElementType {
    Bfe,
    Xfe,
}

impl From<MainElementType> for DataType {
    fn from(value: MainElementType) -> Self {
        match value {
            MainElementType::Bfe => Self::Bfe,
            MainElementType::Xfe => Self::Xfe,
        }
    }
}

/// Calculate inner products of Triton VM
/// [execution trace](triton_vm::table::master_table) rows with weights.
///
/// Calculate inner product of both main columns and auxiliary columns with weights. Returns one
/// scalar in the form of an auxiliary-field element. Main column can be either a base field
/// element, or an auxiliary-field element.
#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub struct InnerProductOfThreeRowsWithWeights {
    main_length: usize,
    main_element_type: MainElementType,
    aux_length: usize,
}

impl InnerProductOfThreeRowsWithWeights {
    pub fn new(main_length: usize, main_element_type: MainElementType, aux_length: usize) -> Self {
        Self {
            main_length,
            main_element_type,
            aux_length,
        }
    }

    pub fn triton_vm_parameters(main_element_type: MainElementType) -> Self {
        Self {
            main_length: MasterMainTable::NUM_COLUMNS,
            aux_length: MasterAuxTable::NUM_COLUMNS,
            main_element_type,
        }
    }
}

impl BasicSnippet for InnerProductOfThreeRowsWithWeights {
    fn inputs(&self) -> Vec<(DataType, String)> {
        fn row_type(name: &str, element_type: DataType, length: usize) -> (DataType, String) {
            let array_type = DataType::Array(Box::new(ArrayType {
                element_type,
                length,
            }));
            (array_type, name.into())
        }

        let num_weights = self.main_length + self.aux_length;

        vec![
            row_type("*aux_row", DataType::Xfe, self.aux_length),
            row_type("*main_row", self.main_element_type.into(), self.main_length),
            row_type("*weights", DataType::Xfe, num_weights),
        ]
    }

    fn outputs(&self) -> Vec<(DataType, String)> {
        vec![(DataType::Xfe, "inner_product".to_owned())]
    }

    fn entrypoint(&self) -> String {
        let element_ty = &self.main_element_type;
        format!("tasmlib_array_inner_product_of_three_rows_with_weights_{element_ty}_mainrowelem")
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
        let acc_all_aux_rows = triton_asm![xx_dot_step; self.aux_length];

        triton_asm! {
            // BEFORE: _ *aux_row *main_row *weights
            // AFTER:  _ [inner_product; 3]
            {entrypoint}:
                push 0
                push 0
                push 0
                // _ *aux_row *main_row *weights 0 0 0

                pick 3
                pick 4
                // _ *aux_row 0 0 0 *weights *main_row

                {&acc_all_main_rows}
                // _ *aux_row acc2 acc1 acc0 *weights_next garbage

                pop 1
                pick 4
                // _ acc2 acc1 acc0 *weights_next *aux_row

                {&acc_all_aux_rows}
                // _ acc2 acc1 acc0 garbage garbage

                pop 2
                // _ result2 result1 result0
                // _ [result; 3]

                return
        }
    }
}

#[cfg(test)]
mod tests {
    use triton_vm::prelude::*;

    use super::*;
    use crate::rust_shadowing_helper_functions::array::array_from_memory;
    use crate::rust_shadowing_helper_functions::array::insert_random_array;
    use crate::test_prelude::*;

    #[test]
    fn three_rows_tvm_parameters_xfe_main_test() {
        let snippet =
            InnerProductOfThreeRowsWithWeights::triton_vm_parameters(MainElementType::Xfe);
        ShadowedFunction::new(snippet).test()
    }

    #[test]
    fn three_rows_tvm_parameters_bfe_main_test() {
        let snippet =
            InnerProductOfThreeRowsWithWeights::triton_vm_parameters(MainElementType::Bfe);
        ShadowedFunction::new(snippet).test()
    }

    #[test]
    fn works_with_main_or_aux_column_count_of_zero() {
        for snippet in [
            InnerProductOfThreeRowsWithWeights::new(0, MainElementType::Bfe, 8),
            InnerProductOfThreeRowsWithWeights::new(0, MainElementType::Xfe, 14),
            InnerProductOfThreeRowsWithWeights::new(12, MainElementType::Bfe, 0),
            InnerProductOfThreeRowsWithWeights::new(16, MainElementType::Xfe, 0),
        ] {
            ShadowedFunction::new(snippet).test()
        }
    }

    #[proptest(cases = 6)]
    fn three_rows_pbt_pbt(
        #[strategy(arb())] main_element_type: MainElementType,
        #[strategy(0_usize..500)] main_length: usize,
        #[strategy(0_usize..500)] aux_length: usize,
    ) {
        let snippet =
            InnerProductOfThreeRowsWithWeights::new(main_length, main_element_type, aux_length);
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
            let weights_len = self.main_length + self.aux_length;
            let weights: Vec<XFieldElement> =
                array_from_memory(weights_address, weights_len, memory);
            let aux_row: Vec<XFieldElement> =
                array_from_memory(auxiliary_row_address, self.aux_length, memory);

            let main_row_as_xfes = match self.main_element_type {
                MainElementType::Bfe => {
                    array_from_memory::<BFieldElement>(main_row_address, self.main_length, memory)
                        .into_iter()
                        .map(|bfe| bfe.lift())
                        .collect_vec()
                }
                MainElementType::Xfe => {
                    array_from_memory(main_row_address, self.main_length, memory)
                }
            };

            // compute inner product
            let inner_product = main_row_as_xfes
                .into_iter()
                .chain(aux_row)
                .zip_eq(weights)
                .map(|(element, weight)| element * weight)
                .sum::<XFieldElement>();

            // write inner product back to stack
            stack.extend(inner_product.coefficients.into_iter().rev());
        }

        fn pseudorandom_initial_state(
            &self,
            seed: [u8; 32],
            _bench_case: Option<BenchmarkCase>,
        ) -> FunctionInitialState {
            let mut rng = StdRng::from_seed(seed);
            let main_address = rng.gen();
            let aux_address = rng.gen();
            let weights_address = rng.gen();

            let mut memory = HashMap::default();
            insert_random_array(
                &self.main_element_type.into(),
                main_address,
                self.main_length,
                &mut memory,
            );
            insert_random_array(&DataType::Xfe, aux_address, self.aux_length, &mut memory);
            insert_random_array(
                &DataType::Xfe,
                weights_address,
                self.main_length + self.aux_length,
                &mut memory,
            );

            let mut stack = self.init_stack_for_isolated_run();
            stack.extend([aux_address, main_address, weights_address]);

            FunctionInitialState { stack, memory }
        }
    }
}

#[cfg(test)]
mod benches {
    use super::*;
    use crate::test_prelude::*;

    /// Benchmark the calculation of the (in-domain) current rows that happen in the
    /// main-loop, where all revealed FRI values are verified.
    #[test]
    fn inner_product_of_three_rows_bench_current_tvm_main_is_bfe() {
        let snippet =
            InnerProductOfThreeRowsWithWeights::triton_vm_parameters(MainElementType::Bfe);
        ShadowedFunction::new(snippet).bench();
    }

    /// Benchmark the calculation of the out-of-domain current and next row values.
    #[test]
    fn inner_product_of_three_rows_bench_current_tvm_main_is_xfe() {
        let snippet =
            InnerProductOfThreeRowsWithWeights::triton_vm_parameters(MainElementType::Xfe);
        ShadowedFunction::new(snippet).bench();
    }
}
