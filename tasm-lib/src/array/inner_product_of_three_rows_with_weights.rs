use std::collections::HashMap;

use arbitrary::Arbitrary;
use strum::Display;
use triton_vm::prelude::*;
use triton_vm::table::master_table::MasterAuxTable;
use triton_vm::table::master_table::MasterMainTable;

use crate::data_type::ArrayType;
use crate::prelude::*;
use crate::traits::basic_snippet::Reviewer;
use crate::traits::basic_snippet::SignOffFingerprint;

/// The type of field element used in
/// [`InnerProductOfThreeRowsWithWeights`].
#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash, Display, Arbitrary)]
pub enum MainElementType {
    /// Corresponds to [`BFieldElement`].
    Bfe,

    /// Corresponds to [`XFieldElement`].
    Xfe,
}

impl MainElementType {
    fn dot_step(&self) -> LabelledInstruction {
        match self {
            Self::Bfe => triton_instr!(xb_dot_step),
            Self::Xfe => triton_instr!(xx_dot_step),
        }
    }
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
/// Calculate inner product of both main columns and auxiliary columns with
/// weights. Returns one scalar in the form of an auxiliary-field element.
/// The main column can be either a base field element, or an auxiliary-field
/// element; see also [`MainElementType`].
///
/// ### Behavior
///
/// ```text
/// BEFORE: _
/// AFTER:  _
/// ```
///
/// ### Preconditions
///
/// None.
///
/// ### Postconditions
///
/// None.
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
    fn parameters(&self) -> Vec<(DataType, String)> {
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

    fn return_values(&self) -> Vec<(DataType, String)> {
        vec![(DataType::Xfe, "inner_product".to_owned())]
    }

    fn entrypoint(&self) -> String {
        let element_ty = &self.main_element_type;
        format!("tasmlib_array_inner_product_of_three_rows_with_weights_{element_ty}_mainrowelem")
    }

    fn code(&self, _: &mut Library) -> Vec<LabelledInstruction> {
        triton_asm! {
            // BEFORE: _ *aux_row *main_row *weights
            // AFTER:  _ [inner_product; 3]
            {self.entrypoint()}:
                push 0
                push 0
                push 0
                // _ *aux_row *main_row *weights [0: XFE]

                pick 3
                pick 4
                // _ *aux_row [0: XFE] *weights *main_row

                {&vec![self.main_element_type.dot_step(); self.main_length]}
                // _ *aux_row [acc: XFE] *weights_next garbage

                pop 1
                pick 4
                // _ [acc: XFE] *weights_next *aux_row

                {&triton_asm![xx_dot_step; self.aux_length]}
                // _ [acc: XFE] garbage garbage

                pop 2
                // _ [result: XFE]

                return
        }
    }

    fn sign_offs(&self) -> HashMap<Reviewer, SignOffFingerprint> {
        let mut sign_offs = HashMap::new();

        if self == &Self::new(379, MainElementType::Bfe, 88) {
            sign_offs.insert(Reviewer("ferdinand"), 0x7bf20b54fa1bbed3.into());
        }
        if self == &Self::new(379, MainElementType::Xfe, 88) {
            sign_offs.insert(Reviewer("ferdinand"), 0x5996476ab229bd47.into());
        }

        sign_offs
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
        ShadowedAccessor::new(snippet).test();
    }

    #[test]
    fn three_rows_tvm_parameters_bfe_main_test() {
        let snippet =
            InnerProductOfThreeRowsWithWeights::triton_vm_parameters(MainElementType::Bfe);
        ShadowedAccessor::new(snippet).test();
    }

    #[proptest(cases = 10)]
    fn main_or_aux_column_count_can_be_zero(#[strategy(0_usize..500)] len: usize) {
        for elt_ty in [MainElementType::Bfe, MainElementType::Xfe] {
            ShadowedAccessor::new(InnerProductOfThreeRowsWithWeights::new(0, elt_ty, len)).test();
            ShadowedAccessor::new(InnerProductOfThreeRowsWithWeights::new(len, elt_ty, 0)).test();
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
        ShadowedAccessor::new(snippet).test();
    }

    impl Accessor for InnerProductOfThreeRowsWithWeights {
        fn rust_shadow(
            &self,
            stack: &mut Vec<BFieldElement>,
            memory: &HashMap<BFieldElement, BFieldElement>,
        ) {
            let weights_address = stack.pop().unwrap();
            let main_row_address = stack.pop().unwrap();
            let aux_row_address = stack.pop().unwrap();

            let weights_len = self.main_length + self.aux_length;
            let weights = array_from_memory::<XFieldElement>(weights_address, weights_len, memory);
            let aux_row =
                array_from_memory::<XFieldElement>(aux_row_address, self.aux_length, memory);

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

            let inner_product = main_row_as_xfes
                .into_iter()
                .chain(aux_row)
                .zip_eq(weights)
                .map(|(element, weight)| element * weight)
                .sum::<XFieldElement>();

            push_encodable(stack, &inner_product)
        }

        fn pseudorandom_initial_state(
            &self,
            seed: [u8; 32],
            _: Option<BenchmarkCase>,
        ) -> AccessorInitialState {
            let mut rng = StdRng::from_seed(seed);
            let main_address = rng.random();
            let aux_address = rng.random();
            let weights_address = rng.random();

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

            AccessorInitialState { stack, memory }
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
    fn bench_current_tvm_bfe() {
        let snippet =
            InnerProductOfThreeRowsWithWeights::triton_vm_parameters(MainElementType::Bfe);
        ShadowedAccessor::new(snippet).bench();
    }

    /// Benchmark the calculation of the out-of-domain current and next row values.
    #[test]
    fn bench_current_tvm_xfe() {
        let snippet =
            InnerProductOfThreeRowsWithWeights::triton_vm_parameters(MainElementType::Xfe);
        ShadowedAccessor::new(snippet).bench();
    }
}
