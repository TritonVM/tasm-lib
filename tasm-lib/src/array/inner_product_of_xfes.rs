use std::collections::HashMap;

use triton_vm::prelude::*;

use crate::data_type::ArrayType;
use crate::prelude::*;
use crate::traits::basic_snippet::Reviewer;
use crate::traits::basic_snippet::SignOffFingerprint;

/// Compute the inner product of two lists of [`XFieldElement`]s.
///
/// ### Behavior
///
/// ```text
/// BEFORE: _ *a *b
/// AFTER:  _ [inner_product: XFieldElement]
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
pub struct InnerProductOfXfes {
    pub length: usize,
}

impl InnerProductOfXfes {
    pub fn new(length: usize) -> Self {
        Self { length }
    }
}

impl BasicSnippet for InnerProductOfXfes {
    fn parameters(&self) -> Vec<(DataType, String)> {
        let argument_type = DataType::Array(Box::new(ArrayType {
            element_type: DataType::Xfe,
            length: self.length,
        }));

        vec![
            (argument_type.clone(), "*a".to_owned()),
            (argument_type, "*b".to_owned()),
        ]
    }

    fn return_values(&self) -> Vec<(DataType, String)> {
        vec![(DataType::Xfe, "inner_product".to_owned())]
    }

    fn entrypoint(&self) -> String {
        format!("tasmlib_array_inner_product_of_{}_xfes", self.length)
    }

    fn code(&self, _: &mut Library) -> Vec<LabelledInstruction> {
        triton_asm!(
            // BEFORE: _ *a *b
            // AFTER:  _ [inner_product: XFieldElement]
            {self.entrypoint()}:

                push 0
                push 0
                push 0
                // _ *a *b [0: XFE]

                pick 4
                pick 4
                // _ [0: XFE] *a *b

                {&triton_asm![xx_dot_step; self.length]}
                // _ [acc: XFE] *garbage0 *garbage1

                pop 2
                // _ [acc: XFE]

                return
        )
    }

    fn sign_offs(&self) -> HashMap<Reviewer, SignOffFingerprint> {
        let mut sign_offs = HashMap::new();

        if self.length == 4 {
            sign_offs.insert(Reviewer("ferdinand"), 0x6c3e24944691423f.into());
        }

        sign_offs
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::rust_shadowing_helper_functions::array::array_from_memory;
    use crate::rust_shadowing_helper_functions::array::insert_as_array;
    use crate::rust_shadowing_helper_functions::array::insert_random_array;
    use crate::test_prelude::*;

    impl Accessor for InnerProductOfXfes {
        fn rust_shadow(
            &self,
            stack: &mut Vec<BFieldElement>,
            memory: &HashMap<BFieldElement, BFieldElement>,
        ) {
            let b = array_from_memory::<XFieldElement>(stack.pop().unwrap(), self.length, memory);
            let a = array_from_memory::<XFieldElement>(stack.pop().unwrap(), self.length, memory);
            let inner_product: XFieldElement = a.into_iter().zip(b).map(|(a, b)| a * b).sum();

            push_encodable(stack, &inner_product);
        }

        fn pseudorandom_initial_state(
            &self,
            seed: [u8; 32],
            _: Option<BenchmarkCase>,
        ) -> AccessorInitialState {
            let mut rng = StdRng::from_seed(seed);
            let pointer_a = rng.random();
            let pointer_b_offset = rng.random_range(self.length..usize::MAX - self.length);
            let pointer_b = pointer_a + bfe!(pointer_b_offset);

            let mut memory = HashMap::default();
            insert_random_array(&DataType::Xfe, pointer_a, self.length, &mut memory);
            insert_random_array(&DataType::Xfe, pointer_b, self.length, &mut memory);

            let mut stack = self.init_stack_for_isolated_run();
            stack.push(pointer_a);
            stack.push(pointer_b);

            AccessorInitialState { stack, memory }
        }

        fn corner_case_initial_states(&self) -> Vec<AccessorInitialState> {
            let all_zeros = AccessorInitialState {
                stack: [self.init_stack_for_isolated_run(), bfe_vec![0, 1_u64 << 40]].concat(),
                memory: HashMap::default(),
            };

            vec![all_zeros]
        }
    }

    #[test]
    fn inner_product_of_xfes_pbt() {
        for test_case in (0..20).chain(100..110).map(InnerProductOfXfes::new) {
            ShadowedAccessor::new(test_case).test()
        }
    }

    #[test]
    fn inner_product_unit_test() {
        let a = xfe_vec![[3, 0, 0], [5, 0, 0]];
        let b = xfe_vec![[501, 0, 0], [1003, 0, 0]];
        let inner_product = xfe!([3 * 501 + 5 * 1003, 0, 0]);

        let rust_inner_product = a
            .iter()
            .zip(&b)
            .map(|(&a, &b)| a * b)
            .sum::<XFieldElement>();
        debug_assert_eq!(inner_product, rust_inner_product);

        let mut memory = HashMap::default();
        let pointer_a = bfe!(1_u64 << 44);
        let pointer_b = bfe!(1_u64 << 45);
        insert_as_array(pointer_a, &mut memory, a);
        insert_as_array(pointer_b, &mut memory, b);

        let snippet = InnerProductOfXfes::new(2);
        let mut initial_stack = snippet.init_stack_for_isolated_run();
        initial_stack.push(pointer_a);
        initial_stack.push(pointer_b);

        let mut expected_final_stack = snippet.init_stack_for_isolated_run();
        push_encodable(&mut expected_final_stack, &inner_product);

        test_rust_equivalence_given_complete_state(
            &ShadowedAccessor::new(snippet),
            &initial_stack,
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

    use super::*;
    use crate::test_prelude::*;

    #[test]
    fn benchmark() {
        ShadowedAccessor::new(InnerProductOfXfes::new(100)).bench();
        ShadowedAccessor::new(InnerProductOfXfes::new(200)).bench();

        let num_columns = MasterMainTable::NUM_COLUMNS + MasterAuxTable::NUM_COLUMNS;
        ShadowedAccessor::new(InnerProductOfXfes::new(num_columns)).bench();
        ShadowedAccessor::new(InnerProductOfXfes::new(MasterAuxTable::NUM_CONSTRAINTS)).bench();
    }
}
