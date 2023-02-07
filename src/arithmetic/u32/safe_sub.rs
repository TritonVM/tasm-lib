use rand::{thread_rng, Rng};
use twenty_first::shared_math::b_field_element::BFieldElement;

use crate::{
    get_init_tvm_stack,
    snippet::{DataType, Snippet},
    ExecutionState,
};

#[derive(Clone)]
pub struct SafeSub;

impl Snippet for SafeSub {
    fn entrypoint(&self) -> String {
        "tasm_arithmetic_u32_safe_sub_u32".to_string()
    }

    fn inputs(&self) -> Vec<String> {
        vec!["rhs".to_string(), "lhs".to_string()]
    }

    fn outputs(&self) -> Vec<String> {
        vec!["lhs - rhs".to_string()]
    }

    fn input_types(&self) -> Vec<crate::snippet::DataType> {
        vec![DataType::U32, DataType::U32]
    }

    fn output_types(&self) -> Vec<crate::snippet::DataType> {
        vec![DataType::U32]
    }

    fn stack_diff(&self) -> isize {
        -1
    }

    fn function_body(&self, _library: &mut crate::library::Library) -> String {
        let entrypoint = self.entrypoint();
        format!(
            "
                // BEFORE: _ rhs lhs
                // AFTER: _ (lhs - rhs)
                {entrypoint}:
                    swap1
                    push -1
                    mul
                    add
                    dup0 // _   (lhs - rhs) (lhs - rhs)
                    split // _  (lhs - rhs) hi lo
                    pop   // _  (lhs - rhs) hi
                    push 0 // _ (lhs - rhs) hi 0
                    eq     // _ (lhs - rhs) (hi == 0)
                    assert // _ (lhs - rhs)
                    return
                    "
        )
    }

    fn crash_conditions() -> Vec<String> {
        vec!["u32 overflow".to_string()]
    }

    fn gen_input_states(&self) -> Vec<crate::ExecutionState> {
        let mut ret: Vec<ExecutionState> = vec![];
        for _ in 0..10 {
            let mut stack = get_init_tvm_stack();
            let lhs = thread_rng().gen_range(0..u32::MAX / 2);
            let rhs = thread_rng().gen_range(0..=lhs);
            let lhs = BFieldElement::new(lhs as u64);
            let rhs = BFieldElement::new(rhs as u64);
            stack.push(rhs);
            stack.push(lhs);
            ret.push(ExecutionState::with_stack(stack));
        }

        ret
    }

    fn rust_shadowing(
        &self,
        stack: &mut Vec<twenty_first::shared_math::b_field_element::BFieldElement>,
        _std_in: Vec<twenty_first::shared_math::b_field_element::BFieldElement>,
        _secret_in: Vec<twenty_first::shared_math::b_field_element::BFieldElement>,
        _memory: &mut std::collections::HashMap<
            twenty_first::shared_math::b_field_element::BFieldElement,
            twenty_first::shared_math::b_field_element::BFieldElement,
        >,
    ) {
        let lhs: u32 = stack.pop().unwrap().try_into().unwrap();
        let rhs: u32 = stack.pop().unwrap().try_into().unwrap();

        let diff = lhs - rhs;
        stack.push(BFieldElement::new(diff as u64));
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use crate::test_helpers::{rust_tasm_equivalence_prop, rust_tasm_equivalence_prop_new};

    use super::*;

    #[test]
    fn snippet_test() {
        rust_tasm_equivalence_prop_new::<SafeSub>(SafeSub);
    }

    #[test]
    fn safe_sub_simple_test() {
        prop_safe_sub(1000, 1, Some(999));
        prop_safe_sub(10_000, 900, Some(9_100));
        prop_safe_sub(123, 123, Some(0));
        prop_safe_sub(1230, 230, Some(1000));
        prop_safe_sub(1 << 31, 1 << 30, Some(1 << 30));
        prop_safe_sub(u32::MAX, 0, Some(u32::MAX));
        prop_safe_sub(u32::MAX, u32::MAX, Some(0));
    }

    #[should_panic]
    #[test]
    fn overflow_test() {
        prop_safe_sub(1 << 31, (1 << 31) + 1000, None);
    }

    #[should_panic]
    #[test]
    fn overflow_test_2() {
        prop_safe_sub(0, 1, None);
    }

    #[should_panic]
    #[test]
    fn overflow_test_3() {
        prop_safe_sub(0, u32::MAX, None);
    }

    fn prop_safe_sub(lhs: u32, rhs: u32, expected: Option<u32>) {
        let mut init_stack = get_init_tvm_stack();
        init_stack.push(BFieldElement::new(rhs as u64));
        init_stack.push(BFieldElement::new(lhs as u64));

        let execution_result = rust_tasm_equivalence_prop::<SafeSub>(
            SafeSub,
            &init_stack,
            &[],
            &[],
            &mut HashMap::default(),
            0,
            None,
        );

        let mut final_stack = execution_result.final_stack;
        if let Some(res) = expected {
            assert_eq!(BFieldElement::new(res as u64), final_stack.pop().unwrap());
        };
    }
}
