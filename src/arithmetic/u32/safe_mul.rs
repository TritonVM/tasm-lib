use rand::{thread_rng, Rng};
use twenty_first::shared_math::b_field_element::BFieldElement;

use crate::{
    get_init_tvm_stack,
    snippet::{DataType, Snippet},
    ExecutionState,
};

/// If the inputs, are valid u32s, then the output is guaranteed to be to.
/// Crashes on overflow.
#[derive(Clone)]
pub struct SafeMul;

impl Snippet for SafeMul {
    fn entrypoint(&self) -> &'static str {
        "u32_safe_mul"
    }

    fn inputs() -> Vec<&'static str>
    where
        Self: Sized,
    {
        vec!["lhs", "rhs"]
    }

    fn input_types(&self) -> Vec<DataType> {
        vec![DataType::U32, DataType::U32]
    }

    fn output_types(&self) -> Vec<DataType> {
        vec![DataType::U32]
    }

    fn outputs() -> Vec<&'static str>
    where
        Self: Sized,
    {
        vec!["lhs * rhs"]
    }

    fn stack_diff() -> isize
    where
        Self: Sized,
    {
        -1
    }

    fn function_body(&self, _library: &mut crate::library::Library) -> String {
        let entrypoint = self.entrypoint();
        format!(
            "
                // BEFORE: _ rhs lhs
                // AFTER: _ (lhs * rhs)
                {entrypoint}:
                    mul
                    dup0 // _   (lhs * rhs) (lhs * rhs)
                    split // _  (lhs * rhs) hi lo
                    pop   // _  (lhs * rhs) hi
                    push 0 // _ (lhs * rhs) hi 0
                    eq     // _ (lhs * rhs) (hi == 0)
                    assert // _ (lhs * rhs)
                    return
                    "
        )
    }

    fn crash_conditions() -> Vec<&'static str>
    where
        Self: Sized,
    {
        vec!["result overflows u32"]
    }

    fn gen_input_states() -> Vec<ExecutionState>
    where
        Self: Sized,
    {
        let mut ret: Vec<ExecutionState> = vec![];
        for _ in 0..10 {
            let mut stack = get_init_tvm_stack();
            let lhs = thread_rng().gen_range(0..(1 << 16));
            let rhs = thread_rng().gen_range(0..(1 << 16));
            let lhs = BFieldElement::new(lhs as u64);
            let rhs = BFieldElement::new(rhs as u64);
            stack.push(lhs);
            stack.push(rhs);
            ret.push(ExecutionState::with_stack(stack));
        }

        ret
    }

    fn rust_shadowing(
        stack: &mut Vec<BFieldElement>,
        _std_in: Vec<BFieldElement>,
        _secret_in: Vec<BFieldElement>,
        _memory: &mut std::collections::HashMap<BFieldElement, BFieldElement>,
    ) where
        Self: Sized,
    {
        let lhs: u32 = stack.pop().unwrap().try_into().unwrap();
        let rhs: u32 = stack.pop().unwrap().try_into().unwrap();

        let prod = lhs * rhs;
        stack.push(BFieldElement::new(prod as u64));
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use crate::test_helpers::{rust_tasm_equivalence_prop, rust_tasm_equivalence_prop_new};

    use super::*;

    #[test]
    fn snippet_test() {
        rust_tasm_equivalence_prop_new(SafeMul);
    }

    #[test]
    fn safe_sub_simple_test() {
        prop_safe_mul(1000, 1, Some(1000));
        prop_safe_mul(10_000, 900, Some(9_000_000));
        prop_safe_mul(1, 1, Some(1));
        prop_safe_mul(10_000, 10_000, Some(100_000_000));
        prop_safe_mul(u32::MAX, 1, Some(u32::MAX));
        prop_safe_mul(1, u32::MAX, Some(u32::MAX));
    }

    #[should_panic]
    #[test]
    fn overflow_test() {
        prop_safe_mul(1 << 16, 1 << 16, None);
    }

    #[should_panic]
    #[test]
    fn overflow_test_2() {
        prop_safe_mul(1 << 31, 2, None);
    }

    #[should_panic]
    #[test]
    fn overflow_test_3() {
        prop_safe_mul(2, 1 << 31, None);
    }

    fn prop_safe_mul(lhs: u32, rhs: u32, expected: Option<u32>) {
        let mut init_stack = get_init_tvm_stack();
        init_stack.push(BFieldElement::new(rhs as u64));
        init_stack.push(BFieldElement::new(lhs as u64));

        let execution_result = rust_tasm_equivalence_prop(
            SafeMul,
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
