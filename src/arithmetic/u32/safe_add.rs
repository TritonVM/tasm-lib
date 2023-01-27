use rand::{thread_rng, Rng};
use twenty_first::shared_math::b_field_element::BFieldElement;

use crate::{get_init_tvm_stack, snippet::Snippet, ExecutionState};

pub struct SafeAdd;

impl Snippet for SafeAdd {
    fn entrypoint() -> &'static str {
        "safe_add_u32"
    }

    fn inputs() -> Vec<&'static str> {
        vec!["lhs", "rhs"]
    }

    fn outputs() -> Vec<&'static str> {
        vec!["lhs + rhs"]
    }

    fn stack_diff() -> isize {
        -1
    }

    fn function_body(_library: &mut crate::library::Library) -> String {
        let entrypoint = Self::entrypoint();
        format!(
            "
                {entrypoint}:
                    add // _    lhs + rhs
                    dup0 // _   (lhs + rhs) (lhs + rhs)
                    split // _  (lhs + rhs) hi lo
                    pop   // _  (lhs + rhs) hi
                    push 0 // _ (lhs + rhs) hi 0
                    eq     // _ (lhs + rhs) (hi == 0)
                    assert // _ (lhs + rhs)
                    return
                    "
        )
    }

    fn crash_conditions() -> Vec<&'static str> {
        vec!["u32 overflow"]
    }

    fn gen_input_states() -> Vec<crate::ExecutionState> {
        let mut ret: Vec<ExecutionState> = vec![];
        for _ in 0..10 {
            let mut stack = get_init_tvm_stack();
            let lhs = thread_rng().gen_range(0..u32::MAX / 2);
            let rhs = thread_rng().gen_range(0..u32::MAX / 2);
            let lhs = BFieldElement::new(lhs as u64);
            let rhs = BFieldElement::new(rhs as u64);
            stack.push(rhs);
            stack.push(lhs);
            ret.push(ExecutionState::with_stack(stack));
        }

        ret
    }

    fn rust_shadowing(
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

        let sum = lhs + rhs;
        stack.push(BFieldElement::new(sum as u64));
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use crate::test_helpers::{rust_tasm_equivalence_prop, rust_tasm_equivalence_prop_new};

    use super::*;

    #[test]
    fn snippet_test() {
        rust_tasm_equivalence_prop_new::<SafeAdd>();
    }

    #[test]
    fn safe_add_simple_test() {
        prop_safe_add(1000, 1, Some(1001));
        prop_safe_add(10_000, 900, Some(10_900));
    }

    #[should_panic]
    #[test]
    fn overflow_test() {
        prop_safe_add((1 << 31) + 1000, 1 << 31, None);
    }

    fn prop_safe_add(lhs: u32, rhs: u32, expected: Option<u32>) {
        let mut init_stack = get_init_tvm_stack();
        init_stack.push(BFieldElement::new(rhs as u64));
        init_stack.push(BFieldElement::new(lhs as u64));

        let execution_result = rust_tasm_equivalence_prop::<SafeAdd>(
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
