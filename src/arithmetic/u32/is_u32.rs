use std::collections::HashMap;

use num::{One, Zero};
use rand::RngCore;
use twenty_first::shared_math::b_field_element::BFieldElement;

use crate::library::Library;
use crate::snippet::{NewSnippet, Snippet};
use crate::{get_init_tvm_stack, push_hashable, ExecutionState};

pub struct IsU32();

impl NewSnippet for IsU32 {
    fn inputs() -> Vec<&'static str> {
        vec!["value"]
    }

    fn outputs() -> Vec<&'static str> {
        vec!["value < 2^32"]
    }

    fn crash_conditions() -> Vec<&'static str> {
        vec![]
    }

    fn gen_input_states() -> Vec<crate::ExecutionState> {
        let n: u32 = rand::thread_rng().next_u32();

        let mut true_stack = get_init_tvm_stack();
        push_hashable(&mut true_stack, &n);

        let mut false_stack = get_init_tvm_stack();
        push_hashable(&mut false_stack, &(u32::MAX));

        vec![
            ExecutionState::with_stack(true_stack),
            ExecutionState::with_stack(false_stack),
        ]
    }

    fn run_tasm(execution_state: &mut crate::ExecutionState) -> crate::ExecutionResult {
        // TODO: Consider adding canaries here to ensure that stack is not modified below where the function

        let stack_prior = execution_state.stack.clone();
        let ret = <Self as Snippet>::run_tasm(
            &mut execution_state.stack,
            execution_state.std_in.clone(),
            execution_state.secret_in.clone(),
            &mut execution_state.memory,
            execution_state.words_allocated,
        );
        let stack_after = execution_state.stack.clone();

        assert_eq!(
            stack_prior[0..(stack_prior.len() - Self::inputs().len())],
            stack_after[0..(stack_after.len() - Self::outputs().len())]
        );

        ret
    }
}

impl Snippet for IsU32 {
    fn stack_diff() -> isize {
        0
    }

    fn entrypoint() -> &'static str {
        "is_u32"
    }

    /// Place 1 on stack iff top element is less than $2^32$. Otherwise
    /// place 0 on stack. Consumes top element of stack, leaves a boolean
    /// on top of stack. So this subroutine does not change the height
    /// of the stack
    fn function_body(_library: &mut Library) -> String {
        let mut unrolled_loop: String = String::default();
        let entrypoint = Self::entrypoint();
        for _ in 0..32 {
            unrolled_loop.push_str("lsb\n");
            unrolled_loop.push_str("pop\n");
        }
        let code: &str = &format!(
            "
            {entrypoint}:
                {unrolled_loop}
                push 0
                eq
                return
            "
        );

        code.to_string()
    }

    fn rust_shadowing(
        stack: &mut Vec<BFieldElement>,
        _std_in: Vec<BFieldElement>,
        _secret_in: Vec<BFieldElement>,
        _memory: &mut HashMap<BFieldElement, BFieldElement>,
    ) {
        let top = stack.pop().unwrap();
        stack.push(if top.value() < (1 << 32) {
            BFieldElement::one()
        } else {
            BFieldElement::zero()
        });
    }
}

#[cfg(test)]
mod tests {
    use rand::RngCore;

    use crate::get_init_tvm_stack;
    use crate::snippet_bencher::bench_and_write;
    use crate::test_helpers::{rust_tasm_equivalence_prop, rust_tasm_equivalence_prop_new};

    use super::*;

    #[test]
    fn is_u32_test() {
        rust_tasm_equivalence_prop_new::<IsU32>();
    }

    #[test]
    fn is_u32_benchmark() {
        bench_and_write::<IsU32>();
    }

    #[test]
    fn is_u32_simple() {
        let stack_true: Vec<BFieldElement> =
            vec![get_init_tvm_stack(), vec![BFieldElement::one()]].concat();
        let stack_false: Vec<BFieldElement> =
            vec![get_init_tvm_stack(), vec![BFieldElement::zero()]].concat();

        prop_is_u32(BFieldElement::zero(), Some(&stack_true));
        prop_is_u32(BFieldElement::one(), Some(&stack_true));
        prop_is_u32(BFieldElement::new(1 << 10), Some(&stack_true));
        prop_is_u32(BFieldElement::new(1 << 20), Some(&stack_true));
        prop_is_u32(BFieldElement::new(1 << 30), Some(&stack_true));
        prop_is_u32(BFieldElement::new(1 << 40), Some(&stack_false));
        prop_is_u32(BFieldElement::new(1 << 50), Some(&stack_false));
        prop_is_u32(BFieldElement::new(1 << 60), Some(&stack_false));
        prop_is_u32(
            BFieldElement::new((1 << 63) + (1 << 42)),
            Some(&stack_false),
        );
    }

    #[test]
    fn is_u32_pbt() {
        let stack_true: Vec<BFieldElement> =
            vec![get_init_tvm_stack(), vec![BFieldElement::one()]].concat();
        let stack_false: Vec<BFieldElement> =
            vec![get_init_tvm_stack(), vec![BFieldElement::zero()]].concat();

        let mut rng = rand::thread_rng();
        for _ in 0..10 {
            prop_is_u32(BFieldElement::new(rng.next_u32() as u64), Some(&stack_true));
            prop_is_u32(
                BFieldElement::new((rng.next_u32() as u64) + (1u64 << 32)),
                Some(&stack_false),
            );
        }
    }

    fn prop_is_u32(some_value: BFieldElement, expected: Option<&[BFieldElement]>) {
        let mut init_stack = get_init_tvm_stack();
        init_stack.push(some_value);

        let _execution_result = rust_tasm_equivalence_prop::<IsU32>(
            &init_stack,
            &[],
            &[],
            &mut HashMap::default(),
            0,
            expected,
        );
    }
}
