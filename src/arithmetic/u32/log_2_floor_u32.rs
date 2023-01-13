use std::collections::HashMap;
use twenty_first::shared_math::b_field_element::BFieldElement;
use twenty_first::shared_math::other::log_2_floor;

use crate::library::Library;
use crate::snippet::{NewSnippet, Snippet};
use crate::{get_init_tvm_stack, push_hashable, ExecutionState};

pub struct Log2FloorU32();

impl NewSnippet for Log2FloorU32 {
    fn inputs() -> Vec<&'static str> {
        vec!["value"]
    }

    fn outputs() -> Vec<&'static str> {
        vec!["log_2_floor(value)"]
    }

    fn crash_conditions() -> Vec<&'static str> {
        vec![]
    }

    fn gen_input_states() -> Vec<ExecutionState> {
        let mut states: Vec<ExecutionState> = (0..32)
            .map(|log_i| {
                let mut stack = get_init_tvm_stack();
                push_hashable(&mut stack, &(2u32.pow(log_i)));

                ExecutionState::with_stack(stack)
            })
            .collect();

        states.insert(0, {
            let mut zero_stack = get_init_tvm_stack();
            push_hashable(&mut zero_stack, &0u32);
            ExecutionState::with_stack(zero_stack)
        });

        states
    }

    fn run_tasm(execution_state: &mut ExecutionState) -> crate::ExecutionResult {
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

impl Snippet for Log2FloorU32 {
    fn stack_diff() -> isize {
        0
    }

    fn entrypoint() -> &'static str {
        "log_2_floor_u32"
    }

    fn function_body(_library: &mut Library) -> String {
        let entrypoint = Self::entrypoint();
        // assumes that top of stack is a valid u32 and not 0
        // BEFORE: _ value
        // AFTER: _ log2_floor(value) = res
        format!(
            "
                {entrypoint}:
                    push 0
                    swap1
                    // stack: _ res value

                    call {entrypoint}_loop
                    // stack: _ res 0

                    pop
                    // stack: _ res

                    return

                // stack, start/end: _ res value
                {entrypoint}_loop:
                    lsb
                    // stack: _ res (value / 2) (value % 2)

                    pop
                    dup0
                    push 0
                    eq
                    skiz return // return if (value / 2 == 0)
                    // stack: _ res (value / 2)

                    swap1 push 1 add swap1
                    // stack: _ (res + 1) (value / 2)

                    recurse
                "
        )
    }

    fn rust_shadowing(
        stack: &mut Vec<BFieldElement>,
        _std_in: Vec<BFieldElement>,
        _secret_in: Vec<BFieldElement>,
        _memory: &mut HashMap<BFieldElement, BFieldElement>,
    ) {
        let val: u32 = stack.pop().unwrap().try_into().unwrap();
        let log_2_floor = log_2_floor(val as u128);
        stack.push(BFieldElement::new(log_2_floor));
    }
}

#[cfg(test)]
mod tests {
    use twenty_first::shared_math::b_field_element::BFieldElement;

    use crate::get_init_tvm_stack;
    use crate::snippet_bencher::bench_and_write;
    use crate::test_helpers::{rust_tasm_equivalence_prop, rust_tasm_equivalence_prop_new};

    use super::*;

    #[test]
    fn log_2_floor_u32_test() {
        rust_tasm_equivalence_prop_new::<Log2FloorU32>();
    }

    #[test]
    fn log_2_floor_u32_benchmark() {
        bench_and_write::<Log2FloorU32>();
    }

    #[test]
    fn u32_log2_floor() {
        let mut expected_stack = get_init_tvm_stack();
        expected_stack.push(BFieldElement::new(0));
        prop_log2_floor(1, &expected_stack);

        expected_stack = get_init_tvm_stack();
        expected_stack.push(BFieldElement::new(1));
        prop_log2_floor(2, &expected_stack);

        expected_stack = get_init_tvm_stack();
        expected_stack.push(BFieldElement::new(1));
        prop_log2_floor(3, &expected_stack);

        expected_stack = get_init_tvm_stack();
        expected_stack.push(BFieldElement::new(2));
        prop_log2_floor(4, &expected_stack);

        expected_stack = get_init_tvm_stack();
        expected_stack.push(BFieldElement::new(2));
        prop_log2_floor(5, &expected_stack);

        expected_stack = get_init_tvm_stack();
        expected_stack.push(BFieldElement::new(2));
        prop_log2_floor(6, &expected_stack);

        expected_stack = get_init_tvm_stack();
        expected_stack.push(BFieldElement::new(2));
        prop_log2_floor(7, &expected_stack);

        expected_stack = get_init_tvm_stack();
        expected_stack.push(BFieldElement::new(3));
        prop_log2_floor(8, &expected_stack);

        expected_stack = get_init_tvm_stack();
        expected_stack.push(BFieldElement::new(4));
        prop_log2_floor(16, &expected_stack);

        expected_stack = get_init_tvm_stack();
        expected_stack.push(BFieldElement::new(4));
        prop_log2_floor(17, &expected_stack);

        expected_stack = get_init_tvm_stack();
        expected_stack.push(BFieldElement::new(4));
        prop_log2_floor(18, &expected_stack);

        expected_stack = get_init_tvm_stack();
        expected_stack.push(BFieldElement::new(4));
        prop_log2_floor(19, &expected_stack);

        expected_stack = get_init_tvm_stack();
        expected_stack.push(BFieldElement::new(4));
        prop_log2_floor(31, &expected_stack);

        expected_stack = get_init_tvm_stack();
        expected_stack.push(BFieldElement::new(5));
        prop_log2_floor(32, &expected_stack);

        for i in 0..32 {
            expected_stack = get_init_tvm_stack();
            expected_stack.push(BFieldElement::new(i));
            prop_log2_floor(1 << i, &expected_stack);

            if i > 0 {
                prop_log2_floor((1 << i) + 1, &expected_stack);
            }
            if i > 1 {
                prop_log2_floor((1 << i) + 2, &expected_stack);
                prop_log2_floor((1 << i) + 3, &expected_stack);
            }
        }
    }

    fn prop_log2_floor(value: u32, expected: &[BFieldElement]) {
        let mut init_stack = get_init_tvm_stack();
        init_stack.push(BFieldElement::new(value as u64));

        let _execution_result = rust_tasm_equivalence_prop::<Log2FloorU32>(
            &init_stack,
            &[],
            &[],
            &mut HashMap::default(),
            0,
            Some(expected),
        );
    }
}