use std::collections::HashMap;

use rand::{thread_rng, Rng, RngCore};
use twenty_first::shared_math::b_field_element::BFieldElement;

use crate::snippet::{DataType, Snippet};
use crate::snippet_state::SnippetState;
use crate::{get_init_tvm_stack, ExecutionState};

#[derive(Clone, Debug)]
pub struct ShiftLeftU32;

impl Snippet for ShiftLeftU32 {
    fn entrypoint(&self) -> String {
        "tasm_arithmetic_u32_shift_left_u32".to_string()
    }

    fn inputs(&self) -> Vec<String>
    where
        Self: Sized,
    {
        vec!["value".to_string(), "shift".to_string()]
    }

    fn input_types(&self) -> Vec<DataType> {
        vec![DataType::U32, DataType::U32]
    }

    fn output_types(&self) -> Vec<DataType> {
        vec![DataType::U32]
    }

    fn outputs(&self) -> Vec<String>
    where
        Self: Sized,
    {
        vec!["value << shift".to_string()]
    }

    fn stack_diff(&self) -> isize
    where
        Self: Sized,
    {
        -1
    }

    fn function_code(&self, _library: &mut SnippetState) -> String {
        let entrypoint = self.entrypoint();

        // I'm unsure if we should do a bounds check to check if `shift < 32`
        format!(
            "
            // BEFORE: _ value shift
            // AFTER: _ (value << shift)
            {entrypoint}:
                // Bounds check. May be superfluous but this mimics Rust's behavior.
                push 32
                dup 1
                lt
                assert

                push 2
                pow
                mul
                split
                swap 1
                pop

                return
                "
        )
    }

    fn crash_conditions(&self) -> Vec<String>
    where
        Self: Sized,
    {
        vec![
            "inputs are not valid u32s".to_string(),
            "attempting to left shift with a value greater than 31".to_string(),
        ]
    }

    fn gen_input_states(&self) -> Vec<crate::ExecutionState> {
        let mut rng = thread_rng();
        let mut ret: Vec<ExecutionState> = vec![];
        for _ in 0..100 {
            let value = rng.next_u32();
            let shift = rng.gen_range(0..32);
            ret.push(prepare_state(value, shift));
        }

        ret
    }

    fn common_case_input_state(&self) -> ExecutionState
    where
        Self: Sized,
    {
        prepare_state((1 << 16) - 1, 16)
    }

    fn worst_case_input_state(&self) -> ExecutionState
    where
        Self: Sized,
    {
        prepare_state(u32::MAX, 31)
    }

    fn rust_shadowing(
        &self,
        stack: &mut Vec<BFieldElement>,
        _std_in: Vec<BFieldElement>,
        _secret_in: Vec<BFieldElement>,
        _memory: &mut HashMap<BFieldElement, BFieldElement>,
    ) where
        Self: Sized,
    {
        // Find shift amount
        let shift_amount: u32 = stack.pop().unwrap().try_into().unwrap();

        // Original value
        let value: u32 = stack.pop().unwrap().try_into().unwrap();

        let ret = value << shift_amount;
        stack.push((ret as u64).into());
    }
}

fn prepare_state(value: u32, shift: u32) -> ExecutionState {
    let mut stack = get_init_tvm_stack();
    let value = BFieldElement::new(value as u64);
    let shift = BFieldElement::new(shift as u64);
    stack.push(value);
    stack.push(shift);

    ExecutionState::with_stack(stack)
}

#[cfg(test)]
mod tests {
    use crate::{
        snippet_bencher::bench_and_write,
        test_helpers::{rust_tasm_equivalence_prop, rust_tasm_equivalence_prop_new},
    };

    use super::*;

    #[test]
    fn shift_left_test() {
        rust_tasm_equivalence_prop_new(&ShiftLeftU32, true);
    }

    #[test]
    fn shift_left_benchmark() {
        bench_and_write(ShiftLeftU32);
    }

    #[test]
    fn shift_left_max_value_test() {
        for i in 0..32 {
            prop_shift_left(u32::MAX, i);
        }
    }

    #[test]
    #[should_panic]
    fn shift_beyond_limit() {
        let mut init_stack = get_init_tvm_stack();
        init_stack.push(BFieldElement::new(u32::MAX as u64));
        init_stack.push(32u64.into());
        ShiftLeftU32.run_tasm(&mut ExecutionState::with_stack(init_stack));
    }

    fn prop_shift_left(value: u32, shift_amount: u32) {
        let mut init_stack = get_init_tvm_stack();
        init_stack.push(BFieldElement::new(value as u64));
        init_stack.push(BFieldElement::new(shift_amount as u64));

        let expected_u32 = value << shift_amount;

        let mut expected_stack = get_init_tvm_stack();
        expected_stack.push((expected_u32 as u64).into());

        let _execution_result = rust_tasm_equivalence_prop(
            &ShiftLeftU32,
            &init_stack,
            &[],
            &[],
            &mut HashMap::default(),
            0,
            Some(&expected_stack),
        );
    }
}
