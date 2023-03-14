use rand::{thread_rng, RngCore};
use twenty_first::amount::u32s::U32s;
use twenty_first::shared_math::b_field_element::BFieldElement;

use crate::library::Library;
use crate::snippet::{DataType, Snippet};
use crate::{get_init_tvm_stack, push_hashable, ExecutionState};

#[derive(Clone)]
pub struct ShiftLeftU64;

impl Snippet for ShiftLeftU64 {
    fn entrypoint(&self) -> String {
        "tasm_arithmetic_u64_shift_left".to_string()
    }

    fn inputs(&self) -> Vec<String>
    where
        Self: Sized,
    {
        vec![
            "value_hi".to_string(),
            "value_lo".to_string(),
            "shift_amount".to_string(),
        ]
    }

    fn input_types(&self) -> Vec<DataType> {
        vec![DataType::U64, DataType::U32]
    }

    fn output_types(&self) -> Vec<DataType> {
        vec![DataType::U64]
    }

    fn outputs(&self) -> Vec<String>
    where
        Self: Sized,
    {
        vec![
            "shifted_value_hi".to_string(),
            "shifted_value_lo".to_string(),
        ]
    }

    fn stack_diff(&self) -> isize
    where
        Self: Sized,
    {
        -1
    }

    fn function_body(&self, _library: &mut Library) -> String {
        let entrypoint = self.entrypoint();
        format!(
            "
            // BEFORE: _ value_hi value_lo shift
            // AFTER: _ (value << shift)_hi (value << shift)_lo
            {entrypoint}:
                push 2
                pow
                // _ value_hi value_lo (2 ^ shift)

                swap2 dup2
                // _ (2 ^ shift) value_lo value_hi (2 ^ shift)

                mul
                // _ (2 ^ shift) value_lo (value_hi << shift)_bfe

                split
                swap1
                pop
                // _ (2 ^ shift) value_lo (value_hi << shift)

                swap2
                mul
                // _ (value_hi << shift) (value_lo << shift)_bfe

                split
                // _ (value_hi << shift) carry (value_lo << shift)_lo

                swap2
                // _ (value_lo << shift)_lo carry (value_hi << shift)

                add
                // _ (value_lo << shift)_lo (value << shift)_hi

                swap1
                // _ (value << shift)_hi (value << shift)_lo

                return
            "
        )
    }

    fn crash_conditions() -> Vec<String>
    where
        Self: Sized,
    {
        vec![]
    }

    fn gen_input_states(&self) -> Vec<ExecutionState>
    where
        Self: Sized,
    {
        let mut rng = thread_rng();
        let mut ret = vec![];
        for i in 0..32 {
            ret.push(prepare_state((rng.next_u32() as u64) * 2, i));
        }

        ret
    }

    fn common_case_input_state(&self) -> ExecutionState
    where
        Self: Sized,
    {
        prepare_state(0x642, 15)
    }

    fn worst_case_input_state(&self) -> ExecutionState
    where
        Self: Sized,
    {
        prepare_state(0x123456789abcdef, 31)
    }

    fn rust_shadowing(
        &self,
        stack: &mut Vec<BFieldElement>,
        _std_in: Vec<BFieldElement>,
        _secret_in: Vec<BFieldElement>,
        _memory: &mut std::collections::HashMap<BFieldElement, BFieldElement>,
    ) where
        Self: Sized,
    {
        // Find shift amount
        let shift_amount: u32 = stack.pop().unwrap().try_into().unwrap();

        // Original value
        let a_lo: u32 = stack.pop().unwrap().try_into().unwrap();
        let a_hi: u32 = stack.pop().unwrap().try_into().unwrap();
        let a = ((a_hi as u64) << 32) + a_lo as u64;

        let ret = a << shift_amount;
        stack.push((ret >> 32).into());
        stack.push((ret & u32::MAX as u64).into());
    }
}

fn prepare_state(value: u64, shift_amount: u32) -> ExecutionState {
    let value = U32s::<2>::try_from(value).unwrap();
    let mut init_stack = get_init_tvm_stack();
    push_hashable(&mut init_stack, &value);
    init_stack.push(BFieldElement::new(shift_amount as u64));
    ExecutionState::with_stack(init_stack)
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use crate::snippet_bencher::bench_and_write;
    use crate::test_helpers::{rust_tasm_equivalence_prop, rust_tasm_equivalence_prop_new};

    use super::*;

    #[test]
    fn shift_left_u64_test() {
        rust_tasm_equivalence_prop_new(ShiftLeftU64);
    }

    #[test]
    fn shift_left_u64_benchmark() {
        bench_and_write(ShiftLeftU64);
    }

    #[test]
    fn shift_left_unit_test() {
        prop_left_left(8, 2);
    }

    fn prop_left_left(value: u64, shift_amount: u32) {
        let mut init_stack = get_init_tvm_stack();
        init_stack.push(BFieldElement::new(value >> 32));
        init_stack.push(BFieldElement::new(value & u32::MAX as u64));
        init_stack.push(BFieldElement::new(shift_amount as u64));

        let expected_u64 = value << shift_amount;

        let mut expected_stack = get_init_tvm_stack();
        expected_stack.push((expected_u64 >> 32).into());
        expected_stack.push((expected_u64 & u32::MAX as u64).into());

        let _execution_result = rust_tasm_equivalence_prop(
            ShiftLeftU64,
            &init_stack,
            &[],
            &[],
            &mut HashMap::default(),
            0,
            Some(&expected_stack),
        );
    }
}
