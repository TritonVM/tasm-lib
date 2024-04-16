use rand::thread_rng;
use rand::RngCore;
use triton_vm::prelude::*;
use triton_vm::twenty_first::prelude::U32s;

use crate::data_type::DataType;
use crate::empty_stack;
use crate::library::Library;
use crate::push_encodable;
use crate::traits::deprecated_snippet::DeprecatedSnippet;
use crate::ExecutionState;

#[derive(Clone, Debug)]
pub struct ShiftLeftU64;

impl DeprecatedSnippet for ShiftLeftU64 {
    fn entrypoint_name(&self) -> String {
        "tasmlib_arithmetic_u64_shift_left".to_string()
    }

    fn input_field_names(&self) -> Vec<String> {
        vec![
            "value_hi".to_string(),
            "value_lo".to_string(),
            "shift_amount".to_string(),
        ]
    }

    fn input_types(&self) -> Vec<DataType> {
        vec![DataType::U64, DataType::U32]
    }

    fn output_field_names(&self) -> Vec<String> {
        vec![
            "shifted_value_hi".to_string(),
            "shifted_value_lo".to_string(),
        ]
    }

    fn output_types(&self) -> Vec<DataType> {
        vec![DataType::U64]
    }

    fn stack_diff(&self) -> isize {
        -1
    }

    fn function_code(&self, _library: &mut Library) -> String {
        let entrypoint = self.entrypoint_name();
        format!(
            "
            // BEFORE: _ value_hi value_lo shift
            // AFTER:  _ (value << shift)_hi (value << shift)_lo
            {entrypoint}:
                // Bounds check: Verify that shift amount is less than 64.
                push 64
                dup 1
                lt
                assert
                // _ value_hi value_lo shift

                // If shift amount is greater than 32, we need to special-case!
                dup 0
                push 32
                lt
                // _ value_hi value_lo shift (shift > 32)

                skiz
                    call {entrypoint}_handle_hi_shift
                // _ value_hi value_lo shift

                push 2
                pow
                // _ value_hi value_lo (2 ^ shift)

                swap 2 dup 2
                // _ (2 ^ shift) value_lo value_hi (2 ^ shift)

                mul
                // _ (2 ^ shift) value_lo (value_hi << shift)_bfe

                split
                swap 1
                pop 1
                // _ (2 ^ shift) value_lo (value_hi << shift)

                swap 2
                mul
                // _ (value_hi << shift) (value_lo << shift)_bfe

                split
                // _ (value_hi << shift) carry (value_lo << shift)_lo

                swap 2
                // _ (value_lo << shift)_lo carry (value_hi << shift)

                add
                // _ (value_lo << shift)_lo (value << shift)_hi

                swap 1
                // _ (value << shift)_hi (value << shift)_lo

                return

            // start: _ value_hi value_lo shift
            // end: _ (value << 32)_hi (value << 32)_lo (shift - 32)
            {entrypoint}_handle_hi_shift:
                push -32
                add
                // _ value_hi value_lo (shift - 32)

                swap 2 swap 1 push 32
                // _ (shift - 32) value_hi value_lo 32

                call {entrypoint}
                // _ (shift - 32) (value << 32)_hi (value << 32)_lo

                swap 1 swap 2
                // _ (value << 32)_hi (value << 32)_lo (shift - 32)

                return
            "
        )
    }

    fn crash_conditions(&self) -> Vec<String> {
        vec![
            "Shift amount is 64 or greater".to_string(),
            "inputs are not valid u32s".to_string(),
        ]
    }

    fn gen_input_states(&self) -> Vec<ExecutionState> {
        let mut rng = thread_rng();
        let mut ret = vec![];
        for _ in 0..3 {
            for i in 0..64 {
                ret.push(prepare_state((rng.next_u32() as u64) * 2, i));
            }
        }

        ret
    }

    fn common_case_input_state(&self) -> ExecutionState {
        prepare_state(0x642, 15)
    }

    fn worst_case_input_state(&self) -> ExecutionState {
        prepare_state(0x123456789abcdef, 33)
    }

    fn rust_shadowing(
        &self,
        stack: &mut Vec<BFieldElement>,
        _std_in: Vec<BFieldElement>,
        _secret_in: Vec<BFieldElement>,
        _memory: &mut std::collections::HashMap<BFieldElement, BFieldElement>,
    ) {
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
    let mut init_stack = empty_stack();
    push_encodable(&mut init_stack, &value);
    init_stack.push(BFieldElement::new(shift_amount as u64));
    ExecutionState::with_stack(init_stack)
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use crate::test_helpers::test_rust_equivalence_given_input_values_deprecated;
    use crate::test_helpers::test_rust_equivalence_multiple_deprecated;

    use super::*;

    #[test]
    fn shift_left_u64_test() {
        test_rust_equivalence_multiple_deprecated(&ShiftLeftU64, true);
    }

    #[test]
    fn shift_left_simple_test() {
        prop_left_left(8, 2);
    }

    #[test]
    fn shift_left_max_values_test() {
        for i in 0..64 {
            prop_left_left(u64::MAX, i);
        }
    }

    #[test]
    #[should_panic]
    fn shift_beyond_limit() {
        let mut init_stack = empty_stack();
        init_stack.push(BFieldElement::new(u32::MAX as u64));
        init_stack.push(BFieldElement::new(u32::MAX as u64));
        init_stack.push(64u64.into());
        ShiftLeftU64
            .link_and_run_tasm_from_state_for_test(&mut ExecutionState::with_stack(init_stack));
    }

    fn prop_left_left(value: u64, shift_amount: u32) {
        let mut init_stack = empty_stack();
        init_stack.push(BFieldElement::new(value >> 32));
        init_stack.push(BFieldElement::new(value & u32::MAX as u64));
        init_stack.push(BFieldElement::new(shift_amount as u64));

        let expected_u64 = value << shift_amount;
        println!("{value} << {shift_amount} = {expected_u64}");

        let mut expected_stack = empty_stack();
        expected_stack.push((expected_u64 >> 32).into());
        expected_stack.push((expected_u64 & u32::MAX as u64).into());

        test_rust_equivalence_given_input_values_deprecated(
            &ShiftLeftU64,
            &init_stack,
            &[],
            HashMap::default(),
            Some(&expected_stack),
        );
    }
}

#[cfg(test)]
mod benches {
    use crate::snippet_bencher::bench_and_write;

    use super::*;

    #[test]
    fn shift_left_u64_benchmark() {
        bench_and_write(ShiftLeftU64);
    }
}
