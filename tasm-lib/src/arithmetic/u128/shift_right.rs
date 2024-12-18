use rand::prelude::*;
use triton_vm::prelude::*;
use twenty_first::prelude::U32s;

use crate::empty_stack;
use crate::prelude::*;
use crate::push_encodable;
use crate::traits::deprecated_snippet::DeprecatedSnippet;
use crate::InitVmState;

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub struct ShiftRight;

impl DeprecatedSnippet for ShiftRight {
    fn entrypoint_name(&self) -> String {
        "tasmlib_arithmetic_u128_shift_right".to_string()
    }

    fn input_field_names(&self) -> Vec<String> {
        vec![
            "input_3".to_string(),
            "input_2".to_string(),
            "input_1".to_string(),
            "input_0".to_string(),
            "shamt".to_string(),
        ]
    }

    fn input_types(&self) -> Vec<DataType> {
        vec![DataType::U128, DataType::U32]
    }

    fn output_field_names(&self) -> Vec<String> {
        vec![
            "shifted_value_3".to_string(),
            "shifted_value_2".to_string(),
            "shifted_value_1".to_string(),
            "shifted_value_0".to_string(),
        ]
    }

    fn output_types(&self) -> Vec<DataType> {
        vec![DataType::U128]
    }

    fn stack_diff(&self) -> isize {
        -1
    }

    fn function_code(&self, _library: &mut Library) -> String {
        let entrypoint = self.entrypoint_name();
        format!(
            "
            // BEFORE: _ v_3 v_2 v_1 v_0 shift
            // AFTER:  _ (v >> shift)_3 (v >> shift)_2 (v >> shift)_1 (v >> shift)_0
            {entrypoint}:
                // Bounds check: Verify that shift amount is less than 128.
                push 128
                dup 1
                lt
                assert
                // _ v_3 v_2 v_1 v_0 shift

                // If shift amount is greater than 32, we need to special-case!
                dup 0
                push 32
                lt
                // _ v_3 v_2 v_1 v_0 shift (shift > 32)

                skiz
                    call {entrypoint}_handle_hi_shift
                // _ v_3 v_2 v_1 v_0 shift

                push -1
                mul
                push 32
                add
                // _ v_3 v_2 v_1 v_0 (32 - shift)

                push 2
                pow
                // _ v_3 v_2 v_1 v_0 (2 ^ (32 - shift))

                swap 4 swap 3 swap 2 swap 1
                // _ (2 ^ (32 - shift)) v_3 v_2 v_1 v_0

                dup 4
                // _ (2 ^ (32 - shift)) v_3 v_2 v_1 v_0 (2 ^ (32 - shift))

                mul
                split
                pop 1
                // _ (2 ^ (32 - shift)) v_3 v_2 v_1 (v_0 >> shift)

                swap 1
                // _ (2 ^ (32 - shift)) v_3 v_2 (v_0 >> shift) v_1

                dup 4 // _ (2 ^ (32 - shift)) v_3 v_2 (v_0 >> shift) v_1 (2 ^ (32 - shift))
                mul
                split
                // _ (2 ^ (32 - shift)) v_3 v_2 (v_0 >> shift) (v_1 >> shift) carry

                swap 1 swap 2 add
                // _ (2 ^ (32 - shift)) v_3 v_2 (v_1 >> shift) (v >> shift)_0

                swap 2 dup 4
                // _ (2 ^ (32 - shift)) v_3 (v >> shift)_0 (v_1 >> shift) v_2 (2 ^ (32 - shift))

                mul split
                // _ (2 ^ (32 - shift)) v_3 (v >> shift)_0 (v_1 >> shift) (v_2 >> shift) carry

                swap 1 swap 2 add
                // _ (2 ^ (32 - shift)) v_3 (v >> shift)_0 (v_2 >> shift) (v >> shift)_1

                swap 3
                // _ (2 ^ (32 - shift)) (v >> shift)_1 (v >> shift)_0 (v_2 >> shift) v_3

                dup 4 mul split
                // _ (2 ^ (32 - shift)) (v >> shift)_1 (v >> shift)_0 (v_2 >> shift) (v_3 >> shift) carry

                swap 1 swap 2 add
                // _ (2 ^ (32 - shift)) (v >> shift)_1 (v >> shift)_0 (v_3 >> shift) (v >> shift)_2

                // _ (2 ^ (32 - shift)) (v >> shift)_1 (v >> shift)_0 (v >> shift)_3 (v >> shift)_2

                swap 1 swap 4 pop 1
                // _ (v >> shift)_3 (v >> shift)_1 (v >> shift)_0 (v >> shift)_2

                swap 2 swap 1
                // _ (v >> shift)_3 (v >> shift)_2 (v >> shift)_0 (v >> shift)_1

                return

            // start: _ v_3 v_2 v_1 v_0 shift
            // end: _ (v >> 32)_3 (v >> 32)_2 (v >> 32)_1 (v >> 32)_0 (shift - 32)
            {entrypoint}_handle_hi_shift:
                push -32
                add
                // _ v_3 v_2 v_1 v_0 (shift - 32)

                swap 4 swap 3 swap 2 swap 1 push 32
                // _ (shift - 32) v_3 v_2 v_1 v_0 32

                call {entrypoint}
                // _ (shift - 32) (v >> 32)_3 (v >> 32)_2 (v >> 32)_1 (v >> 32)_0

                swap 1 swap 2 swap 3 swap 4
                // _ (v >> 32)_3 (v >> 32)_2 (v >> 32)_1 (v >> 32)_0 (shift - 32)

                // if shift - 32 is still greater than 32, we need to special-case again!
                dup 0
                push 32
                lt
                // _ (v >> 32)_3 (v >> 32)_2 (v >> 32)_1 (v >> 32)_0 (shift - 32) (shift - 32 > 32)
                skiz
                    recurse

                return
            "
        )
    }

    fn crash_conditions(&self) -> Vec<String> {
        vec!["Shift amount is greater than 128".to_string()]
    }

    fn gen_input_states(&self) -> Vec<InitVmState> {
        let mut ret = vec![];
        for i in 0..128 {
            ret.push(prepare_state(random::<u128>(), i));
        }

        ret
    }

    fn common_case_input_state(&self) -> InitVmState {
        prepare_state(0x642, 20)
    }

    fn worst_case_input_state(&self) -> InitVmState {
        prepare_state(0x123, 127)
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
        let a_0: u32 = stack.pop().unwrap().try_into().unwrap();
        let a_1: u32 = stack.pop().unwrap().try_into().unwrap();
        let a_2: u32 = stack.pop().unwrap().try_into().unwrap();
        let a_3: u32 = stack.pop().unwrap().try_into().unwrap();
        let a =
            ((a_3 as u128) << 96) + ((a_2 as u128) << 64) + ((a_1 as u128) << 32) + (a_0 as u128);

        let ret = a >> shift_amount;
        push_encodable(stack, &ret);
    }
}

fn prepare_state(value: u128, shift_amount: u32) -> InitVmState {
    let value = U32s::<4>::try_from(value).unwrap();
    let mut init_stack = empty_stack();
    push_encodable(&mut init_stack, &value);
    init_stack.push(BFieldElement::new(shift_amount as u64));
    InitVmState::with_stack(init_stack)
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use super::*;
    use crate::test_helpers::test_rust_equivalence_given_input_values_deprecated;
    use crate::test_helpers::test_rust_equivalence_multiple_deprecated;

    #[test]
    fn shift_right_u128_test() {
        test_rust_equivalence_multiple_deprecated(&ShiftRight, true);
    }

    #[test]
    fn shift_right_unit_test() {
        prop_shift_right(8, 2);
        prop_shift_right(1 << 65, 2);
        prop_shift_right(1 << 127, 31);
        prop_shift_right(1 << 127, 32);
        prop_shift_right(1 << 127, 33);
        prop_shift_right(u128::MAX, 0);
        prop_shift_right(u128::MAX, 1);
        prop_shift_right(u128::MAX, 31);
        prop_shift_right(u128::MAX, 32);
        prop_shift_right(u128::MAX, 33);
    }

    #[test]
    fn regression_test_crashing_code_shift_right_u128() {
        prop_shift_right(1 << 127, 65)
    }

    #[test]
    fn shift_right_max_values_and_zeros_test() {
        for i in 0..128 {
            prop_shift_right((u64::MAX as u128) << 38, i);
        }
        for i in 0..128 {
            prop_shift_right(u128::MAX, i);
            prop_shift_right(0, i);
        }
    }

    fn prop_shift_right(value: u128, shift_amount: u32) {
        let mut init_stack = empty_stack();
        let value_as_u32s = U32s::<4>::try_from(value).unwrap();
        for limb in value_as_u32s.encode().into_iter().rev() {
            init_stack.push(limb);
        }

        init_stack.push(BFieldElement::new(shift_amount as u64));

        let expected_u128 = value >> shift_amount;

        let mut expected_stack = empty_stack();
        let expected_value_as_u32s = U32s::<4>::try_from(expected_u128).unwrap();
        for limb in expected_value_as_u32s.encode().into_iter().rev() {
            expected_stack.push(limb);
        }

        test_rust_equivalence_given_input_values_deprecated(
            &ShiftRight,
            &init_stack,
            &[],
            HashMap::default(),
            Some(&expected_stack),
        );
    }
}

#[cfg(test)]
mod benches {
    use super::*;
    use crate::snippet_bencher::bench_and_write;

    #[test]
    fn benchmark() {
        bench_and_write(ShiftRight);
    }
}
