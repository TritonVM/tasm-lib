use rand::random;
use twenty_first::shared_math::b_field_element::BFieldElement;
use twenty_first::shared_math::bfield_codec::BFieldCodec;

use crate::data_type::DataType;
use crate::library::Library;
use crate::traits::deprecated_snippet::DeprecatedSnippet;
use crate::{empty_stack, push_encodable, ExecutionState};

#[derive(Clone, Debug)]
pub struct ShiftLeftU128;

impl DeprecatedSnippet for ShiftLeftU128 {
    fn entrypoint_name(&self) -> String {
        "tasm_arithmetic_u128_shift_left".to_string()
    }

    fn input_field_names(&self) -> Vec<String>
    where
        Self: Sized,
    {
        vec![
            "value_limb3".to_string(),
            "value_limb2".to_string(),
            "value_limb1".to_string(),
            "value_limb0".to_string(),
            "shift_amount".to_string(),
        ]
    }

    fn input_types(&self) -> Vec<DataType> {
        vec![DataType::U128, DataType::U32]
    }

    fn output_types(&self) -> Vec<DataType> {
        vec![DataType::U128]
    }

    fn output_field_names(&self) -> Vec<String>
    where
        Self: Sized,
    {
        vec![
            "shifted_value_limb3".to_string(),
            "shifted_value_limb2".to_string(),
            "shifted_value_limb1".to_string(),
            "shifted_value_limb0".to_string(),
        ]
    }

    fn stack_diff(&self) -> isize
    where
        Self: Sized,
    {
        -1
    }

    fn function_code(&self, _library: &mut Library) -> String {
        let entrypoint = self.entrypoint_name();
        format!(
            "
            // BEFORE: _ limb3 limb2 limb1 limb0 shamt
            // AFTER:  _ (value << shift)_3 (value << shift)_2 (value << shift)_1 (value << shift)_0
            {entrypoint}:
                // Bounds check: Verify that shift amount is less than 128.
                push 128
                dup 1
                lt
                assert
                // _ limb3 limb2 limb1 limb0 shamt

                // If shift amount is greater than 32, we need to special-case!
                dup 0
                push 32
                lt
                // _ v3 v2 v1 v0 shift (shift > 32)

                skiz
                    call {entrypoint}_handle_hi_shift
                // _ v3 v2 v1 v0 shift

                push 2
                pow
                // _ v3 v2 v1 v0 (2 ^ shift)

                dup 0  // _ v3 v2 v1 v0 (2 ^ shift) (2 ^ shift)
                swap 5 // _ (2 ^ shift) v2 v1 v0 (2 ^ shift) v3

                mul    // _ (2 ^ shift) v2 v1 v0 v3<<shift
                swap 4 // _ v3<<shift v2 v1 v0 (2^shift)
                xbmul  // _ v3<<shift v2<<shift v1<<shift v0<<shift

                split  // _ v3s v2s v1s v0s_hi v0s_lo
                swap 2 // _ v3s v2s v0s_lo v0s_hi v1s
                split  // _ v3s v2s v0s_lo v0s_hi v1s_hi v1s_lo
                swap 4 // _ v3s v1s_lo v0s_lo v0s_hi v1s_hi v2s
                split  // _ v3s v1s_lo v0s_lo v0s_hi v1s_hi v2s_hi v2s_lo
                swap 6 // _ v2s_lo v1s_lo v0s_lo v0s_hi v1s_hi v2s_hi v3s
                split  // _ v2s_lo v1s_lo v0s_lo v0s_hi v1s_hi v2s_hi v3s_hi v3s_lo


                swap 1 // _ v2s_lo v1s_lo v0s_lo v0s_hi v1s_hi v2s_hi v3s_lo v3s_hi
                pop 1  // _ v2s_lo v1s_lo v0s_lo v0s_hi v1s_hi v2s_hi v3s_lo
                add    // _ v2s_lo v1s_lo v0s_lo v0s_hi v1s_hi w3
                swap 5 // _ w3 v1s_lo v0s_lo v0s_hi v1s_hi v2s_lo
                add    // _ w3 v1s_lo v0s_lo v0s_hi w2
                swap 3 // _ w3 w2 v0s_lo v0s_hi v1s_lo
                add    // _ w3 w2 v0s_lo w1
                swap 1 // _ w3 w2 w1 w0

                return

            // start: _ v3 v2 v1 v0 shift
            // end: _ (value << 32)_3 (value << 32)_2 (value << 32)_1 (value << 32)_0 (shift - 32)
            {entrypoint}_handle_hi_shift:
                push -32
                add
                // _ v3 v2 v1 v0 (shift - 32)

                swap 4 // _ (shift-32) v2 v1 v0 v3
                swap 3
                swap 2
                swap 1 // _ (shift-32) v3 v2 v1 v0
                push 32
                // _ (shift - 32) v3 v2 v1 v0 32

                call {entrypoint}
                // _ (shift - 32) (value << 32)_3 (value << 32)_2 (value << 32)_1 (value << 32)_0

                swap 1 // _ (shift - 32) (value << 32)_3 (value << 32)_2 (value << 32)_0 (value << 32)_1
                swap 2 // _ (shift - 32) (value << 32)_3 (value << 32)_1 (value << 32)_0 (value << 32)_2
                swap 3 // _ (shift - 32) (value << 32)_2 (value << 32)_1 (value << 32)_0 (value << 32)_3
                swap 4 // _ (value << 32)_3 (value << 32)_2 (value << 32)_1 (value << 32)_0 (shift - 32)

                // if (shift - 32) > 32, we need to special-case again
                dup 0
                push 32
                lt
                skiz
                    recurse

                return
            "
        )
    }

    fn crash_conditions(&self) -> Vec<String>
    where
        Self: Sized,
    {
        vec![
            "Shift amount is 128 or greater".to_string(),
            "inputs are not valid u32s".to_string(),
        ]
    }

    fn gen_input_states(&self) -> Vec<ExecutionState>
    where
        Self: Sized,
    {
        let mut ret = vec![];
        for _ in 0..3 {
            for i in 0..128 {
                ret.push(prepare_state(random::<u128>(), i));
            }
        }
        ret
    }

    fn common_case_input_state(&self) -> ExecutionState
    where
        Self: Sized,
    {
        prepare_state(0x1282, 15)
    }

    fn worst_case_input_state(&self) -> ExecutionState
    where
        Self: Sized,
    {
        prepare_state(0x123456789abcdef, 125)
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
        let mut value = 0u128;
        for i in 0..4 {
            value |= (stack.pop().unwrap().value() as u128) << (i * 32);
        }

        value <<= shift_amount;

        for i in 0..4 {
            let limb = ((value >> ((3 - i) * 32)) & u32::MAX as u128) as u32;
            stack.push(BFieldElement::new(limb as u64));
        }
    }
}

fn prepare_state(value: u128, shift_amount: u32) -> ExecutionState {
    let mut init_stack = empty_stack();
    push_encodable(&mut init_stack, &value.encode());
    init_stack.push(BFieldElement::new(shift_amount as u64));
    ExecutionState::with_stack(init_stack)
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use crate::test_helpers::{
        test_rust_equivalence_given_input_values_deprecated,
        test_rust_equivalence_multiple_deprecated,
    };

    use super::*;

    #[test]
    fn shift_left_u128_test() {
        test_rust_equivalence_multiple_deprecated(&ShiftLeftU128, true);
    }

    #[test]
    fn shift_left_simple_test() {
        prop_left_left(1, 1);
        prop_left_left(8, 2);
    }

    #[test]
    fn shift_left_max_values_and_zeros_test() {
        for i in 0..128 {
            prop_left_left(u128::MAX, i);
            prop_left_left(0, i);
        }
    }

    #[test]
    #[should_panic]
    fn shift_beyond_limit() {
        let mut init_stack = empty_stack();
        init_stack.push(BFieldElement::new(u32::MAX as u64));
        init_stack.push(BFieldElement::new(u32::MAX as u64));
        init_stack.push(BFieldElement::new(u32::MAX as u64));
        init_stack.push(BFieldElement::new(u32::MAX as u64));
        init_stack.push(128u64.into());
        ShiftLeftU128
            .link_and_run_tasm_from_state_for_test(&mut ExecutionState::with_stack(init_stack));
    }

    fn prop_left_left(value: u128, shift_amount: u32) {
        let mut init_stack = empty_stack();
        for i in 0..4 {
            init_stack.push(BFieldElement::new(
                ((value >> (32 * (3 - i))) as u32) as u64,
            ));
        }
        init_stack.push(BFieldElement::new(shift_amount as u64));

        let expected_u128 = value << shift_amount;
        println!("{value} << {shift_amount} = {expected_u128}");

        let mut expected_stack = empty_stack();
        for i in 0..4 {
            expected_stack.push(BFieldElement::new(
                ((expected_u128 >> (32 * (3 - i))) & u32::MAX as u128) as u64,
            ));
        }

        test_rust_equivalence_given_input_values_deprecated(
            &ShiftLeftU128,
            &init_stack,
            &[],
            HashMap::default(),
            0,
            Some(&expected_stack),
        );
    }
}

#[cfg(test)]
mod benches {
    use super::*;
    use crate::snippet_bencher::bench_and_write;

    #[test]
    fn shift_left_u128_benchmark() {
        bench_and_write(ShiftLeftU128);
    }
}
