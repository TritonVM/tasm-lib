use rand::prelude::*;
use triton_vm::prelude::*;

use crate::data_type::DataType;
use crate::empty_stack;
use crate::library::Library;
use crate::push_encodable;
use crate::traits::deprecated_snippet::DeprecatedSnippet;
use crate::InitVmState;

#[derive(Clone, Debug)]
pub struct ShiftLeftStatic<const N: u8>;

impl<const N: u8> DeprecatedSnippet for ShiftLeftStatic<N> {
    fn entrypoint_name(&self) -> String {
        assert!(
            N <= 32,
            "Static left-shift snippet cannot shift by more than 32 bits"
        );
        format!("tasmlib_arithmetic_u128_shift_left_static_{N}")
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
        ]
    }

    fn input_types(&self) -> Vec<DataType> {
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

    fn output_types(&self) -> Vec<DataType> {
        vec![DataType::U128]
    }

    fn stack_diff(&self) -> isize
    where
        Self: Sized,
    {
        0
    }

    fn function_code(&self, _library: &mut Library) -> String {
        assert!(
            N <= 32,
            "Static shift-snippet cannot left-shift by more than 32 bits"
        );
        let entrypoint = self.entrypoint_name();
        let pow_2_n = 2u64.pow(N as u32);
        format!(
            "
            // BEFORE: _ limb3 limb2 limb1 limb0
            // AFTER:  _ (value << shift)_3 (value << shift)_2 (value << shift)_1 (value << shift)_0
            {entrypoint}:
                push {pow_2_n}
                // _ v3 v2 v1 v0 (2 ^ shift)

                mul             // _ v3 v2 v1 v0<<shift
                swap 3          // _ v0<<shift v2 v1 v3
                push {pow_2_n}
                xb_mul           // _ v0<<shift v2<<shift v1<<shift v3<<shift
                swap 3          // _ v3<<shift v2<<shift v1<<shift v0<<shift

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
            "
        )
    }

    fn crash_conditions(&self) -> Vec<String>
    where
        Self: Sized,
    {
        vec![]
    }

    fn gen_input_states(&self) -> Vec<InitVmState>
    where
        Self: Sized,
    {
        let mut ret = vec![];
        for _ in 0..5 {
            ret.push(prepare_state(random::<u128>()));
        }
        ret
    }

    fn common_case_input_state(&self) -> InitVmState
    where
        Self: Sized,
    {
        prepare_state(0x1282)
    }

    fn worst_case_input_state(&self) -> InitVmState
    where
        Self: Sized,
    {
        prepare_state(0x123456789abcdef)
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
        // Original value
        let mut value = 0u128;
        for i in 0..4 {
            value |= (stack.pop().unwrap().value() as u128) << (i * 32);
        }

        value <<= N;

        for i in 0..4 {
            let limb = ((value >> ((3 - i) * 32)) & u32::MAX as u128) as u32;
            stack.push(BFieldElement::new(limb as u64));
        }
    }
}

fn prepare_state(value: u128) -> InitVmState {
    let mut init_stack = empty_stack();
    push_encodable(&mut init_stack, &value.encode());
    InitVmState::with_stack(init_stack)
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use super::*;
    use crate::test_helpers::test_rust_equivalence_given_input_values_deprecated;
    use crate::test_helpers::test_rust_equivalence_multiple_deprecated;

    #[test]
    fn shift_left_u128_test() {
        test_rust_equivalence_multiple_deprecated(&ShiftLeftStatic::<0>, false);
        test_rust_equivalence_multiple_deprecated(&ShiftLeftStatic::<1>, true);
        test_rust_equivalence_multiple_deprecated(&ShiftLeftStatic::<2>, true);
        test_rust_equivalence_multiple_deprecated(&ShiftLeftStatic::<3>, true);
        test_rust_equivalence_multiple_deprecated(&ShiftLeftStatic::<4>, true);
        test_rust_equivalence_multiple_deprecated(&ShiftLeftStatic::<5>, true);
        test_rust_equivalence_multiple_deprecated(&ShiftLeftStatic::<6>, true);
        test_rust_equivalence_multiple_deprecated(&ShiftLeftStatic::<7>, true);
        test_rust_equivalence_multiple_deprecated(&ShiftLeftStatic::<8>, true);
        test_rust_equivalence_multiple_deprecated(&ShiftLeftStatic::<9>, true);
        test_rust_equivalence_multiple_deprecated(&ShiftLeftStatic::<10>, true);
        test_rust_equivalence_multiple_deprecated(&ShiftLeftStatic::<11>, true);
        test_rust_equivalence_multiple_deprecated(&ShiftLeftStatic::<12>, true);
        test_rust_equivalence_multiple_deprecated(&ShiftLeftStatic::<13>, true);
        test_rust_equivalence_multiple_deprecated(&ShiftLeftStatic::<14>, true);
        test_rust_equivalence_multiple_deprecated(&ShiftLeftStatic::<15>, true);
        test_rust_equivalence_multiple_deprecated(&ShiftLeftStatic::<16>, true);
        test_rust_equivalence_multiple_deprecated(&ShiftLeftStatic::<17>, true);
        test_rust_equivalence_multiple_deprecated(&ShiftLeftStatic::<18>, true);
        test_rust_equivalence_multiple_deprecated(&ShiftLeftStatic::<19>, true);
        test_rust_equivalence_multiple_deprecated(&ShiftLeftStatic::<20>, true);
        test_rust_equivalence_multiple_deprecated(&ShiftLeftStatic::<21>, true);
        test_rust_equivalence_multiple_deprecated(&ShiftLeftStatic::<22>, true);
        test_rust_equivalence_multiple_deprecated(&ShiftLeftStatic::<23>, true);
        test_rust_equivalence_multiple_deprecated(&ShiftLeftStatic::<24>, true);
        test_rust_equivalence_multiple_deprecated(&ShiftLeftStatic::<25>, true);
        test_rust_equivalence_multiple_deprecated(&ShiftLeftStatic::<26>, true);
        test_rust_equivalence_multiple_deprecated(&ShiftLeftStatic::<27>, true);
        test_rust_equivalence_multiple_deprecated(&ShiftLeftStatic::<28>, true);
        test_rust_equivalence_multiple_deprecated(&ShiftLeftStatic::<29>, true);
        test_rust_equivalence_multiple_deprecated(&ShiftLeftStatic::<30>, true);
        test_rust_equivalence_multiple_deprecated(&ShiftLeftStatic::<31>, true);
        test_rust_equivalence_multiple_deprecated(&ShiftLeftStatic::<32>, true);
    }

    #[test]
    fn shift_left_simple_test() {
        prop_left_left::<1>(1);
        prop_left_left::<2>(8);
    }

    #[test]
    fn shift_left_max_values_test() {
        prop_left_left::<0>(u128::MAX);
        prop_left_left::<1>(u128::MAX);
        prop_left_left::<12>(u128::MAX);
        prop_left_left::<24>(u128::MAX);
        prop_left_left::<31>(u128::MAX);
        prop_left_left::<32>(u128::MAX);
    }

    #[test]
    fn shift_left_zero_test() {
        prop_left_left::<0>(0);
        prop_left_left::<1>(0);
        prop_left_left::<12>(0);
        prop_left_left::<24>(0);
        prop_left_left::<31>(0);
        prop_left_left::<32>(0);
    }

    #[test]
    #[should_panic]
    fn shift_beyond_limit() {
        let mut init_stack = empty_stack();
        init_stack.push(BFieldElement::new(u32::MAX as u64));
        init_stack.push(BFieldElement::new(u32::MAX as u64));
        init_stack.push(BFieldElement::new(u32::MAX as u64));
        init_stack.push(BFieldElement::new(u32::MAX as u64));
        ShiftLeftStatic::<33>
            .link_and_run_tasm_from_state_for_test(&mut InitVmState::with_stack(init_stack));
    }

    fn prop_left_left<const N: u8>(value: u128) {
        let mut init_stack = empty_stack();
        for i in 0..4 {
            init_stack.push(BFieldElement::new(
                ((value >> (32 * (3 - i))) as u32) as u64,
            ));
        }

        let expected_u128 = value << N;
        println!("{value} << {N} = {expected_u128}");

        let mut expected_stack = empty_stack();
        for i in 0..4 {
            expected_stack.push(BFieldElement::new(
                ((expected_u128 >> (32 * (3 - i))) & u32::MAX as u128) as u64,
            ));
        }

        test_rust_equivalence_given_input_values_deprecated(
            &ShiftLeftStatic::<N>,
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
    fn shift_left_u128_benchmark() {
        bench_and_write(ShiftLeftStatic::<5>);
    }
}
