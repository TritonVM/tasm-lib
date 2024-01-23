use rand::random;
use triton_vm::prelude::*;

use crate::data_type::DataType;
use crate::empty_stack;
use crate::library::Library;
use crate::push_encodable;
use crate::traits::deprecated_snippet::DeprecatedSnippet;
use crate::ExecutionState;

#[derive(Clone, Debug)]
pub struct ShiftRightStaticU128<const N: u8>;

impl<const N: u8> DeprecatedSnippet for ShiftRightStaticU128<N> {
    fn entrypoint_name(&self) -> String {
        assert!(
            N <= 32,
            "Static right shift cannot shift by more than 32 bits"
        );
        format!("tasm_arithmetic_u128_shift_right_static_{N}")
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
            "Static shift-snippet cannot right-shift by more than 32 bits"
        );
        let entrypoint = self.entrypoint_name();
        let pow_2_alt = 2u64.pow(32 - N as u32);
        format!(
            "
            // BEFORE: _ limb3 limb2 limb1 limb0
            // AFTER:  _ (value >> shift)_3 (value >> shift)_2 (value >> shift)_1 (value >> shift)_0
            {entrypoint}:
                push {pow_2_alt}
                // _ v3 v2 v1 v0 pow2

                mul             // _ v3 v2 v1 v0s
                swap 3          // _ v0s v2 v1 v3
                push {pow_2_alt}
                xbmul           // _ v0s v2s v1s v3s
                swap 3          // _ v3s v2s v1s v0s

                // add: v1_lo + v0s_hi etc.

                split  // _ v3s v2s v1s v0s_hi v0s_lo
                pop 1  // _ v3s v2s v1s v0s_hi
                swap 1 // _ v3s v2s v0s_hi v1s
                split  // _ v3s v2s v0s_hi v1s_hi v1s_lo
                swap 3 // _ v3s v1s_lo v0s_hi v1s_hi v2s
                split  // _ v3s v1s_lo v0s_hi v1s_hi v2s_hi v2s_lo
                swap 5 // _ v2s_lo v1s_lo v0s_hi v1s_hi v2s_hi v3s
                split  // _ v2s_lo v1s_lo v0s_hi v1s_hi v2s_hi v3s_hi v3s_lo

                // send v3s_hi to bottom of stack
                swap 1 // _ v2s_lo v1s_lo v0s_hi v1s_hi v2s_hi v3s_lo v3s_hi
                swap 6 // _ v3s_hi v1s_lo v0s_hi v1s_hi v2s_hi v3s_lo v2s_lo

                // add v3s_lo to v2s_hi
                swap 2 // _ w3 v1s_lo v0s_hi v1s_hi v2s_lo v3s_lo v2s_hi
                add    // _ w3 v1s_lo v0s_hi v1s_hi v2s_lo w2

                swap 4 // _ w3 w2 v0s_hi v1s_hi v2s_lo v1s_lo
                swap 2 // _ w3 w2 v0s_hi v1s_lo v2s_lo v1s_hi
                add    // _ w3 w2 v0s_hi v1s_lo w1

                swap 2 // _ w3 w2 w1 v1s_lo v0s_hi
                add    // _ w3 w2 w1 w0

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

    fn gen_input_states(&self) -> Vec<ExecutionState>
    where
        Self: Sized,
    {
        let mut ret = vec![];
        for _ in 0..5 {
            ret.push(prepare_state(random::<u128>()));
        }
        ret
    }

    fn common_case_input_state(&self) -> ExecutionState
    where
        Self: Sized,
    {
        prepare_state(0x1282)
    }

    fn worst_case_input_state(&self) -> ExecutionState
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

        value >>= N;

        for i in 0..4 {
            let limb = ((value >> ((3 - i) * 32)) & u32::MAX as u128) as u32;
            stack.push(BFieldElement::new(limb as u64));
        }
    }
}

fn prepare_state(value: u128) -> ExecutionState {
    let mut init_stack = empty_stack();
    push_encodable(&mut init_stack, &value.encode());
    ExecutionState::with_stack(init_stack)
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use crate::test_helpers::test_rust_equivalence_given_input_values_deprecated;
    use crate::test_helpers::test_rust_equivalence_multiple_deprecated;

    use super::*;

    #[test]
    fn shift_right_u128_test() {
        test_rust_equivalence_multiple_deprecated(&ShiftRightStaticU128::<0>, false);
        test_rust_equivalence_multiple_deprecated(&ShiftRightStaticU128::<1>, true);
        test_rust_equivalence_multiple_deprecated(&ShiftRightStaticU128::<2>, true);
        test_rust_equivalence_multiple_deprecated(&ShiftRightStaticU128::<3>, true);
        test_rust_equivalence_multiple_deprecated(&ShiftRightStaticU128::<4>, true);
        test_rust_equivalence_multiple_deprecated(&ShiftRightStaticU128::<5>, true);
        test_rust_equivalence_multiple_deprecated(&ShiftRightStaticU128::<6>, true);
        test_rust_equivalence_multiple_deprecated(&ShiftRightStaticU128::<7>, true);
        test_rust_equivalence_multiple_deprecated(&ShiftRightStaticU128::<8>, true);
        test_rust_equivalence_multiple_deprecated(&ShiftRightStaticU128::<9>, true);
        test_rust_equivalence_multiple_deprecated(&ShiftRightStaticU128::<10>, true);
        test_rust_equivalence_multiple_deprecated(&ShiftRightStaticU128::<11>, true);
        test_rust_equivalence_multiple_deprecated(&ShiftRightStaticU128::<12>, true);
        test_rust_equivalence_multiple_deprecated(&ShiftRightStaticU128::<13>, true);
        test_rust_equivalence_multiple_deprecated(&ShiftRightStaticU128::<14>, true);
        test_rust_equivalence_multiple_deprecated(&ShiftRightStaticU128::<15>, true);
        test_rust_equivalence_multiple_deprecated(&ShiftRightStaticU128::<16>, true);
        test_rust_equivalence_multiple_deprecated(&ShiftRightStaticU128::<17>, true);
        test_rust_equivalence_multiple_deprecated(&ShiftRightStaticU128::<18>, true);
        test_rust_equivalence_multiple_deprecated(&ShiftRightStaticU128::<19>, true);
        test_rust_equivalence_multiple_deprecated(&ShiftRightStaticU128::<20>, true);
        test_rust_equivalence_multiple_deprecated(&ShiftRightStaticU128::<21>, true);
        test_rust_equivalence_multiple_deprecated(&ShiftRightStaticU128::<22>, true);
        test_rust_equivalence_multiple_deprecated(&ShiftRightStaticU128::<23>, true);
        test_rust_equivalence_multiple_deprecated(&ShiftRightStaticU128::<24>, true);
        test_rust_equivalence_multiple_deprecated(&ShiftRightStaticU128::<25>, true);
        test_rust_equivalence_multiple_deprecated(&ShiftRightStaticU128::<26>, true);
        test_rust_equivalence_multiple_deprecated(&ShiftRightStaticU128::<27>, true);
        test_rust_equivalence_multiple_deprecated(&ShiftRightStaticU128::<28>, true);
        test_rust_equivalence_multiple_deprecated(&ShiftRightStaticU128::<29>, true);
        test_rust_equivalence_multiple_deprecated(&ShiftRightStaticU128::<30>, true);
        test_rust_equivalence_multiple_deprecated(&ShiftRightStaticU128::<31>, true);
        test_rust_equivalence_multiple_deprecated(&ShiftRightStaticU128::<32>, true);
    }

    #[test]
    fn shift_right_simple_test() {
        prop::<1>(1);
        prop::<2>(8);
        prop::<3>(1000);
        prop::<3>(random());
        prop::<4>(random());
        prop::<5>(random());
        prop::<31>(random());
        prop::<32>(random());
    }

    #[test]
    fn shift_right_max_values_test() {
        prop::<0>(u128::MAX);
        prop::<1>(u128::MAX);
        prop::<12>(u128::MAX);
        prop::<24>(u128::MAX);
        prop::<31>(u128::MAX);
        prop::<32>(u128::MAX);
    }

    #[test]
    fn shift_right_zero_test() {
        prop::<0>(0);
        prop::<1>(0);
        prop::<12>(0);
        prop::<24>(0);
        prop::<31>(0);
        prop::<32>(0);
    }

    #[test]
    #[should_panic]
    fn shift_beyond_limit() {
        let mut init_stack = empty_stack();
        init_stack.push(BFieldElement::new(u32::MAX as u64));
        init_stack.push(BFieldElement::new(u32::MAX as u64));
        init_stack.push(BFieldElement::new(u32::MAX as u64));
        init_stack.push(BFieldElement::new(u32::MAX as u64));
        ShiftRightStaticU128::<33>
            .link_and_run_tasm_from_state_for_test(&mut ExecutionState::with_stack(init_stack));
    }

    fn prop<const N: u8>(value: u128) {
        let mut init_stack = empty_stack();
        for i in 0..4 {
            init_stack.push(BFieldElement::new(
                ((value >> (32 * (3 - i))) as u32) as u64,
            ));
        }

        let expected_u128 = value >> N;
        println!("{value} >> {N} = {expected_u128}");

        let mut expected_stack = empty_stack();
        for i in 0..4 {
            expected_stack.push(BFieldElement::new(
                ((expected_u128 >> (32 * (3 - i))) & u32::MAX as u128) as u64,
            ));
        }

        test_rust_equivalence_given_input_values_deprecated(
            &ShiftRightStaticU128::<N>,
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
    use crate::snippet_bencher::bench_and_write;

    use super::*;

    #[test]
    fn shift_right_u128_benchmark() {
        bench_and_write(ShiftRightStaticU128::<5>);
    }
}
