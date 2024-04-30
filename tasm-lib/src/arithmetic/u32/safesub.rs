use num::Zero;
use rand::thread_rng;
use rand::Rng;
use triton_vm::prelude::*;

use crate::data_type::DataType;
use crate::empty_stack;
use crate::traits::deprecated_snippet::DeprecatedSnippet;
use crate::InitVmState;

#[derive(Clone, Debug)]
pub struct Safesub;

impl DeprecatedSnippet for Safesub {
    fn entrypoint_name(&self) -> String {
        "tasmlib_arithmetic_u32_safesub".to_string()
    }

    fn input_field_names(&self) -> Vec<String> {
        vec!["rhs".to_string(), "lhs".to_string()]
    }

    fn input_types(&self) -> Vec<DataType> {
        vec![DataType::U32, DataType::U32]
    }

    fn output_field_names(&self) -> Vec<String> {
        vec!["lhs - rhs".to_string()]
    }

    fn output_types(&self) -> Vec<DataType> {
        vec![DataType::U32]
    }

    fn stack_diff(&self) -> isize {
        -1
    }

    fn function_code(&self, _library: &mut crate::library::Library) -> String {
        let entrypoint = self.entrypoint_name();
        format!(
            "
                // BEFORE: _ rhs lhs
                // AFTER:  _ (lhs - rhs)
                {entrypoint}:
                    swap 1
                    push -1
                    mul
                    add
                    dup 0  // _ (lhs - rhs) (lhs - rhs)
                    split  // _ (lhs - rhs) hi lo
                    pop 1  // _ (lhs - rhs) hi
                    push 0 // _ (lhs - rhs) hi 0
                    eq     // _ (lhs - rhs) (hi == 0)
                    assert // _ (lhs - rhs)
                    return
                    "
        )
    }

    fn crash_conditions(&self) -> Vec<String> {
        vec!["u32 overflow".to_string()]
    }

    fn gen_input_states(&self) -> Vec<InitVmState> {
        let mut ret: Vec<InitVmState> = vec![];
        for _ in 0..10 {
            let mut stack = empty_stack();
            let lhs = thread_rng().gen_range(0..u32::MAX / 2);
            let rhs = thread_rng().gen_range(0..=lhs);
            let lhs = BFieldElement::new(lhs as u64);
            let rhs = BFieldElement::new(rhs as u64);
            stack.push(rhs);
            stack.push(lhs);
            ret.push(InitVmState::with_stack(stack));
        }

        ret
    }

    fn common_case_input_state(&self) -> InitVmState {
        InitVmState::with_stack(
            [
                empty_stack(),
                vec![BFieldElement::new(1 << 15), BFieldElement::new(1 << 16)],
            ]
            .concat(),
        )
    }

    fn worst_case_input_state(&self) -> InitVmState {
        InitVmState::with_stack(
            [
                empty_stack(),
                vec![BFieldElement::zero(), BFieldElement::new((1 << 32) - 1)],
            ]
            .concat(),
        )
    }

    fn rust_shadowing(
        &self,
        stack: &mut Vec<BFieldElement>,
        _std_in: Vec<BFieldElement>,
        _secret_in: Vec<BFieldElement>,
        _memory: &mut std::collections::HashMap<BFieldElement, BFieldElement>,
    ) {
        let lhs: u32 = stack.pop().unwrap().try_into().unwrap();
        let rhs: u32 = stack.pop().unwrap().try_into().unwrap();

        let diff = lhs - rhs;
        stack.push(BFieldElement::new(diff as u64));
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use crate::test_helpers::test_rust_equivalence_given_input_values_deprecated;
    use crate::test_helpers::test_rust_equivalence_multiple_deprecated;

    use super::*;

    #[test]
    fn snippet_test() {
        test_rust_equivalence_multiple_deprecated(&Safesub, true);
    }

    #[test]
    fn safe_sub_simple_test() {
        prop_safe_sub(1000, 1, Some(999));
        prop_safe_sub(10_000, 900, Some(9_100));
        prop_safe_sub(123, 123, Some(0));
        prop_safe_sub(1230, 230, Some(1000));
        prop_safe_sub(1 << 31, 1 << 30, Some(1 << 30));
        prop_safe_sub(u32::MAX, 0, Some(u32::MAX));
        prop_safe_sub(u32::MAX, u32::MAX, Some(0));
    }

    #[should_panic]
    #[test]
    fn overflow_test() {
        prop_safe_sub(1 << 31, (1 << 31) + 1000, None);
    }

    #[should_panic]
    #[test]
    fn overflow_test_2() {
        prop_safe_sub(0, 1, None);
    }

    #[should_panic]
    #[test]
    fn overflow_test_3() {
        prop_safe_sub(0, u32::MAX, None);
    }

    fn prop_safe_sub(lhs: u32, rhs: u32, _expected: Option<u32>) {
        let mut init_stack = empty_stack();
        init_stack.push(BFieldElement::new(rhs as u64));
        init_stack.push(BFieldElement::new(lhs as u64));
        let expected = lhs.checked_sub(rhs);
        let expected = [
            empty_stack(),
            vec![expected
                .map(|x| BFieldElement::new(x as u64))
                .unwrap_or_else(BFieldElement::zero)],
        ]
        .concat();

        test_rust_equivalence_given_input_values_deprecated(
            &Safesub,
            &init_stack,
            &[],
            HashMap::default(),
            Some(&expected),
        );
    }
}

#[cfg(test)]
mod benches {
    use crate::snippet_bencher::bench_and_write;

    use super::*;

    #[test]
    fn safe_sub_benchmark() {
        bench_and_write(Safesub);
    }
}
