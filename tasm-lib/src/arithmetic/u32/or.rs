use rand::prelude::*;
use triton_vm::prelude::*;

use crate::empty_stack;
use crate::prelude::*;
use crate::traits::deprecated_snippet::DeprecatedSnippet;
use crate::InitVmState;

#[derive(Clone, Debug)]
pub struct Or;

impl DeprecatedSnippet for Or {
    fn entrypoint_name(&self) -> String {
        "tasmlib_arithmetic_u32_or".to_string()
    }

    fn input_field_names(&self) -> Vec<String> {
        vec!["rhs".to_string(), "lhs".to_string()]
    }

    fn input_types(&self) -> Vec<DataType> {
        vec![DataType::U32, DataType::U32]
    }

    fn output_field_names(&self) -> Vec<String> {
        vec!["lhs | rhs".to_string()]
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
                // AFTER:  _ (lhs | rhs)
                {entrypoint}:
                    dup 1
                    dup 1
                    xor
                    // _ rhs lhs (rhs ^ lhs)

                    swap 2
                    and
                    // _ (rhs ^ lhs) (lhs & rhs)

                    add

                    return
                    "
        )
    }

    fn crash_conditions(&self) -> Vec<String> {
        vec!["Inputs are not u32".to_owned()]
    }

    fn gen_input_states(&self) -> Vec<InitVmState> {
        let mut ret: Vec<InitVmState> = vec![];
        for _ in 0..100 {
            let mut stack = empty_stack();
            let lhs = thread_rng().next_u32();
            let rhs = thread_rng().next_u32();
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
                vec![
                    BFieldElement::new((1 << 32) - 1),
                    BFieldElement::new((1 << 32) - 1),
                ],
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

        let or = lhs | rhs;
        stack.push(BFieldElement::new(or as u64));
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use super::*;
    use crate::test_helpers::test_rust_equivalence_given_input_values_deprecated;
    use crate::test_helpers::test_rust_equivalence_multiple_deprecated;

    #[test]
    fn snippet_test() {
        test_rust_equivalence_multiple_deprecated(&Or, true);
    }

    #[test]
    fn or_simple_test() {
        prop_safe_or(10, 1, Some(11));
        prop_safe_or(256, 30, Some(286));
        prop_safe_or(123, 0, Some(123));
        prop_safe_or(0, 123, Some(123));
        prop_safe_or(1 << 31, 1 << 30, Some((1 << 30) + (1 << 31)));
        prop_safe_or(0, 0, Some(0));
        prop_safe_or(14, 0, Some(14));
        prop_safe_or(u32::MAX, 0, Some(u32::MAX));
        prop_safe_or(0, u32::MAX, Some(u32::MAX));
        prop_safe_or(u32::MAX, u32::MAX, Some(u32::MAX));
    }

    fn prop_safe_or(lhs: u32, rhs: u32, _expected: Option<u32>) {
        let mut init_stack = empty_stack();
        init_stack.push(BFieldElement::new(rhs as u64));
        init_stack.push(BFieldElement::new(lhs as u64));

        let expected = lhs | rhs;
        let expected = [empty_stack(), vec![BFieldElement::new(expected as u64)]].concat();

        test_rust_equivalence_given_input_values_deprecated(
            &Or,
            &init_stack,
            &[],
            HashMap::default(),
            Some(&expected),
        );
    }
}

#[cfg(test)]
mod benches {
    use super::*;
    use crate::snippet_bencher::bench_and_write;

    #[test]
    fn u32_or_benchmark() {
        bench_and_write(Or);
    }
}
