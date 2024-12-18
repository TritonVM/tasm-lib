use rand::prelude::*;
use triton_vm::prelude::*;

use crate::empty_stack;
use crate::prelude::*;
use crate::traits::deprecated_snippet::DeprecatedSnippet;
use crate::InitVmState;

#[derive(Clone, Debug)]
pub struct Leadingzeros;

impl DeprecatedSnippet for Leadingzeros {
    fn entrypoint_name(&self) -> String {
        "tasmlib_arithmetic_u32_leadingzeros".to_string()
    }

    fn input_field_names(&self) -> Vec<String> {
        vec!["value".to_string()]
    }

    fn input_types(&self) -> Vec<DataType> {
        vec![DataType::U32]
    }

    fn output_field_names(&self) -> Vec<String> {
        vec!["leading zeros in value".to_string()]
    }

    fn output_types(&self) -> Vec<DataType> {
        vec![DataType::U32]
    }

    fn stack_diff(&self) -> isize {
        0
    }

    fn function_code(&self, _library: &mut crate::library::Library) -> String {
        let entrypoint = self.entrypoint_name();
        format!(
            "
                // BEFORE: _ value
                // AFTER:  _ (leading zeros in value)
                {entrypoint}:
                    dup 0
                    skiz
                      call {entrypoint}_non_zero

                    push -1
                    mul
                    push 32
                    add

                    return

                {entrypoint}_non_zero:
                    log_2_floor
                    push 1
                    add
                    return
                    "
        )
    }

    fn crash_conditions(&self) -> Vec<String> {
        vec!["Input is not u32".to_owned()]
    }

    fn gen_input_states(&self) -> Vec<InitVmState> {
        let mut ret: Vec<InitVmState> = vec![];
        for _ in 0..100 {
            let mut stack = empty_stack();
            let value = thread_rng().next_u32();
            let value = BFieldElement::new(value as u64);
            stack.push(value);
            ret.push(InitVmState::with_stack(stack));
        }

        ret
    }

    fn common_case_input_state(&self) -> InitVmState {
        InitVmState::with_stack([empty_stack(), vec![BFieldElement::new(1 << 15)]].concat())
    }

    fn worst_case_input_state(&self) -> InitVmState {
        InitVmState::with_stack([empty_stack(), vec![BFieldElement::new((1 << 32) - 1)]].concat())
    }

    fn rust_shadowing(
        &self,
        stack: &mut Vec<BFieldElement>,
        _std_in: Vec<BFieldElement>,
        _secret_in: Vec<BFieldElement>,
        _memory: &mut std::collections::HashMap<BFieldElement, BFieldElement>,
    ) {
        let value: u32 = stack.pop().unwrap().try_into().unwrap();

        let value = value.leading_zeros();
        stack.push(BFieldElement::new(value as u64));
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
        test_rust_equivalence_multiple_deprecated(&Leadingzeros, true);
    }

    #[test]
    fn leading_zeros_u32_simple_test() {
        prop_safe_leading_zeros(1, Some(31));
        prop_safe_leading_zeros(2, Some(30));
        prop_safe_leading_zeros(3, Some(30));
        prop_safe_leading_zeros(4, Some(29));
        prop_safe_leading_zeros(256, Some(23));
        prop_safe_leading_zeros(123, Some(25));
        prop_safe_leading_zeros(0, Some(32));
        prop_safe_leading_zeros(1 << 31, Some(0));
        prop_safe_leading_zeros(1 << 30, Some(1));
        prop_safe_leading_zeros(1 << 29, Some(2));
        prop_safe_leading_zeros(1 << 28, Some(3));
        prop_safe_leading_zeros(u32::MAX, Some(0));
    }

    fn prop_safe_leading_zeros(value: u32, _expected: Option<u32>) {
        let mut init_stack = empty_stack();
        init_stack.push(BFieldElement::new(value as u64));

        let expected = value.leading_zeros();
        let expected = [empty_stack(), vec![BFieldElement::new(expected as u64)]].concat();

        test_rust_equivalence_given_input_values_deprecated(
            &Leadingzeros,
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
    fn u32_leading_zeros_benchmark() {
        bench_and_write(Leadingzeros);
    }
}
