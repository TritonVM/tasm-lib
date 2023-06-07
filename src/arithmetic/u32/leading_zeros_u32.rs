use rand::{thread_rng, RngCore};
use twenty_first::shared_math::b_field_element::BFieldElement;

use crate::{
    get_init_tvm_stack,
    snippet::{DataType, Snippet},
    ExecutionState,
};

#[derive(Clone, Debug)]
pub struct LeadingZerosU32;

impl Snippet for LeadingZerosU32 {
    fn entrypoint(&self) -> String {
        "tasm_arithmetic_u32_leading_zeros_u32".to_string()
    }

    fn inputs(&self) -> Vec<String> {
        vec!["value".to_string()]
    }

    fn outputs(&self) -> Vec<String> {
        vec!["leading zeros in value".to_string()]
    }

    fn input_types(&self) -> Vec<crate::snippet::DataType> {
        vec![DataType::U32]
    }

    fn output_types(&self) -> Vec<crate::snippet::DataType> {
        vec![DataType::U32]
    }

    fn stack_diff(&self) -> isize {
        0
    }

    fn function_code(&self, _library: &mut crate::snippet_state::SnippetState) -> String {
        let entrypoint = self.entrypoint();
        format!(
            "
                // BEFORE: _ value
                // AFTER: _ (leading zeros in value)
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

    fn gen_input_states(&self) -> Vec<crate::ExecutionState> {
        let mut ret: Vec<ExecutionState> = vec![];
        for _ in 0..100 {
            let mut stack = get_init_tvm_stack();
            let value = thread_rng().next_u32();
            let value = BFieldElement::new(value as u64);
            stack.push(value);
            ret.push(ExecutionState::with_stack(stack));
        }

        ret
    }

    fn rust_shadowing(
        &self,
        stack: &mut Vec<twenty_first::shared_math::b_field_element::BFieldElement>,
        _std_in: Vec<twenty_first::shared_math::b_field_element::BFieldElement>,
        _secret_in: Vec<twenty_first::shared_math::b_field_element::BFieldElement>,
        _memory: &mut std::collections::HashMap<
            twenty_first::shared_math::b_field_element::BFieldElement,
            twenty_first::shared_math::b_field_element::BFieldElement,
        >,
    ) {
        let value: u32 = stack.pop().unwrap().try_into().unwrap();

        let value = value.leading_zeros();
        stack.push(BFieldElement::new(value as u64));
    }

    fn common_case_input_state(&self) -> ExecutionState
    where
        Self: Sized,
    {
        ExecutionState::with_stack(
            vec![get_init_tvm_stack(), vec![BFieldElement::new(1 << 15)]].concat(),
        )
    }

    fn worst_case_input_state(&self) -> ExecutionState
    where
        Self: Sized,
    {
        ExecutionState::with_stack(
            vec![
                get_init_tvm_stack(),
                vec![BFieldElement::new((1 << 32) - 1)],
            ]
            .concat(),
        )
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use crate::{
        snippet_bencher::bench_and_write,
        test_helpers::{rust_tasm_equivalence_prop, rust_tasm_equivalence_prop_new},
    };

    use super::*;

    #[test]
    fn snippet_test() {
        rust_tasm_equivalence_prop_new(&LeadingZerosU32, true);
    }

    #[test]
    fn u32_leading_zeros_benchmark() {
        bench_and_write(LeadingZerosU32);
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

    fn prop_safe_leading_zeros(value: u32, expected: Option<u32>) {
        let mut init_stack = get_init_tvm_stack();
        init_stack.push(BFieldElement::new(value as u64));

        let execution_result = rust_tasm_equivalence_prop(
            &LeadingZerosU32,
            &init_stack,
            &[],
            &[],
            &mut HashMap::default(),
            0,
            None,
        );

        let mut final_stack = execution_result.final_stack;
        if let Some(res) = expected {
            assert_eq!(BFieldElement::new(res as u64), final_stack.pop().unwrap());
        };
    }
}
