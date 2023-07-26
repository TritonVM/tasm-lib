use rand::{thread_rng, Rng};
use twenty_first::shared_math::b_field_element::BFieldElement;

use crate::{
    get_init_tvm_stack,
    snippet::{DataType, Snippet},
    ExecutionState,
};

#[derive(Clone, Debug)]
pub struct SafeAdd;

impl Snippet for SafeAdd {
    fn entrypoint(&self) -> String {
        "tasm_arithmetic_u32_safe_add_u32".to_string()
    }

    fn inputs(&self) -> Vec<String> {
        vec!["rhs".to_string(), "lhs".to_string()]
    }

    fn outputs(&self) -> Vec<String> {
        vec!["lhs + rhs".to_string()]
    }

    fn input_types(&self) -> Vec<crate::snippet::DataType> {
        vec![DataType::U32, DataType::U32]
    }

    fn output_types(&self) -> Vec<crate::snippet::DataType> {
        vec![DataType::U32]
    }

    fn stack_diff(&self) -> isize {
        -1
    }

    fn function_code(&self, _library: &mut crate::library::Library) -> String {
        let entrypoint = self.entrypoint();
        format!(
            "
                {entrypoint}:
                    add // _    lhs + rhs
                    dup 0 // _   (lhs + rhs) (lhs + rhs)
                    split // _  (lhs + rhs) hi lo
                    pop   // _  (lhs + rhs) hi
                    push 0 // _ (lhs + rhs) hi 0
                    eq     // _ (lhs + rhs) (hi == 0)
                    assert // _ (lhs + rhs)
                    return
                    "
        )
    }

    fn crash_conditions(&self) -> Vec<String> {
        vec!["u32 overflow".to_string()]
    }

    fn gen_input_states(&self) -> Vec<crate::ExecutionState> {
        let mut ret: Vec<ExecutionState> = vec![];
        for _ in 0..10 {
            let mut stack = get_init_tvm_stack();
            let lhs = thread_rng().gen_range(0..u32::MAX / 2);
            let rhs = thread_rng().gen_range(0..u32::MAX / 2);
            let lhs = BFieldElement::new(lhs as u64);
            let rhs = BFieldElement::new(rhs as u64);
            stack.push(rhs);
            stack.push(lhs);
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
        let lhs: u32 = stack.pop().unwrap().try_into().unwrap();
        let rhs: u32 = stack.pop().unwrap().try_into().unwrap();

        let sum = lhs + rhs;
        stack.push(BFieldElement::new(sum as u64));
    }

    fn common_case_input_state(&self) -> ExecutionState {
        ExecutionState::with_stack(
            vec![
                get_init_tvm_stack(),
                vec![BFieldElement::new(1 << 16), BFieldElement::new(1 << 15)],
            ]
            .concat(),
        )
    }

    fn worst_case_input_state(&self) -> ExecutionState {
        ExecutionState::with_stack(
            vec![
                get_init_tvm_stack(),
                vec![
                    BFieldElement::new((1 << 30) - 1),
                    BFieldElement::new((1 << 31) - 1),
                ],
            ]
            .concat(),
        )
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use num::Zero;

    use crate::test_helpers::{
        test_rust_equivalence_given_input_values, test_rust_equivalence_multiple,
    };

    use super::*;

    #[test]
    fn snippet_test() {
        test_rust_equivalence_multiple(&SafeAdd, true);
    }

    #[test]
    fn safe_add_simple_test() {
        prop_safe_add(1000, 1, Some(1001));
        prop_safe_add(10_000, 900, Some(10_900));
    }

    #[should_panic]
    #[test]
    fn overflow_test() {
        prop_safe_add((1 << 31) + 1000, 1 << 31, None);
    }

    fn prop_safe_add(lhs: u32, rhs: u32, _expected: Option<u32>) {
        let mut init_stack = get_init_tvm_stack();
        init_stack.push(BFieldElement::new(rhs as u64));
        init_stack.push(BFieldElement::new(lhs as u64));

        let expected = lhs.checked_add(rhs);
        let expected = vec![
            get_init_tvm_stack(),
            vec![expected
                .map(|x| BFieldElement::new(x as u64))
                .unwrap_or_else(BFieldElement::zero)],
        ]
        .concat();

        test_rust_equivalence_given_input_values::<SafeAdd>(
            &SafeAdd,
            &init_stack,
            &[],
            &[],
            &mut HashMap::default(),
            0,
            Some(&expected),
        );
    }
}

#[cfg(test)]
mod benches {
    use super::*;
    use crate::snippet_bencher::bench_and_write;

    #[test]
    fn safe_add_benchmark() {
        bench_and_write(SafeAdd);
    }
}