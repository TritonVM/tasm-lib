use std::collections::HashMap;

use rand::Rng;
use twenty_first::shared_math::b_field_element::BFieldElement;

use crate::snippet::{DataType, Snippet};
use crate::{get_init_tvm_stack, ExecutionState};

#[derive(Clone)]
pub struct Sub;

impl Snippet for Sub {
    fn inputs(&self) -> Vec<String> {
        vec!["b".to_string(), "a".to_string()]
    }

    fn outputs(&self) -> Vec<String> {
        vec!["a - b".to_string()]
    }

    fn input_types(&self) -> Vec<crate::snippet::DataType> {
        vec![DataType::BFE, DataType::BFE]
    }

    fn output_types(&self) -> Vec<crate::snippet::DataType> {
        vec![DataType::BFE]
    }

    fn crash_conditions() -> Vec<String> {
        vec![]
    }

    fn gen_input_states(&self) -> Vec<ExecutionState> {
        let mut rng = rand::thread_rng();
        let mut stack = get_init_tvm_stack();
        stack.push(rng.gen());
        stack.push(rng.gen());
        vec![ExecutionState::with_stack(stack)]
    }

    fn stack_diff(&self) -> isize {
        -1
    }

    fn entrypoint(&self) -> String {
        "tasm_pseudo_sub".to_string()
    }

    fn function_body(&self, _library: &mut crate::snippet_state::SnippetState) -> String {
        let entrypoint = self.entrypoint();
        format!(
            "
            // before: _ b a
            // after: _ a-b
            {entrypoint}:
                swap 1    // _ a b
                push -1  // _ a b -1
                mul      // _ a -b
                add      // _ a-b
                return
            "
        )
    }

    fn rust_shadowing(
        &self,
        stack: &mut Vec<BFieldElement>,
        _std_in: Vec<BFieldElement>,
        _secret_in: Vec<BFieldElement>,
        _memory: &mut HashMap<BFieldElement, BFieldElement>,
    ) {
        let a = stack.pop().unwrap();
        let b = stack.pop().unwrap();
        stack.push(a - b);
    }

    fn common_case_input_state(&self) -> ExecutionState
    where
        Self: Sized,
    {
        ExecutionState::with_stack(
            vec![
                get_init_tvm_stack(),
                vec![
                    BFieldElement::new(1u64 << 20),
                    BFieldElement::new(1u64 << 25),
                ],
            ]
            .concat(),
        )
    }

    fn worst_case_input_state(&self) -> ExecutionState
    where
        Self: Sized,
    {
        ExecutionState::with_stack(
            vec![
                get_init_tvm_stack(),
                vec![
                    BFieldElement::new(1u64 << 20),
                    BFieldElement::new(1u64 << 20),
                ],
            ]
            .concat(),
        )
    }
}

#[cfg(test)]
mod tests {
    use crate::snippet_bencher::bench_and_write;
    use crate::test_helpers::rust_tasm_equivalence_prop_new;

    use super::*;

    #[test]
    fn sub_test() {
        rust_tasm_equivalence_prop_new(Sub);
    }

    #[test]
    fn sub_benchmark() {
        bench_and_write(Sub);
    }
}
