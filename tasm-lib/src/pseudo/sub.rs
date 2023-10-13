use std::collections::HashMap;

use rand::Rng;
use twenty_first::shared_math::b_field_element::BFieldElement;

use crate::snippet::{DataType, DeprecatedSnippet};
use crate::{empty_stack, ExecutionState};

#[derive(Clone, Debug)]
pub struct Sub;

impl DeprecatedSnippet for Sub {
    fn input_field_names(&self) -> Vec<String> {
        vec!["b".to_string(), "a".to_string()]
    }

    fn output_field_names(&self) -> Vec<String> {
        vec!["a - b".to_string()]
    }

    fn input_types(&self) -> Vec<crate::snippet::DataType> {
        vec![DataType::BFE, DataType::BFE]
    }

    fn output_types(&self) -> Vec<crate::snippet::DataType> {
        vec![DataType::BFE]
    }

    fn crash_conditions(&self) -> Vec<String> {
        vec![]
    }

    fn gen_input_states(&self) -> Vec<ExecutionState> {
        let mut rng = rand::thread_rng();
        let mut stack = empty_stack();
        stack.push(rng.gen());
        stack.push(rng.gen());
        vec![ExecutionState::with_stack(stack)]
    }

    fn stack_diff(&self) -> isize {
        -1
    }

    fn entrypoint_name(&self) -> String {
        "tasm_pseudo_sub".to_string()
    }

    fn function_code(&self, _library: &mut crate::library::Library) -> String {
        let entrypoint = self.entrypoint_name();
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

    fn common_case_input_state(&self) -> ExecutionState {
        ExecutionState::with_stack(
            [
                empty_stack(),
                vec![
                    BFieldElement::new(1u64 << 20),
                    BFieldElement::new(1u64 << 25),
                ],
            ]
            .concat(),
        )
    }

    fn worst_case_input_state(&self) -> ExecutionState {
        ExecutionState::with_stack(
            [
                empty_stack(),
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

    use crate::test_helpers::test_rust_equivalence_multiple_deprecated;

    use super::*;

    #[test]
    fn sub_test() {
        test_rust_equivalence_multiple_deprecated(&Sub, true);
    }
}

#[cfg(test)]
mod benches {
    use super::*;
    use crate::snippet_bencher::bench_and_write;

    #[test]
    fn sub_benchmark() {
        bench_and_write(Sub);
    }
}
