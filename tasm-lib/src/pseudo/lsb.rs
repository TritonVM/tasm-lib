use std::collections::HashMap;

use twenty_first::shared_math::b_field_element::{BFieldElement, BFIELD_ONE, BFIELD_ZERO};

use crate::snippet::{DataType, DeprecatedSnippet};
use crate::{empty_stack, ExecutionState};

#[derive(Clone, Debug)]
pub struct Lsb;

impl DeprecatedSnippet for Lsb {
    fn input_field_names(&self) -> Vec<String> {
        vec!["a".to_string()]
    }

    fn output_field_names(&self) -> Vec<String> {
        vec!["a % 2".to_string(), "a / 2".to_string()]
    }

    fn input_types(&self) -> Vec<crate::snippet::DataType> {
        vec![DataType::BFE]
    }

    fn output_types(&self) -> Vec<crate::snippet::DataType> {
        vec![DataType::BFE, DataType::BFE]
    }

    fn crash_conditions(&self) -> Vec<String> {
        vec![]
    }

    fn gen_input_states(&self) -> Vec<ExecutionState> {
        let mut even_stack = empty_stack();
        even_stack.push(BFIELD_ZERO);

        let mut odd_stack = empty_stack();
        odd_stack.push(BFIELD_ONE);

        vec![
            ExecutionState::with_stack(even_stack),
            ExecutionState::with_stack(odd_stack),
        ]
    }

    fn stack_diff(&self) -> isize {
        1
    }

    fn entrypoint_name(&self) -> String {
        "tasm_pseudo_lsb".to_string()
    }

    fn function_code(&self, _library: &mut crate::library::Library) -> String {
        let entrypoint = self.entrypoint_name();
        format!(
            "
            {entrypoint}:
                push 2  // _ a 2
                swap 1   // _ 2 a
                div     // _ a/2 a%2
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
        let a = stack.pop().unwrap().value();
        stack.push(BFieldElement::new(a / 2));
        stack.push(BFieldElement::new(a % 2));
    }

    fn common_case_input_state(&self) -> ExecutionState {
        ExecutionState::with_stack([empty_stack(), vec![BFieldElement::new(1u64 << 20)]].concat())
    }

    fn worst_case_input_state(&self) -> ExecutionState {
        ExecutionState::with_stack([empty_stack(), vec![BFieldElement::new(1u64 << 31)]].concat())
    }
}

#[cfg(test)]
mod tests {

    use crate::test_helpers::test_rust_equivalence_multiple_deprecated;

    use super::*;

    #[test]
    fn lsb_test() {
        test_rust_equivalence_multiple_deprecated(&Lsb, true);
    }
}

#[cfg(test)]
mod benches {
    use super::*;
    use crate::snippet_bencher::bench_and_write;

    #[test]
    fn lsb_benchmark() {
        bench_and_write(Lsb);
    }
}
