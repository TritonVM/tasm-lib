use std::collections::HashMap;

use rand::Rng;
use twenty_first::shared_math::b_field_element::BFieldElement;

use crate::data_type::DataType;
use crate::snippet::DeprecatedSnippet;
use crate::{empty_stack, ExecutionState};

#[derive(Clone, Debug)]
pub struct Neg;

impl DeprecatedSnippet for Neg {
    fn input_field_names(&self) -> Vec<String> {
        vec!["value".to_string()]
    }

    fn output_field_names(&self) -> Vec<String> {
        vec!["-value".to_string()]
    }

    fn input_types(&self) -> Vec<crate::data_type::DataType> {
        vec![DataType::Bfe]
    }

    fn output_types(&self) -> Vec<crate::data_type::DataType> {
        vec![DataType::Bfe]
    }

    fn crash_conditions(&self) -> Vec<String> {
        vec![]
    }

    fn gen_input_states(&self) -> Vec<ExecutionState> {
        let mut rng = rand::thread_rng();
        let mut stack = empty_stack();
        stack.push(rng.gen());
        vec![ExecutionState::with_stack(stack)]
    }

    fn stack_diff(&self) -> isize {
        0
    }

    fn entrypoint_name(&self) -> String {
        "tasm_pseudo_neg".to_string()
    }

    fn function_code(&self, _library: &mut crate::library::Library) -> String {
        let entrypoint = self.entrypoint_name();

        format!(
            "
            {entrypoint}:
                push -1
                mul
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
        let elem = stack.pop().unwrap();
        stack.push(-elem);
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
    use super::*;

    use crate::test_helpers::test_rust_equivalence_multiple_deprecated;

    #[test]
    fn lsb_test() {
        test_rust_equivalence_multiple_deprecated(&Neg, true);
    }
}

#[cfg(test)]
mod benches {
    use super::*;
    use crate::snippet_bencher::bench_and_write;

    #[test]
    fn lsb_benchmark() {
        bench_and_write(Neg);
    }
}
