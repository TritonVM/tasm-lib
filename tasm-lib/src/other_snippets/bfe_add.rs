use std::collections::HashMap;

use rand::Rng;
use twenty_first::shared_math::b_field_element::BFieldElement;

use crate::library::Library;
use crate::snippet::{DataType, Snippet};
use crate::{get_init_tvm_stack, ExecutionState};

#[derive(Clone, Debug)]
pub struct BfeAdd;

impl Snippet for BfeAdd {
    fn inputs(&self) -> Vec<String> {
        vec!["b".to_string(), "a".to_string()]
    }

    fn outputs(&self) -> Vec<String> {
        vec!["a + b".to_string()]
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
        let mut stack = get_init_tvm_stack();
        stack.push(rng.gen());
        stack.push(rng.gen());
        vec![ExecutionState::with_stack(stack)]
    }

    fn stack_diff(&self) -> isize {
        -1
    }

    fn entrypoint(&self) -> String {
        "tasm_other_bfe_add".to_string()
    }

    fn function_code(&self, _library: &mut Library) -> String {
        let entrypoint = self.entrypoint();
        format!(
            "
            {entrypoint}:
                add
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
        stack.push(a + b);
    }

    fn common_case_input_state(&self) -> ExecutionState {
        ExecutionState::with_stack(
            vec![
                get_init_tvm_stack(),
                vec![BFieldElement::new(10), BFieldElement::new(20)],
            ]
            .concat(),
        )
    }

    fn worst_case_input_state(&self) -> ExecutionState {
        ExecutionState::with_stack(
            vec![
                get_init_tvm_stack(),
                vec![BFieldElement::new(10), BFieldElement::new(20)],
            ]
            .concat(),
        )
    }
}

#[cfg(test)]
mod tests {

    use crate::test_helpers::test_rust_equivalence_multiple;

    use super::*;

    #[test]
    fn bfe_add_test() {
        test_rust_equivalence_multiple(&BfeAdd, true);
    }
}

#[cfg(test)]
mod benches {
    use super::*;
    use crate::snippet_bencher::bench_and_write;

    #[test]
    fn bfe_add_benchmark() {
        bench_and_write(BfeAdd);
    }
}
