use std::collections::HashMap;

use rand::Rng;
use twenty_first::shared_math::b_field_element::BFieldElement;

use crate::snippet::{DataType, Snippet};
use crate::{get_init_tvm_stack, ExecutionState};

#[derive(Clone, Debug)]
pub struct Neg;

impl Snippet for Neg {
    fn inputs(&self) -> Vec<String> {
        vec!["value".to_string()]
    }

    fn outputs(&self) -> Vec<String> {
        vec!["-value".to_string()]
    }

    fn input_types(&self) -> Vec<crate::snippet::DataType> {
        vec![DataType::BFE]
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
        vec![ExecutionState::with_stack(stack)]
    }

    fn stack_diff(&self) -> isize {
        0
    }

    fn entrypoint(&self) -> String {
        "tasm_pseudo_neg".to_string()
    }

    fn function_code(&self, _library: &mut crate::library::Library) -> String {
        let entrypoint = self.entrypoint();

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
        ExecutionState::with_stack(
            vec![get_init_tvm_stack(), vec![BFieldElement::new(1u64 << 20)]].concat(),
        )
    }

    fn worst_case_input_state(&self) -> ExecutionState {
        ExecutionState::with_stack(
            vec![get_init_tvm_stack(), vec![BFieldElement::new(1u64 << 31)]].concat(),
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use crate::test_helpers::test_rust_equivalence_multiple;

    #[test]
    fn lsb_test() {
        test_rust_equivalence_multiple(&Neg, true);
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
