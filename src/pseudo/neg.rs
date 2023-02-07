use std::collections::HashMap;

use rand::Rng;
use twenty_first::shared_math::b_field_element::BFieldElement;

use crate::snippet::{DataType, Snippet};
use crate::{get_init_tvm_stack, ExecutionState};

#[derive(Clone)]
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

    fn crash_conditions() -> Vec<String> {
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

    fn function_body(&self, _library: &mut crate::library::Library) -> String {
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
}
