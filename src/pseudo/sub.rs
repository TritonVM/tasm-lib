use std::collections::HashMap;

use rand::Rng;
use twenty_first::shared_math::b_field_element::BFieldElement;

use crate::snippet::{DataType, Snippet};
use crate::{get_init_tvm_stack, ExecutionState};

#[derive(Clone)]
pub struct Sub();

impl Snippet for Sub {
    fn inputs() -> Vec<&'static str> {
        vec!["b", "a"]
    }

    fn outputs() -> Vec<&'static str> {
        vec!["a - b"]
    }

    fn input_types(&self) -> Vec<crate::snippet::DataType> {
        vec![DataType::BFE, DataType::BFE]
    }

    fn output_types(&self) -> Vec<crate::snippet::DataType> {
        vec![DataType::BFE]
    }

    fn crash_conditions() -> Vec<&'static str> {
        vec![]
    }

    fn gen_input_states() -> Vec<ExecutionState> {
        let mut rng = rand::thread_rng();
        let mut stack = get_init_tvm_stack();
        stack.push(rng.gen());
        stack.push(rng.gen());
        vec![ExecutionState::with_stack(stack)]
    }

    fn stack_diff() -> isize {
        -1
    }

    fn entrypoint() -> &'static str {
        "sub"
    }

    fn function_body(_library: &mut crate::library::Library) -> String {
        let entrypoint = Self::entrypoint();
        format!(
            "
            // before: _ b a
            // after: _ a-b
            {entrypoint}:
                swap1    // _ a b
                push -1  // _ a b -1
                mul      // _ a -b
                add      // _ a-b
            "
        )
    }

    fn rust_shadowing(
        stack: &mut Vec<BFieldElement>,
        _std_in: Vec<BFieldElement>,
        _secret_in: Vec<BFieldElement>,
        _memory: &mut HashMap<BFieldElement, BFieldElement>,
    ) {
        let a = stack.pop().unwrap();
        let b = stack.pop().unwrap();
        stack.push(a - b);
    }
}
