use std::collections::HashMap;

use twenty_first::shared_math::b_field_element::{BFieldElement, BFIELD_ONE, BFIELD_ZERO};

use crate::snippet::Snippet;
use crate::{get_init_tvm_stack, ExecutionState};

pub struct Lsb();

impl Snippet for Lsb {
    fn inputs() -> Vec<&'static str> {
        vec!["a"]
    }

    fn outputs() -> Vec<&'static str> {
        vec!["a % 2", "a / 2"]
    }

    fn crash_conditions() -> Vec<&'static str> {
        vec![]
    }

    fn gen_input_states() -> Vec<ExecutionState> {
        let mut even_stack = get_init_tvm_stack();
        even_stack.push(BFIELD_ZERO);

        let mut odd_stack = get_init_tvm_stack();
        odd_stack.push(BFIELD_ONE);

        vec![
            ExecutionState::with_stack(even_stack),
            ExecutionState::with_stack(odd_stack),
        ]
    }

    fn stack_diff() -> isize {
        1
    }

    fn entrypoint() -> &'static str {
        "lsb"
    }

    fn function_body(_library: &mut crate::library::Library) -> String {
        let entrypoint = Self::entrypoint();
        format!(
            "
            {entrypoint}:
                push 2  // _ a 2
                swap1   // _ 2 a
                div     // _ a/2 a%2
                return
            "
        )
    }

    fn rust_shadowing(
        stack: &mut Vec<BFieldElement>,
        _std_in: Vec<BFieldElement>,
        _secret_in: Vec<BFieldElement>,
        _memory: &mut HashMap<BFieldElement, BFieldElement>,
    ) {
        let a = stack.pop().unwrap().value();
        stack.push(BFieldElement::new(a / 2));
        stack.push(BFieldElement::new(a % 2));
    }
}
