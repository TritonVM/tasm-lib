use triton_vm::op_stack::OP_STACK_REG_COUNT;
use twenty_first::shared_math::b_field_element::BFieldElement;

use crate::{execute, ExecutionResult};

pub trait Snippet {
    const STACK_DIFF: isize;
    fn get_name() -> String;
    fn get_code() -> String;
    fn as_function() -> String {
        [Self::get_name(), Self::get_code(), "return".to_string()].join("\n")
    }
    fn rust_shadowing(stack: &mut Vec<BFieldElement>);
    fn run_tasm(
        stack: &mut Vec<BFieldElement>,
        expected_stack_diff: isize,
        std_in: Vec<BFieldElement>,
        secret_in: Vec<BFieldElement>,
    ) -> ExecutionResult {
        execute(
            &Self::get_code(),
            stack,
            // TODO: This should probably be changed to the stack diff value
            (OP_STACK_REG_COUNT as isize + expected_stack_diff) as usize,
            std_in,
            secret_in,
        )
    }
}
