use twenty_first::shared_math::b_field_element::BFieldElement;

use crate::{execute, ExecutionResult};

pub trait Snippet {
    const STACK_DIFF: isize;
    fn get_name() -> String;
    fn get_code() -> String;
    fn as_function() -> String {
        [Self::get_name(), Self::get_code(), "return".to_string()].join("\n")
    }

    // The rust shadowing and the run tasm function must take the same argument
    // since this makes it possible to auto-generate tests for these two functions
    // for any snippet of code since these two functions must mutate the stack in
    // the same manner.
    fn rust_shadowing(
        stack: &mut Vec<BFieldElement>,
        std_in: Vec<BFieldElement>,
        secret_in: Vec<BFieldElement>,
    );
    fn run_tasm(
        stack: &mut Vec<BFieldElement>,
        std_in: Vec<BFieldElement>,
        secret_in: Vec<BFieldElement>,
    ) -> ExecutionResult {
        execute(
            &Self::get_code(),
            stack,
            Self::STACK_DIFF,
            std_in,
            secret_in,
        )
    }
}
