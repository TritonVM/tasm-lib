use std::collections::HashMap;
use twenty_first::shared_math::b_field_element::BFieldElement;

use crate::execute;
use crate::library::Library;
use crate::ExecutionResult;

pub trait Snippet {
    fn stack_diff() -> isize;

    fn entrypoint() -> &'static str;

    fn function_body(library: &mut Library) -> String;

    // TODO: Consider adding a generator for valid inputs to this trait
    // This input generator could then function as a form of documentation.

    // TODO: Consider adding these associated constants
    // const INIT_STACK_DESC: &'static str;
    // const FINAL_STACK_DESC: &'static str;

    // The rust shadowing and the run tasm function must take the same argument
    // since this makes it possible to auto-generate tests for these two functions
    // for any snippet of code since these two functions must mutate the stack in
    // the same manner.
    fn rust_shadowing(
        stack: &mut Vec<BFieldElement>,
        std_in: Vec<BFieldElement>,
        secret_in: Vec<BFieldElement>,
        memory: &mut HashMap<BFieldElement, BFieldElement>,
    );

    /// The TASM code is always run through a function call, so the 1st instruction
    /// is a call to the function in question.
    fn run_tasm(
        stack: &mut Vec<BFieldElement>,
        std_in: Vec<BFieldElement>,
        secret_in: Vec<BFieldElement>,
        memory: &mut HashMap<BFieldElement, BFieldElement>,
        words_allocated: usize,
    ) -> ExecutionResult {
        let mut library = Library::with_preallocated_memory(words_allocated);
        let entrypoint = Self::entrypoint();
        let function_body = Self::function_body(&mut library);
        let library_code = library.all_imports();

        let code = format!(
            "
            call {entrypoint}
            halt

            {function_body}
            {library_code}
            "
        );
        execute(&code, stack, Self::stack_diff(), std_in, secret_in, memory)
    }
}
