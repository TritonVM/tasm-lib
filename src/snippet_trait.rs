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

    // TODO: Consider adding dependencies so that inclusion can happen exactly once.
    // Dependencies can be listed as a vector of Strings containing names of snippets.

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
    ) -> ExecutionResult {
        let mut library = Library::empty();
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

#[allow(dead_code)]
pub fn rust_tasm_equivalence_prop<T: Snippet>(
    stack: &[BFieldElement],
    stdin: &[BFieldElement],
    secret_in: &[BFieldElement],
    memory: &mut HashMap<BFieldElement, BFieldElement>,
    expected: Option<&[BFieldElement]>,
) -> ExecutionResult {
    let init_memory = memory.clone();
    let mut tasm_stack = stack.to_vec();
    let mut tasm_memory = init_memory.clone();
    let execution_result = T::run_tasm(
        &mut tasm_stack,
        stdin.to_vec(),
        secret_in.to_vec(),
        &mut tasm_memory,
    );
    println!(
        "Cycle count for `{}`: {}",
        T::entrypoint(),
        execution_result.cycle_count
    );
    println!(
        "Hash table height for `{}`: {}",
        T::entrypoint(),
        execution_result.hash_table_height
    );

    let mut rust_memory = init_memory;
    let mut rust_stack = stack.to_vec();
    T::rust_shadowing(
        &mut rust_stack,
        stdin.to_vec(),
        secret_in.to_vec(),
        &mut rust_memory,
    );

    assert_eq!(
        tasm_stack,
        rust_stack,
        "Rust code must match TVM for `{}`",
        T::entrypoint(),
    );
    if let Some(expected) = expected {
        assert_eq!(tasm_stack, expected, "TVM must produce expected stack.");
    }

    assert_eq!(
        rust_memory, tasm_memory,
        "Memory for both implementations must match after execution"
    );

    // Write back memory to be able to probe it in individual tests
    *memory = tasm_memory;

    execution_result
}
