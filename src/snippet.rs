use std::collections::HashMap;
use triton_opcodes::program::Program;
use triton_vm::op_stack::OP_STACK_REG_COUNT;
use triton_vm::vm::{self, AlgebraicExecutionTrace};
use twenty_first::shared_math::b_field_element::BFieldElement;

use crate::library::Library;
use crate::ExecutionResult;
use crate::{execute, ExecutionState};

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

pub trait NewSnippet: Snippet {
    fn inputs() -> Vec<&'static str>;
    fn outputs() -> Vec<&'static str>;
    fn crash_conditions() -> Vec<&'static str>;
    fn gen_input_states() -> Vec<ExecutionState>;

    fn run_tasm(execution_state: &mut ExecutionState) -> ExecutionResult {
        // TODO: Consider adding canaries here to ensure that stack is not modified below where the function

        let stack_prior = execution_state.stack.clone();
        let ret = <Self as Snippet>::run_tasm(
            &mut execution_state.stack,
            execution_state.std_in.clone(),
            execution_state.secret_in.clone(),
            &mut execution_state.memory,
            execution_state.words_allocated,
        );
        let stack_after = execution_state.stack.clone();

        assert_eq!(
            stack_prior[0..(stack_prior.len() - Self::inputs().len())],
            stack_after[0..(stack_after.len() - Self::outputs().len())]
        );

        ret
    }
}

#[allow(dead_code)]
pub fn compile_snippet<T: Snippet>() -> String {
    let mut library = Library::empty();
    let main_entrypoint = T::entrypoint();
    let main_function_body = T::function_body(&mut library);
    let library_code = library.all_imports();

    format!(
        "
        call {main_entrypoint}
        halt

        {main_function_body}
        {library_code}
        "
    )
}

#[allow(dead_code)]
pub fn simulate_snippet<T: Snippet>(
    execution_state: ExecutionState,
) -> (AlgebraicExecutionTrace, usize) {
    let mut code: Vec<String> = vec![];
    let mut inflated_clock_cycles: usize = 0;

    // Prepend the snippet's code with code that injects expected stack
    for element in execution_state.stack.iter().skip(OP_STACK_REG_COUNT) {
        code.push(format!("push {}\n", element.value()));
        inflated_clock_cycles += 1;
    }

    // Prepend the snippet's code with code that injects expected memory
    for (address, value) in execution_state.memory.iter() {
        code.push(format!("push {address} push {value} write_mem pop pop\n"));
        inflated_clock_cycles += 5;
    }

    // Compile the snippet and its library dependencies
    code.push(compile_snippet::<T>());

    // Parse and run the program, bootloader and library
    let code: String = code.concat();
    let program = Program::from_code(&code).unwrap();
    let std_in = execution_state.std_in;
    let secret_in = execution_state.secret_in;
    let (aet, _out, err) = vm::simulate(&program, std_in, secret_in);
    if let Some(err) = err {
        panic!("Program:\n{code}\nFailed:\n{err}");
    }

    (aet, inflated_clock_cycles)
}
