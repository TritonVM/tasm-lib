use std::collections::HashMap;
use triton_opcodes::instruction::{parse, LabelledInstruction};
use triton_opcodes::program::Program;
use triton_vm::op_stack::OP_STACK_REG_COUNT;
use triton_vm::vm::{self, AlgebraicExecutionTrace};
use twenty_first::shared_math::b_field_element::BFieldElement;
use twenty_first::shared_math::rescue_prime_digest::DIGEST_LENGTH;

use crate::library::Library;
use crate::{all_snippets, ExecutionResult};
use crate::{execute, ExecutionState};

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum DataType {
    Bool,
    U32,
    U64,
    BFE,
    XFE,
    Digest,
    List(Box<DataType>),
}

impl DataType {
    pub fn get_size(&self) -> usize {
        match self {
            DataType::Bool => 1,
            DataType::U32 => 1,
            DataType::U64 => 2,
            DataType::BFE => 1,
            DataType::XFE => 3,
            DataType::Digest => DIGEST_LENGTH,
            DataType::List(_) => 1,
        }
    }
}

pub trait Snippet {
    /// The name of a Snippet
    ///
    /// This is used as a unique identifier, e.g. when generating labels.
    fn entrypoint(&self) -> &'static str;

    /// The input stack
    fn inputs() -> Vec<&'static str>
    where
        Self: Sized;

    fn input_types(&self) -> Vec<DataType>;

    fn output_types(&self) -> Vec<DataType>;

    /// The output stack
    fn outputs() -> Vec<&'static str>
    where
        Self: Sized;

    /// The stack difference
    fn stack_diff() -> isize
    where
        Self: Sized;

    /// The function body
    fn function_body(&self, library: &mut Library) -> String;

    /// Ways in which this snippet can crash
    fn crash_conditions() -> Vec<&'static str>
    where
        Self: Sized;

    /// Examples of valid initial states for running this snippet
    fn gen_input_states() -> Vec<ExecutionState>
    where
        Self: Sized;

    fn function_body_as_instructions(
        &self,
        library: &mut Library,
    ) -> Vec<LabelledInstruction<'static>>
    where
        Self: Sized,
    {
        let f_body = self.function_body(library);

        // parse the code to get the list of instructions
        parse(&f_body).unwrap()
    }

    // The rust shadowing and the run tasm function must take the same argument
    // since this makes it possible to auto-generate tests for these two functions
    // for any snippet of code since these two functions must mutate the stack in
    // the same manner.
    fn rust_shadowing(
        stack: &mut Vec<BFieldElement>,
        std_in: Vec<BFieldElement>,
        secret_in: Vec<BFieldElement>,
        memory: &mut HashMap<BFieldElement, BFieldElement>,
    ) where
        Self: Sized;

    /// The TASM code is always run through a function call, so the 1st instruction
    /// is a call to the function in question.
    fn run_tasm_old(
        &self,
        stack: &mut Vec<BFieldElement>,
        std_in: Vec<BFieldElement>,
        secret_in: Vec<BFieldElement>,
        memory: &mut HashMap<BFieldElement, BFieldElement>,
        words_allocated: usize,
    ) -> ExecutionResult
    where
        Self: Sized,
    {
        // Verify that snippet can be found in `all_snippets`, so it's visible to the outside
        // This call will panic if snippet is not found in that function call
        // The data type value is a dummy value for all snippets except those that handle lists.
        all_snippets::name_to_snippet(self.entrypoint(), Some(DataType::Digest));

        let mut library = Library::with_preallocated_memory(words_allocated);
        let entrypoint = self.entrypoint();
        let function_body = self.function_body(&mut library);
        let library_code = library.all_imports();

        let expected_length_prior: usize = self.input_types().iter().map(|x| x.get_size()).sum();
        let expected_length_after: usize = self.output_types().iter().map(|x| x.get_size()).sum();
        assert_eq!(
            Self::stack_diff(),
            (expected_length_after as isize - expected_length_prior as isize),
            "Declared stack diff must match type indicators"
        );

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

    fn run_tasm(&self, execution_state: &mut ExecutionState) -> ExecutionResult
    where
        Self: Sized,
    {
        let stack_prior = execution_state.stack.clone();
        let ret = self.run_tasm_old(
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
pub fn compile_snippet<T: Snippet>(snippet: T) -> String {
    let mut library = Library::with_pseudo_instructions();
    let main_entrypoint = snippet.entrypoint();
    let main_function_body = snippet.function_body(&mut library);
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
    snippet: T,
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
    code.push(compile_snippet::<T>(snippet));

    // Parse and run the program, bootloader and library
    let code: String = code.concat();
    let program = Program::from_code_nom(&code).unwrap();
    let std_in = execution_state.std_in;
    let secret_in = execution_state.secret_in;
    let (aet, _out, err) = vm::simulate(&program, std_in, secret_in);
    if let Some(err) = err {
        panic!("Program:\n{code}\nFailed:\n{err}");
    }

    (aet, inflated_clock_cycles)
}

#[cfg(test)]
mod tests {
    use itertools::Itertools;

    use super::*;
    use crate::arithmetic;

    #[test]
    fn can_return_code() {
        let mut empty_library = Library::default();
        let example_snippet =
            arithmetic::u32::safe_add::SafeAdd.function_body_as_instructions(&mut empty_library);
        assert!(!example_snippet.is_empty());
        println!(
            "{}",
            example_snippet.iter().map(|x| x.to_string()).join("\n")
        );
    }
}
