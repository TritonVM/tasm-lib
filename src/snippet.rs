use itertools::Itertools;
use rand::{thread_rng, Rng};
use std::collections::HashMap;
use std::fmt::Display;
use triton_opcodes::instruction::LabelledInstruction;
use triton_opcodes::parser::{parse, to_labelled};
use twenty_first::shared_math::b_field_element::BFieldElement;
use twenty_first::shared_math::x_field_element::EXTENSION_DEGREE;

use crate::dyn_malloc::DYN_MALLOC_ADDRESS;
use crate::snippet_state::SnippetState;
use crate::{execute, ExecutionState};
use crate::{ExecutionResult, DIGEST_LENGTH};

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum InputSource {
    StdIn,
    SecretIn,
}

impl Display for InputSource {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let str = match self {
            InputSource::StdIn => "stdin",
            InputSource::SecretIn => "secin",
        };

        write!(f, "{}", str)
    }
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum DataType {
    Bool,
    U32,
    U64,
    U128,
    BFE,
    XFE,
    Digest,
    List(Box<DataType>),
}

impl DataType {
    pub fn random_elements(&self, count: usize) -> Vec<Vec<BFieldElement>> {
        let mut rng = thread_rng();
        match self {
            DataType::Bool => {
                let bools: Vec<bool> = (0..count).map(|_| rng.gen_bool(0.5)).collect();
                bools
                    .iter()
                    .map(|x| vec![BFieldElement::new(*x as u64)])
                    .collect_vec()
            }
            DataType::U32 => (0..count)
                .map(|_| vec![BFieldElement::new(rng.gen_range(0..=u32::MAX as u64))])
                .collect_vec(),
            DataType::U64 => (0..2 * count)
                .map(|_| BFieldElement::new(rng.gen_range(0..=u32::MAX as u64)))
                .tuples()
                .map(|(a, b)| vec![a, b])
                .collect_vec(),
            DataType::U128 => (0..4 * count)
                .map(|_| BFieldElement::new(rng.gen_range(0..=u32::MAX as u64)))
                .tuples()
                .map(|(a, b, c, d)| vec![a, b, c, d])
                .collect_vec(),
            DataType::BFE => (0..count)
                .map(|_| vec![BFieldElement::new(rng.gen_range(0..=BFieldElement::MAX))])
                .collect_vec(),
            DataType::XFE => (0..EXTENSION_DEGREE * count)
                .map(|_| BFieldElement::new(rng.gen_range(0..=BFieldElement::MAX)))
                .tuples()
                .map(|(a, b, c)| vec![a, b, c])
                .collect_vec(),
            DataType::Digest => (0..DIGEST_LENGTH * count)
                .map(|_| BFieldElement::new(rng.gen_range(0..=BFieldElement::MAX)))
                .tuples()
                .map(|(a, b, c, d, e)| vec![a, b, c, d, e])
                .collect_vec(),
            DataType::List(_) => panic!("Random generation of lists is not supported"),
        }
    }
}

// Display for list is used to derive seperate entrypoint names for snippet implementations that take a type parameter
impl Display for DataType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                DataType::Bool => "bool".to_string(),
                DataType::U32 => "u32".to_string(),
                DataType::U64 => "u64".to_string(),
                DataType::U128 => "u128".to_string(),
                DataType::BFE => "bfe".to_string(),
                DataType::XFE => "xfe".to_string(),
                DataType::Digest => "digest".to_string(),
                DataType::List(element_type) => format!("list({element_type})"),
            }
        )
    }
}

impl DataType {
    pub fn get_size(&self) -> usize {
        match self {
            DataType::Bool => 1,
            DataType::U32 => 1,
            DataType::U64 => 2,
            DataType::U128 => 4,
            DataType::BFE => 1,
            DataType::XFE => 3,
            DataType::Digest => DIGEST_LENGTH,
            DataType::List(_) => 1,
        }
    }
}

pub trait Snippet: std::fmt::Debug {
    /// The name of a Snippet
    ///
    /// This is used as a unique identifier, e.g. when generating labels.
    // fn entrypoint(&self) -> String;
    fn entrypoint(&self) -> String;

    /// The input stack
    fn inputs(&self) -> Vec<String>
    where
        Self: Sized;

    fn input_types(&self) -> Vec<DataType>;

    fn output_types(&self) -> Vec<DataType>;

    /// The output stack
    fn outputs(&self) -> Vec<String>
    where
        Self: Sized;

    /// The stack difference
    fn stack_diff(&self) -> isize
    where
        Self: Sized;

    /// The function body
    fn function_body(&self, library: &mut SnippetState) -> String;

    /// Ways in which this snippet can crash
    fn crash_conditions() -> Vec<String>
    where
        Self: Sized;

    /// Examples of valid initial states for running this snippet
    fn gen_input_states(&self) -> Vec<ExecutionState>
    where
        Self: Sized;

    fn common_case_input_state(&self) -> ExecutionState
    where
        Self: Sized;

    fn worst_case_input_state(&self) -> ExecutionState
    where
        Self: Sized;

    fn function_body_as_instructions(&self, library: &mut SnippetState) -> Vec<LabelledInstruction>
    where
        Self: Sized,
    {
        let f_body = self.function_body(library);

        // parse the code to get the list of instructions
        to_labelled(&parse(&f_body).unwrap())
    }

    // The rust shadowing and the run tasm function must take the same argument
    // since this makes it possible to auto-generate tests for these two functions
    // for any snippet of code since these two functions must mutate the stack in
    // the same manner.
    fn rust_shadowing(
        &self,
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
        words_statically_allocated: usize,
    ) -> ExecutionResult
    where
        Self: Sized,
    {
        let mut library = SnippetState::with_preallocated_memory(words_statically_allocated);
        let entrypoint = self.entrypoint();
        let function_body = self.function_body(&mut library);
        let library_code = library.all_imports();

        let expected_length_prior: usize = self.input_types().iter().map(|x| x.get_size()).sum();
        let expected_length_after: usize = self.output_types().iter().map(|x| x.get_size()).sum();
        assert_eq!(
            Self::stack_diff(self),
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

        // Initialize a value for malloc, but only if it isn't already set
        let dyn_malloc_init_value = match memory.get(&BFieldElement::new(DYN_MALLOC_ADDRESS as u64))
        {
            Some(_value) => None,
            None => Some(library.get_next_free_address()),
        };
        execute(
            &code,
            stack,
            Self::stack_diff(self),
            std_in,
            secret_in,
            memory,
            dyn_malloc_init_value,
        )
        .unwrap()
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
            stack_prior[0..(stack_prior.len() - Self::inputs(self).len())],
            stack_after[0..(stack_after.len() - Self::outputs(self).len())]
        );

        ret
    }
}

#[cfg(test)]
mod tests {
    use itertools::Itertools;

    use super::*;
    use crate::arithmetic;

    #[test]
    fn can_return_code() {
        let mut empty_library = SnippetState::default();
        let example_snippet =
            arithmetic::u32::safe_add::SafeAdd.function_body_as_instructions(&mut empty_library);
        assert!(!example_snippet.is_empty());
        println!(
            "{}",
            example_snippet.iter().map(|x| x.to_string()).join("\n")
        );
    }
}
