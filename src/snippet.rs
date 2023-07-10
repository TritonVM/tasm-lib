use anyhow::Result;
use itertools::Itertools;
use rand::{random, thread_rng, Rng};
use std::collections::HashMap;
use std::fmt::Display;
use triton_vm::instruction::LabelledInstruction;
use triton_vm::parser::{parse, to_labelled};
use twenty_first::shared_math::b_field_element::BFieldElement;

use crate::snippet_state::SnippetState;
use crate::{execute_bench, ExecutionResult, VmOutputState, DIGEST_LENGTH};
use crate::{execute_test, ExecutionState};

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
    Pair(Box<DataType>, Box<DataType>),
    VoidPointer,
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
            DataType::XFE => (0..count)
                .map(|_| vec![random(), random(), random()])
                .collect_vec(),
            DataType::Digest => (0..DIGEST_LENGTH * count)
                .map(|_| BFieldElement::new(rng.gen_range(0..=BFieldElement::MAX)))
                .tuples()
                .map(|(a, b, c, d, e)| vec![a, b, c, d, e])
                .collect_vec(),
            DataType::List(_) => panic!("Random generation of lists is not supported"),
            DataType::Pair(left, right) => (0..count)
                .map(|_| {
                    vec![
                        left.random_elements(1)[0].clone(),
                        right.random_elements(1)[0].clone(),
                    ]
                    .concat()
                })
                .collect_vec(),
            DataType::VoidPointer => vec![vec![random::<BFieldElement>()]],
        }
    }
}

// Display for list is used to derive seperate entrypoint names for snippet implementations that take a type parameter
impl Display for DataType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let str = match self {
            DataType::Bool => "bool".to_string(),
            DataType::U32 => "u32".to_string(),
            DataType::U64 => "u64".to_string(),
            DataType::U128 => "u128".to_string(),
            DataType::BFE => "bfe".to_string(),
            DataType::XFE => "xfe".to_string(),
            DataType::Digest => "digest".to_string(),
            DataType::List(element_type) => format!("list({element_type})"),
            DataType::Pair(left, right) => format!("pair({left},{right})"),
            DataType::VoidPointer => "void-pointer".to_string(),
        };
        write!(f, "{str}",)
    }
}

impl DataType {
    pub fn label_friendly_name(&self) -> String {
        match self {
            DataType::Pair(left, right) => format!(
                "pair_L{}_and_{}R",
                left.label_friendly_name(),
                right.label_friendly_name()
            ),
            DataType::List(inner_type) => format!("list_L{}R", inner_type),
            DataType::VoidPointer => "void_pointer".to_string(),
            _ => format!("{}", self),
        }
    }
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
            DataType::Pair(left, right) => left.get_size() + right.get_size(),
            DataType::VoidPointer => 1,
        }
    }
}

pub trait Snippet {
    /// The name of a Snippet
    ///
    /// This is used as a unique identifier, e.g. when generating labels.
    // fn entrypoint(&self) -> String;
    fn entrypoint(&self) -> String;

    /// The input stack
    fn inputs(&self) -> Vec<String>;

    fn input_types(&self) -> Vec<DataType>;

    fn output_types(&self) -> Vec<DataType>;

    /// The output stack
    fn outputs(&self) -> Vec<String>;

    /// The stack difference
    fn stack_diff(&self) -> isize;

    /// The function
    fn function_code(&self, library: &mut SnippetState) -> String;

    /// Ways in which this snippet can crash at runtime
    fn crash_conditions(&self) -> Vec<String>;

    /// Examples of valid initial states for running this snippet
    fn gen_input_states(&self) -> Vec<ExecutionState>;

    fn common_case_input_state(&self) -> ExecutionState;

    fn worst_case_input_state(&self) -> ExecutionState;

    fn function_code_as_instructions(
        &self,
        library: &mut SnippetState,
    ) -> Vec<LabelledInstruction> {
        let f_body = self.function_code(library);

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
    );

    fn link_for_isolated_run(&self, words_statically_allocated: usize) -> String {
        let mut snippet_state = SnippetState::with_preallocated_memory(words_statically_allocated);
        let entrypoint = self.entrypoint();
        let function_body = self.function_code(&mut snippet_state);
        let library_code = snippet_state.all_imports();

        // The TASM code is always run through a function call, so the 1st instruction
        // is a call to the function in question.
        let code = format!(
            "
            call {entrypoint}
            halt

            {function_body}
            {library_code}
            "
        );

        code
    }

    fn link_and_run_tasm_for_test(
        &self,
        stack: &mut Vec<BFieldElement>,
        std_in: Vec<BFieldElement>,
        secret_in: Vec<BFieldElement>,
        memory: &mut HashMap<BFieldElement, BFieldElement>,
        words_statically_allocated: usize,
    ) -> VmOutputState {
        let expected_length_prior: usize = self.input_types().iter().map(|x| x.get_size()).sum();
        let expected_length_after: usize = self.output_types().iter().map(|x| x.get_size()).sum();
        assert_eq!(
            Self::stack_diff(self),
            (expected_length_after as isize - expected_length_prior as isize),
            "Declared stack diff must match type indicators"
        );

        let code = self.link_for_isolated_run(words_statically_allocated);

        execute_test(
            &code,
            stack,
            Self::stack_diff(self),
            std_in,
            secret_in,
            memory,
            Some(words_statically_allocated),
        )
        .unwrap()
    }

    fn link_and_run_tasm_for_bench(
        &self,
        stack: &mut Vec<BFieldElement>,
        std_in: Vec<BFieldElement>,
        secret_in: Vec<BFieldElement>,
        memory: &mut HashMap<BFieldElement, BFieldElement>,
        words_statically_allocated: usize,
    ) -> Result<ExecutionResult> {
        let expected_length_prior: usize = self.input_types().iter().map(|x| x.get_size()).sum();
        let expected_length_after: usize = self.output_types().iter().map(|x| x.get_size()).sum();
        assert_eq!(
            Self::stack_diff(self),
            (expected_length_after as isize - expected_length_prior as isize),
            "Declared stack diff must match type indicators"
        );

        let code = self.link_for_isolated_run(words_statically_allocated);

        execute_bench(
            &code,
            stack,
            Self::stack_diff(self),
            std_in,
            secret_in,
            memory,
            Some(words_statically_allocated),
        )
    }

    fn link_and_run_tasm_from_state_for_test(
        &self,
        execution_state: &mut ExecutionState,
    ) -> VmOutputState {
        let stack_prior = execution_state.stack.clone();
        let ret = self.link_and_run_tasm_for_test(
            &mut execution_state.stack,
            execution_state.std_in.clone(),
            execution_state.secret_in.clone(),
            &mut execution_state.memory,
            execution_state.words_allocated,
        );
        let stack_after = execution_state.stack.clone();

        // Assert equality of stack elements under input arguments, but don't check program
        // hash that's located at the bottom of the stack.
        assert_eq!(
            stack_prior[DIGEST_LENGTH..(stack_prior.len() - Self::inputs(self).len())],
            stack_after[DIGEST_LENGTH..(stack_after.len() - Self::outputs(self).len())]
        );

        ret
    }

    fn link_and_run_tasm_from_state_for_bench(
        &self,
        execution_state: &mut ExecutionState,
    ) -> Result<ExecutionResult> {
        let stack_prior = execution_state.stack.clone();
        let ret = self.link_and_run_tasm_for_bench(
            &mut execution_state.stack,
            execution_state.std_in.clone(),
            execution_state.secret_in.clone(),
            &mut execution_state.memory,
            execution_state.words_allocated,
        );
        let stack_after = execution_state.stack.clone();

        // Assert equality of stack elements under input arguments, but don't check program
        // hash that's located at the bottom of the stack.
        assert_eq!(
            stack_prior[DIGEST_LENGTH..(stack_prior.len() - Self::inputs(self).len())],
            stack_after[DIGEST_LENGTH..(stack_after.len() - Self::outputs(self).len())]
        );

        ret
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::arithmetic;

    #[test]
    fn can_return_code() {
        let mut empty_library = SnippetState::default();
        let example_snippet =
            arithmetic::u32::safe_add::SafeAdd.function_code_as_instructions(&mut empty_library);
        assert!(!example_snippet.is_empty());
        println!(
            "{}",
            example_snippet.iter().map(|x| x.to_string()).join("\n")
        );
    }
}
