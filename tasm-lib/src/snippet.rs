use anyhow::Result;
use itertools::Itertools;
use rand::{random, thread_rng, Rng};
use std::collections::HashMap;
use std::fmt::Display;
use std::ops::Deref;
use triton_vm::instruction::LabelledInstruction;
use triton_vm::parser::{to_labelled_instructions, tokenize};
use triton_vm::{triton_asm, NonDeterminism};
use twenty_first::shared_math::b_field_element::BFieldElement;

use crate::library::Library;
use crate::test_helpers::test_rust_equivalence_given_execution_state_deprecated;
use crate::{execute_bench_deprecated, ExecutionResult, VmOutputState, DIGEST_LENGTH};
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

pub enum SnippetType {
    // A Snippet can do anything, but is on its way out.
    DepracatedSnippet,

    // A Closure modifies the top of the stack without reading memory.
    Closure,

    // A Function can modify the top of the stack, and can read and extend memory.
    // Specifically: any memory writes have to happen to addresses larger than the
    // dynamic memory allocator and the dynamic memory allocator value has to be updated
    // accordingly.
    Function,

    // An Algorithm can modify memory even at addresses below the dynamic memory
    // allocator, and can take nondeterministic input.
    Algorithm,

    // A Procedure can do the above and read from standard in and write to standard out.
    Procedure,

    // A CompiledProgram can do the above and additionally be proven and verified.
    CompiledProgram,
}

pub trait BasicSnippet {
    fn inputs(&self) -> Vec<(DataType, String)>;
    fn outputs(&self) -> Vec<(DataType, String)>;
    fn entrypoint(&self) -> String;
    fn code(&self, library: &mut Library) -> Vec<LabelledInstruction>;

    fn stack_diff(&self) -> isize {
        let mut diff = 0isize;
        for (dt, _name) in self.inputs() {
            diff -= dt.get_size() as isize;
        }
        for (dt, _name) in self.outputs() {
            diff += dt.get_size() as isize;
        }
        diff
    }
}

/// ShadowWrapper is a new type that extends a snippet-like type with standardized rust
/// shadowing and other test-related functions.
pub struct ShadowWrapper<T>(pub Box<T>);

impl<T: BasicSnippet> Deref for ShadowWrapper<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

pub trait RustShadow {
    fn inner(&self) -> Box<dyn BasicSnippet>;

    fn rust_shadow_wrapper(
        &self,
        stdin: &[BFieldElement],
        nondeterminism: &NonDeterminism<BFieldElement>,
        stack: &mut Vec<BFieldElement>,
        memory: &mut HashMap<BFieldElement, BFieldElement>,
    ) -> Vec<BFieldElement>;

    fn test(&self);

    fn bench(&self);
}

pub trait DeprecatedSnippet {
    /// The name of a Snippet
    ///
    /// This is used as a unique identifier, e.g. when generating labels.
    // fn entrypoint(&self) -> String;
    fn entrypoint_name(&self) -> String;

    /// The input stack
    fn input_field_names(&self) -> Vec<String>;

    fn input_types(&self) -> Vec<DataType>;

    /// The output stack
    fn output_field_names(&self) -> Vec<String>;

    fn output_types(&self) -> Vec<DataType>;

    /// The stack difference
    fn stack_diff(&self) -> isize;

    /// The function
    fn function_code(&self, library: &mut Library) -> String;

    /// Ways in which this snippet can crash at runtime
    fn crash_conditions(&self) -> Vec<String>;

    /// Examples of valid initial states for running this snippet
    fn gen_input_states(&self) -> Vec<ExecutionState>;

    fn common_case_input_state(&self) -> ExecutionState;

    fn worst_case_input_state(&self) -> ExecutionState;

    fn function_code_as_instructions(&self, library: &mut Library) -> Vec<LabelledInstruction> {
        let f_body = self.function_code(library);

        // parse the string without link-checking
        let (_, tokens) = tokenize(&f_body).unwrap();
        to_labelled_instructions(&tokens)
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

    fn link_for_isolated_run(&self, words_statically_allocated: usize) -> Vec<LabelledInstruction> {
        let mut snippet_state = Library::with_preallocated_memory(words_statically_allocated);
        let entrypoint = self.entrypoint_name();
        let function_body = self.function_code(&mut snippet_state);
        let library_code = snippet_state.all_imports();

        // The TASM code is always run through a function call, so the 1st instruction
        // is a call to the function in question.
        let code = triton_asm!(
            call {entrypoint}
            halt

            {function_body}
            {&library_code}
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
            &NonDeterminism::new(secret_in),
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

        execute_bench_deprecated(
            &code,
            stack,
            Self::stack_diff(self),
            std_in,
            NonDeterminism::new(secret_in),
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
            execution_state.nondeterminism.individual_tokens.clone(),
            &mut execution_state.memory,
            execution_state.words_allocated,
        );
        let stack_after = execution_state.stack.clone();

        // Assert equality of stack elements under input arguments, but don't check program
        // hash that's located at the bottom of the stack.
        assert_eq!(
            stack_prior[DIGEST_LENGTH..(stack_prior.len() - Self::input_field_names(self).len())],
            stack_after[DIGEST_LENGTH..(stack_after.len() - Self::output_field_names(self).len())]
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
            execution_state.nondeterminism.individual_tokens.clone(),
            &mut execution_state.memory,
            execution_state.words_allocated,
        );
        let stack_after = execution_state.stack.clone();

        // Assert equality of stack elements under input arguments, but don't check program
        // hash that's located at the bottom of the stack.
        assert_eq!(
            stack_prior[DIGEST_LENGTH..(stack_prior.len() - Self::input_field_names(self).len())],
            stack_after[DIGEST_LENGTH..(stack_after.len() - Self::output_field_names(self).len())]
        );

        ret
    }
}

struct DeprecatedSnippetWrapper<S: DeprecatedSnippet> {
    pub deprecated_snippet: S,
}

impl<S: DeprecatedSnippet + Clone + 'static> RustShadow for DeprecatedSnippetWrapper<S> {
    fn rust_shadow_wrapper(
        &self,
        stdin: &[BFieldElement],
        nondeterminism: &NonDeterminism<BFieldElement>,
        stack: &mut Vec<BFieldElement>,
        memory: &mut HashMap<BFieldElement, BFieldElement>,
    ) -> Vec<BFieldElement> {
        let mut stack_copy = stack.to_vec();
        self.deprecated_snippet.rust_shadowing(
            &mut stack_copy,
            stdin.to_vec(),
            nondeterminism.individual_tokens.clone(),
            memory,
        );
        *stack = stack_copy;
        vec![]
    }

    fn test(&self) {
        let mut execution_states = self.deprecated_snippet.gen_input_states();

        for execution_state in execution_states.iter_mut() {
            test_rust_equivalence_given_execution_state_deprecated(
                &self.deprecated_snippet,
                execution_state.clone(),
            );
        }
    }

    fn bench(&self) {
        todo!()
    }

    fn inner(&self) -> Box<dyn BasicSnippet> {
        Box::new(self.deprecated_snippet.clone())
    }
}

impl<S: DeprecatedSnippet> BasicSnippet for S {
    fn inputs(&self) -> Vec<(DataType, String)> {
        self.input_types()
            .into_iter()
            .zip(self.input_field_names())
            .collect()
    }

    fn outputs(&self) -> Vec<(DataType, String)> {
        self.output_types()
            .into_iter()
            .zip(self.output_field_names())
            .collect()
    }

    fn entrypoint(&self) -> String {
        self.entrypoint_name()
    }

    fn code(&self, library: &mut Library) -> Vec<LabelledInstruction> {
        self.function_code_as_instructions(library)
    }
}

impl DeprecatedSnippet for ShadowWrapper<Box<dyn DeprecatedSnippet>> {
    fn entrypoint_name(&self) -> String {
        self.0.entrypoint_name()
    }

    fn input_field_names(&self) -> Vec<String> {
        self.0.input_field_names()
    }

    fn input_types(&self) -> Vec<DataType> {
        self.0.input_types()
    }

    fn output_field_names(&self) -> Vec<String> {
        self.0.output_field_names()
    }

    fn output_types(&self) -> Vec<DataType> {
        self.0.output_types()
    }

    fn stack_diff(&self) -> isize {
        self.0.stack_diff()
    }

    fn function_code(&self, library: &mut Library) -> String {
        self.0.function_code(library)
    }

    fn crash_conditions(&self) -> Vec<String> {
        self.0.crash_conditions()
    }

    fn gen_input_states(&self) -> Vec<ExecutionState> {
        self.0.gen_input_states()
    }

    fn common_case_input_state(&self) -> ExecutionState {
        self.0.common_case_input_state()
    }

    fn worst_case_input_state(&self) -> ExecutionState {
        self.0.worst_case_input_state()
    }

    fn rust_shadowing(
        &self,
        stack: &mut Vec<BFieldElement>,
        std_in: Vec<BFieldElement>,
        secret_in: Vec<BFieldElement>,
        memory: &mut HashMap<BFieldElement, BFieldElement>,
    ) {
        self.0.rust_shadowing(stack, std_in, secret_in, memory)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::arithmetic;

    #[test]
    fn can_return_code() {
        let mut empty_library = Library::new();
        let example_snippet =
            arithmetic::u32::safe_add::SafeAdd.function_code_as_instructions(&mut empty_library);
        assert!(!example_snippet.is_empty());
        println!(
            "{}",
            example_snippet.iter().map(|x| x.to_string()).join("\n")
        );
    }
}
