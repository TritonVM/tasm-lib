use anyhow::Result;
use itertools::Itertools;
use rand::{random, thread_rng, Rng};
use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt::Debug;
use std::fmt::Display;
use std::rc::Rc;
use triton_vm::instruction::LabelledInstruction;
use triton_vm::parser::{to_labelled_instructions, tokenize};
use triton_vm::{triton_asm, NonDeterminism, Program};
use twenty_first::shared_math::b_field_element::BFieldElement;

use crate::execute_with_terminal_state;
use crate::library::Library;
use crate::program_with_state_preparation;
use crate::test_helpers::test_rust_equivalence_given_execution_state_deprecated;
use crate::VmHasherState;
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
    Tuple(Vec<DataType>),
    VoidPointer,
}

impl DataType {
    /// Return a string matching how the variant looks in source code
    pub fn variant_name(&self) -> String {
        // This function is used to autogenerate snippets in the tasm-lang compiler
        match self {
            DataType::Bool => "DataType::Bool".to_owned(),
            DataType::U32 => "DataType::U32".to_owned(),
            DataType::U64 => "DataType::U64".to_owned(),
            DataType::U128 => "DataType::U128".to_owned(),
            DataType::BFE => "DataType::BFE".to_owned(),
            DataType::XFE => "DataType::XFE".to_owned(),
            DataType::Digest => "DataType::Digest".to_owned(),
            DataType::List(elem_type) => {
                format!("DataType::List(Box::new({}))", elem_type.variant_name())
            }
            DataType::VoidPointer => "DataType::VoidPointer".to_owned(),
            DataType::Tuple(elements) => {
                let elements_as_variant_names =
                    elements.iter().map(|x| x.variant_name()).collect_vec();
                format!(
                    "DataType::Tuple(vec![{}])",
                    elements_as_variant_names.join(", ")
                )
            }
        }
    }

    /// Return a collection of different data types, used for testing
    #[cfg(test)]
    pub fn big_random_generatable_type_collection() -> Vec<DataType> {
        vec![
            DataType::Bool,
            DataType::U32,
            DataType::U64,
            DataType::U128,
            DataType::BFE,
            DataType::XFE,
            DataType::Digest,
            DataType::VoidPointer,
            DataType::Tuple(vec![DataType::Bool]),
            DataType::Tuple(vec![DataType::XFE, DataType::Bool]),
            DataType::Tuple(vec![DataType::XFE, DataType::Digest]),
            DataType::Tuple(vec![DataType::Bool, DataType::Bool]),
            DataType::Tuple(vec![DataType::Digest, DataType::XFE]),
            DataType::Tuple(vec![DataType::BFE, DataType::XFE, DataType::Digest]),
            DataType::Tuple(vec![DataType::XFE, DataType::BFE, DataType::Digest]),
            DataType::Tuple(vec![
                DataType::U64,
                DataType::Digest,
                DataType::Digest,
                DataType::Digest,
            ]),
            DataType::Tuple(vec![
                DataType::Digest,
                DataType::Digest,
                DataType::Digest,
                DataType::U64,
            ]),
            DataType::Tuple(vec![
                DataType::Digest,
                DataType::XFE,
                DataType::U128,
                DataType::Bool,
            ]),
        ]
    }

    pub fn seeded_random_elements(
        &self,
        count: usize,
        rng: &mut impl Rng,
    ) -> Vec<Vec<BFieldElement>> {
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
            DataType::VoidPointer => (0..count)
                .map(|_| vec![random::<BFieldElement>()])
                .collect_vec(),
            DataType::Tuple(v) => (0..count)
                .map(|_| v.iter().flat_map(|dt| dt.random_elements(1)).concat())
                .collect(),
        }
    }

    pub fn random_elements(&self, count: usize) -> Vec<Vec<BFieldElement>> {
        let mut rng = thread_rng();
        self.seeded_random_elements(count, &mut rng)
    }
}

impl DataType {
    pub fn label_friendly_name(&self) -> String {
        match self {
            DataType::List(inner_type) => format!("list_L{}R", inner_type.label_friendly_name()),
            DataType::Tuple(inner_types) => {
                format!(
                    "tuple_L{}R",
                    inner_types
                        .iter()
                        .map(|x| x.label_friendly_name())
                        .join("_")
                )
            }
            DataType::VoidPointer => "void_pointer".to_string(),
            DataType::Bool => "bool".to_string(),
            DataType::U32 => "u32".to_string(),
            DataType::U64 => "u64".to_string(),
            DataType::U128 => "u128".to_string(),
            DataType::BFE => "bfe".to_string(),
            DataType::XFE => "xfe".to_string(),
            DataType::Digest => "digest".to_string(),
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
            DataType::VoidPointer => 1,
            DataType::Tuple(t) => t.iter().map(|dt| dt.get_size()).sum(),
        }
    }
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

pub trait RustShadow {
    fn inner(&self) -> Rc<RefCell<dyn BasicSnippet>>;

    fn rust_shadow_wrapper(
        &self,
        stdin: &[BFieldElement],
        nondeterminism: &NonDeterminism<BFieldElement>,
        stack: &mut Vec<BFieldElement>,
        memory: &mut HashMap<BFieldElement, BFieldElement>,
        sponge_state: &mut VmHasherState,
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

    fn link_for_isolated_run(
        &self,
        words_statically_allocated: Option<usize>,
    ) -> Vec<LabelledInstruction> {
        let mut snippet_state = if let Some(number_of_words) = words_statically_allocated {
            Library::with_preallocated_memory(number_of_words)
        } else {
            Library::new()
        };
        let entrypoint = self.entrypoint_name();
        let mut function_body = self.function_code(&mut snippet_state);
        function_body.push('\n'); // added bc of limitations in `triton_asm!`
        let library_code = snippet_state.all_imports();

        // The TASM code is always run through a function call, so the 1st instruction
        // is a call to the function in question.
        let code = triton_asm!(
            call {entrypoint}
            halt

            {function_body}
            {&library_code}
        );

        // Verify that program compiles, otherwise panics
        let _program = Program::new(&code);

        code
    }

    /// Return Ok(vm_output_state) if execution succeeds, Err(error_message) if VM execution fails,
    /// panics if anything else goes wrong.
    fn link_and_run_tasm_for_test(
        &self,
        stack: &mut Vec<BFieldElement>,
        std_in: Vec<BFieldElement>,
        secret_in: Vec<BFieldElement>,
        memory: &mut HashMap<BFieldElement, BFieldElement>,
        words_allocated: Option<usize>,
    ) -> anyhow::Result<VmOutputState> {
        let expected_length_prior: usize = self.input_types().iter().map(|x| x.get_size()).sum();
        let expected_length_after: usize = self.output_types().iter().map(|x| x.get_size()).sum();
        assert_eq!(
            Self::stack_diff(self),
            (expected_length_after as isize - expected_length_prior as isize),
            "Declared stack diff must match type indicators"
        );

        let mut nondeterminism = NonDeterminism::new(secret_in).with_ram(memory.clone());

        let code = self.link_for_isolated_run(words_allocated);
        let program =
            program_with_state_preparation(&code, stack, &mut nondeterminism, words_allocated);
        let tvm_result = execute_with_terminal_state(&program, &std_in, &mut nondeterminism);

        let maybe_final_state = tvm_result.map(|st| VmOutputState {
            final_ram: st.ram,
            final_sponge_state: VmHasherState {
                state: st.sponge_state,
            },
            final_stack: st.op_stack.stack,
            output: st.public_output,
        });

        if maybe_final_state.is_ok() {
            execute_test(
                &code,
                stack,
                Self::stack_diff(self),
                std_in,
                &mut nondeterminism,
                memory,
                words_allocated,
            );
        }

        maybe_final_state
    }

    fn link_and_run_tasm_for_bench(
        &self,
        stack: &mut Vec<BFieldElement>,
        std_in: Vec<BFieldElement>,
        secret_in: Vec<BFieldElement>,
        memory: &mut HashMap<BFieldElement, BFieldElement>,
        words_statically_allocated: Option<usize>,
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
            words_statically_allocated,
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
            Some(execution_state.words_allocated),
        );
        let stack_after = execution_state.stack.clone();

        // Assert equality of stack elements under input arguments, but don't check program
        // hash that's located at the bottom of the stack.
        assert_eq!(
            stack_prior[DIGEST_LENGTH..(stack_prior.len() - Self::input_field_names(self).len())],
            stack_after[DIGEST_LENGTH..(stack_after.len() - Self::output_field_names(self).len())]
        );

        ret.unwrap()
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
            Some(execution_state.words_allocated),
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
        _sponge_state: &mut VmHasherState,
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

        let snippet = &self.deprecated_snippet;

        for execution_state in execution_states.iter_mut() {
            test_rust_equivalence_given_execution_state_deprecated(
                snippet,
                execution_state.clone(),
            );
        }
    }

    fn bench(&self) {
        todo!()
    }

    fn inner(&self) -> Rc<RefCell<dyn BasicSnippet>> {
        Rc::new(RefCell::new(self.deprecated_snippet.clone()))
    }
}

impl<S: DeprecatedSnippet> BasicSnippet for S {
    fn inputs(&self) -> Vec<(DataType, String)> {
        // Notice that the deprecated snippet trait has its input and output
        // names listed in terms of words, not values of a given type,
        // so there is no perfect mapping for this function.

        // This is a bit of a hack, but it should give reasonable input
        // value names for most snippets.
        let mut ret = vec![];
        let mut field_name_index = 0;
        let field_names = self.input_field_names();
        for input_type in self.input_types() {
            ret.push((
                input_type.clone(),
                format!("input_{}", field_names[field_name_index]),
            ));

            field_name_index += input_type.get_size();
        }

        ret
    }

    fn outputs(&self) -> Vec<(DataType, String)> {
        // Notice that the deprecated snippet trait has its input and output
        // names listed in terms of words, not values of a given type,
        // so there is no perfect mapping for this function.

        // This is a bit of a hack, but it should give reasonable output
        // value names for most snippets.
        let mut ret = vec![];
        let mut field_name_index = 0;
        let field_names = self.output_field_names();
        for input_type in self.output_types() {
            ret.push((
                input_type.clone(),
                format!("output_{}", field_names[field_name_index]),
            ));

            field_name_index += input_type.get_size();
        }

        ret
    }

    fn entrypoint(&self) -> String {
        self.entrypoint_name()
    }

    fn code(&self, library: &mut Library) -> Vec<LabelledInstruction> {
        self.function_code_as_instructions(library)
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

    #[test]
    fn data_type_string_rep() {
        assert_eq!("DataType::Digest", DataType::Digest.variant_name());
        assert_eq!(
            "DataType::Tuple(vec![DataType::XFE, DataType::BFE, DataType::Digest])",
            DataType::Tuple(vec![DataType::XFE, DataType::BFE, DataType::Digest]).variant_name()
        );
        assert_eq!(
            "DataType::List(Box::new(DataType::Digest))",
            DataType::List(Box::new(DataType::Digest)).variant_name()
        );
    }
}
