use std::collections::HashMap;

use anyhow::Result;
use triton_vm::isa::parser::to_labelled_instructions;
use triton_vm::isa::parser::tokenize;
use triton_vm::prelude::*;

use crate::data_type::DataType;
use crate::execute_bench_deprecated;
use crate::execute_test;
use crate::execute_with_terminal_state;
use crate::library::Library;
use crate::snippet_bencher::BenchmarkResult;
use crate::InitVmState;

use super::basic_snippet::BasicSnippet;

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
    fn gen_input_states(&self) -> Vec<InitVmState>;

    fn common_case_input_state(&self) -> InitVmState;

    fn worst_case_input_state(&self) -> InitVmState;

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

    fn link_for_isolated_run(&self) -> Vec<LabelledInstruction> {
        let mut snippet_state = Library::new();
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
        nondeterminism: NonDeterminism,
    ) -> Result<VMState> {
        let expected_length_prior: usize = self.input_types().iter().map(|x| x.stack_size()).sum();
        let expected_length_after: usize = self.output_types().iter().map(|x| x.stack_size()).sum();
        assert_eq!(
            Self::stack_diff(self),
            expected_length_after as isize - expected_length_prior as isize,
            "Declared stack diff must match type indicators"
        );

        let code = self.link_for_isolated_run();
        let program = Program::new(&code);
        let tvm_result =
            execute_with_terminal_state(&program, &std_in, stack, &nondeterminism, None);

        let final_state = tvm_result?;

        execute_test(
            &code,
            stack,
            Self::stack_diff(self),
            std_in,
            nondeterminism,
            None,
        );
        Ok(final_state)
    }

    fn link_and_run_tasm_for_bench(
        &self,
        stack: &mut Vec<BFieldElement>,
        std_in: Vec<BFieldElement>,
        nondeterminism: NonDeterminism,
    ) -> Result<BenchmarkResult> {
        let expected_length_prior: usize = self.input_types().iter().map(|x| x.stack_size()).sum();
        let expected_length_after: usize = self.output_types().iter().map(|x| x.stack_size()).sum();
        assert_eq!(
            Self::stack_diff(self),
            expected_length_after as isize - expected_length_prior as isize,
            "Declared stack diff must match type indicators"
        );

        let code = self.link_for_isolated_run();

        execute_bench_deprecated(&code, stack, Self::stack_diff(self), std_in, nondeterminism)
    }

    fn link_and_run_tasm_from_state_for_test(&self, execution_state: &mut InitVmState) -> VMState {
        let stack_prior = execution_state.stack.clone();
        let ret = self.link_and_run_tasm_for_test(
            &mut execution_state.stack,
            execution_state.public_input.clone(),
            execution_state.nondeterminism.to_owned(),
        );
        let stack_after = execution_state.stack.clone();

        // Assert equality of stack elements under input arguments, but don't check program
        // hash that's located at the bottom of the stack.
        assert_eq!(
            stack_prior[Digest::LEN..(stack_prior.len() - Self::input_field_names(self).len())],
            stack_after[Digest::LEN..(stack_after.len() - Self::output_field_names(self).len())]
        );

        ret.unwrap()
    }

    fn link_and_run_tasm_from_state_for_bench(
        &self,
        execution_state: &mut InitVmState,
    ) -> Result<BenchmarkResult> {
        let stack_prior = execution_state.stack.clone();
        let ret = self.link_and_run_tasm_for_bench(
            &mut execution_state.stack,
            execution_state.public_input.clone(),
            execution_state.nondeterminism.to_owned(),
        );
        let stack_after = execution_state.stack.clone();

        // Assert equality of stack elements under input arguments, but don't check program
        // hash that's located at the bottom of the stack.
        assert_eq!(
            stack_prior[Digest::LEN..(stack_prior.len() - Self::input_field_names(self).len())],
            stack_after[Digest::LEN..(stack_after.len() - Self::output_field_names(self).len())]
        );

        ret
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

            field_name_index += input_type.stack_size();
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

            field_name_index += input_type.stack_size();
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
pub(crate) mod tests {
    use std::cell::RefCell;
    use std::rc::Rc;
    use std::str::FromStr;

    use itertools::Itertools;

    use crate::arithmetic;
    use crate::test_helpers::test_rust_equivalence_given_execution_state_deprecated;
    use crate::traits::rust_shadow::RustShadow;
    use crate::VmHasher;

    use super::*;

    pub(crate) struct DeprecatedSnippetWrapper<S: DeprecatedSnippet> {
        pub(crate) deprecated_snippet: S,
    }

    impl<S: DeprecatedSnippet> DeprecatedSnippetWrapper<S> {
        pub(crate) fn new(deprecated_snippet: S) -> Self {
            Self { deprecated_snippet }
        }
    }

    impl<S: DeprecatedSnippet + Clone + 'static> RustShadow for DeprecatedSnippetWrapper<S> {
        fn inner(&self) -> Rc<RefCell<dyn BasicSnippet>> {
            Rc::new(RefCell::new(self.deprecated_snippet.clone()))
        }

        fn rust_shadow_wrapper(
            &self,
            stdin: &[BFieldElement],
            nondeterminism: &NonDeterminism,
            stack: &mut Vec<BFieldElement>,
            memory: &mut HashMap<BFieldElement, BFieldElement>,
            _sponge: &mut Option<VmHasher>,
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
    }

    #[test]
    fn can_return_code() {
        let mut empty_library = Library::new();
        let example_snippet =
            arithmetic::u32::safeadd::Safeadd.function_code_as_instructions(&mut empty_library);
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
            DataType::Tuple(vec![DataType::Xfe, DataType::Bfe, DataType::Digest]).variant_name()
        );
        assert_eq!(
            "DataType::List(Box::new(DataType::Digest))",
            DataType::List(Box::new(DataType::Digest)).variant_name()
        );
    }

    #[test]
    fn parse_and_stringify_datatype_test() {
        assert_eq!(DataType::Digest, DataType::from_str("digest").unwrap());
        assert_eq!(
            DataType::Bfe,
            DataType::from_str(&DataType::Bfe.label_friendly_name()).unwrap()
        );
        for data_type in DataType::big_random_generatable_type_collection() {
            assert_eq!(
                data_type,
                DataType::from_str(&data_type.label_friendly_name()).unwrap()
            )
        }
    }
}
