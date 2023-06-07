use std::collections::{HashMap, HashSet};

use itertools::Itertools;
use num::One;
use triton_opcodes::instruction::LabelledInstruction;
use triton_opcodes::parser::{parse, to_labelled};
use twenty_first::shared_math::b_field_element::BFieldElement;

use crate::snippet::{DataType, Snippet};

// Ensure that static allocator does not overwrite the address
// dedicated to the dynamic allocator. Dynamic allocator is,
// by convention, always on address 0.
pub const STATIC_MEMORY_START_ADDRESS: usize = 1;

#[derive(Clone, Debug)]
pub struct SnippetState {
    seen_snippets: HashSet<String>,
    function_bodies: HashSet<String>,
    free_pointer: usize,
}

impl Default for SnippetState {
    fn default() -> Self {
        Self {
            seen_snippets: Default::default(),
            function_bodies: Default::default(),
            free_pointer: STATIC_MEMORY_START_ADDRESS,
        }
    }
}

impl SnippetState {
    /// Create an empty library.
    #[allow(dead_code)]
    pub fn empty() -> Self {
        Self::default()
    }

    pub fn with_preallocated_memory(words_allocated: usize) -> Self {
        SnippetState {
            free_pointer: words_allocated + STATIC_MEMORY_START_ADDRESS,
            ..Default::default()
        }
    }

    /// Import `T: Snippet` into the library.
    ///
    /// This recursively imports `T`'s dependencies.
    ///
    /// Does not import the snippets with the same entrypoint twice.
    ///
    /// Avoid cyclic dependencies by only calling `T::function_body()` which
    /// may call `.import()` if `.import::<T>()` wasn't already called once.
    pub fn import(&mut self, snippet: Box<dyn Snippet>) -> String {
        let dep_entrypoint = snippet.entrypoint();
        if self.seen_snippets.insert(dep_entrypoint) {
            let dep_function_body = snippet.function_code(self);
            self.function_bodies.insert(dep_function_body);
        }

        snippet.entrypoint()
    }

    pub fn explicit_import(&mut self, name: &str, body: String) -> String {
        if self.seen_snippets.insert(name.to_string()) {
            self.function_bodies.insert(body);
        }

        name.to_string()
    }

    /// Return the next free address without allocating anything
    pub fn get_next_free_address(&self) -> usize {
        self.free_pointer
    }

    #[allow(dead_code)]
    pub fn all_imports(&self) -> String {
        self.function_bodies
            .iter()
            .map(|s| format!("{s}\n"))
            .collect()
    }

    pub fn all_imports_as_instruction_lists(&self) -> Vec<LabelledInstruction> {
        to_labelled(&parse(&self.function_bodies.iter().join("\n")).unwrap())
    }

    pub fn kmalloc(&mut self, num_words: usize) -> usize {
        let address = self.free_pointer;
        self.free_pointer += num_words;
        address
    }
}

#[derive(Debug)]
pub struct DummyTestSnippetA;
#[derive(Debug)]
pub struct DummyTestSnippetB;
#[derive(Debug)]
pub struct DummyTestSnippetC;

impl Snippet for DummyTestSnippetA {
    fn stack_diff(&self) -> isize {
        3
    }

    fn entrypoint(&self) -> String {
        "tasm_a_dummy_test_value".to_string()
    }

    fn function_code(&self, library: &mut SnippetState) -> String {
        let entrypoint = self.entrypoint();
        let b = library.import(Box::new(DummyTestSnippetB));
        let c = library.import(Box::new(DummyTestSnippetC));

        format!(
            "
                {entrypoint}:
                    call {b}
                    call {c}
                    return
                "
        )
    }

    fn rust_shadowing(
        &self,
        stack: &mut Vec<BFieldElement>,
        _std_in: Vec<BFieldElement>,
        _secret_in: Vec<BFieldElement>,
        _memory: &mut HashMap<BFieldElement, BFieldElement>,
    ) {
        stack.push(BFieldElement::one());
        stack.push(BFieldElement::one());
        stack.push(BFieldElement::one());
    }

    fn inputs(&self) -> Vec<String> {
        vec![]
    }

    fn outputs(&self) -> Vec<String> {
        vec!["1".to_string(), "1".to_string(), "1".to_string()]
    }

    fn crash_conditions(&self) -> Vec<String> {
        vec![]
    }

    fn gen_input_states(&self) -> Vec<crate::ExecutionState> {
        vec![]
    }

    fn input_types(&self) -> Vec<crate::snippet::DataType> {
        vec![]
    }

    fn output_types(&self) -> Vec<crate::snippet::DataType> {
        vec![DataType::BFE, DataType::BFE, DataType::BFE]
    }

    fn common_case_input_state(&self) -> crate::ExecutionState
    where
        Self: Sized,
    {
        todo!()
    }

    fn worst_case_input_state(&self) -> crate::ExecutionState
    where
        Self: Sized,
    {
        todo!()
    }
}

impl Snippet for DummyTestSnippetB {
    fn stack_diff(&self) -> isize {
        2
    }

    fn entrypoint(&self) -> String {
        "tasm_b_dummy_test_value".to_string()
    }

    fn function_code(&self, library: &mut SnippetState) -> String {
        let entrypoint = self.entrypoint();
        let c = library.import(Box::new(DummyTestSnippetC));

        format!(
            "
                {entrypoint}:
                    call {c}
                    call {c}
                    return
                "
        )
    }

    fn rust_shadowing(
        &self,
        stack: &mut Vec<BFieldElement>,
        _std_in: Vec<BFieldElement>,
        _secret_in: Vec<BFieldElement>,
        _memory: &mut HashMap<BFieldElement, BFieldElement>,
    ) {
        stack.push(BFieldElement::one());
        stack.push(BFieldElement::one());
    }

    fn inputs(&self) -> Vec<String> {
        vec![]
    }

    fn outputs(&self) -> Vec<String> {
        vec!["1".to_string(), "1".to_string()]
    }

    fn input_types(&self) -> Vec<crate::snippet::DataType> {
        vec![]
    }

    fn output_types(&self) -> Vec<crate::snippet::DataType> {
        vec![DataType::BFE, DataType::BFE]
    }

    fn crash_conditions(&self) -> Vec<String> {
        vec![]
    }

    fn gen_input_states(&self) -> Vec<crate::ExecutionState> {
        vec![]
    }

    fn common_case_input_state(&self) -> crate::ExecutionState
    where
        Self: Sized,
    {
        todo!()
    }

    fn worst_case_input_state(&self) -> crate::ExecutionState
    where
        Self: Sized,
    {
        todo!()
    }
}

impl Snippet for DummyTestSnippetC {
    fn stack_diff(&self) -> isize {
        1
    }

    fn entrypoint(&self) -> String {
        "tasm_c_dummy_test_value".to_string()
    }

    fn function_code(&self, _library: &mut SnippetState) -> String {
        let entrypoint = self.entrypoint();

        format!(
            "
                {entrypoint}:
                    push 1
                    return
                "
        )
    }

    fn rust_shadowing(
        &self,
        stack: &mut Vec<BFieldElement>,
        _std_in: Vec<BFieldElement>,
        _secret_in: Vec<BFieldElement>,
        _memory: &mut HashMap<BFieldElement, BFieldElement>,
    ) {
        stack.push(BFieldElement::one())
    }

    fn inputs(&self) -> Vec<String> {
        vec![]
    }

    fn outputs(&self) -> Vec<String> {
        vec!["1".to_string()]
    }

    fn input_types(&self) -> Vec<crate::snippet::DataType> {
        vec![]
    }

    fn output_types(&self) -> Vec<crate::snippet::DataType> {
        vec![DataType::BFE]
    }

    fn crash_conditions(&self) -> Vec<String> {
        vec![]
    }

    fn gen_input_states(&self) -> Vec<crate::ExecutionState> {
        vec![]
    }

    fn common_case_input_state(&self) -> crate::ExecutionState
    where
        Self: Sized,
    {
        todo!()
    }

    fn worst_case_input_state(&self) -> crate::ExecutionState
    where
        Self: Sized,
    {
        todo!()
    }
}

#[cfg(test)]
pub mod library_tests {

    use std::collections::HashMap;

    use crate::get_init_tvm_stack;
    use crate::test_helpers::rust_tasm_equivalence_prop;

    use super::*;

    #[test]
    fn library_includes() {
        let empty_stack = get_init_tvm_stack();

        let expected = None;
        let _execution_result = rust_tasm_equivalence_prop::<DummyTestSnippetA>(
            &DummyTestSnippetA,
            &empty_stack,
            &[],
            &[],
            &mut HashMap::default(),
            0,
            expected,
        );
        let _execution_result = rust_tasm_equivalence_prop::<DummyTestSnippetB>(
            &DummyTestSnippetB,
            &empty_stack,
            &[],
            &[],
            &mut HashMap::default(),
            0,
            expected,
        );
        let _execution_result = rust_tasm_equivalence_prop::<DummyTestSnippetC>(
            &DummyTestSnippetC,
            &empty_stack,
            &[],
            &[],
            &mut HashMap::default(),
            0,
            expected,
        );
    }

    #[test]
    fn all_imports_as_instruction_lists() {
        let mut lib = SnippetState::default();
        lib.import(Box::new(DummyTestSnippetA));
        lib.import(Box::new(DummyTestSnippetA));
        lib.import(Box::new(DummyTestSnippetC));
        let _ret = lib.all_imports_as_instruction_lists();
    }
}
