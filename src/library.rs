use std::collections::{HashMap, HashSet};

use itertools::Itertools;
use num::One;
use triton_opcodes::instruction::LabelledInstruction;
use triton_opcodes::parser::{parse, to_labelled};
use twenty_first::shared_math::b_field_element::BFieldElement;

use crate::pseudo::sub::Sub;
use crate::pseudo::{lsb::Lsb, neg::Neg};
use crate::snippet::{DataType, Snippet};

#[derive(Debug, Default)]
pub struct Library {
    seen_snippets: HashSet<&'static str>,
    function_bodies: HashSet<String>,
    free_pointer: usize,
}

impl Library {
    /// Create an empty library.
    #[allow(dead_code)]
    pub fn empty() -> Self {
        Self::default()
    }

    pub fn with_pseudo_instructions() -> Self {
        let mut library = Self::empty();
        library.import(Box::new(Lsb));
        library.import(Box::new(Neg));
        library.import(Box::new(Sub));
        library
    }

    pub fn with_preallocated_memory(words_allocated: usize) -> Self {
        let mut library = Self::with_pseudo_instructions();
        library.free_pointer = words_allocated;
        library
    }

    /// Import `T: Snippet` into the library.
    ///
    /// This recursively imports `T`'s dependencies.
    ///
    /// Does not import the snippets with the same entrypoint twice.
    ///
    /// Avoid cyclic dependencies by only calling `T::function_body()` which
    /// may call `.import()` if `.import::<T>()` wasn't already called once.
    pub fn import(&mut self, snippet: Box<dyn Snippet>) -> &'static str {
        let dep_entrypoint = snippet.entrypoint();
        if self.seen_snippets.insert(dep_entrypoint) {
            let dep_function_body = snippet.function_body(self);
            self.function_bodies.insert(dep_function_body);
        }

        snippet.entrypoint()
    }

    #[allow(dead_code)]
    pub fn all_imports(&self) -> String {
        self.function_bodies
            .iter()
            .map(|s| format!("{s}\n"))
            .collect()
    }

    pub fn all_imports_as_instruction_lists(&self) -> Vec<LabelledInstruction> {
        self.function_bodies
            .iter()
            .map(|x| to_labelled(&parse(x).unwrap()))
            .collect_vec()
            .concat()
    }

    pub fn kmalloc(&mut self, num_words: usize) -> usize {
        let address = self.free_pointer;
        self.free_pointer += num_words;
        address
    }
}

pub struct DummyTestSnippetA;
pub struct DummyTestSnippetB;
pub struct DummyTestSnippetC;

impl Snippet for DummyTestSnippetA {
    fn stack_diff() -> isize {
        3
    }

    fn entrypoint(&self) -> &'static str {
        "a_dummy_test_value"
    }

    fn function_body(&self, library: &mut Library) -> String {
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
        stack: &mut Vec<BFieldElement>,
        _std_in: Vec<BFieldElement>,
        _secret_in: Vec<BFieldElement>,
        _memory: &mut HashMap<BFieldElement, BFieldElement>,
    ) {
        stack.push(BFieldElement::one());
        stack.push(BFieldElement::one());
        stack.push(BFieldElement::one());
    }

    fn inputs() -> Vec<&'static str> {
        vec![]
    }

    fn outputs() -> Vec<&'static str> {
        vec!["1", "1", "1"]
    }

    fn crash_conditions() -> Vec<&'static str> {
        vec![]
    }

    fn gen_input_states() -> Vec<crate::ExecutionState> {
        vec![]
    }

    fn input_types(&self) -> Vec<crate::snippet::DataType> {
        vec![]
    }

    fn output_types(&self) -> Vec<crate::snippet::DataType> {
        vec![DataType::BFE, DataType::BFE, DataType::BFE]
    }
}

impl Snippet for DummyTestSnippetB {
    fn stack_diff() -> isize {
        2
    }

    fn entrypoint(&self) -> &'static str {
        "b_dummy_test_value"
    }

    fn function_body(&self, library: &mut Library) -> String {
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
        stack: &mut Vec<BFieldElement>,
        _std_in: Vec<BFieldElement>,
        _secret_in: Vec<BFieldElement>,
        _memory: &mut HashMap<BFieldElement, BFieldElement>,
    ) {
        stack.push(BFieldElement::one());
        stack.push(BFieldElement::one());
    }

    fn inputs() -> Vec<&'static str> {
        vec![]
    }

    fn outputs() -> Vec<&'static str> {
        vec!["1", "1"]
    }

    fn input_types(&self) -> Vec<crate::snippet::DataType> {
        vec![]
    }

    fn output_types(&self) -> Vec<crate::snippet::DataType> {
        vec![DataType::BFE, DataType::BFE]
    }

    fn crash_conditions() -> Vec<&'static str> {
        vec![]
    }

    fn gen_input_states() -> Vec<crate::ExecutionState> {
        vec![]
    }
}

impl Snippet for DummyTestSnippetC {
    fn stack_diff() -> isize {
        1
    }

    fn entrypoint(&self) -> &'static str {
        "c_dummy_test_value"
    }

    fn function_body(&self, _library: &mut Library) -> String {
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
        stack: &mut Vec<BFieldElement>,
        _std_in: Vec<BFieldElement>,
        _secret_in: Vec<BFieldElement>,
        _memory: &mut HashMap<BFieldElement, BFieldElement>,
    ) {
        stack.push(BFieldElement::one())
    }

    fn inputs() -> Vec<&'static str> {
        vec![]
    }

    fn outputs() -> Vec<&'static str> {
        vec!["1"]
    }

    fn input_types(&self) -> Vec<crate::snippet::DataType> {
        vec![]
    }

    fn output_types(&self) -> Vec<crate::snippet::DataType> {
        vec![DataType::BFE]
    }

    fn crash_conditions() -> Vec<&'static str> {
        vec![]
    }

    fn gen_input_states() -> Vec<crate::ExecutionState> {
        vec![]
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
            DummyTestSnippetA,
            &empty_stack,
            &[],
            &[],
            &mut HashMap::default(),
            0,
            expected,
        );
        let _execution_result = rust_tasm_equivalence_prop::<DummyTestSnippetB>(
            DummyTestSnippetB,
            &empty_stack,
            &[],
            &[],
            &mut HashMap::default(),
            0,
            expected,
        );
        let _execution_result = rust_tasm_equivalence_prop::<DummyTestSnippetC>(
            DummyTestSnippetC,
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
        let mut lib = Library::default();
        lib.import(Box::new(DummyTestSnippetA));
        lib.import(Box::new(DummyTestSnippetA));
        lib.import(Box::new(DummyTestSnippetC));
        let ret = lib.all_imports_as_instruction_lists();
        println!("ret = {}", ret.iter().map(|x| x.to_string()).join("\n"));
    }
}
