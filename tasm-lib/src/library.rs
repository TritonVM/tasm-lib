use std::collections::HashMap;

use itertools::Itertools;
use num::One;
use triton_vm::instruction::LabelledInstruction;
use twenty_first::shared_math::b_field_element::BFieldElement;

use crate::snippet::{BasicSnippet, DataType, DeprecatedSnippet};

// Ensure that static allocator does not overwrite the address
// dedicated to the dynamic allocator. Dynamic allocator is,
// by convention, always on address 0.
pub const STATIC_MEMORY_START_ADDRESS: usize = 1;

/// A Library represents a set of imports for a single Program or Snippet, and moreover
/// tracks some data used for initializing the memory allocator.
#[derive(Clone, Debug)]
pub struct Library {
    seen_snippets: HashMap<String, Vec<LabelledInstruction>>,
    free_pointer: usize,
}

impl Default for Library {
    fn default() -> Self {
        Self::new()
    }
}

impl Library {
    pub fn new() -> Self {
        Self {
            seen_snippets: Default::default(),
            free_pointer: STATIC_MEMORY_START_ADDRESS,
        }
    }

    /// Create an empty library.
    #[allow(dead_code)]
    pub fn empty() -> Self {
        Self::new()
    }

    pub fn with_preallocated_memory(words_allocated: usize) -> Self {
        Library {
            free_pointer: words_allocated + STATIC_MEMORY_START_ADDRESS,
            ..Self::new()
        }
    }

    /// Import `T: Snippet` into the library.
    ///
    /// This recursively imports `T`'s dependencies.
    ///
    /// Does not import the snippets with the same entrypoint twice.
    ///
    /// Avoid cyclic dependencies by only calling `T::function_code()` which
    /// may call `.import()` if `.import::<T>()` wasn't already called once.
    pub fn import(&mut self, snippet: Box<dyn BasicSnippet>) -> String {
        let dep_entrypoint = snippet.entrypoint();

        // The linter's suggestion doesn't work. This suppression is fine imo.
        #[allow(clippy::map_entry)]
        if !self.seen_snippets.contains_key(&dep_entrypoint) {
            let dep_body = snippet.code(self);
            self.seen_snippets.insert(dep_entrypoint, dep_body);
        }

        snippet.entrypoint()
    }

    pub fn explicit_import(&mut self, name: &str, body: &[LabelledInstruction]) -> String {
        // The linter's suggestion doesn't work. This suppression is fine imo.
        #[allow(clippy::map_entry)]
        if !self.seen_snippets.contains_key(name) {
            self.seen_snippets.insert(name.to_owned(), body.to_vec());
        }

        name.to_string()
    }

    /// Return the next free address without allocating anything
    pub fn get_next_free_address(&self) -> usize {
        self.free_pointer
    }

    // Return a list of all external dependencies sorted by name
    // All snippets are sorted
    // alphabetically to ensure that generated programs are deterministic.
    pub fn all_external_dependencies(&self) -> Vec<Vec<LabelledInstruction>> {
        self.seen_snippets
            .iter()
            .sorted_by_key(|(k, _)| *k)
            .map(|(_, code)| code.clone())
            .collect()
    }

    #[allow(dead_code)]
    pub fn all_imports(&self) -> Vec<LabelledInstruction> {
        // Collect all imports and return. All snippets are sorted
        // alphabetically to ensure that generated programs are deterministic.
        self.all_external_dependencies().concat()
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

impl DeprecatedSnippet for DummyTestSnippetA {
    fn stack_diff(&self) -> isize {
        3
    }

    fn entrypoint_name(&self) -> String {
        "tasm_a_dummy_test_value".to_string()
    }

    fn function_code(&self, library: &mut Library) -> String {
        let entrypoint = self.entrypoint_name();
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

    fn input_field_names(&self) -> Vec<String> {
        vec![]
    }

    fn output_field_names(&self) -> Vec<String> {
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

    fn common_case_input_state(&self) -> crate::ExecutionState {
        todo!()
    }

    fn worst_case_input_state(&self) -> crate::ExecutionState {
        todo!()
    }
}

impl DeprecatedSnippet for DummyTestSnippetB {
    fn stack_diff(&self) -> isize {
        2
    }

    fn entrypoint_name(&self) -> String {
        "tasm_b_dummy_test_value".to_string()
    }

    fn function_code(&self, library: &mut Library) -> String {
        let entrypoint = self.entrypoint_name();
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

    fn input_field_names(&self) -> Vec<String> {
        vec![]
    }

    fn output_field_names(&self) -> Vec<String> {
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

    fn common_case_input_state(&self) -> crate::ExecutionState {
        todo!()
    }

    fn worst_case_input_state(&self) -> crate::ExecutionState {
        todo!()
    }
}

impl DeprecatedSnippet for DummyTestSnippetC {
    fn stack_diff(&self) -> isize {
        1
    }

    fn entrypoint_name(&self) -> String {
        "tasm_c_dummy_test_value".to_string()
    }

    fn function_code(&self, _library: &mut Library) -> String {
        let entrypoint = self.entrypoint_name();

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

    fn input_field_names(&self) -> Vec<String> {
        vec![]
    }

    fn output_field_names(&self) -> Vec<String> {
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

    fn common_case_input_state(&self) -> crate::ExecutionState {
        todo!()
    }

    fn worst_case_input_state(&self) -> crate::ExecutionState {
        todo!()
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;
    use triton_vm::program::Program;
    use triton_vm::triton_asm;

    use crate::get_init_tvm_stack;
    use crate::list::ListType;
    use crate::memory::memcpy::MemCpy;
    use crate::mmr::calculate_new_peaks_from_leaf_mutation::MmrCalculateNewPeaksFromLeafMutationMtIndices;
    use crate::test_helpers::test_rust_equivalence_given_input_values_deprecated;

    use super::*;

    #[test]
    fn library_includes() {
        let empty_stack = get_init_tvm_stack();

        let expected = None;
        test_rust_equivalence_given_input_values_deprecated(
            &DummyTestSnippetA,
            &empty_stack,
            &[],
            &mut HashMap::default(),
            0,
            expected,
        );
        test_rust_equivalence_given_input_values_deprecated(
            &DummyTestSnippetB,
            &empty_stack,
            &[],
            &mut HashMap::default(),
            0,
            expected,
        );
        test_rust_equivalence_given_input_values_deprecated(
            &DummyTestSnippetC,
            &empty_stack,
            &[],
            &mut HashMap::default(),
            0,
            expected,
        );
    }

    #[test]
    fn all_imports_as_instruction_lists() {
        let mut lib = Library::new();
        lib.import(Box::new(DummyTestSnippetA));
        lib.import(Box::new(DummyTestSnippetA));
        lib.import(Box::new(DummyTestSnippetC));
        let _ret = lib.all_imports();
    }

    #[test]
    fn program_is_deterministic() {
        // Ensure that a generated program is deterministic, by checking that the imports
        // are always sorted the same way.
        fn smaller_program() -> Program {
            let mut library = Library::new();
            let memcpy = library.import(Box::new(MemCpy));
            let calculate_new_peaks_from_leaf_mutation =
                library.import(Box::new(MmrCalculateNewPeaksFromLeafMutationMtIndices {
                    list_type: ListType::Safe,
                }));

            let code = triton_asm!(
                lala_entrypoint:
                    push 1 call {memcpy}
                    call {calculate_new_peaks_from_leaf_mutation}

                    return
            );

            let mut src = code;
            let mut imports = library.all_imports();

            // Sanity check on `all_external_dependencies`, checking that they are
            // *also* sorted alphabetically.
            let all_ext_deps = library.all_external_dependencies();
            let imports_repeated = all_ext_deps.concat();
            assert_eq!(imports, imports_repeated);

            src.append(&mut imports);

            Program::new(&src)
        }

        for _ in 0..100 {
            let program = smaller_program();
            let same_program = smaller_program();
            assert_eq!(program, same_program);
        }
    }

    #[test]
    fn kmalloc_test() {
        let mut lib = Library::new();
        assert_eq!(1, lib.get_next_free_address());

        // allocate 1 word and verify that 1 is returned, and that the next free address is 2
        let first_free_address = lib.kmalloc(1);
        assert_eq!(1, first_free_address);
        assert_eq!(2, lib.get_next_free_address());

        // allocate 7 words and verify that 2 is returned, and that the next free address
        // is 9.
        let second_free_address = lib.kmalloc(7);
        assert_eq!(2, second_free_address);
        assert_eq!(9, lib.get_next_free_address());

        // Allocate 1000 words.
        let third_free_address = lib.kmalloc(1000);
        assert_eq!(9, third_free_address);
        assert_eq!(1009, lib.get_next_free_address());
    }
}
