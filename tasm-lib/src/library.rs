use std::collections::HashMap;

use itertools::Itertools;
use num::One;
use triton_vm::memory_layout::MemoryRegion;
use triton_vm::prelude::*;

use crate::data_type::DataType;
use crate::traits::basic_snippet::BasicSnippet;
use crate::traits::deprecated_snippet::DeprecatedSnippet;

/// By [convention](crate::memory), the last full memory page is reserved for the static allocator.
/// For convenience during [debugging],[^1] the static allocator starts at the last address of that
/// page, and grows downwards.
///
/// [^1]: and partly for historic reasons
///
/// [debugging]: crate::maybe_write_debuggable_program_to_disk
const STATIC_MEMORY_FIRST_ADDRESS_AS_U64: u64 = BFieldElement::MAX - 1;
pub const STATIC_MEMORY_FIRST_ADDRESS: BFieldElement =
    BFieldElement::new(STATIC_MEMORY_FIRST_ADDRESS_AS_U64);
pub const STATIC_MEMORY_LAST_ADDRESS: BFieldElement =
    BFieldElement::new(STATIC_MEMORY_FIRST_ADDRESS_AS_U64 - u32::MAX as u64);

/// Represents a set of imports for a single Program or Snippet, and moreover tracks some data used
/// for initializing the [memory allocator](crate::memory).
#[derive(Clone, Debug)]
pub struct Library {
    /// Imported dependencies.
    seen_snippets: HashMap<String, Vec<LabelledInstruction>>,

    /// The number of statically allocated words
    num_allocated_words: u32,
}

impl Default for Library {
    fn default() -> Self {
        Self::new()
    }
}

impl Library {
    pub fn kmalloc_memory_region() -> MemoryRegion {
        MemoryRegion::new(STATIC_MEMORY_LAST_ADDRESS, 1usize << 32)
    }

    pub fn new() -> Self {
        Self {
            seen_snippets: HashMap::default(),
            num_allocated_words: 0,
        }
    }

    /// Create an empty library.
    pub fn empty() -> Self {
        Self::new()
    }

    #[cfg(test)]
    pub fn with_preallocated_memory(words_statically_allocated: u32) -> Self {
        Library {
            num_allocated_words: words_statically_allocated,
            ..Self::new()
        }
    }

    /// Import `T: Snippet`.
    ///
    /// Recursively imports `T`'s dependencies.
    /// Does not import the snippets with the same entrypoint twice.
    ///
    /// Avoid cyclic dependencies by only calling `T::function_code()` which
    /// may call `.import()` if `.import::<T>()` wasn't already called once.
    // todo: Above comment is not overly clear. Improve it.
    pub fn import(&mut self, snippet: Box<dyn BasicSnippet>) -> String {
        let dep_entrypoint = snippet.entrypoint();

        let is_new_dependency = !self.seen_snippets.contains_key(&dep_entrypoint);
        if is_new_dependency {
            let dep_body = snippet.annotated_code(self);
            self.seen_snippets.insert(dep_entrypoint.clone(), dep_body);
        }

        dep_entrypoint
    }

    /// Import code that does not implement the `Snippet` trait
    ///
    /// If possible, you should use the [`import`](Self::import) method as
    /// it gives better protections and allows you to test functions in
    /// isolation. This method is intended to add function to the assembly
    /// that you have defined inline and where a function call is needed due to
    /// e.g. a dynamic counter.
    pub fn explicit_import(&mut self, name: &str, body: &[LabelledInstruction]) -> String {
        if !self.seen_snippets.contains_key(name) {
            self.seen_snippets.insert(name.to_owned(), body.to_vec());
        }

        name.to_string()
    }

    /// Return a list of all external dependencies sorted by name. All snippets are sorted
    /// alphabetically to ensure that generated programs are deterministic.
    pub fn all_external_dependencies(&self) -> Vec<Vec<LabelledInstruction>> {
        self.seen_snippets
            .iter()
            .sorted_by_key(|(k, _)| *k)
            .map(|(_, code)| code.clone())
            .collect()
    }

    /// Return the name of all imported snippets, sorted alphabetically to ensure that output is
    /// deterministic.
    pub fn get_all_snippet_names(&self) -> Vec<String> {
        let mut ret = self.seen_snippets.keys().cloned().collect_vec();
        ret.sort_unstable();
        ret
    }

    /// Return a list of instructions containing all imported snippets.
    pub fn all_imports(&self) -> Vec<LabelledInstruction> {
        self.all_external_dependencies().concat()
    }

    /// Statically allocate `num_words` words of memory. Panics if more static
    /// memory is required than what the capacity allows for.
    pub fn kmalloc(&mut self, num_words: u32) -> BFieldElement {
        assert!(num_words > 0, "must allocate a positive number of words");
        let address = STATIC_MEMORY_FIRST_ADDRESS
            - bfe!(self.num_allocated_words)
            - BFieldElement::new(num_words as u64 - 1);
        self.num_allocated_words = self
            .num_allocated_words
            .checked_add(num_words)
            .expect("Cannot allocate more that u32::MAX words through `kmalloc`.");

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
    fn entrypoint_name(&self) -> String {
        "tasmlib_a_dummy_test_value".to_string()
    }

    fn input_field_names(&self) -> Vec<String> {
        vec![]
    }

    fn input_types(&self) -> Vec<DataType> {
        vec![]
    }

    fn output_field_names(&self) -> Vec<String> {
        vec!["1".to_string(), "1".to_string(), "1".to_string()]
    }

    fn output_types(&self) -> Vec<DataType> {
        vec![DataType::Bfe, DataType::Bfe, DataType::Bfe]
    }

    fn stack_diff(&self) -> isize {
        3
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

    fn crash_conditions(&self) -> Vec<String> {
        vec![]
    }

    fn gen_input_states(&self) -> Vec<crate::InitVmState> {
        vec![]
    }

    fn common_case_input_state(&self) -> crate::InitVmState {
        todo!()
    }

    fn worst_case_input_state(&self) -> crate::InitVmState {
        todo!()
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
}

impl DeprecatedSnippet for DummyTestSnippetB {
    fn entrypoint_name(&self) -> String {
        "tasmlib_b_dummy_test_value".to_string()
    }

    fn input_field_names(&self) -> Vec<String> {
        vec![]
    }

    fn input_types(&self) -> Vec<DataType> {
        vec![]
    }

    fn output_field_names(&self) -> Vec<String> {
        vec!["1".to_string(), "1".to_string()]
    }

    fn output_types(&self) -> Vec<DataType> {
        vec![DataType::Bfe, DataType::Bfe]
    }

    fn stack_diff(&self) -> isize {
        2
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

    fn crash_conditions(&self) -> Vec<String> {
        vec![]
    }

    fn gen_input_states(&self) -> Vec<crate::InitVmState> {
        vec![]
    }

    fn common_case_input_state(&self) -> crate::InitVmState {
        todo!()
    }

    fn worst_case_input_state(&self) -> crate::InitVmState {
        todo!()
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
}

impl DeprecatedSnippet for DummyTestSnippetC {
    fn entrypoint_name(&self) -> String {
        "tasmlib_c_dummy_test_value".to_string()
    }

    fn input_field_names(&self) -> Vec<String> {
        vec![]
    }

    fn input_types(&self) -> Vec<DataType> {
        vec![]
    }

    fn output_field_names(&self) -> Vec<String> {
        vec!["1".to_string()]
    }

    fn output_types(&self) -> Vec<DataType> {
        vec![DataType::Bfe]
    }

    fn stack_diff(&self) -> isize {
        1
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

    fn crash_conditions(&self) -> Vec<String> {
        vec![]
    }

    fn gen_input_states(&self) -> Vec<crate::InitVmState> {
        vec![]
    }

    fn common_case_input_state(&self) -> crate::InitVmState {
        todo!()
    }

    fn worst_case_input_state(&self) -> crate::InitVmState {
        todo!()
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
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use triton_vm::prelude::triton_asm;
    use triton_vm::prelude::Program;

    use crate::empty_stack;
    use crate::memory::memcpy::MemCpy;
    use crate::mmr::calculate_new_peaks_from_leaf_mutation::MmrCalculateNewPeaksFromLeafMutationMtIndices;
    use crate::test_helpers::test_rust_equivalence_given_input_values_deprecated;

    use super::*;

    #[test]
    fn library_includes() {
        let empty_stack = empty_stack();

        let expected = None;
        test_rust_equivalence_given_input_values_deprecated(
            &DummyTestSnippetA,
            &empty_stack,
            &[],
            HashMap::default(),
            expected,
        );
        test_rust_equivalence_given_input_values_deprecated(
            &DummyTestSnippetB,
            &empty_stack,
            &[],
            HashMap::default(),
            expected,
        );
        test_rust_equivalence_given_input_values_deprecated(
            &DummyTestSnippetC,
            &empty_stack,
            &[],
            HashMap::default(),
            expected,
        );
    }

    #[test]
    fn get_all_snippet_names_test_a() {
        let mut lib = Library::new();
        lib.import(Box::new(DummyTestSnippetA));
        assert_eq!(
            vec![
                "tasmlib_a_dummy_test_value",
                "tasmlib_b_dummy_test_value",
                "tasmlib_c_dummy_test_value",
            ],
            lib.get_all_snippet_names()
        );
    }

    #[test]
    fn get_all_snippet_names_test_b() {
        let mut lib = Library::new();
        lib.import(Box::new(DummyTestSnippetB));
        assert_eq!(
            vec!["tasmlib_b_dummy_test_value", "tasmlib_c_dummy_test_value"],
            lib.get_all_snippet_names()
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
                library.import(Box::new(MmrCalculateNewPeaksFromLeafMutationMtIndices));

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
        const MINUS_TWO: BFieldElement = BFieldElement::new(BFieldElement::MAX - 1);
        let mut lib = Library::new();

        let first_free_address = lib.kmalloc(1);
        assert_eq!(MINUS_TWO, first_free_address);

        let second_free_address = lib.kmalloc(7);
        assert_eq!(-BFieldElement::new(9), second_free_address,);

        let third_free_address = lib.kmalloc(1000);
        assert_eq!(-BFieldElement::new(1009), third_free_address);
    }
}
