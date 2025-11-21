use std::collections::HashMap;

use arbitrary::Arbitrary;
use itertools::Itertools;
use num_traits::ConstOne;
use triton_vm::memory_layout::MemoryRegion;
use triton_vm::prelude::*;

use crate::prelude::*;

/// By [convention](crate::memory), the last full memory page is reserved for the static allocator.
/// For convenience during [debugging],[^1] the static allocator starts at the last address of that
/// page, and grows downwards.
///
/// [^1]: and partly for historic reasons
///
/// [debugging]: crate::maybe_write_debuggable_vm_state_to_disk
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

/// Represents a [static memory allocation][kmalloc] within Triton VM.
/// Both its location within Triton VM's memory and its size and are fix.
///
/// [kmalloc]: Library::kmalloc
#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash, Arbitrary)]
pub struct StaticAllocation {
    write_address: BFieldElement,
    num_words: u32,
}

impl StaticAllocation {
    /// The address from which the allocated memory can be read.
    pub fn read_address(&self) -> BFieldElement {
        let offset = bfe!(self.num_words) - BFieldElement::ONE;
        self.write_address() + offset
    }

    /// The address to which the allocated memory can be written.
    pub fn write_address(&self) -> BFieldElement {
        self.write_address
    }

    /// The number of words allocated in this memory block.
    pub fn num_words(&self) -> u32 {
        self.num_words
    }
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

    /// Statically allocate `num_words` words of memory.
    ///
    /// # Panics
    ///
    /// Panics if
    /// - `num_words` is zero,
    /// - the total number of statically allocated words exceeds `u32::MAX`.
    pub fn kmalloc(&mut self, num_words: u32) -> StaticAllocation {
        assert!(num_words > 0, "must allocate a positive number of words");
        let write_address =
            STATIC_MEMORY_FIRST_ADDRESS - bfe!(self.num_allocated_words) - bfe!(num_words - 1);
        self.num_allocated_words = self
            .num_allocated_words
            .checked_add(num_words)
            .expect("Cannot allocate more that u32::MAX words through `kmalloc`.");

        StaticAllocation {
            write_address,
            num_words,
        }
    }
}

#[cfg(test)]
mod tests {
    use triton_vm::prelude::Program;
    use triton_vm::prelude::triton_asm;

    use super::*;
    use crate::mmr::calculate_new_peaks_from_leaf_mutation::MmrCalculateNewPeaksFromLeafMutationMtIndices;
    use crate::test_prelude::*;

    #[derive(Debug, Copy, Clone, BFieldCodec)]
    struct ZeroSizedType;

    #[derive(Debug)]
    struct DummyTestSnippetA;

    #[derive(Debug)]
    struct DummyTestSnippetB;

    #[derive(Debug)]
    struct DummyTestSnippetC;

    impl BasicSnippet for DummyTestSnippetA {
        fn parameters(&self) -> Vec<(DataType, String)> {
            vec![]
        }

        fn return_values(&self) -> Vec<(DataType, String)> {
            vec![(DataType::Xfe, "dummy".to_string())]
        }

        fn entrypoint(&self) -> String {
            "tasmlib_a_dummy_test_value".to_string()
        }

        fn code(&self, library: &mut Library) -> Vec<LabelledInstruction> {
            let b = library.import(Box::new(DummyTestSnippetB));
            let c = library.import(Box::new(DummyTestSnippetC));

            triton_asm!(
                {self.entrypoint()}:
                    call {b}
                    call {c}
                    return
            )
        }
    }

    impl BasicSnippet for DummyTestSnippetB {
        fn parameters(&self) -> Vec<(DataType, String)> {
            vec![]
        }

        fn return_values(&self) -> Vec<(DataType, String)> {
            ["1"; 2]
                .map(|name| (DataType::Bfe, name.to_string()))
                .to_vec()
        }

        fn entrypoint(&self) -> String {
            "tasmlib_b_dummy_test_value".to_string()
        }

        fn code(&self, library: &mut Library) -> Vec<LabelledInstruction> {
            let c = library.import(Box::new(DummyTestSnippetC));

            triton_asm!(
                {self.entrypoint()}:
                    call {c}
                    call {c}
                    return
            )
        }
    }

    impl BasicSnippet for DummyTestSnippetC {
        fn parameters(&self) -> Vec<(DataType, String)> {
            vec![]
        }

        fn return_values(&self) -> Vec<(DataType, String)> {
            vec![(DataType::Bfe, "1".to_string())]
        }

        fn entrypoint(&self) -> String {
            "tasmlib_c_dummy_test_value".to_string()
        }

        fn code(&self, _: &mut Library) -> Vec<LabelledInstruction> {
            triton_asm!({self.entrypoint()}: push 1 return)
        }
    }

    impl Closure for DummyTestSnippetA {
        type Args = ZeroSizedType;

        fn rust_shadow(&self, stack: &mut Vec<BFieldElement>) {
            push_encodable(stack, &xfe![[1, 1, 1]]);
        }

        fn pseudorandom_args(&self, _: [u8; 32], _: Option<BenchmarkCase>) -> Self::Args {
            ZeroSizedType
        }
    }

    impl Closure for DummyTestSnippetB {
        type Args = ZeroSizedType;

        fn rust_shadow(&self, stack: &mut Vec<BFieldElement>) {
            stack.push(bfe!(1));
            stack.push(bfe!(1));
        }

        fn pseudorandom_args(&self, _: [u8; 32], _: Option<BenchmarkCase>) -> Self::Args {
            ZeroSizedType
        }
    }

    impl Closure for DummyTestSnippetC {
        type Args = ZeroSizedType;

        fn rust_shadow(&self, stack: &mut Vec<BFieldElement>) {
            stack.push(bfe!(1));
        }

        fn pseudorandom_args(&self, _: [u8; 32], _: Option<BenchmarkCase>) -> Self::Args {
            ZeroSizedType
        }
    }

    #[test]
    fn library_includes() {
        ShadowedClosure::new(DummyTestSnippetA).test();
        ShadowedClosure::new(DummyTestSnippetB).test();
        ShadowedClosure::new(DummyTestSnippetC).test();
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

        let first_chunk = lib.kmalloc(1);
        assert_eq!(MINUS_TWO, first_chunk.write_address());

        let second_chunk = lib.kmalloc(7);
        assert_eq!(-bfe!(9), second_chunk.write_address());

        let third_chunk = lib.kmalloc(1000);
        assert_eq!(-bfe!(1009), third_chunk.write_address());
    }
}
