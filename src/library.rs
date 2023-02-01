use std::collections::HashSet;

use itertools::Itertools;
use triton_opcodes::instruction::{parse, LabelledInstruction};

use crate::pseudo::sub::Sub;
use crate::pseudo::{lsb::Lsb, neg::Neg};
use crate::snippet::Snippet;

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
        library.import::<Lsb>(Lsb);
        library.import::<Neg>(Neg);
        library.import::<Sub>(Sub);
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
    pub fn import<T: Snippet>(&mut self, snippet: T) -> &'static str {
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
            .map(|x| parse(x).unwrap())
            .collect_vec()
            .concat()
    }

    pub fn kmalloc(&mut self, num_words: usize) -> usize {
        let address = self.free_pointer;
        self.free_pointer += num_words;
        address
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use num::One;
    use twenty_first::shared_math::b_field_element::BFieldElement;

    use crate::get_init_tvm_stack;
    use crate::snippet::DataType;
    use crate::test_helpers::rust_tasm_equivalence_prop;

    use super::*;

    struct A;
    struct B;
    struct C;

    impl Snippet for A {
        fn stack_diff() -> isize {
            3
        }

        fn entrypoint(&self) -> &'static str {
            "a"
        }

        fn function_body(&self, library: &mut Library) -> String {
            let entrypoint = self.entrypoint();
            let b = library.import::<B>(B);
            let c = library.import::<C>(C);

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

    impl Snippet for B {
        fn stack_diff() -> isize {
            2
        }

        fn entrypoint(&self) -> &'static str {
            "b"
        }

        fn function_body(&self, library: &mut Library) -> String {
            let entrypoint = self.entrypoint();
            let c = library.import::<C>(C);

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

    impl Snippet for C {
        fn stack_diff() -> isize {
            1
        }

        fn entrypoint(&self) -> &'static str {
            "c"
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

    #[test]
    fn library_includes() {
        let empty_stack = get_init_tvm_stack();

        let expected = None;
        let _execution_result = rust_tasm_equivalence_prop::<A>(
            A,
            &empty_stack,
            &[],
            &[],
            &mut HashMap::default(),
            0,
            expected,
        );
        let _execution_result = rust_tasm_equivalence_prop::<B>(
            B,
            &empty_stack,
            &[],
            &[],
            &mut HashMap::default(),
            0,
            expected,
        );
        let _execution_result = rust_tasm_equivalence_prop::<C>(
            C,
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
        lib.import::<A>(A);
        lib.import::<A>(A);
        lib.import::<C>(C);
        let ret = lib.all_imports_as_instruction_lists();
        println!("ret = {}", ret.iter().map(|x| x.to_string()).join("\n"));
    }
}
