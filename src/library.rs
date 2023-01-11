use std::collections::HashSet;

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

    pub fn with_preallocated_memory(words_allocated: usize) -> Self {
        Self {
            seen_snippets: HashSet::default(),
            function_bodies: HashSet::default(),
            free_pointer: words_allocated,
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
    pub fn import<T: Snippet>(&mut self) -> &'static str {
        let dep_entrypoint = T::entrypoint();
        if self.seen_snippets.insert(dep_entrypoint) {
            let dep_function_body = T::function_body(self);
            self.function_bodies.insert(dep_function_body);
        }

        T::entrypoint()
    }

    #[allow(dead_code)]
    pub fn all_imports(&self) -> String {
        self.function_bodies
            .iter()
            .map(|s| format!("{s}\n"))
            .collect()
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
    use crate::test_helpers::rust_tasm_equivalence_prop;

    use super::*;

    struct A();
    struct B();
    struct C();

    impl Snippet for A {
        fn stack_diff() -> isize {
            3
        }

        fn entrypoint() -> &'static str {
            "a"
        }

        fn function_body(library: &mut Library) -> String {
            let entrypoint = Self::entrypoint();
            let b = library.import::<B>();
            let c = library.import::<C>();

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
    }

    impl Snippet for B {
        fn stack_diff() -> isize {
            2
        }

        fn entrypoint() -> &'static str {
            "b"
        }

        fn function_body(library: &mut Library) -> String {
            let entrypoint = Self::entrypoint();
            let c = library.import::<C>();

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
    }

    impl Snippet for C {
        fn stack_diff() -> isize {
            1
        }

        fn entrypoint() -> &'static str {
            "c"
        }

        fn function_body(_library: &mut Library) -> String {
            let entrypoint = Self::entrypoint();

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
    }

    #[test]
    fn library_includes() {
        let empty_stack = get_init_tvm_stack();

        let expected = None;
        let _execution_result = rust_tasm_equivalence_prop::<A>(
            &empty_stack,
            &[],
            &[],
            &mut HashMap::default(),
            0,
            expected,
        );
        let _execution_result = rust_tasm_equivalence_prop::<B>(
            &empty_stack,
            &[],
            &[],
            &mut HashMap::default(),
            0,
            expected,
        );
        let _execution_result = rust_tasm_equivalence_prop::<C>(
            &empty_stack,
            &[],
            &[],
            &mut HashMap::default(),
            0,
            expected,
        );
    }
}
