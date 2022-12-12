use std::collections::HashSet;

use crate::snippet_trait::Snippet;

#[derive(Debug, Default)]
pub struct Library {
    seen_snippets: HashSet<&'static str>,
    function_bodies: HashSet<String>,
}

impl Library {
    /// Create an empty library.
    #[allow(dead_code)]
    pub fn empty() -> Self {
        Self::default()
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
}

#[cfg(test)]
mod tests {
    use num::One;
    use twenty_first::shared_math::b_field_element::BFieldElement;

    use super::*;

    struct A();
    struct B();
    struct C();

    impl Snippet for A {
        fn new() -> Self {
            Self()
        }

        fn stack_diff() -> isize {
            B::stack_diff() + C::stack_diff()
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
        ) {
            stack.push(BFieldElement::one());
            stack.push(BFieldElement::one());
            stack.push(BFieldElement::one());
        }
    }

    impl Snippet for B {
        fn new() -> Self {
            Self()
        }

        fn stack_diff() -> isize {
            C::stack_diff()
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
        ) {
            stack.push(BFieldElement::one());
            stack.push(BFieldElement::one());
        }
    }

    impl Snippet for C {
        fn new() -> Self {
            Self()
        }

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
        ) {
            stack.push(BFieldElement::one())
        }
    }

    #[test]
    fn library_includes() {}
}
