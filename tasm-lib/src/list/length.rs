use std::collections::HashMap;

use triton_vm::prelude::*;

use crate::prelude::*;
use crate::traits::basic_snippet::Reviewer;
use crate::traits::basic_snippet::SignOffFingerprint;

/// ### Behavior
///
/// ```text
/// BEFORE: _ *list
/// AFTER:  _ list_length
/// ```
///
/// ### Preconditions
///
/// - the input argument is a pointer to a [`BFieldCodec`] encoded list
///
/// ### Postconditions
///
/// None.
//
// Todo: the return type used to be a u32. However, neither the tasm nor the
//   rust shadowing ever performed any range checks. Should the return type be
//   changed back to u32? If so, should range checks be introduced?
#[derive(Debug, Default, Copy, Clone, Eq, PartialEq, Hash)]
pub struct Length;

impl BasicSnippet for Length {
    fn parameters(&self) -> Vec<(DataType, String)> {
        vec![(DataType::VoidPointer, "*list".to_string())]
    }

    fn return_values(&self) -> Vec<(DataType, String)> {
        vec![(DataType::Bfe, "list_length".to_string())]
    }

    fn entrypoint(&self) -> String {
        "tasmlib_list_length".to_string()
    }

    fn code(&self, _: &mut Library) -> Vec<LabelledInstruction> {
        triton_asm! {
            // BEFORE: _ *list
            // AFTER:  _ list_length
            {self.entrypoint()}:
                read_mem 1
                pop 1
                return
        }
    }

    fn sign_offs(&self) -> HashMap<Reviewer, SignOffFingerprint> {
        let mut sign_offs = HashMap::new();
        sign_offs.insert(Reviewer("ferdinand"), 0x5d27afc762ace284.into());
        sign_offs
    }
}

#[cfg(test)]
mod tests {
    use rand::prelude::*;

    use super::*;
    use crate::rust_shadowing_helper_functions::list::insert_random_list;
    use crate::test_prelude::*;

    impl Accessor for Length {
        fn rust_shadow(
            &self,
            stack: &mut Vec<BFieldElement>,
            memory: &HashMap<BFieldElement, BFieldElement>,
        ) {
            let list_ptr = stack.pop().unwrap();
            let list_length = memory[&list_ptr];
            stack.push(list_length);
        }

        fn pseudorandom_initial_state(
            &self,
            seed: [u8; 32],
            _: Option<BenchmarkCase>,
        ) -> AccessorInitialState {
            let mut rng = StdRng::from_seed(seed);
            let list_ptr = rng.random();
            let list_len = rng.random_range(0..=1 << 10);

            let mut memory = HashMap::default();
            insert_random_list(&DataType::Digest, list_ptr, list_len, &mut memory);
            let stack = [self.init_stack_for_isolated_run(), vec![list_ptr]].concat();

            AccessorInitialState { stack, memory }
        }
    }

    #[test]
    fn rust_shadow() {
        ShadowedAccessor::new(Length).test();
    }
}

#[cfg(test)]
mod benches {
    use super::*;
    use crate::test_prelude::*;

    #[test]
    fn length_long_benchmark() {
        ShadowedAccessor::new(Length).bench();
    }
}
