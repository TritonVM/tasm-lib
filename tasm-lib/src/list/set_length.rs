use std::collections::HashMap;

use triton_vm::prelude::*;

use crate::prelude::*;
use crate::traits::basic_snippet::Reviewer;
use crate::traits::basic_snippet::SignOffFingerprint;

/// Set the length of a [`BFieldCodec`]-encoded list in memory.
///
/// This snippet does not perform any checks. It is to be considered “unsafe”.
///
/// ### Behavior
///
/// ```text
/// BEFORE: _ *list [list_length: u32]
/// AFTER:  _ *list
/// ```
///
/// ### Preconditions
///
/// - all input arguments are properly [`BFieldCodec`] encoded
/// - the input argument `*list` is a pointer to a [`BFieldCodec`] encoded list
///
/// ### Postconditions
///
/// None.
#[derive(Debug, Default, Copy, Clone, Eq, PartialEq, Hash)]
pub struct SetLength;

impl BasicSnippet for SetLength {
    fn parameters(&self) -> Vec<(DataType, String)> {
        vec![
            (DataType::VoidPointer, "*list".to_string()),
            (DataType::U32, "list_length".to_string()),
        ]
    }

    fn return_values(&self) -> Vec<(DataType, String)> {
        vec![(DataType::VoidPointer, "*list".to_string())]
    }

    fn entrypoint(&self) -> String {
        "tasmlib_list_set_length".to_string()
    }

    fn code(&self, _: &mut Library) -> Vec<LabelledInstruction> {
        triton_asm!(
            // BEFORE: _ *list list_length
            // AFTER:  _ *list
            {self.entrypoint()}:
                pick 1      // _ list_length *list
                write_mem 1 // _ (*list + 1)
                addi -1     // _ *list
                return
        )
    }

    fn sign_offs(&self) -> HashMap<Reviewer, SignOffFingerprint> {
        let mut sign_offs = HashMap::new();
        sign_offs.insert(Reviewer("ferdinand"), 0xff218b8ee1882c10.into());
        sign_offs
    }
}

#[cfg(test)]
pub(crate) mod tests {
    use super::*;
    use crate::U32_TO_USIZE_ERR;
    use crate::empty_stack;
    use crate::rust_shadowing_helper_functions::list::list_set_length;
    use crate::test_prelude::*;

    impl Function for SetLength {
        fn rust_shadow(
            &self,
            stack: &mut Vec<BFieldElement>,
            memory: &mut HashMap<BFieldElement, BFieldElement>,
        ) {
            let new_length = pop_encodable::<u32>(stack);
            let list_address = stack.pop().unwrap();
            stack.push(list_address);

            let new_length = new_length.try_into().expect(U32_TO_USIZE_ERR);
            list_set_length(list_address, new_length, memory);
        }

        fn pseudorandom_initial_state(
            &self,
            seed: [u8; 32],
            _: Option<BenchmarkCase>,
        ) -> FunctionInitialState {
            let mut rng = StdRng::from_seed(seed);

            let mut stack = empty_stack();
            stack.push(rng.random());
            stack.push(bfe!(rng.next_u32()));

            FunctionInitialState {
                stack,
                ..Default::default()
            }
        }
    }

    #[test]
    fn rust_shadow() {
        ShadowedFunction::new(SetLength).test();
    }
}

#[cfg(test)]
mod benches {
    use super::*;
    use crate::test_prelude::*;

    #[test]
    fn benchmark() {
        ShadowedFunction::new(SetLength).bench();
    }
}
