use std::collections::HashMap;

use triton_vm::prelude::*;

use crate::prelude::*;
use crate::traits::basic_snippet::Reviewer;
use crate::traits::basic_snippet::SignOffFingerprint;

/// Is the top of the stack a u32?
///
/// ### Behavior
///
/// ```text
/// BEFORE: _ value
/// AFTER:  _ [is_u32: bool]
/// ```
///
/// ### Preconditions
///
/// None.
///
/// ### Postconditions
///
/// - the output is properly [`BFieldCodec`] encoded
#[derive(Debug, Default, Copy, Clone, Eq, PartialEq, Hash)]
pub struct IsU32;

impl BasicSnippet for IsU32 {
    fn parameters(&self) -> Vec<(DataType, String)> {
        vec![(DataType::U32, "value".to_string())]
    }

    fn return_values(&self) -> Vec<(DataType, String)> {
        vec![(DataType::Bool, "value < 2^32".to_string())]
    }

    fn entrypoint(&self) -> String {
        "tasmlib_arithmetic_u32_is_u32".to_string()
    }

    fn code(&self, _: &mut Library) -> Vec<LabelledInstruction> {
        triton_asm!(
            {self.entrypoint()}:
                        // _ a
                split   // _ hi lo
                pop 1   // _ hi
                push 0  // _ hi 0
                eq      // _ (hi == 0)
                return
        )
    }

    fn sign_offs(&self) -> HashMap<Reviewer, SignOffFingerprint> {
        let mut sign_offs = HashMap::new();
        sign_offs.insert(Reviewer("ferdinand"), 0x7eeef18dd91aa7a7.into());
        sign_offs
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_prelude::*;

    impl Closure for IsU32 {
        type Args = BFieldElement;

        fn rust_shadow(&self, stack: &mut Vec<BFieldElement>) {
            let value = stack.pop().unwrap();
            let is_u32 = u32::try_from(value).is_ok();
            push_encodable(stack, &is_u32);
        }

        fn pseudorandom_args(
            &self,
            seed: [u8; 32],
            bench_case: Option<BenchmarkCase>,
        ) -> Self::Args {
            match bench_case {
                Some(BenchmarkCase::CommonCase) => bfe!(1 << 16),
                Some(BenchmarkCase::WorstCase) => bfe!(u32::MAX),
                None => StdRng::from_seed(seed).random(),
            }
        }

        fn corner_case_args(&self) -> Vec<Self::Args> {
            bfe_vec![0, u32::MAX, u64::from(u32::MAX) + 1, BFieldElement::MAX]
        }
    }

    #[test]
    fn rust_shadow() {
        ShadowedClosure::new(IsU32).test();
    }
}

#[cfg(test)]
mod benches {
    use super::*;
    use crate::test_prelude::*;

    #[test]
    fn is_u32_benchmark() {
        ShadowedClosure::new(IsU32).bench();
    }
}
