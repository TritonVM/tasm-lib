use std::collections::HashMap;

use triton_vm::prelude::*;

use crate::prelude::*;
use crate::traits::basic_snippet::Reviewer;
use crate::traits::basic_snippet::SignOffFingerprint;

/// Is the top of the stack an odd u32?
///
/// ### Behavior
///
/// ```text
/// BEFORE: _ [value: u32]
/// AFTER:  _ [is_odd: bool]
/// ```
///
/// ### Preconditions
///
/// - all input arguments are properly [`BFieldCodec`] encoded
///
/// ### Postconditions
///
/// - the output is properly [`BFieldCodec`] encoded
#[derive(Debug, Default, Copy, Clone, Eq, PartialEq, Hash)]
pub struct IsOdd;

impl BasicSnippet for IsOdd {
    fn parameters(&self) -> Vec<(DataType, String)> {
        vec![(DataType::U32, "value".to_string())]
    }

    fn return_values(&self) -> Vec<(DataType, String)> {
        vec![(DataType::Bool, "value % 2".to_string())]
    }

    fn entrypoint(&self) -> String {
        "tasmlib_arithmetic_u32_is_odd".to_string()
    }

    fn code(&self, _: &mut Library) -> Vec<LabelledInstruction> {
        triton_asm!(
            // BEFORE: _ value
            // AFTER:  _ (value % 2)
            {self.entrypoint()}:
                push 2
                pick 1
                div_mod
                pick 1
                pop 1
                return
        )
    }

    fn sign_offs(&self) -> HashMap<Reviewer, SignOffFingerprint> {
        let mut sign_offs = HashMap::new();
        sign_offs.insert(Reviewer("ferdinand"), 0xa573894027409e47.into());
        sign_offs
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_prelude::*;

    impl Closure for IsOdd {
        type Args = u32;

        fn rust_shadow(&self, stack: &mut Vec<BFieldElement>) {
            let v = pop_encodable::<Self::Args>(stack);
            let is_odd = v % 2 == 1;
            push_encodable(stack, &is_odd);
        }

        fn pseudorandom_args(
            &self,
            seed: [u8; 32],
            bench_case: Option<BenchmarkCase>,
        ) -> Self::Args {
            match bench_case {
                Some(BenchmarkCase::CommonCase) => 1 << 16,
                Some(BenchmarkCase::WorstCase) => u32::MAX,
                None => StdRng::from_seed(seed).random(),
            }
        }

        fn corner_case_args(&self) -> Vec<Self::Args> {
            (0..32).chain(u32::MAX - 32..=u32::MAX).collect_vec()
        }
    }

    #[test]
    fn rust_shadow() {
        ShadowedClosure::new(IsOdd).test();
    }
}

#[cfg(test)]
mod benches {
    use super::*;
    use crate::test_prelude::*;

    #[test]
    fn benchmark() {
        ShadowedClosure::new(IsOdd).bench();
    }
}
