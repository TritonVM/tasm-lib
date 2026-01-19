use std::collections::HashMap;

use triton_vm::prelude::*;

use crate::prelude::*;
use crate::traits::basic_snippet::Reviewer;
use crate::traits::basic_snippet::SignOffFingerprint;

/// [Bitwise “or”][bitor] (“`|`”) for `u64`s.
///
/// ### Behavior
///
/// ```text
/// BEFORE: _ [right: u64] [left: u64]
/// AFTER:  _ [left | right: u64]
/// ```
///
/// ### Preconditions
///
/// - all input arguments are properly [`BFieldCodec`] encoded
///
/// ### Postconditions
///
/// - the output is the bitwise “or” of the input
/// - the output is properly [`BFieldCodec`] encoded
///
/// [bitor]: core::ops::BitOr
#[derive(Debug, Default, Copy, Clone, Eq, PartialEq, Hash)]
pub struct Or;

impl BasicSnippet for Or {
    fn parameters(&self) -> Vec<(DataType, String)> {
        ["right", "left"]
            .map(|s| (DataType::U64, s.to_string()))
            .to_vec()
    }

    fn return_values(&self) -> Vec<(DataType, String)> {
        vec![(DataType::U64, "left | right".to_string())]
    }

    fn entrypoint(&self) -> String {
        "tasmlib_arithmetic_u64_or".to_string()
    }

    fn code(&self, _: &mut Library) -> Vec<LabelledInstruction> {
        let entrypoint = self.entrypoint();
        triton_asm!(
            // BEFORE: _ r_hi r_lo l_hi l_lo
            // AFTER:  _ (l | r)_hi (l | r)_lo
            {entrypoint}:
                dup 2
                dup 1
                xor         // _ r_hi r_lo l_hi l_lo (r_lo ^ l_lo)
                swap 3
                and         // _ r_hi (r_lo ^ l_lo) l_hi (l_lo & r_lo)
                swap 3      // _ (l_lo & r_lo) (r_lo ^ l_lo) l_hi r_hi
                dup 1
                dup 1
                xor         // _ (l_lo & r_lo) (r_lo ^ l_lo) l_hi r_hi (l_hi ^ r_hi)
                place 2
                and         // _ (l_lo & r_lo) (r_lo ^ l_lo) (l_hi ^ r_hi) (r_hi & l_hi)
                add         // _ (l_lo & r_lo) (r_lo ^ l_lo) (l_hi | r_hi)
                place 2
                add         // _ (l_hi | r_hi) (r_lo | l_lo)
                return
        )
    }

    fn sign_offs(&self) -> HashMap<Reviewer, SignOffFingerprint> {
        let mut sign_offs = HashMap::new();
        sign_offs.insert(Reviewer("ferdinand"), 0x56e1a169cdc00179.into());
        sign_offs
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::arithmetic::u64::and::And;
    use crate::test_prelude::*;

    impl Closure for Or {
        type Args = (u64, u64);

        fn rust_shadow(&self, stack: &mut Vec<BFieldElement>) {
            let (right, left) = pop_encodable::<Self::Args>(stack);
            let or = left | right;
            push_encodable(stack, &or);
        }

        fn pseudorandom_args(
            &self,
            seed: [u8; 32],
            bench_case: Option<BenchmarkCase>,
        ) -> Self::Args {
            match bench_case {
                Some(BenchmarkCase::CommonCase) => (u32::MAX.into(), u32::MAX.into()),
                Some(BenchmarkCase::WorstCase) => (u64::MAX, u64::MAX),
                None => StdRng::from_seed(seed).random(),
            }
        }

        fn corner_case_args(&self) -> Vec<Self::Args> {
            And.corner_case_args()
        }
    }

    #[test]
    fn rust_shadow() {
        ShadowedClosure::new(Or).test();
    }
}

#[cfg(test)]
mod benches {
    use super::*;
    use crate::test_prelude::*;

    #[test]
    fn benchmark() {
        ShadowedClosure::new(Or).bench();
    }
}
