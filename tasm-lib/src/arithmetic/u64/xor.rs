use std::collections::HashMap;

use triton_vm::prelude::*;

use crate::prelude::*;
use crate::traits::basic_snippet::Reviewer;
use crate::traits::basic_snippet::SignOffFingerprint;

/// [Bitwise xor][bitxor] of two `u64`s.
///
/// ### Behavior
///
/// ```text
/// BEFORE: _ [right: u64] [left: u64]
/// AFTER:  _ [xor: u64]
/// ```
///
/// ### Preconditions
///
/// - all input arguments are properly [`BFieldCodec`] encoded
///
/// ### Postconditions
///
/// - the output is the bitwise “xor” of the input
/// - the output is properly [`BFieldCodec`] encoded
///
/// [bitxor]: core::ops::BitXor
#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub struct Xor;

impl BasicSnippet for Xor {
    fn parameters(&self) -> Vec<(DataType, String)> {
        ["right", "left"]
            .map(|side| (DataType::U64, side.to_string()))
            .to_vec()
    }

    fn return_values(&self) -> Vec<(DataType, String)> {
        vec![(DataType::U64, "xor".to_string())]
    }

    fn entrypoint(&self) -> String {
        "tasmlib_arithmetic_u64_xor".to_string()
    }

    fn code(&self, _: &mut Library) -> Vec<LabelledInstruction> {
        triton_asm!(
            // BEFORE: _ rhs_hi rhs_lo lhs_hi lhs_lo
            // AFTER:  _ (rhs ^ lhs)_hi (rhs ^ lhs)_lo
            {self.entrypoint()}:
                swap 3
                xor
                // _ lhs_lo rhs_lo (lhs_hi ^ rhs_hi)

                swap 2
                xor
                // _ (lhs_hi ^ rhs_hi) (lhs_lo ^ rhs_lo)
                // _ (lhs ^ rhs)_hi    (lhs ^ rhs)_lo

                return
        )
    }

    fn sign_offs(&self) -> HashMap<Reviewer, SignOffFingerprint> {
        let mut sign_offs = HashMap::new();
        sign_offs.insert(Reviewer("ferdinand"), 0x72c3fab42b063e55.into());
        sign_offs
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_prelude::*;

    impl Closure for Xor {
        type Args = (u64, u64);

        fn rust_shadow(&self, stack: &mut Vec<BFieldElement>) {
            let (right, left) = pop_encodable::<Self::Args>(stack);
            push_encodable(stack, &(left ^ right));
        }

        fn pseudorandom_args(
            &self,
            seed: [u8; 32],
            bench_case: Option<BenchmarkCase>,
        ) -> Self::Args {
            match bench_case {
                Some(BenchmarkCase::CommonCase) => (0x7fff_ffff, 0x3ff),
                Some(BenchmarkCase::WorstCase) => (0x8000_0000_8000_0000, 0x4000_0000_8000_000a),
                None => StdRng::from_seed(seed).random(),
            }
        }
    }

    #[test]
    fn rust_shadow() {
        ShadowedClosure::new(Xor).test();
    }
}

#[cfg(test)]
mod benches {
    use super::*;
    use crate::test_prelude::*;

    #[test]
    fn benchmark() {
        ShadowedClosure::new(Xor).bench();
    }
}
