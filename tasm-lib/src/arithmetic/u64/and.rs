use std::collections::HashMap;

use triton_vm::prelude::*;

use crate::prelude::*;
use crate::traits::basic_snippet::Reviewer;
use crate::traits::basic_snippet::SignOffFingerprint;

/// [Bitwise “and”](u64::bitand) (“`&`”) for `u64`s.
///
/// ### Behavior
///
/// ```text
/// BEFORE: _ [right: u64] [left: u64]
/// AFTER:  _ [right & left: u64]
/// ```
///
/// ### Preconditions
///
/// - all input arguments are properly [`BFieldCodec`] encoded
///
/// ### Postconditions
///
/// - the output is the bitwise “and” of the input
/// - the output is properly [`BFieldCodec`] encoded
#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub struct And;

impl BasicSnippet for And {
    fn inputs(&self) -> Vec<(DataType, String)> {
        ["rhs", "lhs"]
            .map(|side| (DataType::U64, side.to_string()))
            .to_vec()
    }

    fn outputs(&self) -> Vec<(DataType, String)> {
        vec![(DataType::U64, "(lhs & rhs)".to_string())]
    }

    fn entrypoint(&self) -> String {
        "tasmlib_arithmetic_u64_and".to_string()
    }

    fn code(&self, _: &mut Library) -> Vec<LabelledInstruction> {
        triton_asm!(
            // BEFORE: _ rhs_hi rhs_lo lhs_hi lhs_lo
            // AFTER:  _ (rhs & lhs)_hi (rhs & lhs)_lo
            {self.entrypoint()}:
                pick 2
                and
                // _ rhs_hi lhs_hi (rhs_lo & lhs_lo)

                place 2
                and
                pick 1
                // _ (lhs_hi & rhs_hi) (rhs_lo & lhs_lo)
                // _ (rhs & lhs)_hi    (lhs & rhs)_lo

                return
        )
    }

    fn sign_offs(&self) -> HashMap<Reviewer, SignOffFingerprint> {
        let mut sign_offs = HashMap::new();
        sign_offs.insert(Reviewer("ferdinand"), 0xfd2a6e206fb3abab.into());
        sign_offs
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_prelude::*;

    impl Closure for And {
        type Args = (u64, u64);

        fn rust_shadow(&self, stack: &mut Vec<BFieldElement>) {
            let (right, left) = pop_encodable::<Self::Args>(stack);
            push_encodable(stack, &(left & right));
        }

        fn pseudorandom_args(
            &self,
            seed: [u8; 32],
            bench_case: Option<BenchmarkCase>,
        ) -> Self::Args {
            match bench_case {
                Some(BenchmarkCase::CommonCase) => (u32::MAX.into(), 1 << 10),
                Some(BenchmarkCase::WorstCase) => (u64::MAX, u64::MAX - 1),
                None => StdRng::from_seed(seed).gen(),
            }
        }

        fn corner_case_args(&self) -> Vec<Self::Args> {
            let edge_points = [0, 1 << 32, u64::MAX]
                .into_iter()
                .flat_map(|p| [p.checked_sub(1), Some(p), p.checked_add(1)])
                .flatten()
                .collect_vec();

            edge_points
                .iter()
                .cartesian_product(&edge_points)
                .map(|(&l, &r)| (l, r))
                .collect()
        }
    }

    #[test]
    fn rust_shadow() {
        ShadowedClosure::new(And).test();
    }
}

#[cfg(test)]
mod benches {
    use super::*;
    use crate::test_prelude::*;

    #[test]
    fn benchmark() {
        ShadowedClosure::new(And).bench();
    }
}
