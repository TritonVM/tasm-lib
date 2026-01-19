use std::collections::HashMap;

use triton_vm::prelude::*;

use crate::prelude::*;
use crate::traits::basic_snippet::Reviewer;
use crate::traits::basic_snippet::SignOffFingerprint;

/// Add two `u64`s, indicating whether overflow occurred.
///
/// ### Behavior
///
/// ```text
/// BEFORE: _ [right: u64] [left: u64]
/// AFTER:  _ [sum: u64] [is_overflow: bool]
/// ```
///
/// ### Preconditions
///
/// - all input arguments are properly [`BFieldCodec`] encoded
///
/// ### Postconditions
///
/// - the `sum` is the sum of the input modulo [`u64::MAX`]
/// - `is_overflow` is `true` if and only if the sum of the input exceeds
///   [`u64::MAX`]
/// - the `sum` is properly [`BFieldCodec`] encoded
/// - `is_overflow` is properly [`BFieldCodec`] encoded
#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub struct OverflowingAdd;

impl OverflowingAdd {
    /// (See [`OverflowingAdd`])
    pub(crate) fn addition_code() -> Vec<LabelledInstruction> {
        triton_asm! {
            pick 2
            // _ rhs_hi lhs_hi lhs_lo rhs_lo

            add
            split
            // _ rhs_hi lhs_hi carry sum_lo

            place 3
            // _ sum_lo rhs_hi lhs_hi carry

            add
            add
            // _ sum_lo (lhs_hi+rhs_hi+carry)

            split
            // _ sum_lo overflow sum_hi

            place 2
        }
    }
}

impl BasicSnippet for OverflowingAdd {
    fn parameters(&self) -> Vec<(DataType, String)> {
        ["lhs", "rhs"]
            .map(|s| (DataType::U64, s.to_string()))
            .to_vec()
    }

    fn return_values(&self) -> Vec<(DataType, String)> {
        vec![
            (DataType::U64, "wrapped_sum".to_string()),
            (DataType::Bool, "overflow".to_string()),
        ]
    }

    fn entrypoint(&self) -> String {
        "tasmlib_arithmetic_u64_overflowing_add".to_string()
    }

    fn code(&self, _: &mut Library) -> Vec<LabelledInstruction> {
        triton_asm! {
            {self.entrypoint()}:
                {&Self::addition_code()}
                return
        }
    }

    fn sign_offs(&self) -> HashMap<Reviewer, SignOffFingerprint> {
        let mut sign_offs = HashMap::new();
        sign_offs.insert(Reviewer("ferdinand"), 0x77f1dc470c2bb719.into());
        sign_offs
    }
}

#[cfg(test)]
pub mod tests {
    use super::*;
    use crate::test_prelude::*;

    impl OverflowingAdd {
        pub fn corner_case_points() -> Vec<u64> {
            [0, 1 << 32, u64::MAX]
                .into_iter()
                .flat_map(|p| [p.checked_sub(1), Some(p), p.checked_add(1)])
                .flatten()
                .collect()
        }
    }

    impl Closure for OverflowingAdd {
        type Args = (u64, u64);

        fn rust_shadow(&self, stack: &mut Vec<BFieldElement>) {
            let right = pop_encodable::<u64>(stack);
            let left = pop_encodable::<u64>(stack);
            let (sum, is_overflow) = left.overflowing_add(right);
            push_encodable(stack, &sum);
            push_encodable(stack, &is_overflow);
        }

        fn pseudorandom_args(
            &self,
            seed: [u8; 32],
            bench_case: Option<BenchmarkCase>,
        ) -> Self::Args {
            match bench_case {
                Some(BenchmarkCase::CommonCase) => (1 << 63, (1 << 63) - 1),
                Some(BenchmarkCase::WorstCase) => (1 << 63, 1 << 50),
                None => StdRng::from_seed(seed).random(),
            }
        }

        fn corner_case_args(&self) -> Vec<Self::Args> {
            let corner_case_points = Self::corner_case_points();

            corner_case_points
                .iter()
                .cartesian_product(&corner_case_points)
                .map(|(&l, &r)| (l, r))
                .collect()
        }
    }

    #[test]
    fn u64_overflowing_add_pbt() {
        ShadowedClosure::new(OverflowingAdd).test()
    }
}

#[cfg(test)]
mod benches {
    use super::*;
    use crate::test_prelude::*;

    #[test]
    fn benchmark() {
        ShadowedClosure::new(OverflowingAdd).bench()
    }
}
