use std::collections::HashMap;

use triton_vm::prelude::*;

use crate::prelude::*;
use crate::traits::basic_snippet::Reviewer;
use crate::traits::basic_snippet::SignOffFingerprint;

/// Perform the “[less than](u64::lt)” operation.
///
/// ### Behavior
///
/// ```text
/// BEFORE: _ [rhs: u64] [lhs: u64]
/// AFTER:  _ [lhs < rhs: bool]
/// ```
///
/// ### Preconditions
///
/// - all input arguments are properly [`BFieldCodec`] encoded
///
/// ### Postconditions
///
/// - the output is `true` if and only if the input argument `lhs` is less than
///   the input argument `rhs`
/// - the output is properly [`BFieldCodec`] encoded
#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub struct Lt;

impl BasicSnippet for Lt {
    fn parameters(&self) -> Vec<(DataType, String)> {
        ["rhs", "lhs"]
            .map(|s| (DataType::U64, s.to_owned()))
            .to_vec()
    }

    fn return_values(&self) -> Vec<(DataType, String)> {
        vec![(DataType::Bool, "lhs < rhs".to_owned())]
    }

    fn entrypoint(&self) -> String {
        "tasmlib_arithmetic_u64_lt".to_owned()
    }

    fn code(&self, _: &mut Library) -> Vec<LabelledInstruction> {
        triton_asm!(
            {self.entrypoint()}:
                // _ rhs_hi rhs_lo lhs_hi lhs_lo

                /* lhs < rhs if and only if
                 * lhs_hi < rhs_hi or
                 * lhs_hi == rhs_hi and lhs_lo < rhs_lo
                 */

                dup 3
                dup 2
                // _ rhs_hi rhs_lo lhs_hi lhs_lo rhs_hi lhs_hi

                lt
                // _ rhs_hi rhs_lo lhs_hi lhs_lo (lhs_hi < rhs_hi)

                pick 4
                pick 3
                eq
                // _ rhs_lo lhs_lo (lhs_hi < rhs_hi) (rhs_hi == lhs_hi)

                pick 3
                pick 3
                // _ (lhs_hi < rhs_hi) (rhs_hi == lhs_hi) rhs_lo lhs_lo

                lt
                // _ (lhs_hi < rhs_hi) (rhs_hi == lhs_hi) (lhs_lo < rhs_lo)

                mul
                add
                // _ (lhs < rhs)

                return
        )
    }

    fn sign_offs(&self) -> HashMap<Reviewer, SignOffFingerprint> {
        let mut sign_offs = HashMap::new();
        sign_offs.insert(Reviewer("ferdinand"), 0x18418167b2d68326.into());
        sign_offs
    }
}

#[cfg(test)]
pub(crate) mod tests {
    use super::*;
    use crate::test_prelude::*;

    impl Lt {
        pub fn assert_expected_lt_behavior(&self, lhs: u64, rhs: u64) {
            let initial_stack = self.set_up_test_stack((lhs, rhs));

            let mut expected_stack = initial_stack.clone();
            self.rust_shadow(&mut expected_stack);

            test_rust_equivalence_given_complete_state(
                &ShadowedClosure::new(Self),
                &initial_stack,
                &[],
                &NonDeterminism::default(),
                &None,
                Some(&expected_stack),
            );
        }
    }

    impl Closure for Lt {
        type Args = (u64, u64);

        fn rust_shadow(&self, stack: &mut Vec<BFieldElement>) {
            let (right, left) = pop_encodable::<Self::Args>(stack);
            push_encodable(stack, &(left < right));
        }

        fn pseudorandom_args(
            &self,
            seed: [u8; 32],
            bench_case: Option<BenchmarkCase>,
        ) -> Self::Args {
            match bench_case {
                Some(BenchmarkCase::CommonCase) => (0x100_ffff_ffff, 0x100_ffff_fffe),
                Some(BenchmarkCase::WorstCase) => (u64::MAX - 1, u64::MAX),
                None => StdRng::from_seed(seed).random(),
            }
        }

        fn corner_case_args(&self) -> Vec<Self::Args> {
            let edge_case_points = [0, 1 << 29, 1 << 31, 1 << 32, u64::MAX]
                .into_iter()
                .flat_map(|p| [p.checked_sub(1), Some(p), p.checked_add(1)])
                .flatten()
                .collect_vec();

            edge_case_points
                .iter()
                .cartesian_product(&edge_case_points)
                .map(|(&left, &right)| (left, right))
                .collect()
        }
    }

    #[test]
    fn rust_shadow_test() {
        ShadowedClosure::new(Lt).test()
    }

    #[test]
    fn unit_test() {
        Lt.assert_expected_lt_behavior(11 * (1 << 32), 15 * (1 << 32));
    }

    #[proptest]
    fn property_test(left: u64, right: u64) {
        Lt.assert_expected_lt_behavior(left, right);
    }
}

#[cfg(test)]
mod benches {
    use super::*;
    use crate::test_prelude::*;

    #[test]
    fn benchmark() {
        ShadowedClosure::new(Lt).bench();
    }
}
