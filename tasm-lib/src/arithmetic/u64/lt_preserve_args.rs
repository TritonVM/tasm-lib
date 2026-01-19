use std::collections::HashMap;

use triton_vm::prelude::*;

use crate::arithmetic::u64::lt::Lt;
use crate::prelude::*;
use crate::traits::basic_snippet::Reviewer;
use crate::traits::basic_snippet::SignOffFingerprint;

/// Perform the “[less than](u64::lt)” operation without consuming the
/// arguments.
///
/// See also [`Lt`].
///
/// ### Behavior
///
/// ```text
/// BEFORE: _ [rhs: u64] [lhs: u64]
/// AFTER:  _ [rhs: u64] [lhs: u64] [lhs < rhs: bool]
/// ```
///
/// ### Preconditions
///
/// - all input arguments are properly [`BFieldCodec`] encoded
///
/// ### Postconditions
///
/// - the output `lhs` is equal to the input argument `lhs`
/// - the output `rhs` is equal to the input argument `rhs`
/// - the output is `true` if and only if the input argument `lhs` is less than
///   the input argument `rhs`
/// - all output is properly [`BFieldCodec`] encoded
#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub struct LtPreserveArgs;

impl BasicSnippet for LtPreserveArgs {
    fn parameters(&self) -> Vec<(DataType, String)> {
        Lt.parameters()
    }

    fn return_values(&self) -> Vec<(DataType, String)> {
        [self.parameters(), Lt.return_values()].concat()
    }

    fn entrypoint(&self) -> String {
        "tasmlib_arithmetic_u64_lt_preserve_args".to_string()
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

                dup 4
                dup 3
                eq
                // _ rhs_hi rhs_lo lhs_hi lhs_lo (lhs_hi < rhs_hi) (rhs_hi == lhs_hi)

                dup 4
                dup 3
                // _ rhs_hi rhs_lo lhs_hi lhs_lo (lhs_hi < rhs_hi) (rhs_hi == lhs_hi) rhs_lo lhs_lo

                lt
                // _ rhs_hi rhs_lo lhs_hi lhs_lo (lhs_hi < rhs_hi) (rhs_hi == lhs_hi) (lhs_lo < rhs_lo)

                mul
                add
                // _ rhs_hi rhs_lo lhs_hi lhs_lo (lhs < rhs)

                return
        )
    }

    fn sign_offs(&self) -> HashMap<Reviewer, SignOffFingerprint> {
        let mut sign_offs = HashMap::new();
        sign_offs.insert(Reviewer("ferdinand"), 0xd553ed4340f6a42d.into());
        sign_offs
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_prelude::*;

    impl LtPreserveArgs {
        pub fn assert_expected_lt_behavior(&self, left: u64, right: u64) {
            let initial_stack = self.set_up_test_stack((left, right));

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

    impl Closure for LtPreserveArgs {
        type Args = <Lt as Closure>::Args;

        fn rust_shadow(&self, stack: &mut Vec<BFieldElement>) {
            let (right, left) = pop_encodable::<Self::Args>(stack);
            push_encodable(stack, &(right, left));
            push_encodable(stack, &(left < right));
        }

        fn pseudorandom_args(
            &self,
            seed: [u8; 32],
            bench_case: Option<BenchmarkCase>,
        ) -> Self::Args {
            Lt.pseudorandom_args(seed, bench_case)
        }

        fn corner_case_args(&self) -> Vec<Self::Args> {
            Lt.corner_case_args()
        }
    }

    #[test]
    fn rust_shadow_test() {
        ShadowedClosure::new(LtPreserveArgs).test()
    }

    #[test]
    fn unit_test() {
        LtPreserveArgs.assert_expected_lt_behavior(11 * (1 << 32), 15 * (1 << 32));
    }

    #[proptest]
    fn property_test(left: u64, right: u64) {
        LtPreserveArgs.assert_expected_lt_behavior(left, right);
    }
}

#[cfg(test)]
mod benches {
    use super::*;
    use crate::test_prelude::*;

    #[test]
    fn benchmark() {
        ShadowedClosure::new(LtPreserveArgs).bench();
    }
}
