use std::collections::HashMap;

use triton_vm::prelude::*;

use crate::arithmetic::u64::lt::Lt;
use crate::data_type::DataType;
use crate::library::Library;
use crate::traits::basic_snippet::BasicSnippet;
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
    fn inputs(&self) -> Vec<(DataType, String)> {
        Lt.inputs()
    }

    fn outputs(&self) -> Vec<(DataType, String)> {
        [self.inputs(), Lt.outputs()].concat()
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
        sign_offs.insert(Reviewer("ferdinand"), 0x858f91bcf192c41a.into());
        sign_offs
    }
}

#[cfg(test)]
mod tests {
    use test_strategy::proptest;

    use super::*;
    use crate::pop_encodable;
    use crate::push_encodable;
    use crate::snippet_bencher::BenchmarkCase;
    use crate::test_helpers::test_rust_equivalence_given_complete_state;
    use crate::traits::closure::Closure;
    use crate::traits::closure::ShadowedClosure;
    use crate::traits::rust_shadow::RustShadow;

    impl LtPreserveArgs {
        pub fn set_up_initial_stack(&self, lhs: u64, rhs: u64) -> Vec<BFieldElement> {
            Lt.set_up_initial_stack(lhs, rhs)
        }

        pub fn assert_expected_lt_behavior(&self, lhs: u64, rhs: u64) {
            let mut expected_stack = self.set_up_initial_stack(lhs, rhs);
            push_encodable(&mut expected_stack, &(lhs < rhs));

            test_rust_equivalence_given_complete_state(
                &ShadowedClosure::new(Self),
                &self.set_up_initial_stack(lhs, rhs),
                &[],
                &NonDeterminism::default(),
                &None,
                Some(&expected_stack),
            );
        }
    }

    impl Closure for LtPreserveArgs {
        fn rust_shadow(&self, stack: &mut Vec<BFieldElement>) {
            let left = pop_encodable::<u64>(stack);
            let right = pop_encodable::<u64>(stack);
            push_encodable(stack, &right);
            push_encodable(stack, &left);
            push_encodable(stack, &(left < right));
        }

        fn pseudorandom_initial_state(
            &self,
            seed: [u8; 32],
            bench_case: Option<BenchmarkCase>,
        ) -> Vec<BFieldElement> {
            Lt.pseudorandom_initial_state(seed, bench_case)
        }

        fn corner_case_initial_states(&self) -> Vec<Vec<BFieldElement>> {
            Lt.corner_case_initial_states()
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
    use crate::traits::closure::ShadowedClosure;
    use crate::traits::rust_shadow::RustShadow;

    #[test]
    fn benchmark() {
        ShadowedClosure::new(LtPreserveArgs).bench();
    }
}
