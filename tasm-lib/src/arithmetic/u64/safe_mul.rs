use std::collections::HashMap;

use triton_vm::prelude::*;

use crate::prelude::*;
use crate::traits::basic_snippet::Reviewer;
use crate::traits::basic_snippet::SignOffFingerprint;

/// Multiply two `u64`s and crash on overflow.
///
/// ### Behavior
///
/// ```text
/// BEFORE: _ [right: u64] [left: u64]
/// AFTER:  _ [right · left: u64]
/// ```
///
/// ### Preconditions
///
/// - all input arguments are properly [`BFieldCodec`] encoded
/// - the product of `left` and `right` is less than or equal to [`u64::MAX`]
///
/// ### Postconditions
///
/// - the output is the product of the input
/// - the output is properly [`BFieldCodec`] encoded
#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub struct SafeMul;

impl BasicSnippet for SafeMul {
    fn parameters(&self) -> Vec<(DataType, String)> {
        ["rhs", "lhs"]
            .map(|side| (DataType::U64, side.to_string()))
            .to_vec()
    }

    fn return_values(&self) -> Vec<(DataType, String)> {
        vec![(DataType::U64, "product".to_string())]
    }

    fn entrypoint(&self) -> String {
        "tasmlib_arithmetic_u64_safe_mul".to_string()
    }

    fn code(&self, _: &mut Library) -> Vec<LabelledInstruction> {
        triton_asm!(
            // BEFORE: _ right_hi right_lo left_hi left_lo
            // AFTER:  _ prod_hi prod_lo
            {self.entrypoint()}:
                /* left_lo · right_lo */
                dup 0
                dup 3
                mul
                // _ right_hi right_lo left_hi left_lo (left_lo · right_lo)

                /* left_lo · right_hi (consume left_lo) */
                dup 4
                pick 2
                mul
                // _ right_hi right_lo left_hi (left_lo · right_lo) (left_lo · right_hi)

                /* left_hi · right_lo (consume right_lo) */
                pick 3
                dup 3
                mul
                // _ right_hi left_hi (left_lo · right_lo) (left_lo · right_hi) (left_hi · right_lo)

                /* left_hi · right_hi (consume left_hi and right_hi) */
                pick 4
                pick 4
                mul
                // _ (left_lo · right_lo) (left_lo · right_hi) (left_hi · right_lo) (left_hi · right_hi)

                /* assert left_hi · right_hi == 0 */
                push 0
                eq
                assert error_id 100
                // _ (left_lo · right_lo) (left_lo · right_hi) (left_hi · right_lo)
                // _ lolo                 lohi                 hilo

                /* prod_hi = lolo_hi + lohi_lo + hilo_lo */
                split
                pick 1
                push 0
                eq
                assert error_id 101
                // _ lolo lohi hilo_lo

                pick 1
                split
                pick 1
                push 0
                eq
                assert error_id 102
                // _ lolo hilo_lo lohi_lo


                pick 2
                split
                // _ hilo_lo lohi_lo lolo_hi lolo_lo
                // _ hilo_lo lohi_lo lolo_hi prod_lo

                place 3
                add
                add
                // _ prod_lo (hilo_lo + lohi_lo + lolo_hi)

                split
                pick 1
                push 0
                eq
                assert error_id 103
                // _ prod_lo (hilo_lo + lohi_lo + lolo_hi)_lo
                // _ prod_lo prod_hi

                place 1
                return
        )
    }

    fn sign_offs(&self) -> HashMap<Reviewer, SignOffFingerprint> {
        let mut sign_offs = HashMap::new();
        sign_offs.insert(Reviewer("ferdinand"), 0xe472ad0d428ed30c.into());
        sign_offs
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_prelude::*;

    impl Closure for SafeMul {
        type Args = (u64, u64);

        fn rust_shadow(&self, stack: &mut Vec<BFieldElement>) {
            let (right, left) = pop_encodable::<Self::Args>(stack);
            let (product, is_overflow) = left.overflowing_mul(right);
            assert!(!is_overflow);
            push_encodable(stack, &product);
        }

        fn pseudorandom_args(
            &self,
            seed: [u8; 32],
            bench_case: Option<BenchmarkCase>,
        ) -> Self::Args {
            let Some(bench_case) = bench_case else {
                let mut rng = StdRng::from_seed(seed);
                return (rng.next_u32().into(), rng.next_u32().into());
            };

            match bench_case {
                BenchmarkCase::CommonCase => (1 << 31, (1 << 25) - 1),
                BenchmarkCase::WorstCase => (1 << 31, (1 << 31) - 1),
            }
        }
    }

    #[test]
    fn rust_shadow() {
        ShadowedClosure::new(SafeMul).test();
    }

    #[test]
    fn overflow_tests() {
        let failure_conditions = [
            (1 << 32, 1 << 32, 100),             // (left_hi · right_hi) != 0
            (1 << 31, 1 << 33, 101),             // (left_lo · right_hi)_hi != 0
            (1 << 33, 1 << 31, 102),             // (left_hi · right_lo)_hi != 0
            ((1 << 31) - 1, (1 << 33) + 5, 103), // (hilo_lo + lohi_lo + lolo_hi)_hi != 0
        ];

        for (left, right, error_id) in failure_conditions {
            let safe_mul = ShadowedClosure::new(SafeMul);
            let stack = SafeMul.set_up_test_stack((left, right));
            let vm_state = InitVmState::with_stack(stack);
            test_assertion_failure(&safe_mul, vm_state, &[error_id]);
        }
    }
}

#[cfg(test)]
mod benches {
    use super::*;
    use crate::test_prelude::*;

    #[test]
    fn benchmark() {
        ShadowedClosure::new(SafeMul).bench();
    }
}
