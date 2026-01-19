use std::collections::HashMap;

use triton_vm::prelude::*;

use crate::prelude::*;
use crate::traits::basic_snippet::Reviewer;
use crate::traits::basic_snippet::SignOffFingerprint;

/// Multiply two `u128`s and crash on overflow.
///
/// ### Behavior
///
/// ```text
/// BEFORE: _ [right: u128] [left: u128]
/// AFTER:  _ [left · right: u128]
/// ```
///
/// ### Preconditions
///
/// - all input arguments are properly [`BFieldCodec`] encoded
/// - the product of `left` and `right` is less than or equal to [`u128::MAX`]
///
/// ### Postconditions
///
/// - the output is the product of the input
/// - the output is properly [`BFieldCodec`] encoded
#[derive(Debug, Default, Copy, Clone, Eq, PartialEq, Hash)]
pub struct SafeMul;

impl BasicSnippet for SafeMul {
    fn parameters(&self) -> Vec<(DataType, String)> {
        ["right", "left"]
            .map(|side| (DataType::U128, side.to_string()))
            .to_vec()
    }

    fn return_values(&self) -> Vec<(DataType, String)> {
        vec![(DataType::U128, "product".to_string())]
    }

    fn entrypoint(&self) -> String {
        "tasmlib_arithmetic_u128_safe_mul".to_string()
    }

    fn code(&self, _: &mut Library) -> Vec<LabelledInstruction> {
        triton_asm!(
            // BEFORE: _ r_3 r_2 r_1 r_0 l_3 l_2 l_1 l_0
            // AFTER:  _ p_3 p_2 p_1 p_0
            {self.entrypoint()}:
                /*
                 * p_0 is low limb, c_0 high limb of
                 *        l_0·r_0
                 *
                 * p_1 is low limb, c_1 high limb of
                 *        (l_1·r_0)_lo + (l_0·r_1)_lo
                 *      + c_0
                 *
                 * p_2 is low limb, c_2 high limb of
                 *        (l_1·r_0)_hi + (l_0·r_1)_hi
                 *      + (l_2·r_0)_lo + (l_1·r_1)_lo + (l_0·r_2)_lo
                 *      + c_1
                 *
                 * p_3 is low limb, c_3 high limb of
                 *        (l_2·r_0)_hi + (l_1·r_1)_hi + (l_0·r_2)_hi
                 *      + (l_3·r_0)_lo + (l_2·r_1)_lo + (l_1·r_2)_lo + (l_0·r_3)_lo
                 *      + c_2
                 *
                 * All remaining limb combinations (l_3·r_1, l_3·r_2, l_3·r_3 l_2·r_2,
                 * l_2·r_3, and l_1·r_3) as well as c_3 must be 0.
                 */

                /* p_0 */
                dup 0 dup 5 mul split
                // _ r_3 r_2 r_1 r_0 l_3 l_2 l_1 l_0 c_0 p_0

                place 9
                // _ p_0 r_3 r_2 r_1 r_0 l_3 l_2 l_1 l_0 c_0

                /* p_1 */
                dup 2 dup 6 mul split
                // _ p_0 r_3 r_2 r_1 r_0 l_3 l_2 l_1 l_0 c_0 (l_1·r_0)_hi (l_1·r_0)_lo

                dup 3 dup 9 mul split
                // _ p_0 r_3 r_2 r_1 r_0 l_3 l_2 l_1 l_0 c_0 (l_1·r_0)_hi (l_1·r_0)_lo (l_0·r_1)_hi (l_0·r_1)_lo
                //                                       ^^^              ^^^^^^^^^^^^              ^^^^^^^^^^^^

                pick 2 pick 4
                add add
                split
                // _ p_0 r_3 r_2 r_1 r_0 l_3 l_2 l_1 l_0 (l_1·r_0)_hi (l_0·r_1)_hi c_1 p_1

                place 12
                // _ p_1 p_0 r_3 r_2 r_1 r_0 l_3 l_2 l_1 l_0 (l_1·r_0)_hi (l_0·r_1)_hi c_1

                /* p_2 */
                add add
                // _ p_1 p_0 r_3 r_2 r_1 r_0 l_3 l_2 l_1 l_0 p_2_wip

                dup 3 dup 6 mul split
                // _ p_1 p_0 r_3 r_2 r_1 r_0 l_3 l_2 l_1 l_0 p_2_wip (l_2·r_0)_hi (l_2·r_0)_lo

                dup 4 dup 9 mul split
                // _ p_1 p_0 r_3 r_2 r_1 r_0 l_3 l_2 l_1 l_0 p_2_wip (l_2·r_0)_hi (l_2·r_0)_lo (l_1·r_1)_hi (l_1·r_1)_lo

                dup 5 dup 12 mul split
                // _ p_1 p_0 r_3 r_2 r_1 r_0 l_3 l_2 l_1 l_0 p_2_wip (l_2·r_0)_hi (l_2·r_0)_lo (l_1·r_1)_hi (l_1·r_1)_lo (l_0·r_2)_hi (l_0·r_2)_lo
                //                                           ^^^^^^^              ^^^^^^^^^^^^              ^^^^^^^^^^^^              ^^^^^^^^^^^^

                pick 2 pick 4 pick 6
                add add add
                split
                // _ p_1 p_0 r_3 r_2 r_1 r_0 l_3 l_2 l_1 l_0 (l_2·r_0)_hi (l_1·r_1)_hi (l_0·r_2)_hi c_2 p_2

                place 14
                // _ p_2 p_1 p_0 r_3 r_2 r_1 r_0 l_3 l_2 l_1 l_0 (l_2·r_0)_hi (l_1·r_1)_hi (l_0·r_2)_hi c_2

                /* p_3 */
                add add add
                // _ p_2 p_1 p_0 r_3 r_2 r_1 r_0 l_3 l_2 l_1 l_0 p_3_wip

                dup 4 pick 6 mul split
                // _ p_2 p_1 p_0 r_3 r_2 r_1 l_3 l_2 l_1 l_0 p_3_wip (l_3·r_0)_hi (l_3·r_0)_lo

                dup 5 dup 8 mul split
                // _ p_2 p_1 p_0 r_3 r_2 r_1 l_3 l_2 l_1 l_0 p_3_wip (l_3·r_0)_hi (l_3·r_0)_lo (l_2·r_1)_hi (l_2·r_1)_lo

                dup 6 dup 11 mul split
                // _ p_2 p_1 p_0 r_3 r_2 r_1 l_3 l_2 l_1 l_0 p_3_wip (l_3·r_0)_hi (l_3·r_0)_lo (l_2·r_1)_hi (l_2·r_1)_lo (l_1·r_2)_hi (l_1·r_2)_lo

                pick 7 dup 13 mul split
                // _ p_2 p_1 p_0 r_3 r_2 r_1 l_3 l_2 l_1 p_3_wip (l_3·r_0)_hi (l_3·r_0)_lo (l_2·r_1)_hi (l_2·r_1)_lo (l_1·r_2)_hi (l_1·r_2)_lo (l_0·l_3)_hi (l_0·l_3)_lo
                //                                       ^^^^^^^              ^^^^^^^^^^^^              ^^^^^^^^^^^^              ^^^^^^^^^^^^              ^^^^^^^^^^^^

                pick 2 pick 4 pick 6 pick 8
                add add add add
                split
                // _ p_2 p_1 p_0 r_3 r_2 r_1 l_3 l_2 l_1 (l_3·r_0)_hi (l_2·r_1)_hi (l_1·r_2)_hi (l_0·l_3)_hi c_3 p_3

                place 14
                // _ [p; 4] r_3 r_2 r_1 l_3 l_2 l_1 (l_3·r_0)_hi (l_2·r_1)_hi (l_1·r_2)_hi (l_0·l_3)_hi c_3

                /* overflow checks
                 *
                 * Carry c_3 and the high limbs still on stack are guaranteed to be smaller than
                 * 2^32 since they resulted from instruction `split`. The sum of those 5 elements
                 * cannot “wrap around” `BFieldElement::P`.
                 */
                add add add add
                push 0 eq assert error_id 500
                // _ [p; 4] r_3 r_2 r_1 l_3 l_2 l_1

                /* l_3·r_1 */
                dup 2 pick 4 mul
                push 0 eq assert error_id 501
                // _ [p; 4] r_3 r_2 l_3 l_2 l_1

                /* l_2·r_2 */
                dup 1 dup 4 mul
                push 0 eq assert error_id 502
                // _ [p; 4] r_3 r_2 l_3 l_2 l_1

                /* l_1·r_3 */
                dup 4 mul
                push 0 eq assert error_id 503
                // _ [p; 4] r_3 r_2 l_3 l_2

                /* l_3·r_2 */
                dup 1 pick 3 mul
                push 0 eq assert error_id 504
                // _ [p; 4] r_3 l_3 l_2

                /* l_2·r_3 */
                dup 2 mul
                push 0 eq assert error_id 505
                // _ [p; 4] r_3 l_3

                /* l_3·r_3 */
                mul
                push 0 eq assert error_id 506
                // _ [p; 4]

                return
        )
    }

    fn sign_offs(&self) -> HashMap<Reviewer, SignOffFingerprint> {
        let mut sign_offs = HashMap::new();
        sign_offs.insert(Reviewer("ferdinand"), 0xbba006a82c82b12f.into());
        sign_offs
    }
}

#[cfg(test)]
mod tests {
    use rand::rngs::StdRng;

    use super::*;
    use crate::test_prelude::*;

    impl SafeMul {
        fn test_assertion_failure(&self, left: u128, right: u128, error_ids: &[i128]) {
            test_assertion_failure(
                &ShadowedClosure::new(Self),
                InitVmState::with_stack(self.set_up_test_stack((right, left))),
                error_ids,
            );
        }
    }

    impl Closure for SafeMul {
        type Args = (u128, u128);

        fn rust_shadow(&self, stack: &mut Vec<BFieldElement>) {
            let (right, left) = pop_encodable::<Self::Args>(stack);
            let product = left.checked_mul(right).unwrap();
            push_encodable(stack, &product);
        }

        fn pseudorandom_args(
            &self,
            seed: [u8; 32],
            bench_case: Option<BenchmarkCase>,
        ) -> Self::Args {
            let Some(bench_case) = bench_case else {
                let mut rng = StdRng::from_seed(seed);
                let left = rng.random_range(1..=u128::MAX);
                let right = rng.random_range(0..=u128::MAX / left);

                return (right, left);
            };

            match bench_case {
                BenchmarkCase::CommonCase => (1 << 63, (1 << 45) - 1),
                BenchmarkCase::WorstCase => (1 << 63, (1 << 63) - 1),
            }
        }

        fn corner_case_args(&self) -> Vec<Self::Args> {
            const LEFT_NOISE: u128 = 0xfd4e_3f84_8677_df6b_da64_b83c_8267_c72d;
            const RIGHT_NOISE: u128 = 0x538e_e051_c430_3e7a_0a29_a45a_5efb_67fa;

            (0..u128::BITS)
                .cartesian_product(0..u128::BITS)
                .map(|(l, r)| {
                    let left = (1 << l) | ((1 << l) - 1) & LEFT_NOISE;
                    let right = (1 << r) | ((1 << r) - 1) & RIGHT_NOISE;
                    (right, left)
                })
                .filter(|&(right, left)| left.checked_mul(right).is_some())
                .step_by(5) // test performance is atrocious otherwise
                .chain([(0, 0)])
                .collect()
        }
    }

    #[test]
    fn rust_shadow() {
        ShadowedClosure::new(SafeMul).test();
    }

    #[test]
    fn overflow_crashes_vm() {
        SafeMul.test_assertion_failure(1 << 127, 1 << 1, &[500]);
        SafeMul.test_assertion_failure(1 << 96, 1 << 32, &[501]);
        SafeMul.test_assertion_failure(1 << 64, 1 << 64, &[502]);
        SafeMul.test_assertion_failure(1 << 32, 1 << 96, &[503]);
        SafeMul.test_assertion_failure(1 << 96, 1 << 64, &[504]);
        SafeMul.test_assertion_failure(1 << 64, 1 << 96, &[505]);
        SafeMul.test_assertion_failure(1 << 96, 1 << 96, &[506]);

        for i in 1..64 {
            let left = u128::MAX >> i;
            let right = (1 << i) + 1;
            SafeMul.test_assertion_failure(left, right, &[500]);
            SafeMul.test_assertion_failure(right, left, &[500]);
        }

        for i in 1..128 {
            let left = 1 << i;
            let right = 1 << (128 - i);
            SafeMul.test_assertion_failure(left, right, &[500, 501, 502, 503]);
        }
    }

    #[proptest(cases = 80)]
    fn arbitrary_overflow_crashes_vm(
        #[strategy(2_u8..128)] _log_upper_bound: u8,
        #[strategy(2_u128..(1 << #_log_upper_bound))] left: u128,
        #[strategy(u128::MAX / #left + 1..)] right: u128,
    ) {
        SafeMul.test_assertion_failure(left, right, &[500, 501, 502, 503, 504, 505, 506]);
    }

    #[proptest(cases = 80)]
    fn marginal_overflow_crashes_vm(
        #[strategy(2_u8..128)] _log_upper_bound: u8,
        #[strategy(2_u128..(1 << #_log_upper_bound))] left: u128,
    ) {
        let right = u128::MAX / left + 1;
        SafeMul.test_assertion_failure(left, right, &[500, 501, 502, 503, 504, 505, 506]);
    }

    #[proptest]
    fn arbitrary_overflow_crashes_vm_u128(
        #[strategy(2_u128..)] left: u128,
        #[strategy(u128::MAX / #left + 1..)] right: u128,
    ) {
        SafeMul.test_assertion_failure(left, right, &[500, 501, 502, 503, 504, 505, 506]);
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
