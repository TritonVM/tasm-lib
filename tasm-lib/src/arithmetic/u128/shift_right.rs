use std::collections::HashMap;

use triton_vm::prelude::*;

use crate::prelude::*;
use crate::traits::basic_snippet::Reviewer;
use crate::traits::basic_snippet::SignOffFingerprint;

/// [Shift right][shr] for unsigned 128-bit integers.
///
/// # Behavior
///
/// ```text
/// BEFORE: _ [arg: u128] shift_amount
/// AFTER:  _ [result: u128]
/// ```
///
/// # Preconditions
///
/// - input argument `arg` is properly [`BFieldCodec`] encoded
/// - input argument `shift_amount` is in `0..128`
///
/// # Postconditions
///
/// - the output is the input argument `arg` bit-shifted to the right by
///   input argument `shift_amount`
/// - the output is properly [`BFieldCodec`] encoded
///
/// [shr]: core::ops::Shr
#[derive(Debug, Default, Copy, Clone, Eq, PartialEq, Hash)]
pub struct ShiftRight;

impl ShiftRight {
    pub const SHIFT_AMOUNT_TOO_BIG_ERROR_ID: i128 = 540;
}

impl BasicSnippet for ShiftRight {
    fn parameters(&self) -> Vec<(DataType, String)> {
        let arg = (DataType::U128, "arg".to_string());
        let shift_amount = (DataType::U32, "shift_amount".to_string());

        vec![arg, shift_amount]
    }

    fn return_values(&self) -> Vec<(DataType, String)> {
        vec![(DataType::U128, "shifted_arg".to_string())]
    }

    fn entrypoint(&self) -> String {
        "tasmlib_arithmetic_u128_shift_right".to_string()
    }

    fn code(&self, _: &mut Library) -> Vec<LabelledInstruction> {
        let entrypoint = self.entrypoint();
        let shift_amount_gt_32 = format!("{entrypoint}_shift_amount_gt_32");

        triton_asm!(
            // BEFORE: _ v_3 v_2 v_1 v_0 s
            // AFTER:  _ (v >> s)_3 (v >> s)_2 (v >> s)_1 (v >> s)_0
            {entrypoint}:
                /* bounds check */
                push 128
                dup 1
                lt
                assert error_id {Self::SHIFT_AMOUNT_TOO_BIG_ERROR_ID}
                            // _ v_3 v_2 v_1 v_0 s

                /* special case if shift amount is greater than 32 */
                dup 0
                push 32
                lt          // _ v_3 v_2 v_1 v_0 s (s > 32)
                skiz
                    call {shift_amount_gt_32}
                            // _ v_3 v_2 v_1 v_0 s

                /* for an explanation, see snippet “u64::ShiftRight” */
                push -1
                mul
                addi 32     // _ v_3 v_2 v_1 v_0 (32 - s)
                push 2
                pow         // _ v_3 v_2 v_1 v_0 (2^(32 - s))

                dup 0
                pick 5
                mul         // _ v_2 v_1 v_0 (2^(32 - s)) v_3s
                place 4
                xb_mul      // _ v_3s v_2s v_1s v_0s

                pick 3
                split       // _ v_2s v_1s v_0s (v >> s)_3 c_2
                pick 4
                split       // _ v_1s v_0s (v >> s)_3 c_2 (v_2 >> s) c_1
                pick 5
                split       // _ v_0s (v >> s)_3 c_2 (v_2 >> s) c_1 (v_1 >> s) c_0
                pick 6
                split       // _ (v >> s)_3 c_2 (v_2 >> s) c_1 (v_1 >> s) c_0 (v_0 >> s) trash

                pop 1       // _ (v >> s)_3 c_2 (v_2 >> s) c_1 (v_1 >> s) c_0 (v_0 >> s)
                add         // _ (v >> s)_3 c_2 (v_2 >> s) c_1 (v_1 >> s) (v >> s)_0
                place 4     // _ (v >> s)_3 (v >> s)_0 c_2 (v_2 >> s) c_1 (v_1 >> s)
                add         // _ (v >> s)_3 (v >> s)_0 c_2 (v_2 >> s) (v >> s)_1
                place 3     // _ (v >> s)_3 (v >> s)_1 (v >> s)_0 c_2 (v_2 >> s)
                add         // _ (v >> s)_3 (v >> s)_1 (v >> s)_0 (v_2 >> s)_2
                place 2     // _ (v >> s)_3 (v_2 >> s)_2 (v >> s)_1 (v >> s)_0

                return

            // BEFORE: _ [v: u128] s
            // AFTER:  _ [v >> i·32: u128] (s - i·32)
            // such that i·32 <= s < (i+1)·32
            {shift_amount_gt_32}:
                addi -32    // _ v_3 v_2 v_1 v_0 (s - 32)
                pick 1
                pop 1       // _ v_3 v_2 v_1 (s - 32)
                push 0
                place 4     // _ 0 v_3 v_2 v_1 (s - 32)

                dup 0
                push 32
                lt
                skiz
                    recurse
                return
        )
    }

    fn sign_offs(&self) -> HashMap<Reviewer, SignOffFingerprint> {
        let mut sign_offs = HashMap::new();
        sign_offs.insert(Reviewer("ferdinand"), 0x68440e40d1aa55e9.into());
        sign_offs
    }
}

#[cfg(test)]
mod tests {
    use rand::rngs::StdRng;

    use super::*;
    use crate::test_prelude::*;

    impl Closure for ShiftRight {
        type Args = (u128, u32);

        fn rust_shadow(&self, stack: &mut Vec<BFieldElement>) {
            let (arg, shift_amount) = pop_encodable::<Self::Args>(stack);
            assert!(shift_amount < 128);
            push_encodable(stack, &(arg >> shift_amount));
        }

        fn pseudorandom_args(
            &self,
            seed: [u8; 32],
            bench_case: Option<BenchmarkCase>,
        ) -> Self::Args {
            let mut rng = StdRng::from_seed(seed);

            match bench_case {
                Some(BenchmarkCase::CommonCase) => (0x642, 20),
                Some(BenchmarkCase::WorstCase) => (0x123, 127),
                None => (rng.random(), rng.random_range(0..128)),
            }
        }

        fn corner_case_args(&self) -> Vec<Self::Args> {
            [0, 1 << 3, 1 << 64, u64::MAX.into(), 1 << 127, u128::MAX]
                .into_iter()
                .cartesian_product(0..128)
                .collect()
        }
    }

    #[test]
    fn rust_shadow() {
        ShadowedClosure::new(ShiftRight).test();
    }

    #[proptest]
    fn too_large_shift_crashes_vm(arg: u128, #[strategy(128_u32..)] shift_amount: u32) {
        test_assertion_failure(
            &ShadowedClosure::new(ShiftRight),
            InitVmState::with_stack(ShiftRight.set_up_test_stack((arg, shift_amount))),
            &[ShiftRight::SHIFT_AMOUNT_TOO_BIG_ERROR_ID],
        )
    }
}

#[cfg(test)]
mod benches {
    use super::*;
    use crate::test_prelude::*;

    #[test]
    fn benchmark() {
        ShadowedClosure::new(ShiftRight).bench();
    }
}
