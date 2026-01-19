use std::collections::HashMap;

use triton_vm::prelude::*;

use crate::arithmetic::u128::shift_right::ShiftRight;
use crate::prelude::*;
use crate::traits::basic_snippet::Reviewer;
use crate::traits::basic_snippet::SignOffFingerprint;

/// [Shift left][shl] for unsigned 128-bit integers.
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
/// - the output is the input argument `arg` bit-shifted to the left by
///   input argument `shift_amount`
/// - the output is properly [`BFieldCodec`] encoded
///
/// [shl]: core::ops::Shl
#[derive(Debug, Default, Copy, Clone, Eq, PartialEq, Hash)]
pub struct ShiftLeft;

impl ShiftLeft {
    pub const SHIFT_AMOUNT_TOO_BIG_ERROR_ID: i128 = 530;
}

impl BasicSnippet for ShiftLeft {
    fn parameters(&self) -> Vec<(DataType, String)> {
        ShiftRight.parameters()
    }

    fn return_values(&self) -> Vec<(DataType, String)> {
        ShiftRight.return_values()
    }

    fn entrypoint(&self) -> String {
        "tasmlib_arithmetic_u128_shift_left".to_string()
    }

    fn code(&self, _: &mut Library) -> Vec<LabelledInstruction> {
        let entrypoint = self.entrypoint();
        let shift_amount_gt_32 = format!("{entrypoint}_shift_amount_gt_32");

        triton_asm!(
            // BEFORE: _ v_3 v_2 v_1 v_0 s
            // AFTER:  _ (value << s)_3 (value << s)_2 (value << s)_1 (value << s)_0
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
                lt
                skiz
                    call {shift_amount_gt_32}
                            // _ v_3 v_2 v_1 v_0 s

                push 2
                pow         // _ v_3 v_2 v_1 v_0 (2^s)

                dup 0
                pick 5      // _ v_2 v_1 v_0 (2^s) (2^s) v_3
                mul
                place 4     // _ v_3<<s v_2 v_1 v_0 (2^s)
                xb_mul      // _ v_3<<s v_2<<s v_1<<s v_0<<s

                pick 3
                split
                pick 4
                split
                pick 5
                split
                pick 6
                split       // _ v_3s_hi v_3s_lo v_2s_hi v_2s_lo v_1s_hi v_1s_lo v_0s_hi v_0s_lo
                            // _ ╰─ 🗑 ╯ ╰──── w_3 ────╯ ╰──── w_2 ────╯ ╰──── w_1 ────╯ ╰ w_0 ╯
                place 7
                add
                place 6
                add
                place 5
                add
                place 4
                pop 1

                return

            // BEFORE: _ [v: u128] s
            // AFTER:  _ [v << i·32: u128] (s - i·32)
            // such that i·32 <= s < (i+1)·32
            {shift_amount_gt_32}:
                addi -32    // _ v_3 v_2 v_1 v_0 (s - 32)
                pick 4
                pop 1       // _ v_2 v_1 v_0 (s - 32)
                push 0
                place 1     // _ v_2 v_1 v_0 0 (s - 32)

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
        sign_offs.insert(Reviewer("ferdinand"), 0x4d9d78686bfa6221.into());
        sign_offs
    }
}

#[cfg(test)]
mod tests {
    use rand::rngs::StdRng;

    use super::*;
    use crate::test_prelude::*;

    impl Closure for ShiftLeft {
        type Args = <ShiftRight as Closure>::Args;

        fn rust_shadow(&self, stack: &mut Vec<BFieldElement>) {
            let (arg, shift_amount) = pop_encodable::<Self::Args>(stack);
            assert!(shift_amount < 128);
            push_encodable(stack, &(arg << shift_amount));
        }

        fn pseudorandom_args(
            &self,
            seed: [u8; 32],
            bench_case: Option<BenchmarkCase>,
        ) -> Self::Args {
            let mut rng = StdRng::from_seed(seed);

            match bench_case {
                Some(BenchmarkCase::CommonCase) => (0x1282, 15),
                Some(BenchmarkCase::WorstCase) => (0x123456789abcdef, 125),
                None => (rng.random(), rng.random_range(0..128)),
            }
        }

        fn corner_case_args(&self) -> Vec<Self::Args> {
            ShiftRight.corner_case_args()
        }
    }

    #[test]
    fn rust_shadow() {
        ShadowedClosure::new(ShiftLeft).test();
    }

    #[proptest]
    fn too_large_shift_crashes_vm(arg: u128, #[strategy(128_u32..)] shift_amount: u32) {
        test_assertion_failure(
            &ShadowedClosure::new(ShiftLeft),
            InitVmState::with_stack(ShiftLeft.set_up_test_stack((arg, shift_amount))),
            &[ShiftLeft::SHIFT_AMOUNT_TOO_BIG_ERROR_ID],
        )
    }
}

#[cfg(test)]
mod benches {
    use super::*;
    use crate::test_prelude::*;

    #[test]
    fn benchmark() {
        ShadowedClosure::new(ShiftLeft).bench();
    }
}
