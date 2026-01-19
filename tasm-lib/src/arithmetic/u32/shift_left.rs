use std::collections::HashMap;

use triton_vm::prelude::*;

use crate::arithmetic::u32::shift_right::ShiftRight;
use crate::prelude::*;
use crate::traits::basic_snippet::Reviewer;
use crate::traits::basic_snippet::SignOffFingerprint;

/// [Shift left][shl] for unsigned 32-bit integers.
///
/// # Behavior
///
/// ```text
/// BEFORE: _ [arg: u32] shift_amount
/// AFTER:  _ [result: u32]
/// ```
///
/// # Preconditions
///
/// - input argument `arg` is properly [`BFieldCodec`] encoded
/// - input argument `shift_amount` is in `0..32`
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
    pub const SHIFT_AMOUNT_TOO_BIG_ERROR_ID: i128 = 480;
}

impl BasicSnippet for ShiftLeft {
    fn parameters(&self) -> Vec<(DataType, String)> {
        ShiftRight.parameters()
    }

    fn return_values(&self) -> Vec<(DataType, String)> {
        ShiftRight.return_values()
    }

    fn entrypoint(&self) -> String {
        "tasmlib_arithmetic_u32_shift_left".to_string()
    }

    fn code(&self, _: &mut Library) -> Vec<LabelledInstruction> {
        triton_asm!(
            // BEFORE: _ value shift
            // AFTER:  _ (value << shift)
            {self.entrypoint()}:
                /* bounds check mimics Rust's behavior */
                push 32
                dup 1
                lt
                assert error_id {Self::SHIFT_AMOUNT_TOO_BIG_ERROR_ID}

                push 2
                pow
                mul
                split
                pick 1
                pop 1

                return
        )
    }

    fn sign_offs(&self) -> HashMap<Reviewer, SignOffFingerprint> {
        let mut sign_offs = HashMap::new();
        sign_offs.insert(Reviewer("ferdinand"), 0x78cbb7a9ad26612c.into());
        sign_offs
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_prelude::*;

    impl Closure for ShiftLeft {
        type Args = (u32, u32);

        fn rust_shadow(&self, stack: &mut Vec<BFieldElement>) {
            let (arg, shift_amount) = pop_encodable::<Self::Args>(stack);
            assert!(shift_amount < 32);
            push_encodable(stack, &(arg << shift_amount));
        }

        fn pseudorandom_args(
            &self,
            seed: [u8; 32],
            bench_case: Option<BenchmarkCase>,
        ) -> Self::Args {
            let mut rng = StdRng::from_seed(seed);
            match bench_case {
                Some(BenchmarkCase::CommonCase) => ((1 << 16) - 1, 16),
                Some(BenchmarkCase::WorstCase) => (u32::MAX, 31),
                None => (rng.random(), rng.random_range(0..32)),
            }
        }

        fn corner_case_args(&self) -> Vec<Self::Args> {
            (0..32).map(|i| (u32::MAX, i)).collect()
        }
    }

    #[test]
    fn rust_shadow() {
        ShadowedClosure::new(ShiftLeft).test();
    }

    #[proptest]
    fn too_big_shift_amount_crashes_vm(arg: u32, #[strategy(32_u32..)] shift_amount: u32) {
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
