use std::collections::HashMap;

use triton_vm::prelude::*;

use crate::prelude::*;
use crate::traits::basic_snippet::Reviewer;
use crate::traits::basic_snippet::SignOffFingerprint;

/// [Shift right][shr] for unsigned 32-bit integers.
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
/// - the output is the input argument `arg` bit-shifted to the right by
///   input argument `shift_amount`
/// - the output is properly [`BFieldCodec`] encoded
///
/// [shr]: core::ops::Shr
#[derive(Debug, Default, Copy, Clone, Eq, PartialEq, Hash)]
pub struct ShiftRight;

impl ShiftRight {
    pub const SHIFT_AMOUNT_TOO_BIG_ERROR_ID: i128 = 490;
}

impl BasicSnippet for ShiftRight {
    fn parameters(&self) -> Vec<(DataType, String)> {
        let arg = (DataType::U32, "arg".to_string());
        let shift_amount = (DataType::U32, "shift_amount".to_string());

        vec![arg, shift_amount]
    }

    fn return_values(&self) -> Vec<(DataType, String)> {
        vec![(DataType::U32, "shifted_arg".to_string())]
    }

    fn entrypoint(&self) -> String {
        "tasmlib_arithmetic_u32_shift_right".to_string()
    }

    fn code(&self, _: &mut Library) -> Vec<LabelledInstruction> {
        triton_asm!(
            // BEFORE: _ value shift
            // AFTER:  _ (value >> shift)
            {self.entrypoint()}:
                /* bounds check mimics Rust's behavior */
                push 32
                dup 1
                lt
                assert error_id {Self::SHIFT_AMOUNT_TOO_BIG_ERROR_ID}

                /* for an explanation of how & why this works, see `u64::shift_right` */
                push -1
                mul
                addi 32
                push 2
                pow
                // _ value (2 ^ (32 - shift))

                mul
                // _ (value << (32 - shift))

                split
                pop 1
                // _ (value >> shift))

                return
        )
    }

    fn sign_offs(&self) -> HashMap<Reviewer, SignOffFingerprint> {
        let mut sign_offs = HashMap::new();
        sign_offs.insert(Reviewer("ferdinand"), 0x1b65003ff2618480.into());
        sign_offs
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_prelude::*;

    impl Closure for ShiftRight {
        type Args = (u32, u32);

        fn rust_shadow(&self, stack: &mut Vec<BFieldElement>) {
            let (arg, shift_amount) = pop_encodable::<Self::Args>(stack);
            assert!(shift_amount < 32);
            push_encodable(stack, &(arg >> shift_amount));
        }

        fn pseudorandom_args(
            &self,
            seed: [u8; 32],
            bench_case: Option<BenchmarkCase>,
        ) -> Self::Args {
            let mut rng = StdRng::from_seed(seed);
            match bench_case {
                Some(BenchmarkCase::CommonCase) => ((1 << 16) - 1, 16),
                Some(BenchmarkCase::WorstCase) => (u32::MAX, 1),
                None => (rng.random(), rng.random_range(0..32)),
            }
        }

        fn corner_case_args(&self) -> Vec<Self::Args> {
            (0..32).map(|i| (u32::MAX, i)).collect()
        }
    }

    #[test]
    fn rust_shadow() {
        ShadowedClosure::new(ShiftRight).test();
    }

    #[proptest]
    fn too_big_shift_amount_crashes_vm(arg: u32, #[strategy(32_u32..)] shift_amount: u32) {
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
