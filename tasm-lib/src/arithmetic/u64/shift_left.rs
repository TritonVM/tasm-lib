use std::collections::HashMap;

use triton_vm::prelude::*;

use crate::arithmetic::u64::shift_right::ShiftRight;
use crate::prelude::*;
use crate::traits::basic_snippet::Reviewer;
use crate::traits::basic_snippet::SignOffFingerprint;

/// [Shift left][shl] for unsigned 64-bit integers.
///
/// # Behavior
///
/// ```text
/// BEFORE: _ [arg: u64] shift_amount
/// AFTER:  _ [result: u64]
/// ```
///
/// # Preconditions
///
/// - input argument `arg` is properly [`BFieldCodec`] encoded
/// - input argument `shift_amount` is in `0..64`
///
/// # Postconditions
///
/// - the output is the input argument `arg` bit-shifted to the left by
///   input argument `shift_amount`
/// - the output is properly [`BFieldCodec`] encoded
///
/// [shl]: core::ops::Shl
#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub struct ShiftLeft;

impl ShiftLeft {
    pub const SHIFT_AMOUNT_TOO_BIG_ERROR_ID: i128 = 370;
}

impl BasicSnippet for ShiftLeft {
    fn parameters(&self) -> Vec<(DataType, String)> {
        ShiftRight.parameters()
    }

    fn return_values(&self) -> Vec<(DataType, String)> {
        ShiftRight.return_values()
    }

    fn entrypoint(&self) -> String {
        "tasmlib_arithmetic_u64_shift_left".to_string()
    }

    fn code(&self, _: &mut Library) -> Vec<LabelledInstruction> {
        let entrypoint = self.entrypoint();
        let handle_hi_shift = format!("{entrypoint}_handle_hi_shift");

        triton_asm!(
            // BEFORE: _ arg_hi arg_lo shift
            // AFTER:  _ (arg<< shift)_hi (arg<< shift)_lo
            {entrypoint}:
                /* bounds check */
                push 64
                dup 1
                lt
                assert error_id {Self::SHIFT_AMOUNT_TOO_BIG_ERROR_ID}
                // _ arg_hi arg_lo shift

                /* special case: shift amount is greater than 32 */
                dup 0
                push 32
                lt
                // _ arg_hi arg_lo shift (shift > 32)

                skiz call {handle_hi_shift}
                // _ arg_hi arg_lo shift
                // where 32 <= shift < 64

                push 2
                pow
                // _ arg_hi arg_lo (2^shift)

                pick 2
                dup 1
                mul
                // _ arg_lo (2^shift) (arg_hi << shift)_bfe

                split
                place 3
                pop 1
                // _ (arg_hi << shift) arg_lo (2^shift)

                mul
                split
                // _ (arg_hi << shift) carry (arg_lo << shift)

                place 2
                add
                pick 1
                // _ (arg << shift)_hi (arg_lo << shift)

                return

            // BEFORE: _ arg_hi arg_lo shift
            // AFTER:  _ (arg<< 32)_hi (arg<< 32)_lo (shift - 32)
            {handle_hi_shift}:
                addi -32
                // _ arg_hi arg_lo (shift - 32)

                pick 2
                pop 1
                push 0
                place 1
                // _ arg_lo         0            (shift - 32)
                // _ (arg<< 32)_hi (arg<< 32)_lo (shift - 32)

                return
        )
    }

    fn sign_offs(&self) -> HashMap<Reviewer, SignOffFingerprint> {
        let mut sign_offs = HashMap::new();
        sign_offs.insert(Reviewer("ferdinand"), 0x4a03ab75c8546370.into());
        sign_offs
    }
}

#[cfg(test)]
pub(crate) mod tests {
    use super::*;
    use crate::test_prelude::*;

    impl ShiftLeft {
        pub fn assert_expected_behavior(&self, shift_amount: u32, arg: u64) {
            let initial_stack = self.set_up_test_stack((arg, shift_amount));

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

    impl Closure for ShiftLeft {
        type Args = (u64, u32);

        fn rust_shadow(&self, stack: &mut Vec<BFieldElement>) {
            let (arg, shift_amount) = pop_encodable::<Self::Args>(stack);
            assert!(shift_amount < 64);
            push_encodable(stack, &(arg << shift_amount));
        }

        fn pseudorandom_args(
            &self,
            seed: [u8; 32],
            bench_case: Option<BenchmarkCase>,
        ) -> Self::Args {
            let mut rng = StdRng::from_seed(seed);

            match bench_case {
                Some(BenchmarkCase::CommonCase) => (0x642, 15),
                Some(BenchmarkCase::WorstCase) => (0x123456789abcdef, 33),
                None => (rng.random(), rng.random_range(0..64)),
            }
        }

        fn corner_case_args(&self) -> Vec<Self::Args> {
            (0..64).map(|i| (1, i)).collect()
        }
    }

    #[test]
    fn rust_shadow() {
        ShadowedClosure::new(ShiftLeft).test()
    }

    #[test]
    fn edge_cases() {
        for i in 0..64 {
            ShiftLeft.assert_expected_behavior(i, u64::MAX);
        }
    }

    #[proptest]
    fn property_test(arg: u64, #[strategy(0_u32..64)] shift_amount: u32) {
        ShiftLeft.assert_expected_behavior(shift_amount, arg);
    }

    #[proptest]
    fn negative_property_test(arg: u64, #[strategy(64_u32..)] shift_amount: u32) {
        test_assertion_failure(
            &ShadowedClosure::new(ShiftLeft),
            InitVmState::with_stack(ShiftLeft.set_up_test_stack((arg, shift_amount))),
            &[ShiftLeft::SHIFT_AMOUNT_TOO_BIG_ERROR_ID],
        );
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
