use std::collections::HashMap;

use triton_vm::prelude::*;

use crate::data_type::DataType;
use crate::prelude::*;
use crate::traits::basic_snippet::Reviewer;
use crate::traits::basic_snippet::SignOffFingerprint;

/// [Shift right][shr] for unsigned 64-bit integers.
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
/// - the output is the input argument `arg` bit-shifted to the right by
///   input argument `shift_amount`
/// - the output is properly [`BFieldCodec`] encoded
///
/// [shr]: core::ops::Shr
#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub struct ShiftRight;

impl ShiftRight {
    pub const SHIFT_AMOUNT_TOO_BIG_ERROR_ID: i128 = 330;
}

impl BasicSnippet for ShiftRight {
    fn inputs(&self) -> Vec<(DataType, String)> {
        let arg = (DataType::U64, "arg".to_string());
        let shift_amount = (DataType::U32, "shift_amount".to_string());

        vec![arg, shift_amount]
    }

    fn outputs(&self) -> Vec<(DataType, String)> {
        vec![(DataType::U64, "shifted_arg".to_string())]
    }

    fn entrypoint(&self) -> String {
        "tasmlib_arithmetic_u64_shift_right".to_string()
    }

    fn code(&self, _: &mut Library) -> Vec<LabelledInstruction> {
        let entrypoint = self.entrypoint();
        let shift_amount_gt_32 = format!("{entrypoint}_shift_amount_gt_32");

        triton_asm!(
            // BEFORE: _ arg_hi arg_lo shift
            // AFTER:  _ (arg >> shift)_hi (arg >> shift)_lo
            {entrypoint}:
                /* bounds check */
                push 64
                dup 1
                lt
                assert error_id {Self::SHIFT_AMOUNT_TOO_BIG_ERROR_ID}
                // _ arg_hi arg_lo shift

                /* special case if shift amount is greater than 32 */
                dup 0
                push 32
                lt
                // _ arg_hi arg_lo shift (32 < shift)

                skiz
                    call {shift_amount_gt_32}
                // _ arg_hi arg_lo shift

                /* Over a finite field, both right shift and integer division are difficult.
                 * However, integer multiplication and therefore left shift are easy, provided
                 * the field elements are within the right ranges.
                 * General strategy: multiply by the correct power of 2 to shift left by
                 * (32 - shift_amount), then `split` and throw away the low limb. For example,
                 * 0b10_0001 >> 5 gives 0b1, as does a left shift by 27 after throwing away the
                 * low limb:
                 *
                 *   0b10_0001 << 27 = 0b1_0000_1000_0000_0000_0000_0000_0000_0000
                 *                         ╰─────────────╴ low limb ╶────────────╯
                 */
                push -1
                mul
                addi 32
                // _ arg_hi arg_lo (32 - shift)

                push 2
                pow
                // _ arg_hi arg_lo (2^(32 - shift))

                pick 2
                dup 1
                mul
                // _ arg_lo (2^(32 - shift)) (arg_hi << (32 - shift))

                split
                // _ arg_lo (2^(32 - shift)) (arg_hi >> shift) carry
                // _ arg_lo (2^(32 - shift)) (arg >> shift)_hi carry

                place 3
                place 3
                // _ (arg >> shift)_hi carry arg_lo (2^(32 - shift))

                mul
                split
                pop 1
                // _ (arg >> shift)_hi carry (arg_lo >> shift)

                add
                // _ (arg >> shift)_hi (arg >> shift)_lo

                return

            // BEFORE: _ arg_hi arg_lo shift
            // AFTER:  _ 0      arg_hi (shift - 32)
            {shift_amount_gt_32}:
                addi -32
                // _ arg_hi arg_lo (shift - 32)

                pick 1
                pop 1
                // _ arg_hi (shift - 32)

                push 0
                place 2
                // _ 0 arg_hi (shift - 32)

                return
        )
    }

    fn sign_offs(&self) -> HashMap<Reviewer, SignOffFingerprint> {
        let mut sign_offs = HashMap::new();
        sign_offs.insert(Reviewer("ferdinand"), 0xa1531b9db1f3f021.into());
        sign_offs
    }
}

#[cfg(test)]
mod tests {
    use rand::prelude::*;
    use test_strategy::proptest;

    use super::*;
    use crate::pop_encodable;
    use crate::push_encodable;
    use crate::snippet_bencher::BenchmarkCase;
    use crate::test_helpers::test_assertion_failure;
    use crate::test_helpers::test_rust_equivalence_given_complete_state;
    use crate::traits::closure::Closure;
    use crate::traits::closure::ShadowedClosure;
    use crate::traits::rust_shadow::RustShadow;
    use crate::InitVmState;

    impl ShiftRight {
        pub fn set_up_initial_stack(&self, arg: u64, shift_amount: u32) -> Vec<BFieldElement> {
            let mut stack = self.init_stack_for_isolated_run();
            push_encodable(&mut stack, &arg);
            push_encodable(&mut stack, &shift_amount);

            stack
        }

        pub fn assert_expected_shift_right_behavior(&self, arg: u64, shift_amount: u32) {
            let mut expected_stack = self.init_stack_for_isolated_run();
            push_encodable(&mut expected_stack, &(arg >> shift_amount));

            test_rust_equivalence_given_complete_state(
                &ShadowedClosure::new(Self),
                &self.set_up_initial_stack(arg, shift_amount),
                &[],
                &NonDeterminism::default(),
                &None,
                Some(&expected_stack),
            );
        }
    }

    impl Closure for ShiftRight {
        fn rust_shadow(&self, stack: &mut Vec<BFieldElement>) {
            let shift_amount = pop_encodable::<u32>(stack);
            assert!(shift_amount < 64);

            let arg = pop_encodable::<u64>(stack);
            push_encodable(stack, &(arg >> shift_amount));
        }

        fn pseudorandom_initial_state(
            &self,
            seed: [u8; 32],
            bench_case: Option<BenchmarkCase>,
        ) -> Vec<BFieldElement> {
            let mut rng = StdRng::from_seed(seed);

            let (arg, shift_amount) = match bench_case {
                Some(BenchmarkCase::CommonCase) => (0x642, 15),
                Some(BenchmarkCase::WorstCase) => (0x123, 33),
                None => (rng.gen(), rng.gen_range(0..64)),
            };

            self.set_up_initial_stack(arg, shift_amount)
        }

        fn corner_case_initial_states(&self) -> Vec<Vec<BFieldElement>> {
            (0..64)
                .map(|i| self.set_up_initial_stack(1 << i, i))
                .collect()
        }
    }

    #[test]
    fn rust_shadow() {
        ShadowedClosure::new(ShiftRight).test()
    }

    #[test]
    fn unit_test() {
        ShiftRight.assert_expected_shift_right_behavior(8, 2);
    }

    #[proptest]
    fn property_test(arg: u64, #[strategy(0_u32..64)] shift_amount: u32) {
        ShiftRight.assert_expected_shift_right_behavior(arg, shift_amount);
    }

    #[proptest]
    fn negative_property_test(arg: u64, #[strategy(64_u32..)] shift_amount: u32) {
        test_assertion_failure(
            &ShadowedClosure::new(ShiftRight),
            InitVmState::with_stack(ShiftRight.set_up_initial_stack(arg, shift_amount)),
            &[ShiftRight::SHIFT_AMOUNT_TOO_BIG_ERROR_ID],
        );
    }
}

#[cfg(test)]
mod benches {
    use super::*;
    use crate::traits::closure::ShadowedClosure;
    use crate::traits::rust_shadow::RustShadow;

    #[test]
    fn benchmark() {
        ShadowedClosure::new(ShiftRight).bench();
    }
}
