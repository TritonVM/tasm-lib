use std::collections::HashMap;

use triton_vm::prelude::*;

use crate::prelude::*;
use crate::traits::basic_snippet::Reviewer;
use crate::traits::basic_snippet::SignOffFingerprint;

/// [Subtraction][sub] for unsigned 128-bit integers.
///
/// # Behavior
///
/// ```text
/// BEFORE: _ [subtrahend: u128] [minuend: u128]
/// AFTER:  _ [difference: u128]
/// ```
///
/// # Preconditions
///
/// - the `minuend` is greater than or equal to the `subtrahend`
/// - all input arguments are properly [`BFieldCodec`] encoded
///
/// # Postconditions
///
/// - the output is the `minuend` minus the `subtrahend`
/// - the output is properly [`BFieldCodec`] encoded
///
/// [sub]: core::ops::Sub
#[derive(Debug, Default, Copy, Clone, Eq, PartialEq, Hash)]
pub struct Sub;

impl Sub {
    pub const OVERFLOW_ERROR_ID: i128 = 520;
}

impl BasicSnippet for Sub {
    fn parameters(&self) -> Vec<(DataType, String)> {
        ["subtrahend", "minuend"]
            .map(|s| (DataType::U128, s.to_string()))
            .to_vec()
    }

    fn return_values(&self) -> Vec<(DataType, String)> {
        vec![(DataType::U128, "difference".to_string())]
    }

    fn entrypoint(&self) -> String {
        "tasmlib_arithmetic_u128_sub".to_string()
    }

    fn code(&self, _: &mut Library) -> Vec<LabelledInstruction> {
        triton_asm!(
            // BEFORE: _ r_3 r_2 r_1 r_0 l_3 l_2 l_1 l_0
            // AFTER:  _ diff_3 diff_2 diff_1 diff_0
            {self.entrypoint()}:
                pick 4
                push -1
                mul
                add         // _ r_3 r_2 r_1 l_3 l_2 l_1 (l_0 - r_0)
                push {1_u64 << 32}
                add
                split       // _ r_3 r_2 r_1 l_3 l_2 l_1 !carry diff_0
                place 7     // _ diff_0 r_3 r_2 r_1 l_3 l_2 l_1 !carry

                push 0
                eq          // _ diff_0 r_3 r_2 r_1 l_3 l_2 l_1 carry
                pick 4
                add
                push -1
                mul         // _ diff_0 r_3 r_2 l_3 l_2 l_1 (-r_1-carry)
                add         // _ diff_0 r_3 r_2 l_3 l_2 (l_1-r_1-carry)
                push {1_u64 << 32}
                add
                split       // _ diff_0 r_3 r_2 l_3 l_2 !carry diff_1
                place 6     // _ diff_1 diff_0 r_3 r_2 l_3 l_2 !carry

                push 0
                eq          // _ diff_1 diff_0 r_3 r_2 l_3 l_2 carry
                pick 3
                add
                push -1
                mul         // _ diff_1 diff_0 r_3 l_3 l_2 (-r_2-carry)
                add         // _ diff_1 diff_0 r_3 l_3 (l_2-r_2-carry)
                push {1_u64 << 32}
                add
                split       // _ diff_1 diff_0 r_3 l_3 !carry diff_2
                place 5     // _ diff_2 diff_1 diff_0 r_3 l_3 !carry

                push 0
                eq          // _ diff_2 diff_1 diff_0 r_3 l_3 carry
                pick 2
                add
                push -1
                mul         // _ diff_2 diff_1 diff_0 l_3 (-r_3-carry)
                add
                split       // _ diff_2 diff_1 diff_0 overflow diff_3
                place 4     // _ [diff: u128] overflow

                push 0
                eq
                assert error_id {Self::OVERFLOW_ERROR_ID}
                            // _ [diff: u128]
                return
        )
    }

    fn sign_offs(&self) -> HashMap<Reviewer, SignOffFingerprint> {
        let mut sign_offs = HashMap::new();
        sign_offs.insert(Reviewer("ferdinand"), 0xd1effbb4a16f9979.into());
        sign_offs
    }
}

#[cfg(test)]
mod tests {
    use BFieldElement;

    use super::*;
    use crate::test_prelude::*;

    impl Closure for Sub {
        type Args = (u128, u128);

        fn rust_shadow(&self, stack: &mut Vec<BFieldElement>) {
            let (subtrahend, minuend) = pop_encodable::<Self::Args>(stack);
            let difference = minuend.checked_sub(subtrahend).unwrap();
            push_encodable(stack, &difference);
        }

        fn pseudorandom_args(
            &self,
            seed: [u8; 32],
            bench_case: Option<BenchmarkCase>,
        ) -> Self::Args {
            let Some(bench_case) = bench_case else {
                let mut rng = StdRng::from_seed(seed);
                let subtrahend = rng.random();
                let minuend = rng.random_range(subtrahend..=u128::MAX);
                return (subtrahend, minuend);
            };

            match bench_case {
                BenchmarkCase::CommonCase => (1 << 126, 1 << 127),
                BenchmarkCase::WorstCase => ((1 << 126) + (1 << 56), (1 << 127) + (1 << 64)),
            }
        }

        fn corner_case_args(&self) -> Vec<Self::Args> {
            vec![
                (0, 0),
                (0, 1),
                (1, 1),
                (1, 0xffff_ffff_ffff_ffff_ffff_ffff_0000_0000),
                (2, 0xffff_ffff_ffff_ffff_0000_0000_0000_0001),
                (3, 0xffff_ffff_0000_0000_0000_0000_0000_0002),
                (4, 1 << 127),
                (u32::MAX.into(), u32::MAX.into()),
                (u64::MAX.into(), u64::MAX.into()),
                (u128::MAX, u128::MAX),
            ]
        }
    }

    #[test]
    fn rust_shadow() {
        ShadowedClosure::new(Sub).test();
    }

    #[proptest]
    fn overflow_crashes_vm(
        #[strategy(1_u128..)] subtrahend: u128,
        #[strategy(..#subtrahend)] minuend: u128,
    ) {
        test_assertion_failure(
            &ShadowedClosure::new(Sub),
            InitVmState::with_stack(Sub.set_up_test_stack((subtrahend, minuend))),
            &[Sub::OVERFLOW_ERROR_ID],
        );
    }
}

#[cfg(test)]
mod benches {
    use super::*;
    use crate::test_prelude::*;

    #[test]
    fn benchmark() {
        ShadowedClosure::new(Sub).bench();
    }
}
