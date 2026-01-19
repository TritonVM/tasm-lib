use std::collections::HashMap;

use triton_vm::prelude::*;

use crate::arithmetic::u64::overflowing_sub::OverflowingSub;
use crate::prelude::*;
use crate::traits::basic_snippet::Reviewer;
use crate::traits::basic_snippet::SignOffFingerprint;

/// [Subtraction][sub] for unsigned 64-bit integers.
///
/// # Behavior
///
/// ```text
/// BEFORE: _ [subtrahend: u64] [minuend: u64]
/// AFTER:  _ [difference: u64]
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
#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub struct Sub;

impl Sub {
    pub const OVERFLOW_ERROR_ID: i128 = 340;
}

impl BasicSnippet for Sub {
    fn parameters(&self) -> Vec<(DataType, String)> {
        OverflowingSub.parameters()
    }

    fn return_values(&self) -> Vec<(DataType, String)> {
        vec![(DataType::U64, "difference".to_string())]
    }

    fn entrypoint(&self) -> String {
        "tasmlib_arithmetic_u64_sub".to_string()
    }

    fn code(&self, _: &mut Library) -> Vec<LabelledInstruction> {
        triton_asm!(
            // BEFORE: _ subtrahend_hi subtrahend_lo minuend_hi minuend_lo
            // AFTER:  _ difference_hi difference_lo
            {self.entrypoint()}:
                {&OverflowingSub::common_subtraction_code()}
                // _ difference_lo (minuend_hi - subtrahend_hi - carry)

                split
                place 2
                // _ difference_hi difference_lo only_0_if_no_overflow

                push 0
                eq
                assert error_id {Self::OVERFLOW_ERROR_ID}
                // _ difference_hi difference_lo

                return
        )
    }

    fn sign_offs(&self) -> HashMap<Reviewer, SignOffFingerprint> {
        let mut sign_offs = HashMap::new();
        sign_offs.insert(Reviewer("ferdinand"), 0x8dad59997cb49bd9.into());
        sign_offs
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_prelude::*;

    impl Sub {
        pub fn assert_expected_behavior(&self, subtrahend: u64, minuend: u64) {
            let mut expected_stack = self.set_up_test_stack((subtrahend, minuend));
            self.rust_shadow(&mut expected_stack);

            test_rust_equivalence_given_complete_state(
                &ShadowedClosure::new(Self),
                &self.set_up_test_stack((subtrahend, minuend)),
                &[],
                &NonDeterminism::default(),
                &None,
                Some(&expected_stack),
            );
        }
    }

    impl Closure for Sub {
        type Args = <OverflowingSub as Closure>::Args;

        fn rust_shadow(&self, stack: &mut Vec<BFieldElement>) {
            let (subtrahend, minuend) = pop_encodable::<Self::Args>(stack);
            push_encodable(stack, &(minuend - subtrahend));
        }

        fn pseudorandom_args(
            &self,
            seed: [u8; 32],
            bench_case: Option<BenchmarkCase>,
        ) -> Self::Args {
            let Some(bench_case) = bench_case else {
                let mut rng = StdRng::from_seed(seed);
                let subtrahend = rng.random();
                let minuend = rng.random_range(subtrahend..=u64::MAX);
                return (subtrahend, minuend);
            };

            match bench_case {
                BenchmarkCase::CommonCase => (0x3ff, 0x7fff_ffff),
                BenchmarkCase::WorstCase => (0x1_7fff_ffff, 0x64_0000_03ff),
            }
        }

        fn corner_case_args(&self) -> Vec<Self::Args> {
            let edge_case_values = OverflowingSub::edge_case_values();

            edge_case_values
                .iter()
                .cartesian_product(&edge_case_values)
                .filter(|&(&subtrahend, &minuend)| minuend.checked_sub(subtrahend).is_some())
                .map(|(&subtrahend, &minuend)| (subtrahend, minuend))
                .collect()
        }
    }

    #[test]
    fn rust_shadow() {
        ShadowedClosure::new(Sub).test();
    }

    #[test]
    fn unit_test() {
        Sub.assert_expected_behavior(129, 256);
        Sub.assert_expected_behavior(1, 1 << 32);
    }

    #[proptest]
    fn property_test(subtrahend: u64, #[strategy(#subtrahend..)] minuend: u64) {
        Sub.assert_expected_behavior(subtrahend, minuend);
    }

    #[proptest]
    fn negative_property_test(
        #[strategy(1_u64..)] subtrahend: u64,
        #[strategy(..#subtrahend)] minuend: u64,
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
