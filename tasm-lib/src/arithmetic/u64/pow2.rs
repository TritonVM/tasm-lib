use std::collections::HashMap;

use triton_vm::prelude::*;

use crate::prelude::*;
use crate::traits::basic_snippet::Reviewer;
use crate::traits::basic_snippet::SignOffFingerprint;

/// Raise 2 to the power of the given exponent.
///
/// ### Behavior
///
/// ```text
/// BEFORE: _ [arg: u32]
/// AFTER:  _ [2^arg: u64]
/// ```
///
/// ### Preconditions
///
/// - the input `arg` is less than 64
///
/// ### Postconditions
///
/// - the output is properly [`BFieldCodec`] encoded
#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
pub struct Pow2;

impl Pow2 {
    pub const INPUT_TOO_LARGE_ERROR_ID: i128 = 360;
}

impl BasicSnippet for Pow2 {
    fn parameters(&self) -> Vec<(DataType, String)> {
        vec![(DataType::U32, "arg".to_string())]
    }

    fn return_values(&self) -> Vec<(DataType, String)> {
        vec![(DataType::U64, "(2^arg)".to_string())]
    }

    fn entrypoint(&self) -> String {
        "tasmlib_arithmetic_u64_pow2".to_string()
    }

    fn code(&self, _: &mut Library) -> Vec<LabelledInstruction> {
        triton_asm! {
            {self.entrypoint()}:
                push 64
                dup 1
                lt
                assert error_id {Self::INPUT_TOO_LARGE_ERROR_ID}

                push 2
                pow
                split
                return
        }
    }

    fn sign_offs(&self) -> HashMap<Reviewer, SignOffFingerprint> {
        let mut sign_offs = HashMap::new();
        sign_offs.insert(Reviewer("ferdinand"), 0xcb940285f823bded.into());
        sign_offs
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_prelude::*;

    impl Pow2 {
        fn assert_expected_behavior(&self, arg: u32) {
            let initial_stack = self.set_up_test_stack(arg);

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

    impl Closure for Pow2 {
        type Args = u32;

        fn rust_shadow(&self, stack: &mut Vec<BFieldElement>) {
            let arg = pop_encodable::<Self::Args>(stack);
            push_encodable(stack, &2_u64.pow(arg));
        }

        fn pseudorandom_args(
            &self,
            seed: [u8; 32],
            bench_case: Option<BenchmarkCase>,
        ) -> Self::Args {
            let Some(bench_case) = bench_case else {
                return StdRng::from_seed(seed).random_range(0..64);
            };

            match bench_case {
                BenchmarkCase::CommonCase => 31,
                BenchmarkCase::WorstCase => 63,
            }
        }
    }

    #[test]
    fn rust_shadow() {
        ShadowedClosure::new(Pow2).test();
    }

    #[test]
    fn unit_test() {
        for arg in 0..64 {
            Pow2.assert_expected_behavior(arg);
        }
    }

    #[proptest]
    fn negative_property_test(#[strategy(64_u32..)] arg: u32) {
        let stack = Pow2.set_up_test_stack(arg);

        test_assertion_failure(
            &ShadowedClosure::new(Pow2),
            InitVmState::with_stack(stack),
            &[Pow2::INPUT_TOO_LARGE_ERROR_ID],
        );
    }
}

#[cfg(test)]
mod benches {
    use super::*;
    use crate::test_prelude::*;

    #[test]
    fn benchmark() {
        ShadowedClosure::new(Pow2).bench();
    }
}
