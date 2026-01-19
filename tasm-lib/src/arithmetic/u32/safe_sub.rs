use std::collections::HashMap;

use triton_vm::prelude::*;

use crate::prelude::*;
use crate::traits::basic_snippet::Reviewer;
use crate::traits::basic_snippet::SignOffFingerprint;

/// Subtract two `u32`s and crash on overflow.
///
/// ### Behavior
///
/// ```text
/// BEFORE: _ [right: 32] [left: u32]
/// AFTER:  _ [left - right: u32]
/// ```
///
/// ### Preconditions
///
/// - all input arguments are properly [`BFieldCodec`] encoded
/// - `left` is greater than or equal to `right`
///
/// ### Postconditions
///
/// - the output is properly [`BFieldCodec`] encoded
#[derive(Debug, Default, Copy, Clone, Eq, PartialEq, Hash)]
pub struct SafeSub;

impl SafeSub {
    pub const OVERFLOW_ERROR_ID: i128 = 470;
}

impl BasicSnippet for SafeSub {
    fn parameters(&self) -> Vec<(DataType, String)> {
        ["right", "left"]
            .map(|s| (DataType::U32, s.to_string()))
            .to_vec()
    }

    fn return_values(&self) -> Vec<(DataType, String)> {
        vec![(DataType::U32, "left - right".to_string())]
    }

    fn entrypoint(&self) -> String {
        "tasmlib_arithmetic_u32_safe_sub".to_string()
    }

    fn code(&self, _: &mut Library) -> Vec<LabelledInstruction> {
        triton_asm!(
            // BEFORE: _ right left
            // AFTER:  _ (left - right)
            {self.entrypoint()}:
                pick 1
                push -1
                mul
                add
                dup 0   // _ diff diff
                split   // _ diff hi lo
                pop 1   // _ diff hi
                push 0  // _ diff hi 0
                eq      // _ diff (hi == 0)
                assert error_id {Self::OVERFLOW_ERROR_ID}
                return
        )
    }

    fn sign_offs(&self) -> HashMap<Reviewer, SignOffFingerprint> {
        let mut sign_offs = HashMap::new();
        sign_offs.insert(Reviewer("ferdinand"), 0x75a4191cfa996c3f.into());
        sign_offs
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_prelude::*;

    impl Closure for SafeSub {
        type Args = (u32, u32);

        fn rust_shadow(&self, stack: &mut Vec<BFieldElement>) {
            let (right, left) = pop_encodable::<Self::Args>(stack);
            let diff = left.checked_sub(right).unwrap();
            push_encodable(stack, &diff);
        }

        fn pseudorandom_args(
            &self,
            seed: [u8; 32],
            bench_case: Option<BenchmarkCase>,
        ) -> Self::Args {
            let Some(bench_case) = bench_case else {
                let mut rng = StdRng::from_seed(seed);
                let left = rng.random();
                let right = rng.random_range(0..=left);

                return (right, left);
            };

            match bench_case {
                BenchmarkCase::CommonCase => (1 << 15, 1 << 16),
                BenchmarkCase::WorstCase => (0, u32::MAX),
            }
        }

        fn corner_case_args(&self) -> Vec<Self::Args> {
            vec![(0, 0), (0, u32::MAX), (u32::MAX, u32::MAX)]
        }
    }

    #[test]
    fn rust_shadow() {
        ShadowedClosure::new(SafeSub).test();
    }

    #[proptest]
    fn overflow_crashes_vm(
        #[filter(#left != u32::MAX)] left: u32,
        #[strategy(#left..)] right: u32,
    ) {
        test_assertion_failure(
            &ShadowedClosure::new(SafeSub),
            InitVmState::with_stack(SafeSub.set_up_test_stack((right, left))),
            &[SafeSub::OVERFLOW_ERROR_ID],
        )
    }
}

#[cfg(test)]
mod benches {
    use super::*;
    use crate::test_prelude::*;

    #[test]
    fn safe_sub_benchmark() {
        ShadowedClosure::new(SafeSub).bench();
    }
}
