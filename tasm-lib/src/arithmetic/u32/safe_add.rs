use std::collections::HashMap;

use triton_vm::prelude::*;

use crate::prelude::*;
use crate::traits::basic_snippet::Reviewer;
use crate::traits::basic_snippet::SignOffFingerprint;

/// Add two `u32`s and crash on overflow.
///
/// ### Behavior
///
/// ```text
/// BEFORE: _ [right: 32] [left: u32]
/// AFTER:  _ [left + right: u32]
/// ```
///
/// ### Preconditions
///
/// - all input arguments are properly [`BFieldCodec`] encoded
/// - the sum of `left` and `right` is less than or equal to [`u32::MAX`]
///
/// ### Postconditions
///
/// - the output is the sum of the input
/// - the output is properly [`BFieldCodec`] encoded
#[derive(Debug, Default, Copy, Clone, Eq, PartialEq, Hash)]
pub struct SafeAdd;

impl SafeAdd {
    pub const OVERFLOW_ERROR_ID: i128 = 450;
}

impl BasicSnippet for SafeAdd {
    fn parameters(&self) -> Vec<(DataType, String)> {
        ["right", "left"]
            .map(|s| (DataType::U32, s.to_string()))
            .to_vec()
    }

    fn return_values(&self) -> Vec<(DataType, String)> {
        vec![(DataType::U32, "left + right".to_string())]
    }

    fn entrypoint(&self) -> String {
        "tasmlib_arithmetic_u32_safe_add".to_string()
    }

    fn code(&self, _: &mut Library) -> Vec<LabelledInstruction> {
        triton_asm!(
            {self.entrypoint()}:
                add    // _ sum
                dup 0  // _ sum sum
                split  // _ sum hi lo
                pop 1  // _ sum hi
                push 0 // _ sum hi 0
                eq     // _ sum (hi == 0)
                assert error_id {Self::OVERFLOW_ERROR_ID}
                return
        )
    }

    fn sign_offs(&self) -> HashMap<Reviewer, SignOffFingerprint> {
        let mut sign_offs = HashMap::new();
        sign_offs.insert(Reviewer("ferdinand"), 0xa54b9cd25d7dd941.into());
        sign_offs
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_prelude::*;

    impl Closure for SafeAdd {
        type Args = (u32, u32);

        fn rust_shadow(&self, stack: &mut Vec<BFieldElement>) {
            let (right, left) = pop_encodable::<Self::Args>(stack);
            let sum = left.checked_add(right).unwrap();
            push_encodable(stack, &sum);
        }

        fn pseudorandom_args(
            &self,
            seed: [u8; 32],
            bench_case: Option<BenchmarkCase>,
        ) -> Self::Args {
            let Some(bench_case) = bench_case else {
                let mut rng = StdRng::from_seed(seed);
                let left = rng.random();
                let right = rng.random_range(0..=u32::MAX - left);

                return (right, left);
            };

            match bench_case {
                BenchmarkCase::CommonCase => (1 << 16, 1 << 15),
                BenchmarkCase::WorstCase => (u32::MAX >> 1, u32::MAX >> 2),
            }
        }

        fn corner_case_args(&self) -> Vec<Self::Args> {
            vec![(0, u32::MAX)]
        }
    }

    #[test]
    fn rust_shadow() {
        ShadowedClosure::new(SafeAdd).test();
    }

    #[proptest]
    fn overflow_crashes_vm(
        #[filter(#left != 0)] left: u32,
        #[strategy(u32::MAX - #left + 1..)] right: u32,
    ) {
        debug_assert!(left.checked_add(right).is_none());
        test_assertion_failure(
            &ShadowedClosure::new(SafeAdd),
            InitVmState::with_stack(SafeAdd.set_up_test_stack((left, right))),
            &[SafeAdd::OVERFLOW_ERROR_ID],
        )
    }
}

#[cfg(test)]
mod benches {
    use super::*;
    use crate::test_prelude::*;

    #[test]
    fn safe_add_benchmark() {
        ShadowedClosure::new(SafeAdd).bench();
    }
}
