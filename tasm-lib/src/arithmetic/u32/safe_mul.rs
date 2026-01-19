use std::collections::HashMap;

use triton_vm::prelude::*;

use crate::prelude::*;
use crate::traits::basic_snippet::Reviewer;
use crate::traits::basic_snippet::SignOffFingerprint;

/// Multiply two `u32`s and crash on overflow.
///
/// ### Behavior
///
/// ```text
/// BEFORE: _ [right: 32] [left: u32]
/// AFTER:  _ [left · right: u32]
/// ```
///
/// ### Preconditions
///
/// - all input arguments are properly [`BFieldCodec`] encoded
/// - the product of `left` and `right` is less than or equal to [`u32::MAX`]
///
/// ### Postconditions
///
/// - the output is the product of the input
/// - the output is properly [`BFieldCodec`] encoded
#[derive(Debug, Default, Copy, Clone, Eq, PartialEq, Hash)]
pub struct SafeMul;

impl SafeMul {
    pub const OVERFLOW_ERROR_ID: i128 = 460;
}

impl BasicSnippet for SafeMul {
    fn parameters(&self) -> Vec<(DataType, String)> {
        ["right", "left"]
            .map(|s| (DataType::U32, s.to_string()))
            .to_vec()
    }

    fn return_values(&self) -> Vec<(DataType, String)> {
        vec![(DataType::U32, "left · right".to_string())]
    }

    fn entrypoint(&self) -> String {
        "tasmlib_arithmetic_u32_safe_mul".to_string()
    }

    fn code(&self, _: &mut Library) -> Vec<LabelledInstruction> {
        triton_asm!(
            // BEFORE: _ right left
            // AFTER:  _ product
            {self.entrypoint()}:
                mul
                dup 0  // _ product product
                split  // _ product hi lo
                pop 1  // _ product hi
                push 0 // _ product hi 0
                eq     // _ product (hi == 0)
                assert error_id {Self::OVERFLOW_ERROR_ID}
                return
        )
    }

    fn sign_offs(&self) -> HashMap<Reviewer, SignOffFingerprint> {
        let mut sign_offs = HashMap::new();
        sign_offs.insert(Reviewer("ferdinand"), 0x502e4ccfdbf1b531.into());
        sign_offs
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_prelude::*;

    impl Closure for SafeMul {
        type Args = (u32, u32);

        fn rust_shadow(&self, stack: &mut Vec<BFieldElement>) {
            let (right, left) = pop_encodable::<Self::Args>(stack);
            let product = left.checked_mul(right).unwrap();
            push_encodable(stack, &product);
        }

        fn pseudorandom_args(
            &self,
            seed: [u8; 32],
            bench_case: Option<BenchmarkCase>,
        ) -> Self::Args {
            let Some(bench_case) = bench_case else {
                let mut rng = StdRng::from_seed(seed);
                let left = rng.random_range(1..=u32::MAX);
                let right = rng.random_range(0..=u32::MAX / left);

                return (right, left);
            };

            match bench_case {
                BenchmarkCase::CommonCase => (1 << 8, 1 << 9),
                BenchmarkCase::WorstCase => (1 << 15, 1 << 16),
            }
        }

        fn corner_case_args(&self) -> Vec<Self::Args> {
            [0, 1]
                .into_iter()
                .cartesian_product([0, 1, u32::MAX])
                .collect()
        }
    }

    #[test]
    fn rust_shadow() {
        ShadowedClosure::new(SafeMul).test();
    }

    #[proptest]
    fn overflow_crashes_vm(
        #[strategy(1_u32..)] left: u32,
        #[strategy(u32::MAX / #left..)]
        #[filter(#left.checked_mul(#right).is_none())]
        right: u32,
    ) {
        test_assertion_failure(
            &ShadowedClosure::new(SafeMul),
            InitVmState::with_stack(SafeMul.set_up_test_stack((left, right))),
            &[SafeMul::OVERFLOW_ERROR_ID],
        )
    }
}

#[cfg(test)]
mod benches {
    use super::*;
    use crate::test_prelude::*;

    #[test]
    fn safe_mul_benchmark() {
        ShadowedClosure::new(SafeMul).bench();
    }
}
