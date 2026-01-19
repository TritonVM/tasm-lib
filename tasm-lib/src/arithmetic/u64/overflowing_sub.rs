use std::collections::HashMap;

use triton_vm::prelude::*;

use crate::prelude::*;
use crate::traits::basic_snippet::Reviewer;
use crate::traits::basic_snippet::SignOffFingerprint;

/// [Overflowing subtraction][sub] for unsigned 64-bit integers.
///
/// # Behavior
///
/// ```text
/// BEFORE: _ [subtrahend: u64] [minuend: u64]
/// AFTER:  _ [difference: u64] [is_overflow: bool]
/// ```
///
/// # Preconditions
///
/// - all input arguments are properly [`BFieldCodec`] encoded
///
/// # Postconditions
///
/// - the output `difference` is the `minuend` minus the `subtrahend`
/// - the output `is_overflow` is `true` if and only if the minuend is greater
///   than the subtrahend
/// - the output is properly [`BFieldCodec`] encoded
///
/// [sub]: u64::overflowing_sub
#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub struct OverflowingSub;

impl OverflowingSub {
    /// The code shared between [`crate::arithmetic::u64::sub::Sub`],
    /// [`crate::arithmetic::u64::wrapping_sub::WrappingSub`], and
    /// [`OverflowingSub`]. Take care to treat the `difference_hi` correctly,
    /// depending on how you want to handle overflow.
    ///
    /// ```text
    /// BEFORE: _ subtrahend_hi subtrahend_lo minuend_hi minuend_lo
    /// AFTER:  _ difference_lo (minuend_hi - subtrahend_hi - carry)
    /// ```
    pub(crate) fn common_subtraction_code() -> Vec<LabelledInstruction> {
        triton_asm! {
            // BEFORE: _ subtrahend_hi subtrahend_lo minuend_hi minuend_lo
            // AFTER:  _ difference_hi difference_lo is_overflow
            pick 2
            // _ subtrahend_hi minuend_hi minuend_lo subtrahend_lo

            push -1
            mul
            add
            // _ subtrahend_hi minuend_hi (minuend_lo - subtrahend_lo)

            /* Any overflow manifests in the high limb. By adding 2^32, this high limb
             * is “pushed back” to be either 0 or 1; 1 in the case where _no_ overflow
             * has occurred, and 0 if overflow has occurred.
             *
             * To be honest, I don't fully understand all the subtlety going on here.
             * However, all the edge cases that I have identified pass all the tests,
             * indicating that things are fine. 👍
             */
            addi {1_u64 << 32}
            split
            // _ subtrahend_hi minuend_hi !carry difference_lo

            place 3
            // _ difference_lo subtrahend_hi minuend_hi !carry

            push 0
            eq
            // _ difference_lo subtrahend_hi minuend_hi carry

            pick 2
            add
            // _ difference_lo minuend_hi (subtrahend_hi + carry)

            push -1
            mul
            add
            // _ difference_lo (minuend_hi - subtrahend_hi - carry)
        }
    }
}

impl BasicSnippet for OverflowingSub {
    fn parameters(&self) -> Vec<(DataType, String)> {
        ["subtrahend", "minuend"]
            .map(|s| (DataType::U64, s.to_string()))
            .to_vec()
    }

    fn return_values(&self) -> Vec<(DataType, String)> {
        vec![
            (DataType::U64, "wrapped_diff".to_string()),
            (DataType::Bool, "is_overflow".to_string()),
        ]
    }

    fn entrypoint(&self) -> String {
        "tasmlib_arithmetic_u64_overflowing_sub".to_string()
    }

    fn code(&self, _: &mut Library) -> Vec<LabelledInstruction> {
        triton_asm!(
            {self.entrypoint()}:
                {&Self::common_subtraction_code()}
                // _ difference_lo (minuend_hi - subtrahend_hi - carry)

                addi {1_u64 << 32}
                split
                // _ difference_lo !is_overflow difference_hi

                place 2
                // _ difference_hi difference_lo !is_overflow

                push 0
                eq
                // _ difference_hi difference_lo is_overflow

                return
        )
    }

    fn sign_offs(&self) -> HashMap<Reviewer, SignOffFingerprint> {
        let mut sign_offs = HashMap::new();
        sign_offs.insert(Reviewer("ferdinand"), 0xad11205197978c1d.into());
        sign_offs
    }
}

#[cfg(test)]
pub(crate) mod tests {
    use super::*;
    use crate::test_prelude::*;

    impl OverflowingSub {
        pub fn edge_case_values() -> Vec<u64> {
            let wiggle_edge_case_point = |p: u64| {
                [
                    p.checked_sub(3),
                    p.checked_sub(2),
                    p.checked_sub(1),
                    Some(p),
                    p.checked_add(1),
                    p.checked_add(2),
                    p.checked_add(3),
                ]
            };

            [1, 1 << 32, 1 << 33, 1 << 34, 1 << 40, 1 << 63, u64::MAX]
                .into_iter()
                .flat_map(wiggle_edge_case_point)
                .flatten()
                .collect()
        }
    }

    impl Closure for OverflowingSub {
        type Args = (u64, u64);

        fn rust_shadow(&self, stack: &mut Vec<BFieldElement>) {
            let (subtrahend, minuend) = pop_encodable::<Self::Args>(stack);
            push_encodable(stack, &minuend.overflowing_sub(subtrahend));
        }

        fn pseudorandom_args(
            &self,
            seed: [u8; 32],
            bench_case: Option<BenchmarkCase>,
        ) -> Self::Args {
            match bench_case {
                Some(BenchmarkCase::CommonCase) => ((1 << 63) - 1, 1 << 63),
                Some(BenchmarkCase::WorstCase) => (1 << 50, 1 << 63),
                None => StdRng::from_seed(seed).random(),
            }
        }

        fn corner_case_args(&self) -> Vec<Self::Args> {
            let edge_case_values = Self::edge_case_values();

            edge_case_values
                .iter()
                .cartesian_product(&edge_case_values)
                .map(|(&subtrahend, &minuend)| (subtrahend, minuend))
                .collect()
        }
    }

    #[test]
    fn rust_shadow() {
        ShadowedClosure::new(OverflowingSub).test()
    }
}

#[cfg(test)]
mod benches {
    use super::*;
    use crate::test_prelude::*;

    #[test]
    fn benchmark() {
        ShadowedClosure::new(OverflowingSub).bench()
    }
}
