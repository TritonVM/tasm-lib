use std::collections::HashMap;

use triton_vm::prelude::*;

use crate::arithmetic::u64::overflowing_sub::OverflowingSub;
use crate::data_type::DataType;
use crate::prelude::*;
use crate::traits::basic_snippet::Reviewer;
use crate::traits::basic_snippet::SignOffFingerprint;

/// [Wrapping subtraction][sub] for unsigned 64-bit integers.
///
/// # Behavior
///
/// ```text
/// BEFORE: _ [subtrahend: u64] [minuend: u64]
/// AFTER:  _ [wrapped_difference: u64]
/// ```
///
/// # Preconditions
///
/// - all input arguments are properly [`BFieldCodec`] encoded
///
/// # Postconditions
///
/// - the output `difference` is the `minuend` minus the `subtrahend`, wrapping
///   around if the minuend is greater than the subtrahend
/// - the output is properly [`BFieldCodec`] encoded
///
/// [sub]: u64::wrapping_sub
#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
pub struct WrappingSub;

impl BasicSnippet for WrappingSub {
    fn inputs(&self) -> Vec<(DataType, String)> {
        OverflowingSub.inputs()
    }

    fn outputs(&self) -> Vec<(DataType, String)> {
        vec![(DataType::U64, "wrapped_difference".to_string())]
    }

    fn entrypoint(&self) -> String {
        "tasmlib_arithmetic_u64_wrapping_sub".to_string()
    }

    fn code(&self, _: &mut Library) -> Vec<LabelledInstruction> {
        triton_asm! {
            {self.entrypoint()}:
                {&OverflowingSub::common_subtraction_code()}
                // _ difference_lo (minuend_hi - subtrahend_hi - carry)

                addi {1_u64 << 32}
                split
                // _ difference_lo !is_overflow difference_hi

                place 2
                pop 1
                // _ difference_hi difference_lo

                return
        }
    }

    fn sign_offs(&self) -> HashMap<Reviewer, SignOffFingerprint> {
        let mut sign_offs = HashMap::new();
        sign_offs.insert(Reviewer("ferdinand"), 0xe6d83d5f88c389e3.into());
        sign_offs
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::pop_encodable;
    use crate::push_encodable;
    use crate::traits::closure::Closure;
    use crate::traits::closure::ShadowedClosure;
    use crate::traits::rust_shadow::RustShadow;

    impl Closure for WrappingSub {
        fn rust_shadow(&self, stack: &mut Vec<BFieldElement>) {
            let minuend = pop_encodable::<u64>(stack);
            let subtrahend = pop_encodable::<u64>(stack);
            let difference = minuend.wrapping_sub(subtrahend);
            push_encodable(stack, &difference);
        }

        fn pseudorandom_initial_state(
            &self,
            seed: [u8; 32],
            bench_case: Option<crate::snippet_bencher::BenchmarkCase>,
        ) -> Vec<BFieldElement> {
            OverflowingSub.pseudorandom_initial_state(seed, bench_case)
        }

        fn corner_case_initial_states(&self) -> Vec<Vec<BFieldElement>> {
            OverflowingSub.corner_case_initial_states()
        }
    }

    #[test]
    fn rust_shadow() {
        ShadowedClosure::new(WrappingSub).test()
    }
}

#[cfg(test)]
mod benches {
    use super::*;
    use crate::traits::closure::ShadowedClosure;
    use crate::traits::rust_shadow::RustShadow;

    #[test]
    fn benchmark() {
        ShadowedClosure::new(WrappingSub).bench()
    }
}
