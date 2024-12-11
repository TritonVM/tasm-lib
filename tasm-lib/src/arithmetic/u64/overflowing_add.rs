use std::collections::HashMap;
use triton_vm::prelude::*;

use crate::data_type::DataType;
use crate::library::Library;
use crate::traits::basic_snippet::BasicSnippet;
use crate::traits::basic_snippet::Reviewer;
use crate::traits::basic_snippet::SignOffFingerprint;

/// Add two `u64`s, indicating whether overflow occurred.
///
/// ### Behavior
///
/// ```text
/// BEFORE: _ [right: u64] [left: u64]
/// AFTER:  _ [sum: u64] [is_overflow: bool]
/// ```
///
/// ### Preconditions
///
/// - all input arguments are properly [`BFieldCodec`] encoded
///
/// ### Postconditions
///
/// - the `sum` is the sum of the input modulo [`u64::MAX`]
/// - `is_overflow` is `true` if and only if the sum of the input exceeds
///   [`u64::MAX`]
/// - the `sum` is properly [`BFieldCodec`] encoded
/// - `is_overflow` is properly [`BFieldCodec`] encoded
#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub struct OverflowingAdd;

impl OverflowingAdd {
    /// (See [`OverflowingAdd`])
    pub(crate) fn addition_code() -> Vec<LabelledInstruction> {
        triton_asm! {
            pick 2
            // _ rhs_hi lhs_hi lhs_lo rhs_lo

            add
            split
            // _ rhs_hi lhs_hi carry sum_lo

            place 3
            // _ sum_lo rhs_hi lhs_hi carry

            add
            add
            // _ sum_lo (lhs_hi+rhs_hi+carry)

            split
            // _ sum_lo overflow sum_hi

            place 2
        }
    }
}

impl BasicSnippet for OverflowingAdd {
    fn inputs(&self) -> Vec<(DataType, String)> {
        ["lhs", "rhs"]
            .map(|s| (DataType::U64, s.to_string()))
            .to_vec()
    }

    fn outputs(&self) -> Vec<(DataType, String)> {
        vec![
            (DataType::U64, "wrapped_sum".to_string()),
            (DataType::Bool, "overflow".to_string()),
        ]
    }

    fn entrypoint(&self) -> String {
        "tasmlib_arithmetic_u64_overflowing_add".to_string()
    }

    fn code(&self, _: &mut Library) -> Vec<LabelledInstruction> {
        triton_asm! {
            {self.entrypoint()}:
                {&Self::addition_code()}
                return
        }
    }

    fn sign_offs(&self) -> HashMap<Reviewer, SignOffFingerprint> {
        let mut sign_offs = HashMap::new();
        sign_offs.insert(Reviewer("ferdinand"), 0x7226f66f533b188a.into());
        sign_offs
    }
}

#[cfg(test)]
pub mod tests {
    use itertools::Itertools;
    use rand::prelude::*;

    use super::*;
    use crate::pop_encodable;
    use crate::push_encodable;
    use crate::snippet_bencher::BenchmarkCase;
    use crate::traits::closure::Closure;
    use crate::traits::closure::ShadowedClosure;
    use crate::traits::rust_shadow::RustShadow;

    impl OverflowingAdd {
        pub fn set_up_initial_stack(&self, left: u64, right: u64) -> Vec<BFieldElement> {
            let mut stack = self.init_stack_for_isolated_run();
            push_encodable(&mut stack, &right);
            push_encodable(&mut stack, &left);

            stack
        }

        pub fn corner_case_points() -> Vec<u64> {
            [0, 1 << 32, u64::MAX]
                .into_iter()
                .flat_map(|p| [p.checked_sub(1), Some(p), p.checked_add(1)])
                .flatten()
                .collect()
        }
    }

    impl Closure for OverflowingAdd {
        fn rust_shadow(&self, stack: &mut Vec<BFieldElement>) {
            let right = pop_encodable::<u64>(stack);
            let left = pop_encodable::<u64>(stack);
            let (sum, is_overflow) = left.overflowing_add(right);
            push_encodable(stack, &sum);
            push_encodable(stack, &is_overflow);
        }

        fn pseudorandom_initial_state(
            &self,
            seed: [u8; 32],
            bench_case: Option<BenchmarkCase>,
        ) -> Vec<BFieldElement> {
            let (left, right) = match bench_case {
                Some(BenchmarkCase::CommonCase) => (1 << 63, (1 << 63) - 1),
                Some(BenchmarkCase::WorstCase) => (1 << 63, 1 << 50),
                None => StdRng::from_seed(seed).gen(),
            };

            self.set_up_initial_stack(left, right)
        }

        fn corner_case_initial_states(&self) -> Vec<Vec<BFieldElement>> {
            let corner_case_points = Self::corner_case_points();

            corner_case_points
                .iter()
                .cartesian_product(&corner_case_points)
                .map(|(&l, &r)| self.set_up_initial_stack(l, r))
                .collect()
        }
    }

    #[test]
    fn u64_overflowing_add_pbt() {
        ShadowedClosure::new(OverflowingAdd).test()
    }
}

#[cfg(test)]
mod benches {
    use super::*;
    use crate::traits::closure::ShadowedClosure;
    use crate::traits::rust_shadow::RustShadow;

    #[test]
    fn u64_overflowing_add_bench() {
        ShadowedClosure::new(OverflowingAdd).bench()
    }
}
