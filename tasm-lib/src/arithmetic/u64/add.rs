use std::collections::HashMap;
use tasm_lib::prelude::BasicSnippet;
use triton_vm::prelude::*;

use crate::arithmetic;
use crate::data_type::DataType;
use crate::library::Library;
use crate::traits::basic_snippet::Reviewer;
use crate::traits::basic_snippet::SignOffFingerprint;

/// Add two `u64`s.
///
/// ### Behavior
///
/// ```text
/// BEFORE: _ [right: u64] [left: u64]
/// AFTER:  _ [sum: u64]
/// ```
///
/// ### Preconditions
///
/// - all input arguments are properly [`BFieldCodec`] encoded
/// - the sum of the arguments does not exceed [`u64::MAX`]
///
/// ### Postconditions
///
/// - the `sum` is the sum of the arguments
/// - the `sum` is properly [`BFieldCodec`] encoded
#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub struct Add;

impl Add {
    pub const OVERFLOW_ERROR_ID: i128 = 310;
}

impl BasicSnippet for Add {
    fn inputs(&self) -> Vec<(DataType, String)> {
        ["rhs", "lhs"]
            .map(|side| (DataType::U64, side.to_string()))
            .to_vec()
    }

    fn outputs(&self) -> Vec<(DataType, String)> {
        vec![(DataType::U64, "sum".to_string())]
    }

    fn entrypoint(&self) -> String {
        "tasmlib_arithmetic_u64_add".to_string()
    }

    fn code(&self, _: &mut Library) -> Vec<LabelledInstruction> {
        triton_asm!({self.entrypoint()}:
            {&arithmetic::u64::overflowing_add::OverflowingAdd::addition_code()}
            // _ sum_hi sum_lo is_overflow

            push 0
            eq
            assert error_id {Self::OVERFLOW_ERROR_ID}
            // _ sum_hi sum_lo

            return
        )
    }

    fn sign_offs(&self) -> HashMap<Reviewer, SignOffFingerprint> {
        let mut sign_offs = HashMap::new();
        sign_offs.insert(Reviewer("ferdinand"), 0xb84891929efdb6cd.into());
        sign_offs
    }
}

#[cfg(test)]
mod tests {
    use itertools::Itertools;
    use proptest::prop_assume;
    use rand::prelude::*;
    use test_strategy::proptest;

    use super::*;
    use crate::arithmetic::u64::overflowing_add::OverflowingAdd;
    use crate::pop_encodable;
    use crate::push_encodable;
    use crate::snippet_bencher::BenchmarkCase;
    use crate::test_helpers::test_assertion_failure;
    use crate::test_helpers::test_rust_equivalence_given_execution_state;
    use crate::traits::closure::Closure;
    use crate::traits::closure::ShadowedClosure;
    use crate::traits::rust_shadow::RustShadow;
    use crate::InitVmState;

    impl Add {
        fn set_up_initial_stack(&self, left: u64, right: u64) -> Vec<BFieldElement> {
            OverflowingAdd.set_up_initial_stack(left, right)
        }
    }

    impl Closure for Add {
        fn rust_shadow(&self, stack: &mut Vec<BFieldElement>) {
            let left = pop_encodable::<u64>(stack);
            let right = pop_encodable::<u64>(stack);
            let sum = left.checked_add(right).expect("overflow occurred");
            push_encodable(stack, &sum);
        }

        fn pseudorandom_initial_state(
            &self,
            seed: [u8; 32],
            _: Option<BenchmarkCase>,
        ) -> Vec<BFieldElement> {
            let mut rng = StdRng::from_seed(seed);
            let left = rng.gen();
            let right = rng.gen_range(0..=u64::MAX - left);

            self.set_up_initial_stack(left, right)
        }

        fn corner_case_initial_states(&self) -> Vec<Vec<BFieldElement>> {
            let corner_case_points = OverflowingAdd::corner_case_points();

            corner_case_points
                .iter()
                .cartesian_product(&corner_case_points)
                .filter(|(&l, &r)| l.checked_add(r).is_some())
                .map(|(&l, &r)| self.set_up_initial_stack(l, r))
                .collect()
        }
    }

    #[test]
    fn unit_test() {
        ShadowedClosure::new(Add).test();
    }

    #[proptest]
    fn proptest(left: u64, #[strategy(0..u64::MAX - #left)] right: u64) {
        let initial_state = InitVmState::with_stack(Add.set_up_initial_stack(left, right));
        test_rust_equivalence_given_execution_state(&ShadowedClosure::new(Add), initial_state);
    }

    #[proptest]
    fn triton_vm_crashes_on_overflowing_add(left: u64, #[strategy(u64::MAX - #left..)] right: u64) {
        prop_assume!(left.checked_add(right).is_none());

        test_assertion_failure(
            &ShadowedClosure::new(Add),
            InitVmState::with_stack(Add.set_up_initial_stack(left, right)),
            &[Add::OVERFLOW_ERROR_ID],
        );
    }
}

#[cfg(test)]
mod benches {
    use super::*;
    use crate::traits::closure::ShadowedClosure;
    use crate::traits::rust_shadow::RustShadow;

    #[test]
    fn bench() {
        ShadowedClosure::new(Add).bench()
    }
}
