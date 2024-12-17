use triton_vm::prelude::*;

use crate::arithmetic::u128::overflowing_add::OverflowingAdd;
use crate::data_type::DataType;
use crate::library::Library;
use crate::traits::basic_snippet::BasicSnippet;

#[derive(Clone, Debug, Copy)]
pub struct SafeAdd;

impl SafeAdd {
    pub(crate) const OVERFLOW_ERROR_ID: i128 = 170;
}

impl BasicSnippet for SafeAdd {
    fn inputs(&self) -> Vec<(DataType, String)> {
        vec![
            (DataType::U128, "lhs".to_owned()),
            (DataType::U128, "rhs".to_owned()),
        ]
    }

    fn outputs(&self) -> Vec<(DataType, String)> {
        vec![(DataType::U128, "sum".to_owned())]
    }

    fn entrypoint(&self) -> String {
        "tasmlib_arithmetic_u128_safe_add".to_string()
    }

    /// Four top elements of stack are assumed to be valid u32s. So to have
    /// a value that's less than 2^32.
    fn code(&self, _: &mut Library) -> Vec<LabelledInstruction> {
        let add_code = OverflowingAdd::addition_code();

        triton_asm! {
            // BEFORE: _ rhs_3 rhs_2 rhs_1 rhs_0 lhs_3 lhs_2 lhs_1 lhs_0
            // AFTER:  _ sum_3 sum_2 sum_1 sum_0
            {self.entrypoint()}:
                {&add_code}
                // _ sum_3 sum_2 sum_1 sum_0 overflow

                push 0
                eq
                assert error_id {Self::OVERFLOW_ERROR_ID}
                return
        }
    }
}

#[cfg(test)]
mod tests {
    use itertools::Itertools;
    use num::Zero;
    use rand::rngs::StdRng;
    use rand::Rng;
    use rand::SeedableRng;
    use tasm_lib::test_helpers::test_assertion_failure;

    use super::*;
    use crate::pop_encodable;
    use crate::push_encodable;
    use crate::snippet_bencher::BenchmarkCase;
    use crate::test_helpers::test_rust_equivalence_given_complete_state;
    use crate::traits::closure::Closure;
    use crate::traits::closure::ShadowedClosure;
    use crate::traits::rust_shadow::RustShadow;
    use crate::InitVmState;

    impl SafeAdd {
        fn assert_expected_add_behavior(&self, lhs: u128, rhs: u128) {
            let initial_stack = self.set_up_test_stack((lhs, rhs));

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

    impl Closure for SafeAdd {
        type Args = <OverflowingAdd as Closure>::Args;

        fn rust_shadow(&self, stack: &mut Vec<BFieldElement>) {
            let (left, right) = pop_encodable::<Self::Args>(stack);
            let sum = left.checked_add(right).unwrap();
            push_encodable(stack, &sum);
        }

        fn pseudorandom_args(&self, seed: [u8; 32], _: Option<BenchmarkCase>) -> Self::Args {
            let mut rng = StdRng::from_seed(seed);
            let lhs = rng.gen();
            let rhs = rng.gen_range(0..=u128::MAX - lhs);

            (lhs, rhs)
        }

        fn corner_case_args(&self) -> Vec<Self::Args> {
            let edge_case_points = OverflowingAdd::edge_case_points();

            edge_case_points
                .iter()
                .cartesian_product(&edge_case_points)
                .filter(|(&l, &r)| l.checked_add(r).is_some())
                .map(|(&l, &r)| (l, r))
                .collect()
        }
    }

    #[test]
    fn rust_shadow() {
        ShadowedClosure::new(SafeAdd).test()
    }

    #[test]
    fn unit_test() {
        let snippet = SafeAdd;
        let mut expected = snippet.init_stack_for_isolated_run();
        expected.push(BFieldElement::new(0));
        expected.push(BFieldElement::new(1 << 4));
        expected.push(BFieldElement::new(0));
        expected.push(BFieldElement::new(0));
        snippet.assert_expected_add_behavior(1u128 << 67, 1u128 << 67)
    }

    #[test]
    fn overflow_test() {
        let snippet = SafeAdd;

        for (a, b) in [
            (1u128 << 127, 1u128 << 127),
            (u128::MAX, u128::MAX),
            (u128::MAX, 1),
            (u128::MAX, 1 << 31),
            (u128::MAX, 1 << 32),
            (u128::MAX, 1 << 33),
            (u128::MAX, 1 << 63),
            (u128::MAX, 1 << 64),
            (u128::MAX, 1 << 65),
            (u128::MAX, 1 << 95),
            (u128::MAX, 1 << 96),
            (u128::MAX, 1 << 97),
            (u128::MAX - 1, 2),
        ] {
            test_assertion_failure(
                &ShadowedClosure::new(snippet),
                InitVmState::with_stack(snippet.set_up_test_stack((a, b))),
                &[170],
            );
            test_assertion_failure(
                &ShadowedClosure::new(snippet),
                InitVmState::with_stack(snippet.set_up_test_stack((b, a))),
                &[170],
            );
        }

        for i in 0..128 {
            let a = u128::MAX - ((1u128 << i) - 1);
            let b = 1u128 << i;

            // sanity check of test input values
            let (wrapped_add, is_overflow) = a.overflowing_add(b);
            assert!(is_overflow, "i = {i}. a = {a}, b = {b}");
            assert!(wrapped_add.is_zero());

            test_assertion_failure(
                &ShadowedClosure::new(snippet),
                InitVmState::with_stack(snippet.set_up_test_stack((a, b))),
                &[170],
            );
            test_assertion_failure(
                &ShadowedClosure::new(snippet),
                InitVmState::with_stack(snippet.set_up_test_stack((b, a))),
                &[170],
            );
        }
    }
}

#[cfg(test)]
mod benches {
    use super::*;
    use crate::traits::closure::ShadowedClosure;
    use crate::traits::rust_shadow::RustShadow;

    #[test]
    fn add_u128_benchmark() {
        ShadowedClosure::new(SafeAdd).bench()
    }
}
