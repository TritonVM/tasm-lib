use triton_vm::prelude::*;

use crate::arithmetic::u128::overflowing_add::OverflowingAdd;
use crate::prelude::*;

#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
pub struct SafeAdd;

impl SafeAdd {
    pub(crate) const OVERFLOW_ERROR_ID: i128 = 170;
}

impl BasicSnippet for SafeAdd {
    fn parameters(&self) -> Vec<(DataType, String)> {
        vec![
            (DataType::U128, "lhs".to_owned()),
            (DataType::U128, "rhs".to_owned()),
        ]
    }

    fn return_values(&self) -> Vec<(DataType, String)> {
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
    use rand::rngs::StdRng;

    use super::*;
    use crate::test_prelude::*;

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
            let lhs = rng.random();
            let rhs = rng.random_range(0..=u128::MAX - lhs);

            (lhs, rhs)
        }

        fn corner_case_args(&self) -> Vec<Self::Args> {
            let edge_case_points = OverflowingAdd::edge_case_points();

            edge_case_points
                .iter()
                .cartesian_product(&edge_case_points)
                .filter(|&(&l, &r)| l.checked_add(r).is_some())
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
        SafeAdd.assert_expected_add_behavior(1 << 67, 1 << 67)
    }

    #[test]
    fn overflow_test() {
        for args in [
            (1 << 127, 1 << 127),
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
        ]
        .into_iter()
        .flat_map(|(left, right)| [(left, right), (right, left)])
        {
            test_assertion_failure(
                &ShadowedClosure::new(SafeAdd),
                InitVmState::with_stack(SafeAdd.set_up_test_stack(args)),
                &[SafeAdd::OVERFLOW_ERROR_ID],
            );
        }

        for i in 0..128 {
            let a = 1 << i;
            let b = u128::MAX - a + 1;

            assert_eq!((0, true), a.overflowing_add(b), "i = {i}. a = {a}, b = {b}");
            test_assertion_failure(
                &ShadowedClosure::new(SafeAdd),
                InitVmState::with_stack(SafeAdd.set_up_test_stack((a, b))),
                &[SafeAdd::OVERFLOW_ERROR_ID],
            );
        }
    }
}

#[cfg(test)]
mod benches {
    use super::*;
    use crate::test_prelude::*;

    #[test]
    fn benchmark() {
        ShadowedClosure::new(SafeAdd).bench()
    }
}
