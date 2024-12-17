use triton_vm::prelude::*;

use crate::data_type::DataType;
use crate::library::Library;
use crate::traits::basic_snippet::BasicSnippet;

#[derive(Clone, Debug, Copy)]
pub struct OverflowingAdd;

impl OverflowingAdd {
    /// Generate code to perform an addition on `u128`s.
    ///
    /// This function is called by both this snippet and [`SafeAdd`].
    ///
    /// BEFORE: _ rhs_3 rhs_2 rhs_1 rhs_0 lhs_3 lhs_2 lhs_1 lhs_0
    /// AFTER:  _ sum_3 sum_2 sum_1 sum_0 is_overflow
    ///                                   ^^^^^^^^^^^
    /// Don't forget to adapt the signature when using this function elsewhere.
    pub(crate) fn addition_code() -> Vec<LabelledInstruction> {
        triton_asm!(
                pick 4
                // _ rhs_3 rhs_2 rhs_1 lhs_3 lhs_2 lhs_1 lhs_0 rhs_0

                add
                split
                // _ rhs_3 rhs_2 rhs_1 lhs_3 lhs_2 lhs_1 (lhs_0 + rhs_0)_hi (lhs_0 + rhs_0)_lo
                // _ rhs_3 rhs_2 rhs_1 lhs_3 lhs_2 lhs_1 carry_1            sum_0

                place 7
                pick 4
                // _ sum_0 rhs_3 rhs_2 lhs_3 lhs_2 lhs_1 carry_1 rhs_1

                add
                add
                split
                // _ sum_0 rhs_3 rhs_2 lhs_3 lhs_2 carry_2 sum_1

                place 6
                pick 3
                // _ sum_1 sum_0 rhs_3 lhs_3 lhs_2 carry_2 rhs_2

                add
                add
                split
                // _ sum_1 sum_0 rhs_3 lhs_3 carry_3 sum_2

                place 5
                // _ sum_2 sum_1 sum_0 rhs_3 lhs_3 carry_3

                add
                add
                split
                // _ sum_2 sum_1 sum_0 carry_4 sum_3

                place 4
                // _ sum_3 sum_2 sum_1 sum_0 carry_4
                // _ sum_3 sum_2 sum_1 sum_0 is_overflow
        )
    }
}

impl BasicSnippet for OverflowingAdd {
    fn entrypoint(&self) -> String {
        "tasmlib_arithmetic_u128_overflowing_add".to_string()
    }

    fn inputs(&self) -> Vec<(DataType, String)> {
        vec![
            (DataType::U128, "lhs".to_owned()),
            (DataType::U128, "rhs".to_owned()),
        ]
    }

    fn outputs(&self) -> Vec<(DataType, String)> {
        vec![
            (DataType::U128, "sum".to_owned()),
            (DataType::Bool, "overflow".to_owned()),
        ]
    }

    /// Four top elements of stack are assumed to be valid u32s. So to have
    /// a value that's less than 2^32.
    fn code(&self, _: &mut Library) -> Vec<LabelledInstruction> {
        let add_code = Self::addition_code();

        triton_asm! {
            {self.entrypoint()}:
                {&add_code}
                return
        }
    }
}

#[cfg(test)]
pub(crate) mod tests {
    use itertools::Itertools;
    use num::Zero;
    use rand::rngs::StdRng;
    use rand::Rng;
    use rand::SeedableRng;

    use super::*;
    use crate::pop_encodable;
    use crate::push_encodable;
    use crate::snippet_bencher::BenchmarkCase;
    use crate::test_helpers::test_rust_equivalence_given_complete_state;
    use crate::traits::closure::Closure;
    use crate::traits::closure::ShadowedClosure;
    use crate::traits::rust_shadow::RustShadow;

    impl OverflowingAdd {
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

        pub fn edge_case_points() -> Vec<u128> {
            [0, 0x200000002fffffffffff908f8, 1 << 127, u128::MAX]
                .into_iter()
                .flat_map(|p| [p.checked_sub(1), Some(p), p.checked_add(1)])
                .flatten()
                .collect()
        }
    }

    impl Closure for OverflowingAdd {
        type Args = (u128, u128);

        fn rust_shadow(&self, stack: &mut Vec<BFieldElement>) {
            let (left, right) = pop_encodable::<Self::Args>(stack);
            let (sum, is_overflow) = left.overflowing_add(right);
            push_encodable(stack, &sum);
            push_encodable(stack, &is_overflow);
        }

        fn pseudorandom_args(&self, seed: [u8; 32], _: Option<BenchmarkCase>) -> Self::Args {
            StdRng::from_seed(seed).gen()
        }

        fn corner_case_args(&self) -> Vec<Self::Args> {
            let edge_case_points = Self::edge_case_points();

            edge_case_points
                .iter()
                .cartesian_product(&edge_case_points)
                .map(|(&l, &r)| (l, r))
                .collect()
        }
    }

    #[test]
    fn overflowing_add_u128_test() {
        ShadowedClosure::new(OverflowingAdd).test()
    }

    #[test]
    fn overflowing_add_u128_unit_test() {
        let snippet = OverflowingAdd;
        snippet.assert_expected_add_behavior(1u128 << 67, 1u128 << 67)
    }

    #[test]
    fn overflowing_add_u128_overflow_test() {
        let snippet = OverflowingAdd;

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
            snippet.assert_expected_add_behavior(a, b);
            snippet.assert_expected_add_behavior(b, a);
        }

        for i in 0..128 {
            let a = u128::MAX - ((1u128 << i) - 1);
            let b = 1u128 << i;

            // sanity check of test input values
            let (wrapped_add, is_overflow) = a.overflowing_add(b);
            assert!(is_overflow, "i = {i}. a = {a}, b = {b}");
            assert!(wrapped_add.is_zero());

            snippet.assert_expected_add_behavior(b, a);
        }
    }
}

#[cfg(test)]
mod benches {
    use super::*;
    use crate::traits::closure::ShadowedClosure;
    use crate::traits::rust_shadow::RustShadow;

    #[test]
    fn overflowing_add_u128_benchmark() {
        ShadowedClosure::new(OverflowingAdd).bench()
    }
}
