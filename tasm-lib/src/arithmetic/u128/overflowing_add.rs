use std::collections::HashMap;

use triton_vm::prelude::*;

use crate::prelude::*;
use crate::traits::basic_snippet::Reviewer;
use crate::traits::basic_snippet::SignOffFingerprint;

/// Mimics [`u128::overflowing_add`].
///
/// ### Behavior
///
/// ```text
/// BEFORE: _ [rhs: u128] [lhs: u128]
/// AFTER:  _ [sum: u128] [is_overflow: bool]
/// ```
///
/// ### Preconditions
///
/// - all input arguments are properly [`BFieldCodec`] encoded
///
/// ### Postconditions
///
/// - the output is properly [`BFieldCodec`] encoded
#[derive(Debug, Default, Copy, Clone, Eq, PartialEq, Hash)]
pub struct OverflowingAdd;

impl OverflowingAdd {
    /// Generate code to perform an addition on `u128`s.
    ///
    /// This function is called by both this snippet and
    /// [`SafeAdd`](super::safe_add::SafeAdd).
    ///
    /// ```text
    /// BEFORE: _ rhs_3 rhs_2 rhs_1 rhs_0 lhs_3 lhs_2 lhs_1 lhs_0
    /// AFTER:  _ sum_3 sum_2 sum_1 sum_0 is_overflow
    /// ```
    /// Don't forget to adapt the signature when using this function elsewhere.
    pub(crate) fn addition_code() -> Vec<LabelledInstruction> {
        triton_asm!(
            pick 4  // _ rhs_3 rhs_2 rhs_1 lhs_3 lhs_2 lhs_1 lhs_0 rhs_0
            add
            split   // _ rhs_3 rhs_2 rhs_1 lhs_3 lhs_2 lhs_1 (lhs_0 + rhs_0)_hi (lhs_0 + rhs_0)_lo
                    // _ rhs_3 rhs_2 rhs_1 lhs_3 lhs_2 lhs_1 carry_1            sum_0
            swap 5  // _ rhs_3 rhs_2 sum_0 lhs_3 lhs_2 lhs_1 carry_1 rhs_1
            add
            add
            split   // _ rhs_3 rhs_2 sum_0 lhs_3 lhs_2 carry_2 sum_1
            swap 5  // _ rhs_3 sum_1 sum_0 lhs_3 lhs_2 carry_2 rhs_2
            add
            add
            split   // _ rhs_3 sum_1 sum_0 lhs_3 carry_3 sum_2
            swap 5  // _ sum_2 sum_1 sum_0 lhs_3 carry_3 rhs_3
            add
            add
            split   // _ sum_2 sum_1 sum_0 carry_4 sum_3
            place 4 // _ sum_3 sum_2 sum_1 sum_0 carry_4
                    // _ sum_3 sum_2 sum_1 sum_0 is_overflow
        )
    }
}

impl BasicSnippet for OverflowingAdd {
    fn parameters(&self) -> Vec<(DataType, String)> {
        ["lhs", "rhs"]
            .map(|s| (DataType::U128, s.to_owned()))
            .to_vec()
    }

    fn return_values(&self) -> Vec<(DataType, String)> {
        vec![
            (DataType::U128, "sum".to_owned()),
            (DataType::Bool, "overflow".to_owned()),
        ]
    }

    fn entrypoint(&self) -> String {
        "tasmlib_arithmetic_u128_overflowing_add".to_string()
    }

    fn code(&self, _: &mut Library) -> Vec<LabelledInstruction> {
        triton_asm! { {self.entrypoint()}: {&Self::addition_code()} return }
    }

    fn sign_offs(&self) -> HashMap<Reviewer, SignOffFingerprint> {
        let mut sign_offs = HashMap::new();
        sign_offs.insert(Reviewer("ferdinand"), 0xd5c365d6252846bd.into());
        sign_offs
    }
}

#[cfg(test)]
pub(crate) mod tests {
    use rand::rngs::StdRng;

    use super::*;
    use crate::test_prelude::*;

    impl OverflowingAdd {
        fn assert_expected_add_behavior(&self, lhs: u128, rhs: u128) {
            let initial_stack = self.set_up_test_stack((rhs, lhs));

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
            StdRng::from_seed(seed).random()
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
    fn rust_shadow() {
        ShadowedClosure::new(OverflowingAdd).test();
    }

    #[test]
    fn unit_test() {
        let snippet = OverflowingAdd;
        snippet.assert_expected_add_behavior(1u128 << 67, 1u128 << 67)
    }

    #[test]
    fn overflow_test() {
        let test_overflowing_add = |a, b| {
            OverflowingAdd.assert_expected_add_behavior(a, b);
            OverflowingAdd.assert_expected_add_behavior(b, a);
        };

        test_overflowing_add(1, u128::MAX);
        test_overflowing_add(2, u128::MAX - 1);
        test_overflowing_add(1 << 127, 1 << 127);
        test_overflowing_add(u128::MAX, u128::MAX);

        for a in [31, 32, 33, 63, 64, 65, 95, 96, 97].map(|p| 1 << p) {
            test_overflowing_add(u128::MAX, a);
        }

        for i in 0..128 {
            let a = 1 << i;
            let b = u128::MAX - a + 1;
            debug_assert_eq!((0, true), a.overflowing_add(b), "i = {i}; a = {a}, b = {b}");

            test_overflowing_add(a, b);
        }
    }
}

#[cfg(test)]
mod benches {
    use super::*;
    use crate::test_prelude::*;

    #[test]
    fn benchmark() {
        ShadowedClosure::new(OverflowingAdd).bench();
    }
}
