use triton_vm::prelude::*;

use crate::prelude::*;

/// Mimics overflowing_add but works on u192 data types.
///
/// ### Behavior
///
/// ```text
/// BEFORE: _ [rhs: u192] [lhs: u192]
/// AFTER:  _ [sum: u192] [is_overflow: bool]
/// ```
///
/// ### Preconditions
///
/// - all input arguments are properly [`BFieldCodec`] encoded
///
/// ### Postconditions
///
/// - the output is properly [`BFieldCodec`] encoded
#[derive(Debug, Clone)]
pub struct OverflowingAdd;

impl OverflowingAdd {
    /// Generate code to perform an addition on `u192`s.
    ///
    /// ```text
    /// BEFORE: _ r5 r4 r3 r2 r1 r0 l5 l4 l3 l2 l1 l0
    /// AFTER:  _ sum_5 sum_4 sum_3 sum_2 sum_1 sum_0 is_overflow
    /// ```
    pub(crate) fn addition_code() -> Vec<LabelledInstruction> {
        triton_asm!(
            pick 6
            // _ r5 r4 r3 r2 r1 l5 l4 l3 l2 l1 l0 r0

            add
            split
            // _ r5 r4 r3 r2 r1 l5 l4 l3 l2 l1 (l0 + r0)_hi (l0 + r0)_lo
            // _ r5 r4 r3 r2 r1 l5 l4 l3 l2 l1 carry_1 sum_0

            swap 7
            add
            add
            split
            // _ r5 r4 r3 r2 sum_0 l5 l4 l3 l2 carry_2 sum_1

            swap 7
            add
            add
            split
            // _ r5 r4 r3 sum_1 sum_0 l5 l4 l3 carry_3 sum_2

            swap 7
            add
            add
            split
            // _ r5 r4 sum_2 sum_1 sum_0 l5 l4 carry_4 sum_3

            swap 7
            add
            add
            split
            // _ r5 sum_3 sum_2 sum_1 sum_0 l5 carry_5 sum_4

            swap 7
            add
            add
            split
            // _ sum_4 sum_3 sum_2 sum_1 sum_0 (is_overflow: bool) sum_5

            place 6
            // _ sum_5 sum_4 sum_3 sum_2 sum_1 sum_0 is_overflow

            // _ [sum] overflow
        )
    }
}

impl BasicSnippet for OverflowingAdd {
    fn parameters(&self) -> Vec<(DataType, String)> {
        ["lhs", "rhs"]
            .map(|s| (DataType::U192, s.to_owned()))
            .to_vec()
    }

    fn return_values(&self) -> Vec<(DataType, String)> {
        vec![
            (DataType::U192, "sum".to_owned()),
            (DataType::Bool, "overflow".to_owned()),
        ]
    }

    fn entrypoint(&self) -> String {
        "tasmlib_arithmetic_u192_overflowing_add".to_string()
    }

    fn code(&self, _: &mut Library) -> Vec<LabelledInstruction> {
        triton_asm! { {self.entrypoint()}: {&Self::addition_code()} return }
    }
}

#[cfg(test)]
pub(crate) mod tests {
    use num::BigUint;
    use rand::rngs::StdRng;

    use super::*;
    use crate::arithmetic::u192::U192;
    use crate::arithmetic::u192::to_u192;
    use crate::arithmetic::u192::u128_to_u192;
    use crate::arithmetic::u192::u128_to_u192_shl64;
    use crate::test_prelude::*;

    impl OverflowingAdd {
        fn assert_expected_add_behavior(&self, lhs: U192, rhs: U192) {
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

        pub fn edge_case_points() -> Vec<U192> {
            [0, 0x200000002fffffffffff908f8, 1 << 127, u128::MAX]
                .into_iter()
                .flat_map(|p| [p.checked_sub(1), Some(p), p.checked_add(1)])
                .flatten()
                .map(u128_to_u192)
                .chain([
                    [u32::MAX; 6],
                    [0, 0, 0, 0, 0, 1],
                    [0, 0, 0, 0, 0, 1 << 31],
                    [0, 0, 0, 0, 1 << 31, 0],
                ])
                .collect()
        }
    }

    impl Closure for OverflowingAdd {
        type Args = (U192, U192);

        fn rust_shadow(&self, stack: &mut Vec<BFieldElement>) {
            let left: U192 = pop_encodable(stack);
            let left: BigUint = BigUint::new(left.to_vec());
            let right: U192 = pop_encodable(stack);
            let right: BigUint = BigUint::new(right.to_vec());
            let sum = left + right;
            let mut sum = sum.to_u32_digits();
            assert!(
                sum.len() <= 6 || sum.len() == 7 && *sum.last().unwrap() == 1,
                "Value must be bounded thusly"
            );

            let is_overflow = sum.len() == 7;

            sum.resize(6, 0);
            let sum: U192 = sum.to_vec().try_into().unwrap();
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
        snippet.assert_expected_add_behavior(u128_to_u192(1u128 << 67), u128_to_u192(1u128 << 67))
    }

    #[test]
    fn overflow_test() {
        let test_overflowing_add = |a, b| {
            OverflowingAdd.assert_expected_add_behavior(a, b);
            OverflowingAdd.assert_expected_add_behavior(b, a);
        };

        let max = to_u192(u128::MAX, u64::MAX);
        let max_minus_one = to_u192(u128::MAX, u64::MAX - 1);
        test_overflowing_add(u128_to_u192(1), max);
        test_overflowing_add(u128_to_u192(2), max_minus_one);
        test_overflowing_add(u128_to_u192_shl64(1 << 127), u128_to_u192_shl64(1 << 127));
        test_overflowing_add(max, max);

        for a in [31, 32, 33, 63, 64, 65, 95, 96, 97].map(|p| 1 << p) {
            test_overflowing_add(max, u128_to_u192(a));
        }

        for i in 0..128 {
            let a = 1 << i;
            let b = u128::MAX - a + 1;
            debug_assert_eq!((0, true), a.overflowing_add(b), "i = {i}; a = {a}, b = {b}");

            test_overflowing_add(u128_to_u192(a), u128_to_u192_shl64(b));
        }
    }
}
