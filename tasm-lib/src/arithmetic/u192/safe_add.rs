use triton_vm::prelude::*;

use crate::arithmetic::u192::overflowing_add::OverflowingAdd;
use crate::prelude::*;

#[derive(Debug, Clone)]
pub struct SafeAdd;

/// Sum two u192 values, crashing the VM on overflow.
///
/// ### Behavior
///
/// ```text
/// BEFORE: _ [rhs: u192] [lhs: u192]
/// AFTER:  _ [sum: u192]
/// ```
///
/// ### Preconditions
///
/// - all input arguments are properly [`BFieldCodec`] encoded
///
/// ### Postconditions
///
/// - the output is properly [`BFieldCodec`] encoded
impl SafeAdd {
    pub(crate) const OVERFLOW_ERROR_ID: i128 = 600;
}

impl BasicSnippet for SafeAdd {
    fn parameters(&self) -> Vec<(DataType, String)> {
        vec![
            (DataType::U192, "l".to_owned()),
            (DataType::U192, "r".to_owned()),
        ]
    }

    fn return_values(&self) -> Vec<(DataType, String)> {
        vec![(DataType::U192, "sum".to_owned())]
    }

    fn entrypoint(&self) -> String {
        "tasmlib_arithmetic_u192_safe_add".to_string()
    }

    fn code(&self, _: &mut Library) -> Vec<LabelledInstruction> {
        let add_code = OverflowingAdd::addition_code();

        triton_asm! {
            // BEFORE: _ r5 r4 r3 r2 r1 r0 l5 l4 l3 l2 l1 l0
            // AFTER:  _ sum_5 sum_4 sum_3 sum_2 sum_1 sum_0
            {self.entrypoint()}:
                {&add_code}
                // _ sum_5 sum_4 sum_3 sum_2 sum_1 sum_0 overflow

                push 0
                eq
                assert error_id {Self::OVERFLOW_ERROR_ID}
                // _ sum_5 sum_4 sum_3 sum_2 sum_1 sum_0
                // _ [sum]

                return
        }
    }
}

#[cfg(test)]
mod tests {
    use num::BigUint;
    use rand::rngs::StdRng;

    use super::*;
    use crate::arithmetic::u192::U192;
    use crate::arithmetic::u192::to_u192;
    use crate::arithmetic::u192::u128_to_u192_shl64;
    use crate::test_prelude::*;

    #[test]
    fn rust_shadow() {
        ShadowedClosure::new(SafeAdd).test()
    }

    #[test]
    fn overflow_test() {
        for (left, right) in [
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
            let left = to_u192(left, u64::MAX);
            let right = u128_to_u192_shl64(right);
            test_assertion_failure(
                &ShadowedClosure::new(SafeAdd),
                InitVmState::with_stack(SafeAdd.set_up_test_stack((left, right))),
                &[SafeAdd::OVERFLOW_ERROR_ID],
            );
        }

        for i in 0..128 {
            let left = 1 << i;
            let right = u128::MAX - left + 1;

            assert_eq!(
                (0, true),
                left.overflowing_add(right),
                "i = {i}. a = {left}, b = {right}"
            );

            let left = to_u192(left, u64::MAX);
            let right = u128_to_u192_shl64(right);

            test_assertion_failure(
                &ShadowedClosure::new(SafeAdd),
                InitVmState::with_stack(SafeAdd.set_up_test_stack((left, right))),
                &[SafeAdd::OVERFLOW_ERROR_ID],
            );
        }
    }

    impl Closure for SafeAdd {
        type Args = <OverflowingAdd as Closure>::Args;

        fn rust_shadow(&self, stack: &mut Vec<BFieldElement>) {
            let left: U192 = pop_encodable(stack);
            let left: BigUint = BigUint::new(left.to_vec());
            let right: U192 = pop_encodable(stack);
            let right: BigUint = BigUint::new(right.to_vec());
            let sum = left + right;
            let mut sum = sum.to_u32_digits();
            assert!(sum.len() <= 6, "Overflow");

            sum.resize(6, 0);
            let sum: U192 = sum.try_into().unwrap();

            push_encodable(stack, &sum);
        }

        fn pseudorandom_args(&self, seed: [u8; 32], _: Option<BenchmarkCase>) -> Self::Args {
            let mut rng = StdRng::from_seed(seed);
            let lhs: U192 = rng.random();
            let lhs_as_biguint: BigUint = BigUint::new(lhs.to_vec());

            let u192_max = BigUint::from_bytes_be(&[0xFF; 24]);
            let max = &u192_max - &lhs_as_biguint;

            // Generate random bytes for rhs, making sure the value < max
            let mut rhs_bytes = [0u8; 24];
            let rhs = loop {
                rng.fill(&mut rhs_bytes);
                let candidate = BigUint::from_bytes_be(&rhs_bytes);
                if candidate < max {
                    break candidate;
                }
            };

            let mut rhs = rhs.to_u32_digits();
            rhs.resize(6, 0);

            (lhs, rhs.try_into().unwrap())
        }

        fn corner_case_args(&self) -> Vec<Self::Args> {
            fn u192_checked_add(l: U192, r: U192) -> Option<U192> {
                let l: BigUint = BigUint::new(l.to_vec());
                let r: BigUint = BigUint::new(r.to_vec());

                let sum = l + r;
                let mut sum = sum.to_u32_digits();

                if sum.len() > 6 {
                    None
                } else {
                    sum.resize(6, 0);
                    Some(sum.try_into().unwrap())
                }
            }

            let edge_case_points = OverflowingAdd::edge_case_points();

            edge_case_points
                .iter()
                .cartesian_product(&edge_case_points)
                .filter(|&(&l, &r)| u192_checked_add(l, r).is_some())
                .map(|(&l, &r)| (l, r))
                .collect()
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
