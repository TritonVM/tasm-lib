use triton_vm::prelude::*;

use crate::arithmetic;
use crate::arithmetic::u64::mul_two_u64s_to_u128::MulTwoU64sToU128;
use crate::prelude::*;

/// Multiply two `u160`s and crash on overflow.
///
/// ### Behavior
///
/// ```text
/// BEFORE: _ [right: u160] [left: u160]
/// AFTER:  _ [left · right: u160]
/// ```
///
/// ### Preconditions
///
/// - all input arguments are properly [`BFieldCodec`] encoded
/// - the product of `left` and `right` is less than or equal to
///   0xff..ff = $2^{160} - 1$.
///
/// ### Postconditions
///
/// - the output is the product of the input
/// - the output is properly [`BFieldCodec`] encoded
#[derive(Debug, Clone)]
pub struct SafeMul;

impl SafeMul {
    pub(crate) const OVERFLOW_0: i128 = 580;
    pub(crate) const OVERFLOW_1: i128 = 581;
    pub(crate) const OVERFLOW_2: i128 = 582;
    pub(crate) const OVERFLOW_3: i128 = 583;
    pub(crate) const OVERFLOW_4: i128 = 584;
}

impl BasicSnippet for SafeMul {
    fn parameters(&self) -> Vec<(DataType, String)> {
        ["right", "left"]
            .map(|side| (DataType::U160, side.to_string()))
            .to_vec()
    }

    fn return_values(&self) -> Vec<(DataType, String)> {
        vec![(DataType::U160, "product".to_string())]
    }

    fn entrypoint(&self) -> String {
        "tasmlib_arithmetic_u160_safe_mul".to_string()
    }

    fn code(&self, library: &mut Library) -> Vec<LabelledInstruction> {
        let u64_to_u128_mul = library.import(Box::new(MulTwoU64sToU128));
        let u64_safe_mul = library.import(Box::new(arithmetic::u64::safe_mul::SafeMul));
        let u64_safe_add = library.import(Box::new(arithmetic::u64::add::Add));
        let u128_safe_add = library.import(Box::new(arithmetic::u128::safe_add::SafeAdd));
        let u160_safe_add = library.import(Box::new(arithmetic::u160::safe_add::SafeAdd));

        triton_asm!(
            // BEFORE: _ r_4 r_3 r_2 r_1 r_0 l_4 l_3 l_2 l_1 l_0
            // AFTER:  _ p_4 p_3 p_2 p_1 p_0
            {self.entrypoint()}:



                /* Cast higest limbs, r_4 & l_4, to u64 */
                push 0
                place 10
                push 0
                place 5
                // _ 0 r_4 r_3 r_2 r_1 r_0 0 l_4 l_3 l_2 l_1 l_0

                /* Reinterpret as limbs of u64s */
                // _ [r_c] [r_b] [r_a] [l_c] [l_b] [l_a]

                /* Verify required high limbs are zero */
                /*
                   r_b * l_c == 0 ∧ r_c * l_b == 0 ∧ r_c * l_c == 0 =>
                   (r_b == 0 || l_c == 0) &&
                   (r_c == 0 || l_b == 0) &&
                   (r_c == 0 || l_c == 0)
                */

                dup 9
                push 0
                eq
                dup 9
                push 0
                eq
                mul
                // _ ... (r_b == 0)

                dup 5
                push 0
                eq
                // _ ... (r_b == 0) (l_c == 0)

                dup 12
                push 0
                eq
                // _ ... (r_b == 0) (l_c == 0) (r_c == 0)

                dup 6
                push 0
                eq
                dup 6
                push 0
                eq
                mul
                // _ ... (r_b == 0) (l_c == 0) (r_c == 0) (l_b == 0)

                dup 2
                dup 2
                add
                pop_count
                // _ ... (r_b == 0) (l_c == 0) (r_c == 0) (l_b == 0) (r_c == 0 || l_c == 0)

                assert error_id {Self::OVERFLOW_0}
                // _ ... (r_b == 0) (l_c == 0) (r_c == 0) (l_b == 0)

                add
                pop_count
                // _ ... (r_b == 0) (l_c == 0) (r_c == 0 || l_b == 0)

                assert error_id {Self::OVERFLOW_1}
                // _ ... (r_b == 0) (l_c == 0)

                add
                pop_count
                // _ ... (r_b == 0 || l_c == 0)

                assert error_id {Self::OVERFLOW_2}
                // _ ...

                // _ [r_c] [r_b] [r_a] [l_c] [l_b] [l_a]


                /* Calculate 2^128 products */
                /* r_c*l_a + r_b*l_b + r_a*l_c */
                pick 11
                pick 11
                dup 3
                dup 3
                call {u64_safe_mul}
                // _ [r_b] [r_a] [l_c] [l_b] [l_a] [r_c * l_a]

                dup 11
                dup 11
                dup 7
                dup 7
                call {u64_safe_mul}
                // _ [r_b] [r_a] [l_c] [l_b] [l_a] [r_c * l_a] [r_b * l_b]

                dup 11
                dup 11
                pick 11
                pick 11
                call {u64_safe_mul}
                // _ [r_b] [r_a] [l_b] [l_a] [r_c * l_a] [r_b * l_b] [r_a * l_c]


                call {u64_safe_add}
                call {u64_safe_add}
                // _ [r_b] [r_a] [l_b] [l_a] [r_c * l_a + r_b * l_b + r_a * l_c]


                /* Verify bound by u32::MAX as term is multiples of 2^{128} */
                pick 1
                push 0
                eq
                assert error_id {Self::OVERFLOW_3}
                // _ [r_b] [r_a] [l_b] [l_a] (r_c * l_a + r_b * l_b + r_a * l_c: u32)
                // _ [r_b] [r_a] [l_b] [l_a] (fact2: u32) <-rename

                push 0
                push 0
                push 0
                push 0
                // _ [r_b] [r_a] [l_b] [l_a] [term2: u160]


                /* Calculate 2^64 products */
                pick 12
                pick 12
                dup 8
                dup 8
                call {u64_to_u128_mul}
                // _ [r_a] [l_b] [l_a] [term2: u160] [r_b * l_a: u128]

                dup 14
                dup 14
                pick 14
                pick 14
                call {u64_to_u128_mul}
                // _ [r_a] [l_a] [term2: u160] [r_b * l_a: u128] [r_a * l_b: u128]

                call {u128_safe_add}
                // _ [r_a] [l_a] [term2: u160] [r_b * l_a + r_a * l_b: u128]
                // _ [r_a] [l_a] [term2: u160] [fact1: u128] <- rename

                /* Ensure fact1 bounded by 2^{96} */
                pick 3
                push 0
                eq
                assert error_id {Self::OVERFLOW_4}
                // _ [r_a] [l_a] [term2: u160] [fact1: u96]

                push 0
                push 0
                // _ [r_a] [l_a] [term2: u160] [term1: u160]

                push 0
                // _ [r_a] [l_a] [term2: u160] [term1: u160] 0

                pick 14
                pick 14
                pick 14
                pick 14
                call {u64_to_u128_mul}
                // _ [term2: u160] [term1: u160] 0 [r_a * l_a: u128]
                // _ [term2: u160] [term1: u160] [r_a * l_a: u160]
                // _ [term2: u160] [term1: u160] [term0: u160] <- rename

                call {u160_safe_add}
                call {u160_safe_add}
                // _ [term2 + term1 + term0: u160]

                // _ [prod: u160]

                return
        )
    }
}

#[cfg(test)]
mod tests {
    use num::BigUint;
    use num::One;
    use rand::rngs::StdRng;

    use super::*;
    use crate::arithmetic::u160::u128_to_u160;
    use crate::arithmetic::u160::u128_to_u160_shl_32;
    use crate::arithmetic::u160::u128_to_u160_shl_32_lower_limb_filled;
    use crate::test_prelude::*;

    impl SafeMul {
        fn test_assertion_failure(&self, left: [u32; 5], right: [u32; 5], error_ids: &[i128]) {
            test_assertion_failure(
                &ShadowedClosure::new(Self),
                InitVmState::with_stack(self.set_up_test_stack((right, left))),
                error_ids,
            );
        }
    }

    #[test]
    fn rust_shadow() {
        ShadowedClosure::new(SafeMul).test()
    }

    #[test]
    fn overflow_unit_test() {
        SafeMul.test_assertion_failure(
            u128_to_u160_shl_32(u128::MAX),
            u128_to_u160_shl_32(u128::MAX),
            &[580],
        );
        SafeMul.test_assertion_failure(
            u128_to_u160_shl_32(1u128 << 64),
            u128_to_u160_shl_32(u128::MAX),
            &[581],
        );
        SafeMul.test_assertion_failure(
            u128_to_u160_shl_32(u128::MAX),
            u128_to_u160_shl_32(1u128 << 64),
            &[582],
        );
        SafeMul.test_assertion_failure(
            u128_to_u160(1u128 << 64),
            u128_to_u160(1u128 << 96),
            &[583],
        );
        SafeMul.test_assertion_failure(
            u128_to_u160(1u128 << 96),
            u128_to_u160(1u128 << 64),
            &[583],
        );
        SafeMul.test_assertion_failure(
            u128_to_u160((1u128 << 64) - 1),
            u128_to_u160(1u128 << 99),
            &[584],
        );
        SafeMul.test_assertion_failure(
            u128_to_u160(1u128 << 99),
            u128_to_u160((1u128 << 64) - 1),
            &[584],
        );
        SafeMul.test_assertion_failure(u128_to_u160(2), u128_to_u160_shl_32(1 << 127), &[583]);
        SafeMul.test_assertion_failure(u128_to_u160_shl_32(1 << 127), u128_to_u160(2), &[583]);
    }

    #[proptest(cases = 100)]
    fn arbitrary_overflow_crashes_vm_u128(
        #[strategy(2_u128..)] left: u128,
        #[strategy(u128::MAX / #left + 1..)] right: u128,
    ) {
        let left = u128_to_u160_shl_32(left);
        let right = u128_to_u160(right);
        SafeMul.test_assertion_failure(left, right, &[580, 581, 582, 583, 584, 570]);
    }

    #[proptest(cases = 50)]
    fn marginal_overflow_crashes_vm(
        #[strategy(2_u8..128)] _log_upper_bound: u8,
        #[strategy(2_u128..(1 << #_log_upper_bound))] left: u128,
    ) {
        let right = u128::MAX / left + 1;

        let expected_error_codes = [580, 581, 582, 583, 584, 100, 101, 102, 103, 570];
        SafeMul.test_assertion_failure(
            u128_to_u160_shl_32(left),
            u128_to_u160(right),
            &expected_error_codes,
        );
        SafeMul.test_assertion_failure(
            u128_to_u160(left),
            u128_to_u160_shl_32(right),
            &expected_error_codes,
        );
    }

    #[proptest(cases = 50)]
    fn arbitrary_overflow_crashes_vm(
        #[strategy(2_u8..128)] _log_upper_bound: u8,
        #[strategy(2_u128..(1 << #_log_upper_bound))] left: u128,
        #[strategy(u128::MAX / #left + 1..)] right: u128,
    ) {
        let expected_error_codes = [580, 581, 582, 583, 584, 100, 101, 102, 103, 570];
        SafeMul.test_assertion_failure(
            u128_to_u160_shl_32(left),
            u128_to_u160(right),
            &expected_error_codes,
        );
        SafeMul.test_assertion_failure(
            u128_to_u160(left),
            u128_to_u160_shl_32(right),
            &expected_error_codes,
        );
        SafeMul.test_assertion_failure(
            u128_to_u160_shl_32_lower_limb_filled(left),
            u128_to_u160(right),
            &expected_error_codes,
        );
        SafeMul.test_assertion_failure(
            u128_to_u160(left),
            u128_to_u160_shl_32_lower_limb_filled(right),
            &expected_error_codes,
        );

        // Much overflow
        SafeMul.test_assertion_failure(
            u128_to_u160_shl_32(left),
            u128_to_u160_shl_32(right),
            &expected_error_codes,
        );
        SafeMul.test_assertion_failure(
            u128_to_u160_shl_32(left),
            u128_to_u160_shl_32(right),
            &expected_error_codes,
        );
    }

    impl Closure for SafeMul {
        type Args = ([u32; 5], [u32; 5]);

        fn rust_shadow(&self, stack: &mut Vec<BFieldElement>) {
            let left: [u32; 5] = pop_encodable(stack);
            let left: BigUint = BigUint::new(left.to_vec());
            let right: [u32; 5] = pop_encodable(stack);
            let right: BigUint = BigUint::new(right.to_vec());
            let prod = left.clone() * right.clone();
            let mut prod = prod.to_u32_digits();
            assert!(prod.len() <= 5, "Overflow: left: {left}, right: {right}.");

            prod.resize(5, 0);
            let prod: [u32; 5] = prod.try_into().unwrap();

            push_encodable(stack, &prod);
        }

        fn pseudorandom_args(&self, seed: [u8; 32], _: Option<BenchmarkCase>) -> Self::Args {
            let mut rng = StdRng::from_seed(seed);
            let lhs: [u32; 5] = rng.random();
            let lhs_as_biguint = BigUint::new(lhs.to_vec());

            let u160_max = BigUint::from_bytes_be(&[0xFF; 20]);
            let max = &u160_max / &lhs_as_biguint;

            let bits: u32 = max.bits().try_into().unwrap();
            let bit_mask = BigUint::from(2u32).pow(bits) - BigUint::one();
            let mut bit_mask = bit_mask.to_bytes_be();
            bit_mask.reverse();
            bit_mask.resize(20, 0);
            bit_mask.reverse();
            let mut rhs_bytes = [0u8; 20];
            let rhs = loop {
                rng.fill(&mut rhs_bytes);
                for i in 0..20 {
                    rhs_bytes[i] &= bit_mask[i];
                }
                let candidate = BigUint::from_bytes_be(&rhs_bytes);
                if candidate < max {
                    break candidate;
                }
            };

            {
                let prod = lhs_as_biguint * rhs.clone();
                assert!(prod.to_u32_digits().len() <= 5);
            }

            let mut rhs = rhs.to_u32_digits();
            rhs.resize(5, 0);

            (lhs, rhs.try_into().unwrap())
        }

        fn corner_case_args(&self) -> Vec<Self::Args> {
            fn u160_checked_mul(l: [u32; 5], r: [u32; 5]) -> Option<[u32; 5]> {
                let l: BigUint = BigUint::new(l.to_vec());
                let r: BigUint = BigUint::new(r.to_vec());

                let prod = l * r;
                let mut prod = prod.to_u32_digits();

                if prod.len() > 5 {
                    None
                } else {
                    prod.resize(5, 0);
                    Some(prod.try_into().unwrap())
                }
            }

            let edge_case_points = vec![
                u128_to_u160(0),
                u128_to_u160(1),
                u128_to_u160(2),
                u128_to_u160(u8::MAX as u128),
                u128_to_u160(1 << 8),
                u128_to_u160(u16::MAX as u128),
                u128_to_u160(1 << 16),
                u128_to_u160(u32::MAX as u128),
                u128_to_u160(1 << 32),
                u128_to_u160(u64::MAX as u128),
                u128_to_u160(1 << 64),
                [u32::MAX, u32::MAX, u32::MAX, 0, 0],
                u128_to_u160(1 << 96),
                u128_to_u160(u128::MAX),
                [u32::MAX, u32::MAX, u32::MAX, u32::MAX, u32::MAX >> 1],
                [u32::MAX; 5],
            ];

            edge_case_points
                .iter()
                .cartesian_product(&edge_case_points)
                .filter(|&(&l, &r)| u160_checked_mul(l, r).is_some())
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
        ShadowedClosure::new(SafeMul).bench()
    }
}
