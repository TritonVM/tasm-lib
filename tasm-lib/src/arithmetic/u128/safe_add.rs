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
    fn entrypoint(&self) -> String {
        "tasmlib_arithmetic_u128_safe_add".to_string()
    }

    fn inputs(&self) -> Vec<(DataType, String)> {
        vec![
            (DataType::U128, "lhs".to_owned()),
            (DataType::U128, "rhs".to_owned()),
        ]
    }

    fn outputs(&self) -> Vec<(DataType, String)> {
        vec![(DataType::U128, "sum".to_owned())]
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
    use crate::snippet_bencher::BenchmarkCase;
    use crate::test_helpers::test_rust_equivalence_given_complete_state;
    use crate::traits::closure::Closure;
    use crate::traits::closure::ShadowedClosure;
    use crate::traits::rust_shadow::RustShadow;
    use crate::InitVmState;

    #[test]
    fn add_u128_test() {
        ShadowedClosure::new(SafeAdd).test()
    }

    #[test]
    fn add_u128_unit_test() {
        let snippet = SafeAdd;
        let mut expected = snippet.init_stack_for_isolated_run();
        expected.push(BFieldElement::new(0));
        expected.push(BFieldElement::new(1 << 4));
        expected.push(BFieldElement::new(0));
        expected.push(BFieldElement::new(0));
        snippet.assert_expected_add_behavior(1u128 << 67, 1u128 << 67, Some(&expected))
    }

    #[test]
    fn add_u128_overflow_test() {
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
                InitVmState::with_stack(snippet.setup_init_stack(a, b)),
                &[170],
            );
            test_assertion_failure(
                &ShadowedClosure::new(snippet),
                InitVmState::with_stack(snippet.setup_init_stack(b, a)),
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
                InitVmState::with_stack(snippet.setup_init_stack(a, b)),
                &[170],
            );
            test_assertion_failure(
                &ShadowedClosure::new(snippet),
                InitVmState::with_stack(snippet.setup_init_stack(b, a)),
                &[170],
            );
        }
    }

    impl SafeAdd {
        fn assert_expected_add_behavior(
            &self,
            lhs: u128,
            rhs: u128,
            expected: Option<&[BFieldElement]>,
        ) {
            let init_stack = self.setup_init_stack(lhs, rhs);

            test_rust_equivalence_given_complete_state(
                &ShadowedClosure::new(SafeAdd),
                &init_stack,
                &[],
                &NonDeterminism::default(),
                &None,
                expected,
            );
        }

        fn setup_init_stack(&self, lhs: u128, rhs: u128) -> Vec<BFieldElement> {
            [
                self.init_stack_for_isolated_run(),
                lhs.encode().into_iter().rev().collect_vec(),
                rhs.encode().into_iter().rev().collect_vec(),
            ]
            .concat()
        }
    }

    impl Closure for SafeAdd {
        fn rust_shadow(&self, stack: &mut Vec<BFieldElement>) {
            fn to_u128(a: u32, b: u32, c: u32, d: u32) -> u128 {
                a as u128
                    + b as u128 * (1u128 << 32)
                    + c as u128 * (1u128 << 64)
                    + d as u128 * (1u128 << 96)
            }

            // top element on stack
            let a0: u32 = stack.pop().unwrap().try_into().unwrap();
            let b0: u32 = stack.pop().unwrap().try_into().unwrap();
            let c0: u32 = stack.pop().unwrap().try_into().unwrap();
            let d0: u32 = stack.pop().unwrap().try_into().unwrap();
            let ab0 = to_u128(a0, b0, c0, d0);

            // second element on stack
            let a1: u32 = stack.pop().unwrap().try_into().unwrap();
            let b1: u32 = stack.pop().unwrap().try_into().unwrap();
            let c1: u32 = stack.pop().unwrap().try_into().unwrap();
            let d1: u32 = stack.pop().unwrap().try_into().unwrap();
            let ab1 = to_u128(a1, b1, c1, d1);

            let ab0_plus_ab1 = ab0.checked_add(ab1).unwrap();

            let mut res = ab0_plus_ab1.encode();
            for _ in 0..res.len() {
                stack.push(res.pop().unwrap());
            }
        }

        fn pseudorandom_initial_state(
            &self,
            seed: [u8; 32],
            _bench_case: Option<BenchmarkCase>,
        ) -> Vec<BFieldElement> {
            let mut rng = StdRng::from_seed(seed);
            let lhs: u128 = rng.gen();
            let lhs = lhs / 2;
            let rhs: u128 = rng.gen();
            let rhs = rhs / 2;

            self.setup_init_stack(lhs, rhs)
        }

        fn corner_case_initial_states(&self) -> Vec<Vec<BFieldElement>> {
            let points_with_plus_minus_one = [0, 0x200000002fffffffffff908f8, 1 << 127, u128::MAX]
                .into_iter()
                .flat_map(|p| [p.checked_sub(1), Some(p), p.checked_add(1)])
                .flatten()
                .collect_vec();

            points_with_plus_minus_one
                .iter()
                .cartesian_product(&points_with_plus_minus_one)
                .filter(|(&l, &r)| l.checked_add(r).is_some())
                .map(|(&l, &r)| self.setup_init_stack(l, r))
                .collect()
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
