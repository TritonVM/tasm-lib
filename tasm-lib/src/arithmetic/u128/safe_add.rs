use triton_vm::prelude::*;

use crate::data_type::DataType;
use crate::library::Library;
use crate::traits::basic_snippet::BasicSnippet;

#[derive(Clone, Debug, Copy)]
pub struct SafeAddU128;

impl BasicSnippet for SafeAddU128 {
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
    fn code(&self, _library: &mut Library) -> Vec<LabelledInstruction> {
        let entrypoint = self.entrypoint();

        triton_asm!(
            // BEFORE: _ rhs_3 rhs_2 rhs_1 rhs_0 lhs_3 lhs_2 lhs_1 lhs_0
            // AFTER:  _ sum_3 sum_2 sum_1 sum_0
            {entrypoint}:
                swap 1 swap 4
                add
                // _ rhs_3 rhs_2 rhs_1 lhs_1 lhs_3 lhs_2 (lhs_0 + rhs_0)

                split
                // _ rhs_3 rhs_2 rhs_1 lhs_1 lhs_3 lhs_2 (lhs_0 + rhs_0)_hi (lhs_0 + rhs_0)_lo

                // rename:
                // _ rhs_3 rhs_2 rhs_1 lhs_1 lhs_3 lhs_2 carry_1 sum_0

                swap 4
                // _ rhs_3 rhs_2 rhs_1 sum_0 lhs_3 lhs_2 carry_1 lhs_1

                add
                // _ rhs_3 rhs_2 rhs_1 sum_0 lhs_3 lhs_2 lhs_1'

                swap 1 swap 4
                // _ rhs_3 rhs_2 lhs_2 sum_0 lhs_3 lhs_1' rhs_1

                add
                // _ rhs_3 rhs_2 lhs_2 sum_0 lhs_3 (lhs_1' + rhs_1)

                split
                // _ rhs_3 rhs_2 lhs_2 sum_0 lhs_3 carry_2 sum_1

                swap 4
                // _ rhs_3 rhs_2 sum_1 sum_0 lhs_3 carry_2 lhs_2

                add
                // _ rhs_3 rhs_2 sum_1 sum_0 lhs_3 lhs_2'

                swap 1 swap 4
                // _ rhs_3 lhs_3 sum_1 sum_0 lhs_2' rhs_2

                add
                // _ rhs_3 lhs_3 sum_1 sum_0 (lhs_2' + rhs_2)

                split
                // _ rhs_3 lhs_3 sum_1 sum_0 carry_3 sum_2

                swap 4
                // _ rhs_3 sum_2 sum_1 sum_0 carry_3 lhs_3

                add
                // _ rhs_3 sum_2 sum_1 sum_0 lhs_3'

                dup 4
                // _ rhs_3 sum_2 sum_1 sum_0 lhs_3' rhs_3

                add
                // _ rhs_3 sum_2 sum_1 sum_0 (lhs_3' + rhs_3)

                split
                // _ rhs_3 sum_2 sum_1 sum_0 overflow sum_3

                swap 5
                pop 1
                // _ sum_3 sum_2 sum_1 sum_0 overflow

                push 0
                eq
                assert
                // _ sum_3 sum_2 sum_1 sum_0

                return
        )
    }
}

#[cfg(test)]
mod tests {
    use itertools::Itertools;
    use num::Zero;
    use rand::rngs::StdRng;
    use rand::Rng;
    use rand::SeedableRng;

    use crate::snippet_bencher::BenchmarkCase;
    use crate::test_helpers::negative_test;
    use crate::test_helpers::test_rust_equivalence_given_complete_state;
    use crate::traits::closure::Closure;
    use crate::traits::closure::ShadowedClosure;
    use crate::traits::rust_shadow::RustShadow;
    use crate::InitVmState;

    use super::*;

    #[test]
    fn add_u128_test() {
        ShadowedClosure::new(SafeAddU128).test()
    }

    #[test]
    fn add_u128_unit_test() {
        let snippet = SafeAddU128;
        let mut expected = snippet.init_stack_for_isolated_run();
        expected.push(BFieldElement::new(0));
        expected.push(BFieldElement::new(1 << 4));
        expected.push(BFieldElement::new(0));
        expected.push(BFieldElement::new(0));
        snippet.prop_add(1u128 << 67, 1u128 << 67, Some(&expected))
    }

    #[test]
    fn add_u128_overflow_test() {
        let snippet = SafeAddU128;

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
            negative_test(
                &ShadowedClosure::new(snippet),
                InitVmState::with_stack(snippet.setup_init_stack(a, b)),
                &[InstructionError::AssertionFailed],
            );
            negative_test(
                &ShadowedClosure::new(snippet),
                InitVmState::with_stack(snippet.setup_init_stack(b, a)),
                &[InstructionError::AssertionFailed],
            );
        }

        for i in 0..128 {
            let a = u128::MAX - ((1u128 << i) - 1);
            let b = 1u128 << i;

            // sanity check of test input values
            assert!(a.wrapping_add(b).is_zero());

            negative_test(
                &ShadowedClosure::new(snippet),
                InitVmState::with_stack(snippet.setup_init_stack(a, b)),
                &[InstructionError::AssertionFailed],
            );
            negative_test(
                &ShadowedClosure::new(snippet),
                InitVmState::with_stack(snippet.setup_init_stack(b, a)),
                &[InstructionError::AssertionFailed],
            );
        }
    }

    impl SafeAddU128 {
        fn prop_add(&self, lhs: u128, rhs: u128, expected: Option<&[BFieldElement]>) {
            let init_stack = self.setup_init_stack(lhs, rhs);

            test_rust_equivalence_given_complete_state(
                &ShadowedClosure::new(SafeAddU128),
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

    impl Closure for SafeAddU128 {
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
            let mut rng: StdRng = SeedableRng::from_seed(seed);
            let lhs: u128 = rng.gen();
            let lhs = lhs / 2;
            let rhs: u128 = rng.gen();
            let rhs = rhs / 2;

            self.setup_init_stack(lhs, rhs)
        }

        fn corner_case_initial_states(&self) -> Vec<Vec<BFieldElement>> {
            let some_none_zero_value = (1u128 << 97) + (1u128 << 65) + u64::MAX as u128 - 456456;
            let rhs_is_zero = self.setup_init_stack(some_none_zero_value, 0);
            let lhs_is_zero = self.setup_init_stack(0, some_none_zero_value);
            let rhs_is_zero_lhs_max = self.setup_init_stack(u128::MAX, 0);
            let lhs_is_zero_rhs_max = self.setup_init_stack(0, u128::MAX);

            vec![
                rhs_is_zero,
                lhs_is_zero,
                rhs_is_zero_lhs_max,
                lhs_is_zero_rhs_max,
            ]
        }
    }
}

#[cfg(test)]
mod benches {
    use crate::traits::closure::ShadowedClosure;
    use crate::traits::rust_shadow::RustShadow;

    use super::*;

    #[test]
    fn add_u128_benchmark() {
        ShadowedClosure::new(SafeAddU128).bench()
    }
}
