use triton_vm::prelude::*;

use crate::data_type::DataType;
use crate::library::Library;
use crate::traits::basic_snippet::BasicSnippet;

#[derive(Clone, Debug, Copy)]
pub struct OverflowingAddU128;

impl BasicSnippet for OverflowingAddU128 {
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

    use super::*;
    use crate::push_encodable;
    use crate::snippet_bencher::BenchmarkCase;
    use crate::test_helpers::test_rust_equivalence_given_complete_state;
    use crate::traits::closure::Closure;
    use crate::traits::closure::ShadowedClosure;
    use crate::traits::rust_shadow::RustShadow;

    #[test]
    fn overflowing_add_u128_test() {
        ShadowedClosure::new(OverflowingAddU128).test()
    }

    #[test]
    fn overflowing_add_u128_unit_test() {
        let snippet = OverflowingAddU128;
        snippet.prop_add(1u128 << 67, 1u128 << 67)
    }

    #[test]
    fn overflowing_add_u128_overflow_test() {
        let snippet = OverflowingAddU128;

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
            snippet.prop_add(a, b);
            snippet.prop_add(b, a);
        }

        for i in 0..128 {
            let a = u128::MAX - ((1u128 << i) - 1);
            let b = 1u128 << i;

            // sanity check of test input values
            let (wrapped_add, is_overflow) = a.overflowing_add(b);
            assert!(is_overflow, "i = {i}. a = {a}, b = {b}");
            assert!(wrapped_add.is_zero());

            snippet.prop_add(b, a);
        }
    }

    impl OverflowingAddU128 {
        fn prop_add(&self, lhs: u128, rhs: u128) {
            let init_stack = self.setup_init_stack(lhs, rhs);

            let expected = {
                let (sum, overflow) = lhs.overflowing_add(rhs);
                let mut stack = self.init_stack_for_isolated_run();
                push_encodable(&mut stack, &sum);
                push_encodable(&mut stack, &overflow);
                stack
            };

            test_rust_equivalence_given_complete_state(
                &ShadowedClosure::new(OverflowingAddU128),
                &init_stack,
                &[],
                &NonDeterminism::default(),
                &None,
                Some(&expected),
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

    impl Closure for OverflowingAddU128 {
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

            let (sum, overflow) = ab0.overflowing_add(ab1);

            let mut res = sum.encode();
            for _ in 0..res.len() {
                stack.push(res.pop().unwrap());
            }

            stack.push(bfe!(overflow as u64));
        }

        fn pseudorandom_initial_state(
            &self,
            seed: [u8; 32],
            _bench_case: Option<BenchmarkCase>,
        ) -> Vec<BFieldElement> {
            let mut rng = StdRng::from_seed(seed);
            let lhs: u128 = rng.gen();
            let rhs: u128 = rng.gen();

            self.setup_init_stack(lhs, rhs)
        }

        fn corner_case_initial_states(&self) -> Vec<Vec<BFieldElement>> {
            let some_none_zero_value = (1u128 << 97) + (1u128 << 65) + u64::MAX as u128 - 456456;
            let rhs_is_zero = self.setup_init_stack(some_none_zero_value, 0);
            let lhs_is_zero = self.setup_init_stack(0, some_none_zero_value);
            let rhs_is_zero_lhs_max = self.setup_init_stack(u128::MAX, 0);
            let lhs_is_zero_rhs_max = self.setup_init_stack(0, u128::MAX);
            let both_are_2_to_127 = self.setup_init_stack(1u128 << 127, 1u128 << 127);
            let two_to_127_var0 = self.setup_init_stack((1u128 << 127) - 1, (1u128 << 127) - 1);
            let two_to_127_var1 = self.setup_init_stack((1u128 << 127) - 1, 1u128 << 127);
            let two_to_127_var2 = self.setup_init_stack((1u128 << 127) - 1, (1u128 << 127) + 1);
            let two_to_127_var3 = self.setup_init_stack(1u128 << 127, (1u128 << 127) - 1);
            let two_to_127_var4 = self.setup_init_stack(1u128 << 127, (1u128 << 127) + 1);
            let two_to_127_var5 = self.setup_init_stack((1u128 << 127) + 1, (1u128 << 127) - 1);
            let two_to_127_var6 = self.setup_init_stack((1u128 << 127) + 1, 1u128 << 127);
            let two_to_127_var7 = self.setup_init_stack((1u128 << 127) + 1, (1u128 << 127) + 1);
            let max_plus_max = self.setup_init_stack(u128::MAX, u128::MAX);

            vec![
                rhs_is_zero,
                lhs_is_zero,
                rhs_is_zero_lhs_max,
                lhs_is_zero_rhs_max,
                both_are_2_to_127,
                two_to_127_var0,
                two_to_127_var1,
                two_to_127_var2,
                two_to_127_var3,
                two_to_127_var4,
                two_to_127_var5,
                two_to_127_var6,
                two_to_127_var7,
                max_plus_max,
            ]
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
        ShadowedClosure::new(OverflowingAddU128).bench()
    }
}
