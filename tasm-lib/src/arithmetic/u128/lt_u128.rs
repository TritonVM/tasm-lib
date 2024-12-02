use triton_vm::isa::triton_asm;

use crate::arithmetic::u64::lt_u64::LtU64ConsumeArgs;
use crate::data_type::DataType;
use crate::prelude::BasicSnippet;

/// Test Less-Than for `U128`s.
///
/// Consumes arguments from stack and leaves a boolean behind. When the
/// arguments are not 8 u32s, the behavior is undefined.
///
/// BEFORE: _ [rhs; 4] [lhs; 4]
/// AFTER: _ (lhs < rhs)
pub struct LtU128;

impl BasicSnippet for LtU128 {
    fn inputs(&self) -> Vec<(crate::data_type::DataType, String)> {
        vec![
            (DataType::U128, "rhs".to_string()),
            (DataType::U128, "lhs".to_string()),
        ]
    }

    fn outputs(&self) -> Vec<(crate::data_type::DataType, String)> {
        vec![(DataType::Bool, "cmp".to_string())]
    }

    fn entrypoint(&self) -> String {
        "tasmlib_arithmetic_u128_lt".to_string()
    }

    fn code(
        &self,
        library: &mut crate::prelude::Library,
    ) -> Vec<triton_vm::prelude::LabelledInstruction> {
        let entrypoint = self.entrypoint();

        let compare_u64 = DataType::U64.compare();
        let lt_u64 = library.import(Box::new(LtU64ConsumeArgs));

        triton_asm! {
            // BEFORE: _ r3 r2 r1 r0 l3 l2 l1 l0
            // AFTER: _ (l < r)
            {entrypoint}:

                pick 5 pick 5
                pick 3 pick 3
                // _ r3 r2 l3 l2 r1 r0 l1 l0

                call {lt_u64}
                // _ r3 r2 l3 l2 (l1 || l0  <  r1 || r0)

                dup 4
                dup 4
                dup 4
                dup 4
                {&compare_u64}
                // _ r3 r2 l3 l2 (l1 || l0  <  r1 || r0) (l3 || l2  ==  r3 || r2)

                mul
                // _ r3 r2 l3 l2 ((l1 || l0  <  r1 || r0) * (l3 || l2  ==  r3 || r2))

                place 4
                // _ ((l1 || l0  <  r1 || r0) * (l3 || l2  ==  r3 || r2)) r3 r2 l3 l2

                call {lt_u64}
                // _ ((l1 || l0  <  r1 || r0) * (l3 || l2  ==  r3 || r2)) (l3 || l2  <  r3 || r2)

                add
                // _ (l < r)
                return
        }
    }
}

#[cfg(test)]
mod test {

    use itertools::Itertools;
    use num::bigint::ToBigUint;
    use rand::rngs::StdRng;
    use rand::thread_rng;
    use rand::Rng;
    use rand::RngCore;
    use rand::SeedableRng;
    use triton_vm::prelude::bfe;
    use triton_vm::prelude::BFieldElement;

    use crate::test_helpers::test_rust_equivalence_given_execution_state;
    use crate::traits::closure::Closure;
    use crate::traits::closure::ShadowedClosure;
    use crate::traits::rust_shadow::RustShadow;
    use crate::InitVmState;

    use super::*;

    impl LtU128 {
        fn prepare_stack(&self, lhs: u128, rhs: u128) -> Vec<BFieldElement> {
            let init_stack = self.init_stack_for_isolated_run();
            let rhs_limbs = rhs
                .to_biguint()
                .unwrap()
                .iter_u32_digits()
                .pad_using(4, |_| 0)
                .collect_vec();
            let lhs_limbs = lhs
                .to_biguint()
                .unwrap()
                .iter_u32_digits()
                .pad_using(4, |_| 0)
                .collect_vec();

            [
                init_stack,
                rhs_limbs.into_iter().map(|l| bfe!(l)).rev().collect_vec(),
                lhs_limbs.into_iter().map(|l| bfe!(l)).rev().collect_vec(),
            ]
            .concat()
        }
    }

    impl Closure for LtU128 {
        fn rust_shadow(&self, stack: &mut Vec<triton_vm::prelude::BFieldElement>) {
            let mut left_limbs = vec![];
            for _ in 0..4 {
                left_limbs.push(stack.pop().unwrap().value() as u32);
            }
            let mut right_limbs = vec![];
            for _ in 0..4 {
                right_limbs.push(stack.pop().unwrap().value() as u32);
            }

            let right = right_limbs
                .into_iter()
                .rev()
                .fold(0u128, |acc, limb| (acc << 32) | (limb as u128));
            let left = left_limbs
                .into_iter()
                .rev()
                .fold(0u128, |acc, limb| (acc << 32) | (limb as u128));

            stack.push(bfe!((left < right) as u64));
        }

        fn pseudorandom_initial_state(
            &self,
            seed: [u8; 32],
            _bench_case: Option<crate::snippet_bencher::BenchmarkCase>,
        ) -> Vec<triton_vm::prelude::BFieldElement> {
            let mut rng: StdRng = SeedableRng::from_seed(seed);
            self.prepare_stack(rng.gen(), rng.gen())
        }
    }

    #[test]
    fn lt_u128_standard_test() {
        ShadowedClosure::new(LtU128).test()
    }

    fn test_rust_tasm_equivalence(lhs: u128, rhs: u128) {
        let initial_state = InitVmState::with_stack(LtU128.prepare_stack(lhs, rhs));
        test_rust_equivalence_given_execution_state(&ShadowedClosure::new(LtU128), initial_state);
    }

    #[test]
    fn lt_u128_unit_test() {
        test_rust_tasm_equivalence(18446744073709551616, 4294967296);
        test_rust_tasm_equivalence(4294967296, 18446744073709551616);
    }

    #[test]
    fn lt_u128_edge_cases_test() {
        let boundary_points = [0u128, 1 << 32, 1 << 64, 1 << 96, u128::MAX];
        for left_boundary in boundary_points {
            for lhs in [
                left_boundary.checked_sub(1),
                Some(left_boundary),
                left_boundary.checked_add(1),
            ]
            .into_iter()
            .flatten()
            {
                for right_boundary in boundary_points {
                    for rhs in [
                        right_boundary.checked_sub(1),
                        Some(right_boundary),
                        right_boundary.checked_add(1),
                    ]
                    .into_iter()
                    .flatten()
                    {
                        test_rust_tasm_equivalence(lhs, rhs);
                    }
                }
            }
        }
    }

    #[test]
    fn lt_u128_randomized_test_identical_args() {
        let mut rng = thread_rng();
        let lhs = rng.gen();
        test_rust_tasm_equivalence(lhs, lhs);
    }

    #[test]
    fn lt_u128_randomized_test_identical_top_limbs() {
        let mut rng = thread_rng();
        let lhs: u128 = rng.gen();
        for num in 1..=3 {
            let mut rhs = lhs;
            for i in 0..num {
                rhs |= (rng.next_u32() as u128) << (32 * i);
            }
            test_rust_tasm_equivalence(lhs, rhs);
            test_rust_tasm_equivalence(rhs, lhs);
        }
    }
}

#[cfg(test)]
mod benches {
    use super::*;
    use crate::traits::closure::ShadowedClosure;
    use crate::traits::rust_shadow::RustShadow;

    #[test]
    fn lt_u128_benchmark() {
        ShadowedClosure::new(LtU128).bench()
    }
}
