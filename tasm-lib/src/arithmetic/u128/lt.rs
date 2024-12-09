use triton_vm::isa::triton_asm;

use crate::data_type::DataType;
use crate::prelude::BasicSnippet;

/// Test Less-Than for `U128`s.
///
/// Consumes arguments from stack and leaves a boolean behind. When the
/// arguments are not 8 u32s, the behavior is undefined.
///
/// ```text
/// BEFORE: _ [rhs; 4] [lhs; 4]
/// AFTER:  _ (lhs < rhs)
/// ```
pub struct Lt;

impl BasicSnippet for Lt {
    fn inputs(&self) -> Vec<(DataType, String)> {
        ["rhs", "lhs"]
            .map(|s| (DataType::U128, s.to_string()))
            .to_vec()
    }

    fn outputs(&self) -> Vec<(DataType, String)> {
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
        let lt_u64 = library.import(Box::new(crate::arithmetic::u64::lt::Lt));

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
    use rand::rngs::StdRng;
    use rand::Rng;
    use rand::SeedableRng;
    use test_strategy::proptest;
    use triton_vm::prelude::bfe;
    use triton_vm::prelude::BFieldElement;

    use crate::test_helpers::test_rust_equivalence_given_execution_state;
    use crate::traits::closure::Closure;
    use crate::traits::closure::ShadowedClosure;
    use crate::traits::rust_shadow::RustShadow;
    use crate::twenty_first::prelude::BFieldCodec;
    use crate::InitVmState;

    use super::*;

    impl Lt {
        fn prepare_stack(&self, left: u128, right: u128) -> Vec<BFieldElement> {
            let u128_for_stack = |v: u128| v.encode().into_iter().rev();

            let mut stack = self.init_stack_for_isolated_run();
            stack.extend(u128_for_stack(right));
            stack.extend(u128_for_stack(left));

            stack
        }
    }

    impl Closure for Lt {
        fn rust_shadow(&self, stack: &mut Vec<BFieldElement>) {
            fn pop_u128(stack: &mut Vec<BFieldElement>) -> u128 {
                let num_limbs = u128::static_length().unwrap();
                let limbs = (0..num_limbs).map(|_| stack.pop().unwrap()).collect_vec();
                *BFieldCodec::decode(&limbs).unwrap()
            }

            let left = pop_u128(stack);
            let right = pop_u128(stack);
            stack.push(bfe!((left < right) as u64));
        }

        fn pseudorandom_initial_state(
            &self,
            seed: [u8; 32],
            _bench_case: Option<crate::snippet_bencher::BenchmarkCase>,
        ) -> Vec<BFieldElement> {
            let mut rng = StdRng::from_seed(seed);
            self.prepare_stack(rng.gen(), rng.gen())
        }
    }

    #[test]
    fn lt_u128_standard_test() {
        ShadowedClosure::new(Lt).test()
    }

    fn test_rust_tasm_equivalence(left: u128, right: u128) {
        let initial_state = InitVmState::with_stack(Lt.prepare_stack(left, right));
        test_rust_equivalence_given_execution_state(&ShadowedClosure::new(Lt), initial_state);
    }

    #[test]
    fn lt_u128_unit_test() {
        test_rust_tasm_equivalence(1 << 64, 1 << 32);
        test_rust_tasm_equivalence(1 << 32, 1 << 64);
    }

    #[test]
    fn lt_u128_edge_cases_test() {
        let boundary_points = [0, 1 << 32, 1 << 64, 1 << 96, u128::MAX]
            .into_iter()
            .flat_map(|p| [p.checked_sub(1), Some(p), p.checked_add(1)])
            .flatten()
            .collect_vec();

        for (&left, &right) in boundary_points.iter().cartesian_product(&boundary_points) {
            test_rust_tasm_equivalence(left, right);
        }
    }

    #[proptest]
    fn lt_u128_randomized_test_identical_args(v: u128) {
        test_rust_tasm_equivalence(v, v);
    }

    #[proptest]
    fn lt_u128_randomized_test_identical_top_limbs(left: u128, right: u128) {
        test_rust_tasm_equivalence(left, replace_bottom_n_bits::<32>(left, right));
        test_rust_tasm_equivalence(left, replace_bottom_n_bits::<64>(left, right));
        test_rust_tasm_equivalence(left, replace_bottom_n_bits::<96>(left, right));
    }

    /// Modify the first argument by replacing its least-significant 0 <= `N` < 128
    /// bits with those of the second argument.
    const fn replace_bottom_n_bits<const N: u8>(left: u128, right: u128) -> u128 {
        let bottom_bits_mask = (1 << N) - 1;

        ((left >> N) << N) | (right & bottom_bits_mask)
    }

    #[test]
    fn bit_replacement_works_as_expected() {
        const fn abcd0xf<const N: u8>() -> u128 {
            replace_bottom_n_bits::<N>(0xaaaa_aaaa_bbbb_bbbb_cccc_cccc_dddd_dddd, u128::MAX)
        }

        assert_eq!(0xaaaa_aaaa_bbbb_bbbb_cccc_cccc_dddd_dddd, abcd0xf::<0>());
        assert_eq!(0xaaaa_aaaa_bbbb_bbbb_cccc_cccc_ffff_ffff, abcd0xf::<32>());
        assert_eq!(0xaaaa_aaaa_bbbb_bbbb_ffff_ffff_ffff_ffff, abcd0xf::<64>());
        assert_eq!(0xaaaa_aaaa_ffff_ffff_ffff_ffff_ffff_ffff, abcd0xf::<96>());
        assert_eq!(0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff, abcd0xf::<127>());
    }
}

#[cfg(test)]
mod benches {
    use super::*;
    use crate::traits::closure::ShadowedClosure;
    use crate::traits::rust_shadow::RustShadow;

    #[test]
    fn lt_u128_benchmark() {
        ShadowedClosure::new(Lt).bench()
    }
}
