use std::collections::HashMap;

use triton_vm::prelude::*;

use crate::prelude::*;
use crate::traits::basic_snippet::Reviewer;
use crate::traits::basic_snippet::SignOffFingerprint;

/// Less-Than for `U128`s.
///
/// ### Behavior
///
/// ```text
/// BEFORE: _ [rhs; 4] [lhs; 4]
/// AFTER:  _ (lhs < rhs)
/// ```
///
/// ### Preconditions
///
/// - all input arguments are properly [`BFieldCodec`] encoded
///
/// ### Postconditions
///
/// - the output is properly [`BFieldCodec`] encoded
#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub struct Lt;

impl BasicSnippet for Lt {
    fn parameters(&self) -> Vec<(DataType, String)> {
        ["rhs", "lhs"]
            .map(|s| (DataType::U128, s.to_string()))
            .to_vec()
    }

    fn return_values(&self) -> Vec<(DataType, String)> {
        vec![(DataType::Bool, "cmp".to_string())]
    }

    fn entrypoint(&self) -> String {
        "tasmlib_arithmetic_u128_lt".to_string()
    }

    fn code(&self, library: &mut Library) -> Vec<LabelledInstruction> {
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

    fn sign_offs(&self) -> HashMap<Reviewer, SignOffFingerprint> {
        let mut sign_offs = HashMap::new();
        sign_offs.insert(Reviewer("ferdinand"), 0xf6188121bd7f252b.into());
        sign_offs
    }
}

#[cfg(test)]
mod tests {
    use rand::rngs::StdRng;

    use super::*;
    use crate::test_helpers::test_rust_equivalence_given_execution_state;
    use crate::test_prelude::*;

    impl Closure for Lt {
        type Args = (u128, u128);

        fn rust_shadow(&self, stack: &mut Vec<BFieldElement>) {
            let (right, left) = pop_encodable::<Self::Args>(stack);
            push_encodable(stack, &(left < right));
        }

        fn pseudorandom_args(&self, seed: [u8; 32], _: Option<BenchmarkCase>) -> Self::Args {
            StdRng::from_seed(seed).random()
        }
    }

    #[test]
    fn lt_u128_standard_test() {
        ShadowedClosure::new(Lt).test()
    }

    fn test_rust_tasm_equivalence(left: u128, right: u128) {
        let initial_state = InitVmState::with_stack(Lt.set_up_test_stack((left, right)));
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
    use crate::test_prelude::*;

    #[test]
    fn benchmark() {
        ShadowedClosure::new(Lt).bench()
    }
}
