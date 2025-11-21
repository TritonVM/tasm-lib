use triton_vm::prelude::*;

use crate::prelude::*;

/// Less-Than for `U160`s.
///
/// ### Behavior
///
/// ```text
/// BEFORE: _ [rhs; 5] [lhs; 5]
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
            .map(|s| (DataType::U160, s.to_string()))
            .to_vec()
    }

    fn return_values(&self) -> Vec<(DataType, String)> {
        vec![(DataType::Bool, "cmp".to_string())]
    }

    fn entrypoint(&self) -> String {
        "tasmlib_arithmetic_u160_lt".to_string()
    }

    fn code(&self, library: &mut Library) -> Vec<LabelledInstruction> {
        let entrypoint = self.entrypoint();

        let lt_u128 = library.import(Box::new(crate::arithmetic::u128::lt::Lt));

        triton_asm! {
            // BEFORE: _ r4 r3 r2 r1 r0 l4 l3 l2 l1 l0
            // AFTER: _ (l < r)
            {entrypoint}:

                // l < r <=> l4 < r4 || l4 == r4 && l3 < r3 || l4 == r4 && l3 == r3 && l2 < r2
                //        || l4 == r4 && l3 == r3 && l2 == r2 && l1 < r1
                //        || l4 == r4 && l3 == r3 && l2 == r2 && l1 == r1 && l0 < r0

                // Expressed as u128 inequality
                // l4 < r4 || l4 == r4 && l_lo < r_lo

                pick 4
                place 8
                // _ r4 l4 r3 r2 r1 r0 l3 l2 l1 l0

                call {lt_u128}
                // _ r4 l4 (r_lo > l_lo)

                dup 2
                dup 2
                eq
                // _ r4 l4 (r_lo > l_lo) (r4 == l4)

                mul
                // _ r4 l4 ((r_lo > l_lo) && (r4 == l4))

                place 2
                lt
                add
                // _ (((r_lo > l_lo) && (r4 == l4)) || (r4 > l4))
                // _ l < r

                return
        }
    }
}

#[cfg(test)]
mod tests {
    use num::BigUint;
    use rand::rngs::StdRng;

    use super::*;
    use crate::arithmetic::u160::u128_to_u160;
    use crate::test_helpers::test_rust_equivalence_given_execution_state;
    use crate::test_prelude::*;

    impl Closure for Lt {
        type Args = ([u32; 5], [u32; 5]);

        fn rust_shadow(&self, stack: &mut Vec<BFieldElement>) {
            let left: [u32; 5] = pop_encodable(stack);
            let left: BigUint = BigUint::new(left.to_vec());

            let right: [u32; 5] = pop_encodable(stack);
            let right: BigUint = BigUint::new(right.to_vec());
            push_encodable(stack, &(left < right));
        }

        fn pseudorandom_args(&self, seed: [u8; 32], _: Option<BenchmarkCase>) -> Self::Args {
            StdRng::from_seed(seed).random()
        }
    }

    fn test_rust_tasm_equivalence(left: [u32; 5], right: [u32; 5]) {
        let initial_state = InitVmState::with_stack(Lt.set_up_test_stack((left, right)));
        test_rust_equivalence_given_execution_state(&ShadowedClosure::new(Lt), initial_state);
    }

    #[test]
    fn lt_u160_standard_test() {
        ShadowedClosure::new(Lt).test()
    }

    #[test]
    fn lt_u160_edge_cases_test() {
        let mut boundary_points = [0, 1 << 32, 1 << 64, 1 << 96, u128::MAX]
            .into_iter()
            .flat_map(|p| [p.checked_sub(1), Some(p), p.checked_add(1)])
            .flatten()
            .map(u128_to_u160)
            .collect_vec();
        boundary_points.extend([
            [u32::MAX; 5],
            [1; 5],
            [0, 0, 0, 0, 1],
            [0, 0, 0, 0, u32::MAX],
            [0x72581af6, 0, 0, 0, 0x72581af6],
            [0x72581af6, 0, 0, 0, 0x72581af5],
            [0x72581af5, 0, 0, 0, 0x72581af6],
            [0x72581af5, 0, 0, 0, 0x72581af5],
        ]);

        for (&left, &right) in boundary_points.iter().cartesian_product(&boundary_points) {
            test_rust_tasm_equivalence(left, right);
        }
    }

    #[proptest]
    fn lt_u160_randomized_test_identical_args(v: [u32; 5]) {
        test_rust_tasm_equivalence(v, v);
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
