use std::collections::HashMap;

use num::Zero;
use rand::Rng;
use twenty_first::amount::u32s::U32s;
use twenty_first::shared_math::b_field_element::BFieldElement;
use twenty_first::shared_math::bfield_codec::BFieldCodec;

use crate::library::Library;
use crate::snippet::{DataType, DeprecatedSnippet};
use crate::{empty_stack, push_encodable, ExecutionState};

#[derive(Clone, Debug)]
pub struct AddU64;

impl DeprecatedSnippet for AddU64 {
    fn input_field_names(&self) -> Vec<String> {
        vec![
            "rhs_hi".to_string(),
            "rhs_lo".to_string(),
            "lhs_hi".to_string(),
            "lhs_lo".to_string(),
        ]
    }

    fn output_field_names(&self) -> Vec<String> {
        vec!["(lhs + rhs)_hi".to_string(), "(lhs + rhs)_lo".to_string()]
    }

    fn input_types(&self) -> Vec<crate::snippet::DataType> {
        vec![DataType::U64, DataType::U64]
    }

    fn output_types(&self) -> Vec<crate::snippet::DataType> {
        vec![DataType::U64]
    }

    fn crash_conditions(&self) -> Vec<String> {
        vec!["if (lhs + rhs) overflows u64".to_string()]
    }

    fn gen_input_states(&self) -> Vec<ExecutionState> {
        let mut rng = rand::thread_rng();

        let zero = U32s::<2>::zero();
        let small_a = U32s::<2>::try_from(rng.gen::<u32>()).unwrap();
        let small_b = U32s::<2>::try_from(rng.gen::<u32>()).unwrap();
        let large_a = U32s::<2>::try_from(rng.gen::<u64>()).unwrap();
        // let large_b = U32s::<2>::try_from(rng.gen::<u64>()).unwrap();

        let mut states = vec![];

        // 0. one zero, one large
        states.push({
            let mut stack = empty_stack();
            push_encodable(&mut stack, &zero);
            push_encodable(&mut stack, &large_a);
            ExecutionState::with_stack(stack)
        });

        // 1. two small
        states.push({
            let mut stack = empty_stack();
            push_encodable(&mut stack, &small_a);
            push_encodable(&mut stack, &small_b);
            ExecutionState::with_stack(stack)
        });

        states
    }

    fn stack_diff(&self) -> isize {
        -2
    }

    fn entrypoint_name(&self) -> String {
        "tasm_arithmetic_u64_add".to_string()
    }

    /// Four top elements of stack are assumed to be valid u32s. So to have
    /// a value that's less than 2^32.
    fn function_code(&self, _library: &mut Library) -> String {
        let entrypoint = self.entrypoint_name();

        format!(
            "
            // BEFORE: _ rhs_hi rhs_lo lhs_hi lhs_lo
            // AFTER: _ sum_hi sum_lo
            {entrypoint}:
                swap 1 swap 2
                // _ rhs_hi lhs_hi lhs_lo rhs_lo

                add
                split
                // _ rhs_hi lhs_hi carry sum_lo

                swap 3
                // _ sum_lo lhs_hi carry rhs_hi

                add
                add
                // _ sum_lo (lhs_hi+rhs_hi+carry)

                split
                // _ sum_lo overflow sum_hi

                swap 1
                push 0
                eq
                assert
                // _ sum_lo sum_hi

                swap 1
                // _ sum_hi sum_lo

                return
            "
        )
    }

    fn rust_shadowing(
        &self,
        stack: &mut Vec<BFieldElement>,
        _std_in: Vec<BFieldElement>,
        _secret_in: Vec<BFieldElement>,
        _memory: &mut HashMap<BFieldElement, BFieldElement>,
    ) {
        // top element on stack
        let a0: u32 = stack.pop().unwrap().try_into().unwrap();
        let b0: u32 = stack.pop().unwrap().try_into().unwrap();
        let ab0 = U32s::<2>::new([a0, b0]);

        // second element on stack
        let a1: u32 = stack.pop().unwrap().try_into().unwrap();
        let b1: u32 = stack.pop().unwrap().try_into().unwrap();
        let ab1 = U32s::<2>::new([a1, b1]);
        let ab0_plus_ab1 = ab0 + ab1;
        let mut res = ab0_plus_ab1.encode();
        for _ in 0..res.len() {
            stack.push(res.pop().unwrap());
        }
    }

    fn common_case_input_state(&self) -> ExecutionState {
        ExecutionState::with_stack(
            [
                empty_stack(),
                vec![BFieldElement::zero(), BFieldElement::new(1 << 31)],
                vec![BFieldElement::zero(), BFieldElement::new(1 << 30)],
            ]
            .concat(),
        )
    }

    fn worst_case_input_state(&self) -> ExecutionState {
        ExecutionState::with_stack(
            [
                empty_stack(),
                vec![BFieldElement::new(1 << 31), BFieldElement::new(1 << 31)],
                vec![
                    BFieldElement::new(1 << 30),
                    BFieldElement::new((1 << 31) + 10),
                ],
            ]
            .concat(),
        )
    }
}

#[cfg(test)]
mod tests {
    use num::{BigUint, One, Zero};
    use rand::RngCore;

    use crate::empty_stack;

    use crate::test_helpers::{
        test_rust_equivalence_given_input_values_deprecated,
        test_rust_equivalence_multiple_deprecated,
    };

    use super::*;

    #[test]
    fn add_u64_test() {
        test_rust_equivalence_multiple_deprecated(&AddU64, true);
    }

    #[test]
    fn u32s_2_add_no_overflow() {
        // 127 + 129 = 256
        let mut expected_end_stack = [
            empty_stack(),
            vec![BFieldElement::zero(), BFieldElement::new(256)],
        ]
        .concat();
        prop_add(
            U32s::new([127, 0]),
            U32s::new([129, 0]),
            Some(&expected_end_stack),
        );

        // 127 + 129 + 45 * 2^32 + 1000 * 2^32 = 256 + 1045*2^32
        expected_end_stack = [
            empty_stack(),
            vec![BFieldElement::new(1045), BFieldElement::new(256)],
        ]
        .concat();
        prop_add(
            U32s::new([127, 45]),
            U32s::new([129, 1000]),
            Some(&expected_end_stack),
        );

        // (2^32 - 1) + 0 + 0 * 2^32 + 2004 * 2^32 = (2^32 - 1) + 2004*2^32
        expected_end_stack = [
            empty_stack(),
            vec![
                BFieldElement::new(2004),
                BFieldElement::new(u32::MAX as u64),
            ],
        ]
        .concat();
        prop_add(
            U32s::new([u32::MAX, 0]),
            U32s::new([0, 2004]),
            Some(&expected_end_stack),
        );

        // (2^31 - 1) + 2^31 + 14 * 2^32 + 10^9 * 2^32 = (2^32 - 1) + (10^9 + 14) * 2^32
        expected_end_stack = [
            empty_stack(),
            vec![
                BFieldElement::new(1_000_000_014),
                BFieldElement::new(u32::MAX as u64),
            ],
        ]
        .concat();
        prop_add(
            U32s::new([(1 << 31) - 1, 14]),
            U32s::new([1 << 31, 1_000_000_000]),
            Some(&expected_end_stack),
        );
    }

    #[test]
    fn u32s_2_add_with_overflow_in_least_significant_u32() {
        // 2 ^ 31 + 2 ^ 31 = 0 + 1 * 2 ^32
        let expected_end_stack = [
            empty_stack(),
            vec![BFieldElement::one(), BFieldElement::zero()],
        ]
        .concat();
        prop_add(
            U32s::new([1 << 31, 0]),
            U32s::new([1 << 31, 0]),
            Some(&expected_end_stack),
        );

        // 2 ^ 32 + 2 ^ 32 - 1 - 2 = (1^32 - 3) + 1 * 2^32
        let expected_end_stack = [
            empty_stack(),
            vec![BFieldElement::one(), BFieldElement::new((1 << 32) - 3)],
        ]
        .concat();
        prop_add(
            U32s::new([((1u64 << 32) - 1) as u32, 0]),
            U32s::new([((1u64 << 32) - 2) as u32, 0]),
            Some(&expected_end_stack),
        );
    }

    #[test]
    fn u32s_2_add_pbt() {
        let mut rng = rand::thread_rng();
        for _ in 0..100 {
            prop_add(
                U32s::new([rng.next_u32(), rng.next_u32() / 2]),
                U32s::new([rng.next_u32(), rng.next_u32() / 2]),
                None,
            );
        }
    }

    #[should_panic]
    #[test]
    fn overflow_test() {
        let lhs: U32s<2> = U32s::from(BigUint::from(1u64 << 63));
        let rhs: U32s<2> = U32s::from(BigUint::from((1u64 << 63) + 1));
        let mut init_stack = empty_stack();
        for elem in rhs.encode().into_iter().rev() {
            init_stack.push(elem);
        }
        for elem in lhs.encode().into_iter().rev() {
            init_stack.push(elem);
        }

        AddU64.link_and_run_tasm_from_state_for_test(&mut ExecutionState::with_stack(init_stack));
    }

    #[should_panic]
    #[test]
    fn overflow_test_2() {
        let lhs: U32s<2> = U32s::from(BigUint::from(u64::MAX));
        let rhs: U32s<2> = U32s::from(BigUint::from(u64::MAX));
        let mut init_stack = empty_stack();
        for elem in rhs.encode().into_iter().rev() {
            init_stack.push(elem);
        }
        for elem in lhs.encode().into_iter().rev() {
            init_stack.push(elem);
        }

        AddU64.link_and_run_tasm_from_state_for_test(&mut ExecutionState::with_stack(init_stack));
    }

    fn prop_add(lhs: U32s<2>, rhs: U32s<2>, expected: Option<&[BFieldElement]>) {
        let mut init_stack = empty_stack();
        for elem in rhs.encode().into_iter().rev() {
            init_stack.push(elem);
        }
        for elem in lhs.encode().into_iter().rev() {
            init_stack.push(elem);
        }

        test_rust_equivalence_given_input_values_deprecated(
            &AddU64,
            &init_stack,
            &[],
            &mut HashMap::default(),
            0,
            expected,
        );
    }
}

#[cfg(test)]
mod benches {
    use super::*;
    use crate::snippet_bencher::bench_and_write;

    #[test]
    fn add_u64_benchmark() {
        bench_and_write(AddU64);
    }
}
