use num::{One, Zero};
use rand::Rng;
use std::collections::HashMap;
use twenty_first::shared_math::bfield_codec::BFieldCodec;

use twenty_first::amount::u32s::U32s;
use twenty_first::shared_math::b_field_element::BFieldElement;

use crate::library::Library;
use crate::snippet::{DataType, DeprecatedSnippet};
use crate::{get_init_tvm_stack, push_encodable, ExecutionState};

#[derive(Clone, Debug)]
pub struct SubU64;

impl DeprecatedSnippet for SubU64 {
    fn input_field_names(&self) -> Vec<String> {
        vec![
            "rhs_hi".to_string(),
            "rhs_lo".to_string(),
            "lhs_hi".to_string(),
            "lhs_lo".to_string(),
        ]
    }

    fn output_field_names(&self) -> Vec<String> {
        vec!["(lhs - rhs)_hi".to_string(), "(lhs - rhs)_lo".to_string()]
    }

    fn input_types(&self) -> Vec<crate::snippet::DataType> {
        vec![DataType::U64, DataType::U64]
    }

    fn output_types(&self) -> Vec<crate::snippet::DataType> {
        vec![DataType::U64]
    }

    fn crash_conditions(&self) -> Vec<String> {
        vec!["if (lhs - rhs) overflows u64".to_string()]
    }

    fn gen_input_states(&self) -> Vec<ExecutionState> {
        let mut rng = rand::thread_rng();

        let mut ret = vec![];
        for _ in 0..30 {
            // no overflow, no carry: small_a - smaller_b
            // small_a is 0..2^32, smaller_b < small_a
            let (small_a, smaller_b) = {
                let a: u32 = rng.gen();
                let b: u32 = rng.gen_range(0..=a);

                (
                    U32s::<2>::try_from(a).unwrap(),
                    U32s::<2>::try_from(b).unwrap(),
                )
            };

            let mut stack_1 = get_init_tvm_stack();
            push_encodable(&mut stack_1, &smaller_b);
            push_encodable(&mut stack_1, &small_a);
            ret.push(ExecutionState::with_stack(stack_1));

            // no overflow, carry: large_c - smaller_carry_d
            // large_c is 2^32..2^64, smaller_carry_d < large_c
            let (large_c, smaller_carry_d) = {
                let c: u64 = rng.gen::<u32>() as u64 + (1 << 32);
                let d: u64 = rng.gen_range(0..=c);
                (
                    U32s::<2>::try_from(c).unwrap(),
                    U32s::<2>::try_from(d).unwrap(),
                )
            };

            let mut stack_2 = get_init_tvm_stack();
            push_encodable(&mut stack_2, &smaller_carry_d);
            push_encodable(&mut stack_2, &large_c);
            ret.push(ExecutionState::with_stack(stack_2));

            // no overflow, no carry: large_e - smaller_f
            // large_e is 0..2^64, smaller_f < large_e
            let (large_e, smaller_f) = {
                let e: u64 = rng.gen_range((1 << 32)..u64::MAX);
                let f: u64 = rng.gen_range(0..(e & 0xffff_ffff));
                (
                    U32s::<2>::try_from(e).unwrap(),
                    U32s::<2>::try_from(f).unwrap(),
                )
            };

            let mut stack_3 = get_init_tvm_stack();
            push_encodable(&mut stack_3, &smaller_f);
            push_encodable(&mut stack_3, &large_e);
            ret.push(ExecutionState::with_stack(stack_3));
        }

        ret
    }

    fn stack_diff(&self) -> isize {
        -2
    }

    fn entrypoint_name(&self) -> String {
        "tasm_arithmetic_u64_sub".to_string()
    }

    /// Four top elements of stack are assumed to be valid u32s. So to have
    /// a value that's less than 2^32.
    fn function_code(&self, _library: &mut Library) -> String {
        let entrypoint = self.entrypoint_name();
        const TWO_POW_32: &str = "4294967296";

        format!(
            "
            // Before: _ rhs_hi rhs_lo lhs_hi lhs_lo
            // After: _ hi_diff lo_diff
            {entrypoint}:
                swap 1 swap 2
                // _ rhs_hi lhs_hi lhs_lo rhs_lo

                push -1
                mul
                add
                // _ rhs_hi lhs_hi (lhs_lo - rhs_lo)

                push {TWO_POW_32}
                add

                split
                // _ rhs_hi lhs_hi !carry diff_lo

                swap 3 swap 1
                // _ diff_lo lhs_hi rhs_hi !carry

                push 0
                eq
                // _ diff_lo lhs_hi rhs_hi carry

                add
                // _ diff_lo lhs_hi rhs_hi'

                push -1
                mul
                add
                // _ diff_lo (lhs_hi - rhs_hi')

                split
                // _ diff_lo overflow diff_hi

                swap 1
                push 0
                eq
                assert
                // _ diff_lo diff_hi

                swap 1

                return
            "
        )
    }

    fn rust_shadowing(
        &self,
        stack: &mut Vec<BFieldElement>,
        _std_in: Vec<BFieldElement>,
        _secret_in: Vec<BFieldElement>,
        _init_memory: &mut HashMap<BFieldElement, BFieldElement>,
    ) {
        // top element on stack
        let a0: u32 = stack.pop().unwrap().try_into().unwrap();
        let b0: u32 = stack.pop().unwrap().try_into().unwrap();
        let ab0 = U32s::<2>::new([a0, b0]);

        // second element on stack
        let a1: u32 = stack.pop().unwrap().try_into().unwrap();
        let b1: u32 = stack.pop().unwrap().try_into().unwrap();
        let ab1 = U32s::<2>::new([a1, b1]);
        let ab0_minus_ab1 = ab0 - ab1;
        let mut res = ab0_minus_ab1.encode();
        for _ in 0..res.len() {
            stack.push(res.pop().unwrap());
        }
    }

    fn common_case_input_state(&self) -> ExecutionState {
        // no carry
        ExecutionState::with_stack(
            vec![
                get_init_tvm_stack(),
                vec![BFieldElement::zero(), BFieldElement::new((1 << 10) - 1)],
                vec![BFieldElement::zero(), BFieldElement::new((1 << 31) - 1)],
            ]
            .concat(),
        )
    }

    fn worst_case_input_state(&self) -> ExecutionState {
        // with carry
        ExecutionState::with_stack(
            vec![
                get_init_tvm_stack(),
                vec![BFieldElement::one(), BFieldElement::new((1 << 31) - 1)],
                vec![BFieldElement::new(100), BFieldElement::new((1 << 10) - 1)],
            ]
            .concat(),
        )
    }
}

#[cfg(test)]
mod tests {
    use num::{BigUint, Zero};
    use rand::Rng;
    use twenty_first::shared_math::b_field_element::BFieldElement;

    use crate::get_init_tvm_stack;

    use crate::test_helpers::{
        test_rust_equivalence_given_input_values_deprecated,
        test_rust_equivalence_multiple_deprecated,
    };

    use super::*;

    #[test]
    fn sub_u64_test() {
        test_rust_equivalence_multiple_deprecated(&SubU64, true);
    }

    #[test]
    fn subtraction_involving_zeros() {
        // 0 - 0 = 0
        let mut expected_end_stack = vec![
            get_init_tvm_stack(),
            vec![BFieldElement::zero(), BFieldElement::zero()],
        ]
        .concat();
        prop_sub(U32s::from(0), U32s::from(0), Some(&expected_end_stack));

        // 1 - 0 = 1
        expected_end_stack = vec![
            get_init_tvm_stack(),
            vec![BFieldElement::zero(), BFieldElement::one()],
        ]
        .concat();
        prop_sub(U32s::from(1), U32s::from(0), Some(&expected_end_stack));

        // 1 - 1 = 0
        expected_end_stack = vec![
            get_init_tvm_stack(),
            vec![BFieldElement::zero(), BFieldElement::zero()],
        ]
        .concat();
        prop_sub(U32s::from(1), U32s::from(1), Some(&expected_end_stack));

        // u64::MAX - u64::MAX = 0
        expected_end_stack = vec![
            get_init_tvm_stack(),
            vec![BFieldElement::new(0), BFieldElement::new(0)],
        ]
        .concat();
        prop_sub(
            U32s::try_from(u64::MAX).unwrap(),
            U32s::try_from(u64::MAX).unwrap(),
            Some(&expected_end_stack),
        );
    }

    #[test]
    fn u32s_2_sub_no_overflow() {
        // 256 - 129 = 127
        let expected_end_stack = vec![
            get_init_tvm_stack(),
            vec![BFieldElement::zero(), BFieldElement::new(127)],
        ]
        .concat();
        prop_sub(U32s::from(256), U32s::from(129), Some(&expected_end_stack));
    }

    #[test]
    fn u32s_2_sub_carry() {
        // 2^32 - 1 = ...
        let expected_end_stack = vec![
            get_init_tvm_stack(),
            vec![BFieldElement::zero(), BFieldElement::new(u32::MAX as u64)],
        ]
        .concat();
        prop_sub(
            U32s::from(BigUint::from(1u64 << 32)),
            U32s::from(1),
            Some(&expected_end_stack),
        );
    }

    #[test]
    fn u32s_2_sub_pbt() {
        let mut rng = rand::thread_rng();
        for _ in 0..100 {
            let lhs: u64 = rng.gen();
            let rhs: u64 = rng.gen_range(0..=lhs);

            prop_sub(
                U32s::from(BigUint::from(lhs)),
                U32s::from(BigUint::from(rhs)),
                None,
            );
        }
    }

    #[should_panic]
    #[test]
    fn overflow_test() {
        let lhs: U32s<2> = U32s::from(BigUint::from(1u64 << 33));
        let rhs: U32s<2> = U32s::from(BigUint::from((1u64 << 33) + 1));
        let mut init_stack = get_init_tvm_stack();
        for elem in rhs.encode().into_iter().rev() {
            init_stack.push(elem);
        }
        for elem in lhs.encode().into_iter().rev() {
            init_stack.push(elem);
        }

        SubU64.link_and_run_tasm_from_state_for_test(&mut ExecutionState::with_stack(init_stack));
    }

    fn prop_sub(lhs: U32s<2>, rhs: U32s<2>, expected: Option<&[BFieldElement]>) {
        let mut init_stack = get_init_tvm_stack();
        for elem in rhs.encode().into_iter().rev() {
            init_stack.push(elem);
        }
        for elem in lhs.encode().into_iter().rev() {
            init_stack.push(elem);
        }

        test_rust_equivalence_given_input_values_deprecated(
            &SubU64,
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
    fn sub_u64_benchmark() {
        bench_and_write(SubU64);
    }
}
