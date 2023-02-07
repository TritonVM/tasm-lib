use rand::Rng;
use std::collections::HashMap;

use twenty_first::amount::u32s::U32s;
use twenty_first::shared_math::b_field_element::BFieldElement;
use twenty_first::util_types::algebraic_hasher::Hashable;

use crate::arithmetic::u32::is_u32::IsU32;
use crate::library::Library;
use crate::snippet::{DataType, Snippet};
use crate::{get_init_tvm_stack, push_hashable, ExecutionState};

#[derive(Clone)]
pub struct SubU64;

impl Snippet for SubU64 {
    fn inputs(&self) -> Vec<String> {
        vec![
            "rhs_hi".to_string(),
            "rhs_lo".to_string(),
            "lhs_hi".to_string(),
            "lhs_lo".to_string(),
        ]
    }

    fn outputs(&self) -> Vec<String> {
        vec!["(lhs - rhs)_hi".to_string(), "(lhs - rhs)_lo".to_string()]
    }

    fn input_types(&self) -> Vec<crate::snippet::DataType> {
        vec![DataType::U64, DataType::U64]
    }

    fn output_types(&self) -> Vec<crate::snippet::DataType> {
        vec![DataType::U64]
    }

    fn crash_conditions() -> Vec<String> {
        vec!["if (lhs - rhs) overflows u64".to_string()]
    }

    fn gen_input_states(&self) -> Vec<ExecutionState> {
        let mut rng = rand::thread_rng();

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
        push_hashable(&mut stack_1, &smaller_b);
        push_hashable(&mut stack_1, &small_a);

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
        push_hashable(&mut stack_2, &smaller_carry_d);
        push_hashable(&mut stack_2, &large_c);

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
        push_hashable(&mut stack_3, &smaller_f);
        push_hashable(&mut stack_3, &large_e);

        vec![
            ExecutionState::with_stack(stack_1),
            ExecutionState::with_stack(stack_2),
            ExecutionState::with_stack(stack_3),
        ]
    }

    fn stack_diff(&self) -> isize {
        -2
    }

    fn entrypoint(&self) -> String {
        "sub_u64".to_string()
    }

    /// Four top elements of stack are assumed to be valid u32s. So to have
    /// a value that's less than 2^32.
    fn function_body(&self, library: &mut Library) -> String {
        let entrypoint = self.entrypoint();
        let is_u32 = library.import(Box::new(IsU32));
        const TWO_POW_32: &str = "4294967296";

        format!(
            "
            {entrypoint}_carry:
                push {TWO_POW_32}
                add
                swap2  // -> _ lo_diff hi_l hi_r
                push 1
                add    // -> _ lo_diff hi_l (hi_r + 1)
                swap2  // -> _ (hi_r + 1) hi_l lo_diff
                return

            // Before: _ hi_r lo_r hi_l lo_l
            // After: _ hi_diff lo_diff
            {entrypoint}:
                swap1  // -> _ hi_r lo_r lo_l hi_l
                swap2  // -> _ hi_r hi_l lo_l lo_r
                push -1
                mul
                add    // -> _ hi_r hi_l (lo_l - lo_r)

                dup0
                call {is_u32}
                push 0
                eq
                skiz
                    call {entrypoint}_carry

                swap2  // -> lo_diff hi_l hi_r
                push -1
                mul
                add    // -> lo_diff (hi_l - hi_r)
                dup0
                call {is_u32}
                assert
                swap1  // -> (hi_l - hi_r) lo_diff

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
        let mut res = ab0_minus_ab1.to_sequence();
        for _ in 0..res.len() {
            stack.push(res.pop().unwrap());
        }
    }
}

#[cfg(test)]
mod tests {
    use num::{BigUint, Zero};
    use rand::Rng;
    use twenty_first::shared_math::b_field_element::BFieldElement;

    use crate::get_init_tvm_stack;
    use crate::snippet_bencher::bench_and_write;
    use crate::test_helpers::{rust_tasm_equivalence_prop, rust_tasm_equivalence_prop_new};

    use super::*;

    #[test]
    fn sub_u64_test() {
        rust_tasm_equivalence_prop_new::<SubU64>(SubU64);
    }

    #[test]
    fn sub_u64_benchmark() {
        bench_and_write::<SubU64>(SubU64);
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
        for _ in 0..10 {
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
        for elem in rhs.to_sequence().into_iter().rev() {
            init_stack.push(elem);
        }
        for elem in lhs.to_sequence().into_iter().rev() {
            init_stack.push(elem);
        }

        SubU64.run_tasm(&mut ExecutionState::with_stack(init_stack));
    }

    fn prop_sub(lhs: U32s<2>, rhs: U32s<2>, expected: Option<&[BFieldElement]>) {
        let mut init_stack = get_init_tvm_stack();
        for elem in rhs.to_sequence().into_iter().rev() {
            init_stack.push(elem);
        }
        for elem in lhs.to_sequence().into_iter().rev() {
            init_stack.push(elem);
        }

        let _execution_result = rust_tasm_equivalence_prop::<SubU64>(
            SubU64,
            &init_stack,
            &[],
            &[],
            &mut HashMap::default(),
            0,
            expected,
        );
    }
}
