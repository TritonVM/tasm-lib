use std::collections::HashMap;

use num::Zero;
use rand::Rng;
use twenty_first::amount::u32s::U32s;
use twenty_first::shared_math::b_field_element::BFieldElement;
use twenty_first::util_types::algebraic_hasher::Hashable;

use crate::arithmetic::u32::is_u32::IsU32;
use crate::library::Library;
use crate::snippet::{DataType, Snippet};
use crate::{get_init_tvm_stack, push_hashable, ExecutionState};

#[derive(Clone)]
pub struct AddU64;

impl Snippet for AddU64 {
    fn inputs() -> Vec<&'static str> {
        vec!["rhs_hi", "rhs_lo", "lhs_hi", "lhs_lo"]
    }

    fn outputs() -> Vec<&'static str> {
        vec!["(lhs + rhs)_hi", "(lhs + rhs)_lo"]
    }

    fn input_types(&self) -> Vec<crate::snippet::DataType> {
        vec![DataType::U64, DataType::U64]
    }

    fn output_types(&self) -> Vec<crate::snippet::DataType> {
        vec![DataType::U64]
    }

    fn crash_conditions() -> Vec<&'static str> {
        vec!["if (lhs + rhs) overflows u64"]
    }

    fn gen_input_states() -> Vec<ExecutionState> {
        let mut rng = rand::thread_rng();

        let zero = U32s::<2>::zero();
        let small_a = U32s::<2>::try_from(rng.gen::<u32>()).unwrap();
        let small_b = U32s::<2>::try_from(rng.gen::<u32>()).unwrap();
        let large_a = U32s::<2>::try_from(rng.gen::<u64>()).unwrap();
        // let large_b = U32s::<2>::try_from(rng.gen::<u64>()).unwrap();

        let mut states = vec![];

        // 0. one zero, one large
        states.push({
            let mut stack = get_init_tvm_stack();
            push_hashable(&mut stack, &zero);
            push_hashable(&mut stack, &large_a);
            ExecutionState::with_stack(stack)
        });

        // 1. two small
        states.push({
            let mut stack = get_init_tvm_stack();
            push_hashable(&mut stack, &small_a);
            push_hashable(&mut stack, &small_b);
            ExecutionState::with_stack(stack)
        });

        // FIXME: Disable benchmarks that may overflow, since the benchmark panics when the VM panics.
        // // 2. one small, one large
        // states.push({
        //     let mut stack = get_init_tvm_stack();
        //     push_hashable(&mut stack, &small_a);
        //     push_hashable(&mut stack, &large_a);
        //     ExecutionState::with_stack(stack)
        // });

        // states.push({
        //     let mut stack = get_init_tvm_stack();
        //     push_hashable(&mut stack, &large_a);
        //     push_hashable(&mut stack, &small_a);
        //     ExecutionState::with_stack(stack)
        // });

        // // 3. two large
        // states.push({
        //     let mut stack = get_init_tvm_stack();
        //     push_hashable(&mut stack, &large_a);
        //     push_hashable(&mut stack, &large_b);
        //     ExecutionState::with_stack(stack)
        // });

        states
    }

    fn stack_diff() -> isize {
        -2
    }

    fn entrypoint() -> &'static str {
        "add_u64"
    }

    /// Four top elements of stack are assumed to be valid u32s. So to have
    /// a value that's less than 2^32.
    fn function_body(library: &mut Library) -> String {
        let entrypoint = Self::entrypoint();
        let is_u32 = library.import::<IsU32>();
        const MINUS_2_POW_32: &str = "18446744065119617025";

        format!(
            "
            {entrypoint}:
                swap1
                swap2
                add
                dup0
                call {is_u32}
                push 0
                eq
                skiz
                    call {entrypoint}_carry
                swap2
                add
                dup0
                call {is_u32}
                assert
                swap1
                return

            {entrypoint}_carry:
                push {MINUS_2_POW_32}
                add
                swap2
                push 1
                add
                swap2
                return
            "
        )
    }

    fn rust_shadowing(
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
        let mut res = ab0_plus_ab1.to_sequence();
        for _ in 0..res.len() {
            stack.push(res.pop().unwrap());
        }
    }
}

#[cfg(test)]
mod tests {
    use num::{BigUint, One, Zero};
    use rand::RngCore;

    use crate::get_init_tvm_stack;
    use crate::snippet_bencher::bench_and_write;
    use crate::test_helpers::{rust_tasm_equivalence_prop, rust_tasm_equivalence_prop_new};

    use super::*;

    #[test]
    fn add_u64_test() {
        rust_tasm_equivalence_prop_new::<AddU64>(AddU64);
    }

    #[test]
    fn add_u64_benchmark() {
        bench_and_write::<AddU64>();
    }

    #[test]
    fn u32s_2_add_no_overflow() {
        // 127 + 129 = 256
        let mut expected_end_stack = vec![
            get_init_tvm_stack(),
            vec![BFieldElement::zero(), BFieldElement::new(256)],
        ]
        .concat();
        prop_add(
            U32s::new([127, 0]),
            U32s::new([129, 0]),
            Some(&expected_end_stack),
        );

        // 127 + 129 + 45 * 2^32 + 1000 * 2^32 = 256 + 1045*2^32
        expected_end_stack = vec![
            get_init_tvm_stack(),
            vec![BFieldElement::new(1045), BFieldElement::new(256)],
        ]
        .concat();
        prop_add(
            U32s::new([127, 45]),
            U32s::new([129, 1000]),
            Some(&expected_end_stack),
        );

        // (2^32 - 1) + 0 + 0 * 2^32 + 2004 * 2^32 = (2^32 - 1) + 2004*2^32
        expected_end_stack = vec![
            get_init_tvm_stack(),
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
        expected_end_stack = vec![
            get_init_tvm_stack(),
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
        let expected_end_stack = vec![
            get_init_tvm_stack(),
            vec![BFieldElement::one(), BFieldElement::zero()],
        ]
        .concat();
        prop_add(
            U32s::new([1 << 31, 0]),
            U32s::new([1 << 31, 0]),
            Some(&expected_end_stack),
        );

        // 2 ^ 32 + 2 ^ 32 - 1 - 2 = (1^32 - 3) + 1 * 2^32
        let expected_end_stack = vec![
            get_init_tvm_stack(),
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
        let mut init_stack = get_init_tvm_stack();
        for elem in rhs.to_sequence().into_iter().rev() {
            init_stack.push(elem);
        }
        for elem in lhs.to_sequence().into_iter().rev() {
            init_stack.push(elem);
        }

        AddU64.run_tasm(&mut ExecutionState::with_stack(init_stack));
    }

    #[should_panic]
    #[test]
    fn overflow_test_2() {
        let lhs: U32s<2> = U32s::from(BigUint::from(u64::MAX));
        let rhs: U32s<2> = U32s::from(BigUint::from(u64::MAX));
        let mut init_stack = get_init_tvm_stack();
        for elem in rhs.to_sequence().into_iter().rev() {
            init_stack.push(elem);
        }
        for elem in lhs.to_sequence().into_iter().rev() {
            init_stack.push(elem);
        }

        AddU64.run_tasm(&mut ExecutionState::with_stack(init_stack));
    }

    fn prop_add(lhs: U32s<2>, rhs: U32s<2>, expected: Option<&[BFieldElement]>) {
        let mut init_stack = get_init_tvm_stack();
        for elem in rhs.to_sequence().into_iter().rev() {
            init_stack.push(elem);
        }
        for elem in lhs.to_sequence().into_iter().rev() {
            init_stack.push(elem);
        }

        let _execution_result = rust_tasm_equivalence_prop::<AddU64>(
            AddU64,
            &init_stack,
            &[],
            &[],
            &mut HashMap::default(),
            0,
            expected,
        );
    }
}
