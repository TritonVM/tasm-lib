use std::collections::HashMap;

use num::{One, Zero};
use rand::RngCore;
use twenty_first::amount::u32s::U32s;
use twenty_first::shared_math::b_field_element::BFieldElement;

use crate::library::Library;
use crate::snippet::{NewSnippet, Snippet};
use crate::{get_init_tvm_stack, push_hashable, ExecutionState};

pub struct LtU64();

pub struct LtStandardU64();

impl NewSnippet for LtU64 {
    fn inputs() -> Vec<&'static str> {
        vec!["rhs_hi", "rhs_lo", "lhs_hi", "lhs_lo"]
    }

    fn outputs() -> Vec<&'static str> {
        vec!["rhs_hi", "rhs_lo", "lhs_hi", "lhs_lo", "(lhs < rhs)"]
    }

    fn crash_conditions() -> Vec<&'static str> {
        vec!["if inputs are not u32"]
    }

    fn gen_input_states() -> Vec<crate::ExecutionState> {
        let mut ret: Vec<ExecutionState> = vec![];

        for _ in 0..30 {
            let n: u64 = rand::thread_rng().next_u64();
            let n: U32s<2> = n.try_into().unwrap();
            let m: u64 = rand::thread_rng().next_u64();
            let m: U32s<2> = m.try_into().unwrap();
            let mut input_stack = get_init_tvm_stack();

            push_hashable(&mut input_stack, &n);
            push_hashable(&mut input_stack, &m);

            ret.push(ExecutionState::with_stack(input_stack))
        }

        ret
    }
}

/// This `lt_u64` does not consume its arguments, which is the norm for tasm functions.
///
/// See `LtStandardU64` for a variant that does.
impl Snippet for LtU64 {
    fn stack_diff() -> isize {
        1
    }

    fn entrypoint() -> &'static str {
        "lt_u64"
    }

    /// Before: _ rhs_hi rhs_lo lhs_hi lhs_lo
    /// After: _ rhs_hi rhs_lo lhs_hi lhs_lo  (lhs < rhs)
    fn function_body(_library: &mut Library) -> String {
        let entrypoint = Self::entrypoint();
        format!(
            "
            // Before: _ rhs_hi rhs_lo lhs_hi lhs_lo
            // After: _ rhs_hi rhs_lo lhs_hi lhs_lo (lhs < rhs)
            {entrypoint}:
                dup3
                dup2
                lt   // => _ rhs_hi rhs_lo lhs_hi lhs_lo (lhs_hi < rhs_hi)
                dup0
                skiz
                    return // => _ rhs_hi rhs_lo lhs_hi lhs_lo 1

                // => _ rhs_hi rhs_lo lhs_hi lhs_lo 0
                dup4 // => _ rhs_hi rhs_lo lhs_hi lhs_lo 0 rhs_hi
                dup3 // => _ rhs_hi rhs_lo lhs_hi lhs_lo 0 rhs_hi lhs_hi
                eq   // => _ rhs_hi rhs_lo lhs_hi lhs_lo 0 (rhs_hi == lhs_hi)
                skiz
                    call {entrypoint}_lo // => _ rhs_hi rhs_lo lhs_hi lhs_lo 0

                // _ rhs_hi rhs_lo lhs_hi lhs_lo (lhs_lo < rhs_lo)|0
                return

            // Before: _ rhs_hi rhs_lo lhs_hi lhs_lo 0
            // After: _ rhs_hi rhs_lo lhs_hi lhs_lo (lhs_lo < rhs_lo)
            {entrypoint}_lo:
                pop   // _ rhs_hi rhs_lo lhs_hi lhs_lo
                dup2  // _ rhs_hi rhs_lo lhs_hi lhs_lo rhs_lo
                dup1  // _ rhs_hi rhs_lo lhs_hi lhs_lo rhs_lo lhs_lo
                lt    // _ rhs_hi rhs_lo lhs_hi lhs_lo (lhs_lo < rhs_lo)
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
        let a0: u32 = stack[stack.len() - 1].try_into().unwrap();
        let b0: u32 = stack[stack.len() - 2].try_into().unwrap();
        let lhs = U32s::<2>::new([a0, b0]);

        // second element on stack
        let a1: u32 = stack[stack.len() - 3].try_into().unwrap();
        let b1: u32 = stack[stack.len() - 4].try_into().unwrap();
        let rhs = U32s::<2>::new([a1, b1]);

        stack.push(if lhs < rhs {
            BFieldElement::one()
        } else {
            BFieldElement::zero()
        });
    }
}

/// This `lt_standard_u64` does consume its argument.
///
/// The fastest way we know is to calculate without consuming, and then pop the operands.
/// This is because there are three branches, so sharing cleanup unconditionally means
/// less branching (fewer cycles) and less local cleanup (smaller program).
impl NewSnippet for LtStandardU64 {
    fn inputs() -> Vec<&'static str> {
        vec!["rhs_hi", "rhs_lo", "lhs_hi", "lhs_lo"]
    }

    fn outputs() -> Vec<&'static str> {
        vec!["(lhs < rhs)"]
    }

    fn crash_conditions() -> Vec<&'static str> {
        vec!["Either input is not u32"]
    }

    fn gen_input_states() -> Vec<ExecutionState> {
        // The input states for the two u64::lt operators can be reused. But the
        // rust shadowin cannot.
        LtU64::gen_input_states()
    }
}

impl Snippet for LtStandardU64 {
    fn stack_diff() -> isize {
        -3
    }

    fn entrypoint() -> &'static str {
        "lt_standard_u64"
    }

    fn function_body(_library: &mut Library) -> String {
        let entrypoint = Self::entrypoint();
        format!(
            "
            // Before: _ rhs_hi rhs_lo lhs_hi lhs_lo
            // After: _ (lhs < rhs)
            {entrypoint}:
                call {entrypoint}_aux // _ rhs_hi rhs_lo lhs_hi lhs_lo (lhs < rhs)
                swap4 pop pop pop pop // _ (lhs < rhs)
                return

            // Before: _ rhs_hi rhs_lo lhs_hi lhs_lo
            // After: _ rhs_hi rhs_lo lhs_hi lhs_lo (lhs < rhs)
            {entrypoint}_aux:
                dup3 // _ rhs_hi rhs_lo lhs_hi lhs_lo rhs_hi
                dup2 // _ rhs_hi rhs_lo lhs_hi lhs_lo rhs_hi lhs_hi
                lt   // _ rhs_hi rhs_lo lhs_hi lhs_lo (lhs_hi < rhs_hi)
                dup0 // _ rhs_hi rhs_lo lhs_hi lhs_lo (lhs_hi < rhs_hi) (lhs_hi < rhs_hi)
                skiz return
                     // true: _ rhs_hi rhs_lo lhs_hi lhs_lo (lhs < rhs)
                     // false: _ rhs_hi rhs_lo lhs_hi lhs_lo 0

                dup4 // _ rhs_hi rhs_lo lhs_hi lhs_lo 0 rhs_hi
                dup3 // _ rhs_hi rhs_lo lhs_hi lhs_lo 0 rhs_hi lhs_hi
                eq   // _ rhs_hi rhs_lo lhs_hi lhs_lo 0 (lhs_hi == rhs_hi)
                skiz call {entrypoint}_lo
                     // true: _ rhs_hi rhs_lo lhs_hi lhs_lo (lhs < rhs, aka lhs_lo < rhs_lo)
                     // false: _ rhs_hi rhs_lo lhs_hi lhs_lo (lhs < rhs, aka 0)
                return

            // Before: _ rhs_hi rhs_lo lhs_hi lhs_lo 0
            // After: _ rhs_hi rhs_lo lhs_hi lhs_lo (lhs_lo < rhs_lo)
            {entrypoint}_lo:
                pop  // _ rhs_hi rhs_lo lhs_hi lhs_lo
                dup2 // _ rhs_hi rhs_lo lhs_hi lhs_lo rhs_lo
                dup1 // _ rhs_hi rhs_lo lhs_hi lhs_lo rhs_lo lhs_lo
                lt   // _ rhs_hi rhs_lo lhs_hi lhs_lo (lhs < rhs)
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
        let lhs_lo: u32 = stack.pop().unwrap().try_into().unwrap();
        let lhs_hi: u32 = stack.pop().unwrap().try_into().unwrap();
        let lhs = U32s::new([lhs_lo, lhs_hi]);

        let rhs_lo: u32 = stack.pop().unwrap().try_into().unwrap();
        let rhs_hi: u32 = stack.pop().unwrap().try_into().unwrap();
        let rhs = U32s::new([rhs_lo, rhs_hi]);

        stack.push(BFieldElement::new((lhs < rhs) as u64));
    }
}

#[cfg(test)]
mod tests {
    use rand::Rng;
    use twenty_first::shared_math::b_field_element::BFieldElement;
    use twenty_first::util_types::algebraic_hasher::Hashable;

    use crate::get_init_tvm_stack;
    use crate::snippet_bencher::bench_and_write;
    use crate::test_helpers::{rust_tasm_equivalence_prop, rust_tasm_equivalence_prop_new};

    use super::*;

    // FIXME: Use `rng.gen()` after this is released:
    // https://github.com/Neptune-Crypto/twenty-first/pull/80
    fn random_gen() -> U32s<2> {
        let mut rng = rand::thread_rng();
        U32s::new([rng.gen(), rng.gen()])
    }

    #[test]
    fn lt_u64_test_new_snippet() {
        rust_tasm_equivalence_prop_new::<LtU64>();
    }

    #[test]
    fn standard_lt_u64_test_new_snippet() {
        rust_tasm_equivalence_prop_new::<LtStandardU64>();
    }

    #[test]
    fn log_2_floor_u64_benchmark() {
        bench_and_write::<LtStandardU64>();
    }

    #[test]
    fn u32s_lt_true_with_hi() {
        // 15 * 2^32 > 11 * 2^32
        let expected_end_stack = vec![
            get_init_tvm_stack(),
            vec![BFieldElement::new(15), BFieldElement::new(0)],
            vec![BFieldElement::new(11), BFieldElement::new(0)],
            vec![BFieldElement::one()],
        ]
        .concat();

        let lhs = U32s::try_from(11 * (1u64 << 32)).unwrap();
        let rhs = U32s::try_from(15 * (1u64 << 32)).unwrap();
        prop_lt(lhs, rhs, Some(&expected_end_stack));
        prop_lt_standard(lhs, rhs);
    }

    #[test]
    fn u32s_lt_false_with_zero() {
        // eval(0 < 0) = false
        let expected_end_stack = vec![
            get_init_tvm_stack(),
            vec![BFieldElement::new(0), BFieldElement::new(0)],
            vec![BFieldElement::new(0), BFieldElement::new(0)],
            vec![BFieldElement::zero()],
        ]
        .concat();
        let zero = U32s::zero();
        prop_lt(zero, zero, Some(&expected_end_stack));
        prop_lt_standard(zero, zero);
    }

    #[test]
    fn u32s_lt_pbt() {
        for _ in 0..100 {
            let lhs: U32s<2> = random_gen();
            let rhs: U32s<2> = random_gen();
            prop_lt(lhs, rhs, None);
            prop_lt_standard(lhs, rhs);
        }
    }

    #[test]
    fn u32s_lt_true_pbt() {
        let mut rng = rand::thread_rng();
        let init_stack = get_init_tvm_stack();
        for _ in 0..100 {
            let rhs: u64 = rng.gen();
            let lhs: u64 = rng.gen_range(0..rhs);
            let rhs: U32s<2> = U32s::try_from(rhs).unwrap();
            let lhs: U32s<2> = U32s::try_from(lhs).unwrap();
            let expected = vec![
                init_stack.clone(),
                rhs.to_sequence().into_iter().rev().collect(),
                lhs.to_sequence().into_iter().rev().collect(),
                vec![BFieldElement::one()],
            ]
            .concat();

            prop_lt(lhs, rhs, Some(&expected));
            prop_lt_standard(lhs, rhs);
        }
    }

    #[test]
    fn u32s_lt_false_pbt() {
        let mut rng = rand::thread_rng();
        let init_stack = get_init_tvm_stack();
        for _ in 0..100 {
            let lhs: u64 = rng.gen();
            let rhs: u64 = rng.gen_range(0..=lhs);
            let rhs: U32s<2> = U32s::try_from(rhs).unwrap();
            let lhs: U32s<2> = U32s::try_from(lhs).unwrap();
            let expected = vec![
                init_stack.clone(),
                rhs.to_sequence().into_iter().rev().collect(),
                lhs.to_sequence().into_iter().rev().collect(),
                vec![BFieldElement::zero()],
            ]
            .concat();

            prop_lt(lhs, rhs, Some(&expected));
            prop_lt_standard(lhs, rhs);
        }
    }

    #[test]
    fn u32s_lt_false_because_equal_pbt() {
        let mut rng = rand::thread_rng();
        let init_stack = get_init_tvm_stack();
        for _ in 0..100 {
            let lhs: u64 = rng.gen();
            let rhs: u64 = lhs;
            let rhs: U32s<2> = U32s::try_from(rhs).unwrap();
            let lhs: U32s<2> = U32s::try_from(lhs).unwrap();
            let expected = vec![
                init_stack.clone(),
                rhs.to_sequence().into_iter().rev().collect(),
                lhs.to_sequence().into_iter().rev().collect(),
                vec![BFieldElement::zero()],
            ]
            .concat();

            prop_lt(lhs, rhs, Some(&expected));
            prop_lt_standard(lhs, rhs);
        }
    }

    fn prop_lt(lhs: U32s<2>, rhs: U32s<2>, expected: Option<&[BFieldElement]>) {
        let mut init_stack = get_init_tvm_stack();
        init_stack.append(&mut rhs.to_sequence().into_iter().rev().collect());
        init_stack.append(&mut lhs.to_sequence().into_iter().rev().collect());

        let stdin = &[];
        let secret_in = &[];
        let mut memory = HashMap::default();
        let words_allocated = 0;
        let _execution_result = rust_tasm_equivalence_prop::<LtU64>(
            &init_stack,
            stdin,
            secret_in,
            &mut memory,
            words_allocated,
            expected,
        );
    }

    fn prop_lt_standard(lhs: U32s<2>, rhs: U32s<2>) {
        let mut init_stack = get_init_tvm_stack();
        init_stack.append(&mut rhs.to_sequence().into_iter().rev().collect());
        init_stack.append(&mut lhs.to_sequence().into_iter().rev().collect());

        let stdin = &[];
        let secret_in = &[];
        let mut memory = HashMap::default();
        let words_allocated = 0;
        let expected = None;
        let _execution_result = rust_tasm_equivalence_prop::<LtU64>(
            &init_stack,
            stdin,
            secret_in,
            &mut memory,
            words_allocated,
            expected,
        );
    }
}
