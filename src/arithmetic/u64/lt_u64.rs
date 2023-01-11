use std::collections::HashMap;

use num::{One, Zero};
use twenty_first::amount::u32s::U32s;
use twenty_first::shared_math::b_field_element::BFieldElement;

use crate::library::Library;
use crate::snippet_trait::Snippet;

pub struct LtU64();

pub struct LtStandardU64();

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
    use num::BigUint;
    use rand::Rng;
    use twenty_first::{
        shared_math::b_field_element::BFieldElement, util_types::algebraic_hasher::Hashable,
    };

    use crate::{get_init_tvm_stack, snippet_trait::rust_tasm_equivalence_prop};

    use super::*;

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
        prop_lt(
            U32s::from(BigUint::from(11 * (1u64 << 32))),
            U32s::from(BigUint::from(15 * (1u64 << 32))),
            Some(&expected_end_stack),
        );
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
        prop_lt(U32s::from(0), U32s::from(0), Some(&expected_end_stack));
    }

    #[test]
    fn u32s_lt_pbt() {
        let mut rng = rand::thread_rng();
        for _ in 0..100 {
            let lhs: u64 = rng.gen();
            let rhs: u64 = rng.gen();

            prop_lt(
                U32s::from(BigUint::from(lhs)),
                U32s::from(BigUint::from(rhs)),
                None,
            );
        }
    }

    #[test]
    fn u32s_lt_true_pbt() {
        let mut rng = rand::thread_rng();
        let init_stack = get_init_tvm_stack();
        for _ in 0..100 {
            let rhs: u64 = rng.gen();
            let lhs: u64 = rng.gen_range(0..rhs);
            let lhs: U32s<2> = U32s::from(BigUint::from(lhs));
            let rhs: U32s<2> = U32s::from(BigUint::from(rhs));
            let expected = vec![
                init_stack.clone(),
                rhs.to_sequence().into_iter().rev().collect(),
                lhs.to_sequence().into_iter().rev().collect(),
                vec![BFieldElement::one()],
            ]
            .concat();

            prop_lt(
                U32s::from(BigUint::from(lhs)),
                U32s::from(BigUint::from(rhs)),
                Some(&expected),
            );
        }
    }

    #[test]
    fn u32s_lt_false_pbt() {
        let mut rng = rand::thread_rng();
        let init_stack = get_init_tvm_stack();
        for _ in 0..100 {
            let lhs: u64 = rng.gen();
            let rhs: u64 = rng.gen_range(0..=lhs);
            let lhs: U32s<2> = U32s::from(BigUint::from(lhs));
            let rhs: U32s<2> = U32s::from(BigUint::from(rhs));
            let expected = vec![
                init_stack.clone(),
                rhs.to_sequence().into_iter().rev().collect(),
                lhs.to_sequence().into_iter().rev().collect(),
                vec![BFieldElement::zero()],
            ]
            .concat();

            prop_lt(
                U32s::from(BigUint::from(lhs)),
                U32s::from(BigUint::from(rhs)),
                Some(&expected),
            );
        }
    }

    #[test]
    fn u32s_lt_false_because_equal_pbt() {
        let mut rng = rand::thread_rng();
        let init_stack = get_init_tvm_stack();
        for _ in 0..100 {
            let lhs: u64 = rng.gen();
            let rhs: u64 = lhs;
            let lhs: U32s<2> = U32s::from(BigUint::from(lhs));
            let rhs: U32s<2> = U32s::from(BigUint::from(rhs));
            let expected = vec![
                init_stack.clone(),
                rhs.to_sequence().into_iter().rev().collect(),
                lhs.to_sequence().into_iter().rev().collect(),
                vec![BFieldElement::zero()],
            ]
            .concat();

            prop_lt(
                U32s::from(BigUint::from(lhs)),
                U32s::from(BigUint::from(rhs)),
                Some(&expected),
            );
        }
    }

    fn prop_lt(lhs: U32s<2>, rhs: U32s<2>, expected: Option<&[BFieldElement]>) {
        let mut init_stack = get_init_tvm_stack();
        for elem in rhs.to_sequence().into_iter().rev() {
            init_stack.push(elem);
        }
        for elem in lhs.to_sequence().into_iter().rev() {
            init_stack.push(elem);
        }

        let _execution_result = rust_tasm_equivalence_prop::<LtU64>(
            &init_stack,
            &[],
            &[],
            &mut HashMap::default(),
            0,
            expected,
        );
    }
}
