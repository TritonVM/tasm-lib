use num::{One, Zero};
use twenty_first::{amount::u32s::U32s, shared_math::b_field_element::BFieldElement};

use crate::snippet_trait::Snippet;

pub struct U32s2Lt;

const SNIPPET_NAME: &str = "u32_2_lt";

impl Snippet for U32s2Lt {
    const STACK_DIFF: isize = 1;

    const NAME: &'static str = SNIPPET_NAME;

    /// Before: _ rhs_hi rhs_lo lhs_hi lhs_lo
    /// After: _ rhs_hi rhs_lo lhs_hi lhs_lo  (lhs < rhs)
    fn get_function() -> String {
        format!(
            "
            {SNIPPET_NAME}:
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
                    call {SNIPPET_NAME}_lo // => _ rhs_hi rhs_lo lhs_hi lhs_lo 0

                // _ rhs_hi rhs_lo lhs_hi lhs_lo (lhs_lo < rhs_lo)|0
                return

            // Before: _ rhs_hi rhs_lo lhs_hi lhs_lo 0
            // After: _ rhs_hi rhs_lo lhs_hi lhs_lo (lhs_lo < rhs_lo)
            {SNIPPET_NAME}_lo:
                pop   // _ rhs_hi rhs_lo lhs_hi lhs_lo
                dup2  // _ rhs_hi rhs_lo lhs_hi lhs_lo rhs_lo
                dup1  // _ rhs_hi rhs_lo lhs_hi lhs_lo rhs_lo lhs_lo
                lt    // _ rhs_hi rhs_lo lhs_hi lhs_lo (lhs_lo < rhs_lo)
                return
            "
        )
    }

    fn rust_shadowing(
        stack: &mut Vec<twenty_first::shared_math::b_field_element::BFieldElement>,
        _std_in: Vec<twenty_first::shared_math::b_field_element::BFieldElement>,
        _secret_in: Vec<twenty_first::shared_math::b_field_element::BFieldElement>,
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

#[cfg(test)]
mod tests {
    use num::BigUint;
    use rand::Rng;
    use twenty_first::{
        shared_math::b_field_element::BFieldElement, util_types::algebraic_hasher::Hashable,
    };

    use crate::get_init_tvm_stack;

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

        let mut tasm_stack = init_stack.clone();
        let execution_result = U32s2Lt::run_tasm(&mut tasm_stack, vec![], vec![]);
        println!(
            "Cycle count for `{SNIPPET_NAME}`: {}",
            execution_result.cycle_count
        );
        println!(
            "Hash table height for `{SNIPPET_NAME}`: {}",
            execution_result.hash_table_height
        );

        let mut rust_stack = init_stack;
        U32s2Lt::rust_shadowing(&mut rust_stack, vec![], vec![]);

        assert_eq!(
            tasm_stack, rust_stack,
            "Rust code must match TVM for `{SNIPPET_NAME}`"
        );
        if let Some(expected) = expected {
            assert_eq!(
                tasm_stack, expected,
                "TVM must produce expected stack. lhs: {lhs}, rhs: {rhs}"
            );
        }
    }
}
