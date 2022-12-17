use std::collections::HashMap;

use num::{One, Zero};
use twenty_first::amount::u32s::U32s;
use twenty_first::shared_math::b_field_element::BFieldElement;

use crate::library::Library;
use crate::snippet_trait::Snippet;

pub struct U32s2Eq;

impl Snippet for U32s2Eq {
    fn stack_diff() -> isize {
        -3
    }

    fn entrypoint() -> &'static str {
        "u32_2_eq"
    }

    fn function_body(_library: &mut Library) -> String {
        let entrypoint = Self::entrypoint();
        format!(
            "
            // Before: _ hi_r lo_r hi_l lo_l
            // After: _ (r == l)
            {entrypoint}:
                swap3
                eq
                swap2
                eq
                add

                // The sum of the two equality operations must be 2
                push 2
                eq
                return
            "
        )
    }

    fn rust_shadowing(
        stack: &mut Vec<BFieldElement>,
        _std_in: Vec<BFieldElement>,
        _secret_in: Vec<BFieldElement>,
        _init_memory: HashMap<BFieldElement, BFieldElement>,
    ) where
        Self: Sized,
    {
        let a_lo: u32 = stack.pop().unwrap().try_into().unwrap();
        let a_hi: u32 = stack.pop().unwrap().try_into().unwrap();
        let a = U32s::<2>::new([a_lo, a_hi]);

        let b_lo: u32 = stack.pop().unwrap().try_into().unwrap();
        let b_hi: u32 = stack.pop().unwrap().try_into().unwrap();
        let b = U32s::<2>::new([b_lo, b_hi]);

        stack.push(if a == b {
            BFieldElement::one()
        } else {
            BFieldElement::zero()
        })
    }
}

#[cfg(test)]
mod tests {
    use rand::RngCore;
    use twenty_first::shared_math::b_field_element::BFieldElement;
    use twenty_first::util_types::algebraic_hasher::Hashable;

    use crate::{get_init_tvm_stack, snippet_trait::rust_tasm_equivalence_prop};

    use super::*;

    #[test]
    fn u32s_2_eq_false() {
        // Should return false
        let expected_end_stack = vec![get_init_tvm_stack(), vec![BFieldElement::zero()]].concat();
        prop_eq(
            U32s::new([239, 16]),
            U32s::new([239, 17]),
            Some(&expected_end_stack),
        );
        prop_eq(
            U32s::new([239, 16]),
            U32s::new([128, 16]),
            Some(&expected_end_stack),
        );
        prop_eq(
            U32s::new([239, 16]),
            U32s::new([128, 17]),
            Some(&expected_end_stack),
        );
        prop_eq(
            U32s::new([0, 16]),
            U32s::new([0, 17]),
            Some(&expected_end_stack),
        );
        prop_eq(
            U32s::new([239, 0]),
            U32s::new([128, 0]),
            Some(&expected_end_stack),
        );
        prop_eq(
            U32s::new([0, 1]),
            U32s::new([0, 0]),
            Some(&expected_end_stack),
        );
        prop_eq(
            U32s::new([0, 0]),
            U32s::new([0, 1]),
            Some(&expected_end_stack),
        );
        prop_eq(
            U32s::new([0, 0]),
            U32s::new([0, 1]),
            Some(&expected_end_stack),
        );
        prop_eq(
            U32s::new([1, 0]),
            U32s::new([0, 0]),
            Some(&expected_end_stack),
        );
        prop_eq(
            U32s::new([0, 0]),
            U32s::new([1, 0]),
            Some(&expected_end_stack),
        );
    }

    #[test]
    fn u32s_2_eq_true() {
        // Should return true
        let expected_end_stack = vec![get_init_tvm_stack(), vec![BFieldElement::one()]].concat();
        prop_eq(
            U32s::new([239, 17]),
            U32s::new([239, 17]),
            Some(&expected_end_stack),
        );
        prop_eq(
            U32s::new([u32::MAX, 16]),
            U32s::new([u32::MAX, 16]),
            Some(&expected_end_stack),
        );
        prop_eq(
            U32s::new([239, u32::MAX]),
            U32s::new([239, u32::MAX]),
            Some(&expected_end_stack),
        );
        prop_eq(
            U32s::new([u32::MAX, u32::MAX]),
            U32s::new([u32::MAX, u32::MAX]),
            Some(&expected_end_stack),
        );
        prop_eq(
            U32s::new([0, 0]),
            U32s::new([0, 0]),
            Some(&expected_end_stack),
        );
        prop_eq(
            U32s::new([1, 0]),
            U32s::new([1, 0]),
            Some(&expected_end_stack),
        );
        prop_eq(
            U32s::new([0, 1]),
            U32s::new([0, 1]),
            Some(&expected_end_stack),
        );
        prop_eq(
            U32s::new([1, 1]),
            U32s::new([1, 1]),
            Some(&expected_end_stack),
        );
    }

    #[test]
    fn u32s_2_eq_pbt_true() {
        let expected_end_stack = vec![get_init_tvm_stack(), vec![BFieldElement::one()]].concat();
        let mut rng = rand::thread_rng();
        for _ in 0..10 {
            let lhs = U32s::new([rng.next_u32(), rng.next_u32()]);
            let rhs = lhs;
            prop_eq(lhs, rhs, Some(&expected_end_stack));
        }
    }

    fn prop_eq(lhs: U32s<2>, rhs: U32s<2>, expected: Option<&[BFieldElement]>) {
        let mut init_stack = get_init_tvm_stack();
        for elem in rhs.to_sequence().into_iter().rev() {
            init_stack.push(elem);
        }
        for elem in lhs.to_sequence().into_iter().rev() {
            init_stack.push(elem);
        }

        let _execution_result = rust_tasm_equivalence_prop::<U32s2Eq>(
            &init_stack,
            &[],
            &[],
            HashMap::default(),
            expected,
        );
    }
}
