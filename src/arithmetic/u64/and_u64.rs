use std::collections::HashMap;

use twenty_first::amount::u32s::U32s;
use twenty_first::shared_math::b_field_element::BFieldElement;
use twenty_first::util_types::algebraic_hasher::Hashable;

use crate::library::Library;
use crate::snippet::Snippet;

pub struct AndU64();

impl Snippet for AndU64 {
    fn stack_diff() -> isize {
        -2
    }

    fn entrypoint() -> &'static str {
        "and_u64"
    }

    fn function_body(_library: &mut Library) -> String {
        let entrypoint = Self::entrypoint();
        format!(
            "
            // BEFORE: rhs_hi rhs_lo lhs_hi lhs_lo
            // AFTER: (rhs & lhs)_hi (rhs & lhs)_lo
            {entrypoint}:
                swap3
                and
                // stack: _ lhs_lo rhs_lo (lhs_hi & rhs_hi)

                swap2
                and
                // stack: _ (lhs_hi & rhs_hi) (rhs_lo & lhs_lo)

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
        let a_lo: u32 = stack.pop().unwrap().try_into().unwrap();
        let a_hi: u32 = stack.pop().unwrap().try_into().unwrap();

        // second element on stack
        let b_lo: u32 = stack.pop().unwrap().try_into().unwrap();
        let b_hi: u32 = stack.pop().unwrap().try_into().unwrap();

        // Perform calculation and write the result back to the stack
        let and_res = U32s::<2>::new([a_lo & b_lo, a_hi & b_hi]);
        let mut res = and_res.to_sequence();
        for _ in 0..res.len() {
            stack.push(res.pop().unwrap());
        }
    }
}

#[cfg(test)]
mod tests {
    use num::BigUint;
    use rand::{thread_rng, RngCore};

    use crate::get_init_tvm_stack;
    use crate::test_helpers::rust_tasm_equivalence_prop;

    use super::*;

    #[test]
    fn and_test_simple() {
        prop_and(4, 3);
        prop_and(4, 4);
        prop_and(u64::MAX, u64::MAX);
        prop_and(0, u64::MAX);
        prop_and(u64::MAX, 0);
    }

    #[test]
    fn and_test_pbt() {
        let mut rng = thread_rng();
        for _ in 0..100 {
            let lhs = rng.next_u64();
            let rhs = rng.next_u64();
            prop_and(lhs, rhs);
        }
    }

    fn prop_and(lhs: u64, rhs: u64) {
        println!("lhs = {lhs}; rhs = {rhs}");
        let mut init_stack = get_init_tvm_stack();

        let rhs_u32_2 = U32s::<2>::new([(rhs & u32::MAX as u64) as u32, (rhs >> 32) as u32]);
        for elem in rhs_u32_2.to_sequence().into_iter().rev() {
            init_stack.push(elem);
        }

        let lhs_u32_2 = U32s::<2>::new([(lhs & u32::MAX as u64) as u32, (lhs >> 32) as u32]);
        for elem in lhs_u32_2.to_sequence().into_iter().rev() {
            init_stack.push(elem);
        }

        let expected_res: BigUint = (lhs & rhs).into();
        println!("Expected: {expected_res}");
        let expected_u32_2: U32s<2> = expected_res.into();
        let mut expected_end_stack = get_init_tvm_stack();
        for elem in expected_u32_2.to_sequence().into_iter().rev() {
            expected_end_stack.push(elem);
        }

        let _execution_result = rust_tasm_equivalence_prop::<AndU64>(
            &init_stack,
            &[],
            &[],
            &mut HashMap::default(),
            0,
            Some(&expected_end_stack),
        );
    }
}
