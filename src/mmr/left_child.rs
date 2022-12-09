use num::BigUint;
use twenty_first::{
    amount::u32s::U32s,
    util_types::{algebraic_hasher::Hashable, mmr},
};

use crate::{
    arithmetic::u32s_2::{powers_of_two::U322PowersOfTwoStatic, sub::U32s2Sub},
    snippet_trait::Snippet,
};

pub struct MmrLeftChild;

const SNIPPET_NAME: &str = "mmr_left_child";

impl Snippet for MmrLeftChild {
    const STACK_DIFF: isize = -1;

    const NAME: &'static str = SNIPPET_NAME;

    fn get_function() -> String {
        let powers_of_two = U322PowersOfTwoStatic::NAME;
        let powers_of_two_function = U322PowersOfTwoStatic::get_function();
        let u32s_2_sub = U32s2Sub::NAME;
        let u32s_2_sub_function = U32s2Sub::get_function();
        format!(
            "
            // Before: _ ni_hi ni_lo height
            // After: _ left_child_hi left_child_lo
            {SNIPPET_NAME}:
                call {powers_of_two} // -> _ ni_hi ni_lo (2^height)_hi (2^height)_lo
                swap2
                swap1
                swap3
                swap1                // -> _ (2^height)_hi (2^height)_lo ni_hi ni_lo
                call {u32s_2_sub}    // -> _ left_child_hi left_child_lo
                return

            {powers_of_two_function}
            {u32s_2_sub_function}
            "
        )
    }

    fn rust_shadowing(
        stack: &mut Vec<twenty_first::shared_math::b_field_element::BFieldElement>,
        _std_in: Vec<twenty_first::shared_math::b_field_element::BFieldElement>,
        _secret_in: Vec<twenty_first::shared_math::b_field_element::BFieldElement>,
    ) {
        let height: u32 = stack.pop().unwrap().try_into().unwrap();
        let node_index_lo: u32 = stack.pop().unwrap().try_into().unwrap();
        let node_index_hi: u32 = stack.pop().unwrap().try_into().unwrap();
        let node_index: u64 = (node_index_hi as u64) * (1u64 << 32) + node_index_lo as u64;
        let ret: u64 = mmr::shared::left_child(node_index as u128, height as u128) as u64;
        let ret: U32s<2> = U32s::from(BigUint::from(ret));

        stack.append(&mut ret.to_sequence().into_iter().rev().collect());
    }
}

#[cfg(test)]
mod tests {
    use num::Zero;
    use twenty_first::{
        shared_math::b_field_element::BFieldElement, util_types::algebraic_hasher::Hashable,
    };

    use crate::get_init_tvm_stack;

    use super::*;

    #[test]
    fn u32s_left_child_simple() {
        let mut expected_stack = get_init_tvm_stack();
        expected_stack.push(BFieldElement::zero());
        expected_stack.push(BFieldElement::new(3));
        prop_left_child(U32s::<2>::from(7), 2, Some(&expected_stack));

        expected_stack = get_init_tvm_stack();
        expected_stack.push(BFieldElement::zero());
        expected_stack.push(BFieldElement::new(10));
        prop_left_child(U32s::<2>::from(14), 2, Some(&expected_stack));

        expected_stack = get_init_tvm_stack();
        expected_stack.push(BFieldElement::zero());
        expected_stack.push(BFieldElement::new(4));
        prop_left_child(U32s::<2>::from(6), 1, Some(&expected_stack));

        expected_stack = get_init_tvm_stack();
        expected_stack.push(BFieldElement::zero());
        expected_stack.push(BFieldElement::new(16));
        prop_left_child(U32s::<2>::from(18), 1, Some(&expected_stack));
    }

    fn prop_left_child(node_index: U32s<2>, height: u32, expected: Option<&[BFieldElement]>) {
        let mut init_stack = get_init_tvm_stack();
        for elem in node_index.to_sequence().into_iter().rev() {
            init_stack.push(elem);
        }
        init_stack.push(BFieldElement::new(height as u64));

        let mut tasm_stack = init_stack.clone();
        let execution_result = MmrLeftChild::run_tasm(&mut tasm_stack, vec![], vec![]);
        println!(
            "Cycle count for `{SNIPPET_NAME}`: {}",
            execution_result.cycle_count
        );
        println!(
            "Hash table height for `{SNIPPET_NAME}`: {}",
            execution_result.hash_table_height
        );

        let mut rust_stack = init_stack;
        MmrLeftChild::rust_shadowing(&mut rust_stack, vec![], vec![]);

        assert_eq!(
            tasm_stack, rust_stack,
            "Rust code must match TVM for `{SNIPPET_NAME}`"
        );
        if let Some(expected) = expected {
            assert_eq!(
                tasm_stack, expected,
                "TVM must produce expected stack. node_index: {node_index}, height: {height}"
            );
        }
    }
}
