use num::BigUint;
use twenty_first::{
    amount::u32s::U32s,
    shared_math::b_field_element::BFieldElement,
    util_types::{algebraic_hasher::Hashable, mmr},
};

use crate::{
    arithmetic::u32s_2::{add::U32s2Add, decr::U32s2Decr, lt::U32s2Lt},
    snippet_trait::Snippet,
};

pub struct MmrLeftmostAncestor;

const SNIPPET_NAME: &str = "mmr_leftmost_ancestor";

impl Snippet for MmrLeftmostAncestor {
    const STACK_DIFF: isize = 1;

    const NAME: &'static str = SNIPPET_NAME;

    fn get_function() -> String {
        let u32s_2_add = U32s2Add::NAME;
        let u32s_2_add_function = U32s2Add::get_function();
        let u32s_2_decr = U32s2Decr::NAME;
        let u32s_2_decr_function = U32s2Decr::get_function();
        let u32s_2_lt = U32s2Lt::NAME;
        let u32s_2_lt_function = U32s2Lt::get_function();
        format!(
            "
            // Before: _ node_index_hi node_index_lo
            // After: _ leftmost_ancestor_hi leftmost_ancestor_lo height
            {SNIPPET_NAME}:
                push 0        // -> _ node_index_hi node_index_lo h
                push 0 push 2 // -> _ node_index_hi node_index_lo h ret_hi ret_lo
                call {SNIPPET_NAME}_while // -> _ node_index_hi node_index_lo (h + 1) ret_hi ret_lo node_index_hi node_index_lo (ret - 1)_hi (ret - 1)_lo

                pop pop pop pop    // -> _ node_index_hi node_index_lo h ret_hi ret_lo
                call {u32s_2_decr} // -> _ node_index_hi node_index_lo h (ret - 1)_hi (ret - 1)_lo

                swap3              // -> _ node_index_hi (ret - 1)_lo h (ret - 1)_hi node_index_lo
                pop                // -> _ node_index_hi (ret - 1)_lo h (ret - 1)_hi
                swap3              // -> _ (ret - 1)_hi (ret - 1)_lo h node_index_hi
                pop                // -> _ (ret - 1)_hi (ret - 1)_lo h

                return

            // Before: _ node_index_hi node_index_lo h ret_hi ret_lo
            // After: _ node_index_hi node_index_lo h ret_hi ret_lo node_index_hi node_index_lo (ret - 1)_hi (ret - 1)_lo
            {SNIPPET_NAME}_while:
                dup4 dup4 // -> _ node_index_hi node_index_lo h ret_hi ret_lo node_index_hi node_index_lo
                dup3 dup3 // -> _ node_index_hi node_index_lo h ret_hi ret_lo node_index_hi node_index_lo ret_hi ret_lo
                call {u32s_2_decr} // -> _ node_index_hi node_index_lo h ret_hi ret_lo node_index_hi node_index_lo (ret - 1)_hi (ret - 1)_lo
                call {u32s_2_lt}   // -> _ node_index_hi node_index_lo h ret_hi ret_lo node_index_hi node_index_lo (ret - 1)_hi (ret - 1)_lo (ret - 1 < node_index)
                push 0 eq          // -> _ node_index_hi node_index_lo h ret_hi ret_lo node_index_hi node_index_lo (ret - 1)_hi (ret - 1)_lo (ret - 1 >= node_index)
                skiz return        // if (ret - 1 >= node_index) return

                                       // -> _ node_index_hi node_index_lo h ret_hi ret_lo node_index_hi node_index_lo (ret - 1)_hi (ret - 1)_lo
                pop pop pop pop        // -> _ node_index_hi node_index_lo h ret_hi ret_lo
                swap2 push 1 add swap2 // -> _ node_index_hi node_index_lo (h + 1) ret_hi ret_lo
                dup1 dup1              // -> _ node_index_hi node_index_lo (h + 1) ret_hi ret_lo ret_hi ret_lo
                call {u32s_2_add}      // -> _ node_index_hi node_index_lo (h + 1) (2 * ret)_hi (2 * ret)_lo
                recurse

            {u32s_2_add_function}
            {u32s_2_decr_function}
            {u32s_2_lt_function}
            "
        )
    }

    fn rust_shadowing(
        stack: &mut Vec<twenty_first::shared_math::b_field_element::BFieldElement>,
        _std_in: Vec<twenty_first::shared_math::b_field_element::BFieldElement>,
        _secret_in: Vec<twenty_first::shared_math::b_field_element::BFieldElement>,
    ) {
        let node_index_lo: u32 = stack.pop().unwrap().try_into().unwrap();
        let node_index_hi: u32 = stack.pop().unwrap().try_into().unwrap();
        let node_index: u64 = (node_index_hi as u64) * (1u64 << 32) + node_index_lo as u64;

        let (ret, h): (u128, u128) = mmr::shared::leftmost_ancestor(node_index as u128);
        let ret: U32s<2> = U32s::from(BigUint::from(ret));
        let h: u32 = h as u32;

        stack.append(&mut ret.to_sequence().into_iter().rev().collect());
        stack.push(BFieldElement::from(h));
    }
}

#[cfg(test)]
mod tests {
    use twenty_first::{
        shared_math::b_field_element::BFieldElement, util_types::algebraic_hasher::Hashable,
    };

    use crate::get_init_tvm_stack;

    use super::*;

    #[test]
    fn u32s_leftmost_ancestor_simple() {
        // leftmost_ancestor(1) -> height = 0, index = 1
        let mut expected_stack = get_init_tvm_stack();
        expected_stack.push(BFieldElement::new(0));
        expected_stack.push(BFieldElement::new(1));
        expected_stack.push(BFieldElement::new(0));
        prop_leftmost_ancestor(U32s::<2>::from(1), Some(&expected_stack));

        // leftmost_ancestor(2) -> height = 1, index = 3
        let mut expected_stack = get_init_tvm_stack();
        expected_stack.push(BFieldElement::new(0));
        expected_stack.push(BFieldElement::new(3));
        expected_stack.push(BFieldElement::new(1));
        prop_leftmost_ancestor(U32s::<2>::from(2), Some(&expected_stack));
        prop_leftmost_ancestor(U32s::<2>::from(3), Some(&expected_stack));

        let mut expected_stack = get_init_tvm_stack();
        expected_stack.push(BFieldElement::new(0));
        expected_stack.push(BFieldElement::new(7));
        expected_stack.push(BFieldElement::new(2));
        prop_leftmost_ancestor(U32s::<2>::from(4), Some(&expected_stack));
        prop_leftmost_ancestor(U32s::<2>::from(5), Some(&expected_stack));
        prop_leftmost_ancestor(U32s::<2>::from(6), Some(&expected_stack));
        prop_leftmost_ancestor(U32s::<2>::from(7), Some(&expected_stack));

        let mut expected_stack = get_init_tvm_stack();
        expected_stack.push(BFieldElement::new(0));
        expected_stack.push(BFieldElement::new(15));
        expected_stack.push(BFieldElement::new(3));
        for i in 8..=15 {
            prop_leftmost_ancestor(U32s::<2>::from(i), Some(&expected_stack));
        }

        let mut expected_stack = get_init_tvm_stack();
        expected_stack.push(BFieldElement::new(0));
        expected_stack.push(BFieldElement::new(31));
        expected_stack.push(BFieldElement::new(4));
        prop_leftmost_ancestor(U32s::<2>::from(16), Some(&expected_stack));

        let mut expected_stack = get_init_tvm_stack();
        expected_stack.push(BFieldElement::new(1));
        expected_stack.push(BFieldElement::from(u32::MAX));
        expected_stack.push(BFieldElement::new(32));
        prop_leftmost_ancestor(
            U32s::<2>::from(BigUint::from(1u64 << 32)),
            Some(&expected_stack),
        );

        let mut expected_stack = get_init_tvm_stack();
        expected_stack.push(BFieldElement::new(3));
        expected_stack.push(BFieldElement::from(u32::MAX));
        expected_stack.push(BFieldElement::new(33));
        prop_leftmost_ancestor(
            U32s::<2>::from(BigUint::from(1u64 << 33)),
            Some(&expected_stack),
        );

        let mut expected_stack = get_init_tvm_stack();
        expected_stack.push(BFieldElement::new((1u64 << 31) - 1));
        expected_stack.push(BFieldElement::from(u32::MAX));
        expected_stack.push(BFieldElement::new(62));
        prop_leftmost_ancestor(
            U32s::<2>::from(BigUint::from(1u64 << 62)),
            Some(&expected_stack),
        );

        // This test fails but maybe it should succeed?
        // let mut expected_stack = get_init_tvm_stack();
        // expected_stack.push(BFieldElement::new((1u64 << 32) - 1));
        // expected_stack.push(BFieldElement::from(u32::MAX));
        // expected_stack.push(BFieldElement::new(63));
        // prop_leftmost_ancestor(
        //     U32s::<2>::from(BigUint::from(1u64 << 63)),
        //     Some(&expected_stack),
        // );
    }

    fn prop_leftmost_ancestor(node_index: U32s<2>, expected: Option<&[BFieldElement]>) {
        let mut init_stack = get_init_tvm_stack();
        for elem in node_index.to_sequence().into_iter().rev() {
            init_stack.push(elem);
        }

        let mut tasm_stack = init_stack.clone();
        let execution_result = MmrLeftmostAncestor::run_tasm(&mut tasm_stack, vec![], vec![]);
        println!(
            "Cycle count for `{SNIPPET_NAME}`: {}",
            execution_result.cycle_count
        );
        println!(
            "Hash table height for `{SNIPPET_NAME}`: {}",
            execution_result.hash_table_height
        );

        let mut rust_stack = init_stack;
        MmrLeftmostAncestor::rust_shadowing(&mut rust_stack, vec![], vec![]);

        assert_eq!(
            tasm_stack, rust_stack,
            "Rust code must match TVM for `{SNIPPET_NAME}`"
        );
        if let Some(expected) = expected {
            assert_eq!(
                tasm_stack, expected,
                "TVM must produce expected stack. node_index: {node_index}"
            );
        }
    }
}
