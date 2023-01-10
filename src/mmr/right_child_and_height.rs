use std::collections::HashMap;

use num::{One, Zero};
use twenty_first::shared_math::b_field_element::BFieldElement;
use twenty_first::util_types::mmr;

use crate::arithmetic::u64::eq_u64::EqU64;
use crate::arithmetic::u64::lt::U32s2Lt;
use crate::snippet_trait::Snippet;

use super::left_child::MmrLeftChild;
use super::leftmost_ancestor::MmrLeftMostAncestor;
use super::right_child::MmrRightChild;

pub struct MmrRightChildAndHeight;

impl Snippet for MmrRightChildAndHeight {
    fn stack_diff() -> isize
    where
        Self: Sized,
    {
        0
    }

    fn entrypoint() -> &'static str
    where
        Self: Sized,
    {
        "right_child_and_height"
    }

    fn function_body(library: &mut crate::library::Library) -> String
    where
        Self: Sized,
    {
        let entrypoint = Self::entrypoint();
        let eq_u64 = library.import::<EqU64>();
        let u32s_2_lt = library.import::<U32s2Lt>();
        let left_child = library.import::<MmrLeftChild>();
        let right_child = library.import::<MmrRightChild>();
        let leftmost_ancestor = library.import::<MmrLeftMostAncestor>();

        format!(
            "
            // Before: _ ni_hi ni_lo
            // After: _ is_right_child height
            {entrypoint}:
                // Get leftmost ancestor and its height on top of stack
                push 0 // is `is_r` onto stack
                dup2
                dup2
                call {leftmost_ancestor}
                // stack: _ ni_hi ni_lo is_r c_hi c_lo height

                swap2
                swap1
                // stack: _ ni_hi ni_lo is_r height c_hi c_lo
                call {entrypoint}_loop
                // Stack: ni_hi ni_lo is_r height c_hi c_lo
                pop
                pop
                swap2
                pop
                swap2
                pop

                // Stack: _ is_r height
                return

            // Stack start and end:
            // _ ni_hi ni_lo is_r height c_hi c_lo
            {entrypoint}_loop:
                dup5
                dup5
                dup3
                dup3
                call {eq_u64}
                // Stack: _ ni_hi ni_lo is_r height c_hi c_lo (c == ni)
                skiz return

                // Stack: ni_hi ni_lo is_r height c_hi c_lo
                dup1
                dup1
                dup4
                // Stack: ni_hi ni_lo is_r height c_hi c_lo c_hi c_lo height

                call {left_child}
                // Stack: ni_hi ni_lo is_r height c_hi c_lo lc_hi lc_lo

                dup7 dup7
                // Stack: ni_hi ni_lo is_r height c_hi c_lo lc_hi lc_lo ni_hi ni_lo
                swap2
                swap1
                swap3
                swap1
                // Stack: ni_hi ni_lo is_r height c_hi c_lo ni_hi ni_lo lc_hi lc_lo

                call {u32s_2_lt}


                // Stack: ni_hi ni_lo prev_is_r height c_hi c_lo ni_hi ni_lo lc_hi lc_lo is_r
                push 1
                dup1
                // Stack: _ ni_hi ni_lo prev_is_r height c_hi c_lo ni_hi ni_lo lc_hi lc_lo is_r 1 is_r

                skiz call {entrypoint}_branch_then
                skiz call {entrypoint}_branch_else
                // Stack: _ ni_hi ni_lo is_r height c_hi c_lo

                // Decrement height by one
                swap2
                push -1
                add
                swap2

                // Stack: _ ni_hi ni_lo is_r (height - 1) c_hi c_lo

                recurse

            {entrypoint}_branch_then:
                // purpose: Set candidate to right child
                // Stack: _ ni_hi ni_lo prev_is_r height c_hi c_lo ni_hi ni_lo lc_hi lc_lo is_r 1
                pop
                swap8
                pop
                // Stack: ni_hi ni_lo is_r height c_hi c_lo ni_hi ni_lo lc_hi lc_lo

                pop pop pop pop
                // Stack: ni_hi ni_lo is_r height c_hi c_lo

                call {right_child}
                // Stack: ni_hi ni_lo is_r height rc_hi rc_lo
                // Stack: ni_hi ni_lo is_r height c_hi c_lo (after rename)

                push 0
                // End stack: ni_hi ni_lo is_r height c_hi c_lo 0

                return

            {entrypoint}_branch_else:
                // purpose: Set candidate to left child
                // Stack: _ ni_hi ni_lo prev_is_r height c_hi c_lo ni_hi ni_lo lc_hi lc_lo is_r

                swap8
                pop
                // Stack: ni_hi ni_lo is_r height c_hi c_lo ni_hi ni_lo lc_hi lc_lo

                swap4 pop swap4 pop
                // Stack: ni_hi ni_lo is_r height lc_hi lc_lo ni_hi ni_lo

                pop pop
                // Stack: ni_hi ni_lo is_r height lc_hi lc_lo

                // Stack: ni_hi ni_lo is_r height lc_hi lc_lo
                // End stack: ni_hi ni_lo is_r height c_hi c_lo (after rename)
                return

            "
        )
    }

    fn rust_shadowing(
        stack: &mut Vec<twenty_first::shared_math::b_field_element::BFieldElement>,
        _std_in: Vec<twenty_first::shared_math::b_field_element::BFieldElement>,
        _secret_in: Vec<twenty_first::shared_math::b_field_element::BFieldElement>,
        _memory: &mut HashMap<BFieldElement, BFieldElement>,
    ) where
        Self: Sized,
    {
        let node_index_lo: u32 = stack.pop().unwrap().try_into().unwrap();
        let node_index_hi: u32 = stack.pop().unwrap().try_into().unwrap();
        let node_index: u64 = (node_index_hi as u64) * (1u64 << 32) + node_index_lo as u64;

        let (ret, height) = mmr::shared::right_child_and_height(node_index as u128);
        stack.push(if ret {
            BFieldElement::one()
        } else {
            BFieldElement::zero()
        });

        stack.push(BFieldElement::new(height as u64));
    }
}

#[cfg(test)]
mod tests {
    use twenty_first::{
        amount::u32s::U32s, shared_math::b_field_element::BFieldElement,
        util_types::algebraic_hasher::Hashable,
    };

    use crate::{get_init_tvm_stack, snippet_trait::rust_tasm_equivalence_prop};

    use super::*;

    #[test]
    fn right_child_and_height_node_index_equal_leftmost_ancestor() {
        // All should return (false, height) as leftmost ancestors are always left-children.
        let expected_end_stack = vec![get_init_tvm_stack(), vec![BFieldElement::zero()]].concat();
        prop_right_child_and_height(
            U32s::new([1, 0]),
            Some(&vec![expected_end_stack.clone(), vec![BFieldElement::zero()]].concat()),
        );
        prop_right_child_and_height(
            U32s::new([3, 0]),
            Some(&vec![expected_end_stack.clone(), vec![BFieldElement::one()]].concat()),
        );
        prop_right_child_and_height(
            U32s::new([7, 0]),
            Some(&vec![expected_end_stack.clone(), vec![BFieldElement::new(2)]].concat()),
        );
        prop_right_child_and_height(
            U32s::new([15, 0]),
            Some(&vec![expected_end_stack.clone(), vec![BFieldElement::new(3)]].concat()),
        );
        prop_right_child_and_height(
            U32s::new([31, 0]),
            Some(&vec![expected_end_stack.clone(), vec![BFieldElement::new(4)]].concat()),
        );
        prop_right_child_and_height(
            U32s::new([63, 0]),
            Some(&vec![expected_end_stack, vec![BFieldElement::new(5)]].concat()),
        );
    }

    #[test]
    fn right_child_and_height_node_index_any() {
        prop_right_child_and_height(
            U32s::new([1, 0]),
            Some(
                &vec![
                    vec![get_init_tvm_stack(), vec![BFieldElement::zero()]].concat(),
                    vec![BFieldElement::zero()],
                ]
                .concat(),
            ),
        );
        prop_right_child_and_height(
            U32s::new([2, 0]),
            Some(
                &vec![
                    vec![get_init_tvm_stack(), vec![BFieldElement::one()]].concat(),
                    vec![BFieldElement::zero()],
                ]
                .concat(),
            ),
        );
        prop_right_child_and_height(
            U32s::new([3, 0]),
            Some(
                &vec![
                    vec![get_init_tvm_stack(), vec![BFieldElement::zero()]].concat(),
                    vec![BFieldElement::one()],
                ]
                .concat(),
            ),
        );
        prop_right_child_and_height(
            U32s::new([4, 0]),
            Some(
                &vec![
                    vec![get_init_tvm_stack(), vec![BFieldElement::zero()]].concat(),
                    vec![BFieldElement::zero()],
                ]
                .concat(),
            ),
        );
        prop_right_child_and_height(
            U32s::new([5, 0]),
            Some(
                &vec![
                    vec![get_init_tvm_stack(), vec![BFieldElement::one()]].concat(),
                    vec![BFieldElement::zero()],
                ]
                .concat(),
            ),
        );
        prop_right_child_and_height(
            U32s::new([6, 0]),
            Some(
                &vec![
                    vec![get_init_tvm_stack(), vec![BFieldElement::one()]].concat(),
                    vec![BFieldElement::one()],
                ]
                .concat(),
            ),
        );
        prop_right_child_and_height(
            U32s::new([7, 0]),
            Some(
                &vec![
                    vec![get_init_tvm_stack(), vec![BFieldElement::zero()]].concat(),
                    vec![BFieldElement::new(2)],
                ]
                .concat(),
            ),
        );
        prop_right_child_and_height(
            U32s::new([8, 0]),
            Some(
                &vec![
                    vec![get_init_tvm_stack(), vec![BFieldElement::zero()]].concat(),
                    vec![BFieldElement::zero()],
                ]
                .concat(),
            ),
        );
        prop_right_child_and_height(
            U32s::new([14, 0]),
            Some(
                &vec![
                    vec![get_init_tvm_stack(), vec![BFieldElement::one()]].concat(),
                    vec![BFieldElement::new(2)],
                ]
                .concat(),
            ),
        );
        prop_right_child_and_height(
            U32s::new([15, 0]),
            Some(
                &vec![
                    vec![get_init_tvm_stack(), vec![BFieldElement::zero()]].concat(),
                    vec![BFieldElement::new(3)],
                ]
                .concat(),
            ),
        );
        prop_right_child_and_height(
            U32s::new([16, 0]),
            Some(
                &vec![
                    vec![get_init_tvm_stack(), vec![BFieldElement::zero()]].concat(),
                    vec![BFieldElement::zero()],
                ]
                .concat(),
            ),
        );
        prop_right_child_and_height(
            U32s::new([17, 0]),
            Some(
                &vec![
                    vec![get_init_tvm_stack(), vec![BFieldElement::one()]].concat(),
                    vec![BFieldElement::zero()],
                ]
                .concat(),
            ),
        );
        println!("18");
        prop_right_child_and_height(
            U32s::new([18, 0]),
            Some(
                &vec![
                    vec![get_init_tvm_stack(), vec![BFieldElement::zero()]].concat(),
                    vec![BFieldElement::one()],
                ]
                .concat(),
            ),
        );
        prop_right_child_and_height(
            U32s::new([u32::MAX - 1, u32::MAX / 2]),
            Some(
                &vec![
                    vec![get_init_tvm_stack(), vec![BFieldElement::one()]].concat(),
                    vec![BFieldElement::new(61)],
                ]
                .concat(),
            ),
        );
        prop_right_child_and_height(
            U32s::new([u32::MAX, u32::MAX / 2]),
            Some(
                &vec![
                    vec![get_init_tvm_stack(), vec![BFieldElement::zero()]].concat(),
                    vec![BFieldElement::new(62)],
                ]
                .concat(),
            ),
        );
    }

    #[test]
    fn right_child_and_height_node_is_left_child() {
        // All should return (false, height) as leftmost ancestors are always left-children.
        let expected_end_stack = vec![get_init_tvm_stack(), vec![BFieldElement::zero()]].concat();
        prop_right_child_and_height(
            U32s::new([1, 0]),
            Some(&vec![expected_end_stack.clone(), vec![BFieldElement::zero()]].concat()),
        );
        prop_right_child_and_height(
            U32s::new([3, 0]),
            Some(&vec![expected_end_stack.clone(), vec![BFieldElement::one()]].concat()),
        );
        prop_right_child_and_height(
            U32s::new([4, 0]),
            Some(&vec![expected_end_stack, vec![BFieldElement::zero()]].concat()),
        );
    }

    fn prop_right_child_and_height(node_index: U32s<2>, expected: Option<&[BFieldElement]>) {
        let mut init_stack = get_init_tvm_stack();
        for elem in node_index.to_sequence().into_iter().rev() {
            init_stack.push(elem);
        }

        let _execution_result = rust_tasm_equivalence_prop::<MmrRightChildAndHeight>(
            &init_stack,
            &[],
            &[],
            &mut HashMap::default(),
            0,
            expected,
        );
    }
}
