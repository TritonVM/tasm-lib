use std::collections::HashMap;

use twenty_first::shared_math::b_field_element::BFieldElement;
use twenty_first::util_types::mmr;

use crate::arithmetic::u64::eq_u64::EqU64;
use crate::arithmetic::u64::lt_u64::LtU64;
use crate::snippet::Snippet;

use super::left_child::MmrLeftChild;
use super::leftmost_ancestor::MmrLeftMostAncestor;
use super::right_child::MmrRightChild;

pub struct MmrRightAncestorCountAndHeight;

impl Snippet for MmrRightAncestorCountAndHeight {
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
        "right_ancestor_count_and_own_height"
    }

    fn function_body(library: &mut crate::library::Library) -> String
    where
        Self: Sized,
    {
        let entrypoint = Self::entrypoint();
        let eq_u64 = library.import::<EqU64>();
        let lt_u64 = library.import::<LtU64>();
        let left_child = library.import::<MmrLeftChild>();
        let right_child = library.import::<MmrRightChild>();
        let leftmost_ancestor = library.import::<MmrLeftMostAncestor>();

        format!(
            "
            // Before: _ ni_hi ni_lo
            // After: _ (right_lineage_count:u32) (height: u32)
            {entrypoint}:
                // Get leftmost ancestor and its height on top of stack
                push 0 // is `right_ancestor_count` (`rac`) onto stack
                dup2
                dup2
                call {leftmost_ancestor}
                // stack: _ ni_hi ni_lo rac c_hi c_lo height

                swap2
                swap1
                // stack: _ ni_hi ni_lo rac height c_hi c_lo
                call {entrypoint}_loop
                // Stack: ni_hi ni_lo rac height c_hi c_lo
                pop
                pop
                swap2
                pop
                swap2
                pop

                // Stack: _ rac height
                return

            // Stack start and end:
            // _ ni_hi ni_lo rac height c_hi c_lo
            {entrypoint}_loop:
                // Loop condition: Return if candidate (c) == node index (ni)
                dup5
                dup5
                dup3
                dup3
                call {eq_u64}
                // Stack: _ ni_hi ni_lo rac height c_hi c_lo (c == ni)
                skiz return

                // Stack: _ ni_hi ni_lo rac height c_hi c_lo
                dup1
                dup1
                dup4
                // Stack: _ ni_hi ni_lo rac height c_hi c_lo c_hi c_lo height

                call {left_child}
                // Stack: ni_hi ni_lo rac height c_hi c_lo lc_hi lc_lo

                dup7 dup7
                // Stack: ni_hi ni_lo rac height c_hi c_lo lc_hi lc_lo ni_hi ni_lo
                swap2
                swap1
                swap3
                swap1
                // Stack: ni_hi ni_lo rac height c_hi c_lo ni_hi ni_lo lc_hi lc_lo

                call {lt_u64}
                // Stack: ni_hi ni_lo rac height c_hi c_lo ni_hi ni_lo lc_hi lc_lo c_is_right

                push 1
                swap1
                // Stack: _ ni_hi ni_lo rac height c_hi c_lo ni_hi ni_lo lc_hi lc_lo 1 c_is_right

                skiz call {entrypoint}_branch_then
                skiz call {entrypoint}_branch_else
                // Stack: _ ni_hi ni_lo rac height c_hi c_lo

                // Decrement height by one
                swap2
                push -1
                add
                swap2

                // Stack: _ ni_hi ni_lo rac (height - 1) c_hi c_lo

                recurse

            {entrypoint}_branch_then:
                // purpose: Set candidate to right child and increase rac by one
                // Stack: _ ni_hi ni_lo rac height c_hi c_lo ni_hi ni_lo lc_hi lc_lo 1
                pop
                swap7
                push 1
                add
                swap7
                // Stack: _ ni_hi ni_lo (rac + 1) height c_hi c_lo ni_hi ni_lo lc_hi lc_lo

                pop
                pop
                pop
                pop
                call {right_child}
                // Stack: _ ni_hi ni_lo (rac + 1) height rc_hi rc_lo

                push 0
                // Stack: _ ni_hi ni_lo (rac + 1) height c_hi c_lo 0 (aftrer rename)

                return

            {entrypoint}_branch_else:
                // purpose: Set candidate to left child, set rac counter to zero
                // Stack: _ ni_hi ni_lo rac height c_hi c_lo ni_hi ni_lo lc_hi lc_lo

                swap7
                pop
                push 0
                swap7
                // Stack: _ ni_hi ni_lo (rac = 0) height c_hi c_lo ni_hi ni_lo lc_hi lc_lo

                swap4 pop swap4 pop
                // Stack: ni_hi ni_lo (rac = 0) height lc_hi lc_lo ni_hi ni_lo

                pop pop
                // Stack: ni_hi ni_lo (rac = 0) height lc_hi lc_lo

                // End stack: ni_hi ni_lo (rac = 0) height c_hi c_lo (after rename)
                return

            "
        )
    }

    fn rust_shadowing(
        stack: &mut Vec<BFieldElement>,
        _std_in: Vec<BFieldElement>,
        _secret_in: Vec<BFieldElement>,
        _memory: &mut HashMap<BFieldElement, BFieldElement>,
    ) where
        Self: Sized,
    {
        // TODO: Remove this when twenty-first is updated
        /// Traversing from this node upwards, count how many of the ancestor (including itself)
        /// is a right child. This number is used to determine how many nodes to insert when a
        /// new leaf is added.
        fn right_ancestor_count_and_own_height(node_index: u128) -> (u32, u32) {
            let (mut candidate, mut candidate_height) = mmr::shared::leftmost_ancestor(node_index);

            // leftmost ancestor is always a left node, so count starts at 0.
            let mut right_ancestor_count = 0;

            loop {
                if candidate == node_index {
                    return (right_ancestor_count, candidate_height as u32);
                }

                let left_child = mmr::shared::left_child(candidate, candidate_height);
                let candidate_is_right_child = left_child < node_index;
                if candidate_is_right_child {
                    candidate = mmr::shared::right_child(candidate);
                    right_ancestor_count += 1;
                } else {
                    candidate = left_child;
                    right_ancestor_count = 0;
                };

                candidate_height -= 1;
            }
        }

        let node_index_lo: u32 = stack.pop().unwrap().try_into().unwrap();
        let node_index_hi: u32 = stack.pop().unwrap().try_into().unwrap();
        let node_index: u64 = (node_index_hi as u64) * (1u64 << 32) + node_index_lo as u64;

        let (ret, height) = right_ancestor_count_and_own_height(node_index as u128);
        stack.push(BFieldElement::new(ret as u64));

        stack.push(BFieldElement::new(height as u64));
    }
}

#[cfg(test)]
mod tests {
    use twenty_first::shared_math::b_field_element::BFieldElement;

    use crate::{get_init_tvm_stack, snippet::rust_tasm_equivalence_prop};

    use super::*;

    #[test]
    fn right_ancestor_count_test() {
        prop_right_ancestor_count_and_own_height(1, 0, 0);
        prop_right_ancestor_count_and_own_height(2, 1, 0);
        prop_right_ancestor_count_and_own_height(3, 0, 1);
        prop_right_ancestor_count_and_own_height(4, 0, 0);
        prop_right_ancestor_count_and_own_height(5, 2, 0);
        prop_right_ancestor_count_and_own_height(6, 1, 1);
        prop_right_ancestor_count_and_own_height(7, 0, 2);
        prop_right_ancestor_count_and_own_height(8, 0, 0);
        prop_right_ancestor_count_and_own_height(9, 1, 0);

        prop_right_ancestor_count_and_own_height(10, 0, 1);
        prop_right_ancestor_count_and_own_height(11, 0, 0);
        prop_right_ancestor_count_and_own_height(12, 3, 0);
        prop_right_ancestor_count_and_own_height(13, 2, 1);
        prop_right_ancestor_count_and_own_height(14, 1, 2);
        prop_right_ancestor_count_and_own_height(15, 0, 3);
        prop_right_ancestor_count_and_own_height(16, 0, 0);
        prop_right_ancestor_count_and_own_height(17, 1, 0);
        prop_right_ancestor_count_and_own_height(18, 0, 1);
        prop_right_ancestor_count_and_own_height(19, 0, 0);

        prop_right_ancestor_count_and_own_height(20, 2, 0);
        prop_right_ancestor_count_and_own_height(21, 1, 1);
        prop_right_ancestor_count_and_own_height(22, 0, 2);
        prop_right_ancestor_count_and_own_height(23, 0, 0);
        prop_right_ancestor_count_and_own_height(24, 1, 0);
        prop_right_ancestor_count_and_own_height(25, 0, 1);
        prop_right_ancestor_count_and_own_height(26, 0, 0);
        prop_right_ancestor_count_and_own_height(27, 4, 0);
        prop_right_ancestor_count_and_own_height(28, 3, 1);
        prop_right_ancestor_count_and_own_height(29, 2, 2);

        prop_right_ancestor_count_and_own_height(30, 1, 3);
        prop_right_ancestor_count_and_own_height(31, 0, 4);
        prop_right_ancestor_count_and_own_height(32, 0, 0);
        prop_right_ancestor_count_and_own_height(33, 1, 0);
        prop_right_ancestor_count_and_own_height(34, 0, 1);
        prop_right_ancestor_count_and_own_height(35, 0, 0);
        prop_right_ancestor_count_and_own_height(36, 2, 0);
        prop_right_ancestor_count_and_own_height(37, 1, 1);
        prop_right_ancestor_count_and_own_height(38, 0, 2);
        prop_right_ancestor_count_and_own_height(39, 0, 0);

        prop_right_ancestor_count_and_own_height(40, 1, 0);
        prop_right_ancestor_count_and_own_height(41, 0, 1);

        // Run test for big numbers
        for i in 0..32 {
            prop_right_ancestor_count_and_own_height(u32::MAX as u64 - i, i as u32, 31 - i as u32);
        }

        // Run test for very big numbers
        for i in 0..63 {
            prop_right_ancestor_count_and_own_height((u64::MAX >> 1) - i, i as u32, 62 - i as u32);
        }
    }

    fn prop_right_ancestor_count_and_own_height(
        node_index: u64,
        expected_count: u32,
        expected_height: u32,
    ) {
        println!("node_index = {node_index}");
        let mut init_stack = get_init_tvm_stack();
        let node_index_hi = BFieldElement::new(node_index >> 32);
        let node_index_lo = BFieldElement::new(node_index & u32::MAX as u64);
        init_stack.push(node_index_hi);
        init_stack.push(node_index_lo);

        // _ (right_lineage_count:u32) (height: u32)
        let expected = vec![
            get_init_tvm_stack(),
            vec![
                BFieldElement::new(expected_count as u64),
                BFieldElement::new(expected_height as u64),
            ],
        ]
        .concat();
        let _execution_result = rust_tasm_equivalence_prop::<MmrRightAncestorCountAndHeight>(
            &init_stack,
            &[],
            &[],
            &mut HashMap::default(),
            0,
            Some(&expected),
        );
    }
}
