use std::collections::HashMap;

use num::{One, Zero};
use twenty_first::{shared_math::b_field_element::BFieldElement, util_types::mmr};

use crate::{
    arithmetic::u32s_2::{eq::U32s2Eq, lt::U32s2Lt},
    snippet_trait::Snippet,
};

use super::{
    left_child::MmrLeftChild, leftmost_ancestor::MmrLeftmostAncestor, right_child::MmrRightChild,
};

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
        "right_ancestor_count_and_own_height"
    }

    fn function_body(library: &mut crate::library::Library) -> String
    where
        Self: Sized,
    {
        let entrypoint = Self::entrypoint();
        let u32s_2_eq = library.import::<U32s2Eq>();
        let u32s_2_lt = library.import::<U32s2Lt>();
        let left_child = library.import::<MmrLeftChild>();
        let right_child = library.import::<MmrRightChild>();
        let leftmost_ancestor = library.import::<MmrLeftmostAncestor>();

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
                call {u32s_2_eq}
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

                call {u32s_2_lt}
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
                push1
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
}
