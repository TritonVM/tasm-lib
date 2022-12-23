use std::collections::HashMap;
use twenty_first::shared_math::b_field_element::BFieldElement;
use twenty_first::shared_math::other::log_2_floor;

use crate::arithmetic::u32s_2::add::U32s2Add;
use crate::arithmetic::u32s_2::decr::U32s2Decr;
use crate::arithmetic::u32s_2::powers_of_two::U32s2PowersOfTwoStatic;
use crate::arithmetic::u32s_2::sub::U32s2Sub;
use crate::library::Library;
use crate::snippet_trait::Snippet;

use super::get_height_from_data_index::GetHeightFromDataIndex;

pub struct MmrNonLeafNodesLeft();

impl Snippet for MmrNonLeafNodesLeft {
    fn stack_diff() -> isize {
        // Pops a U32<2> and pushes a U32<2>
        0
    }

    fn entrypoint() -> &'static str {
        "non_leaf_nodes_left"
    }

    fn function_body(library: &mut Library) -> String {
        let entrypoint = Self::entrypoint();
        let get_height_from_data_index = library.import::<GetHeightFromDataIndex>();
        let decr = library.import::<U32s2Decr>();
        let sub = library.import::<U32s2Sub>();
        let add = library.import::<U32s2Add>();
        let two_pow = library.import::<U32s2PowersOfTwoStatic>();

        format!(
            "
                // BEFORE: _ data_index_hi data_index_lo
                // AFTER: _ node_count_hi node_count_lo
                {entrypoint}:
                    push 0
                    push 0
                    // stack: _ data_index_hi data_index_lo 0 0

                    swap3
                    swap1
                    swap2
                    // stack: _ 0 0 data_index_hi data_index_lo

                    // rename (0 0) to (ret_hi ret_lo)
                    // rename (data_index_hi data_index_lo) to (dia_hi dia_lo)
                    // stack: _ ret_hi ret_lo dia_hi dia_lo

                    call {entrypoint}_while
                    // stack: _ ret_hi ret_lo dia_hi dia_lo

                    pop pop
                    return

                    // Start and end: _ ret_hi ret_lo dia_hi dia_lo
                    {entrypoint}_while:
                        dup1 push 0 eq
                        // stack: _ ret_hi ret_lo dia_hi dia_lo (dia_hi == 0)

                        dup1 push 0 eq
                        // stack: _ ret_hi ret_lo dia_hi dia_lo (dia_hi == 0) (dia_lo == 0)

                        add push 2 eq
                        // stack: _ ret_hi ret_lo dia_hi dia_lo (dia_hi == 0 && dia_lo == 0)

                        skiz return
                        // stack: _ ret_hi ret_lo dia_hi dia_lo

                        dup1 dup1
                        call {decr}
                        // stack: _ ret_hi ret_lo dia_hi dia_lo (dia - 1)_hi (dia - 1)_lo

                        call {get_height_from_data_index}
                        // stack: _ ret_hi ret_lo dia_hi dia_lo left_data_height

                        call {two_pow}
                        // stack: _ ret_hi ret_lo dia_hi dia_lo two_pow_hi two_pow_lo

                        // Goal: ret -> ret + two_pow_lo - 1
                        swap4
                        swap1
                        // stack: _ ret_hi two_pow_lo dia_hi dia_lo ret_lo two_pow_hi

                        swap5
                        swap1
                        // stack: _ two_pow_hi two_pow_lo dia_hi dia_lo ret_hi ret_lo

                        dup5 dup5
                        // stack: _ two_pow_hi two_pow_lo dia_hi dia_lo ret_hi ret_lo two_pow_hi two_pow_lo

                        call {add}
                        // stack: _ two_pow_hi two_pow_lo dia_hi dia_lo (ret + two_pow)_hi (ret + two_pow)_lo

                        call {decr}
                        // stack: _ two_pow_hi two_pow_lo dia_hi dia_lo new_ret_hi new_ret_lo

                        swap4 swap1 swap5 swap1
                        // stack: _ new_ret_hi new_ret_lo dia_hi dia_lo two_pow_hi two_pow_lo

                        swap2 swap1 swap3 swap1
                        // stack: _ new_ret_hi new_ret_lo two_pow_hi two_pow_lo dia_hi dia_lo

                        call {sub}
                        // stack: _ new_ret_hi new_ret_lo new_dia_hi new_dia_lo

                        recurse
                "
        )
    }

    fn rust_shadowing(
        stack: &mut Vec<BFieldElement>,
        _std_in: Vec<BFieldElement>,
        _secret_in: Vec<BFieldElement>,
        _memory: &mut HashMap<BFieldElement, BFieldElement>,
    ) {
        // TODO: REMOVE these two local functions once needed helper functions have been made public in twenty-first/MMR
        fn non_leaf_nodes_left(data_index: u128) -> u128 {
            fn get_height_from_data_index(data_index: u128) -> u128 {
                log_2_floor(data_index + 1) as u128
            }

            let mut acc = 0;
            let mut data_index_acc = data_index;
            while data_index_acc > 0 {
                // Accumulate how many nodes in the tree of the nearest left neighbor that are not leafs.
                // We count this number for the nearest left neighbor since only the non-leafs in that
                // tree were inserted prior to the leaf this function is called for.
                // For a tree of height 2, there are 2^2 - 1 non-leaf nodes, note that height starts at
                // 0.
                // Since more than one subtree left of the requested index can contain non-leafs, we have
                // to run this accumulater untill data_index_acc is zero.
                let left_data_height = get_height_from_data_index(data_index_acc - 1);
                acc += (1 << left_data_height) - 1;
                data_index_acc -= 1 << left_data_height;
            }

            acc
        }

        let data_index_lo: u32 = stack.pop().unwrap().try_into().unwrap();
        let data_index_hi: u32 = stack.pop().unwrap().try_into().unwrap();
        let data_index: u64 = (data_index_hi as u64) * (1u64 << 32) + data_index_lo as u64;

        // TODO: Call `non_leaf_nodes_left` from MMR here once it has been made public
        let result = non_leaf_nodes_left(data_index as u128) as u64;

        stack.push(BFieldElement::new(result >> 32));
        stack.push(BFieldElement::new(result & 0xFFFFFFFFu32 as u64));
    }
}

#[cfg(test)]
mod tests {
    use rand::{thread_rng, RngCore};
    use twenty_first::{
        amount::u32s::U32s, shared_math::b_field_element::BFieldElement,
        util_types::algebraic_hasher::Hashable,
    };

    use crate::{get_init_tvm_stack, snippet_trait::rust_tasm_equivalence_prop};

    use super::*;

    #[test]
    fn non_leaf_nodes_left_simple_test() {
        let mut expected = get_init_tvm_stack();
        expected.push(BFieldElement::new(0));
        expected.push(BFieldElement::new(0));
        prop_non_leaf_nodes_left(0, Some(&expected));
        prop_non_leaf_nodes_left(1, Some(&expected));

        expected = get_init_tvm_stack();
        expected.push(BFieldElement::new(0));
        expected.push(BFieldElement::new(1));
        prop_non_leaf_nodes_left(2, Some(&expected));
        prop_non_leaf_nodes_left(3, Some(&expected));

        expected = get_init_tvm_stack();
        expected.push(BFieldElement::new(0));
        expected.push(BFieldElement::new(3));
        prop_non_leaf_nodes_left(4, Some(&expected));
        prop_non_leaf_nodes_left(5, Some(&expected));

        expected = get_init_tvm_stack();
        expected.push(BFieldElement::new(0));
        expected.push(BFieldElement::new(4));
        prop_non_leaf_nodes_left(6, Some(&expected));
        prop_non_leaf_nodes_left(7, Some(&expected));

        expected = get_init_tvm_stack();
        expected.push(BFieldElement::new(0));
        expected.push(BFieldElement::new(7));
        prop_non_leaf_nodes_left(8, Some(&expected));
        prop_non_leaf_nodes_left(9, Some(&expected));

        expected = get_init_tvm_stack();
        expected.push(BFieldElement::new(0));
        expected.push(BFieldElement::new(8));
        prop_non_leaf_nodes_left(10, Some(&expected));
        prop_non_leaf_nodes_left(11, Some(&expected));

        expected = get_init_tvm_stack();
        expected.push(BFieldElement::new(0));
        expected.push(BFieldElement::new(10));
        prop_non_leaf_nodes_left(12, Some(&expected));
        prop_non_leaf_nodes_left(13, Some(&expected));

        prop_non_leaf_nodes_left(u32::MAX as u64, None);
        prop_non_leaf_nodes_left(u64::MAX / 2, None);
    }

    #[test]
    fn non_leaf_nodes_pbt() {
        let mut rng = thread_rng();
        for _ in 0..100 {
            prop_non_leaf_nodes_left(rng.next_u64(), None);
        }
    }

    fn prop_non_leaf_nodes_left(data_index: u64, expected: Option<&[BFieldElement]>) {
        let mut init_stack = get_init_tvm_stack();
        let value_as_u32_2 = U32s::new([
            (data_index & 0xFFFFFFFFu32 as u64) as u32,
            (data_index >> 32) as u32,
        ]);
        for elem in value_as_u32_2.to_sequence().into_iter().rev() {
            init_stack.push(elem);
        }

        let _execution_result = rust_tasm_equivalence_prop::<MmrNonLeafNodesLeft>(
            &init_stack,
            &[],
            &[],
            &mut HashMap::default(),
            expected,
        );
    }
}
