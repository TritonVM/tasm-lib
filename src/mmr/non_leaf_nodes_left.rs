use std::collections::HashMap;
use twenty_first::shared_math::b_field_element::BFieldElement;
use twenty_first::shared_math::other::log_2_floor;

use crate::arithmetic::u64::add_u64::AddU64;
use crate::arithmetic::u64::and_u64::AndU64;
use crate::arithmetic::u64::decr_u64::DecrU64;
use crate::arithmetic::u64::eq_u64::EqU64;
use crate::arithmetic::u64::incr_u64::IncrU64;
use crate::arithmetic::u64::log_2_floor_u64::Log2FloorU64;
use crate::arithmetic::u64::pow2_u64::Pow2StaticU64;
use crate::arithmetic::u64::sub_u64::SubU64;
use crate::library::Library;
use crate::snippet::Snippet;

use super::get_height_from_data_index::GetHeightFromDataIndex;

pub struct MmrNonLeafNodesLeftUsingAnd();

impl Snippet for MmrNonLeafNodesLeftUsingAnd {
    fn stack_diff() -> isize {
        0
    }

    fn entrypoint() -> &'static str {
        "non_leaf_nodes_left"
    }

    fn function_body(library: &mut Library) -> String {
        let entrypoint = Self::entrypoint();
        let log_2_floor_u64 = library.import::<Log2FloorU64>();
        let pow2_u64 = library.import::<Pow2StaticU64>();
        let and_u64 = library.import::<AndU64>();
        let eq_u64 = library.import::<EqU64>();
        let decr_u64 = library.import::<DecrU64>();
        let incr_u64 = library.import::<IncrU64>();
        let add_u64 = library.import::<AddU64>();

        format!(
            "
        // BEFORE: _ data_index_hi data_index_lo
        // AFTER: _ node_count_hi node_count_lo
        {entrypoint}:
            dup1 dup1
            call {log_2_floor_u64}
            call {incr_u64}
            // stack: _ di_hi di_lo log2_floor

            push 0
            push 0 push 0
            // stack: _ di_hi di_lo log2_floor h ret_hi ret_lo

            call {entrypoint}_while
            // stack: _ di_hi di_lo log2_floor h ret_hi ret_lo

            swap4 pop
            // stack: _ di_hi ret_lo log2_floor h ret_hi

            swap4 pop
            // stack: _ ret_hi ret_lo log2_floor h

            pop pop
            // stack: _ ret_hi ret_lo

            return

        // Start/end stack: _ di_hi di_lo log2_floor h ret_hi ret_lo
        {entrypoint}_while:
            dup3 dup3 eq
            // stack: _ di_hi di_lo log2_floor h ret_hi ret_lo (h == log2_floor)

            skiz return
            // stack: _ di_hi di_lo log2_floor h ret_hi ret_lo

            dup2
            call {pow2_u64}
            // stack: _ di_hi di_lo log2_floor h ret_hi ret_lo pow_hi pow_lo

            dup1 dup1
            dup9 dup9
            // stack: _ di_hi di_lo log2_floor h ret_hi ret_lo pow_hi pow_lo pow_hi pow_lo di_hi di_lo

            call {and_u64}
            // stack: _ di_hi di_lo log2_floor h ret_hi ret_lo pow_hi pow_lo and_hi and_lo

            push 0 push 0
            call {eq_u64}
            // stack: _ di_hi di_lo log2_floor h ret_hi ret_lo pow_hi pow_lo (and_expr == 0)

            push 0
            eq
            skiz call {entrypoint}_if_then
            // stack: _ di_hi di_lo log2_floor h ret_hi ret_lo pow_hi pow_lo

            pop pop
            // stack: _ di_hi di_lo log2_floor h ret_hi ret_lo

            swap2 push 1 add swap2
            // stack: _ di_hi di_lo log2_floor (h + 1) ret_hi ret_lo

            recurse

            // Start/end stack: _ di_hi di_lo log2_floor h ret_hi ret_lo pow_hi pow_lo
        {entrypoint}_if_then:
            call {decr_u64}
            // stack: _ di_hi di_lo log2_floor h ret_hi ret_lo (pow - 1)_hi (pow - 1)_lo

            call {add_u64}
            // stack: _ di_hi di_lo log2_floor h (ret + 2^h - 1)_hi (ret + 2^h - 1)_lo

            push 0 push 0
            // rename: ret expression to `ret`
            // stack: _ di_hi di_lo log2_floor h ret_hi ret_lo 0 0
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
        // TODO: Replace this by function from twenty-first when that has been made public
        fn non_leaf_nodes_left(data_index: u128) -> u128 {
            let log_2_floor_plus_one = u128::BITS - data_index.leading_zeros();
            let mut h = 0;
            let mut ret = 0;
            while h != log_2_floor_plus_one {
                let pow = (1 << h) & data_index;
                if pow != 0 {
                    ret += pow - 1;
                }
                h += 1;
            }

            ret
        }

        let data_index_lo: u32 = stack.pop().unwrap().try_into().unwrap();
        let data_index_hi: u32 = stack.pop().unwrap().try_into().unwrap();
        let data_index: u64 = (data_index_hi as u64) * (1u64 << 32) + data_index_lo as u64;

        // // TODO: Call `non_leaf_nodes_left` from MMR here once it has been made public
        let result = non_leaf_nodes_left(data_index as u128) as u64;

        stack.push(BFieldElement::new(result >> 32));
        stack.push(BFieldElement::new(result & 0xFFFFFFFFu32 as u64));
    }
}

pub struct MmrNonLeafNodesLeftOld();

impl Snippet for MmrNonLeafNodesLeftOld {
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
        let decr_u64 = library.import::<DecrU64>();
        let sub_u64 = library.import::<SubU64>();
        let add_u64 = library.import::<AddU64>();
        let two_pow = library.import::<Pow2StaticU64>();

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
                        call {decr_u64}
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

                        call {add_u64}
                        // stack: _ two_pow_hi two_pow_lo dia_hi dia_lo (ret + two_pow)_hi (ret + two_pow)_lo

                        call {decr_u64}
                        // stack: _ two_pow_hi two_pow_lo dia_hi dia_lo new_ret_hi new_ret_lo

                        swap4 swap1 swap5 swap1
                        // stack: _ new_ret_hi new_ret_lo dia_hi dia_lo two_pow_hi two_pow_lo

                        swap2 swap1 swap3 swap1
                        // stack: _ new_ret_hi new_ret_lo two_pow_hi two_pow_lo dia_hi dia_lo

                        call {sub_u64}
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
mod nlnl_tests {
    use rand::{thread_rng, RngCore};
    use twenty_first::amount::u32s::U32s;
    use twenty_first::shared_math::b_field_element::BFieldElement;
    use twenty_first::util_types::algebraic_hasher::Hashable;

    use crate::get_init_tvm_stack;
    use crate::test_helpers::rust_tasm_equivalence_prop;

    use super::*;

    #[test]
    fn non_leaf_nodes_left_using_and_test() {
        let mut expected = get_init_tvm_stack();
        expected.push(BFieldElement::new(0));
        expected.push(BFieldElement::new(0));
        prop_non_leaf_nodes_left_using_and(0, Some(&expected));
        prop_non_leaf_nodes_left_using_and(1, Some(&expected));

        expected = get_init_tvm_stack();
        expected.push(BFieldElement::new(0));
        expected.push(BFieldElement::new(1));
        prop_non_leaf_nodes_left_using_and(2, Some(&expected));
        prop_non_leaf_nodes_left_using_and(3, Some(&expected));

        expected = get_init_tvm_stack();
        expected.push(BFieldElement::new(0));
        expected.push(BFieldElement::new(3));
        prop_non_leaf_nodes_left_using_and(4, Some(&expected));
        prop_non_leaf_nodes_left_using_and(5, Some(&expected));

        expected = get_init_tvm_stack();
        expected.push(BFieldElement::new(0));
        expected.push(BFieldElement::new(4));
        prop_non_leaf_nodes_left_using_and(6, Some(&expected));
        prop_non_leaf_nodes_left_using_and(7, Some(&expected));

        expected = get_init_tvm_stack();
        expected.push(BFieldElement::new(0));
        expected.push(BFieldElement::new(7));
        prop_non_leaf_nodes_left_using_and(8, Some(&expected));
        prop_non_leaf_nodes_left_using_and(9, Some(&expected));

        expected = get_init_tvm_stack();
        expected.push(BFieldElement::new(0));
        expected.push(BFieldElement::new(8));
        prop_non_leaf_nodes_left_using_and(10, Some(&expected));
        prop_non_leaf_nodes_left_using_and(11, Some(&expected));

        expected = get_init_tvm_stack();
        expected.push(BFieldElement::new(0));
        expected.push(BFieldElement::new(10));
        prop_non_leaf_nodes_left_using_and(12, Some(&expected));
        prop_non_leaf_nodes_left_using_and(13, Some(&expected));

        prop_non_leaf_nodes_left_using_and(u32::MAX as u64, None);
        prop_non_leaf_nodes_left_using_and(u64::MAX / 2, None);
    }

    #[test]
    fn non_leaf_nodes_left_old_test() {
        let mut expected = get_init_tvm_stack();
        expected.push(BFieldElement::new(0));
        expected.push(BFieldElement::new(0));
        prop_non_leaf_nodes_left_old(0, Some(&expected));
        prop_non_leaf_nodes_left_old(1, Some(&expected));

        expected = get_init_tvm_stack();
        expected.push(BFieldElement::new(0));
        expected.push(BFieldElement::new(1));
        prop_non_leaf_nodes_left_old(2, Some(&expected));
        prop_non_leaf_nodes_left_old(3, Some(&expected));

        expected = get_init_tvm_stack();
        expected.push(BFieldElement::new(0));
        expected.push(BFieldElement::new(3));
        prop_non_leaf_nodes_left_old(4, Some(&expected));
        prop_non_leaf_nodes_left_old(5, Some(&expected));

        expected = get_init_tvm_stack();
        expected.push(BFieldElement::new(0));
        expected.push(BFieldElement::new(4));
        prop_non_leaf_nodes_left_old(6, Some(&expected));
        prop_non_leaf_nodes_left_old(7, Some(&expected));

        expected = get_init_tvm_stack();
        expected.push(BFieldElement::new(0));
        expected.push(BFieldElement::new(7));
        prop_non_leaf_nodes_left_old(8, Some(&expected));
        prop_non_leaf_nodes_left_old(9, Some(&expected));

        expected = get_init_tvm_stack();
        expected.push(BFieldElement::new(0));
        expected.push(BFieldElement::new(8));
        prop_non_leaf_nodes_left_old(10, Some(&expected));
        prop_non_leaf_nodes_left_old(11, Some(&expected));

        expected = get_init_tvm_stack();
        expected.push(BFieldElement::new(0));
        expected.push(BFieldElement::new(10));
        prop_non_leaf_nodes_left_old(12, Some(&expected));
        prop_non_leaf_nodes_left_old(13, Some(&expected));

        prop_non_leaf_nodes_left_old(u32::MAX as u64, None);
        prop_non_leaf_nodes_left_old(u64::MAX / 2, None);
    }

    #[test]
    fn non_leaf_nodes_using_and_pbt() {
        let mut rng = thread_rng();
        for _ in 0..10 {
            prop_non_leaf_nodes_left_using_and(rng.next_u64(), None);
        }
    }

    #[test]
    fn non_leaf_nodes_old_pbt() {
        let mut rng = thread_rng();
        for _ in 0..10 {
            prop_non_leaf_nodes_left_old(rng.next_u64(), None);
        }
    }

    fn prop_non_leaf_nodes_left_using_and(data_index: u64, expected: Option<&[BFieldElement]>) {
        println!("data_index = {data_index}");
        let mut init_stack = get_init_tvm_stack();
        let value_as_u32_2 = U32s::new([
            (data_index & 0xFFFFFFFFu32 as u64) as u32,
            (data_index >> 32) as u32,
        ]);
        for elem in value_as_u32_2.to_sequence().into_iter().rev() {
            init_stack.push(elem);
        }

        let _execution_result = rust_tasm_equivalence_prop::<MmrNonLeafNodesLeftUsingAnd>(
            &init_stack,
            &[],
            &[],
            &mut HashMap::default(),
            0,
            expected,
        );
    }

    fn prop_non_leaf_nodes_left_old(data_index: u64, expected: Option<&[BFieldElement]>) {
        println!("data_index = {data_index}");
        let mut init_stack = get_init_tvm_stack();
        let value_as_u32_2 = U32s::new([
            (data_index & 0xFFFFFFFFu32 as u64) as u32,
            (data_index >> 32) as u32,
        ]);
        for elem in value_as_u32_2.to_sequence().into_iter().rev() {
            init_stack.push(elem);
        }

        let _execution_result = rust_tasm_equivalence_prop::<MmrNonLeafNodesLeftOld>(
            &init_stack,
            &[],
            &[],
            &mut HashMap::default(),
            0,
            expected,
        );
    }
}
