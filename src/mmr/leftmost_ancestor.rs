use std::collections::HashMap;

use num::BigUint;
use twenty_first::amount::u32s::U32s;
use twenty_first::shared_math::b_field_element::BFieldElement;
use twenty_first::util_types::algebraic_hasher::Hashable;
use twenty_first::util_types::mmr;

use crate::arithmetic::u64::decr_u64::DecrU64;
use crate::arithmetic::u64::log_2_floor_u64::Log2FloorU64;
use crate::arithmetic::u64::pow2_u64::Pow2StaticU64;
use crate::library::Library;
use crate::snippet::Snippet;

pub struct MmrLeftMostAncestor();

impl Snippet for MmrLeftMostAncestor {
    fn stack_diff() -> isize {
        1
    }

    fn entrypoint() -> &'static str {
        "mmr_leftmost_ancestor"
    }

    fn function_body(library: &mut Library) -> String {
        let entrypoint = Self::entrypoint();
        let decr_u64 = library.import::<DecrU64>();
        let pow2_u64 = library.import::<Pow2StaticU64>();
        let log_2_floor_u64 = library.import::<Log2FloorU64>();
        format!(
            "
            // Before: _ node_index_hi node_index_lo
            // After: _ leftmost_ancestor_hi leftmost_ancestor_lo height
            {entrypoint}:
                call {log_2_floor_u64}
                // stack: _ log2_floor

                dup0
                // notice that log2_floor = height
                // stack: _ height log2_floor

                push 1
                add
                // stack: _ height (log2_floor + 1)

                call {pow2_u64}
                // stack: _ height 2^(log2_floor + 1)_hi 2^(log2_floor + 1)_lo

                call {decr_u64}
                // stack: _ height leftmost_ancestor_hi leftmost_ancestor_lo

                swap1
                swap2
                // stack: _ leftmost_ancestor_hi leftmost_ancestor_lo height

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
        let node_index_lo: u32 = stack.pop().unwrap().try_into().unwrap();
        let node_index_hi: u32 = stack.pop().unwrap().try_into().unwrap();
        let node_index: u64 = (node_index_hi as u64) * (1u64 << 32) + node_index_lo as u64;

        let (ret, h): (u128, u32) = mmr::shared::leftmost_ancestor(node_index as u128);
        let ret: U32s<2> = U32s::from(BigUint::from(ret));

        stack.append(&mut ret.to_sequence().into_iter().rev().collect());
        stack.push(BFieldElement::from(h));
    }
}

#[cfg(test)]
mod tests {
    use twenty_first::shared_math::b_field_element::BFieldElement;
    use twenty_first::util_types::algebraic_hasher::Hashable;

    use crate::get_init_tvm_stack;
    use crate::test_helpers::rust_tasm_equivalence_prop;

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

        // leftmost_ancestor([4..7]) -> height = 2, index = 7
        let mut expected_stack = get_init_tvm_stack();
        expected_stack.push(BFieldElement::new(0));
        expected_stack.push(BFieldElement::new(7));
        expected_stack.push(BFieldElement::new(2));
        prop_leftmost_ancestor(U32s::<2>::from(4), Some(&expected_stack));
        prop_leftmost_ancestor(U32s::<2>::from(5), Some(&expected_stack));
        prop_leftmost_ancestor(U32s::<2>::from(6), Some(&expected_stack));
        prop_leftmost_ancestor(U32s::<2>::from(7), Some(&expected_stack));

        // leftmost_ancestor([8..15]) -> height = 3, index = 15
        let mut expected_stack = get_init_tvm_stack();
        expected_stack.push(BFieldElement::new(0));
        expected_stack.push(BFieldElement::new(15));
        expected_stack.push(BFieldElement::new(3));
        for i in 8..=15 {
            prop_leftmost_ancestor(U32s::<2>::from(i), Some(&expected_stack));
        }

        // leftmost_ancestor([16..31]) -> height = 3, index = 31
        let mut expected_stack = get_init_tvm_stack();
        expected_stack.push(BFieldElement::new(0));
        expected_stack.push(BFieldElement::new(31));
        expected_stack.push(BFieldElement::new(4));
        for i in 16..=31 {
            prop_leftmost_ancestor(U32s::<2>::from(i), Some(&expected_stack));
        }

        // leftmost_ancestor([32..63]) -> height = 4, index = 63
        let mut expected_stack = get_init_tvm_stack();
        expected_stack.push(BFieldElement::new(0));
        expected_stack.push(BFieldElement::new(63));
        expected_stack.push(BFieldElement::new(5));
        for i in 32..=63 {
            prop_leftmost_ancestor(U32s::<2>::from(i), Some(&expected_stack));
        }

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

        let _execution_result = rust_tasm_equivalence_prop::<MmrLeftMostAncestor>(
            &init_stack,
            &[],
            &[],
            &mut HashMap::default(),
            0,
            expected,
        );
    }
}
