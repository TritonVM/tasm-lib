use std::collections::HashMap;

use num::BigUint;
use twenty_first::amount::u32s::U32s;
use twenty_first::shared_math::b_field_element::BFieldElement;
use twenty_first::util_types::algebraic_hasher::Hashable;
use twenty_first::util_types::mmr;

use crate::arithmetic::u32s_2::powers_of_two::U32s2PowersOfTwoStatic;
use crate::arithmetic::u32s_2::sub::U32s2Sub;
use crate::library::Library;
use crate::snippet_trait::Snippet;

pub struct MmrLeftChild();

impl Snippet for MmrLeftChild {
    fn stack_diff() -> isize {
        -1
    }

    fn entrypoint() -> &'static str {
        "mmr_left_child"
    }

    fn function_body(library: &mut Library) -> String {
        let entrypoint = Self::entrypoint();
        let powers_of_two = library.import::<U32s2PowersOfTwoStatic>();
        let u32s_2_sub = library.import::<U32s2Sub>();
        format!(
            "
            // Before: _ ni_hi ni_lo height
            // After: _ left_child_hi left_child_lo
            {entrypoint}:
                call {powers_of_two} // -> _ ni_hi ni_lo (2^height)_hi (2^height)_lo
                swap2
                swap1
                swap3
                swap1                // -> _ (2^height)_hi (2^height)_lo ni_hi ni_lo
                call {u32s_2_sub}    // -> _ left_child_hi left_child_lo
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
    use twenty_first::shared_math::b_field_element::BFieldElement;
    use twenty_first::util_types::algebraic_hasher::Hashable;

    use crate::{get_init_tvm_stack, snippet_trait::rust_tasm_equivalence_prop};

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

        let _execution_result = rust_tasm_equivalence_prop::<MmrLeftChild>(
            &init_stack,
            &[],
            &[],
            &mut HashMap::default(),
            0,
            expected,
        );
    }
}
