use std::collections::HashMap;

use num::BigUint;
use twenty_first::amount::u32s::U32s;
use twenty_first::shared_math::b_field_element::BFieldElement;
use twenty_first::util_types::algebraic_hasher::Hashable;
use twenty_first::util_types::mmr;

use crate::arithmetic::u64::decr_u64::DecrU64;
use crate::library::Library;
use crate::snippet::Snippet;

pub struct MmrRightChild();

impl Snippet for MmrRightChild {
    fn stack_diff() -> isize {
        0
    }

    fn entrypoint() -> &'static str {
        "mmr_right_child"
    }

    /// Consider inlining this, instead of calling a function
    fn function_body(library: &mut Library) -> String {
        let entrypoint = Self::entrypoint();
        let decr_u64 = library.import::<DecrU64>();
        format!(
            "
            // Before: _ nodex_index_hi node_index_lo
            // After: _ right_child_hi right_child_lo
            {entrypoint}:
                call {decr_u64}
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
        let ret: u64 = mmr::shared::right_child(node_index as u128) as u64;
        let ret: U32s<2> = U32s::from(BigUint::from(ret));

        stack.append(&mut ret.to_sequence().into_iter().rev().collect());
    }
}

#[cfg(test)]
mod tests {
    use num::Zero;
    use twenty_first::shared_math::b_field_element::BFieldElement;
    use twenty_first::util_types::algebraic_hasher::Hashable;

    use crate::get_init_tvm_stack;
    use crate::test_helpers::rust_tasm_equivalence_prop;

    use super::*;

    #[test]
    fn u32s_right_child_simple() {
        let mut expected_stack = get_init_tvm_stack();
        expected_stack.push(BFieldElement::zero());
        expected_stack.push(BFieldElement::new(2));
        prop_right_child(U32s::<2>::from(3), Some(&expected_stack));

        expected_stack = get_init_tvm_stack();
        expected_stack.push(BFieldElement::zero());
        expected_stack.push(BFieldElement::new(9));
        prop_right_child(U32s::<2>::from(10), Some(&expected_stack));

        expected_stack = get_init_tvm_stack();
        expected_stack.push(BFieldElement::zero());
        expected_stack.push(BFieldElement::new(14));
        prop_right_child(U32s::<2>::from(15), Some(&expected_stack));
    }

    fn prop_right_child(node_index: U32s<2>, expected: Option<&[BFieldElement]>) {
        let mut init_stack = get_init_tvm_stack();
        for elem in node_index.to_sequence().into_iter().rev() {
            init_stack.push(elem);
        }

        let _execution_result = rust_tasm_equivalence_prop::<MmrRightChild>(
            &init_stack,
            &[],
            &[],
            &mut HashMap::default(),
            0,
            expected,
        );
    }
}
