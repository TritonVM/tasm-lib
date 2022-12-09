use num::BigUint;
use twenty_first::{
    amount::u32s::U32s,
    util_types::{algebraic_hasher::Hashable, mmr},
};

use crate::{arithmetic::u32s_2::decr::U32s2Decr, snippet_trait::Snippet};

pub struct MmrRightChild;

const SNIPPET_NAME: &str = "mmr_right_child";

impl Snippet for MmrRightChild {
    const STACK_DIFF: isize = 0;

    const NAME: &'static str = SNIPPET_NAME;

    /// Consider inlining this, instead of calling a function
    fn get_function() -> String {
        let u32s_2_decr = U32s2Decr::NAME;
        let u32s_2_decr_function = U32s2Decr::get_function();
        format!(
            "
            // Before: _ nodex_index_hi node_index_lo
            // After: _ right_child_hi right_child_lo
            {SNIPPET_NAME}:
                call {u32s_2_decr}
                return

            {u32s_2_decr_function}
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
        let ret: u64 = mmr::shared::right_child(node_index as u128) as u64;
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

        let mut tasm_stack = init_stack.clone();
        let execution_result = MmrRightChild::run_tasm(&mut tasm_stack, vec![], vec![]);
        println!(
            "Cycle count for `{SNIPPET_NAME}`: {}",
            execution_result.cycle_count
        );
        println!(
            "Hash table height for `{SNIPPET_NAME}`: {}",
            execution_result.hash_table_height
        );

        let mut rust_stack = init_stack;
        MmrRightChild::rust_shadowing(&mut rust_stack, vec![], vec![]);

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
