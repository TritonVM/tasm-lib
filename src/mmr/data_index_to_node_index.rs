use std::collections::HashMap;
use twenty_first::shared_math::b_field_element::BFieldElement;
use twenty_first::util_types::mmr;

use crate::arithmetic::u64::add_u64::AddU64;
use crate::arithmetic::u64::incr::U32s2Incr;
use crate::library::Library;
use crate::snippet_trait::Snippet;

use super::non_leaf_nodes_left::MmrNonLeafNodesLeftOld;

pub struct DataIndexToNodeIndex();

impl Snippet for DataIndexToNodeIndex {
    fn stack_diff() -> isize {
        // Pops a U32s<2> and pushes a U32s<2>
        0
    }

    fn entrypoint() -> &'static str {
        "data_index_to_node_index"
    }

    fn function_body(library: &mut Library) -> String {
        let entrypoint = Self::entrypoint();
        let non_leaf_nodes_left = library.import::<MmrNonLeafNodesLeftOld>();
        let incr = library.import::<U32s2Incr>();
        let add_u64 = library.import::<AddU64>();
        format!("
                // BEFORE: _ leaf_index_hi leaf_index_lo
                // AFTER: _ node_index_hi node_index_lo
                {entrypoint}:
                    dup1 dup1
                    // stack: _ leaf_index_hi leaf_index_lo leaf_index_hi leaf_index_lo

                    call {non_leaf_nodes_left}
                    // stack: _ leaf_index_hi leaf_index_lo non_leaf_nodes_left_hi non_leaf_nodes_left_lo

                    call {add_u64}
                    call {incr}
                    // stack: _ node_index_hi node_index_lo

                    return
                    ")
    }

    fn rust_shadowing(
        stack: &mut Vec<BFieldElement>,
        _std_in: Vec<BFieldElement>,
        _secret_in: Vec<BFieldElement>,
        _memory: &mut HashMap<BFieldElement, BFieldElement>,
    ) {
        let data_index_lo: u32 = stack.pop().unwrap().try_into().unwrap();
        let data_index_hi: u32 = stack.pop().unwrap().try_into().unwrap();
        let data_index: u64 = (data_index_hi as u64) * (1u64 << 32) + data_index_lo as u64;

        let node_index: u64 = mmr::shared::data_index_to_node_index(data_index as u128) as u64;
        stack.push(BFieldElement::new(node_index >> 32));
        stack.push(BFieldElement::new(node_index & 0xFFFFFFFFu32 as u64));
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
    fn data_index_to_node_index_simple_test() {
        let mut expected = get_init_tvm_stack();
        expected.push(BFieldElement::new(0));
        expected.push(BFieldElement::new(1));
        prop_data_index_to_node_index(0, Some(&expected));

        expected = get_init_tvm_stack();
        expected.push(BFieldElement::new(0));
        expected.push(BFieldElement::new(2));
        prop_data_index_to_node_index(1, Some(&expected));

        expected = get_init_tvm_stack();
        expected.push(BFieldElement::new(0));
        expected.push(BFieldElement::new(4));
        prop_data_index_to_node_index(2, Some(&expected));

        expected = get_init_tvm_stack();
        expected.push(BFieldElement::new(0));
        expected.push(BFieldElement::new(5));
        prop_data_index_to_node_index(3, Some(&expected));

        expected = get_init_tvm_stack();
        expected.push(BFieldElement::new(0));
        expected.push(BFieldElement::new(8));
        prop_data_index_to_node_index(4, Some(&expected));

        expected = get_init_tvm_stack();
        expected.push(BFieldElement::new(0));
        expected.push(BFieldElement::new(9));
        prop_data_index_to_node_index(5, Some(&expected));

        expected = get_init_tvm_stack();
        expected.push(BFieldElement::new(0));
        expected.push(BFieldElement::new(11));
        prop_data_index_to_node_index(6, Some(&expected));

        expected = get_init_tvm_stack();
        expected.push(BFieldElement::new(0));
        expected.push(BFieldElement::new(12));
        prop_data_index_to_node_index(7, Some(&expected));

        expected = get_init_tvm_stack();
        expected.push(BFieldElement::new(0));
        expected.push(BFieldElement::new(16));
        prop_data_index_to_node_index(8, Some(&expected));

        expected = get_init_tvm_stack();
        expected.push(BFieldElement::new(0));
        expected.push(BFieldElement::new(17));
        prop_data_index_to_node_index(9, Some(&expected));

        expected = get_init_tvm_stack();
        expected.push(BFieldElement::new(0));
        expected.push(BFieldElement::new(19));
        prop_data_index_to_node_index(10, Some(&expected));

        expected = get_init_tvm_stack();
        expected.push(BFieldElement::new(0));
        expected.push(BFieldElement::new(20));
        prop_data_index_to_node_index(11, Some(&expected));

        expected = get_init_tvm_stack();
        expected.push(BFieldElement::new(0));
        expected.push(BFieldElement::new(23));
        prop_data_index_to_node_index(12, Some(&expected));

        expected = get_init_tvm_stack();
        expected.push(BFieldElement::new(0));
        expected.push(BFieldElement::new(24));
        prop_data_index_to_node_index(13, Some(&expected));

        expected = get_init_tvm_stack();
        expected.push(BFieldElement::new(0));
        expected.push(BFieldElement::new(199_999_989));
        prop_data_index_to_node_index(100_000_000, Some(&expected));

        expected = get_init_tvm_stack();
        expected.push(BFieldElement::new(1));
        expected.push(BFieldElement::new(4_294_967_263));
        prop_data_index_to_node_index(u32::MAX as u64, Some(&expected));

        expected = get_init_tvm_stack();
        expected.push(BFieldElement::new(16777216));
        expected.push(BFieldElement::new(0));
        prop_data_index_to_node_index(1u64 << 55, Some(&expected));

        expected = get_init_tvm_stack();
        expected.push(BFieldElement::new(2147483648));
        expected.push(BFieldElement::new(0));
        prop_data_index_to_node_index(1u64 << 62, Some(&expected));

        expected = get_init_tvm_stack();
        expected.push(BFieldElement::new(2147483648));
        expected.push(BFieldElement::new(38));
        prop_data_index_to_node_index((1u64 << 62) + 20, Some(&expected));

        expected = get_init_tvm_stack();
        expected.push(BFieldElement::new(2147483648));
        expected.push(BFieldElement::new(797));
        prop_data_index_to_node_index((1u64 << 62) + 400, Some(&expected));

        expected = get_init_tvm_stack();
        expected.push(BFieldElement::new(2147483647));
        expected.push(BFieldElement::new(4294857690));
        prop_data_index_to_node_index((1u64 << 62) - 54778, Some(&expected));

        println!("2^32");
        prop_data_index_to_node_index((1u64 << 32) - 1, None);
        prop_data_index_to_node_index(1u64 << 32, None);
        prop_data_index_to_node_index((1u64 << 32) + 1, None);

        println!("2^33");
        prop_data_index_to_node_index((1u64 << 33) - 1, None);
        prop_data_index_to_node_index(1u64 << 33, None);
        prop_data_index_to_node_index((1u64 << 33) + 1, None);
    }

    #[test]
    fn data_index_to_node_index_pbt() {
        let mut rng = thread_rng();
        for _ in 0..20 {
            let rand = rng.next_u64() / 2;
            prop_data_index_to_node_index(rand, None);
        }
    }

    fn prop_data_index_to_node_index(data_index: u64, expected: Option<&[BFieldElement]>) {
        let mut init_stack = get_init_tvm_stack();
        let value_as_u32_2 = U32s::new([
            (data_index & 0xFFFFFFFFu32 as u64) as u32,
            (data_index >> 32) as u32,
        ]);
        for elem in value_as_u32_2.to_sequence().into_iter().rev() {
            init_stack.push(elem);
        }

        let _execution_result = rust_tasm_equivalence_prop::<DataIndexToNodeIndex>(
            &init_stack,
            &[],
            &[],
            &mut HashMap::default(),
            0,
            expected,
        );
    }
}
