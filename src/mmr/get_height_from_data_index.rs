use std::collections::HashMap;

use twenty_first::shared_math::b_field_element::BFieldElement;
use twenty_first::shared_math::other::log_2_floor;

use crate::arithmetic::u64::incr_u64::IncrU64;
use crate::arithmetic::u64::log2_floor_u64::Log2FloorU64;
use crate::snippet::Snippet;

pub struct GetHeightFromDataIndex;

impl Snippet for GetHeightFromDataIndex {
    // Pops `data_index` from stack (U32s<2>). Returns height in the form of one u32.
    fn stack_diff() -> isize {
        -1
    }

    fn entrypoint() -> &'static str {
        "get_height_from_data_index"
    }

    fn function_body(library: &mut crate::library::Library) -> String {
        let entrypoint = Self::entrypoint();
        let incr_u64 = library.import::<IncrU64>();
        let log2_floor_u64 = library.import::<Log2FloorU64>();
        format!(
            "
            // Return the height of the MMR if this data index was the last leaf inserted
            // Before: _ data_index_hi data_index_lo
            // After: _ height
            {entrypoint}:
                call {incr_u64}
                call {log2_floor_u64}
                return"
        )
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
        let height: u32 = log_2_floor(data_index as u128 + 1) as u32;
        stack.push(BFieldElement::new(height as u64));
    }
}

#[cfg(test)]
mod tests {
    use twenty_first::amount::u32s::U32s;
    use twenty_first::util_types::algebraic_hasher::Hashable;

    use crate::get_init_tvm_stack;
    use crate::test_helpers::rust_tasm_equivalence_prop;

    use super::*;

    #[test]
    fn get_height_from_data_index_test_simple() {
        let mut expected = get_init_tvm_stack();
        expected.push(BFieldElement::new(0));
        prop_get_height_from_data_index(0, &expected);

        expected = get_init_tvm_stack();
        expected.push(BFieldElement::new(1));
        prop_get_height_from_data_index(1, &expected);
        prop_get_height_from_data_index(2, &expected);

        expected = get_init_tvm_stack();
        expected.push(BFieldElement::new(2));
        prop_get_height_from_data_index(3, &expected);
        prop_get_height_from_data_index(4, &expected);
        prop_get_height_from_data_index(5, &expected);
        prop_get_height_from_data_index(6, &expected);

        expected = get_init_tvm_stack();
        expected.push(BFieldElement::new(3));
        prop_get_height_from_data_index(7, &expected);
        prop_get_height_from_data_index(8, &expected);
        prop_get_height_from_data_index(9, &expected);
        prop_get_height_from_data_index(10, &expected);
        prop_get_height_from_data_index(11, &expected);
        prop_get_height_from_data_index(12, &expected);
        prop_get_height_from_data_index(13, &expected);
        prop_get_height_from_data_index(14, &expected);

        expected = get_init_tvm_stack();
        expected.push(BFieldElement::new(4));
        prop_get_height_from_data_index(15, &expected);
        prop_get_height_from_data_index(16, &expected);
        prop_get_height_from_data_index(17, &expected);
        prop_get_height_from_data_index(18, &expected);
        prop_get_height_from_data_index(19, &expected);
        prop_get_height_from_data_index(20, &expected);
        prop_get_height_from_data_index(21, &expected);
        prop_get_height_from_data_index(22, &expected);

        expected = get_init_tvm_stack();
        expected.push(BFieldElement::new(31));
        prop_get_height_from_data_index(u32::MAX as u64 - 2, &expected);
        prop_get_height_from_data_index(u32::MAX as u64 - 1, &expected);

        expected = get_init_tvm_stack();
        expected.push(BFieldElement::new(32));
        prop_get_height_from_data_index(u32::MAX as u64, &expected);
        prop_get_height_from_data_index(u32::MAX as u64 + 1, &expected);
        prop_get_height_from_data_index(u32::MAX as u64 + 2, &expected);

        expected = get_init_tvm_stack();
        expected.push(BFieldElement::new(44));
        prop_get_height_from_data_index((1u64 << 45) - 2, &expected);

        expected = get_init_tvm_stack();
        expected.push(BFieldElement::new(45));
        prop_get_height_from_data_index((1u64 << 45) - 1, &expected);
        prop_get_height_from_data_index(1u64 << 45, &expected);
        prop_get_height_from_data_index((1u64 << 45) + 1, &expected);
        prop_get_height_from_data_index((1u64 << 45) + (1 << 40), &expected);

        expected = get_init_tvm_stack();
        expected.push(BFieldElement::new(63));
        prop_get_height_from_data_index((1u64 << 63) - 1, &expected);
        prop_get_height_from_data_index(1u64 << 63, &expected);
        prop_get_height_from_data_index((1u64 << 63) + 1, &expected);
        prop_get_height_from_data_index((1u64 << 63) + (1 << 40), &expected);
    }

    fn prop_get_height_from_data_index(data_index: u64, expected: &[BFieldElement]) {
        let mut init_stack = get_init_tvm_stack();
        let data_index_as_u32_2 = U32s::new([
            (data_index & 0xFFFFFFFFu32 as u64) as u32,
            (data_index >> 32) as u32,
        ]);
        for elem in data_index_as_u32_2.to_sequence().into_iter().rev() {
            init_stack.push(elem);
        }

        let _execution_result = rust_tasm_equivalence_prop::<GetHeightFromDataIndex>(
            &init_stack,
            &[],
            &[],
            &mut HashMap::default(),
            0,
            Some(expected),
        );
    }
}
