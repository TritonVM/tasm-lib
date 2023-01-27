use std::collections::HashMap;

use rand::{thread_rng, Rng};
use twenty_first::shared_math::b_field_element::BFieldElement;
use twenty_first::shared_math::other::log_2_floor;

use crate::arithmetic::u64::incr_u64::IncrU64;
use crate::arithmetic::u64::log_2_floor_u64::Log2FloorU64;
use crate::library::Library;
use crate::snippet::{NewSnippet, Snippet};
use crate::{get_init_tvm_stack, ExecutionState};

pub struct GetHeightFromDataIndex;

impl NewSnippet for GetHeightFromDataIndex {
    fn inputs() -> Vec<&'static str> {
        vec!["leaf_index_hi", "leaf_index_lo"]
    }

    fn outputs() -> Vec<&'static str> {
        vec!["height"]
    }

    fn crash_conditions() -> Vec<&'static str> {
        vec![]
    }

    fn gen_input_states() -> Vec<crate::ExecutionState> {
        let mut ret: Vec<ExecutionState> = vec![];
        for _ in 0..40 {
            let mut stack = get_init_tvm_stack();
            let leaf_index = thread_rng().gen_range(0..u64::MAX / 2);
            let leaf_index_hi = BFieldElement::new(leaf_index >> 32);
            let leaf_index_lo = BFieldElement::new(leaf_index & u32::MAX as u64);
            stack.push(leaf_index_hi);
            stack.push(leaf_index_lo);
            ret.push(ExecutionState::with_stack(stack));
        }

        ret
    }
}

impl Snippet for GetHeightFromDataIndex {
    // Pops `leaf_index` from stack (U32s<2>). Returns height in the form of one u32.
    fn stack_diff() -> isize {
        -1
    }

    fn entrypoint() -> &'static str {
        "get_height_from_leaf_index"
    }

    fn function_body(library: &mut Library) -> String {
        let entrypoint = Self::entrypoint();
        let incr_u64 = library.import::<IncrU64>();
        let log_2_floor_u64 = library.import::<Log2FloorU64>();
        format!(
            "
            // Return the height of the MMR if this data index was the last leaf inserted
            // Before: _ leaf_index_hi leaf_index_lo
            // After: _ height
            {entrypoint}:
                call {incr_u64}
                call {log_2_floor_u64}
                return"
        )
    }

    fn rust_shadowing(
        stack: &mut Vec<BFieldElement>,
        _std_in: Vec<BFieldElement>,
        _secret_in: Vec<BFieldElement>,
        _memory: &mut HashMap<BFieldElement, BFieldElement>,
    ) {
        let leaf_index_lo: u32 = stack.pop().unwrap().try_into().unwrap();
        let leaf_index_hi: u32 = stack.pop().unwrap().try_into().unwrap();
        let leaf_index: u64 = (leaf_index_hi as u64) * (1u64 << 32) + leaf_index_lo as u64;
        let height: u32 = log_2_floor(leaf_index as u128 + 1) as u32;
        stack.push(BFieldElement::new(height as u64));
    }
}

#[cfg(test)]
mod tests {
    use twenty_first::amount::u32s::U32s;
    use twenty_first::util_types::algebraic_hasher::Hashable;

    use crate::get_init_tvm_stack;
    use crate::snippet_bencher::bench_and_write;
    use crate::test_helpers::{rust_tasm_equivalence_prop, rust_tasm_equivalence_prop_new};

    use super::*;

    #[test]
    fn get_height_from_data_index_test() {
        rust_tasm_equivalence_prop_new::<GetHeightFromDataIndex>();
    }

    #[test]
    fn get_height_from_data_index_benchmark() {
        bench_and_write::<GetHeightFromDataIndex>();
    }

    #[test]
    fn get_height_from_leaf_index_test_simple() {
        let mut expected = get_init_tvm_stack();
        expected.push(BFieldElement::new(0));
        prop_get_height_from_leaf_index(0, &expected);

        expected = get_init_tvm_stack();
        expected.push(BFieldElement::new(1));
        prop_get_height_from_leaf_index(1, &expected);
        prop_get_height_from_leaf_index(2, &expected);

        expected = get_init_tvm_stack();
        expected.push(BFieldElement::new(2));
        prop_get_height_from_leaf_index(3, &expected);
        prop_get_height_from_leaf_index(4, &expected);
        prop_get_height_from_leaf_index(5, &expected);
        prop_get_height_from_leaf_index(6, &expected);

        expected = get_init_tvm_stack();
        expected.push(BFieldElement::new(3));
        prop_get_height_from_leaf_index(7, &expected);
        prop_get_height_from_leaf_index(8, &expected);
        prop_get_height_from_leaf_index(9, &expected);
        prop_get_height_from_leaf_index(10, &expected);
        prop_get_height_from_leaf_index(11, &expected);
        prop_get_height_from_leaf_index(12, &expected);
        prop_get_height_from_leaf_index(13, &expected);
        prop_get_height_from_leaf_index(14, &expected);

        expected = get_init_tvm_stack();
        expected.push(BFieldElement::new(4));
        prop_get_height_from_leaf_index(15, &expected);
        prop_get_height_from_leaf_index(16, &expected);
        prop_get_height_from_leaf_index(17, &expected);
        prop_get_height_from_leaf_index(18, &expected);
        prop_get_height_from_leaf_index(19, &expected);
        prop_get_height_from_leaf_index(20, &expected);
        prop_get_height_from_leaf_index(21, &expected);
        prop_get_height_from_leaf_index(22, &expected);

        expected = get_init_tvm_stack();
        expected.push(BFieldElement::new(31));
        prop_get_height_from_leaf_index(u32::MAX as u64 - 2, &expected);
        prop_get_height_from_leaf_index(u32::MAX as u64 - 1, &expected);

        expected = get_init_tvm_stack();
        expected.push(BFieldElement::new(32));
        prop_get_height_from_leaf_index(u32::MAX as u64, &expected);
        prop_get_height_from_leaf_index(u32::MAX as u64 + 1, &expected);
        prop_get_height_from_leaf_index(u32::MAX as u64 + 2, &expected);

        expected = get_init_tvm_stack();
        expected.push(BFieldElement::new(44));
        prop_get_height_from_leaf_index((1u64 << 45) - 2, &expected);

        expected = get_init_tvm_stack();
        expected.push(BFieldElement::new(45));
        prop_get_height_from_leaf_index((1u64 << 45) - 1, &expected);
        prop_get_height_from_leaf_index(1u64 << 45, &expected);
        prop_get_height_from_leaf_index((1u64 << 45) + 1, &expected);
        prop_get_height_from_leaf_index((1u64 << 45) + (1 << 40), &expected);

        expected = get_init_tvm_stack();
        expected.push(BFieldElement::new(63));
        prop_get_height_from_leaf_index((1u64 << 63) - 1, &expected);
        prop_get_height_from_leaf_index(1u64 << 63, &expected);
        prop_get_height_from_leaf_index((1u64 << 63) + 1, &expected);
        prop_get_height_from_leaf_index((1u64 << 63) + (1 << 40), &expected);
    }

    fn prop_get_height_from_leaf_index(leaf_index: u64, expected: &[BFieldElement]) {
        let mut init_stack = get_init_tvm_stack();
        let leaf_index_as_u32_2 = U32s::new([
            (leaf_index & 0xFFFFFFFFu32 as u64) as u32,
            (leaf_index >> 32) as u32,
        ]);
        for elem in leaf_index_as_u32_2.to_sequence().into_iter().rev() {
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
