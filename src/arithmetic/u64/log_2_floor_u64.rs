use std::collections::HashMap;
use twenty_first::shared_math::b_field_element::BFieldElement;
use twenty_first::shared_math::other::log_2_floor;

use crate::library::Library;
use crate::snippet::Snippet;

pub struct Log2FloorU64();

impl Snippet for Log2FloorU64 {
    fn stack_diff() -> isize {
        -1
    }

    fn entrypoint() -> &'static str {
        "log_2_floor_u64"
    }

    fn function_body(_library: &mut Library) -> String {
        let entrypoint = Self::entrypoint();

        // assumes that top of stack is a valid u32s<2>
        // BEFORE: _ value_hi value_lo
        // AFTER: _ log2_floor(value)
        format!(
            "
                // BEFORE: _ value_hi value_lo
                // AFTER: _ log2_floor(value)
                {entrypoint}:
                    swap1
                    push 1
                    dup1
                    // stack: _ value_lo value_hi 1 value_hi

                    skiz call {entrypoint}_then
                    skiz call {entrypoint}_else
                    // stack: _ log2_floor(value)

                    return

                {entrypoint}_then:
                    // value_hi != 0
                    // stack: // stack: _ value_lo value_hi 1
                    pop
                    swap1
                    pop
                    // stack: _ value_hi

                    log_2_floor
                    push 32
                    add
                    // stack: _ (log2_floor(value_hi) + 32)

                    push 0
                    // stack: _ (log2_floor(value_hi) + 32) 0

                    return

                {entrypoint}_else:
                    // value_hi == 0
                    // stack: _ value_lo value_hi
                    pop
                    log_2_floor
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
        // Get input value as a u64
        let lo: u32 = stack.pop().unwrap().try_into().unwrap();
        let hi: u32 = stack.pop().unwrap().try_into().unwrap();
        let value_u64: u64 = lo as u64 + (1 << 32) * (hi as u64);

        let log_2_floor = log_2_floor(value_u64 as u128);
        stack.push(BFieldElement::new(log_2_floor));
    }
}

#[cfg(test)]
mod tests {
    use twenty_first::{amount::u32s::U32s, util_types::algebraic_hasher::Hashable};

    use crate::get_init_tvm_stack;
    use crate::test_helpers::rust_tasm_equivalence_prop;

    use super::*;

    #[test]
    fn u32s_2_log2_floor() {
        let mut expected = get_init_tvm_stack();
        expected.push(BFieldElement::new(0));
        prop_log2_floor_u32s_2(1, &expected);

        expected = get_init_tvm_stack();
        expected.push(BFieldElement::new(1));
        prop_log2_floor_u32s_2(2, &expected);
        prop_log2_floor_u32s_2(3, &expected);

        expected = get_init_tvm_stack();
        expected.push(BFieldElement::new(2));
        prop_log2_floor_u32s_2(4, &expected);
        prop_log2_floor_u32s_2(5, &expected);
        prop_log2_floor_u32s_2(6, &expected);
        prop_log2_floor_u32s_2(7, &expected);

        expected = get_init_tvm_stack();
        expected.push(BFieldElement::new(31));
        prop_log2_floor_u32s_2(u32::MAX as u64 - 20000, &expected);
        prop_log2_floor_u32s_2(u32::MAX as u64 - 1, &expected);
        prop_log2_floor_u32s_2(u32::MAX as u64, &expected);

        expected = get_init_tvm_stack();
        expected.push(BFieldElement::new(32));
        prop_log2_floor_u32s_2(u32::MAX as u64 + 1, &expected);
        prop_log2_floor_u32s_2(u32::MAX as u64 + 2, &expected);
        prop_log2_floor_u32s_2(u32::MAX as u64 + 800, &expected);
        prop_log2_floor_u32s_2(u32::MAX as u64 + u32::MAX as u64, &expected);
        prop_log2_floor_u32s_2(u32::MAX as u64 + u32::MAX as u64 + 1, &expected);

        for i in 0..64 {
            expected = get_init_tvm_stack();
            expected.push(BFieldElement::new(i));
            prop_log2_floor_u32s_2(1 << i, &expected);
            if i > 0 {
                prop_log2_floor_u32s_2((1 << i) + 1, &expected);
            }
            if i > 1 {
                prop_log2_floor_u32s_2((1 << i) + 2, &expected);
                prop_log2_floor_u32s_2((1 << i) + 3, &expected);

                expected = get_init_tvm_stack();
                expected.push(BFieldElement::new(i - 1));
                prop_log2_floor_u32s_2((1 << i) - 1, &expected);
            }
        }
    }

    fn prop_log2_floor_u32s_2(value: u64, expected: &[BFieldElement]) {
        let mut init_stack = get_init_tvm_stack();
        let value_as_u32_2 =
            U32s::new([(value & 0xFFFFFFFFu32 as u64) as u32, (value >> 32) as u32]);
        for elem in value_as_u32_2.to_sequence().into_iter().rev() {
            init_stack.push(elem);
        }

        let _execution_result = rust_tasm_equivalence_prop::<Log2FloorU64>(
            &init_stack,
            &[],
            &[],
            &mut HashMap::default(),
            0,
            Some(expected),
        );
    }
}
