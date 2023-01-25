use std::collections::HashMap;

use rand::RngCore;
use twenty_first::amount::u32s::U32s;
use twenty_first::shared_math::b_field_element::BFieldElement;

use crate::library::Library;
use crate::snippet::{NewSnippet, Snippet};
use crate::{get_init_tvm_stack, push_hashable, ExecutionState};

pub struct Div2U64;

impl NewSnippet for Div2U64 {
    fn inputs() -> Vec<&'static str> {
        vec!["value_hi", "value_lo"]
    }

    fn outputs() -> Vec<&'static str> {
        vec!["(value / 2)_hi", "(value / 2)_lo"]
    }

    fn crash_conditions() -> Vec<&'static str> {
        vec!["If value_hi is not a u32", "If value_lo is not a u32"]
    }

    fn gen_input_states() -> Vec<crate::ExecutionState> {
        let n: u64 = rand::thread_rng().next_u64();
        let n: U32s<2> = n.try_into().unwrap();
        let mut input_stack = get_init_tvm_stack();

        push_hashable(&mut input_stack, &n);

        vec![ExecutionState::with_stack(input_stack)]
    }
}

impl Snippet for Div2U64 {
    fn stack_diff() -> isize {
        // Pop the input argument (u64) and push the result (u64). Both input and output is two words.
        0
    }

    fn entrypoint() -> &'static str {
        "div2_u64"
    }

    fn function_body(_library: &mut Library) -> String {
        let entrypoint = Self::entrypoint();
        let two_pow_31 = (1u64 << 31).to_string();

        format!(
            "
                // BEFORE: _ value_hi value_lo
                // AFTER: _ (value / 2)_hi (value / 2)_lo
                {entrypoint}:
                    // Divide the lower number
                    [lsb]
                    pop
                    // stack: _ value_hi (value_lo / 2)

                    // Divide the upper number and carry its least significant bit into the lower number
                    swap1
                    // stack: _ (value_lo / 2) value_hi

                    [lsb]
                    // stack: _ (value_lo / 2) (value_hi / 2) (value_hi % 2)

                    push {two_pow_31}
                    mul
                    // stack: _ (value_lo / 2) (value_hi / 2) carry

                    swap1
                    swap2
                    // stack: _ (value_hi / 2) (value_lo / 2) carry

                    add
                    // stack: _ (value / 2)_hi (value / 2)_lo carry

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
        let value_lo: u32 = stack.pop().unwrap().try_into().unwrap();
        let value_hi: u32 = stack.pop().unwrap().try_into().unwrap();
        let value: u64 = ((value_hi as u64) << 32) + value_lo as u64;
        let result: u64 = value / 2;

        stack.push(BFieldElement::new(result >> 32));
        stack.push(BFieldElement::new(result & u32::MAX as u64));
    }
}

#[cfg(test)]
mod tests {
    use rand::{thread_rng, RngCore};
    use twenty_first::shared_math::b_field_element::BFieldElement;

    use crate::get_init_tvm_stack;
    use crate::test_helpers::{rust_tasm_equivalence_prop, rust_tasm_equivalence_prop_new};

    use super::*;

    #[test]
    fn div_2_test_new_snippet() {
        rust_tasm_equivalence_prop_new::<Div2U64>();
    }

    #[should_panic]
    #[test]
    fn lo_is_not_u32() {
        let mut init_stack = get_init_tvm_stack();
        init_stack.push(BFieldElement::new(16));
        init_stack.push(BFieldElement::new(u32::MAX as u64 + 1));

        let _execution_result = rust_tasm_equivalence_prop::<Div2U64>(
            &init_stack,
            &[],
            &[],
            &mut HashMap::default(),
            0,
            None,
        );
    }

    #[should_panic]
    #[test]
    fn hi_is_not_u32() {
        let mut init_stack = get_init_tvm_stack();
        init_stack.push(BFieldElement::new(u32::MAX as u64 + 1));
        init_stack.push(BFieldElement::new(16));

        let _execution_result = rust_tasm_equivalence_prop::<Div2U64>(
            &init_stack,
            &[],
            &[],
            &mut HashMap::default(),
            0,
            None,
        );
    }

    #[test]
    fn div_2_test() {
        prop_div_2(0);
        prop_div_2(1);
        prop_div_2(2);
        prop_div_2(3);
        prop_div_2(4);
        prop_div_2(5);
        prop_div_2(6);
        prop_div_2(7);
        prop_div_2(8);
        prop_div_2(1 << 32);
        prop_div_2((1 << 32) + 1);
        prop_div_2((1 << 32) + 2);
        prop_div_2((1 << 32) + 3);
        prop_div_2((1 << 32) + 4);
        prop_div_2((1 << 63) + 4);
        prop_div_2((1 << 63) + 4);
        prop_div_2((1 << 63) + (1 << 31));
        prop_div_2((1 << 63) + (1 << 33) + (1 << 32) + (1 << 31));

        let mut rng = thread_rng();
        for _ in 0..100 {
            let value = rng.next_u64();
            prop_div_2(value);
        }
    }

    fn prop_div_2(value: u64) {
        let mut init_stack = get_init_tvm_stack();
        init_stack.push(BFieldElement::new(value >> 32));
        init_stack.push(BFieldElement::new(value & u32::MAX as u64));
        let mut expected_stack = get_init_tvm_stack();
        let res = value / 2;
        expected_stack.push(BFieldElement::new(res >> 32));
        expected_stack.push(BFieldElement::new(res & u32::MAX as u64));

        let _execution_result = rust_tasm_equivalence_prop::<Div2U64>(
            &init_stack,
            &[],
            &[],
            &mut HashMap::default(),
            0,
            Some(&expected_stack),
        );
    }
}
