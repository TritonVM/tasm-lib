use std::collections::HashMap;

use twenty_first::shared_math::b_field_element::BFieldElement;

use crate::snippet_trait::Snippet;

// Called "Long" because this logic can be shortened
pub struct LengthLong;

impl Snippet for LengthLong {
    fn stack_diff() -> isize {
        // Consumes a memory address and returns a length in the form of a u32
        0
    }

    fn entrypoint() -> &'static str {
        "list_u32_length_long"
    }

    fn function_body(_library: &mut crate::library::Library) -> String {
        let entry_point = Self::entrypoint();
        // Before: _ *list
        // After: _ list_length_u32
        format!(
            "
            {entry_point}:
                push 0
                read_mem
                swap1
                pop
                return
                "
        )
    }

    fn rust_shadowing(
        stack: &mut Vec<twenty_first::shared_math::b_field_element::BFieldElement>,
        _std_in: Vec<twenty_first::shared_math::b_field_element::BFieldElement>,
        _secret_in: Vec<twenty_first::shared_math::b_field_element::BFieldElement>,
        memory: &mut HashMap<BFieldElement, BFieldElement>,
    ) {
        // Find the list in memory and push its length to the top of the stack
        let list_address = stack.pop().unwrap();
        let list_length = memory[&list_address];
        stack.push(list_length);
    }
}

// Called "Short" because it's efficient code
pub struct LengthShort;

impl Snippet for LengthShort {
    fn stack_diff() -> isize {
        // Adds the length of a vector in the form of a u32 to the top of the stack
        1
    }

    fn entrypoint() -> &'static str {
        "list_u32_length_short"
    }

    fn function_body(_library: &mut crate::library::Library) -> String {
        let entry_point = Self::entrypoint();
        // Before: _ *list
        // After: _ *list list_length_u32
        format!(
            "
            {entry_point}:
                push 0
                read_mem
                return
                "
        )
    }

    fn rust_shadowing(
        stack: &mut Vec<twenty_first::shared_math::b_field_element::BFieldElement>,
        _std_in: Vec<twenty_first::shared_math::b_field_element::BFieldElement>,
        _secret_in: Vec<twenty_first::shared_math::b_field_element::BFieldElement>,
        memory: &mut HashMap<BFieldElement, BFieldElement>,
    ) {
        // Find the list in memory and push its length to the top of the stack
        let list_address = *stack.last().as_ref().unwrap();
        let list_length = memory[list_address];
        stack.push(list_length);
    }
}

#[cfg(test)]
mod tests_long {
    use num::One;
    use rand::{thread_rng, RngCore};
    use twenty_first::shared_math::b_field_element::BFieldElement;

    use crate::{get_init_tvm_stack, snippet_trait::rust_tasm_equivalence_prop};

    use super::*;

    #[test]
    fn list_u32_simple_long() {
        let expected_end_stack = vec![get_init_tvm_stack(), vec![BFieldElement::new(42)]].concat();
        prop_length_long(BFieldElement::one(), 42, Some(&expected_end_stack));

        let expected_end_stack = vec![get_init_tvm_stack(), vec![BFieldElement::new(588)]].concat();
        prop_length_long(BFieldElement::one(), 588, Some(&expected_end_stack));
    }

    // Note that the *actual list* of length `list_length` is *actually constructed in the VM in this test. So you may not
    // want to exaggerate that number.
    fn prop_length_long(
        list_address: BFieldElement,
        list_length: u32,
        expected: Option<&[BFieldElement]>,
    ) {
        let mut init_stack = get_init_tvm_stack();
        init_stack.push(list_address);

        let mut init_memory = HashMap::default();

        // Insert length indicator of list, lives on offset = 0 from `list_address`
        init_memory.insert(list_address, BFieldElement::new(list_length as u64));

        // Insert random values for the elements in the list
        let mut rng = thread_rng();
        for i in 0..list_length {
            init_memory.insert(
                list_address + BFieldElement::new((i + 1) as u64),
                BFieldElement::new(rng.next_u64()),
            );
        }

        let _execution_result = rust_tasm_equivalence_prop::<LengthLong>(
            &init_stack,
            &[],
            &[],
            &mut init_memory,
            0,
            expected,
        );
    }
}

#[cfg(test)]
mod tests_short {
    use rand::{thread_rng, RngCore};
    use twenty_first::shared_math::b_field_element::BFieldElement;

    use crate::{get_init_tvm_stack, snippet_trait::rust_tasm_equivalence_prop};

    use super::*;

    #[test]
    fn list_u32_simple_short() {
        let expected_end_stack = vec![
            get_init_tvm_stack(),
            vec![BFieldElement::new(48), BFieldElement::new(42)],
        ]
        .concat();
        prop_length_long(BFieldElement::new(48), 42, Some(&expected_end_stack));

        let expected_end_stack = vec![
            get_init_tvm_stack(),
            vec![BFieldElement::new(8888906), BFieldElement::new(588)],
        ]
        .concat();
        prop_length_long(BFieldElement::new(8888906), 588, Some(&expected_end_stack));
    }

    // Note that the *actual list* of length `list_length` is *actually constructed in the VM in this test. So you may not
    // want to exaggerate that number.
    fn prop_length_long(
        list_address: BFieldElement,
        list_length: u32,
        expected: Option<&[BFieldElement]>,
    ) {
        let mut init_stack = get_init_tvm_stack();
        init_stack.push(list_address);

        let mut init_memory = HashMap::default();

        // Insert length indicator of list, lives on offset = 0 from `list_address`
        init_memory.insert(list_address, BFieldElement::new(list_length as u64));

        // Insert random values for the elements in the list
        let mut rng = thread_rng();
        for i in 0..list_length {
            init_memory.insert(
                list_address + BFieldElement::new((i + 1) as u64),
                BFieldElement::new(rng.next_u64()),
            );
        }

        let _execution_result = rust_tasm_equivalence_prop::<LengthShort>(
            &init_stack,
            &[],
            &[],
            &mut init_memory,
            0,
            expected,
        );
    }
}
