use std::collections::HashMap;

use twenty_first::shared_math::b_field_element::BFieldElement;

use crate::library::Library;
use crate::snippet_trait::Snippet;

pub struct SetLength;

impl Snippet for SetLength {
    fn stack_diff() -> isize {
        // pops list_length but leaves list_pointer on stack
        -1
    }

    fn entrypoint() -> &'static str {
        "list_u32_set_length"
    }

    fn function_body(_library: &mut Library) -> String {
        let entry_point = Self::entrypoint();
        // It is assumed that the new length is a valid u32 value
        format!(
            "
                // BEFORE: _ *list list_length
                // AFTER: _ *list
                {entry_point}:
                    write_mem
                    // Stack: *list list_length

                    pop
                    // Stack: *list

                    return
                "
        )
    }

    fn rust_shadowing(
        stack: &mut Vec<BFieldElement>,
        _std_in: Vec<BFieldElement>,
        _secret_in: Vec<BFieldElement>,
        memory: &mut HashMap<BFieldElement, BFieldElement>,
    ) {
        let new_length = stack.pop().unwrap();
        let list_address = stack.pop().unwrap();

        memory.insert(list_address, new_length);

        stack.push(list_address);
    }
}

#[cfg(test)]
mod tests_set_length {
    use twenty_first::shared_math::b_field_element::BFieldElement;

    use crate::{get_init_tvm_stack, snippet_trait::rust_tasm_equivalence_prop};

    use super::*;

    #[test]
    fn list_u32_n_is_five_push() {
        let list_address = BFieldElement::new(558);
        prop_set_length(list_address, 2313, 14);
        prop_set_length(list_address, 14, 0);
        prop_set_length(list_address, 0, 0);
    }

    fn prop_set_length(list_address: BFieldElement, init_list_length: u32, new_list_length: u32) {
        let expected_end_stack = vec![get_init_tvm_stack(), vec![list_address]].concat();
        let mut init_stack = get_init_tvm_stack();
        init_stack.push(list_address);
        init_stack.push(BFieldElement::new(new_list_length as u64));

        let mut vm_memory = HashMap::default();

        // Insert length indicator of list, lives on offset = 0 from `list_address`
        vm_memory.insert(list_address, BFieldElement::new(init_list_length as u64));

        let _execution_result = rust_tasm_equivalence_prop::<SetLength>(
            &init_stack,
            &[],
            &[],
            &mut vm_memory,
            0,
            Some(&expected_end_stack),
        );

        // Verify that length indicator has been updated
        assert_eq!(
            BFieldElement::new(new_list_length as u64),
            vm_memory[&list_address]
        );
    }
}
