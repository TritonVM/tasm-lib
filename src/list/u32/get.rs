use std::collections::HashMap;

use twenty_first::shared_math::b_field_element::BFieldElement;

use crate::library::Library;
use crate::rust_shadowing_helper_functions;
use crate::snippet_trait::Snippet;

pub struct Get<const N: usize>;

impl<const N: usize> Snippet for Get<N> {
    fn stack_diff() -> isize {
        assert!(N < 17, "Max element size supported for list is 16");

        // pops a pointer to the list and an index into the list, returns an element of length `N` words
        N as isize - 2
    }

    fn entrypoint() -> &'static str {
        assert!(N < 17, "Max element size supported for list is 16");

        "list_get_element"
    }

    fn function_body(_library: &mut Library) -> String {
        let entrypoint = Self::entrypoint();
        // Code to read an element from a list. No bounds-check.

        let mut code_to_read_elements = String::default();

        // Start and end at loop: Stack: _  [elems], address_of_next_element
        for i in 0..N {
            code_to_read_elements.push_str("push 0\n");
            code_to_read_elements.push_str("read_mem\n");
            // stack: _  address_for_last_unread_element, elem_{{N - 1 - i}}

            code_to_read_elements.push_str("swap1\n");
            // stack: _  [..., elem_{{N - 1 - i}}], address_for_last_unread_element
            if i != N - 1 {
                code_to_read_elements.push_str("push -1\n");
                code_to_read_elements.push_str("add\n");
            }
        }
        format!(
            "
            // BEFORE: _ *list index
            // After: _ elem{{N - 1}}, elem{{N - 2}}, ..., elem{{0}}
            {entrypoint}:
                push 1
                add
                push {N}
                mul
                // stack: _ *list (N * (index + 1))

                add
                // stack: _ (*list + N * index + 1)

                {code_to_read_elements}
                // stack: _ elem{{N - 1}}, elem{{N - 2}}, ..., elem{{0}} address

                pop
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
        let index: u32 = stack.pop().unwrap().try_into().unwrap();
        let list_pointer = stack.pop().unwrap();
        let element: [BFieldElement; N] =
            rust_shadowing_helper_functions::list_read(list_pointer, index as usize, memory);

        // elements are placed on stack as: `elem[N - 1] elem[N - 2] .. elem[0]`
        for i in (0..N).rev() {
            stack.push(element[i]);
        }
    }
}

#[cfg(test)]
mod get_element_tests {
    use std::collections::HashMap;

    use itertools::Itertools;
    use rand::{thread_rng, RngCore};
    use twenty_first::shared_math::b_field_element::BFieldElement;

    use crate::{get_init_tvm_stack, snippet_trait::rust_tasm_equivalence_prop};

    use super::*;

    #[test]
    fn get_simple_1() {
        let list_address = BFieldElement::new(48);
        for i in 0..10 {
            prop_get::<1>(list_address, i, 10);
        }
    }

    #[test]
    fn get_simple_2() {
        let list_address = BFieldElement::new(48);
        for i in 0..10 {
            prop_get::<2>(list_address, i, 10);
        }
    }

    #[test]
    fn get_simple_3() {
        let list_address = BFieldElement::new(48);
        for i in 0..10 {
            prop_get::<3>(list_address, i, 10);
        }
    }

    #[test]
    fn get_simple_15() {
        let list_address = BFieldElement::new(48);
        for i in 0..10 {
            prop_get::<15>(list_address, i, 10);
        }
    }

    fn prop_get<const N: usize>(list_pointer: BFieldElement, index: u32, list_length: u32) {
        let mut init_stack = get_init_tvm_stack();
        init_stack.push(list_pointer);
        init_stack.push(BFieldElement::new(index as u64));

        let mut memory = HashMap::default();

        // Insert length indicator of list, lives on offset = 0 from `list_address`
        memory.insert(list_pointer, BFieldElement::new(list_length as u64));

        // Insert random values for the elements in the list
        let mut rng = thread_rng();
        let mut j = 1;
        for _ in 0..list_length {
            let element: [BFieldElement; N] = (0..N)
                .map(|_| BFieldElement::new(rng.next_u64()))
                .collect_vec()
                .try_into()
                .unwrap();
            for elem in element.iter() {
                memory.insert(list_pointer + BFieldElement::new(j), *elem);
                j += 1;
            }
        }
        let targeted_element: [BFieldElement; N] =
            rust_shadowing_helper_functions::list_read(list_pointer, index as usize, &memory);

        let mut expected_end_stack = get_init_tvm_stack();

        for i in 0..N {
            expected_end_stack.push(targeted_element[N - 1 - i]);
        }

        let _execution_result = rust_tasm_equivalence_prop::<Get<N>>(
            &init_stack,
            &[],
            &[],
            &mut memory,
            0,
            Some(&expected_end_stack),
        );
    }
}
