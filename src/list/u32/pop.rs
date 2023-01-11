use num::One;
use twenty_first::shared_math::b_field_element::BFieldElement;

use crate::snippet::Snippet;

pub struct Pop<const N: usize>;

impl<const N: usize> Snippet for Pop<N> {
    fn stack_diff() -> isize {
        assert!(N < 17, "Max element size supported for list is 16");
        N as isize - 1
    }

    fn entrypoint() -> &'static str {
        assert!(N < 17, "Max element size supported for list is 16");
        "pop_u32"
    }

    /// Pop last element from list. Does *not* actually delete the last
    /// element but instead leaves it in memory.
    fn function_body(_library: &mut crate::library::Library) -> String {
        let entry_point = Self::entrypoint();

        let mut code_to_read_elements = String::default();
        // Start and end at loop: Stack: _  [elems], address_for_last_unread_element
        for i in 0..N {
            code_to_read_elements.push_str("push 0\n");
            code_to_read_elements.push_str("read_mem\n");
            // stack: _  address_for_last_unread_element, elem_{{N - 1 - i}}

            code_to_read_elements.push_str("swap1\n");
            // stack: _  [..., elem_{{N - 1 - i}}], address_for_last_unread_element
            if i != N - 1 {
                // Update offset for last unread element
                code_to_read_elements.push_str("push -1\n");
                code_to_read_elements.push_str("add\n");
            }
        }

        format!(
            // Before: _ *list
            // After: _ elem{{N - 1}}, elem{{N - 2}}, ..., elem{{0}}
            "{entry_point}:
                push 0
                read_mem
                // stack : _  *list, length

                // Assert that length is over 0
                dup0
                push 0
                eq
                push 0
                eq
                assert
                // stack : _  *list, length

                // Decrease length value by one and write back to memory
                push -1
                add
                write_mem
                // stack : _  *list, length - 1

                push {N}
                mul
                push {N}
                add
                // stack : _  *list, offset_for_last_element = (N * initial_length)

                add
                // stack : _  address_for_last_element

                {code_to_read_elements}
                // Stack: _  [elements], address_for_last_unread_element

                pop
                // Stack: _  [elements]

                return
            "
        )
    }

    fn rust_shadowing(
        stack: &mut Vec<BFieldElement>,
        _std_in: Vec<BFieldElement>,
        _secret_in: Vec<BFieldElement>,
        memory: &mut std::collections::HashMap<BFieldElement, BFieldElement>,
    ) {
        let list_address = stack.pop().unwrap();
        let initial_list_length = memory[&list_address];

        // Update length indicator
        memory.insert(list_address, initial_list_length - BFieldElement::one());

        let mut last_used_address =
            list_address + initial_list_length * BFieldElement::new(N as u64);

        for _ in 0..N {
            let elem = memory[&last_used_address];
            stack.push(elem);
            last_used_address -= BFieldElement::one();
        }
    }
}

#[cfg(test)]
mod tests_pop {
    use std::collections::HashMap;

    use itertools::Itertools;
    use num::Zero;
    use rand::{thread_rng, RngCore};
    use twenty_first::shared_math::b_field_element::BFieldElement;

    use crate::get_init_tvm_stack;
    use crate::test_helpers::rust_tasm_equivalence_prop;

    use super::*;

    #[test]
    #[should_panic]
    fn panic_if_pop_on_empty_list_1() {
        let list_address = BFieldElement::new(48);
        prop_pop::<1>(list_address, 0);
    }

    #[test]
    #[should_panic]
    fn panic_if_pop_on_empty_list_2() {
        let list_address = BFieldElement::new(48);
        prop_pop::<2>(list_address, 0);
    }

    #[test]
    #[should_panic]
    fn panic_if_pop_on_empty_list_3() {
        let list_address = BFieldElement::new(48);
        prop_pop::<3>(list_address, 0);
    }

    #[test]
    #[should_panic]
    fn panic_if_pop_on_empty_list_4() {
        let list_address = BFieldElement::new(48);
        prop_pop::<4>(list_address, 0);
    }

    #[test]
    fn list_u32_n_is_n_pop() {
        let list_address = BFieldElement::new(48);
        prop_pop::<1>(list_address, 24);
        prop_pop::<2>(list_address, 48);
        prop_pop::<3>(list_address, 3);
        prop_pop::<4>(list_address, 4);
        prop_pop::<5>(list_address, 20);
        prop_pop::<6>(list_address, 20);
        prop_pop::<7>(list_address, 20);
        prop_pop::<8>(list_address, 20);
        prop_pop::<9>(list_address, 20);
        prop_pop::<10>(list_address, 1);
        prop_pop::<11>(list_address, 33);
        prop_pop::<12>(list_address, 20);
        prop_pop::<13>(list_address, 20);
        prop_pop::<14>(list_address, 20);
        prop_pop::<15>(list_address, 20);
        prop_pop::<16>(list_address, 20);
    }

    fn prop_pop<const N: usize>(list_address: BFieldElement, init_list_length: u32) {
        let mut init_stack = get_init_tvm_stack();
        init_stack.push(list_address);

        let mut vm_memory = HashMap::default();

        // Insert length indicator of list, lives on offset = 0 from `list_address`
        vm_memory.insert(list_address, BFieldElement::new(init_list_length as u64));

        // Insert random values for the elements in the list
        let mut rng = thread_rng();
        let mut last_element: [BFieldElement; N] = [BFieldElement::zero(); N];
        let mut j = 1;
        for _ in 0..init_list_length {
            last_element = (0..N)
                .map(|_| BFieldElement::new(rng.next_u64()))
                .collect_vec()
                .try_into()
                .unwrap();
            for elem in last_element.iter() {
                vm_memory.insert(list_address + BFieldElement::new(j), *elem);
                j += 1;
            }
        }

        let mut expected_end_stack = get_init_tvm_stack();

        for i in 0..N {
            expected_end_stack.push(last_element[N - 1 - i]);
        }

        let _execution_result = rust_tasm_equivalence_prop::<Pop<N>>(
            &init_stack,
            &[],
            &[],
            &mut vm_memory,
            0,
            Some(&expected_end_stack),
        );

        // Verify that length is now indicated to be `init_list_length - 1`
        assert_eq!(
            BFieldElement::new(init_list_length as u64) - BFieldElement::one(),
            vm_memory[&list_address]
        );
    }
}
