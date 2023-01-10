use std::collections::HashMap;

use twenty_first::shared_math::b_field_element::BFieldElement;

use crate::library::Library;
use crate::rust_shadowing_helper_functions;
use crate::snippet_trait::Snippet;

pub struct Set<const N: usize>;

impl<const N: usize> Snippet for Set<N> {
    fn stack_diff() -> isize {
        assert!(N < 17, "Max element size supported for list is 16");

        -2 - N as isize
    }

    fn entrypoint() -> &'static str {
        assert!(N < 17, "Max element size supported for list is 16");
        "list_set_element"
    }

    fn function_body(_library: &mut Library) -> String {
        let entrypoint = Self::entrypoint();

        let mut write_elements_to_memory_code = String::default();
        for i in 0..N {
            write_elements_to_memory_code.push_str("swap1\n");
            write_elements_to_memory_code.push_str("write_mem\n");
            write_elements_to_memory_code.push_str("pop\n");
            if i != N - 1 {
                // Prepare for next write. Not needed for last iteration.
                write_elements_to_memory_code.push_str("push 1\n");
                write_elements_to_memory_code.push_str("add\n");
            }
        }

        format!(
            "
                // BEFORE: _ elem{{N - 1}}, elem{{N - 2}}, ..., elem{{0}} *list index
                // AFTER: _
                {entrypoint}:
                    push {N}
                    mul
                    push 1
                    add
                    add

                    // stack: _ elem{{N - 1}}, elem{{N - 2}}, ..., elem{{0}} *list + offset

                    {write_elements_to_memory_code}
                    // stack: _ *list + offset
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
        let mut element: [BFieldElement; N] = [BFieldElement::new(0); N];
        for i in 0..N {
            element[i] = stack.pop().unwrap();
        }
        rust_shadowing_helper_functions::list_set(list_pointer, index as usize, element, memory);
    }
}

#[cfg(test)]
mod list_set_tests {
    use rand::{thread_rng, RngCore};
    use twenty_first::shared_math::b_field_element::BFieldElement;

    use crate::{get_init_tvm_stack, snippet_trait::rust_tasm_equivalence_prop};

    use super::*;

    #[test]
    fn list_u32_n_is_one_set() {
        let list_address = BFieldElement::new(48);
        let insert_value = [BFieldElement::new(1337)];
        prop_set(list_address, 20, insert_value, 2);
    }

    #[test]
    fn list_u32_n_is_two_set() {
        let list_address = BFieldElement::new(1841);
        let push_value = [BFieldElement::new(133700), BFieldElement::new(32)];
        prop_set(list_address, 20, push_value, 0);
    }

    #[test]
    fn list_u32_n_is_five_set() {
        let list_address = BFieldElement::new(558);
        let push_value = [
            BFieldElement::new(133700),
            BFieldElement::new(32),
            BFieldElement::new(133700),
            BFieldElement::new(19990),
            BFieldElement::new(88888888),
        ];
        prop_set(list_address, 2313, push_value, 589);
    }

    #[test]
    fn list_u32_n_is_sixteen_set() {
        let list_address = BFieldElement::new(558);
        let push_value = [
            BFieldElement::new(133700),
            BFieldElement::new(32),
            BFieldElement::new(133700),
            BFieldElement::new(19990),
            BFieldElement::new(88888888),
            BFieldElement::new(1337001),
            BFieldElement::new(321),
            BFieldElement::new(1337001),
            BFieldElement::new(199901),
            BFieldElement::new(888888881),
            BFieldElement::new(1337002),
            BFieldElement::new(322),
            BFieldElement::new(1337002),
            BFieldElement::new(199902),
            BFieldElement::new(888888882),
            BFieldElement::new(888888883),
        ];
        prop_set(list_address, 2313, push_value, 589);
    }

    fn prop_set<const N: usize>(
        list_address: BFieldElement,
        init_list_length: u32,
        push_value: [BFieldElement; N],
        index: u32,
    ) {
        let expected_end_stack = vec![get_init_tvm_stack()].concat();
        let mut init_stack = get_init_tvm_stack();

        for i in 0..N {
            init_stack.push(push_value[N - 1 - i]);
        }
        init_stack.push(list_address);
        init_stack.push(BFieldElement::new(index as u64));

        let mut vm_memory = HashMap::default();

        // Insert length indicator of list, lives on offset = 0 from `list_address`
        vm_memory.insert(list_address, BFieldElement::new(init_list_length as u64));

        // Insert random values for the elements in the list
        let mut rng = thread_rng();
        for i in 0..init_list_length {
            vm_memory.insert(
                list_address + BFieldElement::new((i + 1) as u64),
                BFieldElement::new(rng.next_u64()),
            );
        }

        let _execution_result = rust_tasm_equivalence_prop::<Set<N>>(
            &init_stack,
            &[],
            &[],
            &mut vm_memory,
            0,
            Some(&expected_end_stack),
        );

        // Verify that length indicator is unchanged
        assert_eq!(
            BFieldElement::new((init_list_length) as u64),
            vm_memory[&list_address]
        );

        // verify that value was inserted at expected place
        for i in 0..N {
            assert_eq!(
                push_value[i],
                vm_memory[&BFieldElement::new(
                    list_address.value() + 1 + N as u64 * index as u64 + i as u64
                )]
            );
        }
    }
}
