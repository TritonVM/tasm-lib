use std::collections::HashMap;

use num::One;
use rand::{random, thread_rng, Rng};
use twenty_first::shared_math::b_field_element::BFieldElement;

use crate::library::Library;
use crate::rust_shadowing_helper_functions::unsafe_insert_random_list;
use crate::snippet::{DataType, Snippet};
use crate::{get_init_tvm_stack, ExecutionState};

#[derive(Clone)]
pub struct Pop<const N: usize>(pub DataType);

impl<const N: usize> Snippet for Pop<N> {
    fn inputs() -> Vec<&'static str> {
        vec!["*list"]
    }

    fn outputs() -> Vec<&'static str> {
        vec!["element"; N]
    }

    fn input_types(&self) -> Vec<crate::snippet::DataType> {
        vec![DataType::List(Box::new(self.0.clone()))]
    }

    fn output_types(&self) -> Vec<crate::snippet::DataType> {
        vec![self.0.clone()]
    }

    fn crash_conditions() -> Vec<&'static str> {
        vec!["stack underflow"]
    }

    fn gen_input_states() -> Vec<ExecutionState> {
        fn prepare_state<const N: usize>() -> ExecutionState {
            let list_pointer: BFieldElement = random();
            let old_length: usize = thread_rng().gen_range(1..30);
            let mut stack = get_init_tvm_stack();
            stack.push(list_pointer);
            let mut memory = HashMap::default();
            unsafe_insert_random_list::<N>(list_pointer, old_length, &mut memory);
            ExecutionState::with_stack_and_memory(stack, memory, 0)
        }

        vec![
            prepare_state::<N>(),
            prepare_state::<N>(),
            prepare_state::<N>(),
            prepare_state::<N>(),
            prepare_state::<N>(),
            prepare_state::<N>(),
        ]
    }

    fn stack_diff() -> isize {
        assert!(N < 17, "Max element size supported for list is 16");
        N as isize - 1
    }

    fn entrypoint(&self) -> &'static str {
        "pop_u32"
    }

    /// Pop last element from list. Does *not* actually delete the last
    /// element but instead leaves it in memory.
    fn function_body(&self, _library: &mut Library) -> String {
        assert!(N < 17, "Max element size supported for list is 16");
        let entry_point = self.entrypoint();

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
        memory: &mut HashMap<BFieldElement, BFieldElement>,
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
    use itertools::Itertools;
    use num::Zero;
    use rand::{thread_rng, RngCore};
    use twenty_first::shared_math::b_field_element::BFieldElement;

    use crate::get_init_tvm_stack;
    use crate::test_helpers::{rust_tasm_equivalence_prop, rust_tasm_equivalence_prop_new};

    use super::*;

    #[test]
    fn new_snippet_test() {
        rust_tasm_equivalence_prop_new::<Pop<1>>(Pop(DataType::U32));
        rust_tasm_equivalence_prop_new::<Pop<2>>(Pop(DataType::U64));
        rust_tasm_equivalence_prop_new::<Pop<3>>(Pop(DataType::XFE));
        rust_tasm_equivalence_prop_new::<Pop<5>>(Pop(DataType::Digest));
    }

    #[test]
    #[should_panic]
    fn panic_if_pop_on_empty_list_1() {
        let list_address = BFieldElement::new(48);
        prop_pop::<1>(DataType::BFE, list_address, 0);
    }

    #[test]
    #[should_panic]
    fn panic_if_pop_on_empty_list_2() {
        let list_address = BFieldElement::new(48);
        prop_pop::<2>(DataType::U64, list_address, 0);
    }

    #[test]
    #[should_panic]
    fn panic_if_pop_on_empty_list_3() {
        let list_address = BFieldElement::new(48);
        prop_pop::<3>(DataType::XFE, list_address, 0);
    }

    #[test]
    fn list_u32_n_is_n_pop() {
        let list_address = BFieldElement::new(48);
        prop_pop::<1>(DataType::BFE, list_address, 24);
        prop_pop::<2>(DataType::U64, list_address, 48);
        prop_pop::<3>(DataType::XFE, list_address, 3);
        // prop_pop::<4>(list_address, 4);
        prop_pop::<5>(DataType::Digest, list_address, 20);
        // prop_pop::<6>(list_address, 20);
        // prop_pop::<7>(list_address, 20);
        // prop_pop::<8>(list_address, 20);
        // prop_pop::<9>(list_address, 20);
        // prop_pop::<10>(list_address, 1);
        // prop_pop::<11>(list_address, 33);
        // prop_pop::<12>(list_address, 20);
        // prop_pop::<13>(list_address, 20);
        // prop_pop::<14>(list_address, 20);
        // prop_pop::<15>(list_address, 20);
        // prop_pop::<16>(list_address, 20);
    }

    fn prop_pop<const N: usize>(
        data_type: DataType,
        list_address: BFieldElement,
        init_list_length: u32,
    ) {
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
            Pop(data_type),
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
