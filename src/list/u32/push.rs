use std::collections::HashMap;

use num::One;
use rand::{random, thread_rng, Rng};
use twenty_first::shared_math::b_field_element::BFieldElement;
use twenty_first::shared_math::other::random_elements;

use crate::library::Library;
use crate::rust_shadowing_helper_functions::insert_random_list;
use crate::snippet::{DataType, Snippet};
use crate::{get_init_tvm_stack, ExecutionState};

#[derive(Clone)]
pub struct Push<const N: usize>(pub DataType);

/// A parameterized version of `Push` where `N` is the size of an element in the list
impl<const N: usize> Snippet for Push<N> {
    fn inputs() -> Vec<&'static str> {
        // See: https://github.com/TritonVM/tasm-snippets/issues/13
        // _ *list, elem{{N - 1}}, elem{{N - 2}}, ..., elem{{0}}
        vec![vec!["*list"], vec!["element"; N]].concat()
    }

    fn outputs() -> Vec<&'static str> {
        vec!["*list"]
    }

    fn input_types(&self) -> Vec<crate::snippet::DataType> {
        vec![DataType::List(Box::new(self.0.clone())), self.0.clone()]
    }

    fn output_types(&self) -> Vec<crate::snippet::DataType> {
        vec![DataType::List(Box::new(self.0.clone()))]
    }

    fn crash_conditions() -> Vec<&'static str> {
        vec![]
    }

    fn gen_input_states() -> Vec<ExecutionState> {
        fn prepare_state<const N: usize>() -> ExecutionState {
            let list_pointer: BFieldElement = random();
            let init_length: usize = thread_rng().gen_range(0..100);
            let mut stack = get_init_tvm_stack();
            stack.push(list_pointer);
            let mut push_value: Vec<BFieldElement> = random_elements(N);
            while let Some(element) = push_value.pop() {
                stack.push(element);
            }

            let mut memory = HashMap::default();
            insert_random_list::<N>(list_pointer, init_length, &mut memory);
            ExecutionState::with_stack_and_memory(stack, memory, 0)
        }

        vec![
            prepare_state::<N>(),
            prepare_state::<N>(),
            prepare_state::<N>(),
            prepare_state::<N>(),
        ]
    }

    fn stack_diff() -> isize {
        assert!(N < 17, "Max element size supported for list is 16");
        -(N as isize)
    }

    fn entrypoint() -> &'static str {
        assert!(N < 17, "Max element size supported for list is 16");
        "push_u32"
    }

    // Push *one* element of size N to stack
    fn function_body(_library: &mut Library) -> String {
        assert!(N < 17, "Max element size supported for list is 16");

        // write the elements to memory
        // Start and end of this loop: _  *list, [elements..], address_of_next_element -- top of stack is where we will store elements
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

        let entry_point = Self::entrypoint();
        format!(
            "
            // Before: _ *list, elem{{N - 1}}, elem{{N - 2}}, ..., elem{{0}}
            // After: _ *list
            {entry_point}:
                dup{N}
                // stack : _  *list, elem{{N - 1}}, elem{{N - 2}}, ..., elem{{0}}, *list

                push 0
                read_mem
                // stack : _  *list, elem{{N - 1}}, elem{{N - 2}}, ..., elem{{0}}, *list, length

                push {N}
                mul
                // stack : _  *list, elem{{N - 1}}, elem{{N - 2}}, ..., elem{{0}}, *list, length * elem_size

                push 1
                add
                // stack : _  *list, elem{{N - 1}}, elem{{N - 2}}, ..., elem{{0}}, *list, length * elem_size + 1 -- top of stack is offset for where we will store elements

                add
                // stack : _  *list, elem{{N - 1}}, elem{{N - 2}}, ..., elem{{0}}, *list + length * elem_size + 1 -- top of stack is where we will store elements

                {write_elements_to_memory_code}
                // stack : _  *list, *list + length * elem_size + 1

                // Increase length indicator by one
                read_mem
                // stack : _  *list, length

                push 1
                add
                // stack : _  *list, length + 1

                write_mem
                pop
                // stack : _  *list

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
        let list_address = stack[stack.len() - 1 - N];
        let initial_list_length = memory[&list_address];

        let mut next_free_address = list_address
            + initial_list_length * BFieldElement::new(N as u64)
            + BFieldElement::one();

        for _ in 0..N {
            let elem = stack.pop().unwrap();
            memory.insert(next_free_address, elem);
            next_free_address += BFieldElement::one();
        }

        // Update length indicator
        memory.insert(list_address, initial_list_length + BFieldElement::one());
    }
}

#[cfg(test)]
mod tests_push {
    use twenty_first::shared_math::b_field_element::BFieldElement;

    use crate::get_init_tvm_stack;
    use crate::rust_shadowing_helper_functions::insert_random_list;
    use crate::test_helpers::{rust_tasm_equivalence_prop, rust_tasm_equivalence_prop_new};

    use super::*;

    #[test]
    fn new_snippet_test() {
        rust_tasm_equivalence_prop_new::<Push<1>>(Push(DataType::Bool));
        rust_tasm_equivalence_prop_new::<Push<2>>(Push(DataType::U64));
        rust_tasm_equivalence_prop_new::<Push<3>>(Push(DataType::XFE));
        rust_tasm_equivalence_prop_new::<Push<5>>(Push(DataType::Digest));
    }

    #[test]
    fn list_u32_n_is_one_push() {
        let list_address = BFieldElement::new(48);
        let push_value = [BFieldElement::new(1337)];
        prop_push(DataType::BFE, list_address, 20, push_value);
    }

    #[test]
    fn list_u32_n_is_two_push() {
        let list_address = BFieldElement::new(1841);
        let push_value = [BFieldElement::new(133700), BFieldElement::new(32)];
        prop_push(DataType::U64, list_address, 20, push_value);
    }

    #[test]
    fn list_u32_n_is_five_push() {
        let list_address = BFieldElement::new(558);
        let push_value = [
            BFieldElement::new(133700),
            BFieldElement::new(32),
            BFieldElement::new(133700),
            BFieldElement::new(19990),
            BFieldElement::new(88888888),
        ];
        prop_push(DataType::Digest, list_address, 2313, push_value);
    }

    fn prop_push<const N: usize>(
        data_type: DataType,
        list_address: BFieldElement,
        init_list_length: u32,
        push_value: [BFieldElement; N],
    ) {
        let expected_end_stack = vec![get_init_tvm_stack(), vec![list_address]].concat();
        let mut init_stack = get_init_tvm_stack();
        init_stack.push(list_address);

        for i in 0..N {
            init_stack.push(push_value[N - 1 - i]);
        }
        let mut memory = HashMap::default();

        insert_random_list::<N>(list_address, init_list_length as usize, &mut memory);

        let _execution_result = rust_tasm_equivalence_prop::<Push<N>>(
            Push(data_type),
            &init_stack,
            &[],
            &[],
            &mut memory,
            0,
            Some(&expected_end_stack),
        );

        // Verify that length indicator has increased by one
        assert_eq!(
            BFieldElement::new((init_list_length + 1) as u64),
            memory[&list_address]
        );

        // verify that value was inserted at expected place
        for i in 0..N {
            assert_eq!(
                push_value[i],
                memory[&BFieldElement::new(
                    list_address.value() + 1 + N as u64 * init_list_length as u64 + i as u64
                )]
            );
        }
    }
}
