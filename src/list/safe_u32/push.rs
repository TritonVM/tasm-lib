use std::collections::HashMap;

use num::One;
use rand::{thread_rng, Rng};
use twenty_first::shared_math::{b_field_element::BFieldElement, other::random_elements};

use crate::{
    get_init_tvm_stack,
    list::safe_u32::SAFE_LIST_ELEMENT_CAPACITY,
    rust_shadowing_helper_functions::safe_list::{safe_insert_random_list, safe_list_push},
    snippet::{DataType, Snippet},
    snippet_state::SnippetState,
    ExecutionState,
};

#[derive(Clone, Debug)]
pub struct SafePush(pub DataType);

impl Snippet for SafePush {
    fn inputs(&self) -> Vec<String> {
        let element_size = self.0.get_size();

        // _ *list, elem{{N - 1}}, elem{{N - 2}}, ..., elem{{0}}
        let mut ret = vec!["*list".to_string()];
        for i in 0..element_size {
            ret.push(format!("element_{}", element_size - 1 - i));
        }

        ret
    }

    fn outputs(&self) -> Vec<String> {
        vec![]
    }

    fn input_types(&self) -> Vec<crate::snippet::DataType> {
        vec![DataType::List(Box::new(self.0.clone())), self.0.clone()]
    }

    fn output_types(&self) -> Vec<crate::snippet::DataType> {
        vec![]
    }

    fn crash_conditions(&self) -> Vec<String> {
        vec!["New length exceeds capacity".to_string()]
    }

    fn gen_input_states(&self) -> Vec<ExecutionState> {
        vec![
            prepare_execution_state(&self.0, 1, 0),
            prepare_execution_state(&self.0, 2, 0),
            prepare_execution_state(&self.0, SAFE_LIST_ELEMENT_CAPACITY, 0),
            prepare_execution_state(
                &self.0,
                SAFE_LIST_ELEMENT_CAPACITY,
                thread_rng().gen_range(0..100),
            ),
            prepare_execution_state(
                &self.0,
                SAFE_LIST_ELEMENT_CAPACITY,
                thread_rng().gen_range(0..100),
            ),
            prepare_execution_state(
                &self.0,
                SAFE_LIST_ELEMENT_CAPACITY,
                thread_rng().gen_range(0..100),
            ),
        ]
    }

    fn stack_diff(&self) -> isize {
        -(self.0.get_size() as isize) - 1
    }

    fn entrypoint(&self) -> String {
        format!("tasm_list_safe_u32_push_{}", self.0.label_friendly_name())
    }

    // Push *one* element of size N to stack
    fn function_code(&self, _library: &mut SnippetState) -> String {
        let element_size = self.0.get_size();
        // write the elements to memory

        // Start and end of this loop: _  *list, [elements..], address_of_next_element
        let mut write_elements_to_memory_code = String::default();
        for i in 0..element_size {
            write_elements_to_memory_code.push_str("swap 1\n");
            write_elements_to_memory_code.push_str("write_mem\n");
            if i != element_size - 1 {
                // Prepare for next write. Not needed for last iteration.
                write_elements_to_memory_code.push_str("push 1\n");
                write_elements_to_memory_code.push_str("add\n");
            }
        }

        // Code to multiply with size. If size is 1, do nothing to save two clock cycles.
        let mul_with_size = if element_size != 1 {
            format!("push {element_size}\n mul\n")
        } else {
            String::default()
        };

        let entry_point = self.entrypoint();
        format!(
            "
            // Before: _ *list, elem{{N - 1}}, elem{{N - 2}}, ..., elem{{0}}
            // After: _
            {entry_point}:
                dup {element_size}
                // stack : _  *list, elem{{N - 1}}, elem{{N - 2}}, ..., elem{{0}}, *list

                read_mem
                // stack : _  *list, elem{{N - 1}}, elem{{N - 2}}, ..., elem{{0}}, *list, length

                // Verify that length < capacity (before increasing length by 1)
                    swap 1
                    push 1
                    add
                    // stack : _  *list, elem{{N - 1}}, elem{{N - 2}}, ..., elem{{0}}, length, (*list + 1)

                    read_mem
                    // stack : _  *list, elem{{N - 1}}, elem{{N - 2}}, ..., elem{{0}}, length, (*list + 1), capacity

                    dup 2 lt
                    // stack : _  *list, elem{{N - 1}}, elem{{N - 2}}, ..., elem{{0}}, length, (*list + 1), capacity > length

                    assert
                    // stack : _  *list, elem{{N - 1}}, elem{{N - 2}}, ..., elem{{0}}, length, (*list + 1)

                    swap 1

                {mul_with_size}
                // stack : _  *list, elem{{N - 1}}, elem{{N - 2}}, ..., elem{{0}}, (*list + 1), length * elem_size

                add
                push 1
                add
                // stack : _  *list, elem{{N - 1}}, elem{{N - 2}}, ..., elem{{0}}, (*list + length * elem_size + 2) -- top of stack is where we will store elements

                {write_elements_to_memory_code}
                // stack : _  *list, address

                pop
                // stack : _  *list

                // Increase length indicator by one
                read_mem
                // stack : _  *list, length

                push 1
                add
                // stack : _  *list, length + 1

                write_mem
                // stack : _  *list

                pop
                // stack : _

                return
                "
        )
    }

    fn rust_shadowing(
        &self,
        stack: &mut Vec<twenty_first::shared_math::b_field_element::BFieldElement>,
        _std_in: Vec<twenty_first::shared_math::b_field_element::BFieldElement>,
        _secret_in: Vec<twenty_first::shared_math::b_field_element::BFieldElement>,
        memory: &mut std::collections::HashMap<
            twenty_first::shared_math::b_field_element::BFieldElement,
            twenty_first::shared_math::b_field_element::BFieldElement,
        >,
    ) {
        let mut elements: Vec<BFieldElement> = vec![];
        for _ in 0..self.0.get_size() {
            elements.push(stack.pop().unwrap());
        }

        let list_pointer = stack.pop().unwrap();
        let initial_list_length = memory[&list_pointer];
        safe_list_push(list_pointer, elements, memory, self.0.get_size());

        // Update length indicator
        memory.insert(list_pointer, initial_list_length + BFieldElement::one());
    }

    fn common_case_input_state(&self) -> ExecutionState {
        prepare_execution_state(&self.0, SAFE_LIST_ELEMENT_CAPACITY, 1 << 5)
    }

    fn worst_case_input_state(&self) -> ExecutionState {
        prepare_execution_state(&self.0, SAFE_LIST_ELEMENT_CAPACITY, 1 << 6)
    }
}

fn prepare_execution_state(
    data_type: &DataType,
    capacity: u32,
    init_length: usize,
) -> ExecutionState {
    let list_pointer: BFieldElement =
        BFieldElement::new(thread_rng().gen_range(0..u32::MAX as u64));
    let mut stack = get_init_tvm_stack();
    stack.push(list_pointer);
    let mut push_value: Vec<BFieldElement> = random_elements(data_type.get_size());
    while let Some(element) = push_value.pop() {
        stack.push(element);
    }

    let mut memory = HashMap::default();
    safe_insert_random_list(data_type, list_pointer, capacity, init_length, &mut memory);
    ExecutionState::with_stack_and_memory(stack, memory, 0)
}

#[cfg(test)]
mod tests {
    use crate::{
        test_helpers::{rust_tasm_equivalence_prop, rust_tasm_equivalence_prop_new},
        DIGEST_LENGTH,
    };

    use super::*;

    #[test]
    fn new_snippet_test() {
        rust_tasm_equivalence_prop_new(&SafePush(DataType::Bool), true);
        rust_tasm_equivalence_prop_new(&SafePush(DataType::U32), true);
        rust_tasm_equivalence_prop_new(&SafePush(DataType::U64), true);
        rust_tasm_equivalence_prop_new(&SafePush(DataType::BFE), true);
        rust_tasm_equivalence_prop_new(&SafePush(DataType::XFE), true);
        rust_tasm_equivalence_prop_new(&SafePush(DataType::Digest), true);
    }

    #[test]
    fn push_to_capacity_u32() {
        let list_address = BFieldElement::new(1841);
        let push_value = vec![BFieldElement::new(133700)];
        let init_length = 19;
        let capacity = 20;
        prop_push(
            DataType::U32,
            list_address,
            init_length,
            capacity,
            push_value,
        );
    }

    #[should_panic]
    #[test]
    fn push_beyond_capacity_u32() {
        let list_address = BFieldElement::new(1841);
        let push_value = vec![BFieldElement::new(133700)];
        let init_length = 20;
        let capacity = 20;
        prop_push(
            DataType::U32,
            list_address,
            init_length,
            capacity,
            push_value,
        );
    }

    #[test]
    fn push_to_capacity_xfe() {
        let list_address = BFieldElement::new(1841);
        let push_value = vec![
            BFieldElement::new(133700),
            BFieldElement::new(133701),
            BFieldElement::new(133702),
        ];
        let init_length = 19;
        let capacity = 20;
        prop_push(
            DataType::XFE,
            list_address,
            init_length,
            capacity,
            push_value,
        );
    }

    #[should_panic]
    #[test]
    fn push_beyond_capacity_xfe() {
        let list_address = BFieldElement::new(1841);
        let push_value = vec![
            BFieldElement::new(133700),
            BFieldElement::new(133701),
            BFieldElement::new(133702),
        ];
        let init_length = 20;
        let capacity = 20;
        prop_push(
            DataType::XFE,
            list_address,
            init_length,
            capacity,
            push_value,
        );
    }

    #[test]
    fn push_to_capacity_u64() {
        let list_address = BFieldElement::new(1841);
        let push_value = vec![BFieldElement::new(133700), BFieldElement::new(32)];
        let init_length = 19;
        let capacity = 20;
        prop_push(
            DataType::U64,
            list_address,
            init_length,
            capacity,
            push_value,
        );
    }

    #[should_panic]
    #[test]
    fn push_beyond_capacity_u64() {
        let list_address = BFieldElement::new(1841);
        let push_value = vec![BFieldElement::new(133700), BFieldElement::new(32)];
        let init_length = 20;
        let capacity = 20;
        prop_push(
            DataType::U64,
            list_address,
            init_length,
            capacity,
            push_value,
        );
    }

    #[test]
    fn push_to_capacity_digest() {
        let list_address = BFieldElement::new(1841);
        let push_value = vec![BFieldElement::new(133700); DIGEST_LENGTH];
        let init_length = 43;
        let capacity = 44;
        prop_push(
            DataType::Digest,
            list_address,
            init_length,
            capacity,
            push_value,
        );
    }

    #[test]
    fn push_to_empty_and_almost_empty_list_digest() {
        let list_address = BFieldElement::new(1);
        let push_value = vec![BFieldElement::new(133700); DIGEST_LENGTH];
        for init_length in 0..1 {
            let capacity = 64;
            prop_push(
                DataType::Digest,
                list_address,
                init_length,
                capacity,
                push_value.clone(),
            );
        }
    }

    #[should_panic]
    #[test]
    fn push_beyond_capacity_digest() {
        let list_address = BFieldElement::new(1841);
        let push_value = vec![BFieldElement::new(133700); DIGEST_LENGTH];
        let init_length = 2;
        let capacity = 2;
        prop_push(
            DataType::U32,
            list_address,
            init_length,
            capacity,
            push_value,
        );
    }

    fn prop_push(
        data_type: DataType,
        list_address: BFieldElement,
        init_list_length: u32,
        list_capacity: u32,
        push_value: Vec<BFieldElement>,
    ) {
        assert_eq!(
            data_type.get_size(),
            push_value.len(),
            "Push value length must match data size"
        );
        let expected_end_stack = get_init_tvm_stack();
        let mut init_stack = get_init_tvm_stack();
        init_stack.push(list_address);

        for i in 0..data_type.get_size() {
            init_stack.push(push_value[data_type.get_size() - 1 - i]);
        }
        let mut memory = HashMap::default();

        safe_insert_random_list(
            &data_type,
            list_address,
            list_capacity,
            init_list_length as usize,
            &mut memory,
        );

        let _execution_result = rust_tasm_equivalence_prop(
            &SafePush(data_type.clone()),
            &init_stack,
            &[],
            &[],
            &mut memory,
            1,
            Some(&expected_end_stack),
        );

        // Verify that length indicator has increased by one
        assert_eq!(
            BFieldElement::new((init_list_length + 1) as u64),
            memory[&list_address]
        );

        // verify that value was inserted at expected place
        for i in 0..data_type.get_size() {
            assert_eq!(
                push_value[i],
                memory[&BFieldElement::new(
                    list_address.value()
                        + 2
                        + data_type.get_size() as u64 * init_list_length as u64
                        + i as u64
                )]
            );
        }
    }
}

#[cfg(test)]
mod benches {
    use super::*;
    use crate::snippet_bencher::bench_and_write;

    #[test]
    fn safe_push_benchmark() {
        bench_and_write(SafePush(DataType::Digest));
    }
}
