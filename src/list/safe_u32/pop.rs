use std::collections::HashMap;

use rand::{random, thread_rng, Rng};
use twenty_first::shared_math::b_field_element::BFieldElement;

use crate::rust_shadowing_helper_functions::safe_list::{safe_insert_random_list, safe_list_pop};
use crate::snippet::{DataType, Snippet};
use crate::snippet_state::SnippetState;
use crate::{get_init_tvm_stack, ExecutionState};

#[derive(Clone, Debug)]
pub struct SafePop(pub DataType);

impl Snippet for SafePop {
    fn inputs(&self) -> Vec<String> {
        vec!["*list".to_string()]
    }

    fn outputs(&self) -> Vec<String> {
        let mut ret: Vec<String> = vec![];
        let element_size = self.0.get_size();
        for i in 0..element_size {
            ret.push(format!("element_{}", element_size - 1 - i));
        }

        ret
    }

    fn input_types(&self) -> Vec<crate::snippet::DataType> {
        vec![DataType::List(Box::new(self.0.clone()))]
    }

    fn output_types(&self) -> Vec<crate::snippet::DataType> {
        vec![self.0.clone()]
    }

    fn crash_conditions() -> Vec<String> {
        vec!["stack underflow".to_string()]
    }

    fn gen_input_states(&self) -> Vec<ExecutionState> {
        vec![prepare_state(&self.0)]
    }

    fn stack_diff(&self) -> isize {
        self.0.get_size() as isize - 1
    }

    fn entrypoint(&self) -> String {
        format!("tasm_list_safe_u32_pop_{}", self.0)
    }

    /// Pop last element from list. Does *not* actually delete the last
    /// element but instead leaves it in memory.
    fn function_body(&self, _library: &mut SnippetState) -> String {
        let entry_point = self.entrypoint();

        let mut code_to_read_elements = String::default();
        // Start and end of loop: Stack: _  [elems], address_for_last_unread_element
        for i in 0..self.0.get_size() {
            code_to_read_elements.push_str("read_mem\n");
            // stack: _  address_for_last_unread_element, elem_{{N - 1 - i}}

            code_to_read_elements.push_str("swap 1\n");
            // stack: _  [..., elem_{{N - 1 - i}}], address_for_last_unread_element
            if i != self.0.get_size() - 1 {
                // Update offset for last unread element
                code_to_read_elements.push_str("push -1\n");
                code_to_read_elements.push_str("add\n");
            }
        }

        let element_size = self.0.get_size();

        // Code to multiply with size. If size is 1, do nothing to save two clock cycles.
        let mul_with_size = if element_size != 1 {
            format!("push {element_size}\n mul\n")
        } else {
            String::default()
        };
        format!(
            "
            // Before: _ *list
            // After: _ elem{{N - 1}}, elem{{N - 2}}, ..., elem{{0}}
            {entry_point}:
                read_mem
                // stack : _  *list, length

                // Assert that length is not 0
                dup 0
                push 0
                eq
                push 0
                eq
                assert
                // stack : _  *list, length

                // Decrease length value by one and write back to memory
                swap 1
                dup 1
                push -1
                add
                write_mem
                swap 1
                // stack : _ *list initial_length

                {mul_with_size}
                // stack : _  *list, (offset_for_last_element = (N * initial_length))

                add
                push 1
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
        &self,
        stack: &mut Vec<BFieldElement>,
        _std_in: Vec<BFieldElement>,
        _secret_in: Vec<BFieldElement>,
        memory: &mut HashMap<BFieldElement, BFieldElement>,
    ) {
        let list_pointer = stack.pop().unwrap();
        let mut popped = safe_list_pop(list_pointer, memory, self.0.get_size());

        for _ in 0..self.0.get_size() {
            stack.push(popped.pop().unwrap());
        }
    }

    fn common_case_input_state(&self) -> ExecutionState
    where
        Self: Sized,
    {
        prepare_state(&self.0)
    }

    fn worst_case_input_state(&self) -> ExecutionState
    where
        Self: Sized,
    {
        prepare_state(&self.0)
    }
}

fn prepare_state(data_type: &DataType) -> ExecutionState {
    let list_pointer: BFieldElement = random();
    let capacity: usize = 30;
    let old_length: usize = thread_rng().gen_range(1..capacity);
    let mut stack = get_init_tvm_stack();
    stack.push(list_pointer);
    let mut memory = HashMap::default();
    safe_insert_random_list(
        data_type,
        list_pointer,
        capacity as u32,
        old_length,
        &mut memory,
    );
    ExecutionState::with_stack_and_memory(stack, memory, 0)
}

#[cfg(test)]
mod tests_pop {
    use num::One;
    use twenty_first::shared_math::b_field_element::BFieldElement;

    use crate::get_init_tvm_stack;
    use crate::rust_shadowing_helper_functions::safe_list::safe_list_push;
    use crate::snippet_bencher::bench_and_write;
    use crate::test_helpers::{rust_tasm_equivalence_prop, rust_tasm_equivalence_prop_new};

    use super::*;

    #[test]
    fn new_snippet_test() {
        rust_tasm_equivalence_prop_new(&SafePop(DataType::Bool), true);
        rust_tasm_equivalence_prop_new(&SafePop(DataType::U32), true);
        rust_tasm_equivalence_prop_new(&SafePop(DataType::U64), true);
        rust_tasm_equivalence_prop_new(&SafePop(DataType::BFE), true);
        rust_tasm_equivalence_prop_new(&SafePop(DataType::XFE), true);
        rust_tasm_equivalence_prop_new(&SafePop(DataType::Digest), true);
    }

    #[test]
    fn safe_pop_benchmark() {
        bench_and_write(SafePop(DataType::Digest));
    }

    #[test]
    #[should_panic]
    fn panic_if_pop_on_empty_list_1() {
        let list_address = BFieldElement::new(48);
        prop_pop(DataType::BFE, list_address, 0);
    }

    #[test]
    #[should_panic]
    fn panic_if_pop_on_empty_list_2() {
        let list_address = BFieldElement::new(48);
        prop_pop(DataType::U64, list_address, 0);
    }

    #[test]
    #[should_panic]
    fn panic_if_pop_on_empty_list_3() {
        let list_address = BFieldElement::new(48);
        prop_pop(DataType::XFE, list_address, 0);
    }

    #[test]
    fn list_u32_n_is_n_pop() {
        let list_address = BFieldElement::new(48);
        prop_pop(DataType::BFE, list_address, 24);
        prop_pop(DataType::Bool, list_address, 24);
        prop_pop(DataType::U32, list_address, 24);
        prop_pop(DataType::U64, list_address, 48);
        prop_pop(DataType::XFE, list_address, 3);
        prop_pop(DataType::Digest, list_address, 20);
    }

    fn prop_pop(data_type: DataType, list_pointer: BFieldElement, init_list_length: usize) {
        let element_size = data_type.get_size();
        let list_capacity = 107;
        let mut init_stack = get_init_tvm_stack();
        init_stack.push(list_pointer);

        let mut memory = HashMap::default();

        // Insert random values for the elements in the list
        safe_insert_random_list(
            &data_type,
            list_pointer,
            list_capacity,
            init_list_length,
            &mut memory,
        );

        let last_element: Vec<BFieldElement> =
            safe_list_pop(list_pointer, &mut memory, element_size);
        safe_list_push(
            list_pointer,
            last_element.clone(),
            &mut memory,
            element_size,
        );

        let mut expected_end_stack = get_init_tvm_stack();

        for i in 0..element_size {
            expected_end_stack.push(last_element[element_size - 1 - i]);
        }

        let _execution_result = rust_tasm_equivalence_prop(
            &SafePop(data_type),
            &init_stack,
            &[],
            &[],
            &mut memory,
            0,
            Some(&expected_end_stack),
        );

        // Verify that length is now indicated to be `init_list_length - 1`
        assert_eq!(
            BFieldElement::new(init_list_length as u64) - BFieldElement::one(),
            memory[&list_pointer]
        );

        // Verify that capacity is unchanged
        assert_eq!(
            BFieldElement::new(list_capacity as u64),
            memory[&(list_pointer + BFieldElement::one())]
        );
    }
}
