use std::collections::HashMap;

use rand::{random, thread_rng, Rng};
use twenty_first::shared_math::b_field_element::BFieldElement;

use crate::library::Library;
use crate::rust_shadowing_helper_functions::unsafe_list::{
    unsafe_list_get, untyped_unsafe_insert_random_list,
};
use crate::snippet::{DataType, Snippet};
use crate::{get_init_tvm_stack, ExecutionState};

#[derive(Clone, Debug)]
pub struct UnsafeGet(pub DataType);

impl Snippet for UnsafeGet {
    fn inputs(&self) -> Vec<String> {
        vec!["*list".to_string(), "index".to_string()]
    }

    fn outputs(&self) -> Vec<String> {
        // This function returns element_0 on the top of the stack and the other elements below it. E.g.: _ elem_2 elem_1 elem_0
        let mut ret: Vec<String> = vec![];
        let size = self.0.get_size();
        for i in 0..size {
            ret.push(format!("element_{}", size - 1 - i));
        }

        ret
    }

    fn input_types(&self) -> Vec<crate::snippet::DataType> {
        vec![DataType::List(Box::new(self.0.clone())), DataType::U32]
    }

    fn output_types(&self) -> Vec<crate::snippet::DataType> {
        vec![DataType::BFE; self.0.get_size()]
    }

    fn crash_conditions(&self) -> Vec<String> {
        vec![]
    }

    fn gen_input_states(&self) -> Vec<crate::ExecutionState> {
        let mut rng = thread_rng();
        vec![input_state(rng.gen_range(1..100))]
    }

    fn stack_diff(&self) -> isize {
        self.0.get_size() as isize - 2
    }

    fn entrypoint(&self) -> String {
        format!(
            "tasm_list_unsafe_u32_get_element_{}",
            self.0.label_friendly_name()
        )
    }

    fn function_code(&self, _library: &mut Library) -> String {
        let entrypoint = self.entrypoint();
        // Code to read an element from a list. No bounds-check.

        let mut code_to_read_elements = String::default();

        // Start and end at loop: Stack: _  [elems], address_of_next_element
        for i in 0..self.0.get_size() {
            code_to_read_elements.push_str("read_mem\n");
            // stack: _  address_for_last_unread_element, elem_{{N - 1 - i}}

            code_to_read_elements.push_str("swap 1\n");
            // stack: _  [..., elem_{{N - 1 - i}}], address_for_last_unread_element
            if i != self.0.get_size() - 1 {
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
            // BEFORE: _ *list index
            // AFTER: _ elem{{N - 1}}, elem{{N - 2}}, ..., elem{{0}}
            {entrypoint}:
                push 1
                add
                {mul_with_size}
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
        &self,
        stack: &mut Vec<BFieldElement>,
        _std_in: Vec<BFieldElement>,
        _secret_in: Vec<BFieldElement>,
        memory: &mut HashMap<BFieldElement, BFieldElement>,
    ) {
        let index: u32 = stack.pop().unwrap().try_into().unwrap();
        let list_pointer = stack.pop().unwrap();
        let element: Vec<BFieldElement> =
            unsafe_list_get(list_pointer, index as usize, memory, self.0.get_size());

        // elements are placed on stack as: `elem[N - 1] elem[N - 2] .. elem[0]`
        for i in (0..self.0.get_size()).rev() {
            stack.push(element[i]);
        }
    }

    fn common_case_input_state(&self) -> ExecutionState {
        input_state(1 << 5)
    }

    fn worst_case_input_state(&self) -> ExecutionState {
        input_state(1 << 6)
    }
}

fn input_state(list_length: usize) -> ExecutionState {
    let mut rng = thread_rng();
    let list_pointer: u32 = random();
    let list_pointer = BFieldElement::new(list_pointer as u64);
    let index: usize = rng.gen_range(0..list_length);
    let mut stack = get_init_tvm_stack();
    stack.push(list_pointer);
    stack.push(BFieldElement::new(index as u64));

    let mut memory = HashMap::default();

    untyped_unsafe_insert_random_list(list_pointer, list_length, &mut memory, 4);

    ExecutionState {
        stack,
        std_in: vec![],
        secret_in: vec![],
        memory,
        words_allocated: 0,
    }
}

#[cfg(test)]
mod tests {
    use itertools::Itertools;
    use rand::{thread_rng, RngCore};
    use twenty_first::shared_math::b_field_element::BFieldElement;

    use crate::get_init_tvm_stack;

    use crate::test_helpers::{
        test_rust_equivalence_given_input_values, test_rust_equivalence_multiple,
    };

    use super::*;

    #[test]
    fn new_snippet_test() {
        test_rust_equivalence_multiple(&UnsafeGet(DataType::XFE), true);
    }

    #[test]
    fn get_simple_1() {
        let list_address = BFieldElement::new(48);
        for i in 0..10 {
            prop_get(DataType::BFE, list_address, i, 10);
        }
    }

    #[test]
    fn get_simple_2() {
        let list_address = BFieldElement::new(48);
        for i in 0..10 {
            prop_get(DataType::U64, list_address, i, 10);
        }
    }

    #[test]
    fn get_simple_3() {
        let list_address = BFieldElement::new(48);
        for i in 0..10 {
            prop_get(DataType::XFE, list_address, i, 10);
        }
    }

    #[test]
    fn get_simple_15() {
        let list_address = BFieldElement::new(48);
        for i in 0..10 {
            prop_get(DataType::Digest, list_address, i, 10);
        }
    }

    fn prop_get(data_type: DataType, list_pointer: BFieldElement, index: u32, list_length: u32) {
        let mut init_stack = get_init_tvm_stack();
        init_stack.push(list_pointer);
        init_stack.push(BFieldElement::new(index as u64));

        let mut memory = HashMap::default();

        // Insert length indicator of list, lives on offset = 0 from `list_address`
        memory.insert(list_pointer, BFieldElement::new(list_length as u64));

        // Insert random values for the elements in the list
        let element_size = data_type.get_size();
        let mut rng = thread_rng();
        let mut j = 1;
        for _ in 0..list_length {
            let element = (0..element_size)
                .map(|_| BFieldElement::new(rng.next_u64()))
                .collect_vec();
            for elem in element.iter() {
                memory.insert(list_pointer + BFieldElement::new(j), *elem);
                j += 1;
            }
        }
        let targeted_element: Vec<BFieldElement> =
            unsafe_list_get(list_pointer, index as usize, &memory, element_size);

        let mut expected_end_stack = get_init_tvm_stack();

        for i in 0..element_size {
            expected_end_stack.push(targeted_element[element_size - 1 - i]);
        }

        test_rust_equivalence_given_input_values(
            &UnsafeGet(data_type),
            &init_stack,
            &[],
            &[],
            &mut memory,
            0,
            Some(&expected_end_stack),
        );
    }
}

#[cfg(test)]
mod benches {
    use super::*;
    use crate::snippet_bencher::bench_and_write;

    #[test]
    fn unsafe_get_benchmark() {
        bench_and_write(UnsafeGet(DataType::Digest));
    }
}