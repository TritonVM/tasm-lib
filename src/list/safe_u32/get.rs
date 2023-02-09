use std::cmp;
use std::collections::HashMap;

use rand::{random, thread_rng, Rng};
use twenty_first::shared_math::b_field_element::BFieldElement;

use crate::library::Library;
use crate::rust_shadowing_helper_functions::safe_list::{safe_insert_random_list, safe_list_read};
use crate::snippet::{DataType, Snippet};
use crate::{get_init_tvm_stack, ExecutionState};

#[derive(Clone)]
pub struct SafeGet(pub DataType);

impl Snippet for SafeGet {
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

    fn crash_conditions() -> Vec<String> {
        vec!["Index out of bounds".to_string()]
    }

    fn gen_input_states(&self) -> Vec<crate::ExecutionState> {
        let mut rng = thread_rng();
        let list_pointer: BFieldElement = random();
        let capacity = rng.gen_range(1..1000);
        let list_length: usize = rng.gen_range(1..cmp::min(capacity, 100));
        let index: usize = rng.gen_range(0..list_length);
        let mut stack = get_init_tvm_stack();
        stack.push(list_pointer);
        stack.push(BFieldElement::new(index as u64));

        let mut memory = HashMap::default();

        safe_insert_random_list(
            &self.0,
            list_pointer,
            capacity as u32,
            list_length,
            &mut memory,
        );

        vec![ExecutionState {
            stack,
            std_in: vec![],
            secret_in: vec![],
            memory,
            words_allocated: 0,
        }]
    }

    fn stack_diff(&self) -> isize {
        self.0.get_size() as isize - 2
    }

    fn entrypoint(&self) -> String {
        format!("tasm_list_safe_u32_get_element_{}", self.0)
    }

    fn function_body(&self, _library: &mut Library) -> String {
        let entrypoint = self.entrypoint();
        // Code to read an element from a list. With bounds check.

        let mut code_to_read_elements = String::default();

        // Start and end at loop: Stack: _  [elems], address_of_next_element
        for i in 0..self.0.get_size() {
            code_to_read_elements.push_str("push 0\n");
            code_to_read_elements.push_str("read_mem\n");
            // stack: _  address_for_last_unread_element, elem_{{N - 1 - i}}

            code_to_read_elements.push_str("swap1\n");
            // stack: _  [..., elem_{{N - 1 - i}}], address_for_last_unread_element
            if i != self.0.get_size() - 1 {
                code_to_read_elements.push_str("push -1\n");
                code_to_read_elements.push_str("add\n");
            }
        }
        let size = self.0.get_size();
        format!(
            "
            // BEFORE: _ *list index
            // After: _ elem{{N - 1}}, elem{{N - 2}}, ..., elem{{0}}
            {entrypoint}:
                dup1
                push 0
                read_mem
                // stack: _ *list index *list length

                dup2
                lt
                // stack: _ *list index *list length < index

                assert
                // stack: _ *list index *list

                pop
                // stack: _ *list index

                push 1
                add
                push {size}
                mul
                // stack: _ *list (N * (index + 1))

                add
                // stack: _ (*list + N * (index + 1))

                push 1
                add
                // stack: _ (*list + N * (index + 1) + 1)

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
            safe_list_read(list_pointer, index as usize, memory, self.0.get_size());

        // elements are placed on stack as: `elem[N - 1] elem[N - 2] .. elem[0]`
        for i in (0..self.0.get_size()).rev() {
            stack.push(element[i]);
        }
    }
}

#[cfg(test)]
mod get_element_tests {
    use twenty_first::shared_math::b_field_element::BFieldElement;

    use crate::get_init_tvm_stack;
    use crate::test_helpers::{rust_tasm_equivalence_prop, rust_tasm_equivalence_prop_new};

    use super::*;

    #[test]
    fn new_snippet_test() {
        for _ in 0..100 {
            rust_tasm_equivalence_prop_new(SafeGet(DataType::Bool));
            rust_tasm_equivalence_prop_new(SafeGet(DataType::U32));
            rust_tasm_equivalence_prop_new(SafeGet(DataType::U64));
            rust_tasm_equivalence_prop_new(SafeGet(DataType::BFE));
            rust_tasm_equivalence_prop_new(SafeGet(DataType::XFE));
            rust_tasm_equivalence_prop_new(SafeGet(DataType::Digest));
        }
    }

    #[test]
    fn get_simple_1() {
        let list_address = BFieldElement::new(48);
        for i in 0..10 {
            prop_get(&DataType::BFE, list_address, i, 10);
        }
    }

    #[test]
    fn read_at_edge_1() {
        let list_address = BFieldElement::new(48);
        prop_get(&DataType::BFE, list_address, 8, 10);
        prop_get(&DataType::BFE, list_address, 9, 10);
    }

    #[should_panic]
    #[test]
    fn read_out_of_bounds_1_a() {
        let list_address = BFieldElement::new(48);
        let length = 10;
        let index = 10;
        prop_get(&DataType::BFE, list_address, index, length);
    }

    #[should_panic]
    #[test]
    fn read_out_of_bounds_1_b() {
        let list_address = BFieldElement::new(48);
        let length = 10;
        let index = 11;
        prop_get(&DataType::BFE, list_address, index, length);
    }

    #[should_panic]
    #[test]
    fn read_out_of_bounds_1_c() {
        let list_address = BFieldElement::new(48);
        let length = 10;
        let index = 12;
        prop_get(&DataType::BFE, list_address, index, length);
    }

    #[test]
    fn get_simple_2() {
        let list_address = BFieldElement::new(48);
        for i in 0..10 {
            prop_get(&DataType::U64, list_address, i, 10);
        }
    }

    #[test]
    fn read_at_edge_2() {
        let list_address = BFieldElement::new(48);
        prop_get(&DataType::U64, list_address, 0, 10);
        prop_get(&DataType::U64, list_address, 1, 10);
        prop_get(&DataType::U64, list_address, 8, 10);
        prop_get(&DataType::U64, list_address, 9, 10);
    }

    #[should_panic]
    #[test]
    fn read_out_of_bounds_2_a() {
        let list_address = BFieldElement::new(48);
        let length = 10;
        let index = 10;
        prop_get(&DataType::U64, list_address, index, length);
    }

    #[should_panic]
    #[test]
    fn read_out_of_bounds_2_b() {
        let list_address = BFieldElement::new(48);
        let length = 10;
        let index = 11;
        prop_get(&DataType::U64, list_address, index, length);
    }

    #[should_panic]
    #[test]
    fn read_out_of_bounds_2_c() {
        let list_address = BFieldElement::new(48);
        let length = 10;
        let index = 12;
        prop_get(&DataType::U64, list_address, index, length);
    }

    #[test]
    fn get_simple_3() {
        let list_address = BFieldElement::new(48);
        for i in 0..10 {
            prop_get(&DataType::XFE, list_address, i, 10);
        }
    }

    #[test]
    fn get_simple_5() {
        let list_address = BFieldElement::new(48);
        for i in 0..10 {
            prop_get(&DataType::Digest, list_address, i, 10);
        }
    }

    #[test]
    fn read_at_edge_5() {
        let list_address = BFieldElement::new(48);
        prop_get(&DataType::Digest, list_address, 0, 10);
        prop_get(&DataType::Digest, list_address, 1, 10);
        prop_get(&DataType::Digest, list_address, 8, 10);
        prop_get(&DataType::Digest, list_address, 9, 10);
    }

    #[should_panic]
    #[test]
    fn read_out_of_bounds_5_a() {
        let list_address = BFieldElement::new(48);
        let length = 10;
        let index = 10;
        prop_get(&DataType::Digest, list_address, index, length);
    }

    #[should_panic]
    #[test]
    fn read_out_of_bounds_5_b() {
        let list_address = BFieldElement::new(48);
        let length = 10;
        let index = 11;
        prop_get(&DataType::Digest, list_address, index, length);
    }

    #[should_panic]
    #[test]
    fn read_out_of_bounds_5_c() {
        let list_address = BFieldElement::new(48);
        let length = 10;
        let index = 12;
        prop_get(&DataType::Digest, list_address, index, length);
    }

    fn prop_get(data_type: &DataType, list_pointer: BFieldElement, index: u32, list_length: u32) {
        let element_size = data_type.get_size();

        let mut init_stack = get_init_tvm_stack();
        init_stack.push(list_pointer);
        init_stack.push(BFieldElement::new(index as u64));

        let mut memory = HashMap::default();

        // Insert length indicator of list, lives on offset = 0 from `list_address`
        memory.insert(list_pointer, BFieldElement::new(list_length as u64));

        let list_capacity = 100;
        safe_insert_random_list(
            data_type,
            list_pointer,
            list_capacity,
            list_length as usize,
            &mut memory,
        );

        let targeted_element: Vec<BFieldElement> =
            safe_list_read(list_pointer, index as usize, &memory, element_size);

        let mut expected_end_stack = get_init_tvm_stack();

        for i in 0..element_size {
            expected_end_stack.push(targeted_element[element_size - 1 - i]);
        }

        let _execution_result = rust_tasm_equivalence_prop::<SafeGet>(
            SafeGet(data_type.to_owned()),
            &init_stack,
            &[],
            &[],
            &mut memory,
            0,
            Some(&expected_end_stack),
        );
    }
}
