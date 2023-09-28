use std::cmp;
use std::collections::HashMap;

use num::One;
use rand::{random, thread_rng, Rng};
use triton_vm::NonDeterminism;
use twenty_first::shared_math::b_field_element::BFieldElement;

use crate::library::Library;
use crate::rust_shadowing_helper_functions::safe_list::{safe_insert_random_list, safe_list_get};
use crate::snippet::{DataType, DeprecatedSnippet};
use crate::{get_init_tvm_stack, ExecutionState};

#[derive(Clone, Debug)]
pub struct SafeGet(pub DataType);

impl DeprecatedSnippet for SafeGet {
    fn input_field_names(&self) -> Vec<String> {
        vec!["*list".to_string(), "index".to_string()]
    }

    fn output_field_names(&self) -> Vec<String> {
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
        vec!["Index out of bounds".to_string()]
    }

    fn gen_input_states(&self) -> Vec<crate::ExecutionState> {
        let mut rng = thread_rng();
        let list_pointer: BFieldElement = random();
        let capacity = rng.gen_range(1..1000);
        let list_length: usize = rng.gen_range(1..=cmp::min(capacity, 100));
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
            nondeterminism: NonDeterminism::new(vec![]),
            memory,
            words_allocated: 0,
        }]
    }

    fn stack_diff(&self) -> isize {
        self.0.get_size() as isize - 2
    }

    fn entrypoint_name(&self) -> String {
        format!(
            "tasm_list_safeimplu32_get_element___{}",
            self.0.label_friendly_name()
        )
    }

    fn function_code(&self, _library: &mut Library) -> String {
        let entrypoint = self.entrypoint_name();
        // Code to read an element from a list. With bounds check.

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
            // After: _ elem{{N - 1}}, elem{{N - 2}}, ..., elem{{0}}
            {entrypoint}:
                dup 1
                read_mem
                // stack: _ *list index *list length

                dup 2
                lt
                // stack: _ *list index *list length < index

                assert
                // stack: _ *list index *list

                pop
                // stack: _ *list index

                push 1
                add
                {mul_with_size}
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
            safe_list_get(list_pointer, index as usize, memory, self.0.get_size());

        // elements are placed on stack as: `elem[N - 1] elem[N - 2] .. elem[0]`
        for i in (0..self.0.get_size()).rev() {
            stack.push(element[i]);
        }
    }

    fn common_case_input_state(&self) -> ExecutionState {
        const COMMON_LENGTH: usize = 1 << 5;
        get_benchmark_input_state(COMMON_LENGTH, &self.0)
    }

    fn worst_case_input_state(&self) -> ExecutionState {
        const COMMON_LENGTH: usize = 1 << 6;
        get_benchmark_input_state(COMMON_LENGTH, &self.0)
    }
}

fn get_benchmark_input_state(list_length: usize, data_type: &DataType) -> ExecutionState {
    let mut memory = HashMap::default();
    let list_pointer: BFieldElement = BFieldElement::one();

    let capacity = list_length * 2;
    safe_insert_random_list(
        data_type,
        list_pointer,
        capacity as u32,
        list_length,
        &mut memory,
    );

    let mut stack = get_init_tvm_stack();
    stack.push(list_pointer);
    stack.push(BFieldElement::new((list_length - 1) as u64));

    ExecutionState {
        stack,
        std_in: vec![],
        nondeterminism: NonDeterminism::new(vec![]),
        memory,
        words_allocated: 1,
    }
}

#[cfg(test)]
mod tests {
    use twenty_first::shared_math::b_field_element::BFieldElement;

    use super::*;
    use crate::get_init_tvm_stack;
    use crate::test_helpers::{
        test_rust_equivalence_given_input_values_deprecated,
        test_rust_equivalence_multiple_deprecated,
    };

    #[test]
    fn new_snippet_test() {
        for _ in 0..10 {
            test_rust_equivalence_multiple_deprecated(&SafeGet(DataType::Bool), true);
            test_rust_equivalence_multiple_deprecated(&SafeGet(DataType::U32), true);
            test_rust_equivalence_multiple_deprecated(&SafeGet(DataType::U64), true);
            test_rust_equivalence_multiple_deprecated(&SafeGet(DataType::BFE), true);
            test_rust_equivalence_multiple_deprecated(&SafeGet(DataType::XFE), true);
            test_rust_equivalence_multiple_deprecated(&SafeGet(DataType::Digest), true);
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
            safe_list_get(list_pointer, index as usize, &memory, element_size);

        let mut expected_end_stack = get_init_tvm_stack();

        for i in 0..element_size {
            expected_end_stack.push(targeted_element[element_size - 1 - i]);
        }

        test_rust_equivalence_given_input_values_deprecated(
            &SafeGet(data_type.to_owned()),
            &init_stack,
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
    fn safe_get_benchmark() {
        bench_and_write(SafeGet(DataType::Digest));
    }
}
