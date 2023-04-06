use rand::{random, thread_rng, Rng};
use std::collections::HashMap;
use twenty_first::shared_math::b_field_element::BFieldElement;
use twenty_first::shared_math::other::random_elements;

use crate::rust_shadowing_helper_functions::safe_list::{safe_insert_random_list, safe_list_set};
use crate::snippet::{DataType, Snippet};
use crate::snippet_state::SnippetState;
use crate::{get_init_tvm_stack, ExecutionState};

#[derive(Clone)]
pub struct SafeSet(pub DataType);

impl Snippet for SafeSet {
    fn inputs(&self) -> Vec<String> {
        // See: https://github.com/TritonVM/tasm-snippets/issues/13
        // _ elem{{N - 1}}, elem{{N - 2}}, ..., elem{{0}} *list index
        vec![
            vec!["element".to_string(); self.0.get_size()],
            vec!["*list".to_string(), "index".to_string()],
        ]
        .concat()
    }

    fn outputs(&self) -> Vec<String> {
        vec![]
    }

    fn input_types(&self) -> Vec<crate::snippet::DataType> {
        vec![
            self.0.clone(),
            DataType::List(Box::new(self.0.clone())),
            DataType::U32,
        ]
    }

    fn output_types(&self) -> Vec<crate::snippet::DataType> {
        vec![]
    }

    fn crash_conditions() -> Vec<String> {
        vec![]
    }

    fn gen_input_states(&self) -> Vec<ExecutionState> {
        let capacity = 100u32;
        vec![
            prepare_state(
                &self.0,
                capacity,
                thread_rng().gen_range(1..capacity) as usize,
            ),
            prepare_state(
                &self.0,
                capacity,
                thread_rng().gen_range(1..capacity) as usize,
            ),
            prepare_state(
                &self.0,
                capacity,
                thread_rng().gen_range(1..capacity) as usize,
            ),
        ]
    }

    fn stack_diff(&self) -> isize {
        -2 - self.0.get_size() as isize
    }

    fn entrypoint(&self) -> String {
        format!("tasm_list_safe_u32_set_element_{}", self.0)
    }

    fn function_body(&self, _library: &mut SnippetState) -> String {
        let entrypoint = self.entrypoint();
        let element_size = self.0.get_size();

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

        format!(
            "
                // BEFORE: _ elem{{N - 1}}, elem{{N - 2}}, ..., elem{{0}} *list index
                // AFTER: _
                {entrypoint}:
                    // Verify that index is less than length
                    swap 1
                    read_mem
                    // _ elem{{N - 1}}, elem{{N - 2}}, ..., elem{{0}} index *list length

                    dup 2 lt
                    // _ elem{{N - 1}}, elem{{N - 2}}, ..., elem{{0}} index *list length < index

                    assert
                    // _ elem{{N - 1}}, elem{{N - 2}}, ..., elem{{0}} index *list

                    swap 1
                    // _ elem{{N - 1}}, elem{{N - 2}}, ..., elem{{0}} *list index

                    {mul_with_size}
                    push 2
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
        &self,
        stack: &mut Vec<BFieldElement>,
        _std_in: Vec<BFieldElement>,
        _secret_in: Vec<BFieldElement>,
        memory: &mut HashMap<BFieldElement, BFieldElement>,
    ) {
        let index: u32 = stack.pop().unwrap().try_into().unwrap();
        let list_pointer = stack.pop().unwrap();
        let list_length: u32 = memory[&list_pointer].value().try_into().unwrap();
        assert!(index < list_length, "Index must be within list bounds");

        let mut element: Vec<BFieldElement> = vec![BFieldElement::new(0); self.0.get_size()];
        for ee in element.iter_mut() {
            *ee = stack.pop().unwrap();
        }

        safe_list_set(
            list_pointer,
            index as usize,
            element,
            memory,
            self.0.get_size(),
        );
    }

    fn common_case_input_state(&self) -> ExecutionState
    where
        Self: Sized,
    {
        prepare_state(&self.0, 1000, 1 << 5)
    }

    fn worst_case_input_state(&self) -> ExecutionState
    where
        Self: Sized,
    {
        prepare_state(&self.0, 1000, 1 << 6)
    }
}

fn prepare_state(data_type: &DataType, capacity: u32, list_length: usize) -> ExecutionState {
    let index: usize = thread_rng().gen_range(0..list_length);
    let mut stack = get_init_tvm_stack();
    let mut push_value: Vec<BFieldElement> = random_elements(data_type.get_size());
    while let Some(element) = push_value.pop() {
        stack.push(element);
    }

    let list_pointer: u32 = random();
    let list_pointer_bfe = BFieldElement::new(list_pointer as u64);
    stack.push(list_pointer_bfe);
    stack.push(BFieldElement::new(index as u64));

    let mut memory = HashMap::default();
    safe_insert_random_list(
        data_type,
        list_pointer_bfe,
        capacity,
        list_length,
        &mut memory,
    );
    ExecutionState::with_stack_and_memory(stack, memory, 0)
}

#[cfg(test)]
mod list_set_tests {
    use twenty_first::shared_math::b_field_element::BFieldElement;

    use crate::get_init_tvm_stack;
    use crate::snippet_bencher::bench_and_write;
    use crate::test_helpers::{rust_tasm_equivalence_prop, rust_tasm_equivalence_prop_new};

    use super::*;

    #[test]
    fn new_snippet_test() {
        for _ in 0..6 {
            rust_tasm_equivalence_prop_new::<SafeSet>(SafeSet(DataType::Bool));
            rust_tasm_equivalence_prop_new::<SafeSet>(SafeSet(DataType::BFE));
            rust_tasm_equivalence_prop_new::<SafeSet>(SafeSet(DataType::U32));
            rust_tasm_equivalence_prop_new::<SafeSet>(SafeSet(DataType::U64));
            rust_tasm_equivalence_prop_new::<SafeSet>(SafeSet(DataType::XFE));
            rust_tasm_equivalence_prop_new::<SafeSet>(SafeSet(DataType::Digest));
        }
    }

    #[test]
    fn safe_set_benchmark() {
        bench_and_write(SafeSet(DataType::Digest));
    }

    #[test]
    fn list_u32_n_is_one_set() {
        let list_address = BFieldElement::new(48);
        let insert_value = vec![BFieldElement::new(1337)];
        let index = 2;
        prop_set(DataType::BFE, list_address, 20, insert_value, index, 21);
    }

    #[test]
    fn list_u32_n_is_one_set_at_edge_a() {
        let list_address = BFieldElement::new(48);
        let insert_value = vec![BFieldElement::new(1337)];
        let index = 19;
        prop_set(DataType::BFE, list_address, 20, insert_value, index, 21);
    }

    #[test]
    fn list_u32_n_is_one_set_at_edge_b() {
        let list_address = BFieldElement::new(48);
        let insert_value = vec![BFieldElement::new(1337)];
        let index = 19;
        prop_set(DataType::BFE, list_address, 20, insert_value, index, 21);
    }

    #[should_panic]
    #[test]
    fn list_u32_n_is_one_set_out_of_bounds_a() {
        let list_address = BFieldElement::new(48);
        let insert_value = vec![BFieldElement::new(1337)];
        let index = 20;
        prop_set(DataType::BFE, list_address, 20, insert_value, index, 21);
    }

    #[should_panic]
    #[test]
    fn list_u32_n_is_one_set_out_of_bounds_b() {
        let list_address = BFieldElement::new(48);
        let insert_value = vec![BFieldElement::new(1337)];
        let index = 21;
        prop_set(DataType::BFE, list_address, 20, insert_value, index, 21);
    }

    #[test]
    fn list_u32_n_is_three_set() {
        let list_address = BFieldElement::new(48);
        let insert_value = vec![
            BFieldElement::new(1337),
            BFieldElement::new(1337),
            BFieldElement::new(1337),
        ];
        let index = 2;
        prop_set(DataType::XFE, list_address, 20, insert_value, index, 20);
    }

    #[test]
    fn list_u32_n_is_two_set() {
        let list_address = BFieldElement::new(1841);
        let push_value = vec![BFieldElement::new(133700), BFieldElement::new(32)];
        let index = 0;
        prop_set(DataType::U64, list_address, 20, push_value, index, 20);
    }

    #[should_panic]
    #[test]
    fn list_u32_n_is_two_set_out_of_bounds_a() {
        let list_address = BFieldElement::new(48);
        let insert_value = vec![BFieldElement::new(1337)];
        let index = 20;
        prop_set(DataType::U64, list_address, 20, insert_value, index, 21);
    }

    #[test]
    fn list_u32_n_is_five_set() {
        let list_address = BFieldElement::new(558);
        let push_value = vec![
            BFieldElement::new(133700),
            BFieldElement::new(32),
            BFieldElement::new(133700),
            BFieldElement::new(19990),
            BFieldElement::new(88888888),
        ];
        prop_set(DataType::Digest, list_address, 233, push_value, 58, 3000);
    }

    #[should_panic]
    #[test]
    fn list_u32_n_is_five_set_out_of_bounds_a() {
        let list_address = BFieldElement::new(48);
        let insert_value = vec![BFieldElement::new(1337)];
        let index = 20;
        prop_set(DataType::U64, list_address, 20, insert_value, index, 21);
    }

    #[should_panic]
    #[test]
    fn list_u32_n_is_five_set_out_of_bounds_b() {
        let list_address = BFieldElement::new(48);
        let insert_value = vec![BFieldElement::new(1337)];
        let index = 21;
        prop_set(DataType::U64, list_address, 20, insert_value, index, 22);
    }

    fn prop_set(
        data_type: DataType,
        list_address: BFieldElement,
        init_list_length: u32,
        push_value: Vec<BFieldElement>,
        index: u32,
        capacity: u32,
    ) {
        let expected_end_stack = vec![get_init_tvm_stack()].concat();
        let mut init_stack = get_init_tvm_stack();

        for i in 0..data_type.get_size() {
            init_stack.push(push_value[data_type.get_size() - 1 - i]);
        }
        init_stack.push(list_address);
        init_stack.push(BFieldElement::new(index as u64));

        let mut vm_memory = HashMap::default();

        // Insert length indicator of list, lives on offset = 0 from `list_address`
        safe_insert_random_list(
            &data_type,
            list_address,
            capacity,
            init_list_length as usize,
            &mut vm_memory,
        );

        let _execution_result = rust_tasm_equivalence_prop::<SafeSet>(
            SafeSet(data_type.clone()),
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
        for i in 0..data_type.get_size() {
            assert_eq!(
                push_value[i],
                vm_memory[&BFieldElement::new(
                    list_address.value()
                        + 2
                        + data_type.get_size() as u64 * index as u64
                        + i as u64
                )]
            );
        }
    }
}
