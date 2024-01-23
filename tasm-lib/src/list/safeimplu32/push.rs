use std::collections::HashMap;

use itertools::Itertools;
use num::One;
use rand::thread_rng;
use rand::Rng;
use triton_vm::prelude::*;
use twenty_first::shared_math::other::random_elements;

use crate::data_type::DataType;
use crate::empty_stack;
use crate::library::Library;
use crate::list::safeimplu32::SAFE_LIST_ELEMENT_CAPACITY;
use crate::rust_shadowing_helper_functions::safe_list::safe_insert_random_list;
use crate::rust_shadowing_helper_functions::safe_list::safe_list_push;
use crate::traits::deprecated_snippet::DeprecatedSnippet;
use crate::ExecutionState;

#[derive(Clone, Debug)]
pub struct SafePush {
    pub data_type: DataType,
}

impl DeprecatedSnippet for SafePush {
    fn entrypoint_name(&self) -> String {
        format!(
            "tasm_list_safeimplu32_push___{}",
            self.data_type.label_friendly_name()
        )
    }

    fn input_field_names(&self) -> Vec<String> {
        let element_size = self.data_type.stack_size();

        // _ *list elem{N - 1} elem{N - 2} … elem{0}
        let mut ret = vec!["*list".to_string()];
        for i in 0..element_size {
            ret.push(format!("element_{}", element_size - 1 - i));
        }

        ret
    }

    fn input_types(&self) -> Vec<DataType> {
        vec![
            DataType::List(Box::new(self.data_type.clone())),
            self.data_type.clone(),
        ]
    }

    fn output_field_names(&self) -> Vec<String> {
        vec![]
    }

    fn output_types(&self) -> Vec<DataType> {
        vec![]
    }

    fn stack_diff(&self) -> isize {
        -(self.data_type.stack_size() as isize) - 1
    }

    fn function_code(&self, _library: &mut Library) -> String {
        let element_size = self.data_type.stack_size();
        let mul_with_size = match element_size {
            1 => vec![],
            _ => triton_asm!(push {element_size} mul),
        };

        triton_asm!(
            // BEFORE: _ *list [word; N]
            // AFTER:  _
            {self.entrypoint_name()}:
                dup {element_size}  // _ *list [word; N] *list
                push 1
                add                 // _ *list [word; N] (*list + 1)
                read_mem 2          // _ *list [word; N] capacity length (*list - 1)
                swap 2              // _ *list [word; N] (*list - 1) length capacity
                dup 1               // _ *list [word; N] (*list - 1) length capacity length
                lt                  // _ *list [word; N] (*list - 1) length (length < capacity)
                assert              // _ *list [word; N] (*list - 1) length
                {&mul_with_size}    // _ *list [word; N] (*list - 1) (length * elem_size)
                add
                push 3
                add                 // _ *list [word; N] (*list + length * elem_size + 2)
                {&self.data_type.write_value_to_memory_pop_pointer()}
                                    // _ *list

                                    // increment length indicator
                read_mem 1          // _ length (*list - 1)
                push 1
                add                 // _ length *list
                swap 1
                push 1
                add
                swap 1              // _ (length + 1) *list
                write_mem 1         // _ (*list + 1)
                pop 1               // _
                return
        )
        .iter()
        .join("\n")
    }

    fn crash_conditions(&self) -> Vec<String> {
        vec!["New length exceeds capacity".to_string()]
    }

    fn gen_input_states(&self) -> Vec<ExecutionState> {
        vec![
            prepare_execution_state(&self.data_type, 1, 0),
            prepare_execution_state(&self.data_type, 2, 0),
            prepare_execution_state(&self.data_type, SAFE_LIST_ELEMENT_CAPACITY, 0),
            prepare_execution_state(
                &self.data_type,
                SAFE_LIST_ELEMENT_CAPACITY,
                thread_rng().gen_range(0..100),
            ),
            prepare_execution_state(
                &self.data_type,
                SAFE_LIST_ELEMENT_CAPACITY,
                thread_rng().gen_range(0..100),
            ),
            prepare_execution_state(
                &self.data_type,
                SAFE_LIST_ELEMENT_CAPACITY,
                thread_rng().gen_range(0..100),
            ),
        ]
    }

    fn common_case_input_state(&self) -> ExecutionState {
        prepare_execution_state(&self.data_type, SAFE_LIST_ELEMENT_CAPACITY, 1 << 5)
    }

    fn worst_case_input_state(&self) -> ExecutionState {
        prepare_execution_state(&self.data_type, SAFE_LIST_ELEMENT_CAPACITY, 1 << 6)
    }

    fn rust_shadowing(
        &self,
        stack: &mut Vec<BFieldElement>,
        _std_in: Vec<BFieldElement>,
        _secret_in: Vec<BFieldElement>,
        memory: &mut HashMap<BFieldElement, BFieldElement>,
    ) {
        let mut elements: Vec<BFieldElement> = vec![];
        for _ in 0..self.data_type.stack_size() {
            elements.push(stack.pop().unwrap());
        }

        let list_pointer = stack.pop().unwrap();
        let initial_list_length = memory[&list_pointer];
        safe_list_push(list_pointer, elements, memory, self.data_type.stack_size());

        // Update length indicator
        memory.insert(list_pointer, initial_list_length + BFieldElement::one());
    }
}

fn prepare_execution_state(
    data_type: &DataType,
    capacity: u32,
    init_length: usize,
) -> ExecutionState {
    let list_pointer: BFieldElement =
        BFieldElement::new(thread_rng().gen_range(0..u32::MAX as u64));
    let mut stack = empty_stack();
    stack.push(list_pointer);
    let mut push_value: Vec<BFieldElement> = random_elements(data_type.stack_size());
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
        test_helpers::{
            test_rust_equivalence_given_input_values_deprecated,
            test_rust_equivalence_multiple_deprecated,
        },
        DIGEST_LENGTH,
    };

    use super::*;

    #[test]
    fn new_snippet_test() {
        fn test_rust_equivalence_and_export(data_type: DataType) {
            test_rust_equivalence_multiple_deprecated(&SafePush { data_type }, true);
        }

        test_rust_equivalence_and_export(DataType::Bool);
        test_rust_equivalence_and_export(DataType::U32);
        test_rust_equivalence_and_export(DataType::U64);
        test_rust_equivalence_and_export(DataType::Bfe);
        test_rust_equivalence_and_export(DataType::Xfe);
        test_rust_equivalence_and_export(DataType::Digest);
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
            DataType::Xfe,
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
            DataType::Xfe,
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
        let data_type_stack_size = data_type.stack_size();
        assert_eq!(
            data_type_stack_size,
            push_value.len(),
            "Push value length must match data size"
        );
        let expected_end_stack = empty_stack();
        let mut init_stack = empty_stack();
        init_stack.push(list_address);

        for i in 0..data_type_stack_size {
            init_stack.push(push_value[data_type_stack_size - 1 - i]);
        }
        let mut memory = HashMap::default();

        safe_insert_random_list(
            &data_type,
            list_address,
            list_capacity,
            init_list_length as usize,
            &mut memory,
        );

        let memory = test_rust_equivalence_given_input_values_deprecated(
            &SafePush { data_type },
            &init_stack,
            &[],
            memory,
            1,
            Some(&expected_end_stack),
        )
        .final_ram;

        // Verify that length indicator has increased by one
        assert_eq!(
            BFieldElement::new((init_list_length + 1) as u64),
            memory[&list_address]
        );

        // verify that value was inserted at expected place
        let initial_list_offset = data_type_stack_size as u64 * init_list_length as u64 + 2;
        for (i, pushed_element) in push_value.into_iter().enumerate() {
            let offset = initial_list_offset + i as u64;
            let address = list_address + BFieldElement::new(offset);
            assert_eq!(pushed_element, memory[&address]);
        }
    }
}

#[cfg(test)]
mod benches {
    use crate::snippet_bencher::bench_and_write;

    use super::*;

    #[test]
    fn safe_push_benchmark() {
        let data_type = DataType::Digest;
        bench_and_write(SafePush { data_type });
    }
}
