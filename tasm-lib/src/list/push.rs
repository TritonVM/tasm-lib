use std::collections::HashMap;

use itertools::Itertools;
use num::One;
use rand::prelude::*;
use triton_vm::prelude::*;
use twenty_first::math::other::random_elements;

use crate::data_type::DataType;
use crate::empty_stack;
use crate::library::Library;
use crate::rust_shadowing_helper_functions::list::untyped_insert_random_list;
use crate::traits::deprecated_snippet::DeprecatedSnippet;
use crate::InitVmState;

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct Push {
    pub element_type: DataType,
}

impl Push {
    pub fn new(element_type: DataType) -> Self {
        Self { element_type }
    }

    fn write_type_to_mem(&self) -> Vec<LabelledInstruction> {
        let data_size = self.element_type.stack_size();
        let num_full_chunk_writes = data_size / 5;
        let num_remaining_words = data_size % 5;
        let mut instructions = vec![triton_instr!(write_mem 5); num_full_chunk_writes];
        if num_remaining_words > 0 {
            instructions.extend(triton_asm!(write_mem {
                num_remaining_words
            }));
        }
        instructions
    }
}

impl DeprecatedSnippet for Push {
    fn entrypoint_name(&self) -> String {
        format!(
            "tasmlib_list_push___{}",
            self.element_type.label_friendly_name()
        )
    }

    fn input_field_names(&self) -> Vec<String> {
        let element_size = self.element_type.stack_size();

        // _ *list elem{N - 1} … elem{0}
        let mut ret = vec!["*list".to_string()];
        for i in 0..element_size {
            ret.push(format!("element_{}", element_size - 1 - i));
        }

        ret
    }

    fn input_types(&self) -> Vec<DataType> {
        vec![
            DataType::List(Box::new(self.element_type.clone())),
            self.element_type.clone(),
        ]
    }

    fn output_field_names(&self) -> Vec<String> {
        vec![]
    }

    fn output_types(&self) -> Vec<DataType> {
        vec![]
    }

    fn stack_diff(&self) -> isize {
        -(self.element_type.stack_size() as isize) - 1
    }

    /// push one `self.element_type` element to the list in memory
    fn function_code(&self, _library: &mut Library) -> String {
        let element_size = self.element_type.stack_size();

        let write_elements_to_memory = self.write_type_to_mem();
        let mul_with_size = match element_size {
            1 => vec![],
            _ => triton_asm!(push {element_size} mul),
        };

        let entry_point = self.entrypoint_name();
        triton_asm!(
            // BEFORE: _ *list elem{N - 1} … elem{0}
            // AFTER:  _
            {entry_point}:
                dup {element_size}  // _ *list elem{N - 1} … elem{0} *list
                read_mem 1          // _ *list elem{N - 1} … elem{0} length *(list - 1)
                swap 1              // _ *list elem{N - 1} … elem{0} *(list - 1) length
                {&mul_with_size}    // _ *list elem{N - 1} … elem{0} *(list - 1) (length * elem_size)

                // set top of stack to offset for where elements will be stored
                push 2
                add
                add                 // _ *list elem{N - 1} … elem{0} *(list + length * elem_size + 1)

                {&write_elements_to_memory}
                                    // _ *list *(list + (length + 1) * elem_size + 1)
                pop 1               // _ *list

                // Increase length indicator by one
                read_mem 1          // _ length *(list - 1)
                push 1              // _ length *(list - 1) 1
                add                 // _ length *list
                swap 1              // _ *list length
                push 1              // _ *list length 1
                add                 // _ *list (length + 1)
                swap 1              // _ (length + 1) *list
                write_mem 1         // _ *(list + 1)
                pop 1               // _
                return
        )
        .iter()
        .join("\n")
    }

    fn crash_conditions(&self) -> Vec<String> {
        vec![]
    }

    fn gen_input_states(&self) -> Vec<InitVmState> {
        vec![
            prepare_state(&self.element_type),
            prepare_state(&self.element_type),
            prepare_state(&self.element_type),
            prepare_state(&self.element_type),
        ]
    }

    fn common_case_input_state(&self) -> InitVmState {
        prepare_state(&self.element_type)
    }

    fn worst_case_input_state(&self) -> InitVmState {
        prepare_state(&self.element_type)
    }

    fn rust_shadowing(
        &self,
        stack: &mut Vec<BFieldElement>,
        _std_in: Vec<BFieldElement>,
        _secret_in: Vec<BFieldElement>,
        memory: &mut HashMap<BFieldElement, BFieldElement>,
    ) {
        let list_address = stack[stack.len() - 1 - self.element_type.stack_size()];
        let initial_list_length = memory[&list_address];

        let mut next_free_address = list_address
            + initial_list_length * BFieldElement::new(self.element_type.stack_size() as u64)
            + BFieldElement::one();

        for _ in 0..self.element_type.stack_size() {
            let elem = stack.pop().unwrap();
            memory.insert(next_free_address, elem);
            next_free_address += BFieldElement::one();
        }

        // Remove list pointer
        stack.pop().unwrap();

        // Update length indicator
        memory.insert(list_address, initial_list_length + BFieldElement::one());
    }
}

fn prepare_state(element_type: &DataType) -> InitVmState {
    let list_pointer: u32 = random();
    let list_pointer = BFieldElement::new(list_pointer as u64);
    let init_length: usize = thread_rng().gen_range(0..100);
    let mut stack = empty_stack();
    stack.push(list_pointer);
    let mut push_value: Vec<BFieldElement> = random_elements(element_type.stack_size());
    while let Some(element) = push_value.pop() {
        stack.push(element);
    }

    let mut memory = HashMap::default();
    untyped_insert_random_list(
        list_pointer,
        init_length,
        &mut memory,
        element_type.stack_size(),
    );
    InitVmState::with_stack_and_memory(stack, memory)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::empty_stack;
    use crate::test_helpers::test_rust_equivalence_given_input_values_deprecated;
    use crate::test_helpers::test_rust_equivalence_multiple_deprecated;

    #[test]
    fn new_snippet_test() {
        fn test_rust_equivalence_and_export(element_type: DataType) {
            test_rust_equivalence_multiple_deprecated(&Push { element_type }, true);
        }

        test_rust_equivalence_and_export(DataType::Bool);
        test_rust_equivalence_and_export(DataType::U64);
        test_rust_equivalence_and_export(DataType::Xfe);
        test_rust_equivalence_and_export(DataType::Digest);
    }

    #[test]
    fn list_u32_n_is_one_push() {
        let list_address = BFieldElement::new(48);
        let push_value = vec![BFieldElement::new(1337)];
        prop_push(DataType::Bfe, list_address, 20, push_value);
    }

    #[test]
    fn list_u32_n_is_two_push() {
        let list_address = BFieldElement::new(1841);
        let push_value = vec![BFieldElement::new(133700), BFieldElement::new(32)];
        prop_push(DataType::U64, list_address, 20, push_value);
    }

    #[test]
    fn list_u32_n_is_five_push() {
        let list_address = BFieldElement::new(558);
        let push_value = vec![
            BFieldElement::new(133700),
            BFieldElement::new(32),
            BFieldElement::new(133700),
            BFieldElement::new(19990),
            BFieldElement::new(88888888),
        ];
        prop_push(DataType::Digest, list_address, 2313, push_value);
    }

    fn prop_push(
        element_type: DataType,
        list_address: BFieldElement,
        init_list_length: u32,
        push_value: Vec<BFieldElement>,
    ) {
        assert_eq!(
            element_type.stack_size(),
            push_value.len(),
            "Push value length must match data size"
        );
        let expected_end_stack = empty_stack();
        let mut init_stack = empty_stack();
        init_stack.push(list_address);

        for i in 0..element_type.stack_size() {
            init_stack.push(push_value[element_type.stack_size() - 1 - i]);
        }
        let mut memory = HashMap::default();

        untyped_insert_random_list(
            list_address,
            init_list_length as usize,
            &mut memory,
            element_type.stack_size(),
        );

        let memory = test_rust_equivalence_given_input_values_deprecated(
            &Push {
                element_type: element_type.clone(),
            },
            &init_stack,
            &[],
            memory,
            Some(&expected_end_stack),
        )
        .ram;

        // Verify that length indicator has increased by one
        assert_eq!(
            BFieldElement::new((init_list_length + 1) as u64),
            memory[&list_address]
        );

        // verify that value was inserted at expected place
        for i in 0..element_type.stack_size() {
            assert_eq!(
                push_value[i],
                memory[&BFieldElement::new(
                    list_address.value()
                        + 1
                        + element_type.stack_size() as u64 * init_list_length as u64
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
    fn push_benchmark() {
        bench_and_write(Push::new(DataType::Digest));
    }
}
