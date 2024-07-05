use std::collections::HashMap;

use itertools::Itertools;
use rand::prelude::*;
use triton_vm::prelude::*;

use crate::data_type::DataType;
use crate::empty_stack;
use crate::library::Library;
use crate::rust_shadowing_helper_functions::list::untyped_insert_random_list;
use crate::traits::deprecated_snippet::DeprecatedSnippet;
use crate::InitVmState;

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct SetLength {
    pub element_type: DataType,
}

impl SetLength {
    pub fn new(data_type: DataType) -> Self {
        Self {
            element_type: data_type,
        }
    }
}

impl DeprecatedSnippet for SetLength {
    fn entrypoint_name(&self) -> String {
        format!(
            "tasmlib_list_set_length___{}",
            self.element_type.label_friendly_name()
        )
    }

    fn input_field_names(&self) -> Vec<String> {
        vec!["*list".to_string(), "list_length".to_string()]
    }

    fn input_types(&self) -> Vec<DataType> {
        vec![
            DataType::List(Box::new(self.element_type.clone())),
            DataType::U32,
        ]
    }

    fn output_field_names(&self) -> Vec<String> {
        vec!["*list".to_string()]
    }

    fn output_types(&self) -> Vec<DataType> {
        vec![DataType::List(Box::new(self.element_type.clone()))]
    }

    fn stack_diff(&self) -> isize {
        // pops list_length but leaves list_pointer on stack
        -1
    }

    fn function_code(&self, _library: &mut Library) -> String {
        let entry_point = self.entrypoint_name();
        // It is assumed that the new length is a valid u32 value
        triton_asm!(
                // BEFORE: _ *list list_length
                // AFTER:  _ *list
                {entry_point}:
                    // _ *list list_length
                    swap 1

                    // _ list_length *list

                    write_mem 1
                    // _ (*list + 1)

                    push -1
                    add
                    // _ *list

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
        let new_length = stack.pop().unwrap();
        let list_address = stack.pop().unwrap();

        memory.insert(list_address, new_length);

        stack.push(list_address);
    }
}

fn prepare_state(data_type: &DataType) -> InitVmState {
    let list_pointer: BFieldElement = random();
    let old_length: usize = thread_rng().gen_range(0..100);
    let new_length: usize = thread_rng().gen_range(0..100);
    let mut stack = empty_stack();
    stack.push(list_pointer);
    stack.push(BFieldElement::new(new_length as u64));
    let mut memory = HashMap::default();
    untyped_insert_random_list(
        list_pointer,
        old_length,
        &mut memory,
        data_type.stack_size(),
    );
    InitVmState::with_stack_and_memory(stack, memory)
}

#[cfg(test)]
mod tests {
    use crate::empty_stack;
    use crate::test_helpers::test_rust_equivalence_given_input_values_deprecated;
    use crate::test_helpers::test_rust_equivalence_multiple_deprecated;

    use super::*;

    #[test]
    fn new_snippet_test() {
        test_rust_equivalence_multiple_deprecated(
            &SetLength {
                element_type: DataType::Xfe,
            },
            true,
        );
    }

    #[test]
    fn list_u32_n_is_one_push() {
        let list_address = BFieldElement::new(58);
        prop_set_length(DataType::Bfe, list_address, 22, 14);
    }

    #[test]
    fn list_u32_n_is_five_push() {
        let list_address = BFieldElement::new(558);
        prop_set_length(DataType::Digest, list_address, 2313, 14);
        prop_set_length(DataType::Digest, list_address, 14, 0);
        prop_set_length(DataType::Digest, list_address, 0, 0);
    }

    fn prop_set_length(
        data_type: DataType,
        list_address: BFieldElement,
        init_list_length: u32,
        new_list_length: u32,
    ) {
        let expected_end_stack = [empty_stack(), vec![list_address]].concat();
        let mut init_stack = empty_stack();
        init_stack.push(list_address);
        init_stack.push(BFieldElement::new(new_list_length as u64));

        let mut vm_memory = HashMap::default();

        // Insert length indicator of list, lives on offset = 0 from `list_address`
        vm_memory.insert(list_address, BFieldElement::new(init_list_length as u64));

        let memory = test_rust_equivalence_given_input_values_deprecated(
            &SetLength {
                element_type: data_type,
            },
            &init_stack,
            &[],
            vm_memory,
            Some(&expected_end_stack),
        )
        .ram;

        // Verify that length indicator has been updated
        assert_eq!(
            BFieldElement::new(new_list_length as u64),
            memory[&list_address]
        );
    }
}

#[cfg(test)]
mod benches {
    use crate::snippet_bencher::bench_and_write;

    use super::*;

    #[test]
    fn set_length_benchmark() {
        bench_and_write(SetLength::new(DataType::Digest));
    }
}
