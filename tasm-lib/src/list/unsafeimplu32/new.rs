use crate::twenty_first::shared_math::b_field_element::BFieldElement;
use itertools::Itertools;
use std::collections::HashMap;
use triton_vm::triton_asm;

use crate::data_type::DataType;
use crate::library::Library;
use crate::rust_shadowing_helper_functions::unsafe_list::unsafe_list_new;
use crate::traits::deprecated_snippet::DeprecatedSnippet;
use crate::{empty_stack, ExecutionState};

#[derive(Clone, Debug)]
pub struct UnsafeNew {
    pub data_type: DataType,
}

impl DeprecatedSnippet for UnsafeNew {
    fn entrypoint_name(&self) -> String {
        format!(
            "tasm_list_unsafeimplu32_new___{}",
            self.data_type.label_friendly_name()
        )
    }

    fn input_field_names(&self) -> Vec<String> {
        vec!["capacity".to_string()]
    }

    fn input_types(&self) -> Vec<DataType> {
        vec![DataType::U32]
    }

    fn output_field_names(&self) -> Vec<String> {
        vec!["list_pointer".to_string()]
    }

    fn output_types(&self) -> Vec<DataType> {
        vec![DataType::List(Box::new(self.data_type.clone()))]
    }

    fn stack_diff(&self) -> isize {
        0
    }

    fn function_code(&self, library: &mut Library) -> String {
        let entrypoint = self.entrypoint_name();

        // Data structure for `list::safeimplu32` is: [length, element0, element1, ...]
        let element_size = self.data_type.stack_size();
        let dyn_alloc = library.import(Box::new(crate::dyn_malloc::DynMalloc));

        let mul_with_size = match element_size {
            1 => vec![],
            _ => triton_asm!(push {element_size} mul),
        };

        triton_asm!(
            // BEFORE: _ capacity
            // AFTER:  _ *list
            {entrypoint}:
                // _ capacity

                // convert capacity in number of elements to required number of VM words
                {&mul_with_size}
                // _ (capacity_in_bfes)

                push 1
                add
                // _ (words to allocate)

                call {dyn_alloc}
                // _ *list

                // Write initial length = 0 to `*list`
                push 0
                swap 1
                write_mem 1
                push -1
                add
                // _ *list

                return
        )
        .iter()
        .join("\n")
    }

    fn crash_conditions(&self) -> Vec<String> {
        vec!["Requested list size exceeds u32::MAX bfe words".to_string()]
    }

    fn gen_input_states(&self) -> Vec<ExecutionState> {
        vec![
            prepare_state(0),
            prepare_state(1),
            prepare_state(2),
            prepare_state(3),
            prepare_state(5),
            prepare_state(102),
        ]
    }

    fn common_case_input_state(&self) -> ExecutionState {
        prepare_state(2)
    }

    fn worst_case_input_state(&self) -> ExecutionState {
        prepare_state(1000000)
    }

    fn rust_shadowing(
        &self,
        stack: &mut Vec<BFieldElement>,
        std_in: Vec<BFieldElement>,
        secret_in: Vec<BFieldElement>,
        memory: &mut HashMap<BFieldElement, BFieldElement>,
    ) {
        let capacity_in_elements = stack.pop().unwrap().value() as usize;
        let capacity_in_bfes = capacity_in_elements * self.data_type.stack_size();
        stack.push(BFieldElement::new(capacity_in_bfes as u64));
        crate::dyn_malloc::DynMalloc.rust_shadowing(stack, std_in, secret_in, memory);

        let list_pointer = stack.pop().unwrap();
        unsafe_list_new(list_pointer, memory);

        stack.push(list_pointer);
    }
}

fn prepare_state(capacity: u32) -> ExecutionState {
    let mut stack = empty_stack();
    stack.push(BFieldElement::new(capacity as u64));
    ExecutionState::with_stack(stack)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_helpers::test_rust_equivalence_multiple_deprecated;

    #[test]
    fn new_snippet_test() {
        fn test_rust_equivalence_and_export(data_type: DataType) {
            test_rust_equivalence_multiple_deprecated(&UnsafeNew { data_type }, true);
        }

        test_rust_equivalence_and_export(DataType::Bool);
        test_rust_equivalence_and_export(DataType::Bfe);
        test_rust_equivalence_and_export(DataType::U32);
        test_rust_equivalence_and_export(DataType::U64);
        test_rust_equivalence_and_export(DataType::Xfe);
        test_rust_equivalence_and_export(DataType::Digest);
    }
}

#[cfg(test)]
mod benches {
    use super::*;
    use crate::snippet_bencher::bench_and_write;

    #[test]
    fn unsafe_new_benchmark() {
        let data_type = DataType::Digest;
        bench_and_write(UnsafeNew { data_type });
    }
}
