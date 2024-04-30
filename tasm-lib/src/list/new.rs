use std::collections::HashMap;

use itertools::Itertools;
use triton_vm::prelude::*;

use crate::data_type::DataType;
use crate::empty_stack;
use crate::library::Library;
use crate::rust_shadowing_helper_functions::list::list_new;
use crate::traits::deprecated_snippet::DeprecatedSnippet;
use crate::traits::function::Function;
use crate::InitVmState;

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct New {
    pub element_type: DataType,
}

impl New {
    #[allow(clippy::self_named_constructors)] // 🤷
    pub fn new(data_type: DataType) -> Self {
        Self {
            element_type: data_type,
        }
    }
}

impl DeprecatedSnippet for New {
    fn entrypoint_name(&self) -> String {
        format!(
            "tasmlib_list_new___{}",
            self.element_type.label_friendly_name()
        )
    }

    fn input_field_names(&self) -> Vec<String> {
        vec![]
    }

    fn input_types(&self) -> Vec<DataType> {
        vec![]
    }

    fn output_field_names(&self) -> Vec<String> {
        vec!["list_pointer".to_string()]
    }

    fn output_types(&self) -> Vec<DataType> {
        vec![DataType::List(Box::new(self.element_type.clone()))]
    }

    fn stack_diff(&self) -> isize {
        1
    }

    fn function_code(&self, library: &mut Library) -> String {
        let entrypoint = self.entrypoint_name();
        let dyn_malloc = library.import(Box::new(crate::dyn_malloc::DynMalloc));

        triton_asm!(
            // BEFORE: _
            // AFTER:  _ *list
            {entrypoint}:
                call {dyn_malloc}
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
        vec![]
    }

    fn gen_input_states(&self) -> Vec<InitVmState> {
        vec![
            prepare_state(0),
            prepare_state(1),
            prepare_state(2),
            prepare_state(3),
            prepare_state(5),
            prepare_state(102),
        ]
    }

    fn common_case_input_state(&self) -> InitVmState {
        prepare_state(2)
    }

    fn worst_case_input_state(&self) -> InitVmState {
        prepare_state(1000000)
    }

    fn rust_shadowing(
        &self,
        stack: &mut Vec<BFieldElement>,
        _std_in: Vec<BFieldElement>,
        _secret_in: Vec<BFieldElement>,
        memory: &mut HashMap<BFieldElement, BFieldElement>,
    ) {
        crate::dyn_malloc::DynMalloc.rust_shadow(stack, memory);

        let list_pointer = stack.pop().unwrap();
        list_new(list_pointer, memory);

        stack.push(list_pointer);
    }
}

fn prepare_state(capacity: u32) -> InitVmState {
    let mut stack = empty_stack();
    stack.push(BFieldElement::new(capacity as u64));
    InitVmState::with_stack(stack)
}

#[cfg(test)]
mod tests {
    use crate::test_helpers::test_rust_equivalence_multiple_deprecated;

    use super::*;

    #[test]
    fn new_snippet_test() {
        fn test_rust_equivalence_and_export(data_type: DataType) {
            test_rust_equivalence_multiple_deprecated(&New::new(data_type), true);
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
    use crate::snippet_bencher::bench_and_write;

    use super::*;

    #[test]
    fn new_benchmark() {
        bench_and_write(New::new(DataType::Digest));
    }
}
