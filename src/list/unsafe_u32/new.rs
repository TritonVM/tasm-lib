use num::One;
use std::collections::HashMap;
use twenty_first::shared_math::b_field_element::BFieldElement;

use crate::library::Library;
use crate::rust_shadowing_helper_functions::unsafe_list::unsafe_list_new;
use crate::snippet::{DataType, Snippet};
use crate::{get_init_tvm_stack, ExecutionState};

const DEFAULT_LIST_CAPACITY: usize = 64;

#[derive(Clone)]
pub struct UnsafeNew(pub DataType);

impl Snippet for UnsafeNew {
    fn entrypoint(&self) -> String {
        format!("tasm_list_unsafe_u32_new_{}", self.0)
    }

    fn inputs(&self) -> Vec<String>
    where
        Self: Sized,
    {
        vec![]
    }

    fn input_types(&self) -> Vec<DataType> {
        vec![]
    }

    fn output_types(&self) -> Vec<DataType> {
        vec![DataType::List(Box::new(self.0.clone()))]
    }

    fn outputs(&self) -> Vec<String> {
        vec!["list_pointer".to_string()]
    }

    fn stack_diff(&self) -> isize
    where
        Self: Sized,
    {
        1
    }

    fn function_body(&self, library: &mut Library) -> String {
        let entrypoint = self.entrypoint();

        // Allocate memory for the returned auth path for the newly inserted element
        // Warning: This auth path is only allocated *once* even though the code is called multiple times.
        // So if this function is called multiple times, previous values are overwritten.
        let element_size = self.0.get_size();
        let static_list_pointer = library.kmalloc(element_size * DEFAULT_LIST_CAPACITY + 1);

        format!(
            "
                // BEFORE: _
                // AFTER: _ *list
                {entrypoint}:
                    push {static_list_pointer}
                    // _ *list

                    push 0
                    write_mem
                    // _ *list

                    return
                    "
        )
    }

    fn crash_conditions() -> Vec<String>
    where
        Self: Sized,
    {
        vec![]
    }

    fn gen_input_states(&self) -> Vec<ExecutionState>
    where
        Self: Sized,
    {
        fn prepare_state() -> ExecutionState {
            ExecutionState::with_stack(get_init_tvm_stack())
        }

        vec![
            prepare_state(),
            prepare_state(),
            prepare_state(),
            prepare_state(),
            prepare_state(),
            prepare_state(),
        ]
    }

    fn rust_shadowing(
        &self,
        stack: &mut Vec<BFieldElement>,
        _std_in: Vec<BFieldElement>,
        _secret_in: Vec<BFieldElement>,
        memory: &mut HashMap<BFieldElement, BFieldElement>,
    ) where
        Self: Sized,
    {
        let list_pointer = BFieldElement::one();
        unsafe_list_new(list_pointer, memory);
        stack.push(list_pointer);
    }

    fn common_case_input_state(&self) -> ExecutionState
    where
        Self: Sized,
    {
        ExecutionState::with_stack(get_init_tvm_stack())
    }

    fn worst_case_input_state(&self) -> ExecutionState
    where
        Self: Sized,
    {
        ExecutionState::with_stack(get_init_tvm_stack())
    }
}

#[cfg(test)]
mod tests {
    use crate::{snippet_bencher::bench_and_write, test_helpers::rust_tasm_equivalence_prop_new};

    use super::*;

    #[test]
    fn new_snippet_test() {
        rust_tasm_equivalence_prop_new(UnsafeNew(DataType::U32));
        rust_tasm_equivalence_prop_new(UnsafeNew(DataType::U64));
        rust_tasm_equivalence_prop_new(UnsafeNew(DataType::XFE));
        rust_tasm_equivalence_prop_new(UnsafeNew(DataType::Digest));
    }

    #[test]
    fn unsafe_new_benchmark() {
        bench_and_write(UnsafeNew(DataType::Digest));
    }
}
