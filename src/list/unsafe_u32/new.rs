use std::collections::HashMap;
use twenty_first::shared_math::b_field_element::BFieldElement;

use crate::rust_shadowing_helper_functions::unsafe_list::unsafe_list_new;
use crate::snippet::{DataType, Snippet};
use crate::snippet_state::SnippetState;
use crate::{get_init_tvm_stack, ExecutionState};

#[derive(Clone, Debug)]
pub struct UnsafeNew(pub DataType);

impl Snippet for UnsafeNew {
    fn entrypoint(&self) -> String {
        format!("tasm_list_unsafe_u32_new_{}", self.0.label_friendly_name())
    }

    fn inputs(&self) -> Vec<String> {
        vec!["capacity".to_string()]
    }

    fn input_types(&self) -> Vec<DataType> {
        vec![DataType::U32]
    }

    fn output_types(&self) -> Vec<DataType> {
        vec![DataType::List(Box::new(self.0.clone()))]
    }

    fn outputs(&self) -> Vec<String> {
        vec!["list_pointer".to_string()]
    }

    fn stack_diff(&self) -> isize {
        0
    }

    fn function_code(&self, library: &mut SnippetState) -> String {
        let entrypoint = self.entrypoint();

        // Data structure for `list::safe_u32` is: [length, element0, element1, ...]
        let element_size = self.0.get_size();
        let dyn_alloc = library.import(Box::new(crate::dyn_malloc::DynMalloc));

        let mul_with_size = if element_size != 1 {
            format!("push {element_size}\n mul\n")
        } else {
            String::default()
        };

        format!(
            "
            // BEFORE: _ capacity
            // AFTER: _ *list
            {entrypoint}:
                // _ capacity

                // Convert capacity in number of elements to number of VM words required for that list
                {mul_with_size}
                // _ (capacity_in_bfes)

                push 1
                add
                // _ (words to allocate)

                call {dyn_alloc}
                // _ *list

                // Write initial length = 0 to `*list`
                push 0
                write_mem
                // _ *list

                return
            "
        )
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

    fn rust_shadowing(
        &self,
        stack: &mut Vec<BFieldElement>,
        std_in: Vec<BFieldElement>,
        secret_in: Vec<BFieldElement>,
        memory: &mut HashMap<BFieldElement, BFieldElement>,
    ) {
        // let list_pointer = BFieldElement::one();
        let capacity_in_elements = stack.pop().unwrap().value() as usize;
        let capacity_in_bfes = capacity_in_elements * self.0.get_size();
        stack.push(BFieldElement::new(capacity_in_bfes as u64));
        crate::dyn_malloc::DynMalloc.rust_shadowing(stack, std_in, secret_in, memory);

        let list_pointer = stack.pop().unwrap();
        unsafe_list_new(list_pointer, memory);

        stack.push(list_pointer);
    }

    fn common_case_input_state(&self) -> ExecutionState {
        prepare_state(2)
    }

    fn worst_case_input_state(&self) -> ExecutionState {
        prepare_state(1000000)
    }
}

fn prepare_state(capacity: u32) -> ExecutionState {
    let mut stack = get_init_tvm_stack();
    stack.push(BFieldElement::new(capacity as u64));
    ExecutionState::with_stack(stack)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_helpers::test_rust_equivalence_multiple;

    #[test]
    fn new_snippet_test() {
        test_rust_equivalence_multiple(&UnsafeNew(DataType::Bool), true);
        test_rust_equivalence_multiple(&UnsafeNew(DataType::BFE), true);
        test_rust_equivalence_multiple(&UnsafeNew(DataType::U32), true);
        test_rust_equivalence_multiple(&UnsafeNew(DataType::XFE), true);
        test_rust_equivalence_multiple(&UnsafeNew(DataType::Digest), true);

        test_rust_equivalence_multiple(&UnsafeNew(DataType::U64), true);
        // let _execution_states = rust_tasm_equivalence_prop_new(&UnsafeNew(DataType::U64), true);
        // let dyn_malloc_address = BFieldElement::new(DYN_MALLOC_ADDRESS as u64);
        // for execution_state in execution_states {
        //     assert_eq!(execution_state.final_stack.len(), OP_STACK_REG_COUNT + 1);
        //     assert!(execution_state.final_stack[0..OP_STACK_REG_COUNT]
        //         .iter()
        //         .all(|x| x.value() == 0));
        //     assert!(!execution_state.final_ram.is_empty());
        //     assert!(execution_state.final_ram.contains_key(&dyn_malloc_address));
        //     let final_dyn_malloc_value =
        //         execution_state.final_ram[&dyn_malloc_address].value() as usize;
        //     assert!(
        //         final_dyn_malloc_value % DataType::U64.get_size() == 2 % DataType::U64.get_size(),
        //         "One word for dyn malloc, one word for length, rest for elements. Final dyn malloc value was: {final_dyn_malloc_value}",
        //     );
        // }
    }
}

#[cfg(test)]
mod benches {
    use super::*;
    use crate::snippet_bencher::bench_and_write;

    #[test]
    fn unsafe_new_benchmark() {
        bench_and_write(UnsafeNew(DataType::Digest));
    }
}
