use std::collections::HashMap;

use rand::{random, thread_rng, Rng};
use twenty_first::shared_math::b_field_element::BFieldElement;

use crate::rust_shadowing_helper_functions::unsafe_list::unsafe_insert_random_list;
use crate::snippet::{DataType, Snippet};
use crate::snippet_state::SnippetState;
use crate::{get_init_tvm_stack, ExecutionState};

#[derive(Clone, Debug)]
pub struct UnsafeSetLength(pub DataType);

impl Snippet for UnsafeSetLength {
    fn inputs(&self) -> Vec<String> {
        vec!["*list".to_string(), "list_length".to_string()]
    }

    fn outputs(&self) -> Vec<String> {
        vec!["*list".to_string()]
    }

    fn input_types(&self) -> Vec<crate::snippet::DataType> {
        vec![DataType::List(Box::new(self.0.clone())), DataType::U32]
    }

    fn output_types(&self) -> Vec<crate::snippet::DataType> {
        vec![DataType::List(Box::new(self.0.clone()))]
    }

    fn crash_conditions(&self) -> Vec<String> {
        vec![]
    }

    fn gen_input_states(&self) -> Vec<ExecutionState> {
        vec![
            prepare_state(&self.0),
            prepare_state(&self.0),
            prepare_state(&self.0),
        ]
    }

    fn stack_diff(&self) -> isize {
        // pops list_length but leaves list_pointer on stack
        -1
    }

    fn entrypoint(&self) -> String {
        format!(
            "tasm_list_unsafe_u32_set_length_{}",
            self.0.label_friendly_name()
        )
    }

    fn function_code(&self, _library: &mut SnippetState) -> String {
        let entry_point = self.entrypoint();
        // It is assumed that the new length is a valid u32 value
        format!(
            "
                // BEFORE: _ *list list_length
                // AFTER: _ *list
                {entry_point}:
                    write_mem
                    // Stack: *list list_length

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
        let new_length = stack.pop().unwrap();
        let list_address = stack.pop().unwrap();

        memory.insert(list_address, new_length);

        stack.push(list_address);
    }

    fn common_case_input_state(&self) -> ExecutionState {
        prepare_state(&self.0)
    }

    fn worst_case_input_state(&self) -> ExecutionState {
        prepare_state(&self.0)
    }
}

fn prepare_state(data_type: &DataType) -> ExecutionState {
    let list_pointer: BFieldElement = random();
    let old_length: usize = thread_rng().gen_range(0..100);
    let new_length: usize = thread_rng().gen_range(0..100);
    let mut stack = get_init_tvm_stack();
    stack.push(list_pointer);
    stack.push(BFieldElement::new(new_length as u64));
    let mut memory = HashMap::default();
    unsafe_insert_random_list(list_pointer, old_length, &mut memory, data_type.get_size());
    ExecutionState::with_stack_and_memory(stack, memory, 0)
}

#[cfg(test)]
mod tests_set_length {
    use twenty_first::shared_math::b_field_element::BFieldElement;

    use crate::get_init_tvm_stack;
    use crate::snippet_bencher::bench_and_write;
    use crate::test_helpers::{rust_tasm_equivalence_prop, rust_tasm_equivalence_prop_new};

    use super::*;

    #[test]
    fn new_snippet_test() {
        rust_tasm_equivalence_prop_new(&UnsafeSetLength(DataType::XFE), true);
    }

    #[test]
    fn unsafe_set_length_benchmark() {
        bench_and_write(UnsafeSetLength(DataType::Digest));
    }

    #[test]
    fn list_u32_n_is_one_push() {
        let list_address = BFieldElement::new(58);
        prop_set_length(DataType::BFE, list_address, 22, 14);
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
        let expected_end_stack = vec![get_init_tvm_stack(), vec![list_address]].concat();
        let mut init_stack = get_init_tvm_stack();
        init_stack.push(list_address);
        init_stack.push(BFieldElement::new(new_list_length as u64));

        let mut vm_memory = HashMap::default();

        // Insert length indicator of list, lives on offset = 0 from `list_address`
        vm_memory.insert(list_address, BFieldElement::new(init_list_length as u64));

        let _execution_result = rust_tasm_equivalence_prop(
            &UnsafeSetLength(data_type),
            &init_stack,
            &[],
            &[],
            &mut vm_memory,
            0,
            Some(&expected_end_stack),
        );

        // Verify that length indicator has been updated
        assert_eq!(
            BFieldElement::new(new_list_length as u64),
            vm_memory[&list_address]
        );
    }
}
