use rand::{random, thread_rng, Rng};
use std::collections::HashMap;
use twenty_first::shared_math::b_field_element::BFieldElement;

use crate::library::Library;
use crate::rust_shadowing_helper_functions::safe_list::safe_insert_random_list;
use crate::snippet::{DataType, Snippet};
use crate::{get_init_tvm_stack, ExecutionState};

#[derive(Clone)]
pub struct SafeLength(pub DataType);

impl Snippet for SafeLength {
    fn inputs(&self) -> Vec<String> {
        vec!["*list".to_string()]
    }

    fn outputs(&self) -> Vec<String> {
        vec!["list_length".to_string()]
    }

    fn input_types(&self) -> Vec<crate::snippet::DataType> {
        vec![DataType::List(Box::new(self.0.clone()))]
    }

    fn output_types(&self) -> Vec<crate::snippet::DataType> {
        vec![DataType::U32]
    }

    fn crash_conditions() -> Vec<String> {
        vec![]
    }

    fn gen_input_states(&self) -> Vec<ExecutionState> {
        let mut ret: Vec<ExecutionState> = vec![];
        let mut rng = thread_rng();
        let mut stack = get_init_tvm_stack();
        let list_pointer: BFieldElement = random();
        let capacity: u32 = 100;
        let list_length: usize = rng.gen_range(0..=capacity as usize);
        stack.push(list_pointer);

        // Test for various values of `N` (list-element size)
        let mut memory = HashMap::default();
        safe_insert_random_list(list_pointer, capacity, list_length, &mut memory, 1);
        ret.push(ExecutionState::with_stack_and_memory(
            stack.clone(),
            memory,
            0,
        ));
        memory = HashMap::default();
        safe_insert_random_list(list_pointer, capacity, list_length, &mut memory, 2);
        ret.push(ExecutionState::with_stack_and_memory(
            stack.clone(),
            memory,
            0,
        ));
        memory = HashMap::default();
        safe_insert_random_list(list_pointer, capacity, list_length, &mut memory, 3);
        ret.push(ExecutionState::with_stack_and_memory(
            stack.clone(),
            memory,
            0,
        ));
        memory = HashMap::default();
        safe_insert_random_list(list_pointer, capacity, list_length, &mut memory, 4);
        ret.push(ExecutionState::with_stack_and_memory(
            stack.clone(),
            memory,
            0,
        ));
        memory = HashMap::default();
        safe_insert_random_list(list_pointer, capacity, list_length, &mut memory, 11);
        ret.push(ExecutionState::with_stack_and_memory(stack, memory, 0));

        ret
    }

    fn stack_diff(&self) -> isize {
        // Consumes a memory address and returns a length in the form of a u32
        0
    }

    fn entrypoint(&self) -> String {
        format!("tasm_list_safe_u32_length_{}", self.0)
    }

    fn function_body(&self, _library: &mut Library) -> String {
        let entry_point = self.entrypoint();
        // Before: _ *list
        // After: _ list_length_u32
        format!(
            "
            {entry_point}:
                push 0
                read_mem
                swap1
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
        // Find the list in memory and push its length to the top of the stack
        let list_address = stack.pop().unwrap();
        let list_length = memory[&list_address];
        stack.push(list_length);
    }
}

#[cfg(test)]
mod tests {
    use num::One;
    use twenty_first::shared_math::b_field_element::BFieldElement;

    use crate::get_init_tvm_stack;
    use crate::test_helpers::{rust_tasm_equivalence_prop, rust_tasm_equivalence_prop_new};

    use super::*;

    #[test]
    fn new_snippet_test_long() {
        rust_tasm_equivalence_prop_new(SafeLength(DataType::Bool));
        rust_tasm_equivalence_prop_new(SafeLength(DataType::U32));
        rust_tasm_equivalence_prop_new(SafeLength(DataType::U64));
        rust_tasm_equivalence_prop_new(SafeLength(DataType::BFE));
        rust_tasm_equivalence_prop_new(SafeLength(DataType::XFE));
        rust_tasm_equivalence_prop_new(SafeLength(DataType::Digest));
    }

    #[test]
    fn list_u32_simple() {
        let expected_end_stack = vec![get_init_tvm_stack(), vec![BFieldElement::new(42)]].concat();
        prop_length(
            &DataType::U64,
            BFieldElement::one(),
            42,
            Some(&expected_end_stack),
        );

        let expected_end_stack = vec![get_init_tvm_stack(), vec![BFieldElement::new(588)]].concat();
        prop_length(
            &DataType::XFE,
            BFieldElement::one(),
            588,
            Some(&expected_end_stack),
        );

        let expected_end_stack = vec![get_init_tvm_stack(), vec![BFieldElement::new(4)]].concat();
        prop_length(
            &DataType::Digest,
            BFieldElement::one(),
            4,
            Some(&expected_end_stack),
        );

        let expected_end_stack = vec![get_init_tvm_stack(), vec![BFieldElement::new(7)]].concat();
        prop_length(
            &DataType::U32,
            BFieldElement::one(),
            7,
            Some(&expected_end_stack),
        );
    }

    // Note that the *actual list* of length `list_length` is *actually constructed in the VM in this test. So you may not
    // want to exaggerate that number.
    fn prop_length(
        element_type: &DataType,
        list_pointer: BFieldElement,
        list_length: usize,
        expected: Option<&[BFieldElement]>,
    ) {
        let capacity = 1000;
        let mut init_stack = get_init_tvm_stack();
        init_stack.push(list_pointer);

        let mut memory = HashMap::default();

        safe_insert_random_list(
            list_pointer,
            capacity,
            list_length,
            &mut memory,
            element_type.get_size(),
        );

        let _execution_result = rust_tasm_equivalence_prop(
            SafeLength(DataType::BFE),
            &init_stack,
            &[],
            &[],
            &mut memory,
            0,
            expected,
        );
    }
}