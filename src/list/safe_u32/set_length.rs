use std::collections::HashMap;

use num::One;
use rand::{random, thread_rng, Rng};
use twenty_first::shared_math::b_field_element::BFieldElement;

use crate::library::Library;
use crate::rust_shadowing_helper_functions::safe_list::safe_insert_random_list;
use crate::snippet::{DataType, Snippet};
use crate::{get_init_tvm_stack, ExecutionState};

#[derive(Clone)]
pub struct SafeSetLength(pub DataType);

impl Snippet for SafeSetLength {
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

    fn crash_conditions() -> Vec<String> {
        vec!["New length exceeds capacity".to_string()]
    }

    fn gen_input_states(&self) -> Vec<ExecutionState> {
        fn prepare_state(data_type: &DataType) -> ExecutionState {
            let capacity = 100;
            let list_pointer: BFieldElement = random();
            let old_length: usize = thread_rng().gen_range(0..capacity);
            let new_length: usize = thread_rng().gen_range(0..capacity);
            let mut stack = get_init_tvm_stack();
            stack.push(list_pointer);
            stack.push(BFieldElement::new(new_length as u64));
            let mut memory = HashMap::default();
            safe_insert_random_list(
                list_pointer,
                capacity as u32,
                old_length,
                &mut memory,
                data_type.get_size(),
            );
            ExecutionState::with_stack_and_memory(stack, memory, 0)
        }

        vec![
            prepare_state(&self.0),
            prepare_state(&self.0),
            prepare_state(&self.0),
        ]
    }

    fn stack_diff(&self) -> isize {
        // TODO: It should probably clear the stack
        // pops list_length but leaves list_pointer on stack
        -1
    }

    fn entrypoint(&self) -> String {
        format!("tasm_list_safe_u32_set_length_{}", self.0)
    }

    fn function_body(&self, _library: &mut Library) -> String {
        let entry_point = self.entrypoint();
        // It is assumed that the new length is a valid u32 value
        format!(
            "
                // BEFORE: _ *list list_length
                // AFTER: _ *list
                {entry_point}:
                    // Verify that new length does not exceed capacity
                    dup0
                    dup2
                    push 1
                    add
                    push 0
                    read_mem
                    // Stack: *list list_length list_length (*list + 1) capacity

                    swap1
                    pop
                    // Stack: *list list_length list_length capacity

                    lt
                    push 0
                    eq
                    // Stack: *list list_length list_length <= capacity

                    assert
                    // Stack: *list list_length

                    write_mem
                    // Stack: *list list_length

                    pop
                    // Stack: *list

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
        let new_length_u32 = new_length.value().try_into().unwrap();
        let list_address = stack.pop().unwrap();

        let capacity: u32 = memory[&(list_address + BFieldElement::one())]
            .value()
            .try_into()
            .unwrap();
        assert!(
            capacity >= new_length_u32,
            "New length cannot exceed list's capacity"
        );

        memory.insert(list_address, new_length);

        stack.push(list_address);
    }
}

#[cfg(test)]
mod tests_set_length {
    use twenty_first::shared_math::b_field_element::BFieldElement;

    use crate::get_init_tvm_stack;
    use crate::test_helpers::{rust_tasm_equivalence_prop, rust_tasm_equivalence_prop_new};

    use super::*;

    #[test]
    fn new_snippet_test() {
        rust_tasm_equivalence_prop_new::<SafeSetLength>(SafeSetLength(DataType::Bool));
        rust_tasm_equivalence_prop_new::<SafeSetLength>(SafeSetLength(DataType::U32));
        rust_tasm_equivalence_prop_new::<SafeSetLength>(SafeSetLength(DataType::U64));
        rust_tasm_equivalence_prop_new::<SafeSetLength>(SafeSetLength(DataType::BFE));
        rust_tasm_equivalence_prop_new::<SafeSetLength>(SafeSetLength(DataType::XFE));
        rust_tasm_equivalence_prop_new::<SafeSetLength>(SafeSetLength(DataType::Digest));
    }

    #[test]
    fn list_u32_n_is_one_decrease() {
        let list_address = BFieldElement::new(58);
        let init_length = 22;
        let new_list_length = 14;
        let capacity = 22;
        prop_set_length(
            DataType::BFE,
            list_address,
            init_length,
            new_list_length,
            capacity,
        );
    }

    #[test]
    fn list_u32_n_is_one_increase() {
        let list_address = BFieldElement::new(58);
        let init_length = 2;
        let new_list_length = 22;
        let capacity = 22;
        prop_set_length(
            DataType::BFE,
            list_address,
            init_length,
            new_list_length,
            capacity,
        );
    }

    #[should_panic]
    #[test]
    fn list_u32_n_is_one_increase_beyond_capacity_a() {
        let list_pointer = BFieldElement::new(1841);
        let init_length = 20;
        let new_list_length = 21;
        let capacity = 20;
        prop_set_length(
            DataType::U32,
            list_pointer,
            init_length,
            new_list_length,
            capacity,
        );
    }

    #[should_panic]
    #[test]
    fn list_u32_n_is_one_increase_beyond_capacity_b() {
        let list_pointer = BFieldElement::new(1841);
        let init_length = 20;
        let new_list_length = 22;
        let capacity = 20;
        prop_set_length(
            DataType::U32,
            list_pointer,
            init_length,
            new_list_length,
            capacity,
        );
    }

    #[should_panic]
    #[test]
    fn list_u32_n_is_one_increase_beyond_capacity_c() {
        let list_pointer = BFieldElement::new(1841);
        let init_length = 20;
        let new_list_length = 21;
        let capacity = 20;
        prop_set_length(
            DataType::XFE,
            list_pointer,
            init_length,
            new_list_length,
            capacity,
        );
    }

    #[test]
    fn list_u32_n_is_five_push() {
        let list_address = BFieldElement::new(558);
        let init_length = 231;
        let new_list_length = 14;
        let capacity = 300;
        prop_set_length(
            DataType::Digest,
            list_address,
            init_length,
            new_list_length,
            capacity,
        );
        let init_length = 14;
        let new_list_length = 0;
        let capacity = 300;
        prop_set_length(
            DataType::Digest,
            list_address,
            init_length,
            new_list_length,
            capacity,
        );
        let init_length = 0;
        let new_list_length = 0;
        let capacity = 300;
        prop_set_length(
            DataType::Digest,
            list_address,
            init_length,
            new_list_length,
            capacity,
        );
    }

    fn prop_set_length(
        data_type: DataType,
        list_pointer: BFieldElement,
        init_list_length: u32,
        new_list_length: u32,
        capacity: u32,
    ) {
        let element_size = data_type.get_size();
        let expected_end_stack = vec![get_init_tvm_stack(), vec![list_pointer]].concat();
        let mut init_stack = get_init_tvm_stack();
        init_stack.push(list_pointer);
        init_stack.push(BFieldElement::new(new_list_length as u64));

        let mut memory = HashMap::default();

        // Insert length indicator of list, lives on offset = 0 from `list_address`
        safe_insert_random_list(
            list_pointer,
            capacity,
            init_list_length as usize,
            &mut memory,
            element_size,
        );

        let _execution_result = rust_tasm_equivalence_prop::<SafeSetLength>(
            SafeSetLength(data_type),
            &init_stack,
            &[],
            &[],
            &mut memory,
            0,
            Some(&expected_end_stack),
        );

        // Verify that length indicator has been updated
        assert_eq!(
            BFieldElement::new(new_list_length as u64),
            memory[&list_pointer]
        );
    }
}