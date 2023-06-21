use std::collections::HashMap;

use num::One;
use rand::{random, thread_rng, Rng};
use twenty_first::shared_math::b_field_element::BFieldElement;

use crate::rust_shadowing_helper_functions::safe_list::safe_insert_random_list;
use crate::snippet::{DataType, Snippet};
use crate::snippet_state::SnippetState;
use crate::{get_init_tvm_stack, ExecutionState};

#[derive(Clone, Debug)]
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

    fn crash_conditions(&self) -> Vec<String> {
        vec!["New length exceeds capacity".to_string()]
    }

    fn gen_input_states(&self) -> Vec<ExecutionState> {
        let capacity = 100;
        vec![
            prepare_state(
                &self.0,
                capacity,
                thread_rng().gen_range(0..capacity) as usize,
                thread_rng().gen_range(0..capacity) as usize,
            ),
            prepare_state(
                &self.0,
                capacity,
                thread_rng().gen_range(0..capacity) as usize,
                thread_rng().gen_range(0..capacity) as usize,
            ),
            prepare_state(
                &self.0,
                capacity,
                thread_rng().gen_range(0..capacity) as usize,
                thread_rng().gen_range(0..capacity) as usize,
            ),
        ]
    }

    fn stack_diff(&self) -> isize {
        // TODO: It should probably clear the stack
        // pops list_length but leaves list_pointer on stack
        -1
    }

    fn entrypoint(&self) -> String {
        format!(
            "tasm_list_safe_u32_set_length_{}",
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
                    // Verify that new length does not exceed capacity
                    dup 0
                    dup 2
                    push 1
                    add
                    read_mem
                    // Stack: *list list_length list_length (*list + 1) capacity

                    swap 1
                    pop
                    // Stack: *list list_length list_length capacity

                    lt
                    push 0
                    eq
                    // Stack: *list list_length list_length <= capacity

                    assert
                    // Stack: *list list_length

                    write_mem
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

    fn common_case_input_state(&self) -> ExecutionState {
        prepare_state(&self.0, 1000, 1 << 5, 1 << 4)
    }

    fn worst_case_input_state(&self) -> ExecutionState {
        prepare_state(&self.0, 1000, 1 << 6, 1 << 5)
    }
}

fn prepare_state(
    data_type: &DataType,
    capacity: u32,
    init_length: usize,
    new_length: usize,
) -> ExecutionState {
    let list_pointer: BFieldElement = random();
    let mut stack = get_init_tvm_stack();
    stack.push(list_pointer);
    stack.push(BFieldElement::new(new_length as u64));
    let mut memory = HashMap::default();
    safe_insert_random_list(data_type, list_pointer, capacity, init_length, &mut memory);
    ExecutionState::with_stack_and_memory(stack, memory, 0)
}

#[cfg(test)]
mod tests {
    use twenty_first::shared_math::b_field_element::BFieldElement;

    use crate::get_init_tvm_stack;

    use crate::test_helpers::{
        test_rust_equivalence_given_input_values, test_rust_equivalence_multiple,
    };

    use super::*;

    #[test]
    fn new_snippet_test() {
        test_rust_equivalence_multiple(&SafeSetLength(DataType::Bool), true);
        test_rust_equivalence_multiple(&SafeSetLength(DataType::U32), true);
        test_rust_equivalence_multiple(&SafeSetLength(DataType::U64), true);
        test_rust_equivalence_multiple(&SafeSetLength(DataType::BFE), true);
        test_rust_equivalence_multiple(&SafeSetLength(DataType::XFE), true);
        test_rust_equivalence_multiple(&SafeSetLength(DataType::Digest), true);
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
        let expected_end_stack = vec![get_init_tvm_stack(), vec![list_pointer]].concat();
        let mut init_stack = get_init_tvm_stack();
        init_stack.push(list_pointer);
        init_stack.push(BFieldElement::new(new_list_length as u64));

        let mut memory = HashMap::default();

        // Insert length indicator of list, lives on offset = 0 from `list_address`
        safe_insert_random_list(
            &data_type,
            list_pointer,
            capacity,
            init_list_length as usize,
            &mut memory,
        );

        test_rust_equivalence_given_input_values::<SafeSetLength>(
            &SafeSetLength(data_type),
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

#[cfg(test)]
mod benches {
    use super::*;
    use crate::snippet_bencher::bench_and_write;

    #[test]
    fn safe_set_length_benchmark() {
        bench_and_write(SafeSetLength(DataType::Digest));
    }
}
