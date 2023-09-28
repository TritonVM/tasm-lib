use std::collections::HashMap;

use rand::{random, thread_rng, Rng};
use twenty_first::shared_math::b_field_element::BFieldElement;
use twenty_first::shared_math::other::random_elements;

use crate::library::Library;
use crate::rust_shadowing_helper_functions::unsafe_list::{
    unsafe_list_set, untyped_unsafe_insert_random_list,
};
use crate::snippet::{DataType, DeprecatedSnippet};
use crate::{get_init_tvm_stack, ExecutionState};

#[derive(Clone, Debug)]
pub struct UnsafeSet(pub DataType);

impl DeprecatedSnippet for UnsafeSet {
    fn input_field_names(&self) -> Vec<String> {
        // See: https://github.com/TritonVM/tasm-snippets/issues/13
        // _ elem{{N - 1}}, elem{{N - 2}}, ..., elem{{0}} *list index
        [
            vec!["element".to_string(); self.0.get_size()],
            vec!["*list".to_string(), "index".to_string()],
        ]
        .concat()
    }

    fn output_field_names(&self) -> Vec<String> {
        vec![]
    }

    fn input_types(&self) -> Vec<crate::snippet::DataType> {
        vec![
            self.0.clone(),
            DataType::List(Box::new(self.0.clone())),
            DataType::U32,
        ]
    }

    fn output_types(&self) -> Vec<crate::snippet::DataType> {
        vec![]
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
        -2 - self.0.get_size() as isize
    }

    fn entrypoint_name(&self) -> String {
        format!(
            "tasm_list_unsafeimplu32_set_element___{}",
            self.0.label_friendly_name()
        )
    }

    fn function_code(&self, _library: &mut Library) -> String {
        let entrypoint = self.entrypoint_name();
        let element_size = self.0.get_size();

        let mut write_elements_to_memory_code = String::default();
        for i in 0..element_size {
            write_elements_to_memory_code.push_str("swap 1\n");
            write_elements_to_memory_code.push_str("write_mem\n");
            if i != element_size - 1 {
                // Prepare for next write. Not needed for last iteration.
                write_elements_to_memory_code.push_str("push 1\n");
                write_elements_to_memory_code.push_str("add\n");
            }
        }

        let mul_with_size = if element_size != 1 {
            format!("push {element_size}\n mul\n")
        } else {
            String::default()
        };
        format!(
            "
                // BEFORE: _ elem{{N - 1}}, elem{{N - 2}}, ..., elem{{0}} *list index
                // AFTER: _
                {entrypoint}:
                    {mul_with_size}
                    push 1 // safety offset
                    add
                    add

                    // stack: _ elem{{N - 1}}, elem{{N - 2}}, ..., elem{{0}} *addr

                    {write_elements_to_memory_code}
                    // stack: _ *addr
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
        let index: u32 = stack.pop().unwrap().try_into().unwrap();
        let list_pointer = stack.pop().unwrap();
        let mut element: Vec<BFieldElement> = vec![BFieldElement::new(0); self.0.get_size()];
        for ee in element.iter_mut() {
            *ee = stack.pop().unwrap();
        }
        unsafe_list_set(
            list_pointer,
            index as usize,
            element,
            memory,
            self.0.get_size(),
        );
    }

    fn common_case_input_state(&self) -> ExecutionState {
        prepare_state(&self.0)
    }

    fn worst_case_input_state(&self) -> ExecutionState {
        prepare_state(&self.0)
    }
}

fn prepare_state(data_type: &DataType) -> ExecutionState {
    let list_length: usize = thread_rng().gen_range(1..100);
    let index: usize = thread_rng().gen_range(0..list_length);
    let mut stack = get_init_tvm_stack();
    let mut push_value: Vec<BFieldElement> = random_elements(data_type.get_size());
    while let Some(element) = push_value.pop() {
        stack.push(element);
    }

    let list_pointer: BFieldElement = random();
    stack.push(list_pointer);
    stack.push(BFieldElement::new(index as u64));

    let mut memory = HashMap::default();
    untyped_unsafe_insert_random_list(list_pointer, list_length, &mut memory, data_type.get_size());
    ExecutionState::with_stack_and_memory(stack, memory, 0)
}

#[cfg(test)]
mod tests {
    use twenty_first::shared_math::b_field_element::BFieldElement;

    use crate::get_init_tvm_stack;

    use crate::test_helpers::{
        test_rust_equivalence_given_input_values_deprecated,
        test_rust_equivalence_multiple_deprecated,
    };

    use super::*;

    #[test]
    fn new_snippet_test() {
        test_rust_equivalence_multiple_deprecated(&UnsafeSet(DataType::Bool), true);
        test_rust_equivalence_multiple_deprecated(&UnsafeSet(DataType::BFE), true);
        test_rust_equivalence_multiple_deprecated(&UnsafeSet(DataType::U32), true);
        test_rust_equivalence_multiple_deprecated(&UnsafeSet(DataType::U64), true);
        test_rust_equivalence_multiple_deprecated(&UnsafeSet(DataType::XFE), true);
        test_rust_equivalence_multiple_deprecated(&UnsafeSet(DataType::Digest), true);
    }

    #[test]
    fn list_u32_n_is_one_set() {
        let list_address = BFieldElement::new(48);
        let insert_value = vec![BFieldElement::new(1337)];
        prop_set(DataType::BFE, list_address, 20, insert_value, 2);
    }

    #[test]
    fn list_u32_n_is_three_set() {
        let list_address = BFieldElement::new(48);
        let insert_value = vec![
            BFieldElement::new(1337),
            BFieldElement::new(1337),
            BFieldElement::new(1337),
        ];
        prop_set(DataType::XFE, list_address, 20, insert_value, 2);
    }

    #[test]
    fn list_u32_n_is_two_set() {
        let list_address = BFieldElement::new(1841);
        let push_value = vec![BFieldElement::new(133700), BFieldElement::new(32)];
        prop_set(DataType::U64, list_address, 20, push_value, 0);
    }

    #[test]
    fn list_u32_n_is_five_set() {
        let list_address = BFieldElement::new(558);
        let push_value = vec![
            BFieldElement::new(133700),
            BFieldElement::new(32),
            BFieldElement::new(133700),
            BFieldElement::new(19990),
            BFieldElement::new(88888888),
        ];
        prop_set(DataType::Digest, list_address, 2313, push_value, 589);
    }

    fn prop_set(
        data_type: DataType,
        list_address: BFieldElement,
        init_list_length: u32,
        push_value: Vec<BFieldElement>,
        index: u32,
    ) {
        let expected_end_stack = [get_init_tvm_stack()].concat();
        let mut init_stack = get_init_tvm_stack();

        for i in 0..data_type.get_size() {
            init_stack.push(push_value[data_type.get_size() - 1 - i]);
        }
        init_stack.push(list_address);
        init_stack.push(BFieldElement::new(index as u64));

        let mut vm_memory = HashMap::default();

        // Insert length indicator of list, lives on offset = 0 from `list_address`
        untyped_unsafe_insert_random_list(
            list_address,
            init_list_length as usize,
            &mut vm_memory,
            data_type.get_size(),
        );

        test_rust_equivalence_given_input_values_deprecated(
            &UnsafeSet(data_type.clone()),
            &init_stack,
            &[],
            &mut vm_memory,
            0,
            Some(&expected_end_stack),
        );

        // Verify that length indicator is unchanged
        assert_eq!(
            BFieldElement::new((init_list_length) as u64),
            vm_memory[&list_address]
        );

        // verify that value was inserted at expected place
        for i in 0..data_type.get_size() {
            assert_eq!(
                push_value[i],
                vm_memory[&BFieldElement::new(
                    list_address.value()
                        + 1
                        + data_type.get_size() as u64 * index as u64
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
    fn unsafe_set_benchmark() {
        bench_and_write(UnsafeSet(DataType::Digest));
    }
}
