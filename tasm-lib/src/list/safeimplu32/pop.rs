use itertools::Itertools;
use rand::random;
use std::collections::HashMap;
use triton_vm::triton_asm;
use twenty_first::shared_math::b_field_element::BFieldElement;

use crate::data_type::DataType;
use crate::library::Library;
use crate::rust_shadowing_helper_functions::safe_list::{safe_insert_random_list, safe_list_pop};
use crate::snippet::DeprecatedSnippet;
use crate::{empty_stack, ExecutionState};

#[derive(Clone, Debug)]
pub struct SafePop {
    pub data_type: DataType,
}

impl DeprecatedSnippet for SafePop {
    fn input_field_names(&self) -> Vec<String> {
        vec!["*list".to_string()]
    }

    fn output_field_names(&self) -> Vec<String> {
        let mut ret: Vec<String> = vec![];
        let element_size = self.data_type.stack_size();
        for i in 0..element_size {
            ret.push(format!("element_{}", element_size - 1 - i));
        }

        ret
    }

    fn input_types(&self) -> Vec<crate::data_type::DataType> {
        vec![DataType::List(Box::new(self.data_type.clone()))]
    }

    fn output_types(&self) -> Vec<crate::data_type::DataType> {
        vec![self.data_type.clone()]
    }

    fn crash_conditions(&self) -> Vec<String> {
        vec!["stack underflow".to_string()]
    }

    fn gen_input_states(&self) -> Vec<ExecutionState> {
        let mut ret = vec![];
        for i in 1..=10 {
            ret.push(prepare_state(&self.data_type, i))
        }

        ret
    }

    fn stack_diff(&self) -> isize {
        self.data_type.stack_size() as isize - 1
    }

    fn entrypoint_name(&self) -> String {
        format!(
            "tasm_list_safeimplu32_pop___{}",
            self.data_type.label_friendly_name()
        )
    }

    /// Pop last element from list. Does *not* actually delete the last
    /// element but instead leaves it in memory.
    fn function_code(&self, _library: &mut Library) -> String {
        let entry_point = self.entrypoint_name();

        // Start and end of loop: Stack: _  [elems], address_for_last_unread_element
        let code_to_read_elements = self.data_type.read_value_from_memory();

        let element_size = self.data_type.stack_size();

        let mul_with_size = if element_size != 1 {
            triton_asm!(push {element_size} mul)
        } else {
            triton_asm!()
        };
        triton_asm!(
            // Before: _ *list
            // After:  _ [value]
            {entry_point}:
                read_mem 1
                // _  length (*list - 1)

                // Assert that length is not 0
                dup 1
                push 0
                eq
                push 0
                eq
                assert
                // _  length (*list - 1)

                push 1
                add
                // _  length *list

                // Decrease length value by one and write back to memory
                dup 1
                push -1
                add
                // _  length *list (length - 1)

                dup 1
                write_mem 1
                pop 1
                // _  length *list

                swap 1
                // stack : _ *list initial_length

                {&mul_with_size}
                // stack : _  *list, (offset_for_last_element = (N * initial_length))

                add
                push 1
                add
                // stack : _  address_for_last_element

                {&code_to_read_elements}
                // Stack: _  [elements], address_for_last_unread_element

                pop 1
                // Stack: _  [elements]

                return
        )
        .iter()
        .join("\n")
    }

    fn rust_shadowing(
        &self,
        stack: &mut Vec<BFieldElement>,
        _std_in: Vec<BFieldElement>,
        _secret_in: Vec<BFieldElement>,
        memory: &mut HashMap<BFieldElement, BFieldElement>,
    ) {
        let list_pointer = stack.pop().unwrap();
        let mut popped = safe_list_pop(list_pointer, memory, self.data_type.stack_size());

        for _ in 0..self.data_type.stack_size() {
            stack.push(popped.pop().unwrap());
        }
    }

    fn common_case_input_state(&self) -> ExecutionState {
        prepare_state(&self.data_type, 30)
    }

    fn worst_case_input_state(&self) -> ExecutionState {
        prepare_state(&self.data_type, 30)
    }
}

fn prepare_state(data_type: &DataType, old_length: usize) -> ExecutionState {
    let list_pointer: BFieldElement = random();
    let capacity: usize = 30;
    let mut stack = empty_stack();
    stack.push(list_pointer);
    let mut memory = HashMap::default();
    safe_insert_random_list(
        data_type,
        list_pointer,
        capacity as u32,
        old_length,
        &mut memory,
    );
    ExecutionState::with_stack_and_memory(stack, memory, 0)
}

#[cfg(test)]
mod tests {
    use num::One;
    use twenty_first::shared_math::b_field_element::BFieldElement;

    use crate::empty_stack;
    use crate::rust_shadowing_helper_functions::safe_list::safe_list_push;
    use crate::test_helpers::{
        test_rust_equivalence_given_input_values_deprecated,
        test_rust_equivalence_multiple_deprecated,
    };

    use super::*;

    #[test]
    fn new_snippet_test() {
        test_rust_equivalence_multiple_deprecated(
            &SafePop {
                data_type: DataType::Bool,
            },
            true,
        );
        test_rust_equivalence_multiple_deprecated(
            &SafePop {
                data_type: DataType::U32,
            },
            true,
        );
        test_rust_equivalence_multiple_deprecated(
            &SafePop {
                data_type: DataType::U64,
            },
            true,
        );
        test_rust_equivalence_multiple_deprecated(
            &SafePop {
                data_type: DataType::Bfe,
            },
            true,
        );
        test_rust_equivalence_multiple_deprecated(
            &SafePop {
                data_type: DataType::Xfe,
            },
            true,
        );
        test_rust_equivalence_multiple_deprecated(
            &SafePop {
                data_type: DataType::Digest,
            },
            true,
        );
        test_rust_equivalence_multiple_deprecated(
            &SafePop {
                data_type: DataType::Tuple(vec![DataType::Digest, DataType::Xfe, DataType::Digest]),
            },
            false,
        );
    }

    #[test]
    #[should_panic]
    fn panic_if_pop_on_empty_list_1() {
        let list_address = BFieldElement::new(48);
        prop_pop(DataType::Bfe, list_address, 0, 107);
    }

    #[test]
    #[should_panic]
    fn panic_if_pop_on_empty_list_2() {
        let list_address = BFieldElement::new(48);
        prop_pop(DataType::U64, list_address, 0, 107);
    }

    #[test]
    #[should_panic]
    fn panic_if_pop_on_empty_list_3() {
        let list_address = BFieldElement::new(48);
        prop_pop(DataType::Xfe, list_address, 0, 107);
    }

    #[test]
    fn list_u32_n_is_n_pop() {
        prop_pop(DataType::Digest, BFieldElement::new(1), 1, 1);
        prop_pop(DataType::Digest, BFieldElement::new(2), 1, 1);
        prop_pop(DataType::Digest, BFieldElement::new(1), 1, 2);
        prop_pop(DataType::Digest, BFieldElement::new(2), 1, 2);
        prop_pop(DataType::Digest, BFieldElement::new(1), 2, 2);
        prop_pop(DataType::Digest, BFieldElement::new(2), 2, 2);
        prop_pop(DataType::Digest, BFieldElement::new(1), 2, 3);
        prop_pop(DataType::Digest, BFieldElement::new(2), 2, 3);

        let list_address = BFieldElement::new(48);
        prop_pop(DataType::Bfe, list_address, 24, 107);
        prop_pop(DataType::Bool, list_address, 24, 107);
        prop_pop(DataType::U32, list_address, 24, 107);
        prop_pop(DataType::U64, list_address, 48, 107);
        prop_pop(DataType::Xfe, list_address, 3, 107);
        prop_pop(DataType::Digest, list_address, 20, 107);
    }

    fn prop_pop(
        data_type: DataType,
        list_pointer: BFieldElement,
        init_list_length: usize,
        list_capacity: u32,
    ) {
        let element_size = data_type.stack_size();
        let mut init_stack = empty_stack();
        init_stack.push(list_pointer);

        let mut memory = HashMap::default();

        // Insert random values for the elements in the list
        safe_insert_random_list(
            &data_type,
            list_pointer,
            list_capacity,
            init_list_length,
            &mut memory,
        );

        let last_element: Vec<BFieldElement> =
            safe_list_pop(list_pointer, &mut memory, element_size);
        safe_list_push(
            list_pointer,
            last_element.clone(),
            &mut memory,
            element_size,
        );

        let mut expected_end_stack = empty_stack();

        for i in 0..element_size {
            expected_end_stack.push(last_element[element_size - 1 - i]);
        }

        test_rust_equivalence_given_input_values_deprecated(
            &SafePop { data_type },
            &init_stack,
            &[],
            &mut memory,
            0,
            Some(&expected_end_stack),
        );

        // Verify that length is now indicated to be `init_list_length - 1`
        assert_eq!(
            BFieldElement::new(init_list_length as u64) - BFieldElement::one(),
            memory[&list_pointer]
        );

        // Verify that capacity is unchanged
        assert_eq!(
            BFieldElement::new(list_capacity as u64),
            memory[&(list_pointer + BFieldElement::one())]
        );
    }
}

#[cfg(test)]
mod benches {
    use super::*;
    use crate::snippet_bencher::bench_and_write;

    #[test]
    fn safe_pop_benchmark() {
        bench_and_write(SafePop {
            data_type: DataType::Digest,
        });
    }
}
