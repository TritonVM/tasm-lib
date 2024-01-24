use std::collections::HashMap;

use itertools::Itertools;
use rand::random;
use rand::thread_rng;
use rand::Rng;
use triton_vm::prelude::*;
use twenty_first::shared_math::other::random_elements;

use crate::data_type::DataType;
use crate::empty_stack;
use crate::library::Library;
use crate::rust_shadowing_helper_functions::safe_list::safe_insert_random_list;
use crate::rust_shadowing_helper_functions::safe_list::safe_list_set;
use crate::traits::deprecated_snippet::DeprecatedSnippet;
use crate::ExecutionState;

#[derive(Clone, Debug)]
pub struct SafeSet {
    pub data_type: DataType,
}

impl DeprecatedSnippet for SafeSet {
    fn entrypoint_name(&self) -> String {
        format!(
            "tasm_list_safeimplu32_set_element___{}",
            self.data_type.label_friendly_name()
        )
    }

    fn input_field_names(&self) -> Vec<String> {
        // See: https://github.com/TritonVM/tasm-snippets/issues/13
        // _ elem{{N - 1}}, elem{{N - 2}}, ..., elem{{0}} *list index
        [
            vec!["element".to_string(); self.data_type.stack_size()],
            vec!["*list".to_string(), "index".to_string()],
        ]
        .concat()
    }

    fn input_types(&self) -> Vec<DataType> {
        vec![
            self.data_type.clone(),
            DataType::List(Box::new(self.data_type.clone())),
            DataType::U32,
        ]
    }

    fn output_field_names(&self) -> Vec<String> {
        vec![]
    }

    fn output_types(&self) -> Vec<DataType> {
        vec![]
    }

    fn stack_diff(&self) -> isize {
        -2 - self.data_type.stack_size() as isize
    }

    fn function_code(&self, _library: &mut Library) -> String {
        let entrypoint = self.entrypoint_name();
        let element_size = self.data_type.stack_size();

        let write_elements_to_memory_code = self.data_type.write_value_to_memory_leave_pointer();

        let mul_with_size = if element_size != 1 {
            format!("push {element_size}\n mul\n")
        } else {
            String::default()
        };

        triton_asm!(
                // BEFORE: _ [value] *list index
                // AFTER:  _
                {entrypoint}:
                    // Verify that index is less than length
                    swap 1
                    // _ [value] index *list

                    read_mem 1
                    // _ [value] index length (*list - 1)

                    swap 2
                    // _ [value] (*list - 1) length index

                    swap 1
                    // _ [value] (*list - 1) index length

                    dup 1
                    // _ [value] (*list - 1) index length index

                    lt
                    // _ [value] (*list - 1) index (length > index)

                    assert
                    // _ [value] (*list - 1) index

                    {mul_with_size}
                    // _ [value] (*list - 1) offset

                    push 3
                    add
                    add

                    // _ [value] *elem

                    {&write_elements_to_memory_code}
                    // _ *next_elem

                    pop 1
                    // _

                    return
        )
        .iter()
        .join("\n")
    }

    fn crash_conditions(&self) -> Vec<String> {
        vec![]
    }

    fn gen_input_states(&self) -> Vec<ExecutionState> {
        let capacity = 100u32;
        vec![
            prepare_state(
                &self.data_type,
                capacity,
                thread_rng().gen_range(1..capacity) as usize,
            ),
            prepare_state(
                &self.data_type,
                capacity,
                thread_rng().gen_range(1..capacity) as usize,
            ),
            prepare_state(
                &self.data_type,
                capacity,
                thread_rng().gen_range(1..capacity) as usize,
            ),
        ]
    }

    fn common_case_input_state(&self) -> ExecutionState {
        prepare_state(&self.data_type, 1000, 1 << 5)
    }

    fn worst_case_input_state(&self) -> ExecutionState {
        prepare_state(&self.data_type, 1000, 1 << 6)
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
        let list_length: u32 = memory[&list_pointer].value().try_into().unwrap();
        assert!(index < list_length, "Index must be within list bounds");

        let mut element: Vec<BFieldElement> =
            vec![BFieldElement::new(0); self.data_type.stack_size()];
        for ee in element.iter_mut() {
            *ee = stack.pop().unwrap();
        }

        safe_list_set(list_pointer, index as usize, element, memory);
    }
}

fn prepare_state(data_type: &DataType, capacity: u32, list_length: usize) -> ExecutionState {
    let index: usize = thread_rng().gen_range(0..list_length);
    let mut stack = empty_stack();
    let mut push_value: Vec<BFieldElement> = random_elements(data_type.stack_size());
    while let Some(element) = push_value.pop() {
        stack.push(element);
    }

    let list_pointer: u32 = random();
    let list_pointer_bfe = BFieldElement::new(list_pointer as u64);
    stack.push(list_pointer_bfe);
    stack.push(BFieldElement::new(index as u64));

    let mut memory = HashMap::default();
    safe_insert_random_list(
        data_type,
        list_pointer_bfe,
        capacity,
        list_length,
        &mut memory,
    );
    ExecutionState::with_stack_and_memory(stack, memory, 0)
}

#[cfg(test)]
mod tests {
    use crate::empty_stack;
    use crate::test_helpers::test_rust_equivalence_given_input_values_deprecated;
    use crate::test_helpers::test_rust_equivalence_multiple_deprecated;

    use super::*;

    #[test]
    fn new_snippet_test() {
        test_rust_equivalence_multiple_deprecated(
            &SafeSet {
                data_type: DataType::Bool,
            },
            true,
        );
        test_rust_equivalence_multiple_deprecated(
            &SafeSet {
                data_type: DataType::Bfe,
            },
            true,
        );
        test_rust_equivalence_multiple_deprecated(
            &SafeSet {
                data_type: DataType::U32,
            },
            true,
        );
        test_rust_equivalence_multiple_deprecated(
            &SafeSet {
                data_type: DataType::U64,
            },
            true,
        );
        test_rust_equivalence_multiple_deprecated(
            &SafeSet {
                data_type: DataType::Xfe,
            },
            true,
        );
        test_rust_equivalence_multiple_deprecated(
            &SafeSet {
                data_type: DataType::Digest,
            },
            true,
        );
        for dt in DataType::big_random_generatable_type_collection() {
            test_rust_equivalence_multiple_deprecated(&SafeSet { data_type: dt }, false);
        }
    }

    #[test]
    fn list_u32_n_is_one_set() {
        let list_address = BFieldElement::new(48);
        let insert_value = vec![BFieldElement::new(1337)];
        let index = 2;
        prop_set(DataType::Bfe, list_address, 20, insert_value, index, 21);
    }

    #[test]
    fn list_u32_n_is_one_set_at_edge_a() {
        let list_address = BFieldElement::new(48);
        let insert_value = vec![BFieldElement::new(1337)];
        let index = 19;
        prop_set(DataType::Bfe, list_address, 20, insert_value, index, 21);
    }

    #[test]
    fn list_u32_n_is_one_set_at_edge_b() {
        let list_address = BFieldElement::new(48);
        let insert_value = vec![BFieldElement::new(1337)];
        let index = 19;
        prop_set(DataType::Bfe, list_address, 20, insert_value, index, 21);
    }

    #[should_panic]
    #[test]
    fn list_u32_n_is_one_set_out_of_bounds_a() {
        let list_address = BFieldElement::new(48);
        let insert_value = vec![BFieldElement::new(1337)];
        let index = 20;
        prop_set(DataType::Bfe, list_address, 20, insert_value, index, 21);
    }

    #[should_panic]
    #[test]
    fn list_u32_n_is_one_set_out_of_bounds_b() {
        let list_address = BFieldElement::new(48);
        let insert_value = vec![BFieldElement::new(1337)];
        let index = 21;
        prop_set(DataType::Bfe, list_address, 20, insert_value, index, 21);
    }

    #[test]
    fn list_u32_n_is_three_set() {
        let list_address = BFieldElement::new(48);
        let insert_value = vec![
            BFieldElement::new(1337),
            BFieldElement::new(1337),
            BFieldElement::new(1337),
        ];
        let index = 2;
        prop_set(DataType::Xfe, list_address, 20, insert_value, index, 20);
    }

    #[test]
    fn list_u32_n_is_two_set() {
        let list_address = BFieldElement::new(1841);
        let push_value = vec![BFieldElement::new(133700), BFieldElement::new(32)];
        let index = 0;
        prop_set(DataType::U64, list_address, 20, push_value, index, 20);
    }

    #[should_panic]
    #[test]
    fn list_u32_n_is_two_set_out_of_bounds_a() {
        let list_address = BFieldElement::new(48);
        let insert_value = vec![BFieldElement::new(1337)];
        let index = 20;
        prop_set(DataType::U64, list_address, 20, insert_value, index, 21);
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
        prop_set(DataType::Digest, list_address, 233, push_value, 58, 3000);
    }

    #[should_panic]
    #[test]
    fn list_u32_n_is_five_set_out_of_bounds_a() {
        let list_address = BFieldElement::new(48);
        let insert_value = vec![BFieldElement::new(1337)];
        let index = 20;
        prop_set(DataType::U64, list_address, 20, insert_value, index, 21);
    }

    #[should_panic]
    #[test]
    fn list_u32_n_is_five_set_out_of_bounds_b() {
        let list_address = BFieldElement::new(48);
        let insert_value = vec![BFieldElement::new(1337)];
        let index = 21;
        prop_set(DataType::U64, list_address, 20, insert_value, index, 22);
    }

    fn prop_set(
        data_type: DataType,
        list_address: BFieldElement,
        init_list_length: u32,
        push_value: Vec<BFieldElement>,
        index: u32,
        capacity: u32,
    ) {
        let expected_end_stack = [empty_stack()].concat();
        let mut init_stack = empty_stack();

        for i in 0..data_type.stack_size() {
            init_stack.push(push_value[data_type.stack_size() - 1 - i]);
        }
        init_stack.push(list_address);
        init_stack.push(BFieldElement::new(index as u64));

        let mut vm_memory = HashMap::default();

        // Insert length indicator of list, lives on offset = 0 from `list_address`
        safe_insert_random_list(
            &data_type,
            list_address,
            capacity,
            init_list_length as usize,
            &mut vm_memory,
        );

        let memory = test_rust_equivalence_given_input_values_deprecated::<SafeSet>(
            &SafeSet {
                data_type: data_type.clone(),
            },
            &init_stack,
            &[],
            vm_memory,
            0,
            Some(&expected_end_stack),
        )
        .final_ram;

        // Verify that length indicator is unchanged
        assert_eq!(
            BFieldElement::new(init_list_length as u64),
            memory[&list_address]
        );

        // verify that value was inserted at expected place
        for i in 0..data_type.stack_size() {
            assert_eq!(
                push_value[i],
                memory[&BFieldElement::new(
                    list_address.value()
                        + 2
                        + data_type.stack_size() as u64 * index as u64
                        + i as u64
                )]
            );
        }
    }
}

#[cfg(test)]
mod benches {
    use crate::snippet_bencher::bench_and_write;

    use super::*;

    #[test]
    fn safe_set_benchmark() {
        bench_and_write(SafeSet {
            data_type: DataType::Digest,
        });
    }
}
