use std::collections::HashMap;

use itertools::Itertools;
use num::One;
use rand::random;
use rand::thread_rng;
use rand::Rng;
use triton_vm::prelude::*;

use crate::data_type::DataType;
use crate::empty_stack;
use crate::library::Library;
use crate::rust_shadowing_helper_functions::list::untyped_insert_random_list;
use crate::traits::deprecated_snippet::DeprecatedSnippet;
use crate::ExecutionState;

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct Pop {
    pub element_type: DataType,
}

impl Pop {
    pub fn new(element_type: DataType) -> Self {
        Self { element_type }
    }
}

impl DeprecatedSnippet for Pop {
    fn entrypoint_name(&self) -> String {
        format!(
            "tasm_list_pop___{}",
            self.element_type.label_friendly_name()
        )
    }

    fn input_field_names(&self) -> Vec<String> {
        vec!["*list".to_string()]
    }

    fn input_types(&self) -> Vec<DataType> {
        vec![DataType::List(Box::new(self.element_type.clone()))]
    }

    fn output_field_names(&self) -> Vec<String> {
        let mut ret: Vec<String> = vec![];
        let element_size = self.element_type.stack_size();
        for i in 0..element_size {
            ret.push(format!("element_{}", element_size - 1 - i));
        }

        ret
    }

    fn output_types(&self) -> Vec<DataType> {
        vec![self.element_type.clone()]
    }

    fn stack_diff(&self) -> isize {
        self.element_type.stack_size() as isize - 1
    }

    /// Pop last element from list. Does *not* actually delete the last
    /// element but instead leaves it in memory.
    fn function_code(&self, _library: &mut Library) -> String {
        let entry_point = self.entrypoint_name();

        let element_size = self.element_type.stack_size();
        let mul_with_size = if element_size.is_one() {
            String::default()
        } else {
            format!("push {element_size}\n mul\n")
        };
        triton_asm!(
            // BEFORE: _ *list
            // AFTER:  _ elem{{N - 1}}, elem{{N - 2}}, ..., elem{{0}}
            {entry_point}:
                read_mem 1
                push 1
                add
                // stack : _  length *list

                // Assert that length is not 0
                dup 1
                push 0
                eq
                push 0
                eq
                assert
                // stack : _  length *list

                // Decrease length value by one and write back to memory
                dup 1
                // _  length *list length

                push -1
                add
                // _  length *list (length - 1)

                swap 1
                // _  length (length - 1) *list

                write_mem 1
                // _  length *first_element

                swap 1
                // _  *first_element, initial_length

                {mul_with_size}
                // stack : _  *first_element, offset_for_last_element = (N * initial_length)

                add
                // stack : _  address_for_next_element

                push -1
                add
                // stack : _  address_for_current_element_last_word

                {&self.element_type.read_value_from_memory_pop_pointer()}
                // Stack: _  [elements]

                return

        )
        .iter()
        .join("\n")
    }

    fn crash_conditions(&self) -> Vec<String> {
        vec!["list stack underflow".to_string()]
    }

    fn gen_input_states(&self) -> Vec<ExecutionState> {
        vec![prepare_state(&self.element_type)]
    }

    fn common_case_input_state(&self) -> ExecutionState {
        prepare_state(&self.element_type)
    }

    fn worst_case_input_state(&self) -> ExecutionState {
        prepare_state(&self.element_type)
    }

    fn rust_shadowing(
        &self,
        stack: &mut Vec<BFieldElement>,
        _std_in: Vec<BFieldElement>,
        _secret_in: Vec<BFieldElement>,
        memory: &mut HashMap<BFieldElement, BFieldElement>,
    ) {
        let list_address = stack.pop().unwrap();
        let initial_list_length = memory[&list_address];

        // Update length indicator
        memory.insert(list_address, initial_list_length - BFieldElement::one());

        let mut last_used_address = list_address
            + initial_list_length * BFieldElement::new(self.element_type.stack_size() as u64);

        for _ in 0..self.element_type.stack_size() {
            let elem = memory[&last_used_address];
            stack.push(elem);
            last_used_address -= BFieldElement::one();
        }
    }
}

fn prepare_state(element_type: &DataType) -> ExecutionState {
    let list_pointer: u32 = random();
    let list_pointer = BFieldElement::new(list_pointer as u64);
    let old_length: usize = thread_rng().gen_range(1..30);
    let mut stack = empty_stack();
    stack.push(list_pointer);
    let mut memory = HashMap::default();
    untyped_insert_random_list(
        list_pointer,
        old_length,
        &mut memory,
        element_type.stack_size(),
    );
    ExecutionState::with_stack_and_memory(stack, memory)
}

#[cfg(test)]
mod tests {
    use num::Zero;
    use rand::RngCore;

    use crate::test_helpers::test_rust_equivalence_given_input_values_deprecated;
    use crate::test_helpers::test_rust_equivalence_multiple_deprecated;

    use super::*;

    #[test]
    fn new_snippet_test() {
        test_rust_equivalence_multiple_deprecated(
            &Pop {
                element_type: DataType::U32,
            },
            true,
        );
        test_rust_equivalence_multiple_deprecated(
            &Pop {
                element_type: DataType::U64,
            },
            true,
        );
        test_rust_equivalence_multiple_deprecated(
            &Pop {
                element_type: DataType::Xfe,
            },
            true,
        );
        test_rust_equivalence_multiple_deprecated(
            &Pop {
                element_type: DataType::Digest,
            },
            true,
        );
    }

    #[test]
    #[should_panic]
    fn panic_if_pop_on_empty_list_1() {
        let list_address = BFieldElement::new(48);
        prop_pop::<1>(DataType::Bfe, list_address, 0);
    }

    #[test]
    #[should_panic]
    fn panic_if_pop_on_empty_list_2() {
        let list_address = BFieldElement::new(48);
        prop_pop::<2>(DataType::U64, list_address, 0);
    }

    #[test]
    #[should_panic]
    fn panic_if_pop_on_empty_list_3() {
        let list_address = BFieldElement::new(48);
        prop_pop::<3>(DataType::Xfe, list_address, 0);
    }

    #[test]
    fn list_u32_n_is_n_pop() {
        let list_address = BFieldElement::new(48);
        prop_pop::<1>(DataType::Bfe, list_address, 24);
        prop_pop::<2>(DataType::U64, list_address, 48);
        prop_pop::<3>(DataType::Xfe, list_address, 3);
        // prop_pop::<4>(list_address, 4);
        prop_pop::<5>(DataType::Digest, list_address, 20);
        // prop_pop::<6>(list_address, 20);
        // prop_pop::<7>(list_address, 20);
        // prop_pop::<8>(list_address, 20);
        // prop_pop::<9>(list_address, 20);
        // prop_pop::<10>(list_address, 1);
        // prop_pop::<11>(list_address, 33);
        // prop_pop::<12>(list_address, 20);
        // prop_pop::<13>(list_address, 20);
        // prop_pop::<14>(list_address, 20);
        // prop_pop::<15>(list_address, 20);
        // prop_pop::<16>(list_address, 20);
    }

    fn prop_pop<const N: usize>(
        element_type: DataType,
        list_address: BFieldElement,
        init_list_length: u32,
    ) {
        let mut init_stack = empty_stack();
        init_stack.push(list_address);

        let mut memory = HashMap::default();

        // Insert length indicator of list, lives on offset = 0 from `list_address`
        memory.insert(list_address, BFieldElement::new(init_list_length as u64));

        // Insert random values for the elements in the list
        let mut rng = thread_rng();
        let mut last_element: [BFieldElement; N] = [BFieldElement::zero(); N];
        let mut j = 1;
        for _ in 0..init_list_length {
            last_element = (0..N)
                .map(|_| BFieldElement::new(rng.next_u64()))
                .collect_vec()
                .try_into()
                .unwrap();
            for elem in last_element.iter() {
                memory.insert(list_address + BFieldElement::new(j), *elem);
                j += 1;
            }
        }

        let mut expected_end_stack = empty_stack();

        for i in 0..N {
            expected_end_stack.push(last_element[N - 1 - i]);
        }

        let memory = test_rust_equivalence_given_input_values_deprecated(
            &Pop { element_type },
            &init_stack,
            &[],
            memory,
            Some(&expected_end_stack),
        )
        .ram;

        // Verify that length is now indicated to be `init_list_length - 1`
        assert_eq!(
            BFieldElement::new(init_list_length as u64) - BFieldElement::one(),
            memory[&list_address]
        );
    }
}

#[cfg(test)]
mod benches {
    use crate::snippet_bencher::bench_and_write;

    use super::*;

    #[test]
    fn pop_benchmark() {
        bench_and_write(Pop::new(DataType::Digest));
    }
}
