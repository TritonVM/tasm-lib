use std::collections::HashMap;

use itertools::Itertools;
use rand::prelude::*;
use triton_vm::prelude::*;

use crate::empty_stack;
use crate::prelude::*;
use crate::rust_shadowing_helper_functions::list::list_get;
use crate::rust_shadowing_helper_functions::list::untyped_insert_random_list;
use crate::traits::deprecated_snippet::DeprecatedSnippet;
use crate::InitVmState;

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct Get {
    pub element_type: DataType,
}

impl Get {
    pub fn new(data_type: DataType) -> Self {
        Self {
            element_type: data_type,
        }
    }
}

impl DeprecatedSnippet for Get {
    fn entrypoint_name(&self) -> String {
        format!(
            "tasmlib_list_get_element___{}",
            self.element_type.label_friendly_name()
        )
    }

    fn input_field_names(&self) -> Vec<String> {
        vec!["*list".to_string(), "index".to_string()]
    }

    fn input_types(&self) -> Vec<DataType> {
        vec![
            DataType::List(Box::new(self.element_type.clone())),
            DataType::U32,
        ]
    }

    fn output_field_names(&self) -> Vec<String> {
        // This function returns element_0 on the top of the stack and the other elements below it.
        // E.g.: _ elem_2 elem_1 elem_0
        let mut ret: Vec<String> = vec![];
        let size = self.element_type.stack_size();
        for i in 0..size {
            ret.push(format!("element_{}", size - 1 - i));
        }

        ret
    }

    fn output_types(&self) -> Vec<DataType> {
        vec![DataType::Bfe; self.element_type.stack_size()]
    }

    fn stack_diff(&self) -> isize {
        self.element_type.stack_size() as isize - 2
    }

    fn function_code(&self, _library: &mut Library) -> String {
        let entrypoint = self.entrypoint_name();
        // Code to read an element from a list. No bounds-check.

        let element_size = self.element_type.stack_size();

        let mul_with_size = match element_size {
            1 => vec![],
            _ => triton_asm!(push {element_size} mul),
        };

        triton_asm!(
            // BEFORE: _ *list index
            // AFTER:  _ elem{N - 1}, elem{N - 2}, ..., elem{0}
            {entrypoint}:
                push 1
                add
                {&mul_with_size}
                // stack: _ *list (N * (index + 1))

                // assert read access is in bounds
                split swap 1 push 0 eq assert
                // stack: _ *list (N * (index + 1))

                add
                // stack: _ (*list + N * index + 1)

                {&self.element_type.read_value_from_memory_pop_pointer()}
                // stack: _ elem{{N - 1}}, elem{{N - 2}}, ..., elem{{0}}

                return
        )
        .iter()
        .join("\n")
    }

    fn crash_conditions(&self) -> Vec<String> {
        vec![]
    }

    fn gen_input_states(&self) -> Vec<InitVmState> {
        let mut rng = thread_rng();
        let list_length = rng.gen_range(1..=100);
        let index_to_read = rng.gen_range(0..list_length);
        vec![self.input_state(list_length, index_to_read)]
    }

    fn common_case_input_state(&self) -> InitVmState {
        self.input_state(1 << 5, 1 << 4)
    }

    fn worst_case_input_state(&self) -> InitVmState {
        self.input_state(1 << 6, (1 << 6) - 1)
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
        let element: Vec<BFieldElement> = list_get(
            list_pointer,
            index as usize,
            memory,
            self.element_type.stack_size(),
        );

        stack.extend(element.into_iter().rev());
    }
}

impl Get {
    fn input_state(&self, list_length: usize, index: usize) -> InitVmState {
        let list_pointer: u32 = random();
        let list_pointer = BFieldElement::new(list_pointer as u64);
        let mut stack = empty_stack();
        stack.push(list_pointer);
        stack.push(BFieldElement::new(index as u64));

        let mut memory = HashMap::default();

        untyped_insert_random_list(
            list_pointer,
            list_length,
            &mut memory,
            self.element_type.stack_size(),
        );

        InitVmState::with_stack_and_memory(stack, memory)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::empty_stack;
    use crate::test_helpers::test_rust_equivalence_given_input_values_deprecated;
    use crate::test_helpers::test_rust_equivalence_multiple_deprecated;

    #[test]
    fn new_snippet_test() {
        test_rust_equivalence_multiple_deprecated(
            &Get {
                element_type: DataType::Xfe,
            },
            true,
        );
    }

    #[test]
    fn get_simple_1() {
        let list_address = BFieldElement::new(48);
        let list_length = 10;
        for i in 0..list_length {
            prop_get(DataType::Bfe, list_address, i, list_length);
        }
    }

    #[test]
    fn get_simple_2() {
        let list_address = BFieldElement::new(48);
        let list_length = 10;
        for i in 0..list_length {
            prop_get(DataType::U64, list_address, i, list_length);
        }
    }

    #[test]
    fn get_simple_3() {
        let list_address = BFieldElement::new(48);
        let list_length = 10;
        for i in 0..list_length {
            prop_get(DataType::Xfe, list_address, i, list_length);
        }
    }

    #[test]
    fn get_simple_15() {
        let list_address = BFieldElement::new(48);
        let list_length = 10;
        for i in 0..list_length {
            prop_get(
                DataType::Tuple(vec![DataType::Digest; 3]),
                list_address,
                i,
                list_length,
            );
        }
    }

    fn prop_get(data_type: DataType, list_pointer: BFieldElement, index: u32, list_length: u32) {
        let mut init_stack = empty_stack();
        init_stack.push(list_pointer);
        init_stack.push(BFieldElement::new(index as u64));

        let mut memory = HashMap::default();

        // Insert length indicator of list, lives on offset = 0 from `list_address`
        memory.insert(list_pointer, BFieldElement::new(list_length as u64));

        // Insert random values for the elements in the list
        let element_size = data_type.stack_size();
        let mut rng = thread_rng();
        let mut j = 1;
        for _ in 0..list_length {
            let element = (0..element_size).map(|_| rng.gen()).collect_vec();
            for elem in element.iter() {
                memory.insert(list_pointer + BFieldElement::new(j), *elem);
                j += 1;
            }
        }
        let targeted_element: Vec<BFieldElement> =
            list_get(list_pointer, index as usize, &memory, element_size);

        let mut expected_end_stack = empty_stack();
        for i in 0..element_size {
            expected_end_stack.push(targeted_element[element_size - 1 - i]);
        }

        test_rust_equivalence_given_input_values_deprecated(
            &Get {
                element_type: data_type,
            },
            &init_stack,
            &[],
            memory,
            Some(&expected_end_stack),
        );
    }
}

#[cfg(test)]
mod benches {
    use super::*;
    use crate::snippet_bencher::bench_and_write;

    #[test]
    fn get_benchmark() {
        bench_and_write(Get::new(DataType::Digest));
    }
}
