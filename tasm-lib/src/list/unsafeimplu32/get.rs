use std::collections::HashMap;

use itertools::Itertools;
use rand::random;
use rand::thread_rng;
use rand::Rng;
use triton_vm::prelude::*;

use crate::data_type::DataType;
use crate::empty_stack;
use crate::library::Library;
use crate::rust_shadowing_helper_functions::unsafe_list::unsafe_list_get;
use crate::rust_shadowing_helper_functions::unsafe_list::untyped_unsafe_insert_random_list;
use crate::traits::deprecated_snippet::DeprecatedSnippet;
use crate::ExecutionState;

#[derive(Clone, Debug)]
pub struct UnsafeGet {
    pub data_type: DataType,
}

impl DeprecatedSnippet for UnsafeGet {
    fn entrypoint_name(&self) -> String {
        format!(
            "tasm_list_unsafeimplu32_get_element___{}",
            self.data_type.label_friendly_name()
        )
    }

    fn input_field_names(&self) -> Vec<String> {
        vec!["*list".to_string(), "index".to_string()]
    }

    fn input_types(&self) -> Vec<DataType> {
        vec![
            DataType::List(Box::new(self.data_type.clone())),
            DataType::U32,
        ]
    }

    fn output_field_names(&self) -> Vec<String> {
        // This function returns element_0 on the top of the stack and the other elements below it.
        // E.g.: _ elem_2 elem_1 elem_0
        let mut ret: Vec<String> = vec![];
        let size = self.data_type.stack_size();
        for i in 0..size {
            ret.push(format!("element_{}", size - 1 - i));
        }

        ret
    }

    fn output_types(&self) -> Vec<DataType> {
        vec![DataType::Bfe; self.data_type.stack_size()]
    }

    fn stack_diff(&self) -> isize {
        self.data_type.stack_size() as isize - 2
    }

    fn function_code(&self, _library: &mut Library) -> String {
        let entrypoint = self.entrypoint_name();
        // Code to read an element from a list. No bounds-check.

        let element_size = self.data_type.stack_size();

        // Code to multiply with size. If size is 1, do nothing to save two clock cycles.
        let mul_with_size = if element_size == 1 {
            vec![]
        } else {
            triton_asm!(push {element_size} mul)
        };

        triton_asm!(
            // BEFORE: _ *list index
            // AFTER:  _ elem{{N - 1}}, elem{{N - 2}}, ..., elem{{0}}
            {entrypoint}:
                push 1
                add
                {&mul_with_size}
                // stack: _ *list (N * (index + 1))

                add
                // stack: _ (*list + N * index + 1)

                {&self.data_type.read_value_from_memory_pop_pointer()}
                // stack: _ elem{{N - 1}}, elem{{N - 2}}, ..., elem{{0}}

                return
        )
        .iter()
        .join("\n")
    }

    fn crash_conditions(&self) -> Vec<String> {
        vec![]
    }

    fn gen_input_states(&self) -> Vec<ExecutionState> {
        let mut rng = thread_rng();
        vec![input_state(rng.gen_range(1..100))]
    }

    fn common_case_input_state(&self) -> ExecutionState {
        input_state(1 << 5)
    }

    fn worst_case_input_state(&self) -> ExecutionState {
        input_state(1 << 6)
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
        let element: Vec<BFieldElement> = unsafe_list_get(
            list_pointer,
            index as usize,
            memory,
            self.data_type.stack_size(),
        );

        // elements are placed on stack as: `elem[N - 1] elem[N - 2] .. elem[0]`
        for i in (0..self.data_type.stack_size()).rev() {
            stack.push(element[i]);
        }
    }
}

fn input_state(list_length: usize) -> ExecutionState {
    let mut rng = thread_rng();
    let list_pointer: u32 = random();
    let list_pointer = BFieldElement::new(list_pointer as u64);
    let index: usize = rng.gen_range(0..list_length);
    let mut stack = empty_stack();
    stack.push(list_pointer);
    stack.push(BFieldElement::new(index as u64));

    let mut memory = HashMap::default();

    untyped_unsafe_insert_random_list(list_pointer, list_length, &mut memory, 4);

    let nondeterminism = NonDeterminism::default().with_ram(memory);
    ExecutionState {
        stack,
        std_in: vec![],
        nondeterminism,
        words_allocated: 0,
    }
}

#[cfg(test)]
mod tests {
    use crate::test_helpers::test_rust_equivalence_given_input_values_deprecated;
    use crate::test_helpers::test_rust_equivalence_multiple_deprecated;

    use super::*;

    #[test]
    fn new_snippet_test() {
        test_rust_equivalence_multiple_deprecated(
            &UnsafeGet {
                data_type: DataType::Xfe,
            },
            true,
        );
    }

    #[test]
    fn get_simple_1() {
        let list_address = BFieldElement::new(48);
        for i in 0..10 {
            prop_get(DataType::Bfe, list_address, i, 10);
        }
    }

    #[test]
    fn get_simple_2() {
        let list_address = BFieldElement::new(48);
        for i in 0..10 {
            prop_get(DataType::U64, list_address, i, 10);
        }
    }

    #[test]
    fn get_simple_3() {
        let list_address = BFieldElement::new(48);
        for i in 0..10 {
            prop_get(DataType::Xfe, list_address, i, 10);
        }
    }

    #[test]
    fn get_simple_15() {
        let list_address = BFieldElement::new(48);
        for i in 0..10 {
            prop_get(DataType::Digest, list_address, i, 10);
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
            unsafe_list_get(list_pointer, index as usize, &memory, element_size);

        let mut expected_end_stack = empty_stack();
        for i in 0..element_size {
            expected_end_stack.push(targeted_element[element_size - 1 - i]);
        }

        test_rust_equivalence_given_input_values_deprecated(
            &UnsafeGet { data_type },
            &init_stack,
            &[],
            memory,
            0,
            Some(&expected_end_stack),
        );
    }
}

#[cfg(test)]
mod benches {
    use crate::snippet_bencher::bench_and_write;

    use super::*;

    #[test]
    fn unsafe_get_benchmark() {
        bench_and_write(UnsafeGet {
            data_type: DataType::Digest,
        });
    }
}
