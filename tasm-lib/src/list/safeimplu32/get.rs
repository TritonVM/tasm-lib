use itertools::Itertools;
use std::cmp;
use std::collections::HashMap;

use crate::twenty_first::shared_math::b_field_element::BFieldElement;
use num::One;
use rand::{random, thread_rng, Rng};
use triton_vm::{triton_asm, NonDeterminism};

use crate::data_type::DataType;
use crate::library::Library;
use crate::rust_shadowing_helper_functions::safe_list::{safe_insert_random_list, safe_list_get};
use crate::traits::deprecated_snippet::DeprecatedSnippet;
use crate::{empty_stack, ExecutionState};

#[derive(Clone, Debug)]
pub struct SafeGet {
    pub data_type: DataType,
}

impl DeprecatedSnippet for SafeGet {
    fn entrypoint_name(&self) -> String {
        format!(
            "tasm_list_safeimplu32_get_element___{}",
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
        // This function returns element_0 on the top of the stack and the other elements below it. E.g.: _ elem_2 elem_1 elem_0
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

        let element_size = self.data_type.stack_size();
        let mul_with_size = match element_size {
            1 => vec![],
            _ => triton_asm!(push {element_size} mul),
        };

        triton_asm!(
            // BEFORE: _ *list index
            // AFTER:  _ elem{N - 1} elem{N - 2} … elem{0}
            {entrypoint}:
                dup 1               // _ *list index *list
                read_mem 1          // _ *list index length *(list - 1)
                swap 1              // _ *list index *(list - 1) length
                dup 2               // _ *list index *(list - 1) length index
                lt                  // _ *list index *(list - 1) (length < index)
                assert              // _ *list index *(list - 1)
                pop 1               // _ *list index

                push 1              // _ *list index 1
                add                 // _ *list (index + 1)
                {&mul_with_size}    // _ *list (N * (index + 1))
                add                 // _ *(list + N * (index + 1))

                push 1              // _ *(list + N * (index + 1)) 1
                add                 // _ *(list + N * (index + 1) + 1)

                {&self.data_type.read_value_from_memory_pop_pointer()}
                // _ elem{N - 1} elem{N - 2} … elem{0}

                return
        )
        .iter()
        .join("\n")
    }

    fn crash_conditions(&self) -> Vec<String> {
        vec!["Index out of bounds".to_string()]
    }

    fn gen_input_states(&self) -> Vec<ExecutionState> {
        let mut rng = thread_rng();
        let list_pointer: BFieldElement = random();
        let capacity = rng.gen_range(1..1000);
        let list_length: usize = rng.gen_range(1..=cmp::min(capacity, 100));
        let index: usize = rng.gen_range(0..list_length);
        let mut stack = empty_stack();
        stack.push(list_pointer);
        stack.push(BFieldElement::new(index as u64));

        let mut memory = HashMap::default();

        safe_insert_random_list(
            &self.data_type,
            list_pointer,
            capacity as u32,
            list_length,
            &mut memory,
        );

        let nondeterminism = NonDeterminism::default().with_ram(memory);
        vec![ExecutionState {
            stack,
            std_in: vec![],
            nondeterminism,
            words_allocated: 0,
        }]
    }

    fn common_case_input_state(&self) -> ExecutionState {
        const COMMON_LENGTH: usize = 1 << 5;
        get_benchmark_input_state(COMMON_LENGTH, &self.data_type)
    }

    fn worst_case_input_state(&self) -> ExecutionState {
        const COMMON_LENGTH: usize = 1 << 6;
        get_benchmark_input_state(COMMON_LENGTH, &self.data_type)
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
        let element: Vec<BFieldElement> = safe_list_get(
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

fn get_benchmark_input_state(list_length: usize, data_type: &DataType) -> ExecutionState {
    let mut memory = HashMap::default();
    let list_pointer: BFieldElement = BFieldElement::one();

    let capacity = list_length * 2;
    safe_insert_random_list(
        data_type,
        list_pointer,
        capacity as u32,
        list_length,
        &mut memory,
    );

    let mut stack = empty_stack();
    stack.push(list_pointer);
    stack.push(BFieldElement::new((list_length - 1) as u64));

    let nondeterminism = NonDeterminism::default().with_ram(memory);
    ExecutionState {
        stack,
        std_in: vec![],
        nondeterminism,
        words_allocated: 1,
    }
}

#[cfg(test)]
mod tests {
    use crate::twenty_first::shared_math::b_field_element::BFieldElement;

    use super::*;
    use crate::empty_stack;
    use crate::test_helpers::{
        test_rust_equivalence_given_input_values_deprecated,
        test_rust_equivalence_multiple_deprecated,
    };

    #[test]
    fn new_snippet_test() {
        for _ in 0..10 {
            test_rust_equivalence_multiple_deprecated(
                &SafeGet {
                    data_type: DataType::Bool,
                },
                true,
            );
            test_rust_equivalence_multiple_deprecated(
                &SafeGet {
                    data_type: DataType::U32,
                },
                true,
            );
            test_rust_equivalence_multiple_deprecated(
                &SafeGet {
                    data_type: DataType::U64,
                },
                true,
            );
            test_rust_equivalence_multiple_deprecated(
                &SafeGet {
                    data_type: DataType::Bfe,
                },
                true,
            );
            test_rust_equivalence_multiple_deprecated(
                &SafeGet {
                    data_type: DataType::Xfe,
                },
                true,
            );
            test_rust_equivalence_multiple_deprecated(
                &SafeGet {
                    data_type: DataType::Digest,
                },
                true,
            );
        }
    }

    #[test]
    fn get_simple_1() {
        let list_address = BFieldElement::new(48);
        for i in 0..10 {
            prop_get(&DataType::Bfe, list_address, i, 10);
        }
    }

    #[test]
    fn read_at_edge_1() {
        let list_address = BFieldElement::new(48);
        prop_get(&DataType::Bfe, list_address, 8, 10);
        prop_get(&DataType::Bfe, list_address, 9, 10);
    }

    #[should_panic]
    #[test]
    fn read_out_of_bounds_1_a() {
        let list_address = BFieldElement::new(48);
        let length = 10;
        let index = 10;
        prop_get(&DataType::Bfe, list_address, index, length);
    }

    #[should_panic]
    #[test]
    fn read_out_of_bounds_1_b() {
        let list_address = BFieldElement::new(48);
        let length = 10;
        let index = 11;
        prop_get(&DataType::Bfe, list_address, index, length);
    }

    #[should_panic]
    #[test]
    fn read_out_of_bounds_1_c() {
        let list_address = BFieldElement::new(48);
        let length = 10;
        let index = 12;
        prop_get(&DataType::Bfe, list_address, index, length);
    }

    #[test]
    fn get_simple_2() {
        let list_address = BFieldElement::new(48);
        for i in 0..10 {
            prop_get(&DataType::U64, list_address, i, 10);
        }
    }

    #[test]
    fn read_at_edge_2() {
        let list_address = BFieldElement::new(48);
        prop_get(&DataType::U64, list_address, 0, 10);
        prop_get(&DataType::U64, list_address, 1, 10);
        prop_get(&DataType::U64, list_address, 8, 10);
        prop_get(&DataType::U64, list_address, 9, 10);
    }

    #[should_panic]
    #[test]
    fn read_out_of_bounds_2_a() {
        let list_address = BFieldElement::new(48);
        let length = 10;
        let index = 10;
        prop_get(&DataType::U64, list_address, index, length);
    }

    #[should_panic]
    #[test]
    fn read_out_of_bounds_2_b() {
        let list_address = BFieldElement::new(48);
        let length = 10;
        let index = 11;
        prop_get(&DataType::U64, list_address, index, length);
    }

    #[should_panic]
    #[test]
    fn read_out_of_bounds_2_c() {
        let list_address = BFieldElement::new(48);
        let length = 10;
        let index = 12;
        prop_get(&DataType::U64, list_address, index, length);
    }

    #[test]
    fn get_simple_3() {
        let list_address = BFieldElement::new(48);
        for i in 0..10 {
            prop_get(&DataType::Xfe, list_address, i, 10);
        }
    }

    #[test]
    fn get_simple_5() {
        let list_address = BFieldElement::new(48);
        for i in 0..10 {
            prop_get(&DataType::Digest, list_address, i, 10);
        }
    }

    #[test]
    fn read_at_edge_5() {
        let list_address = BFieldElement::new(48);
        prop_get(&DataType::Digest, list_address, 0, 10);
        prop_get(&DataType::Digest, list_address, 1, 10);
        prop_get(&DataType::Digest, list_address, 8, 10);
        prop_get(&DataType::Digest, list_address, 9, 10);
    }

    #[should_panic]
    #[test]
    fn read_out_of_bounds_5_a() {
        let list_address = BFieldElement::new(48);
        let length = 10;
        let index = 10;
        prop_get(&DataType::Digest, list_address, index, length);
    }

    #[should_panic]
    #[test]
    fn read_out_of_bounds_5_b() {
        let list_address = BFieldElement::new(48);
        let length = 10;
        let index = 11;
        prop_get(&DataType::Digest, list_address, index, length);
    }

    #[should_panic]
    #[test]
    fn read_out_of_bounds_5_c() {
        let list_address = BFieldElement::new(48);
        let length = 10;
        let index = 12;
        prop_get(&DataType::Digest, list_address, index, length);
    }

    fn prop_get(data_type: &DataType, list_pointer: BFieldElement, index: u32, list_length: u32) {
        let element_size = data_type.stack_size();

        let mut init_stack = empty_stack();
        init_stack.push(list_pointer);
        init_stack.push(BFieldElement::new(index as u64));

        let mut memory = HashMap::default();

        // Insert length indicator of list, lives on offset = 0 from `list_address`
        memory.insert(list_pointer, BFieldElement::new(list_length as u64));

        let list_capacity = 100;
        safe_insert_random_list(
            data_type,
            list_pointer,
            list_capacity,
            list_length as usize,
            &mut memory,
        );

        let targeted_element: Vec<BFieldElement> =
            safe_list_get(list_pointer, index as usize, &memory, element_size);

        let mut expected_end_stack = empty_stack();

        for i in 0..element_size {
            expected_end_stack.push(targeted_element[element_size - 1 - i]);
        }

        test_rust_equivalence_given_input_values_deprecated(
            &SafeGet {
                data_type: data_type.to_owned(),
            },
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
    use super::*;
    use crate::snippet_bencher::bench_and_write;

    #[test]
    fn safe_get_benchmark() {
        bench_and_write(SafeGet {
            data_type: DataType::Digest,
        });
    }
}
