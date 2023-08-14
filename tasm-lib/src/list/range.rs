use std::collections::HashMap;

use triton_vm::NonDeterminism;
use twenty_first::shared_math::b_field_element::BFieldElement;

use crate::{
    get_init_tvm_stack,
    list::{
        safe_u32::{new::SafeNew, set_length::SafeSetLength},
        unsafe_u32::{new::UnsafeNew, set_length::UnsafeSetLength},
    },
    rust_shadowing_helper_functions::{self},
    snippet::{DataType, DepracatedSnippet},
    ExecutionState,
};

use super::ListType;

/// Generates a (safe or unsafe) list containing all integers between
/// the minimum (inclusive lower bound) and the supremum (exclusive
/// upper bound).
#[derive(Clone, Debug)]
pub struct Range {
    pub list_type: ListType,
}

impl Range {
    fn init_state(minimum: u32, supremum: u32) -> ExecutionState {
        let mut stack = get_init_tvm_stack();
        stack.push(BFieldElement::new(minimum as u64));
        stack.push(BFieldElement::new(supremum as u64));
        ExecutionState {
            stack,
            std_in: vec![],
            nondeterminism: NonDeterminism::new(vec![]),
            memory: HashMap::new(),
            words_allocated: 1,
        }
    }
}

impl DepracatedSnippet for Range {
    fn entrypoint_name(&self) -> String {
        format!("tasm_list_{}_range", self.list_type)
    }

    fn input_field_names(&self) -> Vec<String>
    where
        Self: Sized,
    {
        vec!["minimum".to_string(), "supremum".to_string()]
    }

    fn input_types(&self) -> Vec<DataType> {
        vec![DataType::U32, DataType::U32]
    }

    fn output_types(&self) -> Vec<DataType> {
        vec![DataType::List(Box::new(DataType::U32))]
    }

    fn output_field_names(&self) -> Vec<String>
    where
        Self: Sized,
    {
        vec!["*list".to_string()]
    }

    fn stack_diff(&self) -> isize
    where
        Self: Sized,
    {
        -1
    }

    fn function_code(&self, library: &mut crate::library::Library) -> String {
        let entrypoint = self.entrypoint_name();

        let new_list = match self.list_type {
            ListType::Safe => library.import(Box::new(SafeNew(DataType::U32))),
            ListType::Unsafe => library.import(Box::new(UnsafeNew(DataType::U32))),
        };

        let set_length = match self.list_type {
            ListType::Safe => library.import(Box::new(SafeSetLength(DataType::U32))),
            ListType::Unsafe => library.import(Box::new(UnsafeSetLength(DataType::U32))),
        };

        let safety_offset = match self.list_type {
            ListType::Safe => 2,
            ListType::Unsafe => 1,
        };

        format!(
            "
            // BEFORE: _ minimum supremum
            // AFTER: _ list
            {entrypoint}:
                dup 0 push 1 add dup 2 // _ minimum supremum supremum+1 minimum
                lt assert // asserts minimum <= supremum

                // calculate length
                dup 0 dup 2 // _ minimum supremum supremum minimum
                push -1 mul add // _ minimum supremum supremum-minimum
                // _ minimum supremum length

                // create list object
                dup 0 // _ minimum supremum length length
                call {new_list} // _ minimum supremum length list
                dup 1 // _ minimum supremum length list length
                call {set_length} // _ minimum supremum length list

                // loop to populate
                call {entrypoint}_loop // _ minimum supremum 0 list

                // clean up stack
                swap 3 pop pop pop
                return

            // INVARIANT: _ minimum supremum index list
            {entrypoint}_loop:
                // compute termination condition
                dup 1 push 0 eq // _ minimum supremum index list index==0
                skiz return

                // decrement index
                swap 1 push -1 add // _ minimum supremum list index-1

                // calculate write address
                dup 1 push {safety_offset} add // _ minimum supremum list index-1 list_start_address
                dup 1 add // _ minimum supremum list index-1 element_address

                // calculate write value
                dup 4 dup 2 add // _ minimum supremum list index-1 element_address minimum+index-1

                // write
                write_mem // _ minimum supremum list index-1 element_address

                // clean up stack
                pop swap 1 // _ minimum supremum index-1 list

                recurse
            "
        )
    }

    fn crash_conditions(&self) -> Vec<String>
    where
        Self: Sized,
    {
        vec![
            "minimum not u32".to_string(),
            "supremum not u32".to_string(),
            "minimum larger than supremum".to_string(),
        ]
    }

    fn gen_input_states(&self) -> Vec<ExecutionState>
    where
        Self: Sized,
    {
        vec![
            Self::init_state(0, 1),
            Self::init_state(0, 10),
            Self::init_state(5, 15),
            Self::init_state(12, 12), // should generate empty list
        ]
    }

    fn rust_shadowing(
        &self,
        stack: &mut Vec<BFieldElement>,
        _std_in: Vec<BFieldElement>,
        _secret_in: Vec<BFieldElement>,
        memory: &mut std::collections::HashMap<BFieldElement, BFieldElement>,
    ) where
        Self: Sized,
    {
        let supremum: u32 = stack.pop().unwrap().value().try_into().unwrap();
        let minimum: u32 = stack.pop().unwrap().value().try_into().unwrap();
        let num_elements: usize = (supremum - minimum).try_into().unwrap();

        let safety_offset: usize = match self.list_type {
            ListType::Safe => 2,
            ListType::Unsafe => 1,
        };
        let length = num_elements;
        let capacity = num_elements;
        let memory_footprint = num_elements + safety_offset;

        // allocate space
        let list_pointer = rust_shadowing_helper_functions::dyn_malloc::dynamic_allocator(
            memory_footprint,
            memory,
        );

        // initialize list
        match self.list_type {
            ListType::Safe => {
                rust_shadowing_helper_functions::safe_list::safe_list_new(
                    list_pointer,
                    capacity.try_into().unwrap(),
                    memory,
                );
                rust_shadowing_helper_functions::safe_list::safe_list_set_length(
                    list_pointer,
                    length,
                    memory,
                );
            }
            ListType::Unsafe => {
                rust_shadowing_helper_functions::unsafe_list::unsafe_list_new(list_pointer, memory);
                rust_shadowing_helper_functions::unsafe_list::unsafe_list_set_length(
                    list_pointer,
                    length.try_into().unwrap(),
                    memory,
                );
            }
        }

        // write elements
        for i in minimum..supremum {
            memory.insert(
                list_pointer + BFieldElement::new(safety_offset as u64 + i as u64 - minimum as u64),
                BFieldElement::new(i as u64),
            );
        }

        // leave list address on stack
        stack.push(list_pointer);
    }

    fn common_case_input_state(&self) -> ExecutionState
    where
        Self: Sized,
    {
        Self::init_state(0, 45)
    }

    fn worst_case_input_state(&self) -> ExecutionState
    where
        Self: Sized,
    {
        Self::init_state(0, 250)
    }
}

#[cfg(test)]
mod tests {

    use crate::{execute_with_execution_state, test_helpers::test_rust_equivalence_multiple};

    use super::*;

    #[test]
    fn new_snippet_test_safe_lists() {
        test_rust_equivalence_multiple(
            &Range {
                list_type: ListType::Safe,
            },
            true,
        );
    }

    #[test]
    fn new_snippet_test_unsafe_lists() {
        test_rust_equivalence_multiple(
            &Range {
                list_type: ListType::Unsafe,
            },
            true,
        );
    }

    #[test]
    fn bad_range_safe_test() {
        let init_state = Range::init_state(10, 5);
        let snippet = Range {
            list_type: ListType::Safe,
        };
        let res = execute_with_execution_state(
            init_state,
            Box::new(snippet.clone()),
            snippet.stack_diff(),
        );
        assert!(res.is_err());
    }

    #[test]
    fn bad_range_unsafe_test() {
        let init_state = Range::init_state(13, 12);
        let snippet = Range {
            list_type: ListType::Unsafe,
        };
        let res = execute_with_execution_state(
            init_state,
            Box::new(snippet.clone()),
            snippet.stack_diff(),
        );
        assert!(res.is_err());
    }
}

#[cfg(test)]
mod benches {
    use super::*;
    use crate::snippet_bencher::bench_and_write;

    #[test]
    fn safe_benchmark() {
        bench_and_write(Range {
            list_type: ListType::Safe,
        });
    }

    #[test]
    fn unsafe_benchmark() {
        bench_and_write(Range {
            list_type: ListType::Unsafe,
        });
    }
}
