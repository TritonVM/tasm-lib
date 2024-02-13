use std::collections::HashMap;

use itertools::Itertools;
use triton_vm::prelude::*;

use tasm_lib::list::LIST_METADATA_SIZE;

use crate::data_type::DataType;
use crate::empty_stack;
use crate::list::new::New;
use crate::list::set_length::SetLength;
use crate::rust_shadowing_helper_functions;
use crate::traits::deprecated_snippet::DeprecatedSnippet;
use crate::ExecutionState;

/// Generates a list containing all integers between the minimum (inclusive lower bound) and the
/// supremum (exclusive upper bound).
#[derive(Debug, Default, Copy, Clone, Eq, PartialEq)]
pub struct Range;

impl Range {
    fn init_state(minimum: u32, supremum: u32) -> ExecutionState {
        let mut stack = empty_stack();
        stack.push(BFieldElement::new(minimum as u64));
        stack.push(BFieldElement::new(supremum as u64));
        ExecutionState {
            stack,
            std_in: vec![],
            nondeterminism: NonDeterminism::new(vec![]),
            words_allocated: 1,
        }
    }
}

impl DeprecatedSnippet for Range {
    fn entrypoint_name(&self) -> String {
        "tasm_list_range".into()
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

    fn output_field_names(&self) -> Vec<String>
    where
        Self: Sized,
    {
        vec!["*list".to_string()]
    }

    fn output_types(&self) -> Vec<DataType> {
        vec![DataType::List(Box::new(DataType::U32))]
    }

    fn stack_diff(&self) -> isize
    where
        Self: Sized,
    {
        -1
    }

    fn function_code(&self, library: &mut crate::library::Library) -> String {
        let data_type = DataType::U32;

        let new_list = library.import(Box::new(New::new(data_type.clone())));
        let set_length = library.import(Box::new(SetLength::new(data_type)));

        let entrypoint = self.entrypoint_name();
        let inner_loop = format!("{entrypoint}_loop");

        triton_asm!(
            // BEFORE: _ minimum supremum
            // AFTER:  _ *list
            {entrypoint}:
                hint supremum = stack[0]
                hint minimum = stack[1]
                dup 0 push 1 add dup 2  // _ minimum supremum (supremum + 1) minimum
                lt                      // _ minimum supremum (minimum <= supremum)
                assert

                // calculate length
                dup 0 dup 2             // _ minimum supremum supremum minimum
                push -1 mul add         // _ minimum supremum (supremum - minimum)
                                        // _ minimum supremum length

                // create list object
                dup 0                   // _ minimum supremum length length
                call {new_list}         // _ minimum supremum length *list
                dup 1                   // _ minimum supremum length *list length
                call {set_length}       // _ minimum supremum length *list
                call {inner_loop}       // _ minimum supremum 0 *list

                // clean up stack
                swap 3 pop 3            // _ *list
                return

            // BEFORE:    _ minimum supremum length *list
            // INVARIANT: _ minimum supremum (length - i) *list
            // AFTER:     _ minimum supremum 0 *list
            {inner_loop}:
                // compute termination condition
                dup 1 push 0 eq         // _ minimum supremum index *list (index == 0)
                skiz return

                // decrement index
                swap 1 push -1 add      // _ minimum supremum *list (index - 1)

                // value to write
                dup 3 dup 1 add         // _ minimum supremum *list (index - 1) (minimum + index - 1)

                // address to write to
                dup 2                   // _ minimum supremum *list (index - 1) (minimum + index - 1) *list
                push {LIST_METADATA_SIZE}
                hint list_metadata_size = stack[0]
                add                     // _ minimum supremum *list (index - 1) (minimum + index - 1) *list_start
                dup 2 add               // _ minimum supremum *list (index - 1) (minimum + index - 1) *element

                write_mem 1             // _ minimum supremum *list (index - 1) (*element + 1)
                pop 1 swap 1            // _ minimum supremum (index - 1) *list
                recurse
        )
        .iter()
        .join("\n")
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

    fn rust_shadowing(
        &self,
        stack: &mut Vec<BFieldElement>,
        _std_in: Vec<BFieldElement>,
        _secret_in: Vec<BFieldElement>,
        memory: &mut HashMap<BFieldElement, BFieldElement>,
    ) where
        Self: Sized,
    {
        let supremum: u32 = stack.pop().unwrap().value().try_into().unwrap();
        let minimum: u32 = stack.pop().unwrap().value().try_into().unwrap();
        let num_elements: usize = (supremum - minimum).try_into().unwrap();

        let safety_offset = LIST_METADATA_SIZE;
        let length = num_elements;
        let memory_footprint = num_elements + safety_offset;

        // allocate space
        let list_pointer = rust_shadowing_helper_functions::dyn_malloc::dynamic_allocator(
            memory_footprint,
            memory,
        );

        // initialize list
        rust_shadowing_helper_functions::list::list_new(list_pointer, memory);
        rust_shadowing_helper_functions::list::list_set_length(list_pointer, length, memory);

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
}

#[cfg(test)]
mod tests {
    use triton_vm::error::InstructionError;
    use triton_vm::error::VMError;

    use crate::execute_with_execution_state_deprecated;
    use crate::test_helpers::test_rust_equivalence_multiple_deprecated;

    use super::*;

    #[test]
    fn new_snippet_test() {
        test_rust_equivalence_multiple_deprecated(&Range, true);
    }
    #[test]
    fn bad_range_test() {
        let init_state = Range::init_state(13, 12);
        let snippet = Range;
        let stack_diff = snippet.stack_diff();
        let execution_result =
            execute_with_execution_state_deprecated(snippet, init_state, stack_diff);
        let err = execution_result.unwrap_err();
        let err = err.downcast::<VMError>().unwrap();
        assert_eq!(InstructionError::AssertionFailed, err.source);
    }
}

#[cfg(test)]
mod benches {
    use crate::snippet_bencher::bench_and_write;

    use super::*;

    #[test]
    fn benchmark() {
        bench_and_write(Range);
    }
}
