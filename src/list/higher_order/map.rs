use num::Zero;
use std::collections::HashMap;
use twenty_first::shared_math::other::random_elements;
use twenty_first::{
    shared_math::b_field_element::BFieldElement, util_types::algebraic_hasher::AlgebraicHasher,
};

use crate::list::safe_u32::length::SafeLength;
use crate::list::unsafe_u32::length::UnsafeLength;
use crate::list::{self, ListType};
use crate::rust_shadowing_helper_functions;
use crate::{
    snippet::{DataType, Snippet},
    snippet_state::SnippetState,
    ExecutionState, VmHasher,
};

/// Applies a given function to every element of a list, and collects
/// the new elements into a new list. The function must be given as a
/// compile-time argument.
#[derive(Debug, Clone)]
pub struct Map<Function: Snippet> {
    pub list_type: ListType,
    pub f: Function,
}

impl<Function: Snippet> Snippet for Map<Function> {
    fn entrypoint(&self) -> String {
        format!("tasm_list_higher_order_{}_u32_map", self.list_type)
    }

    fn inputs(&self) -> Vec<String>
    where
        Self: Sized,
    {
        vec!["list_a".to_string(), "list_b".to_string()]
    }

    fn input_types(&self) -> Vec<crate::snippet::DataType> {
        vec![
            DataType::List(Box::new(DataType::Digest)),
            DataType::List(Box::new(DataType::Digest)),
        ]
    }

    fn output_types(&self) -> Vec<crate::snippet::DataType> {
        vec![DataType::Bool]
    }

    fn outputs(&self) -> Vec<String>
    where
        Self: Sized,
    {
        vec!["multisets_are_equal".to_string()]
    }

    fn stack_diff(&self) -> isize
    where
        Self: Sized,
    {
        -1
    }

    fn function_body(&self, library: &mut SnippetState) -> String {
        let _list_length = match self.list_type {
            ListType::Safe => library.import(Box::new(SafeLength(DataType::Digest))),
            ListType::Unsafe => library.import(Box::new(UnsafeLength(DataType::Digest))),
        };
        let _first_element_offset = match self.list_type {
            ListType::Safe => 2,
            ListType::Unsafe => 1,
        };
        let entrypoint = self.entrypoint();

        format!(
            "
            {entrypoint}:

                recurse
            "
        )
    }

    fn crash_conditions() -> Vec<String>
    where
        Self: Sized,
    {
        vec![
            "Map function does not take 1 element.".to_string(),
            "Map function does not yield 1 element.".to_string(),
            "Length exceeds u32::MAX".to_string(),
        ]
    }

    fn gen_input_states(&self) -> Vec<ExecutionState>
    where
        Self: Sized,
    {
        vec![]
    }

    fn common_case_input_state(&self) -> ExecutionState
    where
        Self: Sized,
    {
        todo!()
    }

    fn worst_case_input_state(&self) -> ExecutionState
    where
        Self: Sized,
    {
        todo!()
    }

    fn rust_shadowing(
        &self,
        stack: &mut Vec<triton_vm::BFieldElement>,
        std_in: Vec<triton_vm::BFieldElement>,
        secret_in: Vec<triton_vm::BFieldElement>,
        memory: &mut std::collections::HashMap<triton_vm::BFieldElement, triton_vm::BFieldElement>,
    ) where
        Self: Sized,
    {
        let input_types = self.f.input_types();
        let output_types = self.f.output_types();
        assert_eq!(input_types.len(), 1);
        assert_eq!(output_types.len(), 1);
        let input_type = input_types.first().unwrap().to_owned();
        let output_type = output_types.first().unwrap().to_owned();
        let list_pointer = stack.pop().unwrap();

        // get list length
        let len = match self.list_type {
            ListType::Safe => rust_shadowing_helper_functions::safe_list::safe_list_get_length(
                list_pointer,
                memory,
            ),
            ListType::Unsafe => {
                rust_shadowing_helper_functions::unsafe_list::unsafe_list_get_length(
                    list_pointer,
                    memory,
                )
            }
        };

        let list_element = match self.list_type {
            ListType::Safe => rust_shadowing_helper_functions::safe_list::safe_list_read,
            ListType::Unsafe => rust_shadowing_helper_functions::unsafe_list::unsafe_list_read,
        };

        let set_element = match self.list_type {
            ListType::Safe => rust_shadowing_helper_functions::safe_list::safe_list_set,
            ListType::Unsafe => rust_shadowing_helper_functions::unsafe_list::unsafe_list_set,
        };

        let output_list_capacity = len;
        let output_list = match self.list_type {
            ListType::Safe => {
                // Push capacity to stack
                stack.push(BFieldElement::new(output_list_capacity as u64));
                list::safe_u32::new::SafeNew(input_type.clone()).rust_shadowing(
                    stack,
                    std_in.clone(),
                    secret_in.clone(),
                    memory,
                );
                stack.pop().unwrap()
            }
            ListType::Unsafe => {
                stack.push(BFieldElement::new(output_list_capacity as u64));
                list::unsafe_u32::new::UnsafeNew(input_type.clone()).rust_shadowing(
                    stack,
                    std_in.clone(),
                    secret_in.clone(),
                    memory,
                );
                stack.pop().unwrap()
            }
        };

        // forall elements, read + map + write
        for i in 0..len {
            // read
            let mut input_item = list_element(list_pointer, i, memory, input_type.get_size());

            // put on stack
            while !input_item.is_empty() {
                stack.push(input_item.pop().unwrap());
            }

            // map
            self.f
                .rust_shadowing(stack, std_in.clone(), secret_in.clone(), memory);

            // pull from stack
            let mut output_item = vec![];
            for _ in 0..output_type.get_size() {
                output_item.push(stack.pop().unwrap());
            }

            // write
            set_element(output_list, i, output_item, memory, output_type.get_size());
        }
    }
}

#[derive(Debug, Clone)]
struct TestHashXFieldElement {}

impl Snippet for TestHashXFieldElement {
    fn entrypoint(&self) -> String {
        "test_hash_xfield_element".to_string()
    }

    fn inputs(&self) -> Vec<String>
    where
        Self: Sized,
    {
        vec![
            "elem2".to_string(),
            "elem1".to_string(),
            "elem0".to_string(),
        ]
    }

    fn input_types(&self) -> Vec<DataType> {
        vec![DataType::XFE]
    }

    fn output_types(&self) -> Vec<DataType> {
        vec![DataType::Digest]
    }

    fn outputs(&self) -> Vec<String>
    where
        Self: Sized,
    {
        vec![
            "digelem4".to_string(),
            "digelem3".to_string(),
            "digelem2".to_string(),
            "digelem1".to_string(),
            "digelem0".to_string(),
        ]
    }

    fn stack_diff(&self) -> isize
    where
        Self: Sized,
    {
        2
    }

    fn function_body(&self, _library: &mut SnippetState) -> String {
        let entrypoint = self.entrypoint();
        format!(
            "
        // BEFORE: _ x2 x1 x0
        // AFTER: _ d4 d3 d2 d1 d0
        {entrypoint}:
            push 0
            push 0
            push 0
            push 0
            push 0
            push 1 // _ x2 x1 x0 0 0 0 0 0 1
            push 0 swap 9 // _ 0 x1 x0 0 0 0 0 0 1 x2
            push 0 swap 9 // _ 0 0 x0 0 0 0 0 0 1 x2 x1
            push 0 swap 9 // _ 0 0 0 0 0 0 0 0 1 x2 x1 x0
            absorb_init
            squeeze // _ d9 d8 d7 d6 d5 d4 d3 d2 d1 d0
            swap 5 pop // _ d9 d8 d7 d6 d0 d4 d3 d2 d1
            swap 5 pop // _ d9 d8 d7 d1 d0 d4 d3 d2
            swap 5 pop
            swap 5 pop
            swap 5 pop
        "
        )
    }

    fn crash_conditions() -> Vec<String>
    where
        Self: Sized,
    {
        vec![]
    }

    fn gen_input_states(&self) -> Vec<ExecutionState>
    where
        Self: Sized,
    {
        vec![ExecutionState::with_stack(
            vec![
                vec![BFieldElement::zero(); 16],
                random_elements::<BFieldElement>(3),
            ]
            .concat(),
        )]
    }

    fn common_case_input_state(&self) -> ExecutionState
    where
        Self: Sized,
    {
        ExecutionState::with_stack(
            vec![
                vec![BFieldElement::zero(); 16],
                random_elements::<BFieldElement>(3),
            ]
            .concat(),
        )
    }

    fn worst_case_input_state(&self) -> ExecutionState
    where
        Self: Sized,
    {
        ExecutionState::with_stack(
            vec![
                vec![BFieldElement::zero(); 16],
                random_elements::<BFieldElement>(3),
            ]
            .concat(),
        )
    }

    fn rust_shadowing(
        &self,
        stack: &mut Vec<BFieldElement>,
        _std_in: Vec<BFieldElement>,
        _secret_in: Vec<BFieldElement>,
        _memory: &mut HashMap<BFieldElement, BFieldElement>,
    ) where
        Self: Sized,
    {
        let mut xfield_element = vec![];
        for _ in 0..3 {
            xfield_element.push(stack.pop().unwrap());
        }
        let mut digest = VmHasher::hash_varlen(&xfield_element).values().to_vec();
        while !digest.is_empty() {
            stack.push(digest.pop().unwrap());
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{snippet_bencher::bench_and_write, test_helpers::rust_tasm_equivalence_prop_new};

    use super::*;

    #[test]
    fn unsafe_list_prop_test() {
        rust_tasm_equivalence_prop_new(Map::<TestHashXFieldElement> {
            list_type: ListType::Unsafe,
            f: TestHashXFieldElement {},
        });
    }

    #[test]
    fn with_safe_list_prop_test() {
        rust_tasm_equivalence_prop_new(Map::<TestHashXFieldElement> {
            list_type: ListType::Unsafe,
            f: TestHashXFieldElement {},
        });
    }

    #[test]
    fn unsafe_list_multiset_eq_benchmark() {
        bench_and_write(Map::<TestHashXFieldElement> {
            list_type: ListType::Unsafe,
            f: TestHashXFieldElement {},
        });
    }

    #[test]
    fn safe_list_multiset_eq_benchmark() {
        bench_and_write(Map::<TestHashXFieldElement> {
            list_type: ListType::Unsafe,
            f: TestHashXFieldElement {},
        });
    }
}
