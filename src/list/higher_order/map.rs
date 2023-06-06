use num::Zero;
use rand::{thread_rng, Rng};
use std::collections::HashMap;
use twenty_first::shared_math::other::random_elements;
use twenty_first::{
    shared_math::b_field_element::BFieldElement, util_types::algebraic_hasher::AlgebraicHasher,
};

use crate::list::safe_u32::get::SafeGet;
use crate::list::safe_u32::length::SafeLength;
use crate::list::safe_u32::new::SafeNew;
use crate::list::safe_u32::set::SafeSet;
use crate::list::safe_u32::set_length::SafeSetLength;
use crate::list::unsafe_u32::get::UnsafeGet;
use crate::list::unsafe_u32::length::UnsafeLength;
use crate::list::unsafe_u32::new::UnsafeNew;
use crate::list::unsafe_u32::set::UnsafeSet;
use crate::list::unsafe_u32::set_length::UnsafeSetLength;
use crate::list::{self, ListType};
use crate::rust_shadowing_helper_functions::safe_list::safe_insert_random_list;
use crate::rust_shadowing_helper_functions::unsafe_list::unsafe_insert_random_list;
use crate::{get_init_tvm_stack, rust_shadowing_helper_functions};
use crate::{
    snippet::{DataType, Snippet},
    snippet_state::SnippetState,
    ExecutionState, VmHasher,
};

/// Applies a given function to every element of a list, and collects
/// the new elements into a new list. The function must be given as
/// an object as well as a dynamic type parameter.
#[derive(Debug, Clone)]
pub struct Map<Function: Snippet + 'static + Clone> {
    pub list_type: ListType,
    pub f: Function,
}

impl<Function: Snippet + 'static + Clone> Map<Function> {
    fn generate_input_state(
        &self,
        list_pointer: BFieldElement,
        list_length: usize,
    ) -> ExecutionState {
        let capacity = list_length;
        let mut stack = get_init_tvm_stack();
        stack.push(list_pointer);

        let mut memory = HashMap::default();
        memory.insert(
            BFieldElement::zero(),
            match self.list_type {
                ListType::Safe => BFieldElement::new(
                    (1 + 2 + list_length * self.f.input_types()[0].get_size()) as u64,
                ),
                ListType::Unsafe => BFieldElement::new(
                    (1 + 1 + list_length * self.f.input_types()[0].get_size()) as u64,
                ),
            },
        );
        let input_type = self.f.input_types()[0].clone();

        match self.list_type {
            ListType::Safe => safe_insert_random_list(
                &input_type,
                list_pointer,
                capacity as u32,
                list_length,
                &mut memory,
            ),
            ListType::Unsafe => unsafe_insert_random_list(
                list_pointer,
                list_length,
                &mut memory,
                input_type.get_size(),
            ),
        };

        ExecutionState {
            stack,
            std_in: vec![],
            secret_in: vec![],
            memory,
            words_allocated: 0,
        }
    }
}

impl<Function: Snippet + 'static + Clone> Snippet for Map<Function> {
    fn entrypoint(&self) -> String {
        format!(
            "tasm_list_higher_order_{}_u32_map_{}",
            self.list_type,
            self.f.entrypoint()
        )
    }

    fn inputs(&self) -> Vec<String>
    where
        Self: Sized,
    {
        vec!["input_list".to_string()]
    }

    fn input_types(&self) -> Vec<crate::snippet::DataType> {
        vec![DataType::List(Box::new(
            self.f.input_types().first().unwrap().clone(),
        ))]
    }

    fn output_types(&self) -> Vec<crate::snippet::DataType> {
        vec![DataType::List(Box::new(
            self.f.output_types().first().unwrap().clone(),
        ))]
    }

    fn outputs(&self) -> Vec<String>
    where
        Self: Sized,
    {
        vec!["output_list".to_string()]
    }

    fn stack_diff(&self) -> isize
    where
        Self: Sized,
    {
        0
    }

    fn function_body(&self, library: &mut SnippetState) -> String {
        let input_type = self.f.input_types()[0].clone();
        let output_type = self.f.output_types()[0].clone();
        let output_size_plus_one = 1 + output_type.get_size();
        let get_length = match self.list_type {
            ListType::Safe => library.import(Box::new(SafeLength(input_type.clone()))),
            ListType::Unsafe => library.import(Box::new(UnsafeLength(input_type.clone()))),
        };
        let set_length = match self.list_type {
            ListType::Safe => library.import(Box::new(SafeSetLength(input_type.clone()))),
            ListType::Unsafe => library.import(Box::new(UnsafeSetLength(input_type.clone()))),
        };
        let new_list = match self.list_type {
            ListType::Safe => library.import(Box::new(SafeNew(output_type.clone()))),
            ListType::Unsafe => library.import(Box::new(UnsafeNew(output_type.clone()))),
        };
        let list_get = match self.list_type {
            ListType::Safe => library.import(Box::new(SafeGet(input_type))),
            ListType::Unsafe => library.import(Box::new(UnsafeGet(input_type))),
        };
        let list_set = match self.list_type {
            ListType::Safe => library.import(Box::new(SafeSet(output_type))),
            ListType::Unsafe => library.import(Box::new(UnsafeSet(output_type))),
        };
        let function = library.import(Box::new(self.f.clone()));
        let entrypoint = self.entrypoint();

        format!(
            "
            // BEFORE: _ input_list
            // AFTER: _ output_list
            {entrypoint}:
                dup 0 // _ input_list input_list
                call {get_length} // _ input_list len
                dup 0 // _ input_list len len
                call {new_list} // _ input_list len output_list
                dup 1 //  _input_list len output_list len
                call {set_length} // _input_list len output_list
                swap 1 // _ input_list output_list len

                call {entrypoint}_loop // _ input_list output_list 0

                pop // _ input_list output_list
                swap 1 // _ output_list input_list
                pop // _ output_list
                return

            // INVARIANT: _ input_list output_list itr
            {entrypoint}_loop:
                // test return condition
                dup 0 // _ input_list output_list itr itr
                push 0 eq // _ input_list output_list itr itr!=0

                skiz return
                // _ input_list output_list itr

                // body
                push -1 add // _input_list output_list index

                // read
                dup 2 dup 1 // _input_list output_list index _input_list index
                call {list_get} // _ input_list output_list index [input_element]

                // map
                call {function} // _ input_list output_list index [output_element]

                // write
                dup {output_size_plus_one} // _ input_list output_list index [output_element] output_list
                dup {output_size_plus_one} // _ input_list output_list index [output_element] output_list index
                call {list_set} // _ input_list output_list index

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
        // Create random list of input data type
        let list_pointer = BFieldElement::new(1u64);
        let mut rng = thread_rng();
        let list_length: usize = rng.gen_range(1..=100);

        vec![self.generate_input_state(list_pointer, list_length)]
    }

    fn common_case_input_state(&self) -> ExecutionState
    where
        Self: Sized,
    {
        // Create random list of input data type
        let list_pointer = BFieldElement::new(1u64);
        let list_length: usize = 10;
        self.generate_input_state(list_pointer, list_length)
    }

    fn worst_case_input_state(&self) -> ExecutionState
    where
        Self: Sized,
    {
        // Create random list of input data type
        let list_pointer = BFieldElement::new(1u64);
        let list_length: usize = 400;
        self.generate_input_state(list_pointer, list_length)
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

        // set length
        stack.push(output_list);
        stack.push(BFieldElement::new(len as u64));
        match self.list_type {
            ListType::Safe => {
                list::safe_u32::set_length::SafeSetLength(output_type.clone()).rust_shadowing(
                    stack,
                    std_in.clone(),
                    secret_in.clone(),
                    memory,
                );
            }
            ListType::Unsafe => {
                list::unsafe_u32::set_length::UnsafeSetLength(output_type.clone()).rust_shadowing(
                    stack,
                    std_in.clone(),
                    secret_in.clone(),
                    memory,
                );
            }
        }
        stack.pop();

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

        stack.push(output_list);
    }
}

#[derive(Debug, Clone)]
struct TestHashXFieldElement;

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
            push 1 // _ x2 x1 x0 0 0 0 1
            push 0 swap 7 // _ 0 x1 x0 0 0 0 1 x2
            push 0 swap 7 // _ 0 0 x0 0 0 0 1 x2 x1
            push 0 swap 7 // _ 0 0 0 0 0 0 1 x2 x1 x0

            absorb_init
            squeeze // _ d9 d8 d7 d6 d5 d4 d3 d2 d1 d0
            swap 5 pop // _ d9 d8 d7 d6 d0 d4 d3 d2 d1
            swap 5 pop // _ d9 d8 d7 d1 d0 d4 d3 d2
            swap 5 pop
            swap 5 pop
            swap 5 pop
            return
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
            f: TestHashXFieldElement,
        });
    }

    #[test]
    fn with_safe_list_prop_test() {
        rust_tasm_equivalence_prop_new(Map::<TestHashXFieldElement> {
            list_type: ListType::Safe,
            f: TestHashXFieldElement,
        });
    }

    #[test]
    fn unsafe_list_map_benchmark() {
        bench_and_write(Map::<TestHashXFieldElement> {
            list_type: ListType::Unsafe,
            f: TestHashXFieldElement,
        });
    }

    #[test]
    fn safe_list_map_benchmark() {
        bench_and_write(Map::<TestHashXFieldElement> {
            list_type: ListType::Safe,
            f: TestHashXFieldElement,
        });
    }
}
