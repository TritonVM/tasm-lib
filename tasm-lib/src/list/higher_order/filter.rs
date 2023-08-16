use itertools::Itertools;
use num::Zero;
use rand::{thread_rng, Rng};
use std::collections::HashMap;
use triton_vm::parser::tokenize;
use triton_vm::NonDeterminism;
use twenty_first::shared_math::b_field_element::BFieldElement;
use twenty_first::shared_math::other::random_elements;
use twenty_first::util_types::algebraic_hasher::AlgebraicHasher;

use crate::list::safe_u32::get::SafeGet;
use crate::list::safe_u32::length::SafeLength;
use crate::list::safe_u32::new::SafeNew;
use crate::list::safe_u32::set_length::SafeSetLength;
use crate::list::unsafe_u32::get::UnsafeGet;
use crate::list::unsafe_u32::length::UnsafeLength;
use crate::list::unsafe_u32::new::UnsafeNew;
use crate::list::unsafe_u32::set_length::UnsafeSetLength;
use crate::list::{self, ListType};
use crate::memory::memcpy::MemCpy;
use crate::rust_shadowing_helper_functions::safe_list::safe_insert_random_list;
use crate::rust_shadowing_helper_functions::unsafe_list::untyped_unsafe_insert_random_list;
use crate::{arithmetic, get_init_tvm_stack, rust_shadowing_helper_functions, VmHasher};
use crate::{
    library::Library,
    snippet::{DataType, DeprecatedSnippet},
    ExecutionState,
};

use super::inner_function::InnerFunction;

/// Filters a given list for elements that satisfy a predicate. A new
/// list is created, containing only those elements that satisfy the
/// predicate. The predicate must be given as an InnerFunction, which
/// is either another Snippet or a RawCode object.
pub struct Filter {
    pub list_type: ListType,
    pub f: InnerFunction,
}

impl Filter {
    fn generate_input_state(
        &self,
        list_pointer: BFieldElement,
        list_length: usize,
        random: bool,
    ) -> ExecutionState {
        let capacity = list_length;
        let mut stack = get_init_tvm_stack();
        stack.push(list_pointer);

        let mut memory = HashMap::default();
        let input_type = match self.f.get_input_types().len() {
            1 => self.f.get_input_types()[0].clone(),
            _ => panic!("Can only be used with functions taking one argument"),
        };
        memory.insert(
            BFieldElement::zero(),
            match self.list_type {
                ListType::Safe => {
                    list_pointer
                        + BFieldElement::new((2 + list_length * input_type.get_size()) as u64)
                }
                ListType::Unsafe => {
                    list_pointer
                        + BFieldElement::new((1 + list_length * input_type.get_size()) as u64)
                }
            },
        );

        if random {
            match self.list_type {
                ListType::Safe => safe_insert_random_list(
                    &input_type,
                    list_pointer,
                    capacity as u32,
                    list_length,
                    &mut memory,
                ),
                ListType::Unsafe => untyped_unsafe_insert_random_list(
                    list_pointer,
                    list_length,
                    &mut memory,
                    input_type.get_size(),
                ),
            };
        } else {
            match self.list_type {
                ListType::Safe => rust_shadowing_helper_functions::safe_list::safe_list_insert(
                    list_pointer,
                    capacity as u32,
                    (0..list_length as u64)
                        .map(BFieldElement::new)
                        .collect_vec(),
                    &mut memory,
                ),
                ListType::Unsafe => {
                    rust_shadowing_helper_functions::unsafe_list::unsafe_list_insert(
                        list_pointer,
                        (0..list_length as u64)
                            .map(BFieldElement::new)
                            .collect_vec(),
                        &mut memory,
                    )
                }
            };
        }

        ExecutionState {
            stack,
            std_in: vec![],
            nondeterminism: NonDeterminism::new(vec![]),
            memory,
            words_allocated: 0,
        }
    }
}

impl DeprecatedSnippet for Filter {
    fn entrypoint_name(&self) -> String {
        format!(
            "tasm_list_higher_order_{}_u32_filter_{}",
            self.list_type,
            self.f.entrypoint()
        )
    }

    fn input_field_names(&self) -> Vec<String>
    where
        Self: Sized,
    {
        vec!["input_list".to_string()]
    }

    fn input_types(&self) -> Vec<crate::snippet::DataType> {
        vec![DataType::List(Box::new(
            self.f.get_input_types()[0].clone(),
        ))]
    }

    fn output_types(&self) -> Vec<crate::snippet::DataType> {
        vec![DataType::List(Box::new(
            self.f.get_input_types()[0].clone(),
        ))]
    }

    fn output_field_names(&self) -> Vec<String>
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

    fn function_code(&self, library: &mut Library) -> String {
        let input_type = match self.f.get_input_types().len() {
            1 => self.f.get_input_types()[0].clone(),
            _ => panic!("Can only filter with functions with one input"),
        };
        let output_type = match self.f.get_output_types().len() {
            1 => self.f.get_output_types()[0].clone(),
            _ => panic!("Can only filter with functions returning a bool"),
        };
        assert_eq!(output_type, DataType::Bool);
        let safety_offset = match self.list_type {
            ListType::Safe => 2,
            ListType::Unsafe => 1,
        };
        let get_length = match self.list_type {
            ListType::Safe => library.import(Box::new(SafeLength(input_type.clone()))),
            ListType::Unsafe => library.import(Box::new(UnsafeLength(input_type.clone()))),
        };
        let set_length = match self.list_type {
            ListType::Safe => library.import(Box::new(SafeSetLength(input_type.clone()))),
            ListType::Unsafe => library.import(Box::new(UnsafeSetLength(input_type.clone()))),
        };
        let new_list = match self.list_type {
            ListType::Safe => library.import(Box::new(SafeNew(output_type))),
            ListType::Unsafe => library.import(Box::new(UnsafeNew(output_type))),
        };
        let list_get = match self.list_type {
            ListType::Safe => library.import(Box::new(SafeGet(input_type))),
            ListType::Unsafe => library.import(Box::new(UnsafeGet(input_type))),
        };
        let element_size = self.f.get_input_types()[0].get_size();

        let inner_function_name = match &self.f {
            InnerFunction::RawCode(rc) => rc.entrypoint(),
            InnerFunction::Snippet(sn) => {
                let fn_body = sn.function_code(library);
                let (_, instructions) = tokenize(&fn_body).unwrap();
                let labelled_instructions =
                    triton_vm::parser::to_labelled_instructions(&instructions);
                library.explicit_import(&sn.entrypoint_name(), &labelled_instructions)
            }
            InnerFunction::NoFunctionBody(_) => todo!(),
        };

        let memcpy = library.import(Box::new(MemCpy));

        // If function was supplied as raw instructions, we need to append the inner function to the function
        // body. Otherwise, `library` handles the imports.
        let maybe_inner_function_body_raw = match &self.f {
            InnerFunction::RawCode(rc) => rc.function.iter().map(|x| x.to_string()).join("\n"),
            InnerFunction::Snippet(_) => String::default(),
            InnerFunction::NoFunctionBody(_) => todo!(),
        };
        let entrypoint = self.entrypoint_name();

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
                swap 1 // _ input_list output_list input_len

                push 0 push 0 // _ input_list output_list input_len 0 0
                call {entrypoint}_loop // _ input_list output_list input_len input_len output_len

                swap 2 pop pop // _ input_list output_list output_len
                call {set_length} // _input_list output_list

                swap 1 // _ output_list input_list
                pop // _ output_list
                return

            // INVARIANT:  _ input_list output_list input_len input_index output_index
            {entrypoint}_loop:
                // test return condition
                dup 1 // _ input_list output_list input_len input_index output_index input_index
                dup 3 eq // _ input_list output_list input_len input_index output_index input_index==input_len

                skiz return
                // _ input_list output_list input_len input_index output_index 

                // body

                // read
                dup 4 // _ input_list output_list input_len input_index output_index input_list
                dup 2 // _ input_list output_list input_len input_index output_index input_list input_index
                call {list_get} // _ input_list output_list input_len input_index output_index [input_elements]

                // map
                call {inner_function_name} // _ input_list output_list input_len input_index output_index b

                // write
                skiz call {entrypoint}_write //_ input_list output_list input_len input_index output_index*

                // _ input_list output_list input_len input_index output_index*
                swap 1 push 1 add swap 1 // _ input_list output_list input_len input_index+1 output_index*
                recurse

            // BEFORE: _ input_list output_list input_len input_index output_index
            // AFTER: _ input_list output_list input_len input_index output_index+1
            {entrypoint}_write:
                // calculate read address
                dup 4 // _ input_list output_list input_len input_index output_index input_list
                push {safety_offset} add // _ input_list output_list input_len input_index output_index address
                dup 2 // _ input_list output_list input_len input_index output_index address input_index
                push {element_size} mul add // _ input_list output_list input_len input_index output_index read_source

                // calculate write address
                dup 4 // _ input_list output_list input_len input_index output_index read_source output_list
                push {safety_offset} add // _ input_list output_list input_len input_index output_index read_source address
                dup 2 //  _ input_list output_list input_len input_index output_index read_source address output_index
                push {element_size} mul add // _ input_list output_list input_len input_index output_index read_source write_dest

                // calculate number of words
                push {element_size} // _ input_list output_list input_len input_index output_index read_source write_dest num_words

                // copy memory
                call {memcpy} // _ input_list output_list input_len input_index output_index

                // bookkeeping
                push 1 add // _ input_list output_list input_len input_index output_index+1

                return

            {maybe_inner_function_body_raw}
            "
        )
    }

    fn crash_conditions(&self) -> Vec<String>
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
        let mut rng = thread_rng();
        let mut ret = vec![];
        for _ in 0..10 {
            let list_pointer = BFieldElement::new(rng.gen_range(1u64..=1000));
            let list_length: usize = rng.gen_range(1..=100);
            ret.push(self.generate_input_state(list_pointer, list_length, true))
        }

        ret
    }

    fn common_case_input_state(&self) -> ExecutionState
    where
        Self: Sized,
    {
        // Create random list of input data type
        let list_pointer = BFieldElement::new(1u64);
        let list_length: usize = 10;
        self.generate_input_state(list_pointer, list_length, false)
    }

    fn worst_case_input_state(&self) -> ExecutionState
    where
        Self: Sized,
    {
        // Create random list of input data type
        let list_pointer = BFieldElement::new(1u64);
        let list_length: usize = 400;
        self.generate_input_state(list_pointer, list_length, false)
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
        let input_type = match self.f.get_input_types().len() {
            1 => self.f.get_input_types()[0].clone(),
            _ => panic!("Input length must be one when using function in map)"),
        };
        let output_type = match self.f.get_output_types().len() {
            1 => self.f.get_output_types()[0].clone(),
            _ => panic!("Input length must be one when using function in map)"),
        };

        let element_size = self.f.get_input_types()[0].get_size();
        let memcpy = MemCpy::rust_shadowing;
        let safety_offset = match self.list_type {
            ListType::Safe => 2,
            ListType::Unsafe => 1,
        };

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

        let get_element = match self.list_type {
            ListType::Safe => rust_shadowing_helper_functions::safe_list::safe_list_get,
            ListType::Unsafe => rust_shadowing_helper_functions::unsafe_list::unsafe_list_get,
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
                list::safe_u32::set_length::SafeSetLength(output_type).rust_shadowing(
                    stack,
                    std_in.clone(),
                    secret_in.clone(),
                    memory,
                );
            }
            ListType::Unsafe => {
                list::unsafe_u32::set_length::UnsafeSetLength(output_type).rust_shadowing(
                    stack,
                    std_in.clone(),
                    secret_in.clone(),
                    memory,
                );
            }
        }
        stack.pop();

        // forall elements, read + map + maybe copy
        let mut output_index = 0;
        for i in 0..len {
            // read
            let mut input_item = get_element(list_pointer, i, memory, input_type.get_size());

            // put on stack
            while !input_item.is_empty() {
                stack.push(input_item.pop().unwrap());
            }

            self.f
                .rust_shadowing(&std_in.clone(), &secret_in.clone(), stack, memory);
            let satisfied = stack.pop().unwrap().value() != 0;

            // maybe copy
            if satisfied {
                stack.push(
                    list_pointer
                        + BFieldElement::new(safety_offset as u64 + i as u64 * element_size as u64),
                ); // read source
                stack.push(
                    output_list
                        + BFieldElement::new(
                            safety_offset as u64 + output_index as u64 * element_size as u64,
                        ),
                ); // write dest
                stack.push(BFieldElement::new(element_size as u64)); // number of words
                memcpy(&MemCpy, stack, std_in.clone(), secret_in.clone(), memory);
                output_index += 1;
            }
        }

        // set length
        stack.push(output_list);
        stack.push(BFieldElement::new(output_index as u64));
        match self.list_type {
            ListType::Safe => {
                list::safe_u32::set_length::SafeSetLength(input_type)
                    .rust_shadowing(stack, std_in, secret_in, memory);
            }
            ListType::Unsafe => {
                list::unsafe_u32::set_length::UnsafeSetLength(input_type)
                    .rust_shadowing(stack, std_in, secret_in, memory);
            }
        }
    }
}

#[derive(Debug, Clone)]
struct TestHashXFieldElementLsb;

impl DeprecatedSnippet for TestHashXFieldElementLsb {
    fn entrypoint_name(&self) -> String {
        "test_hash_xfield_element_lsb".to_string()
    }

    fn input_field_names(&self) -> Vec<String>
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
        vec![DataType::Bool]
    }

    fn output_field_names(&self) -> Vec<String>
    where
        Self: Sized,
    {
        vec!["bool".to_string()]
    }

    fn stack_diff(&self) -> isize
    where
        Self: Sized,
    {
        -2
    }

    fn function_code(&self, library: &mut Library) -> String {
        let entrypoint = self.entrypoint_name();
        let unused_import = library.import(Box::new(arithmetic::u32::safe_add::SafeAdd));
        format!(
            "
    // BEFORE: _ x2 x1 x0
    // AFTER: _ b
    {entrypoint}:
        // Useless additions, to ensure that dependencies are accepted inside the filter generated code
            push 0
            push 0
            call {unused_import}
            pop

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

        // _ d4 d3 d2 d1 d0

        split // _ d4 d3 d2 d1 hi lo
        push 2 // _ d4 d3 d2 d1 hi lo 2
        swap 1
        div // _ d4 d3 d2 d1 hi q r
        swap 6
        pop pop pop pop pop pop
        return
    "
        )
    }

    fn crash_conditions(&self) -> Vec<String>
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
        let digest = VmHasher::hash_varlen(&xfield_element).values().to_vec();
        let b = digest[0].value() % 2;
        stack.push(BFieldElement::new(b));
    }
}

#[cfg(test)]
mod tests {

    use std::cell::RefCell;

    use triton_vm::triton_asm;

    use crate::{
        list::higher_order::inner_function::RawCode,
        test_helpers::test_rust_equivalence_multiple_deprecated,
    };

    use super::*;

    #[test]
    fn unsafe_list_prop_test() {
        test_rust_equivalence_multiple_deprecated(
            &Filter {
                list_type: ListType::Unsafe,
                f: InnerFunction::Snippet(Box::new(TestHashXFieldElementLsb)),
            },
            false,
        );
    }

    #[test]
    fn with_safe_list_prop_test() {
        test_rust_equivalence_multiple_deprecated(
            &Filter {
                list_type: ListType::Safe,
                f: InnerFunction::Snippet(Box::new(TestHashXFieldElementLsb)),
            },
            false,
        );
    }

    #[test]
    fn test_with_raw_function_lsb_on_bfe() {
        let rawcode = RawCode::new_with_shadowing(
            triton_asm!(
                lsb_bfe:
                    split   // _ hi lo
                    push 2  // _ hi lo 2
                    swap 1  // _ hi 2 lo
                    div     // _ hi q r
                    swap 2  // _ r q hi
                    pop     // _ r q
                    pop     // _ r
                    return
            ),
            vec![DataType::BFE],
            vec![DataType::Bool],
            Box::new(RefCell::new(|vec: &mut Vec<BFieldElement>| {
                let new_value = vec.pop().unwrap().value() % 2;
                vec.push(BFieldElement::new(new_value));
            })),
        );
        test_rust_equivalence_multiple_deprecated(
            &Filter {
                list_type: ListType::Unsafe,
                f: InnerFunction::RawCode(rawcode),
            },
            false,
        );
    }

    #[test]
    fn test_with_raw_function_lsb_on_xfe() {
        let rawcode = RawCode::new_with_shadowing(
            triton_asm!(
                lsb_xfe:
                    split   // _ x2 x1 hi lo
                    push 2  // _ x2 x1 hi lo 2
                    swap 1  // _ x2 x1 hi 2 lo
                    div     // _ x2 x1 hi q r
                    swap 4  // _ r x1 q hi x2
                    pop     // _ r x1 q hi
                    pop     // _ r x1 q
                    pop     // _ r q
                    pop     // _ r
                    return
            ),
            vec![DataType::XFE],
            vec![DataType::Bool],
            Box::new(RefCell::new(|vec: &mut Vec<BFieldElement>| {
                let new_value = vec.pop().unwrap().value() % 2;
                vec.pop();
                vec.pop();
                vec.push(BFieldElement::new(new_value));
            })),
        );
        test_rust_equivalence_multiple_deprecated(
            &Filter {
                list_type: ListType::Unsafe,
                f: InnerFunction::RawCode(rawcode),
            },
            false,
        );
    }
}

#[cfg(test)]
mod benches {
    use super::*;
    use crate::snippet_bencher::bench_and_write;

    #[test]
    fn unsafe_list_filter_benchmark() {
        bench_and_write(Filter {
            list_type: ListType::Unsafe,
            f: InnerFunction::Snippet(Box::new(TestHashXFieldElementLsb)),
        });
    }

    #[test]
    fn safe_list_filter_benchmark() {
        bench_and_write(Filter {
            list_type: ListType::Safe,
            f: InnerFunction::Snippet(Box::new(TestHashXFieldElementLsb)),
        });
    }
}
