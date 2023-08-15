use itertools::Itertools;
use num::Zero;
use rand::{thread_rng, Rng};
use std::collections::HashMap;
use triton_vm::NonDeterminism;
use twenty_first::shared_math::b_field_element::BFieldElement;
use twenty_first::util_types::algebraic_hasher::AlgebraicHasher;

use crate::list::safe_u32::get::SafeGet;
use crate::list::safe_u32::length::SafeLength;
use crate::list::unsafe_u32::get::UnsafeGet;
use crate::list::unsafe_u32::length::UnsafeLength;
use crate::list::ListType;
use crate::rust_shadowing_helper_functions::safe_list::safe_insert_random_list;
use crate::rust_shadowing_helper_functions::unsafe_list::untyped_unsafe_insert_random_list;
use crate::{get_init_tvm_stack, rust_shadowing_helper_functions, VmHasher};
use crate::{
    library::Library,
    snippet::{DataType, DeprecatedSnippet},
    ExecutionState,
};

use super::inner_function::InnerFunction;

/// Runs a predicate over all elements of a list and returns true
/// only if all elements satisfy the predicate.
pub struct All {
    pub list_type: ListType,
    pub f: InnerFunction,
}

impl All {
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

impl DeprecatedSnippet for All {
    fn entrypoint_name(&self) -> String {
        format!(
            "tasm_list_higher_order_{}_u32_all_{}",
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
        vec!["result".to_string()]
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
            _ => panic!("Can only map-reduce 'all' with functions with one input"),
        };
        let output_type = match self.f.get_output_types().len() {
            1 => self.f.get_output_types()[0].clone(),
            _ => panic!("Can only map-reduce 'all' with functions returning a bool"),
        };
        assert_eq!(output_type, DataType::Bool);
        let get_length = match self.list_type {
            ListType::Safe => library.import(Box::new(SafeLength(input_type.clone()))),
            ListType::Unsafe => library.import(Box::new(UnsafeLength(input_type.clone()))),
        };
        let list_get = match self.list_type {
            ListType::Safe => library.import(Box::new(SafeGet(input_type))),
            ListType::Unsafe => library.import(Box::new(UnsafeGet(input_type))),
        };

        let inner_function_name = match &self.f {
            InnerFunction::RawCode(rc) => rc.entrypoint(),
            InnerFunction::Snippet(sn) => {
                let fn_body = sn.function_code(library);
                let instructions = triton_vm::parser::parse(&fn_body).unwrap();
                let labelled_instructions =
                    triton_vm::parser::to_labelled_instructions(&instructions);
                library.explicit_import(&sn.entrypoint_name(), &labelled_instructions)
            }
            InnerFunction::NoFunctionBody(_) => todo!(),
        };

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
            // AFTER: _ result
            {entrypoint}:
                push 1 // _ input_list res
                swap 1 // _ res input_list
                dup 0 // _  res input_list input_list
                call {get_length} // _ res input_list len

                call {entrypoint}_loop // _ res input_list 0

                pop // _ res input_list
                pop // _ res
                return

            // INVARIANT: _ res input_list index
            {entrypoint}_loop:
                // test return condition
                dup 0 push 0 eq // _ res input_list index index==0

                skiz return
                // _ res input_list index

                // decrement index
                push -1 add

                // body

                // read
                dup 1 dup 1 // _ res input_list index input_list index
                call {list_get} // _ res input_list index [input_elements]

                // compute predicate
                call {inner_function_name} // _ res input_list index b

                // accumulate
                dup 3 // _ res input_list index b res
                mul //    _ res input_list index (b && res)
                swap 3 // _ (b && res) input_list index res
                pop

                recurse

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
        let list_pointer = BFieldElement::new(1u64);
        let mut rng = thread_rng();
        let list_length: usize = rng.gen_range(1..=100);

        vec![self.generate_input_state(list_pointer, list_length, true)]
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

        // forall elements, read + map + maybe copy
        let mut satisfied = true;
        for i in 0..len {
            // read
            let mut input_item = get_element(list_pointer, i, memory, input_type.get_size());

            // put on stack
            while !input_item.is_empty() {
                stack.push(input_item.pop().unwrap());
            }

            self.f
                .rust_shadowing(&std_in.clone(), &secret_in.clone(), stack, memory);

            let single_result = stack.pop().unwrap().value() != 0;
            satisfied = satisfied && single_result;
        }

        // set result
        stack.push(BFieldElement::new(satisfied as u64));
    }
}

// Only used for tests. Please don't export this.
#[derive(Debug, Clone)]
struct TestHashXFieldElementLsb;

impl DeprecatedSnippet for TestHashXFieldElementLsb {
    fn entrypoint_name(&self) -> String {
        "test_hash_xfield_element_lsb".to_string()
    }

    fn input_field_names(&self) -> Vec<String> {
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

    fn output_field_names(&self) -> Vec<String> {
        vec!["bool".to_string()]
    }

    fn stack_diff(&self) -> isize {
        -2
    }

    fn function_code(&self, _library: &mut Library) -> String {
        let entrypoint = self.entrypoint_name();
        format!(
            "
        // BEFORE: _ x2 x1 x0
        // AFTER: _ b
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

    fn crash_conditions(&self) -> Vec<String> {
        vec![]
    }

    fn gen_input_states(&self) -> Vec<ExecutionState> {
        // Function does not output random values, since that would make the benchmark output
        // non-deterministic.
        vec![ExecutionState::with_stack(
            vec![
                vec![BFieldElement::zero(); 16],
                vec![
                    BFieldElement::new(4888),
                    BFieldElement::new(1u64 << 63),
                    BFieldElement::new((1u64 << 51) + 1000),
                ],
            ]
            .concat(),
        )]
    }

    fn common_case_input_state(&self) -> ExecutionState {
        ExecutionState::with_stack(
            vec![
                vec![BFieldElement::zero(); 16],
                vec![
                    BFieldElement::new(4888),
                    BFieldElement::new(1u64 << 63),
                    BFieldElement::new((1u64 << 51) + 1000),
                ],
            ]
            .concat(),
        )
    }

    fn worst_case_input_state(&self) -> ExecutionState {
        ExecutionState::with_stack(
            vec![
                vec![BFieldElement::zero(); 16],
                vec![
                    BFieldElement::new(488800000),
                    BFieldElement::new(1u64 << 62),
                    BFieldElement::new((1u64 << 41) + 1001),
                ],
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
    ) {
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
    use num::One;
    use std::cell::RefCell;
    use triton_vm::triton_asm;

    use crate::{
        list::higher_order::inner_function::RawCode,
        test_helpers::{
            test_rust_equivalence_given_input_values_deprecated,
            test_rust_equivalence_multiple_deprecated,
        },
    };

    use super::*;

    #[test]
    fn unsafe_list_prop_test() {
        test_rust_equivalence_multiple_deprecated(
            &All {
                list_type: ListType::Unsafe,
                f: InnerFunction::Snippet(Box::new(TestHashXFieldElementLsb)),
            },
            false,
        );
    }

    #[test]
    fn with_safe_list_prop_test() {
        test_rust_equivalence_multiple_deprecated(
            &All {
                list_type: ListType::Safe,
                f: InnerFunction::Snippet(Box::new(TestHashXFieldElementLsb)),
            },
            false,
        );
    }

    #[test]
    fn safe_list_all_lt_test() {
        const TWO_POW_31: u64 = 1u64 << 31;
        let rawcode = RawCode::new_with_shadowing(
            triton_asm!(
                less_than_2_pow_31:
                    push 2147483648 // == 2^31
                    swap 1
                    lt
                    return
            ),
            vec![DataType::BFE],
            vec![DataType::Bool],
            Box::new(RefCell::new(|vec: &mut Vec<BFieldElement>| {
                let new_value = vec.pop().unwrap().value() < TWO_POW_31;
                vec.push(BFieldElement::new(new_value as u64));
            })),
        );
        let snippet = All {
            list_type: ListType::Safe,
            f: InnerFunction::RawCode(rawcode),
        };
        let mut memory = HashMap::new();

        // Should return true
        rust_shadowing_helper_functions::safe_list::safe_list_insert(
            BFieldElement::new(42),
            42,
            (0..30).map(BFieldElement::new).collect_vec(),
            &mut memory,
        );
        let input_stack = vec![get_init_tvm_stack(), vec![BFieldElement::new(42)]].concat();
        let expected_end_stack_true =
            vec![get_init_tvm_stack(), vec![BFieldElement::one()]].concat();
        test_rust_equivalence_given_input_values_deprecated(
            &snippet,
            &input_stack,
            &[],
            &mut memory,
            1,
            Some(&expected_end_stack_true),
        );

        // Should return false
        rust_shadowing_helper_functions::safe_list::safe_list_insert(
            BFieldElement::new(42),
            42,
            (0..30)
                .map(|x| BFieldElement::new(x + TWO_POW_31 - 20))
                .collect_vec(),
            &mut memory,
        );
        let expected_end_stack_false =
            vec![get_init_tvm_stack(), vec![BFieldElement::zero()]].concat();
        test_rust_equivalence_given_input_values_deprecated(
            &snippet,
            &input_stack,
            &[],
            &mut memory,
            1,
            Some(&expected_end_stack_false),
        );
    }

    #[test]
    fn test_with_raw_function_lsb_on_bfe() {
        let rawcode = RawCode::new_with_shadowing(
            triton_asm!(
                lsb_bfe:
                split    // _ hi lo
                push 2   // _ hi lo 2
                swap 1   // _ hi 2 lo
                div      // _ hi q r
                swap 2   // _ r q hi
                pop      // _ r q
                pop      // _ r
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
            &All {
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
                split    // _ x2 x1 hi lo
                push 2   // _ x2 x1 hi lo 2
                swap 1   // _ x2 x1 hi 2 lo
                div      // _ x2 x1 hi q r
                swap 4   // _ r x1 q hi x2
                pop      // _ r x1 q hi
                pop      // _ r x1 q
                pop      // _ r q
                pop      // _ r
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
            &All {
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
    fn unsafe_list_all_benchmark() {
        bench_and_write(All {
            list_type: ListType::Unsafe,
            f: InnerFunction::Snippet(Box::new(TestHashXFieldElementLsb)),
        });
    }
}
