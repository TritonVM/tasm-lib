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
use crate::{get_init_tvm_stack, rust_shadowing_helper_functions, VmHasher};
use crate::{
    library::Library,
    snippet::{DataType, DeprecatedSnippet},
    ExecutionState,
};

use super::inner_function::InnerFunction;

/// Applies a given function to every element of a list, and collects
/// the new elements into a new list. The function must be given as
/// an object as well as a dynamic type parameter.
pub struct Map {
    pub list_type: ListType,
    pub f: InnerFunction,
}

impl Map {
    fn generate_input_state(
        &self,
        list_pointer: BFieldElement,
        list_length: usize,
        additional_function_args: Vec<BFieldElement>,
    ) -> ExecutionState {
        let capacity = list_length;
        let mut stack = get_init_tvm_stack();

        // Add additional input args to stack, if they exist
        for additional_function_arg in additional_function_args.into_iter().rev() {
            stack.push(additional_function_arg);
        }

        stack.push(list_pointer);

        let mut memory = HashMap::default();
        let input_element_type = self.f.input_list_element_type();
        let input_list_size = match self.list_type {
            ListType::Safe => {
                BFieldElement::new((2 + list_length * input_element_type.get_size()) as u64)
            }
            ListType::Unsafe => {
                BFieldElement::new((1 + list_length * input_element_type.get_size()) as u64)
            }
        };
        rust_shadowing_helper_functions::dyn_malloc::rust_dyn_malloc_initialize(
            &mut memory,
            (input_list_size + list_pointer).value() as usize,
        );

        match self.list_type {
            ListType::Safe => safe_insert_random_list(
                &input_element_type,
                list_pointer,
                capacity as u32,
                list_length,
                &mut memory,
            ),
            ListType::Unsafe => unsafe_insert_random_list(
                &input_element_type,
                list_pointer,
                list_length,
                &mut memory,
            ),
        };

        ExecutionState {
            stack,
            std_in: vec![],
            nondeterminism: NonDeterminism::new(vec![]),
            memory,
            words_allocated: 0,
        }
    }
}

impl DeprecatedSnippet for Map {
    fn entrypoint_name(&self) -> String {
        format!(
            "tasm_list_higher_order_{}_u32_map_{}",
            self.list_type,
            self.f.entrypoint()
        )
    }

    fn input_field_names(&self) -> Vec<String> {
        let mut ret = vec![];
        let additional_input_size = self.f.size_of_additional_inputs();
        for i in 0..additional_input_size {
            ret.push(format!("additional_input_{i}"));
        }

        ret.push("*input_list".to_owned());

        ret
    }

    fn input_types(&self) -> Vec<crate::snippet::DataType> {
        let input_list_type = DataType::List(Box::new(self.f.input_list_element_type()));
        let additional_inputs = self.f.additional_inputs();
        vec![additional_inputs, vec![input_list_type]].concat()
    }

    fn output_types(&self) -> Vec<crate::snippet::DataType> {
        vec![DataType::List(Box::new(
            self.f.get_output_types()[0].clone(),
        ))]
    }

    fn output_field_names(&self) -> Vec<String> {
        vec!["output_list".to_string()]
    }

    fn stack_diff(&self) -> isize {
        -(self.f.size_of_additional_inputs() as isize)
    }

    fn function_code(&self, library: &mut Library) -> String {
        let input_list_element_type = self.f.input_list_element_type();
        let output_type = match self.f.get_output_types().len() {
            1 => self.f.get_output_types()[0].clone(),
            _ => panic!("Can only map over functions with one output"),
        };
        let output_size_plus_one = 1 + output_type.get_size();
        let get_length = match self.list_type {
            ListType::Safe => library.import(Box::new(SafeLength(input_list_element_type.clone()))),
            ListType::Unsafe => {
                library.import(Box::new(UnsafeLength(input_list_element_type.clone())))
            }
        };
        let set_length = match self.list_type {
            ListType::Safe => {
                library.import(Box::new(SafeSetLength(input_list_element_type.clone())))
            }
            ListType::Unsafe => {
                library.import(Box::new(UnsafeSetLength(input_list_element_type.clone())))
            }
        };
        let new_list = match self.list_type {
            ListType::Safe => library.import(Box::new(SafeNew(output_type.clone()))),
            ListType::Unsafe => library.import(Box::new(UnsafeNew(output_type.clone()))),
        };
        let list_get = match self.list_type {
            ListType::Safe => library.import(Box::new(SafeGet(input_list_element_type))),
            ListType::Unsafe => library.import(Box::new(UnsafeGet(input_list_element_type))),
        };
        let list_set = match self.list_type {
            ListType::Safe => library.import(Box::new(SafeSet(output_type))),
            ListType::Unsafe => library.import(Box::new(UnsafeSet(output_type))),
        };

        // Declare the inner function entrypoint name and import inner function in case it's a snippet
        let inner_function_name = match &self.f {
            InnerFunction::RawCode(rc) => rc.entrypoint(),
            InnerFunction::Snippet(sn) => {
                let fn_body = sn.function_code(library);
                let (_, instructions) = tokenize(&fn_body).unwrap();
                let labelled_instructions =
                    triton_vm::parser::to_labelled_instructions(&instructions);
                library.explicit_import(&sn.entrypoint_name(), &labelled_instructions)
            }
            InnerFunction::NoFunctionBody(lnat) => lnat.label_name.to_owned(),
        };

        // If function was supplied as raw instructions, we need to append the inner function to the function
        // body. Otherwise, `library` handles the imports.
        let maybe_inner_function_body_raw = match &self.f {
            InnerFunction::RawCode(rc) => rc.function.iter().map(|x| x.to_string()).join("\n"),
            InnerFunction::Snippet(_) => String::default(),
            InnerFunction::NoFunctionBody(_) => String::default(),
        };
        let entrypoint = self.entrypoint_name();

        let additional_input_arg_size = self.f.size_of_additional_inputs();
        let clean_addition_inputs_args_from_stack = match additional_input_arg_size {
            0 => String::default(),
            n => {
                let mut stack_cleanup_code = format!("swap {additional_input_arg_size}\n");
                stack_cleanup_code.push_str(&"pop\n".repeat(n));
                stack_cleanup_code
            }
        };

        format!(
            "
            // BEFORE: _ <[additional_input_args]> input_list
            // AFTER: _ output_list
            {entrypoint}:
                dup 0                   // _ <aia> input_list input_list
                call {get_length}       // _ <aia> input_list len
                dup 0                   // _ <aia> input_list len len
                call {new_list}         // _ <aia> input_list len output_list
                dup 1                   // _ <aia> input_list len output_list len
                call {set_length}       // _ <aia> input_list len output_list
                swap 1                  // _ <aia> input_list output_list len

                call {entrypoint}_loop  // _ <aia> input_list output_list 0

                pop                     // _ <aia> input_list output_list
                swap 1                  // _ <aia> output_list input_list
                pop                     // _ <aia> output_list
                {clean_addition_inputs_args_from_stack}
                // _ output_list

                return

            // INVARIANT: _ <aia> input_list output_list itr
            {entrypoint}_loop:
                // test return condition
                dup 0                   // _ <aia> input_list output_list itr itr
                push 0 eq               // _ <aia> input_list output_list itr itr==0

                skiz return
                // _ input_list output_list itr

                // body
                push -1 add             // _ <aia> input_list output_list index

                // read
                dup 2 dup 1             // _ <aia> input_list output_list index _input_list index
                call {list_get}         // _ <aia> input_list output_list index [input_element]

                // map
                call {inner_function_name} // _ <aia> input_list output_list index [output_element]

                // write
                dup {output_size_plus_one} // _ <aia> input_list output_list index [output_element] output_list
                dup {output_size_plus_one} // _ <aia> input_list output_list index [output_element] output_list index
                call {list_set}            // _ <aia> input_list output_list index

                recurse

            {maybe_inner_function_body_raw}
            "
        )
    }

    fn crash_conditions(&self) -> Vec<String> {
        vec![
            "Map function does not take 1 element.".to_string(),
            "Map function does not yield 1 element.".to_string(),
            "Length exceeds u32::MAX".to_string(),
        ]
    }

    fn gen_input_states(&self) -> Vec<ExecutionState> {
        // Create random list of input data type
        let mut rng = thread_rng();
        let list_pointer = BFieldElement::new(rng.gen_range(1..=1000));
        let list_length: usize = rng.gen_range(1..=1000);
        let additional_inputs = self.f.additional_inputs();
        let additional_inputs = additional_inputs
            .iter()
            .flat_map(|x| x.random_elements(1)[0].clone())
            .collect_vec();

        vec![self.generate_input_state(list_pointer, list_length, additional_inputs)]
    }

    fn common_case_input_state(&self) -> ExecutionState {
        // Create random list of input data type
        let list_pointer = BFieldElement::new(1u64);
        let list_length: usize = 10;
        let additional_inputs = self.f.additional_inputs();
        let additional_inputs = additional_inputs
            .iter()
            .flat_map(|x| x.random_elements(1)[0].clone())
            .collect_vec();
        self.generate_input_state(list_pointer, list_length, additional_inputs)
    }

    fn worst_case_input_state(&self) -> ExecutionState {
        // Create random list of input data type
        let list_pointer = BFieldElement::new(1u64);
        let list_length: usize = 400;
        let additional_inputs = self.f.additional_inputs();
        let additional_inputs = additional_inputs
            .iter()
            .flat_map(|x| x.random_elements(1)[0].clone())
            .collect_vec();
        self.generate_input_state(list_pointer, list_length, additional_inputs)
    }

    fn rust_shadowing(
        &self,
        stack: &mut Vec<triton_vm::BFieldElement>,
        std_in: Vec<triton_vm::BFieldElement>,
        secret_in: Vec<triton_vm::BFieldElement>,
        memory: &mut std::collections::HashMap<triton_vm::BFieldElement, triton_vm::BFieldElement>,
    ) {
        let input_list_element_type = self.f.input_list_element_type();
        let output_type = match self.f.get_output_types().len() {
            1 => self.f.get_output_types()[0].clone(),
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

        let list_element = match self.list_type {
            ListType::Safe => rust_shadowing_helper_functions::safe_list::safe_list_get,
            ListType::Unsafe => rust_shadowing_helper_functions::unsafe_list::unsafe_list_get,
        };

        let set_element = match self.list_type {
            ListType::Safe => rust_shadowing_helper_functions::safe_list::safe_list_set,
            ListType::Unsafe => rust_shadowing_helper_functions::unsafe_list::unsafe_list_set,
        };

        let output_list_capacity = len;
        let output_list =
            match self.list_type {
                ListType::Safe => {
                    // Push capacity to stack
                    stack.push(BFieldElement::new(output_list_capacity as u64));
                    list::safe_u32::new::SafeNew(input_list_element_type.clone()).rust_shadowing(
                        stack,
                        std_in.clone(),
                        secret_in.clone(),
                        memory,
                    );
                    stack.pop().unwrap()
                }
                ListType::Unsafe => {
                    stack.push(BFieldElement::new(output_list_capacity as u64));
                    list::unsafe_u32::new::UnsafeNew(input_list_element_type.clone())
                        .rust_shadowing(stack, std_in.clone(), secret_in.clone(), memory);
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
            let mut input_item =
                list_element(list_pointer, i, memory, input_list_element_type.get_size());

            // put on stack
            while !input_item.is_empty() {
                stack.push(input_item.pop().unwrap());
            }

            self.f.rust_shadowing(&std_in, &secret_in, stack, memory);

            // pull from stack
            let mut output_item = vec![];
            for _ in 0..output_type.get_size() {
                output_item.push(stack.pop().unwrap());
            }

            // write
            set_element(output_list, i, output_item, memory, output_type.get_size());
        }

        // Remove all additional arguments from stack
        for _ in 0..self.f.size_of_additional_inputs() {
            stack.pop();
        }

        stack.push(output_list);
    }
}

#[derive(Debug, Clone)]
struct TestHashXFieldElement;

impl DeprecatedSnippet for TestHashXFieldElement {
    fn entrypoint_name(&self) -> String {
        "test_hash_xfield_element".to_string()
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
        vec![DataType::Digest]
    }

    fn output_field_names(&self) -> Vec<String>
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

    fn function_code(&self, _library: &mut Library) -> String {
        let entrypoint = self.entrypoint_name();
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
        let mut digest = VmHasher::hash_varlen(&xfield_element).values().to_vec();
        while !digest.is_empty() {
            stack.push(digest.pop().unwrap());
        }
    }
}

#[cfg(test)]
mod tests {

    use std::cell::RefCell;

    use triton_vm::triton_asm;
    use twenty_first::shared_math::{
        bfield_codec::BFieldCodec, traits::FiniteField, x_field_element::XFieldElement,
    };

    use crate::{
        list::higher_order::inner_function::RawCode,
        test_helpers::test_rust_equivalence_multiple_deprecated,
    };

    use super::*;

    #[test]
    fn unsafe_list_prop_test() {
        test_rust_equivalence_multiple_deprecated(
            &Map {
                list_type: ListType::Unsafe,
                f: InnerFunction::Snippet(Box::new(TestHashXFieldElement)),
            },
            false,
        );
    }

    #[test]
    fn with_safe_list_prop_test() {
        test_rust_equivalence_multiple_deprecated(
            &Map {
                list_type: ListType::Safe,
                f: InnerFunction::Snippet(Box::new(TestHashXFieldElement)),
            },
            false,
        );
    }

    #[test]
    fn test_with_raw_function_identity_on_bfe() {
        let rawcode = RawCode::new_with_shadowing(
            triton_asm!(identity_bfe: return),
            vec![DataType::BFE],
            vec![DataType::BFE],
            Box::new(RefCell::new(|_vec: &mut Vec<BFieldElement>| {})),
        );
        test_rust_equivalence_multiple_deprecated(
            &Map {
                list_type: ListType::Unsafe,
                f: InnerFunction::RawCode(rawcode),
            },
            false,
        );
    }

    #[test]
    fn test_with_raw_function_square_on_bfe() {
        let rawcode = RawCode::new_with_shadowing(
            triton_asm!(square_bfe: dup 0 mul return),
            vec![DataType::BFE],
            vec![DataType::BFE],
            Box::new(RefCell::new(|vec: &mut Vec<BFieldElement>| {
                let new_value = vec.pop().unwrap().square();
                vec.push(new_value);
            })),
        );
        test_rust_equivalence_multiple_deprecated(
            &Map {
                list_type: ListType::Unsafe,
                f: InnerFunction::RawCode(rawcode),
            },
            false,
        );
    }

    #[test]
    fn test_with_raw_function_square_plus_n_on_bfe() {
        // Inner function calculates `|(n, x)| -> x*x + n`, where `x` is the list
        // element, and `n` is the same value for all elements.
        let rawcode = RawCode::new_with_shadowing(
            triton_asm!(square_plus_n_bfe: dup 0 mul dup 4 add return),
            vec![DataType::BFE, DataType::BFE],
            vec![DataType::BFE],
            Box::new(RefCell::new(|vec: &mut Vec<BFieldElement>| {
                let mut new_value = vec.pop().unwrap().square();
                new_value += vec[vec.len() - 1];
                vec.push(new_value);
            })),
        );
        test_rust_equivalence_multiple_deprecated(
            &Map {
                list_type: ListType::Unsafe,
                f: InnerFunction::RawCode(rawcode),
            },
            false,
        );
    }

    #[test]
    fn test_with_raw_function_square_on_xfe() {
        let rawcode = RawCode::new_with_shadowing(
            triton_asm!(
                square_xfe: dup 2 dup 2 dup 2 xxmul swap 3 pop swap 3 pop swap 3 pop return
            ),
            vec![DataType::XFE],
            vec![DataType::XFE],
            Box::new(RefCell::new(|vec: &mut Vec<BFieldElement>| {
                let x0 = vec.pop().unwrap();
                let x1 = vec.pop().unwrap();
                let x2 = vec.pop().unwrap();
                let xfe = XFieldElement::new([x0, x1, x2]);
                let new_value = xfe.square();
                vec.push(new_value.coefficients[2]);
                vec.push(new_value.coefficients[1]);
                vec.push(new_value.coefficients[0]);
            })),
        );
        test_rust_equivalence_multiple_deprecated(
            &Map {
                list_type: ListType::Unsafe,
                f: InnerFunction::RawCode(rawcode),
            },
            false,
        );
    }

    #[test]
    fn test_with_raw_function_square_on_xfe_plus_another_xfe() {
        let rawcode = RawCode::new_with_shadowing(
            triton_asm!(
                square_xfe_plus_another_xfe:
                    dup 2 dup 2 dup 2 xxmul
                    swap 3 pop swap 3 pop swap 3 pop
                    dup 8 dup 8 dup 8 xxadd
                    swap 3 pop swap 3 pop swap 3 pop
                    return
            ),
            vec![DataType::XFE, DataType::XFE],
            vec![DataType::XFE],
            Box::new(RefCell::new(|vec: &mut Vec<BFieldElement>| {
                let x0 = vec.pop().unwrap();
                let x1 = vec.pop().unwrap();
                let x2 = vec.pop().unwrap();
                let xfe = XFieldElement::new([x0, x1, x2]);
                let mut new_value = xfe.square();

                let y0 = vec[vec.len() - 1];
                let y1 = vec[vec.len() - 2];
                let y2 = vec[vec.len() - 3];
                let another_xfe = XFieldElement::new([y0, y1, y2]);
                new_value += another_xfe;

                vec.push(new_value.coefficients[2]);
                vec.push(new_value.coefficients[1]);
                vec.push(new_value.coefficients[0]);
            })),
        );
        test_rust_equivalence_multiple_deprecated(
            &Map {
                list_type: ListType::Unsafe,
                f: InnerFunction::RawCode(rawcode),
            },
            false,
        );
    }

    #[test]
    fn test_u32_list_to_u128_list_plus_x() {
        let rawcode = RawCode::new_with_shadowing(
            triton_asm!(
                u32_to_u128_add_another_u128:
                // stack:  _ [x_3, x_2, x_1, x_0] input_list output_list index input_u32
                dup 4
                // stack:  _ [x_3, x_2, x_1, x_0] input_list output_list index input_u32 x_0
                add
                // stack:  _ [x_3, x_2, x_1, x_0] input_list output_list index (input_u32 + x_0)
                split
                // stack:  _ [x_3, x_2, x_1, x_0] input_list output_list index carry_to_1 output_0
                swap 1
                // stack:  _ [x_3, x_2, x_1, x_0] input_list output_list index output_0 carry_to_1
                dup 6
                // stack:  _ [x_3, x_2, x_1, x_0] input_list output_list index output_0 carry_to_1 x_1
                add
                split
                // stack:  _ [x_3, x_2, x_1, x_0] input_list output_list index output_0 carry_to_2 output_1
                swap 1
                // stack:  _ [x_3, x_2, x_1, x_0] input_list output_list index output_0 output_1 carry_to_2
                dup 8
                add
                split
                // stack:  _ [x_3, x_2, x_1, x_0] input_list output_list index output_0 output_1 carry_to_3 output_2
                swap 1
                // stack:  _ [x_3, x_2, x_1, x_0] input_list output_list index output_0 output_1 output_2 carry_to_3
                dup 10
                add
                split
                // stack:  _ [x_3, x_2, x_1, x_0] input_list output_list index output_0 output_1 output_2 overflow output_3
                swap 1
                // stack:  _ [x_3, x_2, x_1, x_0] input_list output_list index output_0 output_1 output_2 output_3 overflow

                // verify no overflow
                push 0
                eq
                assert
                // stack:  _ [x_3, x_2, x_1, x_0] input_list output_list index output_0 output_1 output_2 output_3
                swap 3
                swap 1
                swap 2
                swap 1
                // stack:  _ [x_3, x_2, x_1, x_0] input_list output_list index output_3 output_2 output_1 output_0
                return
            ),
            vec![DataType::U128, DataType::U32],
            vec![DataType::U128],
            Box::new(RefCell::new(|vec: &mut Vec<BFieldElement>| {
                let list_element = vec.pop().unwrap().value();
                let u128_value = vec[vec.len() - 1].value() as u128
                    + ((vec[vec.len() - 2].value() as u128) << 32)
                    + ((vec[vec.len() - 3].value() as u128) << 64)
                    + ((vec[vec.len() - 4].value() as u128) << 96);

                let new_value = list_element as u128 + u128_value;
                let encoded = new_value.encode();
                for elem in encoded.into_iter().rev() {
                    vec.push(elem);
                }
            })),
        );
        test_rust_equivalence_multiple_deprecated(
            &Map {
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
    fn unsafe_list_map_benchmark() {
        bench_and_write(Map {
            list_type: ListType::Unsafe,
            f: InnerFunction::Snippet(Box::new(TestHashXFieldElement)),
        });
    }

    #[test]
    fn safe_list_map_benchmark() {
        bench_and_write(Map {
            list_type: ListType::Safe,
            f: InnerFunction::Snippet(Box::new(TestHashXFieldElement)),
        });
    }
}
