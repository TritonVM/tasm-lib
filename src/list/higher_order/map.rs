use itertools::Itertools;
use num::Zero;
use rand::{thread_rng, Rng};
use std::cell::RefCell;
use std::collections::HashMap;
use triton_opcodes::instruction::LabelledInstruction;
use twenty_first::shared_math::b_field_element::BFieldElement;

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
    ExecutionState,
};

/// A data structure for describing an inner function to a map without using a snippet
pub struct RawCode {
    pub function: Vec<LabelledInstruction>,
    pub input_types: Vec<DataType>,
    pub output_types: Vec<DataType>,
    #[allow(clippy::type_complexity)]
    rust_shadowing: Option<Box<RefCell<dyn FnMut(&mut Vec<BFieldElement>)>>>,
}

impl RawCode {
    pub fn new(
        function: Vec<LabelledInstruction>,
        input_types: Vec<DataType>,
        output_types: Vec<DataType>,
    ) -> Self {
        // Verify that 1st line is a label
        assert!(
            function.len() >= 2,
            "Inner function must have at least two lines: a label and a return or recurse"
        );
        assert!(
            matches!(function[0], LabelledInstruction::Label(_)),
            "First line of inner function must be label. Got: {}",
            function[0]
        );
        assert!(
            matches!(
                function.last().unwrap(),
                LabelledInstruction::Instruction(
                    triton_opcodes::instruction::AnInstruction::Return
                ) | LabelledInstruction::Instruction(
                    triton_opcodes::instruction::AnInstruction::Recurse
                )
            ),
            "Last line of inner function must be either return or recurse. Got: {}",
            function.last().unwrap()
        );

        Self {
            function,
            input_types,
            output_types,
            rust_shadowing: None,
        }
    }
}

impl RawCode {
    fn entrypoint(&self) -> String {
        match &self.function[0] {
            LabelledInstruction::Instruction(inst) => {
                panic!("First line of inner function must be a label. Got: {inst}")
            }
            LabelledInstruction::Label(label) => label.to_owned(),
        }
    }
}

pub enum InnerFunction {
    RawCode(RawCode),
    Snippet(Box<dyn Snippet>),
}

impl InnerFunction {
    fn get_input_types(&self) -> Vec<DataType> {
        match self {
            InnerFunction::RawCode(raw) => raw.input_types.clone(),
            InnerFunction::Snippet(f) => f.input_types(),
        }
    }

    fn get_output_types(&self) -> Vec<DataType> {
        match self {
            InnerFunction::RawCode(rc) => rc.output_types.clone(),
            InnerFunction::Snippet(sn) => sn.output_types(),
        }
    }

    fn entrypoint(&self) -> String {
        match self {
            InnerFunction::RawCode(rc) => rc.entrypoint(),
            InnerFunction::Snippet(sn) => sn.entrypoint(),
        }
    }

    fn rust_shadowing(
        &self,
        std_in: &[BFieldElement],
        secret_in: &[BFieldElement],
        stack: &mut Vec<BFieldElement>,
        memory: &mut HashMap<BFieldElement, BFieldElement>,
    ) {
        match &self {
            InnerFunction::RawCode(rc) => {
                if let Some(func) = &rc.rust_shadowing {
                    let mut func = func.borrow_mut();
                    (*func)(stack)
                } else {
                    panic!("Raw code must have rust shadowing for equivalence testing")
                }
            }
            InnerFunction::Snippet(sn) => {
                sn.rust_shadowing(stack, std_in.to_vec(), secret_in.to_vec(), memory)
            }
        };
    }
}

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
                    BFieldElement::new((1 + 2 + list_length * input_type.get_size()) as u64)
                }
                ListType::Unsafe => {
                    BFieldElement::new((1 + 1 + list_length * input_type.get_size()) as u64)
                }
            },
        );

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

impl Snippet for Map {
    fn entrypoint(&self) -> String {
        format!(
            "tasm_list_higher_order_{}_u32_map_{}",
            self.list_type,
            self.f.entrypoint()
        )
    }

    fn inputs(&self) -> Vec<String> {
        vec!["input_list".to_string()]
    }

    fn input_types(&self) -> Vec<crate::snippet::DataType> {
        vec![DataType::List(Box::new(
            self.f.get_input_types()[0].clone(),
        ))]
    }

    fn output_types(&self) -> Vec<crate::snippet::DataType> {
        vec![DataType::List(Box::new(
            self.f.get_output_types()[0].clone(),
        ))]
    }

    fn outputs(&self) -> Vec<String> {
        vec!["output_list".to_string()]
    }

    fn stack_diff(&self) -> isize {
        0
    }

    fn function_code(&self, library: &mut SnippetState) -> String {
        let input_type = match self.f.get_input_types().len() {
            1 => self.f.get_input_types()[0].clone(),
            _ => panic!("Can only map over functions with one input"),
        };
        let output_type = match self.f.get_output_types().len() {
            1 => self.f.get_output_types()[0].clone(),
            _ => panic!("Can only map over functions with one output"),
        };
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

        let inner_function_name = match &self.f {
            InnerFunction::RawCode(rc) => rc.entrypoint(),
            InnerFunction::Snippet(sn) => {
                let fn_body = sn.function_code(library);
                library.explicit_import(&sn.entrypoint(), fn_body)
            }
        };

        // If function was supplied as raw instructions, we need to append the inner function to the function
        // body. Otherwise, `library` handles the imports.
        let maybe_inner_function_body_raw = match &self.f {
            InnerFunction::RawCode(rc) => rc.function.iter().map(|x| x.to_string()).join("\n"),
            InnerFunction::Snippet(_) => String::default(),
        };
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
                push 0 eq // _ input_list output_list itr itr==0

                skiz return
                // _ input_list output_list itr

                // body
                push -1 add // _input_list output_list index

                // read
                dup 2 dup 1 // _input_list output_list index _input_list index
                call {list_get} // _ input_list output_list index [input_element]

                // map
                call {inner_function_name} // _ input_list output_list index [output_element]

                // write
                dup {output_size_plus_one} // _ input_list output_list index [output_element] output_list
                dup {output_size_plus_one} // _ input_list output_list index [output_element] output_list index
                call {list_set} // _ input_list output_list index

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
        let list_pointer = BFieldElement::new(1u64);
        let mut rng = thread_rng();
        let list_length: usize = rng.gen_range(1..=100);

        vec![self.generate_input_state(list_pointer, list_length)]
    }

    fn common_case_input_state(&self) -> ExecutionState {
        // Create random list of input data type
        let list_pointer = BFieldElement::new(1u64);
        let list_length: usize = 10;
        self.generate_input_state(list_pointer, list_length)
    }

    fn worst_case_input_state(&self) -> ExecutionState {
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
    ) {
        let input_type = match self.f.get_input_types().len() {
            1 => self.f.get_input_types()[0].clone(),
            _ => panic!("Input length must be one when using function in map)"),
        };
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

            self.f.rust_shadowing(&std_in, &secret_in, stack, memory);

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

#[cfg(test)]
mod tests {
    use triton_opcodes::shortcuts::*;
    use twenty_first::{
        shared_math::{
            other::random_elements, traits::FiniteField, x_field_element::XFieldElement,
        },
        util_types::algebraic_hasher::AlgebraicHasher,
    };

    use crate::{
        snippet_bencher::bench_and_write, test_helpers::rust_tasm_equivalence_prop_new, VmHasher,
    };

    use super::*;

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

        fn function_code(&self, _library: &mut SnippetState) -> String {
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

    #[test]
    fn unsafe_list_prop_test() {
        rust_tasm_equivalence_prop_new(
            &Map {
                list_type: ListType::Unsafe,
                f: InnerFunction::Snippet(Box::new(TestHashXFieldElement)),
            },
            false,
        );
    }

    #[test]
    fn with_safe_list_prop_test() {
        rust_tasm_equivalence_prop_new(
            &Map {
                list_type: ListType::Safe,
                f: InnerFunction::Snippet(Box::new(TestHashXFieldElement)),
            },
            false,
        );
    }

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

    #[test]
    fn test_with_raw_function_identity_on_bfe() {
        let rawcode = RawCode {
            function: vec![
                LabelledInstruction::Label("identity_bfe".to_string()),
                return_(),
            ],
            input_types: vec![DataType::BFE],
            output_types: vec![DataType::BFE],
            rust_shadowing: Some(Box::new(RefCell::new(|_vec: &mut Vec<BFieldElement>| {}))),
        };
        rust_tasm_equivalence_prop_new(
            &Map {
                list_type: ListType::Unsafe,
                f: InnerFunction::RawCode(rawcode),
            },
            false,
        );
    }

    #[test]
    fn test_with_raw_function_square_on_bfe() {
        let rawcode = RawCode {
            function: vec![
                LabelledInstruction::Label("square_bfe".to_string()),
                dup(0),
                mul(),
                return_(),
            ],
            input_types: vec![DataType::BFE],
            output_types: vec![DataType::BFE],
            rust_shadowing: Some(Box::new(RefCell::new(|vec: &mut Vec<BFieldElement>| {
                let new_value = vec.pop().unwrap().square();
                vec.push(new_value);
            }))),
        };
        rust_tasm_equivalence_prop_new(
            &Map {
                list_type: ListType::Unsafe,
                f: InnerFunction::RawCode(rawcode),
            },
            false,
        );
    }

    #[test]
    fn test_with_raw_function_square_on_xfe() {
        let rawcode = RawCode {
            function: vec![
                LabelledInstruction::Label("square_xfe".to_string()),
                dup(2),
                dup(2),
                dup(2),
                xxmul(),
                swap(3),
                pop(),
                swap(3),
                pop(),
                swap(3),
                pop(),
                return_(),
            ],
            input_types: vec![DataType::XFE],
            output_types: vec![DataType::XFE],
            rust_shadowing: Some(Box::new(RefCell::new(|vec: &mut Vec<BFieldElement>| {
                let x0 = vec.pop().unwrap();
                let x1 = vec.pop().unwrap();
                let x2 = vec.pop().unwrap();
                let xfe = XFieldElement::new([x0, x1, x2]);
                let new_value = xfe.square();
                vec.push(new_value.coefficients[2]);
                vec.push(new_value.coefficients[1]);
                vec.push(new_value.coefficients[0]);
            }))),
        };
        rust_tasm_equivalence_prop_new(
            &Map {
                list_type: ListType::Unsafe,
                f: InnerFunction::RawCode(rawcode),
            },
            false,
        );
    }
}
