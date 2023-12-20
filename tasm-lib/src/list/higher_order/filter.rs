use std::collections::HashMap;

use itertools::Itertools;
use num::Zero;
use rand::rngs::StdRng;
use rand::{Rng, SeedableRng};
use triton_vm::parser::tokenize;
use triton_vm::triton_asm;
use twenty_first::shared_math::b_field_element::BFieldElement;

use crate::data_type::DataType;
use crate::library::Library;
use crate::list::safeimplu32::get::SafeGet;
use crate::list::safeimplu32::length::Length as SafeLength;
use crate::list::safeimplu32::new::SafeNew;
use crate::list::safeimplu32::set_length::SafeSetLength;
use crate::list::unsafeimplu32::get::UnsafeGet;
use crate::list::unsafeimplu32::length::Length as UnsafeLength;
use crate::list::unsafeimplu32::new::UnsafeNew;
use crate::list::unsafeimplu32::set_length::UnsafeSetLength;
use crate::list::{self, ListType};
use crate::memory::memcpy::MemCpy;
use crate::traits::basic_snippet::BasicSnippet;
use crate::traits::deprecated_snippet::DeprecatedSnippet;
use crate::traits::function::Function;
use crate::{empty_stack, rust_shadowing_helper_functions};

use super::inner_function::InnerFunction;

/// Filters a given list for elements that satisfy a predicate. A new
/// list is created, containing only those elements that satisfy the
/// predicate. The predicate must be given as an InnerFunction.
pub struct Filter {
    pub list_type: ListType,
    pub f: InnerFunction,
}

impl BasicSnippet for Filter {
    fn inputs(&self) -> Vec<(DataType, String)> {
        let list_type = match &self.f {
            InnerFunction::BasicSnippet(basic_snippet) => {
                DataType::List(Box::new(basic_snippet.inputs()[0].0.clone()))
            }
            _ => DataType::VoidPointer,
        };
        vec![(list_type, "*input_list".to_string())]
    }

    fn outputs(&self) -> Vec<(DataType, String)> {
        let list_type = match &self.f {
            InnerFunction::BasicSnippet(basic_snippet) => {
                DataType::List(Box::new(basic_snippet.inputs()[0].0.clone()))
            }
            _ => DataType::VoidPointer,
        };
        vec![(list_type, "*output_list".to_string())]
    }

    fn entrypoint(&self) -> String {
        format!(
            "tasm_list_higher_order_{}_u32_filter_{}",
            self.list_type,
            self.f.entrypoint()
        )
    }

    fn code(&self, library: &mut Library) -> Vec<triton_vm::instruction::LabelledInstruction> {
        let input_type = self.f.domain();
        let output_type = self.f.range();
        assert_eq!(output_type, DataType::Bool);
        let safety_offset = match self.list_type {
            ListType::Safe => 2,
            ListType::Unsafe => 1,
        };
        let get_length = match self.list_type {
            ListType::Safe => library.import(Box::new(SafeLength {
                data_type: input_type.clone(),
            })),
            ListType::Unsafe => library.import(Box::new(UnsafeLength {
                data_type: input_type.clone(),
            })),
        };
        let set_length = match self.list_type {
            ListType::Safe => library.import(Box::new(SafeSetLength {
                data_type: input_type.clone(),
            })),
            ListType::Unsafe => library.import(Box::new(UnsafeSetLength {
                data_type: input_type.clone(),
            })),
        };
        let new_list = match self.list_type {
            ListType::Safe => library.import(Box::new(SafeNew {
                data_type: output_type,
            })),
            ListType::Unsafe => library.import(Box::new(UnsafeNew {
                data_type: output_type,
            })),
        };
        let list_get = match self.list_type {
            ListType::Safe => library.import(Box::new(SafeGet {
                data_type: input_type,
            })),
            ListType::Unsafe => library.import(Box::new(UnsafeGet {
                data_type: input_type,
            })),
        };
        let element_size = self.f.domain().stack_size();

        let inner_function_name = match &self.f {
            InnerFunction::RawCode(rc) => rc.entrypoint(),
            InnerFunction::DeprecatedSnippet(sn) => {
                let fn_body = sn.function_code(library);
                let (_, instructions) = tokenize(&fn_body).unwrap();
                let labelled_instructions =
                    triton_vm::parser::to_labelled_instructions(&instructions);
                library.explicit_import(&sn.entrypoint_name(), &labelled_instructions)
            }
            InnerFunction::NoFunctionBody(_) => todo!(),
            InnerFunction::BasicSnippet(bs) => {
                let labelled_instructions = bs.code(library);
                library.explicit_import(&bs.entrypoint(), &labelled_instructions)
            }
        };

        let memcpy = library.import(Box::new(MemCpy));

        // If function was supplied as raw instructions, we need to append the inner function to the function
        // body. Otherwise, `library` handles the imports.
        let maybe_inner_function_body_raw = match &self.f {
            InnerFunction::RawCode(rc) => rc.function.iter().map(|x| x.to_string()).join("\n"),
            InnerFunction::DeprecatedSnippet(_) => String::default(),
            InnerFunction::NoFunctionBody(_) => todo!(),
            InnerFunction::BasicSnippet(_) => String::default(),
        };
        let entrypoint = self.entrypoint();
        let main_loop = format!("{entrypoint}_loop");
        let main_write = format!("{entrypoint}_write");

        triton_asm!(
            // BEFORE: _ *input_list
            // AFTER: _ *output_list
            {entrypoint}:
                dup 0               // _ *input_list *input_list
                call {get_length}   // _ *input_list len
                dup 0               // _ *input_list len len
                call {new_list}     // _ *input_list len *output_list
                dup 1               // _ *input_list len *output_list len
                call {set_length}   // _ *input_list len *output_list
                swap 1              // _ *input_list *output_list input_len

                push 0 push 0       // _ *input_list *output_list input_len 0 0
                call {main_loop}    // _ *input_list *output_list input_len input_len output_len

                swap 2 pop 2        // _ *input_list *output_list output_len
                call {set_length}   // _input_list *output_list

                swap 1              // _ *output_list *input_list
                pop 1               // _ *output_list
                return

            // INVARIANT:  _ *input_list *output_list input_len input_index output_index
            {main_loop}:
                // test return condition
                dup 1 // _ *input_list *output_list input_len input_index output_index input_index
                dup 3 eq // _ *input_list *output_list input_len input_index output_index input_index==input_len

                skiz return
                // _ *input_list *output_list input_len input_index output_index

                // body

                // read
                dup 4 // _ *input_list *output_list input_len input_index output_index *input_list
                dup 2 // _ *input_list *output_list input_len input_index output_index *input_list input_index
                call {list_get} // _ *input_list *output_list input_len input_index output_index [input_elements]

                // map
                call {inner_function_name} // _ *input_list *output_list input_len input_index output_index b

                // write
                skiz call {main_write} //_ *input_list *output_list input_len input_index output_index*

                // _ *input_list *output_list input_len input_index output_index*
                swap 1 push 1 add swap 1 // _ *input_list *output_list input_len input_index+1 output_index*
                recurse

            // BEFORE: _ *input_list *output_list input_len input_index output_index
            // AFTER: _ *input_list *output_list input_len input_index output_index+1
            {main_write}:
                // calculate read address
                dup 4                        // _ *input_list *output_list input_len input_index output_index *input_list
                push {safety_offset} add     // _ *input_list *output_list input_len input_index output_index address
                dup 2                        // _ *input_list *output_list input_len input_index output_index address input_index
                push {element_size} mul add  // _ *input_list *output_list input_len input_index output_index read_source

                // calculate write address
                dup 4                        // _ *input_list *output_list input_len input_index output_index read_source *output_list
                push {safety_offset} add     // _ *input_list *output_list input_len input_index output_index read_source address
                dup 2                        // _ *input_list *output_list input_len input_index output_index read_source address output_index
                push {element_size} mul add  // _ *input_list *output_list input_len input_index output_index read_source write_dest

                // calculate number of words
                push {element_size}          // _ *input_list *output_list input_len input_index output_index read_source write_dest num_words

                // copy memory
                call {memcpy}                // _ *input_list *output_list input_len input_index output_index

                // bookkeeping
                push 1 add                   // _ *input_list *output_list input_len input_index output_index+1

                return

            {maybe_inner_function_body_raw}
        )
    }
}

impl Function for Filter {
    fn rust_shadow(
        &self,
        stack: &mut Vec<BFieldElement>,
        memory: &mut HashMap<BFieldElement, BFieldElement>,
    ) {
        let input_type = self.f.domain();
        let output_type = self.f.range();

        let element_size = self.f.domain().stack_size();
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
                list::safeimplu32::new::SafeNew {
                    data_type: input_type.clone(),
                }
                .rust_shadowing(stack, vec![], vec![], memory);
                stack.pop().unwrap()
            }
            ListType::Unsafe => {
                stack.push(BFieldElement::new(output_list_capacity as u64));
                list::unsafeimplu32::new::UnsafeNew {
                    data_type: input_type.clone(),
                }
                .rust_shadowing(stack, vec![], vec![], memory);
                stack.pop().unwrap()
            }
        };

        // set length
        stack.push(output_list);
        stack.push(BFieldElement::new(len as u64));
        match self.list_type {
            ListType::Safe => {
                list::safeimplu32::set_length::SafeSetLength {
                    data_type: output_type,
                }
                .rust_shadowing(stack, vec![], vec![], memory);
            }
            ListType::Unsafe => {
                list::unsafeimplu32::set_length::UnsafeSetLength {
                    data_type: output_type,
                }
                .rust_shadowing(stack, vec![], vec![], memory);
            }
        }
        stack.pop();

        // forall elements, read + map + maybe copy
        let mut output_index = 0;
        for i in 0..len {
            // read
            let mut input_item = get_element(list_pointer, i, memory, input_type.stack_size());

            // put on stack
            while let Some(element) = input_item.pop() {
                stack.push(element);
            }

            self.f.apply(stack, memory);

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
                memcpy(&MemCpy, stack, vec![], vec![], memory);
                output_index += 1;
            }
        }

        // set length
        stack.push(output_list);
        stack.push(BFieldElement::new(output_index as u64));
        match self.list_type {
            ListType::Safe => {
                list::safeimplu32::set_length::SafeSetLength {
                    data_type: input_type,
                }
                .rust_shadowing(stack, vec![], vec![], memory);
            }
            ListType::Unsafe => {
                list::unsafeimplu32::set_length::UnsafeSetLength {
                    data_type: input_type,
                }
                .rust_shadowing(stack, vec![], vec![], memory);
            }
        }
    }

    fn pseudorandom_initial_state(
        &self,
        seed: [u8; 32],
        _bench_case: Option<crate::snippet_bencher::BenchmarkCase>,
    ) -> (Vec<BFieldElement>, HashMap<BFieldElement, BFieldElement>) {
        let mut rng: StdRng = SeedableRng::from_seed(seed);
        let list_pointer: u64 = rng.gen_range(0..(1 << 20));
        let list_pointer = BFieldElement::new(list_pointer);

        let log_2_list_length: usize = rng.gen_range(0..4);
        let list_length = 1 << log_2_list_length;

        let input_type = self.f.domain();
        let input_type_size = input_type.stack_size();
        println!(
            "generating list; length: {list_length}, \
            type size: {input_type_size}, \
            address: {list_pointer}"
        );
        let safety_offset = self.list_type.metadata_size();
        let last_element_index = safety_offset + list_length * input_type_size;
        let last_element_index = list_pointer + BFieldElement::new(last_element_index as u64);

        let mut memory = HashMap::default();
        memory.insert(BFieldElement::zero(), last_element_index);

        let capacity = list_length;
        memory.insert(list_pointer, BFieldElement::new(capacity as u64));
        if matches!(self.list_type, ListType::Safe) {
            let length_pointer = list_pointer + BFieldElement::new(1);
            memory.insert(length_pointer, BFieldElement::new(list_length as u64));
        }

        for i in 0..list_length {
            for j in 0..input_type_size {
                let element_offset = (safety_offset + i * input_type_size + j) as u64;
                memory.insert(list_pointer + BFieldElement::new(element_offset), rng.gen());
            }
        }

        let stack = [empty_stack(), vec![list_pointer]].concat();

        (stack, memory)
    }
}

#[cfg(test)]
mod tests {
    use crate::traits::rust_shadow::RustShadow;
    use crate::{
        arithmetic,
        list::higher_order::inner_function::RawCode,
        traits::{deprecated_snippet::DeprecatedSnippet, function::ShadowedFunction},
        ExecutionState, VmHasher,
    };
    use triton_vm::triton_asm;
    use twenty_first::{
        shared_math::other::random_elements, util_types::algebraic_hasher::AlgebraicHasher,
    };

    use super::*;

    #[derive(Debug, Clone)]
    pub struct TestHashXFieldElementLsb;

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
            vec![DataType::Xfe]
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
            let unused_import = library.import(Box::new(arithmetic::u32::safeadd::Safeadd));
            format!(
            "
    // BEFORE: _ x2 x1 x0
    // AFTER: _ b
    {entrypoint}:
        // Useless additions, to ensure that dependencies are accepted inside the filter generated code
            push 0
            push 0
            call {unused_import}
            pop 1

        push 0
        push 0
        push 0
        push 1 // _ x2 x1 x0 0 0 0 1
        push 0 swap 7 // _ 0 x1 x0 0 0 0 1 x2
        push 0 swap 7 // _ 0 0 x0 0 0 0 1 x2 x1
        push 0 swap 7 // _ 0 0 0 0 0 0 1 x2 x1 x0

        sponge_init
        sponge_absorb
        sponge_squeeze  // _ d9 d8 d7 d6 d5 d4 d3 d2 d1 d0
        swap 5 pop 1    // _ d9 d8 d7 d6 d0 d4 d3 d2 d1
        swap 5 pop 1    // _ d9 d8 d7 d1 d0 d4 d3 d2
        swap 5 pop 1
        swap 5 pop 1
        swap 5 pop 1

        // _ d4 d3 d2 d1 d0

        split // _ d4 d3 d2 d1 hi lo
        push 2 // _ d4 d3 d2 d1 hi lo 2
        swap 1
        div_mod // _ d4 d3 d2 d1 hi q r
        swap 6
        pop 5 pop 1
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
                [
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
                [
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
                [
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

    #[test]
    fn unsafe_list_prop_test() {
        ShadowedFunction::new(Filter {
            list_type: ListType::Unsafe,
            f: InnerFunction::DeprecatedSnippet(Box::new(TestHashXFieldElementLsb)),
        })
        .test();
    }

    #[test]
    fn with_safe_list_prop_test() {
        ShadowedFunction::new(Filter {
            list_type: ListType::Safe,
            f: InnerFunction::DeprecatedSnippet(Box::new(TestHashXFieldElementLsb)),
        })
        .test();
    }

    #[test]
    fn test_with_raw_function_lsb_on_bfe() {
        let rawcode = RawCode::new(
            triton_asm!(
                lsb_bfe:
                    split   // _ hi lo
                    push 2  // _ hi lo 2
                    swap 1  // _ hi 2 lo
                    div_mod // _ hi q r
                    swap 2  // _ r q hi
                    pop 2   // _ r
                    return
            ),
            DataType::Bfe,
            DataType::Bool,
        );
        ShadowedFunction::new(Filter {
            list_type: ListType::Unsafe,
            f: InnerFunction::RawCode(rawcode),
        })
        .test();
    }

    #[test]
    fn test_with_raw_function_lsb_on_xfe() {
        let rawcode = RawCode::new(
            triton_asm!(
                lsb_xfe:
                    split   // _ x2 x1 hi lo
                    push 2  // _ x2 x1 hi lo 2
                    swap 1  // _ x2 x1 hi 2 lo
                    div_mod // _ x2 x1 hi q r
                    swap 4  // _ r x1 q hi x2
                    pop 4   // _ r
                    return
            ),
            DataType::Xfe,
            DataType::Bool,
        );
        ShadowedFunction::new(Filter {
            list_type: ListType::Unsafe,
            f: InnerFunction::RawCode(rawcode),
        })
        .test();
    }
}

#[cfg(test)]
mod benches {
    use super::{tests::TestHashXFieldElementLsb, *};
    use crate::traits::function::ShadowedFunction;
    use crate::traits::rust_shadow::RustShadow;

    #[test]
    fn unsafe_list_filter_benchmark() {
        ShadowedFunction::new(Filter {
            list_type: ListType::Unsafe,
            f: InnerFunction::DeprecatedSnippet(Box::new(TestHashXFieldElementLsb)),
        })
        .bench();
    }

    #[test]
    fn safe_list_filter_benchmark() {
        ShadowedFunction::new(Filter {
            list_type: ListType::Safe,
            f: InnerFunction::DeprecatedSnippet(Box::new(TestHashXFieldElementLsb)),
        })
        .bench();
    }
}
