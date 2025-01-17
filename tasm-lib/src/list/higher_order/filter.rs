use itertools::Itertools;
use triton_vm::isa::parser::tokenize;
use triton_vm::prelude::*;

use super::inner_function::InnerFunction;
use crate::list::get::Get;
use crate::list::length::Length;
use crate::list::new::New;
use crate::list::set_length::SetLength;
use crate::list::LIST_METADATA_SIZE;
use crate::memory::memcpy::MemCpy;
use crate::prelude::*;

/// Filters a given list for elements that satisfy a predicate. A new
/// list is created, containing only those elements that satisfy the
/// predicate. The predicate must be given as an [`InnerFunction`].
pub struct Filter {
    pub f: InnerFunction,
}

impl BasicSnippet for Filter {
    fn inputs(&self) -> Vec<(DataType, String)> {
        let element_type = self.f.domain();
        let list_type = DataType::List(Box::new(element_type));
        vec![(list_type, "*input_list".to_string())]
    }

    fn outputs(&self) -> Vec<(DataType, String)> {
        let element_type = self.f.range();
        let list_type = DataType::List(Box::new(element_type));
        vec![(list_type, "*output_list".to_string())]
    }

    fn entrypoint(&self) -> String {
        format!(
            "tasmlib_list_higher_order_u32_filter_{}",
            self.f.entrypoint()
        )
    }

    fn code(&self, library: &mut Library) -> Vec<LabelledInstruction> {
        let input_type = self.f.domain();
        let output_type = self.f.range();
        assert_eq!(output_type, DataType::Bool);

        let safety_offset = LIST_METADATA_SIZE;
        let get_length = library.import(Box::new(Length::new(input_type.clone())));
        let list_get = library.import(Box::new(Get::new(input_type)));
        let new_list = library.import(Box::new(New::new(output_type.clone())));
        let set_length = library.import(Box::new(SetLength::new(output_type)));
        let element_size = self.f.domain().stack_size();

        let inner_function_name = match &self.f {
            InnerFunction::RawCode(rc) => rc.entrypoint(),
            InnerFunction::DeprecatedSnippet(sn) => {
                let fn_body = sn.function_code(library);
                let (_, instructions) = tokenize(&fn_body).unwrap();
                let labelled_instructions = isa::parser::to_labelled_instructions(&instructions);
                library.explicit_import(&sn.entrypoint_name(), &labelled_instructions)
            }
            InnerFunction::NoFunctionBody(_) => todo!(),
            InnerFunction::BasicSnippet(bs) => {
                let labelled_instructions = bs.annotated_code(library);
                library.explicit_import(&bs.entrypoint(), &labelled_instructions)
            }
        };

        let memcpy = library.import(Box::new(MemCpy));

        // If function was supplied as raw instructions, we need to append the inner function to the function
        // body. Otherwise, `library` handles the imports.
        let maybe_inner_function_body_raw = match &self.f {
            InnerFunction::RawCode(rc) => rc.function.iter().join("\n"),
            InnerFunction::DeprecatedSnippet(_) => String::default(),
            InnerFunction::NoFunctionBody(_) => todo!(),
            InnerFunction::BasicSnippet(_) => String::default(),
        };
        let entrypoint = self.entrypoint();
        let main_loop = format!("{entrypoint}_loop");
        let main_write = format!("{entrypoint}_write");

        triton_asm!(
            // BEFORE: _ *input_list
            // AFTER:  _ *output_list
            {entrypoint}:
                dup 0               // _ *input_list *input_list
                call {get_length}   // _ *input_list len
                call {new_list}     // _ *input_list len *output_list
                dup 1               // _ *input_list len *output_list len
                call {set_length}   // _ *input_list len *output_list
                swap 1              // _ *input_list *output_list input_len

                push 0 push 0       // _ *input_list *output_list input_len 0 0
                call {main_loop}    // _ *input_list *output_list input_len input_len output_len

                swap 2 pop 2        // _ *input_list *output_list output_len
                call {set_length}   // _ *input_list *output_list

                swap 1              // _ *output_list *input_list
                pop 1               // _ *output_list
                return

            // INVARIANT:  _ *input_list *output_list input_len input_index output_index
            {main_loop}:
                // test return condition
                dup 1    // _ *input_list *output_list input_len input_index output_index input_index
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
            // AFTER:  _ *input_list *output_list input_len input_index output_index+1
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

#[cfg(test)]
mod tests {
    use num::Zero;

    use super::*;
    use crate::arithmetic;
    use crate::empty_stack;
    use crate::list::higher_order::inner_function::RawCode;
    use crate::rust_shadowing_helper_functions;
    use crate::test_prelude::*;
    use crate::traits::deprecated_snippet::DeprecatedSnippet;

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
            let safety_offset = LIST_METADATA_SIZE;

            let list_pointer = stack.pop().unwrap();

            // get list length
            let len = rust_shadowing_helper_functions::list::list_get_length(list_pointer, memory);

            let get_element = rust_shadowing_helper_functions::list::list_get;

            New::new(input_type.clone()).rust_shadow(stack, memory);
            let output_list = stack.pop().unwrap();

            // set length
            stack.push(output_list);
            stack.push(BFieldElement::new(len as u64));
            SetLength::new(output_type).rust_shadowing(stack, vec![], vec![], memory);
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
                            + BFieldElement::new(
                                safety_offset as u64 + i as u64 * element_size as u64,
                            ),
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
            SetLength::new(input_type).rust_shadowing(stack, vec![], vec![], memory);
        }

        fn pseudorandom_initial_state(
            &self,
            seed: [u8; 32],
            _: Option<BenchmarkCase>,
        ) -> FunctionInitialState {
            let mut rng = StdRng::from_seed(seed);
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
            let safety_offset = LIST_METADATA_SIZE;
            let last_element_index = safety_offset + list_length * input_type_size;
            let last_element_index = list_pointer + BFieldElement::new(last_element_index as u64);

            let mut memory = HashMap::default();
            memory.insert(BFieldElement::zero(), last_element_index);

            let capacity = list_length;
            memory.insert(list_pointer, BFieldElement::new(capacity as u64));

            for i in 0..list_length {
                for j in 0..input_type_size {
                    let element_offset = (safety_offset + i * input_type_size + j) as u64;
                    memory.insert(list_pointer + BFieldElement::new(element_offset), rng.gen());
                }
            }

            let stack = [empty_stack(), vec![list_pointer]].concat();

            FunctionInitialState { stack, memory }
        }
    }

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

        fn output_field_names(&self) -> Vec<String>
        where
            Self: Sized,
        {
            vec!["bool".to_string()]
        }

        fn output_types(&self) -> Vec<DataType> {
            vec![DataType::Bool]
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
    // AFTER:  _ b
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

        fn gen_input_states(&self) -> Vec<InitVmState>
        where
            Self: Sized,
        {
            let mut stack = empty_stack();
            stack.extend(random::<[BFieldElement; 3]>());

            vec![InitVmState::with_stack(stack)]
        }

        fn common_case_input_state(&self) -> InitVmState
        where
            Self: Sized,
        {
            let mut stack = empty_stack();
            stack.extend(random::<[BFieldElement; 3]>());

            InitVmState::with_stack(stack)
        }

        fn worst_case_input_state(&self) -> InitVmState
        where
            Self: Sized,
        {
            let mut stack = empty_stack();
            stack.extend(random::<[BFieldElement; 3]>());

            InitVmState::with_stack(stack)
        }

        fn rust_shadowing(
            &self,
            stack: &mut Vec<BFieldElement>,
            _: Vec<BFieldElement>,
            _: Vec<BFieldElement>,
            _: &mut HashMap<BFieldElement, BFieldElement>,
        ) where
            Self: Sized,
        {
            let mut xfield_element = vec![];
            for _ in 0..3 {
                xfield_element.push(stack.pop().unwrap());
            }
            let digest = Tip5::hash_varlen(&xfield_element).values().to_vec();
            let b = digest[0].value() % 2;
            stack.push(BFieldElement::new(b));
        }
    }

    #[test]
    fn prop_test() {
        ShadowedFunction::new(Filter {
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
            f: InnerFunction::RawCode(rawcode),
        })
        .test();
    }
}

#[cfg(test)]
mod benches {
    use super::tests::TestHashXFieldElementLsb;
    use super::*;
    use crate::test_prelude::*;

    #[test]
    fn benchmark() {
        ShadowedFunction::new(Filter {
            f: InnerFunction::DeprecatedSnippet(Box::new(TestHashXFieldElementLsb)),
        })
        .bench();
    }
}
