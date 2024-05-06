use std::collections::HashMap;

use itertools::Itertools;
use num_traits::One;
use rand::rngs::StdRng;
use rand::Rng;
use rand::SeedableRng;
use triton_vm::parser::tokenize;
use triton_vm::prelude::*;

use crate::data_type::DataType;
use crate::empty_stack;
use crate::library::Library;
use crate::list::new::New;
use crate::list::set_length::SetLength;
use crate::rust_shadowing_helper_functions::dyn_malloc::dynamic_allocator;
use crate::rust_shadowing_helper_functions::list::insert_random_list;
use crate::rust_shadowing_helper_functions::list::list_get;
use crate::rust_shadowing_helper_functions::list::list_get_length;
use crate::rust_shadowing_helper_functions::list::list_set;
use crate::traits::basic_snippet::BasicSnippet;
use crate::traits::deprecated_snippet::DeprecatedSnippet;
use crate::traits::function::Function;
use crate::traits::function::FunctionInitialState;
use crate::InitVmState;

use super::inner_function::InnerFunction;

const MORE_THAN_ONE_INPUT_OR_OUTPUT_TYPE_IN_INNER_FUNCTION: &str = "inner function in `map` \
currently only works with *one* input element. Use a tuple data type to circumvent this.";

/// Applies a given function to every element of a list, and collects the new elements
/// into a new list.
pub struct Map {
    pub f: InnerFunction,
}

impl Map {
    pub fn new(f: InnerFunction) -> Self {
        Self { f }
    }
}

impl BasicSnippet for Map {
    fn inputs(&self) -> Vec<(DataType, String)> {
        match &self.f {
            InnerFunction::BasicSnippet(bs) => {
                assert!(
                    bs.inputs().len().is_one(),
                    "{MORE_THAN_ONE_INPUT_OR_OUTPUT_TYPE_IN_INNER_FUNCTION}"
                );
                let element_type = &bs.inputs()[0].0;
                vec![(
                    DataType::List(Box::new(element_type.clone())),
                    "*input_list".to_string(),
                )]
            }
            _ => vec![(
                DataType::List(Box::new(DataType::VoidPointer)),
                "*input_list".to_string(),
            )],
        }
    }

    fn outputs(&self) -> Vec<(DataType, String)> {
        match &self.f {
            InnerFunction::BasicSnippet(bs) => {
                assert!(
                    bs.inputs().len().is_one(),
                    "{MORE_THAN_ONE_INPUT_OR_OUTPUT_TYPE_IN_INNER_FUNCTION}"
                );
                let element_type = &bs.outputs()[0].0;
                vec![(
                    DataType::List(Box::new(element_type.clone())),
                    "*output_list".to_string(),
                )]
            }
            _ => vec![(
                DataType::List(Box::new(DataType::VoidPointer)),
                "*output_list".to_string(),
            )],
        }
    }

    fn entrypoint(&self) -> String {
        format!("tasmlib_list_higher_order_u32_map_{}", self.f.entrypoint())
    }

    fn code(&self, library: &mut Library) -> Vec<LabelledInstruction> {
        let input_type = self.f.domain();
        let output_type = self.f.range();
        let new_list = library.import(Box::new(New::new(output_type.clone())));

        // Declare the code to call the function argument
        let (call_inner_function, maybe_inner_function_body_raw) = match &self.f {
            InnerFunction::RawCode(rc) => {
                if let Some(inlined_body) = rc.inlined_body() {
                    // Raw functions that can be inlined are inlined to save two clock cycle counts
                    // for each iteration.
                    (inlined_body, String::default())
                } else {
                    // If function was supplied as raw instructions and cannot be inlined, we need
                    // to append the inner function to the function body. Otherwise, `library`
                    // handles the imports.
                    (
                        triton_asm!(
                            call {rc.entrypoint()}
                        ),
                        rc.function.iter().map(|x| x.to_string()).join("\n"),
                    )
                }
            }
            InnerFunction::DeprecatedSnippet(sn) => {
                assert!(
                    sn.input_types().len().is_one(),
                    "{MORE_THAN_ONE_INPUT_OR_OUTPUT_TYPE_IN_INNER_FUNCTION}"
                );
                let fn_body = sn.function_code(library);
                let (_, instructions) = tokenize(&fn_body).unwrap();
                let labelled_instructions =
                    triton_vm::parser::to_labelled_instructions(&instructions);
                let snippet_name =
                    library.explicit_import(&sn.entrypoint_name(), &labelled_instructions);
                (triton_asm!(call { snippet_name }), String::default())
            }
            InnerFunction::NoFunctionBody(lnat) => {
                let snippet_name = lnat.label_name.to_owned();
                (triton_asm!(call { snippet_name }), String::default())
            }
            InnerFunction::BasicSnippet(bs) => {
                assert!(
                    bs.inputs().len().is_one(),
                    "{MORE_THAN_ONE_INPUT_OR_OUTPUT_TYPE_IN_INNER_FUNCTION}"
                );
                let labelled_instructions = bs.annotated_code(library);
                let snippet_name =
                    library.explicit_import(&bs.entrypoint(), &labelled_instructions);
                (triton_asm!(call { snippet_name }), String::default())
            }
        };

        let entrypoint = self.entrypoint();
        let main_loop = format!("{entrypoint}_loop");
        let read_from_input_list = input_type.read_value_from_memory_leave_pointer();
        let write_to_output_list = output_type.write_value_to_memory_leave_pointer();
        let input_elem_size = input_type.stack_size();
        let output_elem_size = output_type.stack_size();
        let input_elem_size_plus_one = input_elem_size + 1;
        let output_elem_size_plus_one = output_type.stack_size() + 1;
        let minus_two_times_output_size = -(output_type.stack_size() as i32 * 2);

        let mul_elem_size = |n| match n {
            0 => triton_asm!(pop 1 push 0),
            1 => triton_asm!(),
            n => triton_asm!(
                push {n}
                mul
            ),
        };

        let adjust_output_list_pointer = match output_elem_size {
            0 => triton_asm!(),
            1 => triton_asm!(),
            n => triton_asm!(
                push {-(n as i32 - 1)}
                add
            ),
        };

        let final_output_list_pointer_adjust = match output_elem_size {
            0 => triton_asm!(),
            1 => triton_asm!(),
            n => triton_asm!(
                push {n - 1}
                add
            ),
        };

        triton_asm!(
            // BEFORE: _ <[additional_input_args]>  *input_list
            // AFTER:  _ <[additional_input_args]>  *output_list
            {entrypoint}:
                dup 0
                read_mem 1
                pop 1
                // _ <aia> *input_list len

                dup 1
                dup 1
                // _ <aia> *input_list len *input_list len

                {&mul_elem_size(input_elem_size)}
                add
                // _ <aia> *input_list len *input_list_last_word

                call {new_list}
                // _ <aia> *input_list len *input_list_last_word *output_list

                // Write output list's length
                dup 2
                dup 1
                write_mem 1
                pop 1

                dup 2
                {&mul_elem_size(output_elem_size)}
                add
                // _ <aia> *input_list len *input_list_last_word *output_list_last_word_last_element

                {&adjust_output_list_pointer}
                // _ <aia> *input_list len *input_list_last_word *output_list_first_word_last_element

                swap 2
                pop 1
                // _ <aia> *input_list *output_list_first_word_last_element *input_list_last_word

                call {main_loop}

                pop 1
                {&final_output_list_pointer_adjust}

                swap 1
                pop 1


                return

            // INVARIANT: _ <aia>  *end_condition_input_list *output_elem *input_elem
            {main_loop}:
                // test return condition
                dup 2
                dup 1
                eq
                skiz
                    return

                dup 0
                {&read_from_input_list}
                // _ <aia>  *end_condition_input_list *output_elem *input_elem [input_elem] *prev_input_elem

                swap {input_elem_size_plus_one}
                pop 1
                // _ <aia>  *end_condition_input_list *output_elem *prev_input_elem [input_elem]

                // map
                {&call_inner_function}
                                // _ <aia>  *end_condition_input_list *output_elem *prev_input_elem [output_elem]

                // write
                dup {output_elem_size_plus_one}
                // _ <aia>  *end_condition_input_list *output_elem *prev_input_elem [output_elem] *output_elem

                {&write_to_output_list}
                // _ <aia>  *end_condition_input_list *output_elem *prev_input_elem *next_output_elem

                push {minus_two_times_output_size}
                add
                // _ <aia>  *end_condition_input_list *output_elem *prev_input_elem *prev_output_elem

                swap 2
                pop 1

                recurse

            {maybe_inner_function_body_raw}
        )
    }
}

impl Function for Map {
    fn rust_shadow(
        &self,
        stack: &mut Vec<BFieldElement>,
        memory: &mut HashMap<BFieldElement, BFieldElement>,
    ) {
        let input_list_element_type = self.f.domain();
        let output_type = self.f.range();
        let input_elem_stack_size = input_list_element_type.stack_size();

        // stack: _ *input_list
        let &list_pointer = stack.last().unwrap();

        New::new(input_list_element_type).rust_shadowing(stack, vec![], vec![], memory);
        // stack: _ *input_list *output_list

        let list_len = list_get_length(list_pointer, memory);
        let list_length = BFieldElement::new(list_len as u64);
        stack.push(list_length);
        // stack: _ *input_list *output_list list_len
        SetLength::new(output_type.clone()).rust_shadowing(stack, vec![], vec![], memory);
        // stack: _ *input_list *output_list

        let &output_list = stack.last().unwrap();
        stack.push(list_length);

        // for all elements, read + map + write
        // stack: _ *input_list *output_list list_len
        for i in (0..list_len).rev() {
            let input_item = list_get(list_pointer, i, memory, input_elem_stack_size);

            for word in input_item.into_iter().rev() {
                stack.push(word);
            }

            self.f.apply(stack, memory);

            let output_item = (0..output_type.stack_size())
                .map(|_| stack.pop().unwrap())
                .collect();
            list_set(output_list, i, output_item, memory);
        }

        let _item_index = stack.pop().unwrap();
        let _output_list = stack.pop().unwrap();
        let _input_list = stack.pop().unwrap();

        stack.push(output_list);
    }

    fn pseudorandom_initial_state(
        &self,
        seed: [u8; 32],
        _bench_case: Option<crate::snippet_bencher::BenchmarkCase>,
    ) -> FunctionInitialState {
        let mut rng: StdRng = SeedableRng::from_seed(seed);
        let list_length = rng.gen_range(0..(1 << 6));

        // Autogenerating these extra arguments seems pretty shady to me. Are they
        // u32s, BFEs, or XFEs? That depends on the inner function!
        let num_additional_function_args = rng.gen_range(0..7);
        let additional_function_args = (0..num_additional_function_args)
            .map(|_| BFieldElement::new(rng.gen_range(0..(1 << 16))))
            .collect_vec();
        let execution_state = self.generate_input_state(list_length, additional_function_args);
        FunctionInitialState {
            stack: execution_state.stack,
            memory: execution_state.nondeterminism.ram,
        }
    }
}

impl Map {
    fn generate_input_state(
        &self,
        list_length: usize,
        additional_function_args: Vec<BFieldElement>,
    ) -> InitVmState {
        let mut stack = empty_stack();

        // Add additional input args to stack, if they exist
        for additional_function_arg in additional_function_args.into_iter().rev() {
            stack.push(additional_function_arg);
        }

        let mut memory = HashMap::default();
        let input_element_type = self.f.domain();
        let list_pointer = dynamic_allocator(&mut memory);
        insert_random_list(&input_element_type, list_pointer, list_length, &mut memory);

        stack.push(list_pointer);

        InitVmState::with_stack_and_memory(stack, memory)
    }
}

#[cfg(test)]
mod tests {
    use num_traits::Zero;
    use triton_vm::twenty_first::prelude::AlgebraicHasher;

    use crate::arithmetic;
    use crate::list::higher_order::inner_function::RawCode;
    use crate::neptune::mutator_set::get_swbf_indices::u32_to_u128_add_another_u128;
    use crate::traits::deprecated_snippet::DeprecatedSnippet;
    use crate::traits::function::ShadowedFunction;
    use crate::traits::rust_shadow::RustShadow;
    use crate::twenty_first::math::other::random_elements;
    use crate::VmHasher;

    use super::*;

    #[derive(Debug, Clone)]
    pub(crate) struct TestHashXFieldElement;

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
            vec![DataType::Xfe]
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

        fn output_types(&self) -> Vec<DataType> {
            vec![DataType::Digest]
        }

        fn stack_diff(&self) -> isize
        where
            Self: Sized,
        {
            2
        }

        fn function_code(&self, library: &mut Library) -> String {
            let entrypoint = self.entrypoint_name();
            let unused_import = library.import(Box::new(arithmetic::u32::safeadd::Safeadd));
            format!(
                "
        // BEFORE: _ x2 x1 x0
        // AFTER:  _ d4 d3 d2 d1 d0
        {entrypoint}:
            push 0
            push 0
            push 0
            push 1 // _ x2 x1 x0 0 0 0 1
            push 0 swap 7 // _ 0 x1 x0 0 0 0 1 x2
            push 0 swap 7 // _ 0 0 x0 0 0 0 1 x2 x1
            push 0 swap 7 // _ 0 0 0 0 0 0 1 x2 x1 x0

            // Useless additions, to ensure that imports are accepted inside the map generated code
            push 0
            push 0
            call {unused_import}
            pop 1

            sponge_init
            sponge_absorb
            sponge_squeeze // _ d9 d8 d7 d6 d5 d4 d3 d2 d1 d0
            swap 5 pop 1   // _ d9 d8 d7 d6 d0 d4 d3 d2 d1
            swap 5 pop 1   // _ d9 d8 d7 d1 d0 d4 d3 d2
            swap 5 pop 1
            swap 5 pop 1
            swap 5 pop 1
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
            vec![InitVmState::with_stack(
                [
                    vec![BFieldElement::zero(); 16],
                    random_elements::<BFieldElement>(3),
                ]
                .concat(),
            )]
        }

        fn common_case_input_state(&self) -> InitVmState
        where
            Self: Sized,
        {
            InitVmState::with_stack(
                [
                    vec![BFieldElement::zero(); 16],
                    random_elements::<BFieldElement>(3),
                ]
                .concat(),
            )
        }

        fn worst_case_input_state(&self) -> InitVmState
        where
            Self: Sized,
        {
            InitVmState::with_stack(
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
            let mut digest = VmHasher::hash_varlen(&xfield_element).values().to_vec();
            while let Some(element) = digest.pop() {
                stack.push(element);
            }
        }
    }

    #[test]
    fn prop_test() {
        let snippet = Map::new(InnerFunction::DeprecatedSnippet(Box::new(
            TestHashXFieldElement,
        )));
        ShadowedFunction::new(snippet).test();
    }

    #[test]
    fn test_with_raw_function_identity_on_bfe() {
        let rawcode = RawCode::new(
            triton_asm!(identity_bfe: return),
            DataType::Bfe,
            DataType::Bfe,
        );
        let snippet = Map::new(InnerFunction::RawCode(rawcode));
        ShadowedFunction::new(snippet).test();
    }

    #[test]
    fn test_with_raw_function_bfe_lift() {
        let rawcode = RawCode::new(
            triton_asm!(bfe_lift: push 0 push 0 swap 2 return),
            DataType::Bfe,
            DataType::Xfe,
        );
        let snippet = Map::new(InnerFunction::RawCode(rawcode));
        ShadowedFunction::new(snippet).test();
    }

    #[test]
    fn test_with_raw_function_xfe_get_coeff_0() {
        let rawcode = RawCode::new(
            triton_asm!(get_0: swap 2 pop 2 return),
            DataType::Xfe,
            DataType::Bfe,
        );
        let snippet = Map::new(InnerFunction::RawCode(rawcode));
        ShadowedFunction::new(snippet).test();
    }

    #[test]
    fn test_with_raw_function_square_on_bfe() {
        let rawcode = RawCode::new(
            triton_asm!(square_bfe: dup 0 mul return),
            DataType::Bfe,
            DataType::Bfe,
        );
        let snippet = Map::new(InnerFunction::RawCode(rawcode));
        ShadowedFunction::new(snippet).test();
    }

    #[test]
    fn test_with_raw_function_square_plus_n_on_bfe() {
        // Inner function calculates `|x| -> x*x + n`, where `x` is the list element, and `n` is the
        // same value for all elements.
        let rawcode = RawCode::new(
            triton_asm!(square_plus_n_bfe: dup 0 mul dup 4 add return),
            DataType::Bfe,
            DataType::Bfe,
        );
        let snippet = Map::new(InnerFunction::RawCode(rawcode));
        ShadowedFunction::new(snippet).test();
    }

    #[test]
    fn test_with_raw_function_square_on_xfe() {
        let rawcode = RawCode::new(
            triton_asm!(
                square_xfe: dup 2 dup 2 dup 2 xx_mul return
            ),
            DataType::Xfe,
            DataType::Xfe,
        );
        let snippet = Map::new(InnerFunction::RawCode(rawcode));
        ShadowedFunction::new(snippet).test();
    }

    #[test]
    fn test_with_raw_function_xfe_to_digest() {
        let rawcode = RawCode::new(
            triton_asm!(
                xfe_to_digest: push 0 push 0 return
            ),
            DataType::Xfe,
            DataType::Digest,
        );
        let snippet = Map::new(InnerFunction::RawCode(rawcode));
        ShadowedFunction::new(snippet).test();
    }

    #[test]
    fn test_with_raw_function_digest_to_xfe() {
        let rawcode = RawCode::new(
            triton_asm!(
                xfe_to_digest: pop 2 return
            ),
            DataType::Digest,
            DataType::Xfe,
        );
        let snippet = Map::new(InnerFunction::RawCode(rawcode));
        ShadowedFunction::new(snippet).test();
    }

    #[test]
    fn test_with_raw_function_square_on_xfe_plus_another_xfe() {
        let rawcode = RawCode::new(
            triton_asm!(
                square_xfe_plus_another_xfe:
                    dup 2 dup 2 dup 2 xx_mul
                    dup 8 dup 8 dup 8 xx_add
                    return
            ),
            DataType::Xfe,
            DataType::Xfe,
        );
        let snippet = Map::new(InnerFunction::RawCode(rawcode));
        ShadowedFunction::new(snippet).test()
    }

    #[test]
    fn test_u32_list_to_unit_list() {
        let remove_elements = RawCode::new(
            triton_asm!(remove_elements: pop 1 return),
            DataType::U32,
            DataType::Tuple(vec![]),
        );
        ShadowedFunction::new(Map::new(InnerFunction::RawCode(remove_elements))).test();
    }

    #[test]
    fn test_u32_list_to_u64_list() {
        let duplicate_u32 = RawCode::new(
            triton_asm!(duplicate_u32: dup 0 return),
            DataType::U32,
            DataType::U64,
        );
        ShadowedFunction::new(Map::new(InnerFunction::RawCode(duplicate_u32))).test();
    }

    #[test]
    fn test_u32_list_to_u128_list_plus_x() {
        ShadowedFunction::new(Map::new(InnerFunction::RawCode(
            u32_to_u128_add_another_u128(),
        )))
        .test();
    }
}

#[cfg(test)]
mod benches {
    use crate::traits::function::ShadowedFunction;
    use crate::traits::rust_shadow::RustShadow;

    use super::tests::TestHashXFieldElement;
    use super::*;

    #[test]
    fn map_benchmark() {
        ShadowedFunction::new(Map {
            f: InnerFunction::DeprecatedSnippet(Box::new(TestHashXFieldElement)),
        })
        .bench();
    }
}
