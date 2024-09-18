use itertools::Itertools;
use tasm_lib::list::higher_order::inner_function::InnerFunction;
use triton_vm::isa;
use triton_vm::isa::parser::tokenize;
use triton_vm::isa::triton_asm;
use triton_vm::prelude::LabelledInstruction;

use crate::data_type::DataType;
use crate::library::Library;
use crate::list::new::New;
use crate::prelude::BasicSnippet;

const INNER_FN_INCORRECT_NUM_INPUTS: &str = "Inner function in `map` only works with *one* input. \
                                             Use a tuple as a workaround.";

/// Applies a given function to every element of a list, and collects the new
/// elements into a new list.
///
/// Mapping over multiple input lists into one output list, effectively chaining
/// inputs before applying the map, is possible with [`ChainMap`]. See there for
/// extended documentation.
pub type Map = ChainMap<1>;

/// Applies a given function to every element of all given lists, collecting the
/// new elements into a new list.
///
/// The stack layout is independent of the concrete list currently being
/// processed. This allows the [`InnerFunction`] to use runtime parameters
/// from the stack. Note that the `NUM_INPUT_LISTS` and the input element's size
/// need to be taken into account in such cases. Additionally, the chain-map
/// requires 3 stack registers for internal purposes. Taken together, the stack
/// layout upon starting execution of the `InnerFunction` is:
///
/// ```txt
/// // _ <accessible> [chain_map internals; 3 + NUM_INPUT_LISTS] [input_element]
/// ```
///
/// The special case of one input list is also accessible through [`Map`].
pub struct ChainMap<const NUM_INPUT_LISTS: usize> {
    pub f: InnerFunction,
}

impl<const NUM_INPUT_LISTS: usize> ChainMap<NUM_INPUT_LISTS> {
    pub fn new(f: InnerFunction) -> Self {
        Self { f }
    }
}

impl<const NUM_INPUT_LISTS: usize> BasicSnippet for ChainMap<NUM_INPUT_LISTS> {
    fn inputs(&self) -> Vec<(DataType, String)> {
        let list_type = DataType::List(Box::new(self.f.domain()));

        (0..NUM_INPUT_LISTS)
            .map(|i| (list_type.clone(), format!("*input_list_{i}")))
            .collect_vec()
    }

    fn outputs(&self) -> Vec<(DataType, String)> {
        let list_type = DataType::List(Box::new(self.f.range()));
        vec![(list_type, "*output_list".to_string())]
    }

    fn entrypoint(&self) -> String {
        let maybe_chain_surely_map = if NUM_INPUT_LISTS == 1 {
            "map".to_string()
        } else {
            format!("chain_map_{NUM_INPUT_LISTS}")
        };

        let f_label = self.f.entrypoint();
        format!("tasmlib_list_higher_order_u32_{maybe_chain_surely_map}_{f_label}")
    }

    fn code(&self, library: &mut Library) -> Vec<LabelledInstruction> {
        let input_type = self.f.domain();
        let output_type = self.f.range();
        let new_list = library.import(Box::new(New::new(output_type.clone())));

        let exec_or_call_inner_function = match &self.f {
            InnerFunction::RawCode(code) => {
                // Inlining saves two clock cycles per iteration. If the function cannot be
                // inlined, it needs to be appended to the function body.
                code.inlined_body()
                    .unwrap_or(triton_asm!(call {code.entrypoint()}))
            }
            InnerFunction::DeprecatedSnippet(sn) => {
                assert_eq!(1, sn.input_types().len(), "{INNER_FN_INCORRECT_NUM_INPUTS}");
                let fn_body = sn.function_code(library);
                let (_, instructions) = tokenize(&fn_body).unwrap();
                let labelled_instructions = isa::parser::to_labelled_instructions(&instructions);
                let label = library.explicit_import(&sn.entrypoint_name(), &labelled_instructions);
                triton_asm!(call { label })
            }
            InnerFunction::BasicSnippet(snippet) => {
                assert_eq!(1, snippet.inputs().len(), "{INNER_FN_INCORRECT_NUM_INPUTS}");
                let labelled_instructions = snippet.annotated_code(library);
                let label = library.explicit_import(&snippet.entrypoint(), &labelled_instructions);
                triton_asm!(call { label })
            }
            InnerFunction::NoFunctionBody(lnat) => {
                triton_asm!(call { lnat.label_name })
            }
        };

        let maybe_inner_function_body_raw = match &self.f {
            InnerFunction::RawCode(code) if code.inlined_body().is_none() => &code.function,
            _ => &vec![],
        };

        let entrypoint = self.entrypoint();
        let main_loop_fn = format!("{entrypoint}_loop");
        let input_elem_size = input_type.stack_size();
        let output_elem_size = output_type.stack_size();

        let mul_elem_size = |n| match n {
            1 => triton_asm!(),
            n => triton_asm!(push {n} mul),
        };
        let adjust_output_list_pointer = match output_elem_size {
            0 | 1 => triton_asm!(),
            n => triton_asm!(addi {-(n as i32 - 1)}),
        };
        let pop_input_lists = match NUM_INPUT_LISTS {
            0 => triton_asm!(),
            i if 0 < i && i <= 5 => triton_asm!(pop { i }),
            i if 5 < i && i <= 10 => triton_asm!(pop 5 pop { i - 5 }),
            i if 10 < i && i <= 15 => triton_asm!(pop 5 pop 5 pop { i - 10 }),
            _ => panic!("too many input lists"),
        };

        let main_loop_body = triton_asm! {
            // INVARIANT: _ *end_condition_in_list *output_elem *input_elem
            {main_loop_fn}:

            /* maybe return */
            // not using `recurse_or_return` to have more room for parameters
            // that might live on the stack, to and used by the inner function
            dup 2 dup 1 eq
            skiz return

            /* read */
            {&input_type.read_value_from_memory_leave_pointer()}
            place {input_elem_size}
                        // _ *end_condition_in_list *output_elem *prev_input_elem [input_elem]

            /* map */
            {&exec_or_call_inner_function}
                        // _ *end_condition_in_list *output_elem *prev_input_elem [output_elem]

            /* write */
            pick {output_type.stack_size() + 1}
            {&output_type.write_value_to_memory_leave_pointer()}
            addi {-2 * output_type.stack_size() as i32}
            place 1     // _ *end_condition_in_list *prev_output_elem *prev_input_elem

            recurse
        };

        let map_one_list = triton_asm! {
            // BEFORE: _ [fill; M]   [*in_list; N-M]   *out_list
            // AFTER:  _ [fill; M+1] [*in_list; N-M-1] *out_list

            /* read list lengths */
            read_mem 1
            addi 1      // _ [_; M] [_; N-M-1] *in_list out_list_len *out_list

            pick 2
            read_mem 1
            addi 1      // _ [_; M] [_; N-M-1] out_list_len *out_list in_list_len *in_list

            /* prepare in_list pointer for main loop */
            dup 1
            {&mul_elem_size(input_elem_size)}
            dup 1
            add         // _ [_; M] [_; N-M-1] out_list_len *out_list in_list_len *in_list *in_list_last_word

            /* update out_list's len */
            pick 2
            pick 4      // _ [_; M] [_; N-M-1] *out_list *in_list *in_list_last_word in_list_len out_list_len
            add         // _ [_; M] [_; N-M-1] *out_list *in_list *in_list_last_word new_out_list_len

            dup 0
            pick 4
            write_mem 1
            addi -1     // _ [_; M] [_; N-M-1] *in_list *in_list_last_word new_out_list_len *out_list

            /* store *out_list for next iterations */
            dup 0
            place 4     // _ [_; M] [_; N-M-1] *out_list *in_list *in_list_last_word new_out_list_len *out_list

            /* prepare out_list pointer for main loop */
            pick 1
            {&mul_elem_size(output_elem_size)}
            add         // _ [_; M] [_; N-M-1] *out_list *in_list *in_list_last_word *out_list_last_elem_last_word

            {&adjust_output_list_pointer}
            place 1     // _ [_; M] [_; N-M-1] *out_list *in_list *out_list_last_elem_first_word *in_list_last_word

            call {main_loop_fn}
                        hint used_list: Pointer = stack[2]
                        // _ [_; M] [_; N-M-1] *out_list fill garbage fill

            /* clean up */
            pop 2
            place {NUM_INPUT_LISTS}
                        // _ [_; M+1] [_; N-M-1] *out_list
        };
        let map_all_lists = vec![map_one_list; NUM_INPUT_LISTS].concat();

        triton_asm! {
            // BEFORE: _ [*in_list; N]
            // AFTER:  _ *out_list
            {entrypoint}:
                call {new_list}
                    hint chain_map_output_list: Pointer = stack[0]
                {&map_all_lists}
                place {NUM_INPUT_LISTS}
                {&pop_input_lists}
                return
            {&main_loop_body}
            {&maybe_inner_function_body_raw}
        }
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use itertools::Itertools;
    use num_traits::Zero;
    use rand::random;
    use rand::rngs::StdRng;
    use rand::Rng;
    use rand::SeedableRng;
    use strum::EnumCount;
    use triton_vm::isa::op_stack::OpStackElement;
    use triton_vm::prelude::*;
    use triton_vm::twenty_first::math::other::random_elements;
    use triton_vm::twenty_first::prelude::*;

    use crate::arithmetic;
    use crate::data_type::DataType;
    use crate::empty_stack;
    use crate::library::Library;
    use crate::list::higher_order::inner_function::InnerFunction;
    use crate::list::higher_order::inner_function::RawCode;
    use crate::neptune::mutator_set::get_swbf_indices::u32_to_u128_add_another_u128;
    use crate::rust_shadowing_helper_functions::dyn_malloc::dynamic_allocator;
    use crate::rust_shadowing_helper_functions::list::insert_random_list;
    use crate::rust_shadowing_helper_functions::list::list_get;
    use crate::rust_shadowing_helper_functions::list::list_get_length;
    use crate::rust_shadowing_helper_functions::list::list_set;
    use crate::rust_shadowing_helper_functions::list::list_set_length;
    use crate::snippet_bencher::BenchmarkCase;
    use crate::traits::deprecated_snippet::DeprecatedSnippet;
    use crate::traits::function::Function;
    use crate::traits::function::FunctionInitialState;
    use crate::traits::function::ShadowedFunction;
    use crate::traits::rust_shadow::RustShadow;
    use crate::twenty_first::prelude::x_field_element::EXTENSION_DEGREE;
    use crate::InitVmState;
    use crate::VmHasher;

    use super::*;

    impl<const NUM_INPUT_LISTS: usize> Function for ChainMap<NUM_INPUT_LISTS> {
        fn rust_shadow(
            &self,
            stack: &mut Vec<BFieldElement>,
            memory: &mut HashMap<BFieldElement, BFieldElement>,
        ) {
            let input_type = self.f.domain();
            let output_type = self.f.range();

            New::new(output_type.clone()).rust_shadowing(stack, vec![], vec![], memory);
            let output_list_pointer = stack.pop().unwrap();

            let input_list_pointers = (0..NUM_INPUT_LISTS)
                .map(|_| stack.pop().unwrap())
                .collect_vec();

            // the inner function _must not_ rely on these elements
            let chain_map_buffer_size = 3 + NUM_INPUT_LISTS;
            let buffer = (0..chain_map_buffer_size).map(|_| random::<BFieldElement>());
            stack.extend(buffer);

            let mut total_output_len = 0;
            for input_list_pointer in input_list_pointers {
                let input_list_len = list_get_length(input_list_pointer, memory);

                for i in (0..input_list_len).rev() {
                    let elem = list_get(input_list_pointer, i, memory, input_type.stack_size());
                    stack.extend(elem.into_iter().rev());
                    self.f.apply(stack, memory);
                    let elem = (0..output_type.stack_size())
                        .map(|_| stack.pop().unwrap())
                        .collect();
                    list_set(output_list_pointer, total_output_len + i, elem, memory);
                }

                total_output_len += input_list_len;
            }

            for _ in 0..chain_map_buffer_size {
                stack.pop();
            }

            stack.push(output_list_pointer);
            list_set_length(output_list_pointer, total_output_len, memory);
        }

        fn pseudorandom_initial_state(
            &self,
            seed: [u8; 32],
            _: Option<BenchmarkCase>,
        ) -> FunctionInitialState {
            let input_type = self.f.domain();
            let mut rng = StdRng::from_seed(seed);
            let mut stack = empty_stack();
            let mut memory = HashMap::default();

            // Some inner functions rely on additional arguments on the stack.
            // Some tested functions panic if these arguments are too large. 😵
            let environment_args = rng
                .gen::<[u32; OpStackElement::COUNT]>()
                .map(BFieldElement::from);
            stack.extend(environment_args);

            for list_length in rng.gen::<[u8; NUM_INPUT_LISTS]>() {
                let list_length = usize::from(list_length);
                let list_pointer = dynamic_allocator(&mut memory);
                insert_random_list(&input_type, list_pointer, list_length, &mut memory);
                stack.push(list_pointer);
            }

            FunctionInitialState { stack, memory }
        }
    }

    /// Specifically exists to implement [`DeprecatedSnippet`]. Should only be
    /// upgraded to a regular snippet once [`InnerFunction`] stops supporting
    /// `DeprecatedSnippet`.2
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
            triton_asm!(
                // BEFORE: _ x2 x1 x0
                // AFTER:  _ d4 d3 d2 d1 d0
                {entrypoint}:
                    push 0 push 0
                    push 0 push 0
                    push 0 push 0   // _ x2 x1 x0 0 0 0 0 0 0
                    push 1          // _ x2 x1 x0 0 0 0 0 0 0 1
                    pick 9          // _ x1 x0 0 0 0 0 0 0 1 x2
                    pick 9          // _ x0 0 0 0 0 0 0 1 x2 x1
                    pick 9          // _ 0 0 0 0 0 0 1 x2 x1 x0

                    // Useless additions, to ensure that imports are accepted inside the map generated code
                    push 0
                    push 0
                    call {unused_import}
                    pop 1

                    sponge_init
                    sponge_absorb
                    sponge_squeeze // _ d9 d8 d7 d6 d5 d4 d3 d2 d1 d0
                    pick 9 pick 9  // _ d7 d6 d5 d4 d3 d2 d1 d0 d9 d8
                    pick 9 pick 9  // _ d5 d4 d3 d2 d1 d0 d9 d8 d7 d6
                    pick 9 pop 5   // _ d4 d3 d2 d1 d0
                    return
            )
            .iter()
            .join("\n")
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
            for _ in 0..EXTENSION_DEGREE {
                xfield_element.push(stack.pop().unwrap());
            }

            let digest = VmHasher::hash_varlen(&xfield_element);
            stack.extend(digest.reversed().values());
        }
    }

    /// `InnerFunction` does not implement `Clone`, and it is hard (impossible?) to
    /// teach it. Hence, take a function `f` to generate the `InnerFunction`.
    ///
    /// Theoretically, this _could_ mean that `f` produces a different
    /// `InnerFunction` each time it is called, as every `Fn()` is `FnMut()`.
    /// Since this is a test helper, how about it doesn't. 😊
    fn test_chain_map_with_different_num_input_lists(f: impl Fn() -> InnerFunction) {
        ShadowedFunction::new(ChainMap::<0>::new(f())).test();
        ShadowedFunction::new(ChainMap::<1>::new(f())).test();
        ShadowedFunction::new(ChainMap::<2>::new(f())).test();
        ShadowedFunction::new(ChainMap::<3>::new(f())).test();
        ShadowedFunction::new(ChainMap::<4>::new(f())).test();
        ShadowedFunction::new(ChainMap::<5>::new(f())).test();

        ShadowedFunction::new(ChainMap::<7>::new(f())).test();
        ShadowedFunction::new(ChainMap::<11>::new(f())).test();
        ShadowedFunction::new(ChainMap::<15>::new(f())).test();
    }

    #[test]
    fn prop_test() {
        let f = || InnerFunction::DeprecatedSnippet(Box::new(TestHashXFieldElement));
        test_chain_map_with_different_num_input_lists(f);
    }

    #[test]
    fn test_with_raw_function_identity_on_bfe() {
        let f = || {
            InnerFunction::RawCode(RawCode::new(
                triton_asm!(identity_bfe: return),
                DataType::Bfe,
                DataType::Bfe,
            ))
        };
        test_chain_map_with_different_num_input_lists(f);
    }

    #[test]
    fn test_with_raw_function_bfe_lift() {
        let f = || {
            InnerFunction::RawCode(RawCode::new(
                triton_asm!(bfe_lift: push 0 push 0 pick 2 return),
                DataType::Bfe,
                DataType::Xfe,
            ))
        };
        test_chain_map_with_different_num_input_lists(f);
    }

    #[test]
    fn test_with_raw_function_xfe_get_coeff_0() {
        let f = || {
            InnerFunction::RawCode(RawCode::new(
                triton_asm!(get_0: place 2 pop 2 return),
                DataType::Xfe,
                DataType::Bfe,
            ))
        };
        test_chain_map_with_different_num_input_lists(f);
    }

    #[test]
    fn test_with_raw_function_square_on_bfe() {
        let f = || {
            InnerFunction::RawCode(RawCode::new(
                triton_asm!(square_bfe: dup 0 mul return),
                DataType::Bfe,
                DataType::Bfe,
            ))
        };
        test_chain_map_with_different_num_input_lists(f);
    }

    #[test]
    fn test_with_raw_function_square_plus_n_on_bfe() {
        // Inner function calculates `|x| -> x*x + n`, where `x` is the list element,
        // and `n` is the same value for all elements.
        fn test_case<const N: usize>() {
            let raw_code = InnerFunction::RawCode(RawCode::new(
                triton_asm!(square_plus_n_bfe: dup 0 mul dup {5 + N} add return),
                DataType::Bfe,
                DataType::Bfe,
            ));
            ShadowedFunction::new(ChainMap::<N>::new(raw_code)).test();
        }

        test_case::<0>();
        test_case::<1>();
        test_case::<2>();
        test_case::<3>();
        test_case::<4>();
        test_case::<5>();
        test_case::<7>();
        test_case::<9>();
        test_case::<10>();
    }

    #[test]
    fn test_with_raw_function_square_on_xfe() {
        let f = || {
            InnerFunction::RawCode(RawCode::new(
                triton_asm!(
                    square_xfe: dup 2 dup 2 dup 2 xx_mul return
                ),
                DataType::Xfe,
                DataType::Xfe,
            ))
        };
        test_chain_map_with_different_num_input_lists(f);
    }

    #[test]
    fn test_with_raw_function_xfe_to_digest() {
        let f = || {
            InnerFunction::RawCode(RawCode::new(
                triton_asm!(
                    xfe_to_digest: push 0 push 0 return
                ),
                DataType::Xfe,
                DataType::Digest,
            ))
        };
        test_chain_map_with_different_num_input_lists(f);
    }

    #[test]
    fn test_with_raw_function_digest_to_xfe() {
        let f = || {
            InnerFunction::RawCode(RawCode::new(
                triton_asm!(
                    xfe_to_digest: pop 2 return
                ),
                DataType::Digest,
                DataType::Xfe,
            ))
        };
        test_chain_map_with_different_num_input_lists(f);
    }

    #[test]
    fn test_with_raw_function_square_on_xfe_plus_another_xfe() {
        fn test_case<const N: usize>() {
            let raw_code = InnerFunction::RawCode(RawCode::new(
                triton_asm!(
                    square_xfe_plus_another_xfe:
                        dup 2 dup 2 dup 2 xx_mul
                        dup {5 + 3 + N}
                        dup {5 + 3 + N}
                        dup {5 + 3 + N}
                        xx_add
                        return
                ),
                DataType::Xfe,
                DataType::Xfe,
            ));
            ShadowedFunction::new(ChainMap::<N>::new(raw_code)).test();
        }

        test_case::<0>();
        test_case::<1>();
        test_case::<2>();
        test_case::<3>();
        test_case::<5>();
        test_case::<4>();
        test_case::<6>();
        test_case::<7>();
    }

    #[test]
    fn test_u32_list_to_unit_list() {
        let f = || {
            InnerFunction::RawCode(RawCode::new(
                triton_asm!(remove_elements: pop 1 return),
                DataType::U32,
                DataType::Tuple(vec![]),
            ))
        };
        test_chain_map_with_different_num_input_lists(f);
    }

    #[test]
    fn test_u32_list_to_u64_list() {
        let f = || {
            InnerFunction::RawCode(RawCode::new(
                triton_asm!(duplicate_u32: dup 0 return),
                DataType::U32,
                DataType::U64,
            ))
        };
        test_chain_map_with_different_num_input_lists(f);
    }

    #[test]
    fn test_u32_list_to_u128_list_plus_x() {
        // this code only works with 1 input list
        let raw_code = InnerFunction::RawCode(u32_to_u128_add_another_u128());
        ShadowedFunction::new(Map::new(raw_code)).test();
    }
}

#[cfg(test)]
mod benches {
    use crate::list::higher_order::inner_function::InnerFunction;
    use crate::traits::function::ShadowedFunction;
    use crate::traits::rust_shadow::RustShadow;

    use super::tests::TestHashXFieldElement;
    use super::*;

    #[test]
    fn map_benchmark() {
        let f = InnerFunction::DeprecatedSnippet(Box::new(TestHashXFieldElement));
        ShadowedFunction::new(Map { f }).bench();
    }
}
