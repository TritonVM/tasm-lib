use itertools::Itertools;
use strum::EnumCount;
use tasm_lib::list::higher_order::inner_function::InnerFunction;
use tasm_lib::structure::tasm_object::DEFAULT_MAX_DYN_FIELD_SIZE;
use triton_vm::isa::op_stack::OpStackElement;
use triton_vm::prelude::*;

use crate::list::new::New;
use crate::list::push::Push;
use crate::prelude::*;

const INNER_FN_INCORRECT_NUM_INPUTS: &str = "Inner function in `map` only works with *one* input. \
                                             Use a tuple as a workaround.";
const INNER_FN_INCORRECT_INPUT_DYN_LEN: &str = "An input type of dynamic length to `map`s inner \
                                                function must be a tuple of form `(bfe, _)`.";

/// Applies a given function to every element of a list, and collects the new
/// elements into a new list.
///
/// Mapping over multiple input lists into one output list, effectively chaining
/// inputs before applying the map, is possible with [`ChainMap`]. See there for
/// extended documentation.
pub type Map = ChainMap<1>;

/// Applies a given function `f` to every element of all given lists, collecting
/// the new elements into a new list.
///
/// The given function `f` must produce elements of a type for which the encoded
/// length is [statically known][len]. The input type may have either
/// statically or dynamically known length:
/// - In the static case, the entire element is placed on the stack before
///   passing control to `f`.
/// - In the dynamic case, a memory pointer to the encoded element and the
///   item's length is placed on the stack before passing control to `f`. The
///   input list **must** be encoded according to [`BFieldCodec`]. Otherwise,
///   behavior of `ChainMap` is undefined!
///
/// The stack layout is independent of the list currently being processed. This
/// allows the [`InnerFunction`] `f` to use runtime parameters from the stack.
/// Note that the chain map requires a certain number of stack registers for
/// internal purposes. This number can be accessed through
/// [`ChainMap::NUM_INTERNAL_REGISTERS`]. As mentioned above, the stack layout
/// upon starting execution of `f` depends on the input type's
/// [static length][len]. In the static case, the stack layout is:
///
/// ```txt
/// // _ <accessible> [_; ChainMap::<N>::NUM_INTERNAL_REGISTERS] [input_element; len]
/// ```
///
/// In the case of input elements with a dynamic length, the stack layout is:
///
/// ```txt
/// // _ <accessible> [_; ChainMap::<N>::NUM_INTERNAL_REGISTERS] *elem_i elem_i_len
/// ```
///
/// [len]: BFieldCodec::static_length
pub struct ChainMap<const NUM_INPUT_LISTS: usize> {
    f: InnerFunction,
}

impl<const NUM_INPUT_LISTS: usize> ChainMap<NUM_INPUT_LISTS> {
    /// The number of registers required internally. See [`ChainMap`] for additional
    /// details.
    pub const NUM_INTERNAL_REGISTERS: usize = {
        assert!(NUM_INPUT_LISTS <= Self::MAX_NUM_INPUT_LISTS);

        3 + NUM_INPUT_LISTS
    };

    /// Need access to all lists, plus a little wiggle room.
    const MAX_NUM_INPUT_LISTS: usize = OpStackElement::COUNT - 1;

    /// # Panics
    ///
    /// - if the input type has [static length] _and_ takes up
    ///   [`OpStackElement::COUNT`] or more words
    /// - if the input type has dynamic length and is _anything but_ a tuple
    ///   `(_, `[`BFieldElement`][bfe]`)`
    /// - if the output type takes up [`OpStackElement::COUNT`]` - 1` or more words
    /// - if the output type does not have a [static length][len]
    ///
    /// [len]: BFieldCodec::static_length
    /// [bfe]: DataType::Bfe
    pub fn new(f: InnerFunction) -> Self {
        let domain = f.domain();
        if let Some(input_len) = domain.static_length() {
            // need instruction `place {input_type.stack_size()}`
            assert!(input_len < OpStackElement::COUNT);
        } else {
            let DataType::Tuple(tuple) = domain else {
                panic!("{INNER_FN_INCORRECT_INPUT_DYN_LEN}");
            };
            let [_, DataType::Bfe] = tuple[..] else {
                panic!("{INNER_FN_INCORRECT_INPUT_DYN_LEN}");
            };
        }

        // need instruction `pick {output_type.stack_size() + 1}`
        let output_len = f
            .range()
            .static_length()
            .expect("output type's encoding length must be static");
        assert!(output_len + 1 < OpStackElement::COUNT);

        Self { f }
    }
}

impl<const NUM_INPUT_LISTS: usize> BasicSnippet for ChainMap<NUM_INPUT_LISTS> {
    fn parameters(&self) -> Vec<(DataType, String)> {
        let list_type = DataType::List(Box::new(self.f.domain()));

        (0..NUM_INPUT_LISTS)
            .map(|i| (list_type.clone(), format!("*input_list_{i}")))
            .collect_vec()
    }

    fn return_values(&self) -> Vec<(DataType, String)> {
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
        if self.f.domain().static_length().is_some() {
            self.code_for_static_len_input_type(library)
        } else {
            self.code_for_dyn_len_input_type(library)
        }
    }
}

struct DecomposedInnerFunction<'body> {
    exec_or_call: Vec<LabelledInstruction>,
    fn_body: Option<&'body [LabelledInstruction]>,
}

impl<const NUM_INPUT_LISTS: usize> ChainMap<NUM_INPUT_LISTS> {
    fn code_for_static_len_input_type(&self, library: &mut Library) -> Vec<LabelledInstruction> {
        let input_type = self.f.domain();
        let output_type = self.f.range();
        assert!(input_type.static_length().is_some());

        let new_list = library.import(Box::new(New));
        let inner_fn = self.decompose_inner_fn(library);

        let entrypoint = self.entrypoint();
        let main_loop_fn = format!("{entrypoint}_loop");

        let mul_elem_size = |n| match n {
            1 => triton_asm!(),
            n => triton_asm!(push {n} mul),
        };
        let adjust_output_list_pointer = match output_type.stack_size() {
            0 | 1 => triton_asm!(),
            n => triton_asm!(addi {-(n as i32 - 1)}),
        };

        let main_loop_body = triton_asm! {
            // INVARIANT: _ *end_condition_in_list *output_elem *input_elem

            /* maybe return */
            // not using `recurse_or_return` to have more room for parameters
            // that might live on the stack, to and used by the inner function
            dup 2 dup 1 eq
            skiz return

            /* read */
            {&input_type.read_value_from_memory_leave_pointer()}
            place {input_type.stack_size()}
                        // _ *end_condition_in_list *output_elem *prev_input_elem [input_elem]

            /* map */
            {&inner_fn.exec_or_call}
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
            {&mul_elem_size(input_type.stack_size())}
            dup 1
            add         // _ [_; M] [_; N-M-1] out_list_len *out_list in_list_len *in_list *in_list_first_elem_last_word

            /* update out_list's len */
            pick 2
            pick 4      // _ [_; M] [_; N-M-1] *out_list *in_list *in_list_first_elem_last_word in_list_len out_list_len
            add         // _ [_; M] [_; N-M-1] *out_list *in_list *in_list_first_elem_last_word new_out_list_len

            dup 0
            pick 4
            write_mem 1
            addi -1     // _ [_; M] [_; N-M-1] *in_list *in_list_first_elem_last_word new_out_list_len *out_list

            /* store *out_list for next iterations */
            dup 0
            place 4     // _ [_; M] [_; N-M-1] *out_list *in_list *in_list_first_elem_last_word new_out_list_len *out_list

            /* prepare out_list pointer for main loop */
            pick 1
            {&mul_elem_size(output_type.stack_size())}
            add         // _ [_; M] [_; N-M-1] *out_list *in_list *in_list_first_elem_last_word *out_list_last_elem_last_word

            {&adjust_output_list_pointer}
            place 1     // _ [_; M] [_; N-M-1] *out_list *in_list *out_list_last_elem_first_word *in_list_first_elem_last_word

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
                {&Self::pop_input_lists()}
                return
            {main_loop_fn}:
                {&main_loop_body}
            {&inner_fn.fn_body.unwrap_or_default()}
        }
    }

    fn code_for_dyn_len_input_type(&self, library: &mut Library) -> Vec<LabelledInstruction> {
        let input_type = self.f.domain();
        let output_type = self.f.range();
        assert!(input_type.static_length().is_none());

        let new_list = library.import(Box::new(New));
        let push = library.import(Box::new(Push::new(output_type.clone())));
        let inner_fn = self.decompose_inner_fn(library);

        let entrypoint = self.entrypoint();
        let main_loop_fn = format!("{entrypoint}_loop");

        let main_loop_body = triton_asm! {
            //                ⬐ for Self::NUM_INTERNAL_REGISTERS
            // BEFORE:    _ fill 0           in_list_len *out_list *in_list[0]_si
            // INVARIANT: _ fill i           in_list_len *out_list *in_list[i]_si
            // AFTER:     _ fill in_list_len in_list_len *out_list garbage

            /* maybe return */
            dup 3
            dup 3
            eq
            skiz return

            /* read field size */
            read_mem 1  hint item_len = stack[0]
            addi 2      // _ fill i in_list_len *out_list elem_len *in_list[i]

            /* check field size is reasonable */
            push {DEFAULT_MAX_DYN_FIELD_SIZE}
                        hint default_max_dyn_field_size = stack[0]
            dup 2       // _ fill i in_list_len *out_list l[i]_len *in_list[i] max l[i]_len
            lt
            assert      // _ fill i in_list_len *out_list l[i]_len *in_list[i]

            /* advance item iterator */
            dup 1
            dup 1
            add
            place 2

            /* prepare for inner function */
            place 1     // _ fill i in_list_len *out_list *in_list[i+1]_si *in_list[i] l[i]_len

            /* map */
            {&inner_fn.exec_or_call}
                        // _ fill i in_list_len *out_list *in_list[i]_si [out_elem]

            /* write */
            dup {output_type.stack_size() + 1}
            place {output_type.stack_size()}
            call {push}
                        // _ fill i in_list_len *out_list *in_list[i]_si

            /* advance i */
            pick 3
            addi 1
            place 3
                        // _ fill (i+i) in_list_len *out_list *in_list[i+1]_si

            recurse
        };

        let map_one_list = triton_asm! {
            // BEFORE: _ [fill; M]   [*in_list; N-M]   *out_list
            // AFTER:  _ [fill; M+1] [*in_list; N-M-1] *out_list

            /* read in_list length */
            pick 1
            read_mem 1  hint in_list_len = stack[1]
            addi 2      // _ [_; M] [_; N-M-1] *out_list in_list_len *in_list[0]_si


            /* setup for main loop */
            pick 2
            place 1
            push 0      hint filler = stack[0]
            place 3
            push 0      hint index = stack[0]
            place 3
                        // _ [_; M] [_; N-M-1] fill 0 in_list_len *out_list *in_list[0]_si

            call {main_loop_fn}

            /* clean up */
            pick 1
            place 4
            pop 3
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
                {&Self::pop_input_lists()}
                return
            {main_loop_fn}:
                {&main_loop_body}
            {&inner_fn.fn_body.unwrap_or_default()}
        }
    }

    fn decompose_inner_fn(&self, library: &mut Library) -> DecomposedInnerFunction<'_> {
        let exec_or_call = match &self.f {
            InnerFunction::RawCode(code) => {
                // Inlining saves two clock cycles per iteration. If the function cannot be
                // inlined, it needs to be appended to the function body.
                code.inlined_body()
                    .unwrap_or(triton_asm!(call {code.entrypoint()}))
            }
            InnerFunction::BasicSnippet(snippet) => {
                assert_eq!(
                    1,
                    snippet.parameters().len(),
                    "{INNER_FN_INCORRECT_NUM_INPUTS}"
                );
                let labelled_instructions = snippet.annotated_code(library);
                let label = library.explicit_import(&snippet.entrypoint(), &labelled_instructions);
                triton_asm!(call { label })
            }
            InnerFunction::NoFunctionBody(lnat) => {
                triton_asm!(call { lnat.label_name })
            }
        };

        let fn_body = if let InnerFunction::RawCode(c) = &self.f {
            c.inlined_body().is_none().then_some(c.function.as_slice())
        } else {
            None
        };

        DecomposedInnerFunction {
            exec_or_call,
            fn_body,
        }
    }

    fn pop_input_lists() -> Vec<LabelledInstruction> {
        match NUM_INPUT_LISTS {
            0 => triton_asm!(),
            i @ 1..=5 => triton_asm!(pop { i }),
            i @ 6..=10 => triton_asm!(pop 5 pop { i - 5 }),
            i @ 11..=15 => triton_asm!(pop 5 pop 5 pop { i - 10 }),
            _ => unreachable!("see compile time checks for `NUM_INPUT_LISTS`"),
        }
    }
}

#[cfg(test)]
mod tests {
    use itertools::Itertools;

    use super::*;
    use crate::arithmetic;
    use crate::list::higher_order::inner_function::InnerFunction;
    use crate::list::higher_order::inner_function::RawCode;
    use crate::neptune::mutator_set::get_swbf_indices::u32_to_u128_add_another_u128;
    use crate::rust_shadowing_helper_functions::dyn_malloc::dynamic_allocator;
    use crate::rust_shadowing_helper_functions::list::list_get;
    use crate::rust_shadowing_helper_functions::list::list_get_length;
    use crate::rust_shadowing_helper_functions::list::list_pointer_to_elem_pointer;
    use crate::rust_shadowing_helper_functions::list::list_set;
    use crate::rust_shadowing_helper_functions::list::list_set_length;
    use crate::test_helpers::test_rust_equivalence_given_execution_state;
    use crate::test_prelude::*;

    impl<const NUM_INPUT_LISTS: usize> ChainMap<NUM_INPUT_LISTS> {
        fn init_state(
            &self,
            environment_args: impl IntoIterator<Item = BFieldElement>,
            list_lengths: [u16; NUM_INPUT_LISTS],
            seed: <StdRng as SeedableRng>::Seed,
        ) -> FunctionInitialState {
            let input_type = self.f.domain();
            let mut stack = self.init_stack_for_isolated_run();
            let mut memory = HashMap::default();
            let mut rng = StdRng::from_seed(seed);

            stack.extend(environment_args);

            for list_length in list_lengths {
                let list_length = usize::from(list_length);
                let list = input_type.random_list(&mut rng, list_length);
                let list_pointer = dynamic_allocator(&mut memory);
                let indexed_list = list
                    .into_iter()
                    .enumerate()
                    .map(|(i, v)| (list_pointer + bfe!(i), v));

                memory.extend(indexed_list);
                stack.push(list_pointer);
            }

            FunctionInitialState { stack, memory }
        }
    }

    impl<const NUM_INPUT_LISTS: usize> Function for ChainMap<NUM_INPUT_LISTS> {
        fn rust_shadow(
            &self,
            stack: &mut Vec<BFieldElement>,
            memory: &mut HashMap<BFieldElement, BFieldElement>,
        ) {
            let input_type = self.f.domain();
            let output_type = self.f.range();

            New.rust_shadow(stack, memory);
            let output_list_pointer = stack.pop().unwrap();

            let input_list_pointers = (0..NUM_INPUT_LISTS)
                .map(|_| stack.pop().unwrap())
                .collect_vec();

            // the inner function _must not_ rely on these elements
            let buffer = (0..Self::NUM_INTERNAL_REGISTERS).map(|_| rand::random::<BFieldElement>());
            stack.extend(buffer);

            let mut total_output_len = 0;
            for input_list_pointer in input_list_pointers {
                let input_list_len = list_get_length(input_list_pointer, memory);
                let output_list_len = list_get_length(output_list_pointer, memory);
                let new_output_list_len = output_list_len + input_list_len;
                list_set_length(output_list_pointer, new_output_list_len, memory);

                for i in (0..input_list_len).rev() {
                    if input_type.static_length().is_some() {
                        let elem = list_get(input_list_pointer, i, memory, input_type.stack_size());
                        stack.extend(elem.into_iter().rev());
                    } else {
                        let (len, ptr) = list_pointer_to_elem_pointer(
                            input_list_pointer,
                            i,
                            memory,
                            &input_type,
                        );
                        stack.push(ptr);
                        stack.push(bfe!(len));
                    };
                    self.f.apply(stack, memory);
                    let elem = (0..output_type.stack_size())
                        .map(|_| stack.pop().unwrap())
                        .collect();
                    list_set(output_list_pointer, total_output_len + i, elem, memory);
                }

                total_output_len += input_list_len;
            }

            for _ in 0..Self::NUM_INTERNAL_REGISTERS {
                stack.pop();
            }

            stack.push(output_list_pointer);
        }

        fn pseudorandom_initial_state(
            &self,
            seed: [u8; 32],
            bench: Option<BenchmarkCase>,
        ) -> FunctionInitialState {
            let mut rng = StdRng::from_seed(seed);
            let environment_args = rng.random::<[BFieldElement; OpStackElement::COUNT]>();

            let list_lengths = match bench {
                None => rng.random::<[u8; NUM_INPUT_LISTS]>(),
                Some(BenchmarkCase::CommonCase) => [10; NUM_INPUT_LISTS],
                Some(BenchmarkCase::WorstCase) => [100; NUM_INPUT_LISTS],
            };
            let list_lengths = list_lengths.map(Into::into);

            self.init_state(environment_args, list_lengths, rng.random())
        }
    }

    #[derive(Debug, Clone)]
    pub(crate) struct TestHashXFieldElement;

    impl BasicSnippet for TestHashXFieldElement {
        fn parameters(&self) -> Vec<(DataType, String)> {
            vec![(DataType::Xfe, "element".to_string())]
        }

        fn return_values(&self) -> Vec<(DataType, String)> {
            vec![(DataType::Digest, "digest".to_string())]
        }

        fn entrypoint(&self) -> String {
            "test_hash_xfield_element".to_string()
        }

        fn code(&self, library: &mut Library) -> Vec<LabelledInstruction> {
            let entrypoint = self.entrypoint();
            let unused_import = library.import(Box::new(arithmetic::u32::safe_add::SafeAdd));
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

                    // Useless additions, to ensure that imports are accepted inside the
                    // map-generated code
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
                triton_asm!(square_xfe: dup 2 dup 2 dup 2 xx_mul return),
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
                triton_asm!(xfe_to_digest: push 0 push 0 return),
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
                triton_asm!(xfe_to_digest: pop 2 return),
                DataType::Digest,
                DataType::Xfe,
            ))
        };
        test_chain_map_with_different_num_input_lists(f);
    }

    #[test]
    fn test_with_raw_function_square_on_xfe_plus_another_xfe() {
        fn test_case<const N: usize>() {
            let offset = ChainMap::<{ N }>::NUM_INTERNAL_REGISTERS;
            let raw_code = InnerFunction::RawCode(RawCode::new(
                triton_asm!(
                    square_xfe_plus_another_xfe:
                        dup 2 dup 2 dup 2 xx_mul
                        dup {5 + offset}
                        dup {5 + offset}
                        dup {5 + offset}
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
        let snippet = Map::new(raw_code);
        let encoded_u128 = rand::random::<u128>().encode();
        let input_list_len = rand::rng().random_range(0u16..200);
        let initial_state = snippet.init_state(encoded_u128, [input_list_len], rand::random());
        test_rust_equivalence_given_execution_state(
            &ShadowedFunction::new(snippet),
            initial_state.into(),
        );
    }

    #[proptest(cases = 10)]
    fn num_internal_registers_is_correct(#[strategy(arb())] guard: BFieldElement) {
        fn test_case<const N: usize>(guard: BFieldElement) {
            let offset = ChainMap::<{ N }>::NUM_INTERNAL_REGISTERS;
            let raw_code = InnerFunction::RawCode(RawCode::new(
                triton_asm! { check_env: dup {offset} push {guard} eq assert return },
                DataType::Tuple(vec![]),
                DataType::Tuple(vec![]),
            ));
            let snippet = ChainMap::<N>::new(raw_code);
            let initial_state = snippet.init_state(vec![guard], [1; N], rand::random());
            test_rust_equivalence_given_execution_state(
                &ShadowedFunction::new(snippet),
                initial_state.into(),
            );
        }

        test_case::<0>(guard);
        test_case::<1>(guard);
        test_case::<2>(guard);
        test_case::<3>(guard);
        test_case::<4>(guard);
        test_case::<5>(guard);
        test_case::<6>(guard);
        test_case::<7>(guard);
        test_case::<8>(guard);
        test_case::<9>(guard);
        test_case::<10>(guard);
        test_case::<11>(guard);
        test_case::<12>(guard);
    }

    #[test]
    fn mapping_over_dynamic_length_items_works() {
        let f = || {
            let list_type = DataType::List(Box::new(DataType::Bfe));
            InnerFunction::RawCode(RawCode::new(
                triton_asm!(just_forty_twos: pop 2 push 42 return),
                DataType::Tuple(vec![list_type, DataType::Bfe]),
                DataType::Bfe,
            ))
        };
        assert!(f().domain().static_length().is_none());

        test_chain_map_with_different_num_input_lists(f);
    }

    #[test]
    fn mapping_over_list_of_lists_writing_their_lengths_works() {
        let f = || {
            let list_type = DataType::List(Box::new(DataType::Bfe));
            InnerFunction::RawCode(RawCode::new(
                triton_asm!(write_list_length: pop 1 read_mem 1 pop 1 return),
                DataType::Tuple(vec![list_type, DataType::Bfe]),
                DataType::Bfe,
            ))
        };
        assert!(f().domain().static_length().is_none());

        test_chain_map_with_different_num_input_lists(f);
    }
}

#[cfg(test)]
mod benches {
    use super::tests::TestHashXFieldElement;
    use super::*;
    use crate::list::higher_order::inner_function::InnerFunction;
    use crate::list::higher_order::inner_function::RawCode;
    use crate::test_prelude::*;

    #[test]
    fn map_benchmark() {
        let f = InnerFunction::BasicSnippet(Box::new(TestHashXFieldElement));
        ShadowedFunction::new(Map::new(f)).bench();
    }

    #[test]
    fn map_with_dyn_items_benchmark() {
        let list_type = DataType::List(Box::new(DataType::Bfe));
        let f = InnerFunction::RawCode(RawCode::new(
            triton_asm!(dyn_length_elements: pop 2 push 42 return),
            DataType::Tuple(vec![list_type, DataType::Bfe]),
            DataType::Bfe,
        ));
        ShadowedFunction::new(Map::new(f)).bench();
    }
}
