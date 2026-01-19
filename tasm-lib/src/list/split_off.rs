use std::collections::HashMap;

use triton_vm::prelude::*;

use crate::list::get::Get;
use crate::prelude::*;
use crate::traits::basic_snippet::Reviewer;
use crate::traits::basic_snippet::SignOffFingerprint;

/// Mutates an existing vector by reducing its length to `at` and returns the
/// new vector. Mimics [`Vec::split_off`].
///
/// Only supports lists with [statically sized](BFieldCodec::static_length)
/// elements.
///
/// ### Behavior
///
/// ```text
/// BEFORE: _ *list [at: u32]
/// AFTER:  _ *new_list
/// ```
///
/// ### Preconditions
///
/// - the argument `*list` points to a properly [`BFieldCodec`]-encoded list
/// - all input arguments are properly [`BFieldCodec`] encoded
///
/// ### Postconditions
///
/// - `*new_list` points to a properly [`BFieldCodec`]-encoded list
#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct SplitOff {
    element_type: DataType,
}

impl SplitOff {
    pub const OUT_OF_BOUNDS_ERROR_ID: i128 = 80;

    /// # Panics
    ///
    /// Panics if the element has [dynamic length][BFieldCodec::static_length], or
    /// if the static length is 0.
    pub fn new(element_type: DataType) -> Self {
        Get::assert_element_type_is_supported(&element_type);

        Self { element_type }
    }

    fn self_type(&self) -> DataType {
        DataType::List(Box::new(self.element_type.to_owned()))
    }
}

impl BasicSnippet for SplitOff {
    fn parameters(&self) -> Vec<(DataType, String)> {
        vec![
            (self.self_type(), "self".to_owned()),
            (DataType::U32, "at".to_owned()),
        ]
    }

    fn return_values(&self) -> Vec<(DataType, String)> {
        vec![(self.self_type(), "new_list".to_owned())]
    }

    fn entrypoint(&self) -> String {
        let element_type = self.element_type.label_friendly_name();
        format!("tasmlib_list_split_off_{element_type}")
    }

    fn code(&self, library: &mut Library) -> Vec<LabelledInstruction> {
        let dyn_malloc = library.import(Box::new(DynMalloc));
        let mem_cpy = library.import(Box::new(MemCpy));

        triton_asm!(
            // BEFORE: _ *list at
            // AFTER:  _ *new_list
            {self.entrypoint()}:
                /* get original length */
                pick 1
                read_mem 1
                addi 1              // _ at original_length *list

                /* assert `at` is in bounds */
                dup 2
                dup 2               // _ at original_length *list at original_length
                lt
                push 0
                eq                  // _ at original_length *list (at <= original_length)
                assert error_id {Self::OUT_OF_BOUNDS_ERROR_ID}
                                    // _ at original_length *list

                /* write new length of original list */
                dup 2
                place 1             // _ at original_length at *list
                write_mem 1         // _ at original_length (*list+1)

                /* prepare mem_cpy: *read_source */
                dup 2               // _ at original_length (*list+1) at
                push {self.element_type.stack_size()}
                mul
                add                 // _ at original_length (*list + 1 + at*element_size)
                                    // _ at original_length *read_source

                /* allocate new list and set its length */
                pick 2
                push -1
                mul                 // _ original_length *read_source (-at)
                pick 2
                add                 // _ *read_source (original_length - at)
                                    // _ *read_source new_len

                dup 0
                call {dyn_malloc}   // _ *read_source new_len new_len *new_list
                dup 0
                place 4             // _ *new_list *read_source new_len new_len *new_list
                write_mem 1         // _ *new_list *read_source new_len (*new_list + 1)
                                    // _ *new_list *read_source new_len *write_dest

                /* prepare mem_cpy: num_words */
                pick 1
                push {self.element_type.stack_size()}
                mul                 // _ *new_list *read_source *write_dest (new_len * element_size)
                                    // _ *new_list *read_source *write_dest num_words

                call {mem_cpy}      // _ *new_list
                return
        )
    }

    fn sign_offs(&self) -> HashMap<Reviewer, SignOffFingerprint> {
        let mut sign_offs = HashMap::new();
        match self.element_type.stack_size() {
            1 => _ = sign_offs.insert(Reviewer("ferdinand"), 0x1e0ba22af4313058.into()),
            2 => _ = sign_offs.insert(Reviewer("ferdinand"), 0x1213c06246c8d536.into()),
            3 => _ = sign_offs.insert(Reviewer("ferdinand"), 0xcc176a741f258a1b.into()),
            4 => _ = sign_offs.insert(Reviewer("ferdinand"), 0x98b6a28ab6be5175.into()),
            5 => _ = sign_offs.insert(Reviewer("ferdinand"), 0x60f106bea20cd03c.into()),
            _ => (),
        }

        sign_offs
    }
}

#[cfg(test)]
mod tests {
    use proptest::strategy::Union;

    use super::*;
    use crate::U32_TO_USIZE_ERR;
    use crate::list::LIST_METADATA_SIZE;
    use crate::rust_shadowing_helper_functions::dyn_malloc::dynamic_allocator;
    use crate::rust_shadowing_helper_functions::list::insert_random_list;
    use crate::rust_shadowing_helper_functions::list::list_set_length;
    use crate::rust_shadowing_helper_functions::list::load_list_unstructured;
    use crate::test_helpers::test_assertion_failure;
    use crate::test_prelude::*;

    impl SplitOff {
        fn set_up_initial_state(
            &self,
            list_length: usize,
            at: usize,
            list_pointer: BFieldElement,
        ) -> FunctionInitialState {
            let mut memory = HashMap::default();
            insert_random_list(&self.element_type, list_pointer, list_length, &mut memory);

            let mut stack = self.init_stack_for_isolated_run();
            stack.push(list_pointer);
            stack.push(bfe!(at));

            FunctionInitialState { stack, memory }
        }
    }

    impl Function for SplitOff {
        fn rust_shadow(
            &self,
            stack: &mut Vec<BFieldElement>,
            memory: &mut HashMap<BFieldElement, BFieldElement>,
        ) {
            let at = pop_encodable::<u32>(stack)
                .try_into()
                .expect(U32_TO_USIZE_ERR);
            let list_pointer = stack.pop().unwrap();

            let mut list =
                load_list_unstructured(self.element_type.stack_size(), list_pointer, memory);
            let new_list = list.split_off(at);

            let new_list_pointer = dynamic_allocator(memory);
            list_set_length(list_pointer, list.len(), memory);
            list_set_length(new_list_pointer, new_list.len(), memory);

            for (offset, word) in (LIST_METADATA_SIZE..).zip(new_list.into_iter().flatten()) {
                memory.insert(new_list_pointer + bfe!(offset), word);
            }
            stack.push(new_list_pointer);
        }

        fn pseudorandom_initial_state(
            &self,
            seed: [u8; 32],
            bench_case: Option<BenchmarkCase>,
        ) -> FunctionInitialState {
            let mut rng = StdRng::from_seed(seed);
            let (list_length, at) = match bench_case {
                Some(BenchmarkCase::CommonCase) => (100, 50),
                Some(BenchmarkCase::WorstCase) => (1000, 0),
                None => {
                    let list_length = rng.random_range(1..1000);
                    (list_length, rng.random_range(0..list_length))
                }
            };
            let list_pointer = rng.random();

            self.set_up_initial_state(list_length, at, list_pointer)
        }
    }

    #[test]
    fn rust_shadow() {
        for element_type in [
            DataType::U32,
            DataType::U64,
            DataType::Xfe,
            DataType::U128,
            DataType::Digest,
        ] {
            ShadowedFunction::new(SplitOff::new(element_type)).test()
        }
    }

    #[proptest]
    fn out_of_bounds_index_crashes_vm(
        #[strategy(Union::new(
            [DataType::U32, DataType::U64, DataType::Xfe, DataType::Digest].map(Just)
        ))]
        element_type: DataType,
        #[strategy(0_usize..100)] list_length: usize,
        #[strategy(#list_length..1 << 30)] at: usize,
        #[strategy(arb())] list_pointer: BFieldElement,
    ) {
        let snippet = SplitOff::new(element_type);
        let initial_state = snippet.set_up_initial_state(list_length, at, list_pointer);

        test_assertion_failure(
            &ShadowedFunction::new(snippet),
            initial_state.into(),
            &[SplitOff::OUT_OF_BOUNDS_ERROR_ID],
        );
    }
}

#[cfg(test)]
mod benches {
    use super::*;
    use crate::test_prelude::*;

    #[test]
    fn benchmark() {
        ShadowedFunction::new(SplitOff::new(DataType::Xfe)).bench();
    }
}
