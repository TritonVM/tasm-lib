use triton_vm::prelude::*;

use crate::list::length::Length;
use crate::prelude::*;

/// Read an element from a list. Performs bounds check.
///
/// Only supports lists with [statically sized](BFieldCodec::static_length)
/// elements.
///
/// ### Behavior
///
/// ```text
/// BEFORE: _ *list [index: u32]
/// AFTER:  _ [element: ElementType]
/// ```
///
/// ### Preconditions
///
/// - the argument `*list` points to a properly [`BFieldCodec`]-encoded list
/// - all input arguments are properly [`BFieldCodec`] encoded
///
/// ### Postconditions
///
/// None.
#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct Get {
    element_type: DataType,
}

impl Get {
    pub const INDEX_OUT_OF_BOUNDS_ERROR_ID: i128 = 380;

    /// Any part of the list is outside the allocated memory page.
    /// See the [memory convention][crate::memory] for more details.
    pub const MEM_PAGE_ACCESS_VIOLATION_ERROR_ID: i128 = 381;

    /// # Panics
    ///
    /// Panics if the element has [dynamic length][BFieldCodec::static_length], or
    /// if the static length is 0.
    pub fn new(element_type: DataType) -> Self {
        Self::assert_element_type_is_supported(&element_type);

        Self { element_type }
    }

    /// # Panics
    ///
    /// Panics if the element has [dynamic length][BFieldCodec::static_length], or
    /// if the static length is 0.
    pub(crate) fn assert_element_type_is_supported(element_type: &DataType) {
        let Some(static_len) = element_type.static_length() else {
            panic!("element should have static length");
        };
        assert_ne!(0, static_len, "element must not be zero-sized");
    }
}

impl BasicSnippet for Get {
    fn parameters(&self) -> Vec<(DataType, String)> {
        let list_type = DataType::List(Box::new(self.element_type.clone()));

        vec![
            (list_type, "*list".to_string()),
            (DataType::U32, "index".to_string()),
        ]
    }

    fn return_values(&self) -> Vec<(DataType, String)> {
        vec![(self.element_type.clone(), "element".to_string())]
    }

    fn entrypoint(&self) -> String {
        let element_type = self.element_type.label_friendly_name();
        format!("tasmlib_list_get_element___{element_type}")
    }

    fn code(&self, library: &mut Library) -> Vec<LabelledInstruction> {
        let list_length = library.import(Box::new(Length));
        let mul_with_element_size = match self.element_type.stack_size() {
            1 => triton_asm!(), // no-op
            n => triton_asm!(push {n} mul),
        };

        triton_asm!(
            // BEFORE: _ *list index
            // AFTER:  _ [element: self.element_type]
            {self.entrypoint()}:
                /* assert access is in bounds */
                dup 1
                call {list_length}  // _ *list index len
                dup 1
                lt                  // _ *list index (index < len)
                assert error_id {Self::INDEX_OUT_OF_BOUNDS_ERROR_ID}

                addi 1
                {&mul_with_element_size}
                                    // _ *list last_word_offset

                /* assert access is within one memory page */
                split
                pick 1
                push 0
                eq
                assert error_id {Self::MEM_PAGE_ACCESS_VIOLATION_ERROR_ID}
                                    // _ *list last_word_offset

                add                 // _ *element_last_word
                {&self.element_type.read_value_from_memory_pop_pointer()}
                                    // _ element
                return
        )
    }
}

#[cfg(test)]
pub(crate) mod tests {
    use triton_vm::error::OpStackError::FailedU32Conversion;

    use super::*;
    use crate::U32_TO_USIZE_ERR;
    use crate::rust_shadowing_helper_functions::list::insert_random_list;
    use crate::rust_shadowing_helper_functions::list::list_get;
    use crate::test_helpers::negative_test;
    use crate::test_prelude::*;

    impl Get {
        fn set_up_initial_state(
            &self,
            list_length: usize,
            index: usize,
            list_pointer: BFieldElement,
        ) -> AccessorInitialState {
            let mut memory = HashMap::default();
            insert_random_list(&self.element_type, list_pointer, list_length, &mut memory);

            let mut stack = self.init_stack_for_isolated_run();
            stack.push(list_pointer);
            stack.push(bfe!(index));

            AccessorInitialState { stack, memory }
        }

        pub fn random_len_idx_ptr(
            bench_case: Option<BenchmarkCase>,
            rng: &mut impl rand::Rng,
        ) -> (usize, usize, BFieldElement) {
            let (index, list_length) = match bench_case {
                Some(BenchmarkCase::CommonCase) => (16, 32),
                Some(BenchmarkCase::WorstCase) => (63, 64),
                None => {
                    let list_length = rng.random_range(1..=100);
                    (rng.random_range(0..list_length), list_length)
                }
            };
            let list_pointer = rng.random();

            (list_length, index, list_pointer)
        }
    }

    impl Accessor for Get {
        fn rust_shadow(
            &self,
            stack: &mut Vec<BFieldElement>,
            memory: &HashMap<BFieldElement, BFieldElement>,
        ) {
            let index: u32 = stack.pop().unwrap().try_into().unwrap();
            let list_pointer = stack.pop().unwrap();

            let index: usize = index.try_into().expect(U32_TO_USIZE_ERR);
            let element_length = self.element_type.static_length().unwrap();
            let element = list_get(list_pointer, index, memory, element_length);

            stack.extend(element.into_iter().rev());
        }

        fn pseudorandom_initial_state(
            &self,
            seed: [u8; 32],
            bench_case: Option<BenchmarkCase>,
        ) -> AccessorInitialState {
            let (list_length, index, list_pointer) =
                Self::random_len_idx_ptr(bench_case, &mut StdRng::from_seed(seed));

            self.set_up_initial_state(list_length, index, list_pointer)
        }
    }

    #[test]
    fn rust_shadow() {
        for ty in [DataType::Bfe, DataType::Digest, DataType::I128] {
            ShadowedAccessor::new(Get::new(ty)).test();
        }
    }

    #[proptest]
    fn out_of_bounds_access_crashes_vm(
        #[strategy(0_usize..=1_000)] list_length: usize,
        #[strategy(#list_length..1 << 32)] index: usize,
        #[strategy(arb())] list_pointer: BFieldElement,
    ) {
        let get = Get::new(DataType::Bfe);
        let initial_state = get.set_up_initial_state(list_length, index, list_pointer);
        test_assertion_failure(
            &ShadowedAccessor::new(get),
            initial_state.into(),
            &[Get::INDEX_OUT_OF_BOUNDS_ERROR_ID],
        );
    }

    #[proptest]
    fn too_large_indices_crash_vm(
        #[strategy(1_usize << 32..)] index: usize,
        #[strategy(arb())] list_pointer: BFieldElement,
    ) {
        let list_length = 0;
        let get = Get::new(DataType::Bfe);
        let initial_state = get.set_up_initial_state(list_length, index, list_pointer);
        let expected_error = InstructionError::OpStackError(FailedU32Conversion(bfe!(index)));
        negative_test(
            &ShadowedAccessor::new(get),
            initial_state.into(),
            &[expected_error],
        );
    }

    /// Create a rather long list containing elements of rather large size, then
    /// try to access one of the higher-index elements and watch the VM crash.
    ///
    /// The goal is to access memory beyond the limit of one [page size]. The
    /// element type's size is chosen to be large-ish to allow for a somewhat
    /// shorter list. For Triton VM to crash (and this test to pass), the total list
    /// size must exceed the page size, and an element outside the page size bound
    /// must be accessed. In order to trigger the _correct_ failure, the element's
    /// index must not be too large. In particular, both the list length and the
    /// element index must be at most [u32::MAX].
    ///
    /// [page size]: crate::memory::dyn_malloc::DYN_MALLOC_PAGE_SIZE
    #[proptest(cases = 100)]
    fn too_large_lists_crash_vm(
        #[strategy(1_u64 << 22..1 << 32)] list_length: u64,
        #[strategy((1 << 22) - 1..#list_length)] index: u64,
        #[strategy(arb())] list_pointer: BFieldElement,
    ) {
        // spare host machine RAM: pretend every element is all-zeros
        let mut memory = HashMap::default();
        memory.insert(list_pointer, bfe!(list_length));

        // type with a large stack size in Triton VM without breaking the host machine
        let tuple_ty = DataType::Tuple(vec![DataType::Bfe; 1 << 10]);
        let get = Get::new(tuple_ty);
        let mut stack = get.init_stack_for_isolated_run();
        stack.push(list_pointer);
        stack.push(bfe!(index));
        let initial_state = AccessorInitialState { stack, memory };

        test_assertion_failure(
            &ShadowedAccessor::new(get),
            initial_state.into(),
            &[Get::MEM_PAGE_ACCESS_VIOLATION_ERROR_ID],
        );
    }
}

#[cfg(test)]
mod benches {
    use super::*;
    use crate::test_prelude::*;

    #[test]
    fn benchmark() {
        ShadowedAccessor::new(Get::new(DataType::Digest)).bench();
    }
}
