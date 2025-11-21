use triton_vm::prelude::*;

use crate::list::get::Get;
use crate::list::length::Length;
use crate::prelude::*;

/// Write an element to a list. Performs bounds check.
///
/// Only supports lists with [statically sized](BFieldCodec::static_length)
/// elements.
///
/// ### Behavior
///
/// ```text
/// BEFORE: _ [element: ElementType] *list [index: u32]
/// AFTER:  _
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
pub struct Set {
    element_type: DataType,
}

impl Set {
    pub const INDEX_OUT_OF_BOUNDS_ERROR_ID: i128 = 390;

    /// Any part of the list is outside the allocated memory page.
    /// See the [memory convention][crate::memory] for more details.
    pub const MEM_PAGE_ACCESS_VIOLATION_ERROR_ID: i128 = 391;

    /// # Panics
    ///
    /// Panics if the element has [dynamic length][BFieldCodec::static_length], or
    /// if the static length is 0.
    pub fn new(element_type: DataType) -> Self {
        Get::assert_element_type_is_supported(&element_type);

        Self { element_type }
    }
}

impl BasicSnippet for Set {
    fn parameters(&self) -> Vec<(DataType, String)> {
        let element_type = self.element_type.clone();
        let list_type = DataType::List(Box::new(element_type.clone()));
        let index_type = DataType::U32;

        vec![
            (element_type, "element".to_string()),
            (list_type, "*list".to_string()),
            (index_type, "index".to_string()),
        ]
    }

    fn return_values(&self) -> Vec<(DataType, String)> {
        vec![]
    }

    fn entrypoint(&self) -> String {
        let element_type = self.element_type.label_friendly_name();
        format!("tasmlib_list_set_element___{element_type}")
    }

    fn code(&self, library: &mut Library) -> Vec<LabelledInstruction> {
        let list_length = library.import(Box::new(Length));
        let mul_with_element_size = match self.element_type.stack_size() {
            1 => triton_asm!(), // no-op
            n => triton_asm!(push {n} mul),
        };
        let add_element_size_minus_1 = match self.element_type.stack_size() {
            1 => triton_asm!(), // no-op
            n => triton_asm!(addi {n - 1}),
        };

        triton_asm!(
            // BEFORE: _ [element: self.element_type] *list index
            // AFTER:  _
            {self.entrypoint()}:
                /* assert access is in bounds */
                dup 1
                call {list_length}  // _ [element] *list index len
                dup 1
                lt                  // _ [element] *list index (index < len)
                assert error_id {Self::INDEX_OUT_OF_BOUNDS_ERROR_ID}
                                    // _ [element] *list index

                {&mul_with_element_size}
                                    // _ [element] *list offset_for_previous_elements
                addi 1              // _ [element] *list offset

                /* assert access is within one memory page */
                dup 0
                {&add_element_size_minus_1}
                                    // _ [element] *list offset highest_word_idx
                split
                pop 1
                push 0
                eq
                assert error_id {Self::MEM_PAGE_ACCESS_VIOLATION_ERROR_ID}
                                    // _ [element] *list offset_including_list_metadata

                add                 // _ [element] *element
                {&self.element_type.write_value_to_memory_pop_pointer()}
                                    // _
                return
        )
    }
}

#[cfg(test)]
mod tests {
    use proptest::collection::vec;
    use triton_vm::error::OpStackError::FailedU32Conversion;

    use super::*;
    use crate::U32_TO_USIZE_ERR;
    use crate::rust_shadowing_helper_functions::list::insert_random_list;
    use crate::rust_shadowing_helper_functions::list::list_set;
    use crate::test_helpers::negative_test;
    use crate::test_prelude::*;

    impl Set {
        fn set_up_initial_state(
            &self,
            list_length: usize,
            index: usize,
            list_pointer: BFieldElement,
            element: Vec<BFieldElement>,
        ) -> FunctionInitialState {
            let mut memory = HashMap::default();
            insert_random_list(&self.element_type, list_pointer, list_length, &mut memory);

            let mut stack = self.init_stack_for_isolated_run();
            stack.extend(element.into_iter().rev());
            stack.push(list_pointer);
            stack.push(bfe!(index));

            FunctionInitialState { stack, memory }
        }
    }

    impl Function for Set {
        fn rust_shadow(
            &self,
            stack: &mut Vec<BFieldElement>,
            memory: &mut HashMap<BFieldElement, BFieldElement>,
        ) {
            let index = pop_encodable::<u32>(stack);
            let list_pointer = stack.pop().unwrap();
            let element = (0..self.element_type.stack_size())
                .map(|_| stack.pop().unwrap())
                .collect_vec();

            let index = index.try_into().expect(U32_TO_USIZE_ERR);
            list_set(list_pointer, index, element, memory);
        }

        fn pseudorandom_initial_state(
            &self,
            seed: [u8; 32],
            bench_case: Option<BenchmarkCase>,
        ) -> FunctionInitialState {
            let mut rng = StdRng::from_seed(seed);
            let (list_length, index, list_pointer) = Get::random_len_idx_ptr(bench_case, &mut rng);
            let element = self.element_type.seeded_random_element(&mut rng);

            self.set_up_initial_state(list_length, index, list_pointer, element)
        }
    }

    #[test]
    fn rust_shadow() {
        for ty in [
            DataType::Bool,
            DataType::Bfe,
            DataType::U32,
            DataType::U64,
            DataType::Xfe,
            DataType::Digest,
        ] {
            ShadowedFunction::new(Set::new(ty)).test();
        }
    }

    #[proptest]
    fn out_of_bounds_access_crashes_vm(
        #[strategy(0_usize..=1_000)] list_length: usize,
        #[strategy(#list_length..1 << 32)] index: usize,
        #[strategy(arb())] list_pointer: BFieldElement,
        #[strategy(vec(arb(), 1))] element: Vec<BFieldElement>,
    ) {
        let set = Set::new(DataType::Bfe);
        let initial_state = set.set_up_initial_state(list_length, index, list_pointer, element);
        test_assertion_failure(
            &ShadowedFunction::new(set),
            initial_state.into(),
            &[Set::INDEX_OUT_OF_BOUNDS_ERROR_ID],
        );
    }

    #[proptest]
    fn too_large_indices_crash_vm(
        #[strategy(1_usize << 32..)] index: usize,
        #[strategy(arb())] list_pointer: BFieldElement,
        #[strategy(vec(arb(), 1))] element: Vec<BFieldElement>,
    ) {
        let list_length = 0;
        let set = Set::new(DataType::Bfe);
        let initial_state = set.set_up_initial_state(list_length, index, list_pointer, element);
        let expected_error = InstructionError::OpStackError(FailedU32Conversion(bfe!(index)));
        negative_test(
            &ShadowedFunction::new(set),
            initial_state.into(),
            &[expected_error],
        );
    }

    /// See mirroring test for [`Get`] for an explanation.
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
        let set = Set::new(tuple_ty);

        // no element on stack: stack underflow implies things have gone wrong already
        let mut stack = set.init_stack_for_isolated_run();
        stack.push(list_pointer);
        stack.push(bfe!(index));
        let initial_state = AccessorInitialState { stack, memory };

        test_assertion_failure(
            &ShadowedFunction::new(set),
            initial_state.into(),
            &[Set::MEM_PAGE_ACCESS_VIOLATION_ERROR_ID],
        );
    }
}

#[cfg(test)]
mod benches {
    use super::*;
    use crate::test_prelude::*;

    #[test]
    fn benchmark() {
        ShadowedFunction::new(Set::new(DataType::Digest)).bench();
    }
}
