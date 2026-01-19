use std::collections::HashMap;

use triton_vm::prelude::*;

use crate::list::get::Get;
use crate::prelude::*;
use crate::traits::basic_snippet::Reviewer;
use crate::traits::basic_snippet::SignOffFingerprint;

/// Pop an element from a list. Performs bounds check.
///
/// Only supports lists with [statically sized](BFieldCodec::static_length)
/// elements.
///
/// ### Behavior
///
/// ```text
/// BEFORE: _ *list
/// AFTER:  _ [element: ElementType]
/// ```
///
/// ### Preconditions
///
/// - the argument `*list` points to a properly [`BFieldCodec`]-encoded list
/// - the list `*list` points must be non-empty
/// - all input arguments are properly [`BFieldCodec`] encoded
///
/// ### Postconditions
///
/// None.
#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct Pop {
    element_type: DataType,
}

impl Pop {
    pub const EMPTY_LIST_ERROR_ID: i128 = 400;

    /// Any part of the list is outside the allocated memory page.
    /// See the [memory convention][crate::memory] for more details.
    pub const MEM_PAGE_ACCESS_VIOLATION_ERROR_ID: i128 = 401;

    /// # Panics
    ///
    /// Panics if the element has [dynamic length][BFieldCodec::static_length], or
    /// if the static length is 0.
    pub fn new(element_type: DataType) -> Self {
        Get::assert_element_type_is_supported(&element_type);

        Self { element_type }
    }
}

impl BasicSnippet for Pop {
    fn parameters(&self) -> Vec<(DataType, String)> {
        let list_type = DataType::List(Box::new(self.element_type.clone()));
        vec![(list_type, "*list".to_string())]
    }

    fn return_values(&self) -> Vec<(DataType, String)> {
        vec![(self.element_type.clone(), "element".to_string())]
    }

    fn entrypoint(&self) -> String {
        let element_type = self.element_type.label_friendly_name();
        format!("tasmlib_list_pop___{element_type}")
    }

    fn code(&self, _: &mut Library) -> Vec<LabelledInstruction> {
        let mul_with_element_size = match self.element_type.stack_size() {
            1 => triton_asm!(), // no-op
            n => triton_asm!(push {n} mul),
        };

        triton_asm!(
            // BEFORE: _ *list
            // AFTER:  _ [element]
            {self.entrypoint()}:
                /* assert that length is not 0 */
                read_mem 1
                addi 1          // _ length *list
                dup 1
                push 0
                eq              // _ length *list (length == 0)
                push 0
                eq              // _ length *list (length != 0)
                assert error_id {Self::EMPTY_LIST_ERROR_ID}
                                // _ length *list

                /* decrement list length */
                dup 1
                addi -1         // _ length *list new_length
                pick 1
                write_mem 1     // _ length *first_element
                                // _ index  *first_element

                /* compute element's word offset */
                pick 1
                {&mul_with_element_size}
                                // _ *first_element offset

                /* assert access is within one memory page */
                dup 0
                split
                pop 1
                push 0
                eq
                assert error_id {Self::MEM_PAGE_ACCESS_VIOLATION_ERROR_ID}
                                // _ *first_element offset

                /* finally, read that thing */
                add             // _ *next_element
                addi -1         // _ *last_element_last_word
                {&self.element_type.read_value_from_memory_pop_pointer()}
                                // _ [elements]

                return
        )
    }

    fn sign_offs(&self) -> HashMap<Reviewer, SignOffFingerprint> {
        let mut sign_offs = HashMap::new();
        match self.element_type.stack_size() {
            1 => _ = sign_offs.insert(Reviewer("ferdinand"), 0x3d7287a7a71d27d0.into()),
            2 => _ = sign_offs.insert(Reviewer("ferdinand"), 0x87273e433f2d09bf.into()),
            3 => _ = sign_offs.insert(Reviewer("ferdinand"), 0xd502e59ed7251525.into()),
            5 => _ = sign_offs.insert(Reviewer("ferdinand"), 0xf8ed5295f3d8a9c7.into()),
            _ => (),
        }

        sign_offs
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::rust_shadowing_helper_functions::list::insert_random_list;
    use crate::rust_shadowing_helper_functions::list::list_pop;
    use crate::test_prelude::*;

    impl Pop {
        fn set_up_initial_state(
            &self,
            list_length: usize,
            list_pointer: BFieldElement,
        ) -> FunctionInitialState {
            let mut memory = HashMap::default();
            insert_random_list(&self.element_type, list_pointer, list_length, &mut memory);

            let mut stack = self.init_stack_for_isolated_run();
            stack.push(list_pointer);

            FunctionInitialState { stack, memory }
        }
    }

    impl Function for Pop {
        fn rust_shadow(
            &self,
            stack: &mut Vec<BFieldElement>,
            memory: &mut HashMap<BFieldElement, BFieldElement>,
        ) {
            let list_address = stack.pop().unwrap();
            let element = list_pop(list_address, memory, self.element_type.stack_size());
            stack.extend(element.into_iter().rev());
        }

        fn pseudorandom_initial_state(
            &self,
            seed: [u8; 32],
            _: Option<BenchmarkCase>,
        ) -> FunctionInitialState {
            let mut rng = StdRng::from_seed(seed);
            let list_length = rng.random_range(1..1 << 12);
            let list_pointer = rng.random();

            self.set_up_initial_state(list_length, list_pointer)
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
            ShadowedFunction::new(Pop::new(ty)).test();
        }
    }

    #[proptest]
    fn empty_list_crashes_vm(#[strategy(arb())] list_pointer: BFieldElement) {
        let pop = Pop::new(DataType::Digest);
        let initial_state = pop.set_up_initial_state(0, list_pointer);
        test_assertion_failure(
            &ShadowedFunction::new(pop),
            initial_state.into(),
            &[Pop::EMPTY_LIST_ERROR_ID],
        );
    }

    /// See similar test for [`Get`] for an explanation.
    #[proptest(cases = 100)]
    fn too_large_lists_crash_vm(
        #[strategy(1_u64 << 22..1 << 32)] list_length: u64,
        #[strategy(arb())] list_pointer: BFieldElement,
    ) {
        // spare host machine RAM: pretend every element is all-zeros
        let mut memory = HashMap::default();
        memory.insert(list_pointer, bfe!(list_length));

        // type with a large stack size in Triton VM without breaking the host machine
        let tuple_ty = DataType::Tuple(vec![DataType::Bfe; 1 << 10]);
        let set = Pop::new(tuple_ty);

        let mut stack = set.init_stack_for_isolated_run();
        stack.push(list_pointer);
        let initial_state = AccessorInitialState { stack, memory };

        test_assertion_failure(
            &ShadowedFunction::new(set),
            initial_state.into(),
            &[Pop::MEM_PAGE_ACCESS_VIOLATION_ERROR_ID],
        );
    }
}

#[cfg(test)]
mod benches {
    use super::*;
    use crate::test_prelude::*;

    #[test]
    fn benchmark() {
        ShadowedFunction::new(Pop::new(DataType::Digest)).bench();
    }
}
