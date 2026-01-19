use std::collections::HashMap;

use triton_vm::prelude::*;

use crate::list::get::Get;
use crate::prelude::*;
use crate::traits::basic_snippet::Reviewer;
use crate::traits::basic_snippet::SignOffFingerprint;

/// Push an element to a list. Performs bounds check.
///
/// Only supports lists with [statically sized](BFieldCodec::static_length)
/// elements. The element's static size must be in range `1..=15`.
///
/// ### Behavior
///
/// ```text
/// BEFORE: _ *list [element: ElementType]
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
pub struct Push {
    element_type: DataType,
}

impl Push {
    /// Any part of the list is outside the allocated memory page.
    /// See the [memory convention][crate::memory] for more details.
    pub const MEM_PAGE_ACCESS_VIOLATION_ERROR_ID: i128 = 410;

    /// # Panics
    ///
    /// Panics
    /// - if the element has [dynamic length][BFieldCodec::static_length], or
    /// - if the static length is 0, or
    /// - if the static length is larger than or equal to 16.
    pub fn new(element_type: DataType) -> Self {
        // need to access argument `*list`
        assert!(element_type.stack_size() < 16);
        Get::assert_element_type_is_supported(&element_type);

        Self { element_type }
    }
}

impl BasicSnippet for Push {
    fn parameters(&self) -> Vec<(DataType, String)> {
        let list_type = DataType::List(Box::new(self.element_type.clone()));
        let element_type = self.element_type.clone();

        vec![
            (list_type, "*list".to_string()),
            (element_type, "element".to_string()),
        ]
    }

    fn return_values(&self) -> Vec<(DataType, String)> {
        vec![]
    }

    fn entrypoint(&self) -> String {
        let element_type = self.element_type.label_friendly_name();
        format!("tasmlib_list_push___{element_type}")
    }

    fn code(&self, _: &mut Library) -> Vec<LabelledInstruction> {
        let mul_with_element_size = match self.element_type.stack_size() {
            1 => triton_asm!(), // no-op
            n => triton_asm!(push {n} mul),
        };
        let add_element_size_minus_1 = match self.element_type.stack_size() {
            1 => triton_asm!(), // no-op
            n => triton_asm!(addi {n - 1}),
        };

        triton_asm!(
            // BEFORE: _ *list [element]
            // AFTER:  _
            {self.entrypoint()}:
                pick {self.element_type.stack_size()}
                                    // _ [element] *list
                read_mem 1          // _ [element] length *list
                addi 1

                /* update list length */
                dup 1               // _ [element] length *list length
                addi 1              // _ [element] length *list new_length
                pick 1
                write_mem 1         // _ [element] length (*list + 1)

                /* compute element's offset in list */
                pick 1
                {&mul_with_element_size}
                                    // _ [element] (*list + 1) offset

                /* assert access is within one memory page */
                dup 0
                {&add_element_size_minus_1}
                                    // _ [element] *list offset highest_word_idx
                split
                pop 1
                push 0
                eq
                assert error_id {Self::MEM_PAGE_ACCESS_VIOLATION_ERROR_ID}
                                    // _ [element] *list offset

                /* finally, write that thing */
                add                 // _ [element] *element
                {&self.element_type.write_value_to_memory_pop_pointer()}
                                    // _
                return
        )
    }

    fn sign_offs(&self) -> HashMap<Reviewer, SignOffFingerprint> {
        let mut sign_offs = HashMap::new();
        match self.element_type.stack_size() {
            1 => _ = sign_offs.insert(Reviewer("ferdinand"), 0x718981e3d21dc9d2.into()),
            2 => _ = sign_offs.insert(Reviewer("ferdinand"), 0x885f097adff5e5f6.into()),
            3 => _ = sign_offs.insert(Reviewer("ferdinand"), 0x9d962aecbcdf0a19.into()),
            5 => _ = sign_offs.insert(Reviewer("ferdinand"), 0x1774f90a0fb1c3eb.into()),
            _ => (),
        }

        sign_offs
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::rust_shadowing_helper_functions::list::insert_random_list;
    use crate::rust_shadowing_helper_functions::list::list_push;
    use crate::test_prelude::*;

    impl Push {
        fn set_up_initial_state(
            &self,
            list_length: usize,
            list_pointer: BFieldElement,
            element: Vec<BFieldElement>,
        ) -> FunctionInitialState {
            let mut memory = HashMap::default();
            insert_random_list(&self.element_type, list_pointer, list_length, &mut memory);

            let mut stack = self.init_stack_for_isolated_run();
            stack.push(list_pointer);
            stack.extend(element.into_iter().rev());

            FunctionInitialState { stack, memory }
        }
    }

    impl Function for Push {
        fn rust_shadow(
            &self,
            stack: &mut Vec<BFieldElement>,
            memory: &mut HashMap<BFieldElement, BFieldElement>,
        ) {
            let element = (0..self.element_type.stack_size())
                .map(|_| stack.pop().unwrap())
                .collect();
            let list_pointer = stack.pop().unwrap();

            list_push(list_pointer, element, memory);
        }

        fn pseudorandom_initial_state(
            &self,
            seed: [u8; 32],
            bench_case: Option<BenchmarkCase>,
        ) -> FunctionInitialState {
            let mut rng = StdRng::from_seed(seed);
            let (list_length, _, list_pointer) = Get::random_len_idx_ptr(bench_case, &mut rng);
            let element = self.element_type.seeded_random_element(&mut rng);

            self.set_up_initial_state(list_length, list_pointer, element)
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
            ShadowedFunction::new(Push::new(ty)).test()
        }
    }

    /// See similar test for [`Get`] for an explanation.
    #[proptest(cases = 100)]
    fn too_large_lists_crash_vm(
        #[strategy(1_u64 << 29..1 << 32)] list_length: u64,
        #[strategy(arb())] list_pointer: BFieldElement,
    ) {
        // spare host machine RAM: pretend every element is all-zeros
        let mut memory = HashMap::default();
        memory.insert(list_pointer, bfe!(list_length));

        // largest supported element type
        let tuple_ty = DataType::Tuple(vec![DataType::Bfe; 15]);
        let push = Push::new(tuple_ty);

        let mut stack = push.init_stack_for_isolated_run();
        stack.push(list_pointer);
        stack.extend(bfe_array![0; 15]);
        let initial_state = AccessorInitialState { stack, memory };

        test_assertion_failure(
            &ShadowedFunction::new(push),
            initial_state.into(),
            &[Push::MEM_PAGE_ACCESS_VIOLATION_ERROR_ID],
        );
    }
}

#[cfg(test)]
mod benches {
    use super::*;
    use crate::test_prelude::*;

    #[test]
    fn benchmark() {
        ShadowedFunction::new(Push::new(DataType::Digest));
    }
}
