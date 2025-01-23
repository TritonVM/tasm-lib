use std::collections::HashMap;

use triton_vm::prelude::*;

use crate::prelude::*;
use crate::traits::basic_snippet::Reviewer;
use crate::traits::basic_snippet::SignOffFingerprint;

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct Pop {
    element_type: DataType,
}

impl Pop {
    pub const EMPTY_LIST_ERROR_ID: i128 = 400;

    pub fn new(element_type: DataType) -> Self {
        Self { element_type }
    }
}

impl BasicSnippet for Pop {
    fn inputs(&self) -> Vec<(DataType, String)> {
        let list_type = DataType::List(Box::new(self.element_type.clone()));
        vec![(list_type, "*list".to_string())]
    }

    fn outputs(&self) -> Vec<(DataType, String)> {
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
                read_mem 1
                addi 1          // _ length *list

                /* assert that length is not 0 */
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

                pick 1
                {&mul_with_element_size}
                                // _ *first_element offset
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
            1 => _ = sign_offs.insert(Reviewer("ferdinand"), 0x28d6e8e81c38c5c3.into()),
            2 => _ = sign_offs.insert(Reviewer("ferdinand"), 0xcc150c79f8d01f5a.into()),
            3 => _ = sign_offs.insert(Reviewer("ferdinand"), 0x0b1c07108fb13ed1.into()),
            5 => _ = sign_offs.insert(Reviewer("ferdinand"), 0x878a01fe736bafce.into()),
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
            let list_length = rng.gen_range(1..1 << 12);
            let list_pointer = rng.gen();

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
