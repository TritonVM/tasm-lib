use triton_vm::prelude::*;

use crate::list::length::Length;
use crate::prelude::*;

/// Read an element from a list. Performs bounds check.
///
/// Only supports lists with [statically sized](DataType::static_length)
/// elements.
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
    /// Panics if the element has [dynamic length][DataType::static_length].
    pub fn new(element_type: DataType) -> Self {
        let has_static_len = element_type.static_length().is_some();
        assert!(has_static_len, "element should have static length");

        Self { element_type }
    }
}

impl BasicSnippet for Get {
    fn inputs(&self) -> Vec<(DataType, String)> {
        let list_type = DataType::List(Box::new(self.element_type.clone()));

        vec![
            (list_type, "*list".to_string()),
            (DataType::U32, "index".to_string()),
        ]
    }

    fn outputs(&self) -> Vec<(DataType, String)> {
        vec![(self.element_type.clone(), "element".to_string())]
    }

    fn entrypoint(&self) -> String {
        let element_type = self.element_type.label_friendly_name();
        format!("tasmlib_list_get_element___{element_type}")
    }

    fn code(&self, library: &mut Library) -> Vec<LabelledInstruction> {
        let mul_with_element_size = match self.element_type.stack_size() {
            1 => triton_asm!(/* no-op */),
            n => triton_asm!(push {n} mul),
        };

        let list_length = library.import(Box::new(Length));

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
mod tests {
    use super::*;
    use crate::rust_shadowing_helper_functions::list::insert_random_list;
    use crate::rust_shadowing_helper_functions::list::list_get;
    use crate::test_prelude::*;
    use crate::U32_TO_USIZE_ERR;

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
            let mut rng = StdRng::from_seed(seed);
            let list_length = match bench_case {
                Some(BenchmarkCase::CommonCase) => 1 << 5,
                Some(BenchmarkCase::WorstCase) => 1 << 6,
                None => rng.gen_range(1..=100),
            };
            let index = match bench_case {
                Some(BenchmarkCase::CommonCase) => list_length / 2,
                Some(BenchmarkCase::WorstCase) => list_length - 1,
                None => rng.gen_range(0..list_length),
            };

            let mut memory = HashMap::default();
            let list_pointer = rng.gen();
            insert_random_list(&self.element_type, list_pointer, list_length, &mut memory);

            let mut stack = self.init_stack_for_isolated_run();
            stack.push(list_pointer);
            stack.push(bfe!(index));

            AccessorInitialState { stack, memory }
        }
    }

    #[test]
    fn rust_shadow() {
        for ty in [DataType::Bfe, DataType::Digest, DataType::I128] {
            ShadowedAccessor::new(Get::new(ty)).test();
        }
    }
}

#[cfg(test)]
mod benches {
    use super::*;
    use crate::test_prelude::*;

    #[test]
    fn get_benchmark() {
        ShadowedAccessor::new(Get::new(DataType::Digest)).bench();
    }
}
