use triton_vm::prelude::*;

use crate::list::get::Get;
use crate::prelude::*;

/// Returns `true` if the list contains an element with the given value.
///
/// This operation is *O*(*n*).
///
/// Mirrors the `contains` method from Rust `core` as closely as possible.
///
/// Only supports lists with [statically sized](BFieldCodec::static_length)
/// elements. The element's static size must be in range `1..=14`.
///
/// ### Behavior
///
/// ```text
/// BEFORE: _ *list [needle: ElementType]
/// AFTER:  _ [needle ∈ list: bool]
/// ```
///
/// ### Preconditions
///
/// - the argument `*list` points to a properly [`BFieldCodec`]-encoded list
/// - all input arguments are properly [`BFieldCodec`] encoded
///
/// ### Postconditions
///
/// - the output is properly [`BFieldCodec`] encoded
#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct Contains {
    element_type: DataType,
}

impl Contains {
    /// # Panics
    ///
    /// Panics
    /// - if the element has [dynamic length][BFieldCodec::static_length], or
    /// - if the static length is 0, or
    /// - if the static length is larger than or equal to 15.
    // Requirement “static length < 15” is needed for comparing elements.
    pub fn new(element_type: DataType) -> Self {
        Get::assert_element_type_is_supported(&element_type);

        Self { element_type }
    }
}

impl BasicSnippet for Contains {
    fn parameters(&self) -> Vec<(DataType, String)> {
        let element_type = self.element_type.clone();
        let list_type = DataType::List(Box::new(element_type.clone()));

        vec![
            (list_type, "self".to_owned()),
            (element_type, "needle".to_owned()),
        ]
    }

    fn return_values(&self) -> Vec<(DataType, String)> {
        vec![(DataType::Bool, "match_found".to_owned())]
    }

    fn entrypoint(&self) -> String {
        let element_type = self.element_type.label_friendly_name();
        format!("tasmlib_list_contains___{element_type}")
    }

    fn code(&self, library: &mut Library) -> Vec<LabelledInstruction> {
        // unwrap is fine: Self::new checks range of stack size
        let element_size = self.element_type.stack_size().try_into().unwrap();
        let needle_alloc = library.kmalloc(element_size);

        let entrypoint = self.entrypoint();
        let loop_label = format!("{entrypoint}_loop");
        let mul_with_element_size = match element_size {
            1 => triton_asm!(), // no-op
            n => triton_asm!(push {n} mul),
        };

        triton_asm!(
            // BEFORE: _ *list [value]
            // AFTER:  _ match_found
            {entrypoint}:
                push {needle_alloc.write_address()}
                {&self.element_type.write_value_to_memory_leave_pointer()}
                pop 1           // _ *list

                push 0          hint match_found: bool = stack[0]
                pick 1          // _ 0 *list

                dup 0
                read_mem 1      // _ 0 *list list_len (*list - 1)
                addi 1          // _ 0 *list list_len *list
                pick 1          // _ 0 *list *list list_len
                {&mul_with_element_size}
                                // _ 0 *list *list (list_len * elem_size)
                add             // _ 0 *list *list_last_word

                call {loop_label}
                                // _ match_found *list *list_last_word
                pop 2           // _ match_found

                return

            // INVARIANT: _ match_found *list *list[i]
            {loop_label}:
                /* loop header – all elements checked, or match found? */
                dup 1
                dup 1
                eq              // _ match_found *list *list[i] (*list == *list[i])
                dup 3
                add             // _ match_found *list *list[i] ((*list == *list[i]) || match_found)
                skiz return     // _ 0           *list *list[i]


                /* Loop body */
                {&self.element_type.read_value_from_memory_leave_pointer()}
                                // _ 0 *list [haystack_element] *list[i-1]
                place {self.element_type.stack_size()}
                                // _ 0 *list *list[i-1] [haystack_element]

                push {needle_alloc.read_address()}
                {&self.element_type.read_value_from_memory_pop_pointer()}
                                // _ 0 *list *list[i-1] [haystack_element] [needle]
                {&self.element_type.compare()}
                                // _ 0 *list *list[i-1] (haystack_element == needle)

                swap 3
                pop 1           // _ (haystack_element == needle) *list *list[i-1]
                recurse
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::library::STATIC_MEMORY_FIRST_ADDRESS;
    use crate::rust_shadowing_helper_functions::list::load_list_unstructured;
    use crate::test_helpers::test_rust_equivalence_given_complete_state;
    use crate::test_prelude::*;

    impl Contains {
        fn static_pointer_isolated_run(&self) -> BFieldElement {
            STATIC_MEMORY_FIRST_ADDRESS - bfe!(self.element_type.stack_size()) + bfe!(1)
        }

        fn prepare_state(
            &self,
            list_pointer: BFieldElement,
            mut needle: Vec<BFieldElement>,
            haystack_elements: Vec<Vec<BFieldElement>>,
        ) -> FunctionInitialState {
            let mut memory: HashMap<BFieldElement, BFieldElement> = HashMap::default();
            let list_length = haystack_elements.len();
            memory.insert(list_pointer, bfe!(list_length));
            let mut word_pointer = list_pointer;
            word_pointer.increment();
            for rand_elem in haystack_elements.iter() {
                for word in rand_elem {
                    memory.insert(word_pointer, *word);
                    word_pointer.increment();
                }
            }

            needle.reverse();
            let init_stack = [
                self.init_stack_for_isolated_run(),
                vec![list_pointer],
                needle,
            ]
            .concat();
            FunctionInitialState {
                stack: init_stack,
                memory,
            }
        }
    }

    impl Function for Contains {
        fn rust_shadow(
            &self,
            stack: &mut Vec<BFieldElement>,
            memory: &mut HashMap<BFieldElement, BFieldElement>,
        ) {
            let needle = (0..self.element_type.stack_size())
                .map(|_| stack.pop().unwrap())
                .collect_vec();

            let haystack_list_ptr = stack.pop().unwrap();
            let haystack_elems =
                load_list_unstructured(self.element_type.stack_size(), haystack_list_ptr, memory);

            stack.push(bfe!(haystack_elems.contains(&needle) as u32));

            // Write needle value to static memory
            let mut static_pointer = self.static_pointer_isolated_run();
            for word in needle {
                memory.insert(static_pointer, word);
                static_pointer.increment();
            }
        }

        fn pseudorandom_initial_state(
            &self,
            seed: [u8; 32],
            bench_case: Option<BenchmarkCase>,
        ) -> FunctionInitialState {
            let mut rng: StdRng = StdRng::from_seed(seed);
            let list_length = match bench_case {
                Some(BenchmarkCase::CommonCase) => 100,
                Some(BenchmarkCase::WorstCase) => 400,
                None => rng.random_range(1..400),
            };
            let haystack_elements = (0..list_length)
                .map(|_| self.element_type.seeded_random_element(&mut rng))
                .collect_vec();

            let list_pointer: BFieldElement = rng.random();

            let needle = match bench_case {
                Some(BenchmarkCase::CommonCase) => haystack_elements[list_length / 2].clone(),
                Some(BenchmarkCase::WorstCase) => haystack_elements[list_length / 2].clone(),
                None => {
                    // An element is guaranteed to exist, as the initial length is never 0
                    if rng.random() {
                        haystack_elements
                            .choose(&mut rng)
                            .as_ref()
                            .unwrap()
                            .to_owned()
                            .to_owned()
                    } else {
                        // Will create a false positive with rate
                        // $ list_length / element-type-value-space $. But
                        // since the rust-shadowing agrees with the TASM code,
                        // the test will not fail.
                        self.element_type.seeded_random_element(&mut rng)
                    }
                }
            };

            self.prepare_state(list_pointer, needle, haystack_elements)
        }

        fn corner_case_initial_states(&self) -> Vec<FunctionInitialState> {
            let empty_list =
                self.prepare_state(bfe!(1), bfe_vec![1; self.element_type.stack_size()], vec![]);

            let an_element = bfe_vec![42; self.element_type.stack_size()];
            let another_element = bfe_vec![420; self.element_type.stack_size()];
            let a_pointer = bfe!(42);
            let one_element_match =
                self.prepare_state(a_pointer, an_element.clone(), vec![an_element.clone()]);
            let one_element_no_match =
                self.prepare_state(a_pointer, an_element.clone(), vec![another_element.clone()]);
            let two_elements_match_first = self.prepare_state(
                a_pointer,
                an_element.clone(),
                vec![an_element.clone(), another_element.clone()],
            );
            let two_elements_match_last = self.prepare_state(
                a_pointer,
                an_element.clone(),
                vec![another_element.clone(), an_element.clone()],
            );
            let two_elements_no_match = self.prepare_state(
                a_pointer,
                an_element.clone(),
                vec![another_element.clone(), another_element.clone()],
            );
            let two_elements_both_match = self.prepare_state(
                a_pointer,
                an_element.clone(),
                vec![an_element.clone(), an_element.clone()],
            );

            let non_symmetric_value = (0..self.element_type.stack_size())
                .map(|i| bfe!(i + 200))
                .collect_vec();
            let mut mirrored_non_symmetric_value = non_symmetric_value.clone();
            mirrored_non_symmetric_value.reverse();
            let no_match_on_inverted_value_unless_size_1 = self.prepare_state(
                a_pointer,
                non_symmetric_value,
                vec![mirrored_non_symmetric_value],
            );

            vec![
                empty_list,
                one_element_match,
                one_element_no_match,
                two_elements_match_first,
                two_elements_match_last,
                two_elements_no_match,
                two_elements_both_match,
                no_match_on_inverted_value_unless_size_1,
            ]
        }
    }

    #[test]
    fn rust_shadow() {
        for element_type in [
            DataType::Bfe,
            DataType::U32,
            DataType::U64,
            DataType::Xfe,
            DataType::U128,
            DataType::Digest,
            DataType::Tuple(vec![DataType::Digest, DataType::Digest]),
        ] {
            ShadowedFunction::new(Contains::new(element_type)).test()
        }
    }

    #[test]
    fn contains_returns_true_on_contained_value() {
        let snippet = Contains::new(DataType::U64);
        let a_u64_element = bfe_vec![2, 3];
        let u64_list = vec![a_u64_element.clone()];
        let init_state = snippet.prepare_state(bfe!(0), a_u64_element, u64_list);
        let nd = NonDeterminism::default().with_ram(init_state.memory);

        let expected_final_stack = [snippet.init_stack_for_isolated_run(), bfe_vec![1]].concat();

        test_rust_equivalence_given_complete_state(
            &ShadowedFunction::new(snippet),
            &init_state.stack,
            &[],
            &nd,
            &None,
            Some(&expected_final_stack),
        );
    }

    #[test]
    fn contains_returns_false_on_mirrored_value() {
        let snippet = Contains::new(DataType::U64);
        let a_u64_element = bfe_vec![2, 3];
        let mirrored_u64_element = bfe_vec![3, 2];
        let init_state = snippet.prepare_state(bfe!(0), a_u64_element, vec![mirrored_u64_element]);
        let nd = NonDeterminism::default().with_ram(init_state.memory);

        let expected_final_stack = [snippet.init_stack_for_isolated_run(), bfe_vec![0]].concat();

        test_rust_equivalence_given_complete_state(
            &ShadowedFunction::new(Contains::new(DataType::U64)),
            &init_state.stack,
            &[],
            &nd,
            &None,
            Some(&expected_final_stack),
        );
    }
}

#[cfg(test)]
mod benches {
    use super::*;
    use crate::test_prelude::*;

    #[test]
    fn benchmark() {
        for element_type in [DataType::U64, DataType::Digest] {
            ShadowedFunction::new(Contains::new(element_type)).bench();
        }
    }
}
