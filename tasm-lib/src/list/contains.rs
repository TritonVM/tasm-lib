use crate::data_type::DataType;
use crate::traits::basic_snippet::BasicSnippet;
use crate::Library;
use triton_vm::prelude::*;

/// Returns `true` if the list contains an element with the given value.
///
/// This operation is *O*(*n*).
///
/// Mirrors the `contains` method from Rust `core` as closely as possible.
pub struct Contains {
    pub element_type: DataType,
}

impl BasicSnippet for Contains {
    fn inputs(&self) -> Vec<(DataType, String)> {
        vec![
            (
                DataType::List(Box::new(self.element_type.clone())),
                "self".to_owned(),
            ),
            // `x` denotes the value you're looking for, the "needle". This
            // name is taken from the rust-implementation of `contains`.
            (self.element_type.clone(), "x".to_owned()),
        ]
    }

    fn outputs(&self) -> Vec<(DataType, String)> {
        vec![(DataType::Bool, "match_found".to_owned())]
    }

    fn entrypoint(&self) -> String {
        format!(
            "tasmlib_list_contains___{}",
            self.element_type.label_friendly_name()
        )
    }

    fn code(&self, library: &mut Library) -> Vec<LabelledInstruction> {
        let entrypoint = self.entrypoint();
        let elem_size: u32 = self.element_type.stack_size().try_into().unwrap();
        let elem_size_plus_one = elem_size + 1;

        let needle_pointer_write = library.kmalloc(self.element_type.stack_size() as u32);
        let needle_pointer_read = needle_pointer_write + bfe!(elem_size - 1);

        let loop_label = format!("{entrypoint}_loop");

        let read_element = self.element_type.read_value_from_memory_leave_pointer();
        let eq_code = self.element_type.compare();

        let loop_code = triton_asm!(
            // INVARIANT: _ match_found *list *list[i]
            /* loop's end-condition: all elements checked, or a match is found */
            {loop_label}:
                /* Loop header */
                dup 1 dup 1 eq
                // _ match_found *list *list[i] (*list == *list[i])

                skiz return
                // _ match_found *list *list[i]

                dup 2
                // _ match_found *list *list[i] match_found

                skiz return
                // _ 0 *list *list[i]


                /* Loop body */
                dup 0
                {&read_element}
                // _ 0 *list *list[i] [haystack] *list[i-1]

                swap {elem_size_plus_one}
                pop 1
                // _ 0 *list *list[i-1] [haystack]

                push {needle_pointer_read}
                {&read_element}
                pop 1
                // _ 0 *list *list[i-1] [haystack] [needle]

                {&eq_code}
                // _ 0 *list *list[i-1] (haystack == needle)

                swap 3
                pop 1
                // _ (haystack == needle) *list *list[i-1]


                recurse
        );

        let write_element = self.element_type.write_value_to_memory_leave_pointer();
        let mul_with_elem_size = match elem_size {
            1 => triton_asm!(),
            n => triton_asm!(push {n} mul),
        };

        triton_asm!(
            // BEFORE: _ *list [value]
            // AFTER: match_found
            {entrypoint}:
                push {needle_pointer_write}
                {&write_element}
                pop 1
                // _ *list

                push 0
                swap 1
                // _ 0 *list

                dup 0
                read_mem 1
                // _ 0 *list list_len (*list - 1)

                push 1 add
                // _ 0 *list list_len *list

                swap 1
                // _ 0 *list *list list_len

                {&mul_with_elem_size}
                // _ 0 *list *list (list_len * elem_size)

                add
                // _ 0 *list *list_last_word

                call {loop_label}
                // _ match_found *list *list_last_word

                pop 2
                // _ match_found

                return

                {&loop_code}
        )
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use itertools::Itertools;
    use num::One;
    use num::Zero;
    use rand::rngs::StdRng;
    use rand::seq::SliceRandom;
    use rand::Rng;
    use rand::SeedableRng;

    use crate::library::STATIC_MEMORY_START_ADDRESS;
    use crate::rust_shadowing_helper_functions::list::load_list_unstructured;
    use crate::snippet_bencher::BenchmarkCase;
    use crate::test_helpers::test_rust_equivalence_given_complete_state;
    use crate::traits::function::Function;
    use crate::traits::function::FunctionInitialState;
    use crate::traits::function::ShadowedFunction;
    use crate::traits::rust_shadow::RustShadow;

    use super::*;

    impl Contains {
        fn static_pointer_isolated_run(&self) -> BFieldElement {
            STATIC_MEMORY_START_ADDRESS - bfe!(self.element_type.stack_size() as u64)
                + BFieldElement::one()
        }

        fn prepare_state(
            &self,
            list_pointer: BFieldElement,
            mut needle: Vec<BFieldElement>,
            haystack_elements: Vec<Vec<BFieldElement>>,
        ) -> FunctionInitialState {
            let mut memory: HashMap<BFieldElement, BFieldElement> = HashMap::default();
            let list_length = haystack_elements.len();
            memory.insert(list_pointer, bfe!(list_length as u64));
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

    #[test]
    fn contains_returns_true_on_contained_value() {
        let snippet = Contains {
            element_type: DataType::U64,
        };
        let a_u64_element = vec![bfe!(2), bfe!(3)];
        let u64_list = vec![a_u64_element.clone()];
        let init_state = snippet.prepare_state(BFieldElement::zero(), a_u64_element, u64_list);
        let nd = NonDeterminism::default().with_ram(init_state.memory);

        let expected_final_stack = [
            snippet.init_stack_for_isolated_run(),
            vec![BFieldElement::one()],
        ]
        .concat();

        test_rust_equivalence_given_complete_state(
            &ShadowedFunction::new(Contains {
                element_type: DataType::U64,
            }),
            &init_state.stack,
            &[],
            &nd,
            &None,
            Some(&expected_final_stack),
        );
    }

    #[test]
    fn contains_returns_false_on_mirrored_value() {
        let snippet = Contains {
            element_type: DataType::U64,
        };
        let a_u64_element = vec![bfe!(2), bfe!(3)];
        let mirrored_u64_element = vec![bfe!(3), bfe!(2)];
        let init_state = snippet.prepare_state(
            BFieldElement::zero(),
            a_u64_element,
            vec![mirrored_u64_element],
        );
        let nd = NonDeterminism::default().with_ram(init_state.memory);

        let expected_final_stack = [
            snippet.init_stack_for_isolated_run(),
            vec![BFieldElement::zero()],
        ]
        .concat();

        test_rust_equivalence_given_complete_state(
            &ShadowedFunction::new(Contains {
                element_type: DataType::U64,
            }),
            &init_state.stack,
            &[],
            &nd,
            &None,
            Some(&expected_final_stack),
        );
    }

    #[test]
    fn contains_pbt() {
        for element_type in [
            DataType::Bfe,
            DataType::U32,
            DataType::U64,
            DataType::Xfe,
            DataType::U128,
            DataType::Digest,
            DataType::Tuple(vec![DataType::Digest, DataType::Digest]),
        ] {
            ShadowedFunction::new(Contains { element_type }).test()
        }
    }

    impl Function for Contains {
        fn rust_shadow(
            &self,
            stack: &mut Vec<BFieldElement>,
            memory: &mut HashMap<BFieldElement, BFieldElement>,
        ) {
            let mut needle = vec![];
            for _ in 0..self.element_type.stack_size() {
                needle.push(stack.pop().unwrap());
            }

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
                None => rng.gen_range(1..400),
            };
            let haystack_elements = self
                .element_type
                .seeded_random_elements(list_length, &mut rng);

            let list_pointer: BFieldElement = rng.gen();

            let needle = match bench_case {
                Some(BenchmarkCase::CommonCase) => haystack_elements[list_length / 2].clone(),
                Some(BenchmarkCase::WorstCase) => haystack_elements[list_length / 2].clone(),
                None => {
                    let expect_match = rng.gen_bool(0.5);
                    // An element is guaranteed to exist, as the initial length is never 0
                    if expect_match {
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
                        self.element_type.seeded_random_elements(1, &mut rng)[0].clone()
                    }
                }
            };

            self.prepare_state(list_pointer, needle, haystack_elements)
        }

        fn corner_case_initial_states(&self) -> Vec<FunctionInitialState> {
            let empty_list = self.prepare_state(
                BFieldElement::one(),
                vec![BFieldElement::one(); self.element_type.stack_size()],
                vec![],
            );

            let an_element = vec![BFieldElement::new(42); self.element_type.stack_size()];
            let another_element = vec![BFieldElement::new(420); self.element_type.stack_size()];
            let a_pointer = BFieldElement::new(42);
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
                .map(|i| bfe!(i as u64 + 200))
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
}

#[cfg(test)]
mod benches {
    use crate::traits::function::ShadowedFunction;
    use crate::traits::rust_shadow::RustShadow;

    use super::*;

    #[test]
    fn contains_bench() {
        ShadowedFunction::new(Contains {
            element_type: DataType::U64,
        })
        .bench();
        ShadowedFunction::new(Contains {
            element_type: DataType::Digest,
        })
        .bench();
    }
}
