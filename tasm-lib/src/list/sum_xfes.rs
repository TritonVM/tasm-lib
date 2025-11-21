use triton_vm::prelude::*;
use twenty_first::math::x_field_element::EXTENSION_DEGREE;

use crate::prelude::*;

/// Calculate the sum of the `XFieldElement`s in a list
#[derive(Debug, Default, Copy, Clone, Eq, PartialEq, Hash)]
#[allow(dead_code)]
struct SumOfXfes;

impl BasicSnippet for SumOfXfes {
    fn parameters(&self) -> Vec<(DataType, String)> {
        vec![(
            // For naming the input argument, I just follow what `Rust` calls this argument in
            // its `sum` method.
            DataType::List(Box::new(DataType::Xfe)),
            "self".to_owned(),
        )]
    }

    fn return_values(&self) -> Vec<(DataType, String)> {
        vec![(DataType::Xfe, "sum".to_owned())]
    }

    fn entrypoint(&self) -> String {
        format!("tasmlib_list_sum_{}", DataType::Xfe.label_friendly_name())
    }

    fn code(&self, _library: &mut Library) -> Vec<LabelledInstruction> {
        assert_eq!(
            3, EXTENSION_DEGREE,
            "Code only works for extension degree = 3, got: {EXTENSION_DEGREE}"
        );
        let entrypoint = self.entrypoint();
        let accumulate_5_elements_loop_label = format!("{entrypoint}_acc_5_elements_loop");
        let accumulate_5_elements_loop = triton_asm!(
            // Invariant: _ *end_loop *element_last_word [acc; 3]
            {accumulate_5_elements_loop_label}:
                // _ *end_loop *element_last_word [acc; 3]
                dup 4
                dup 4
                eq
                skiz
                    return
                // _ *end_loop *element_last_word [acc]

                dup 3
                read_mem 5
                read_mem 5
                read_mem 5
                // _ *end_loop *element_last_word [acc] [elem_4] [elem_3] [elem_2] [elem_1] [elem_0] (*element_last_word - 15)

                pop 1
                // _ *end_loop *element_last_word [acc] [elem_4] [elem_3] [elem_2] [elem_1] [elem_0]

                xx_add
                xx_add
                xx_add
                xx_add
                xx_add
                // _ *end_loop *element_last_word [acc']

                swap 3
                push -15
                add
                swap 3
                // _ *end_loop *element_last_word' [acc']

                recurse
        );

        let accumulate_one_element_loop_label = format!("{entrypoint}_acc_1_element_loop");
        let accumulate_one_element_loop = triton_asm!(
            // Invariant: _ *end_loop *element_last_word [acc; 3]
            {accumulate_one_element_loop_label}:
                // _ *end_loop *element_last_word [acc; 3]
                dup 4
                dup 4
                eq
                skiz
                    return
                // _ *end_loop *element_last_word [acc]

                dup 3
                read_mem 3
                // _ *end_loop *element_last_word [acc; 3] [element_last_words; 3] (*element_last_word - 3)

                swap 7
                pop 1
                // _ *end_loop (*element_last_word - 3) [acc; 3] [element_last_words; 3]

                xx_add
                // _ *end_loop *element_last_word' [acc']

                recurse
        );

        let offset_for_last_word = triton_asm!(
            // _ len
            push 3
            mul // _ offset_last_word
        );

        triton_asm!(
            {entrypoint}:
                // _ *list

                // Calculate pointer to last element
                dup 0
                read_mem 1
                // _ *list len (*list - 1)

                pop 1
                // _ *list len

                {&offset_for_last_word}
                // _ *list offset_last_word

                dup 1
                add
                // _ *list *last_word

                // Get pointer to *end_loop that is the loop termination condition
                push 5
                dup 2
                // _ *list *last_word 5 *list

                read_mem 1
                pop 1
                // _ *list *last_word 5 len

                div_mod
                // _ *list *last_word (len / 5) (len % 5)

                swap 1
                pop 1
                // _ *list *last_word (len % 5)

                push {EXTENSION_DEGREE}
                mul
                // _ *list *last_word ((len % 5) * 3)

                dup 2
                add
                // _ *list *last_word ((len % 5) * 3 + *list)
                // _ *list *last_word *end_5_loop

                swap 1
                push 0
                push 0
                push 0
                // _ *list *end_5_loop *last_word [acc]

                call {accumulate_5_elements_loop_label}
                // _ *list *end_5_loop *end_5_loop [acc]

                swap 1
                swap 2
                swap 3
                pop 1
                // _ *list *end_5_loop [acc]
                // _ *end_condition_1_loop *end_5_loop [acc]

                call {accumulate_one_element_loop_label}
                // _ *end_condition_1_loop *end_5_loop [acc]

                swap 2
                swap 4
                pop 1
                swap 2
                pop 1
                // _ [acc]

                return

                {&accumulate_one_element_loop}
                {&accumulate_5_elements_loop}
        )
    }
}

#[cfg(test)]
mod tests {
    use twenty_first::math::x_field_element::EXTENSION_DEGREE;

    use super::*;
    use crate::rust_shadowing_helper_functions::list::insert_random_list;
    use crate::rust_shadowing_helper_functions::list::load_list_with_copy_elements;
    use crate::test_helpers::test_rust_equivalence_given_complete_state;
    use crate::test_prelude::*;

    impl Function for SumOfXfes {
        fn rust_shadow(
            &self,
            stack: &mut Vec<BFieldElement>,
            memory: &mut HashMap<BFieldElement, BFieldElement>,
        ) {
            let list_pointer = stack.pop().unwrap();
            let list = load_list_with_copy_elements::<EXTENSION_DEGREE>(list_pointer, memory);

            let sum: XFieldElement = list
                .into_iter()
                .map(|x| XFieldElement::new([x[0], x[1], x[2]]))
                .sum();
            for elem in sum.coefficients.into_iter().rev() {
                stack.push(elem);
            }
        }

        fn pseudorandom_initial_state(
            &self,
            seed: [u8; 32],
            bench_case: Option<BenchmarkCase>,
        ) -> FunctionInitialState {
            let mut rng = StdRng::from_seed(seed);
            let list_pointer = rng.random();
            let list_length = match bench_case {
                Some(BenchmarkCase::CommonCase) => 104,
                Some(BenchmarkCase::WorstCase) => 1004,
                None => rng.random_range(0..200),
            };
            self.prepare_state(list_pointer, list_length)
        }

        fn corner_case_initial_states(&self) -> Vec<FunctionInitialState> {
            (0..13)
                .map(|len| self.prepare_state(BFieldElement::new((1u64 << 44) + 1313), len))
                .collect_vec()
        }
    }

    impl SumOfXfes {
        fn prepare_state(
            &self,
            list_pointer: BFieldElement,
            list_length: usize,
        ) -> FunctionInitialState {
            let mut memory = HashMap::default();
            insert_random_list(&DataType::Xfe, list_pointer, list_length, &mut memory);

            let mut init_stack = self.init_stack_for_isolated_run();
            init_stack.push(list_pointer);
            FunctionInitialState {
                stack: init_stack,
                memory,
            }
        }
    }

    #[test]
    fn sum_xfes_pbt() {
        ShadowedFunction::new(SumOfXfes).test()
    }

    #[test]
    fn sum_xfes_unit_test() {
        let snippet = SumOfXfes;
        let input_list_2_long: Vec<XFieldElement> = vec![rand::random(), rand::random()];
        let expected_sum: XFieldElement = input_list_2_long.clone().into_iter().sum();

        let mut memory = HashMap::default();
        let list_pointer = BFieldElement::new(1u64 << 33);
        insert_xfe_list_into_memory(list_pointer, input_list_2_long, &mut memory);
        let init_stack = [snippet.init_stack_for_isolated_run(), vec![list_pointer]].concat();
        let expected_final_stack = [
            snippet.init_stack_for_isolated_run(),
            vec![
                expected_sum.coefficients[2],
                expected_sum.coefficients[1],
                expected_sum.coefficients[0],
            ],
        ]
        .concat();

        test_rust_equivalence_given_complete_state(
            &ShadowedFunction::new(snippet),
            &init_stack,
            &[],
            &NonDeterminism::default().with_ram(memory),
            &None,
            Some(&expected_final_stack),
        );
    }

    fn insert_xfe_list_into_memory(
        list_pointer: BFieldElement,
        list: Vec<XFieldElement>,
        memory: &mut HashMap<BFieldElement, BFieldElement>,
    ) {
        let mut pointer = list_pointer;
        memory.insert(pointer, BFieldElement::new(list.len() as u64));
        pointer.increment();
        for xfe in list.iter() {
            for bfe in xfe.coefficients.iter() {
                memory.insert(pointer, *bfe);
                pointer.increment();
            }
        }
    }
}

#[cfg(test)]
mod benches {
    use super::*;
    use crate::test_prelude::*;

    #[test]
    fn benchmark() {
        ShadowedFunction::new(SumOfXfes).bench();
    }
}
