use triton_vm::prelude::*;

use crate::prelude::*;

/// Calculate the sum of the `BFieldElement`s in a list
#[allow(dead_code)]
#[derive(Debug, Default, Copy, Clone, Eq, PartialEq, Hash)]
struct SumOfBfes;

impl BasicSnippet for SumOfBfes {
    fn parameters(&self) -> Vec<(DataType, String)> {
        vec![(
            // For naming the input argument, I just follow what `Rust` calls this argument
            DataType::List(Box::new(DataType::Bfe)),
            "self".to_owned(),
        )]
    }

    fn return_values(&self) -> Vec<(DataType, String)> {
        vec![(DataType::Bfe, "sum".to_owned())]
    }

    fn entrypoint(&self) -> String {
        format!("tasmlib_list_sum_{}", DataType::Bfe.label_friendly_name())
    }

    fn code(&self, _library: &mut Library) -> Vec<LabelledInstruction> {
        let entrypoint = self.entrypoint();
        let accumulate_five_elements_loop_label = format!("{entrypoint}_acc_5_elements");

        let accumulate_five_elements_loop = triton_asm!(
            // Invariant: _ *end_loop *element acc
            {accumulate_five_elements_loop_label}:

                dup 2
                dup 2
                eq
                skiz
                    return
                // _ *end_loop *element acc

                dup 1
                read_mem 5
                // _ *end_loop *element acc [elements] (*element - 5)

                swap 7
                pop 1
                // _ *end_loop (*element - 5) acc [elements]
                // _ *end_loop *element' acc [elements]

                add
                add
                add
                add
                add
                // _ *end_loop *element' acc'

                recurse
        );

        let accumulate_one_element_loop_label = format!("{entrypoint}_acc_1_element");
        let accumulate_one_element_loop = triton_asm!(
            // Invariant: _ *end_loop *element acc
            {accumulate_one_element_loop_label}:
                dup 2
                dup 2
                eq
                skiz
                    return
                // _ *end_loop *element acc

                dup 1
                read_mem 1
                swap 3
                pop 1
                // _ *end_loop (*element - 1) acc element

                add
                // _ *end_loop *element' acc'

                recurse
        );

        triton_asm!(
            {entrypoint}:
                // _ *list

                // Get pointer to last element
                dup 0
                read_mem 1
                // _ *list length (*list - 1)

                pop 1
                // _ *list length

                dup 1
                dup 1
                add
                // _ *list length *last_element

                // Get pointer to *end_loop that is the loop termination condition

                push 5
                dup 2
                // _ *list length *last_element 5 length

                div_mod
                // _ *list length *last_element (length / 5) (length % 5)

                swap 1
                pop 1
                // _ *list length *last_element (length % 5)

                dup 3
                add
                // _ *list length *last_element *element[length % 5]
                // _ *list length *last_element *end_loop

                swap 1
                push 0
                // _ *list length *end_loop *last_element 0

                call {accumulate_five_elements_loop_label}
                // _ *list length *end_loop *next_element sum

                swap 1
                // _ *list length *end_loop sum *next_element

                swap 3
                // _ *list *next_element *end_loop sum length

                pop 1
                // _ *list *next_element *end_loop sum

                swap 1
                pop 1
                // _ *list *next_element sum

                call {accumulate_one_element_loop_label}
                // _ *list *list sum

                swap 2
                pop 2
                // _ sum

                return

                {&accumulate_five_elements_loop}
                {&accumulate_one_element_loop}
        )
    }
}

#[cfg(test)]
mod tests {
    use num_traits::Zero;

    use super::*;
    use crate::rust_shadowing_helper_functions::list::insert_random_list;
    use crate::rust_shadowing_helper_functions::list::load_list_with_copy_elements;
    use crate::test_prelude::*;

    impl Function for SumOfBfes {
        fn rust_shadow(
            &self,
            stack: &mut Vec<BFieldElement>,
            memory: &mut HashMap<BFieldElement, BFieldElement>,
        ) {
            const BFIELDELEMENT_SIZE: usize = 1;
            let list_pointer = stack.pop().unwrap();
            let list = load_list_with_copy_elements::<BFIELDELEMENT_SIZE>(list_pointer, memory);

            let sum: BFieldElement = list.into_iter().map(|x| x[0]).sum();
            stack.push(sum);
        }

        fn pseudorandom_initial_state(
            &self,
            seed: [u8; 32],
            bench_case: Option<BenchmarkCase>,
        ) -> FunctionInitialState {
            let mut rng = StdRng::from_seed(seed);
            let list_pointer = BFieldElement::new(rng.random());
            let list_length = match bench_case {
                Some(BenchmarkCase::CommonCase) => 104,
                Some(BenchmarkCase::WorstCase) => 1004,
                None => rng.random_range(0..200),
            };
            self.prepare_state(list_pointer, list_length)
        }

        fn corner_case_initial_states(&self) -> Vec<FunctionInitialState> {
            (0..13)
                .map(|len| self.prepare_state(BFieldElement::zero(), len))
                .collect_vec()
        }
    }

    impl SumOfBfes {
        fn prepare_state(
            &self,
            list_pointer: BFieldElement,
            list_length: usize,
        ) -> FunctionInitialState {
            let mut memory = HashMap::default();
            insert_random_list(&DataType::Bfe, list_pointer, list_length, &mut memory);

            let mut init_stack = self.init_stack_for_isolated_run();
            init_stack.push(list_pointer);
            FunctionInitialState {
                stack: init_stack,
                memory,
            }
        }
    }

    #[test]
    fn sum_bfes_pbt() {
        ShadowedFunction::new(SumOfBfes).test()
    }
}

#[cfg(test)]
mod benches {
    use super::*;
    use crate::test_prelude::*;

    #[test]
    fn benchmark() {
        ShadowedFunction::new(SumOfBfes).bench();
    }
}
