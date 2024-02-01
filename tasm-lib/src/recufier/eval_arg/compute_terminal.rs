use crate::data_type::DataType;
use crate::library::Library;
use crate::list::ListType;
use crate::traits::basic_snippet::BasicSnippet;
use triton_vm::prelude::*;

struct ComputeTerminal {
    list_type: ListType,
}

impl BasicSnippet for ComputeTerminal {
    fn inputs(&self) -> Vec<(DataType, String)> {
        vec![
            (DataType::Xfe, "challenge".to_owned()),
            (DataType::Xfe, "initial".to_owned()),
            (
                DataType::List(Box::new(DataType::Bfe)),
                "symbols".to_owned(),
            ),
        ]
    }

    fn outputs(&self) -> Vec<(DataType, String)> {
        vec![(DataType::Xfe, "terminal".to_owned())]
    }

    fn entrypoint(&self) -> String {
        format!("tasm_recufier_eval_arg_compute_terminal_{}", self.list_type)
    }

    fn code(&self, _library: &mut Library) -> Vec<LabelledInstruction> {
        let entrypoint = self.entrypoint();
        let loop_label = format!("{entrypoint}_loop");

        let metadata_size_plus_one = self.list_type.metadata_size() + 1;
        triton_asm!(
            {entrypoint}:
                // _ [challenge] [initial] *symbols

                read_mem 1
                // _ [challenge] [initial] symbols_len (*symbols - 1)

                push {metadata_size_plus_one}
                add
                // _ [challenge] [initial] symbols_len *first_bfe

                swap 1
                dup 1
                // _ [challenge] [initial] *first_bfe symbols_len *first_bfe

                add
                // _ [challenge] [initial] *first_bfe (*last_bfe + 1)

                swap 1
                // _ [challenge] [initial] (*last_bfe + 1) *first_bfe
                // _ [challenge] initial_2 initial_1 initial_0 (*last_bfe + 1) *first_bfe

                swap 4
                // _ [challenge] *first_bfe initial_1 initial_0 (*last_bfe + 1) initial_2

                swap 1
                // _ [challenge] *first_bfe initial_1 initial_0 initial_2 (*last_bfe + 1)

                swap 3
                // _ [challenge] *first_bfe (*last_bfe + 1) initial_0 initial_2 initial_1

                swap 1
                // _ [challenge] *first_bfe (*last_bfe + 1) initial_0 initial_1 initial_2

                swap 2
                // _ [challenge] *first_bfe (*last_bfe + 1) initial_2 initial_1 initial_0

                call {loop_label}
                // _ [challenge] (*last_bfe + 1) (*last_bfe + 1) [acc]

                swap 5
                pop 1
                swap 5
                pop 1
                swap 5
                pop 1
                // _  [acc] (*last_bfe + 1) (*last_bfe + 1)

                pop 2
                // _  [acc]

                return

            // Invariant: [challenge] *next_bfe (*last_bfe + 1) [acc]
            {loop_label}:
                dup 4
                dup 4
                eq
                skiz
                    return
                //  [challenge] *next_bfe (*last_bfe + 1) [acc]

                dup 7
                dup 7
                dup 7
                // [challenge] *next_bfe (*last_bfe + 1) [acc] [challenge]

                xxmul
                // [challenge] *next_bfe (*last_bfe + 1) [acc * challenge]

                dup 4
                read_mem 1
                // [challenge] *next_bfe (*last_bfe + 1) [acc * challenge] next_bfe (*next_bfe - 1)

                push 2
                add
                // [challenge] *next_bfe (*last_bfe + 1) [acc * challenge] next_bfe *next_bfe'

                swap 6
                pop 1
                // [challenge] *next_bfe' (*last_bfe + 1) [acc * challenge] next_bfe

                add
                // [challenge] *next_bfe' (*last_bfe + 1) [acc * challenge + next_bfe]

                // [challenge] *next_bfe' (*last_bfe + 1) [acc']
                recurse
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::snippet_bencher::BenchmarkCase;
    use crate::traits::function::Function;
    use crate::traits::function::FunctionInitialState;
    use crate::traits::function::ShadowedFunction;
    use crate::traits::rust_shadow::RustShadow;
    use itertools::Itertools;
    use num_traits::One;
    use num_traits::Zero;
    use rand::rngs::StdRng;
    use rand::Rng;
    use rand::SeedableRng;
    use std::collections::HashMap;
    use triton_vm::table::cross_table_argument::CrossTableArg;
    use triton_vm::table::cross_table_argument::EvalArg;

    impl Function for ComputeTerminal {
        fn rust_shadow(
            &self,
            stack: &mut Vec<BFieldElement>,
            memory: &mut std::collections::HashMap<BFieldElement, BFieldElement>,
        ) {
            let symbols_pointer = stack.pop().unwrap();
            let initial = XFieldElement::new([
                stack.pop().unwrap(),
                stack.pop().unwrap(),
                stack.pop().unwrap(),
            ]);
            let challenge = XFieldElement::new([
                stack.pop().unwrap(),
                stack.pop().unwrap(),
                stack.pop().unwrap(),
            ]);
            let mut symbols_elem_pointer =
                symbols_pointer + BFieldElement::new(self.list_type.metadata_size() as u64);

            let symbols_length = memory[&symbols_pointer].value();
            let mut symbols = vec![];
            for _ in 0..symbols_length {
                symbols.push(memory[&symbols_elem_pointer]);
                symbols_elem_pointer.increment()
            }

            let result = EvalArg::compute_terminal(&symbols, initial, challenge);

            for elem in result.coefficients.into_iter().rev() {
                stack.push(elem);
            }
        }

        fn pseudorandom_initial_state(
            &self,
            seed: [u8; 32],
            bench_case: Option<BenchmarkCase>,
        ) -> FunctionInitialState {
            let mut rng: StdRng = SeedableRng::from_seed(seed);
            let symbols_length = match bench_case {
                Some(BenchmarkCase::CommonCase) => 30,
                Some(BenchmarkCase::WorstCase) => 100,
                None => rng.gen_range(0..100),
            };
            let symbols_pointer = rng.gen();
            let rand_initial = rng.gen();
            let rand_challenge = rng.gen();
            self.prepare_state(
                symbols_length,
                symbols_pointer,
                rand_initial,
                rand_challenge,
            )
        }

        fn corner_case_initial_states(&self) -> Vec<FunctionInitialState> {
            let mut rng: StdRng = SeedableRng::from_seed([101u8; 32]);
            let symbols_pointer = rng.gen();
            let rand_initial = rng.gen();
            let rand_challenge = rng.gen();
            vec![
                self.prepare_state(0, symbols_pointer, rand_initial, rand_challenge),
                self.prepare_state(1, symbols_pointer, rand_initial, rand_challenge),
                self.prepare_state(2, symbols_pointer, rand_initial, rand_challenge),
                self.prepare_state(3, symbols_pointer, rand_initial, rand_challenge),
                self.prepare_state(4, symbols_pointer, rand_initial, rand_challenge),
                self.prepare_state(
                    0,
                    symbols_pointer,
                    XFieldElement::zero(),
                    XFieldElement::zero(),
                ),
                self.prepare_state(
                    3,
                    symbols_pointer,
                    XFieldElement::zero(),
                    XFieldElement::zero(),
                ),
                self.prepare_state(
                    3,
                    symbols_pointer,
                    XFieldElement::one(),
                    XFieldElement::one(),
                ),
            ]
        }
    }

    impl ComputeTerminal {
        fn prepare_state(
            &self,
            symbols_length: usize,
            symbols_pointer: BFieldElement,
            initial: XFieldElement,
            challenge: XFieldElement,
        ) -> FunctionInitialState {
            let initial = initial.coefficients.into_iter().rev().collect_vec();
            let challenge = challenge.coefficients.into_iter().rev().collect_vec();

            let mut init_memory = HashMap::default();
            self.list_type.rust_shadowing_insert_random_list(
                &DataType::Bfe,
                symbols_pointer,
                symbols_length,
                &mut init_memory,
            );
            let init_stack = [
                self.init_stack_for_isolated_run(),
                challenge,
                initial,
                vec![symbols_pointer],
            ]
            .concat();

            FunctionInitialState {
                stack: init_stack,
                memory: init_memory,
            }
        }
    }

    #[test]
    fn eval_compute_terminal_test_safe_lists() {
        ShadowedFunction::new(ComputeTerminal {
            list_type: ListType::Safe,
        })
        .test()
    }

    #[test]
    fn eval_compute_terminal_test_unsafe_lists() {
        ShadowedFunction::new(ComputeTerminal {
            list_type: ListType::Unsafe,
        })
        .test()
    }
}

#[cfg(test)]
mod bench {
    use super::*;
    use crate::traits::function::ShadowedFunction;
    use crate::traits::rust_shadow::RustShadow;

    #[test]
    fn compute_terminal_unsafe_lists() {
        ShadowedFunction::new(ComputeTerminal {
            list_type: ListType::Unsafe,
        })
        .bench()
    }

    #[test]
    fn compute_terminal_safe_lists() {
        ShadowedFunction::new(ComputeTerminal {
            list_type: ListType::Safe,
        })
        .bench()
    }
}
