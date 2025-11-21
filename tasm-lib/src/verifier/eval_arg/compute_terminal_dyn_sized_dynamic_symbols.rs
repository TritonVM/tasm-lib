use triton_vm::prelude::*;

use crate::list::LIST_METADATA_SIZE;
use crate::prelude::*;

#[derive(Debug, Default, Copy, Clone, Eq, PartialEq, Hash)]
pub struct ComputeTerminalDynSizedDynamicSymbols;

impl BasicSnippet for ComputeTerminalDynSizedDynamicSymbols {
    fn parameters(&self) -> Vec<(DataType, String)> {
        vec![
            (DataType::Xfe, "challenge".to_owned()),
            (DataType::Xfe, "initial".to_owned()),
            (
                DataType::List(Box::new(DataType::Bfe)),
                "symbols".to_owned(),
            ),
        ]
    }

    fn return_values(&self) -> Vec<(DataType, String)> {
        vec![(DataType::Xfe, "terminal".to_owned())]
    }

    fn entrypoint(&self) -> String {
        "tasmlib_verifier_eval_arg_compute_terminal_dyn_sized_dynamic_symbols".into()
    }

    fn code(&self, _library: &mut Library) -> Vec<LabelledInstruction> {
        let entrypoint = self.entrypoint();
        let loop_label = format!("{entrypoint}_loop");

        let metadata_size_plus_one = LIST_METADATA_SIZE + 1;
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

                xx_mul
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
    use num_traits::One;
    use num_traits::Zero;
    use triton_vm::air::cross_table_argument::CrossTableArg;
    use triton_vm::air::cross_table_argument::EvalArg;

    use super::*;
    use crate::rust_shadowing_helper_functions::list::insert_random_list;
    use crate::test_prelude::*;

    impl Function for ComputeTerminalDynSizedDynamicSymbols {
        fn rust_shadow(
            &self,
            stack: &mut Vec<BFieldElement>,
            memory: &mut HashMap<BFieldElement, BFieldElement>,
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
                symbols_pointer + BFieldElement::new(LIST_METADATA_SIZE as u64);

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
            let mut rng = StdRng::from_seed(seed);
            let symbols_length = match bench_case {
                Some(BenchmarkCase::CommonCase) => 30,
                Some(BenchmarkCase::WorstCase) => 100,
                None => rng.random_range(0..100),
            };
            let symbols_pointer = rng.random();
            let rand_initial = rng.random();
            let rand_challenge = rng.random();
            self.prepare_state(
                symbols_length,
                symbols_pointer,
                rand_initial,
                rand_challenge,
            )
        }

        fn corner_case_initial_states(&self) -> Vec<FunctionInitialState> {
            let mut rng = StdRng::from_seed([101u8; 32]);
            let symbols_pointer = rng.random();
            let rand_initial = rng.random();
            let rand_challenge = rng.random();
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

    impl ComputeTerminalDynSizedDynamicSymbols {
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
            insert_random_list(
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
    fn eval_compute_terminal_test() {
        ShadowedFunction::new(ComputeTerminalDynSizedDynamicSymbols).test()
    }
}

#[cfg(test)]
mod bench {
    use super::*;
    use crate::test_prelude::*;

    #[test]
    fn benchmark() {
        ShadowedFunction::new(ComputeTerminalDynSizedDynamicSymbols).bench()
    }
}
