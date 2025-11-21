use triton_vm::prelude::*;

use crate::data_type::ArrayType;
use crate::prelude::*;

/// A snippet for calculating terminal value by traversing the symbols
/// list in reverse order and where symbols list length is statically
/// known. Produces verbose code but minimizes clock cycle count.
#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub struct ComputeTerminalConstSizedDynamicSymbolsReversed<const N: usize>;

impl<const N: usize> BasicSnippet for ComputeTerminalConstSizedDynamicSymbolsReversed<N> {
    fn parameters(&self) -> Vec<(DataType, String)> {
        vec![
            (DataType::Xfe, "challenge".to_owned()),
            (DataType::Xfe, "initial".to_owned()),
            (
                DataType::Array(Box::new(ArrayType {
                    element_type: DataType::Bfe,
                    length: N,
                })),
                "symbols".to_owned(),
            ),
        ]
    }

    fn return_values(&self) -> Vec<(DataType, String)> {
        vec![(DataType::Xfe, "terminal".to_owned())]
    }

    fn entrypoint(&self) -> String {
        format!(
            "tasmlib_verifier_eval_arg_compute_terminal_\
            const_sized_dynamic_symbols_reversed_sym_len_{N}"
        )
    }

    fn code(&self, _library: &mut Library) -> Vec<LabelledInstruction> {
        assert!(
            N > 0,
            "Cannot generate code for const-symbols length of zero"
        );

        let entrypoint = self.entrypoint();
        let one_iteration = triton_asm!(
                // _ [challenge] *symbol [acc]

                dup 6
                dup 6
                dup 6
                xx_mul
                // _ [challenge] *symbol [acc * challenge]

                dup 3
                read_mem 1
                // _ [challenge] *symbol [acc * challenge] symbol *symbol'

                swap 5
                pop 1
                // _ [challenge] *symbol' [acc * challenge] symbol

                add
                // _ [challenge] *previous_elem [acc * challenge + symbol]
                // _ [challenge] *previous_elem [acc']
        );
        let iterations = vec![one_iteration; N].concat();
        triton_asm!(
            // _ [challenge] [initial] *symbols
            {entrypoint}:
                push {N - 1}
                add
                // _ [challenge] [initial] *last_symbol
                // _ [challenge] init_2 init_1 init_0 *last_symbol

                swap 3
                // _ [challenge] *last_symbol init_1 init_0 init_2

                swap 2
                // _ [challenge] *last_symbol init_2 init_0 init_1

                swap 1
                // _ [challenge] *last_symbol init_2 init_1 init_0
                // _ [challenge] *last_symbol [initial]

                {&iterations}
                 // _ [challenge] (*first_symbol - 1) [terminal]

                swap 4 pop 1
                swap 4 pop 1
                swap 4 pop 1
                // _ [terminal] (*first_symbol - 1)

                pop 1
                // _ [terminal]

                return
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
    use crate::test_prelude::*;

    impl<const N: usize> Function for ComputeTerminalConstSizedDynamicSymbolsReversed<N> {
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
            let mut symbols_elem_pointer = symbols_pointer + BFieldElement::new(N as u64 - 1);

            let mut symbols = vec![];
            for _ in 0..N {
                symbols.push(memory[&symbols_elem_pointer]);
                symbols_elem_pointer.decrement();
            }

            let result = EvalArg::compute_terminal(&symbols, initial, challenge);

            for elem in result.coefficients.into_iter().rev() {
                stack.push(elem);
            }
        }

        fn pseudorandom_initial_state(
            &self,
            seed: [u8; 32],
            _bench_case: Option<BenchmarkCase>,
        ) -> FunctionInitialState {
            let mut rng = StdRng::from_seed(seed);
            let symbols_pointer = rng.random();
            let rand_initial = rng.random();
            let rand_challenge = rng.random();
            self.prepare_state(symbols_pointer, rand_initial, rand_challenge)
        }

        fn corner_case_initial_states(&self) -> Vec<FunctionInitialState> {
            let mut rng = StdRng::from_seed([101u8; 32]);
            let symbols_pointer = rng.random();
            vec![
                self.prepare_state(
                    symbols_pointer,
                    XFieldElement::zero(),
                    XFieldElement::zero(),
                ),
                self.prepare_state(symbols_pointer, XFieldElement::one(), XFieldElement::zero()),
                self.prepare_state(symbols_pointer, XFieldElement::zero(), XFieldElement::one()),
                self.prepare_state(symbols_pointer, XFieldElement::one(), XFieldElement::one()),
            ]
        }
    }

    impl<const N: usize> ComputeTerminalConstSizedDynamicSymbolsReversed<N> {
        fn prepare_state(
            &self,
            symbols_pointer: BFieldElement,
            initial: XFieldElement,
            challenge: XFieldElement,
        ) -> FunctionInitialState {
            let initial = initial.coefficients.into_iter().rev().collect_vec();
            let challenge = challenge.coefficients.into_iter().rev().collect_vec();

            let mut init_memory = HashMap::default();
            for i in 0..N {
                let i = BFieldElement::new(i as u64);
                init_memory.insert(symbols_pointer + i, i + BFieldElement::new(404040));
            }
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
    fn eval_compute_terminal_test_rev_const_sized_1() {
        ShadowedFunction::new(ComputeTerminalConstSizedDynamicSymbolsReversed::<1>).test()
    }

    #[test]
    fn eval_compute_terminal_test_rev_const_sized_2() {
        ShadowedFunction::new(ComputeTerminalConstSizedDynamicSymbolsReversed::<2>).test()
    }

    #[test]
    fn eval_compute_terminal_test_rev_const_sized_3() {
        ShadowedFunction::new(ComputeTerminalConstSizedDynamicSymbolsReversed::<3>).test()
    }

    #[test]
    fn eval_compute_terminal_test_rev_const_sized_4() {
        ShadowedFunction::new(ComputeTerminalConstSizedDynamicSymbolsReversed::<4>).test()
    }

    #[test]
    fn eval_compute_terminal_test_rev_const_sized_5() {
        ShadowedFunction::new(ComputeTerminalConstSizedDynamicSymbolsReversed::<5>).test()
    }

    #[test]
    fn eval_compute_terminal_test_rev_const_sized_34() {
        ShadowedFunction::new(ComputeTerminalConstSizedDynamicSymbolsReversed::<34>).test()
    }

    #[test]
    fn eval_compute_terminal_test_rev_const_sized_64() {
        ShadowedFunction::new(ComputeTerminalConstSizedDynamicSymbolsReversed::<64>).test()
    }

    #[test]
    fn eval_compute_terminal_test_rev_const_sized_256() {
        ShadowedFunction::new(ComputeTerminalConstSizedDynamicSymbolsReversed::<256>).test()
    }
}

#[cfg(test)]
mod bench {
    use super::*;
    use crate::test_prelude::*;

    #[test]
    fn bench_const_sized_terminal_calc_rev_5() {
        // What's the cost if input is a digest? Does not include code to move digest into
        // memory as an array.
        ShadowedFunction::new(ComputeTerminalConstSizedDynamicSymbolsReversed::<{ Digest::LEN }>)
            .bench()
    }

    #[test]
    fn bench_const_sized_terminal_calc_rev_30() {
        ShadowedFunction::new(ComputeTerminalConstSizedDynamicSymbolsReversed::<30>).bench()
    }

    #[test]
    fn bench_const_sized_terminal_calc_rev_100() {
        ShadowedFunction::new(ComputeTerminalConstSizedDynamicSymbolsReversed::<100>).bench()
    }

    #[test]
    fn bench_const_sized_terminal_calc_rev_256() {
        ShadowedFunction::new(ComputeTerminalConstSizedDynamicSymbolsReversed::<256>).bench()
    }
}
