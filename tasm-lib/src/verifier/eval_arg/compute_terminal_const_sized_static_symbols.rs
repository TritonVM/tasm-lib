use itertools::Itertools;
use triton_vm::prelude::*;

use crate::data_type::Literal;
use crate::prelude::*;

/// Compute a terminal-value for an evaluation argument.
///
/// Compute the terminal for an evaluation argument as specified by `initial`, `challenge`,
/// and `symbols`. This amounts to evaluating polynomial
/// `f(x) = initial·x^n + Σ_i symbols[n-i]·x^i`
/// at point `challenge`, _i.e._, returns `f(challenge)`.
#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub struct ComputeTerminalConstSizedStaticSymbols<const N: usize> {
    pub symbols: [BFieldElement; N],
    pub initial: XFieldElement,
}

// This snippet is implemented as:
// fn compute_terminal(
//     symbols: &[BFieldElement],
//     initial: XFieldElement,
//     challenge: XFieldElement,
// ) -> XFieldElement {
//     symbols.iter().fold(initial, |running_evaluation, &symbol| {
//         challenge * running_evaluation + symbol
//     })
// }
impl<const N: usize> BasicSnippet for ComputeTerminalConstSizedStaticSymbols<N> {
    fn parameters(&self) -> Vec<(DataType, String)> {
        vec![(DataType::Xfe, "challenge".to_owned())]
    }

    fn return_values(&self) -> Vec<(DataType, String)> {
        vec![(DataType::Xfe, "terminal".to_owned())]
    }

    fn entrypoint(&self) -> String {
        const IDENTIFIER: &str =
            "tasmlib_verifier_eval_arg_compute_terminal_const_sized_static_symbols_symbol_count";

        // Prevent name-clashes by hashing all parameters and using this for entrypoint name
        let all_variables = [self.symbols.to_vec(), self.initial.coefficients.to_vec()].concat();
        let variables_as_digest = Tip5::hash_varlen(&all_variables);
        let first_word = variables_as_digest.encode()[0];

        format!("{IDENTIFIER}_{N}_{first_word}")
    }

    fn code(&self, _library: &mut Library) -> Vec<LabelledInstruction> {
        fn one_iteration(symbol: BFieldElement, last_iteration: bool) -> Vec<LabelledInstruction> {
            let dup_challenge = if last_iteration {
                triton_asm!()
            } else {
                triton_asm!(
                    dup 5
                    dup 5
                    dup 5
                )
            };
            triton_asm!(
                // _ [challenge] [acc]

                {&dup_challenge}
                xx_mul
                // _ <[challenge]> [acc * challenge]

                push {symbol}
                add
                // _ <[challenge]> [acc * challenge + symbol]
            )
        }

        let entrypoint = self.entrypoint();
        let push_initial = Literal::Xfe(self.initial).push_to_stack_code();
        let iterations = self
            .symbols
            .iter()
            .enumerate()
            .map(|(symbol_index, symbol)| one_iteration(*symbol, symbol_index == N - 1))
            .concat();
        triton_asm!(
            {entrypoint}:
                // _ [challenge]

                {&push_initial}
                // _ [challenge] [initial]

                {&iterations}
                // _ [result]

                return
        )
    }
}

#[cfg(test)]
mod tests {
    use num::One;
    use num::Zero;
    use proptest_arbitrary_interop::arb;
    use test_strategy::proptest;
    use triton_vm::air::cross_table_argument::CrossTableArg;
    use triton_vm::air::cross_table_argument::EvalArg;

    use super::*;
    use crate::rust_shadowing_helper_functions::array::insert_as_array;
    use crate::rust_shadowing_helper_functions::list::list_insert;
    use crate::test_helpers::tasm_final_state;
    use crate::test_prelude::*;
    use crate::verifier::eval_arg::compute_terminal_const_sized_dynamic_symbols_reversed::ComputeTerminalConstSizedDynamicSymbolsReversed;
    use crate::verifier::eval_arg::compute_terminal_dyn_sized_dynamic_symbols::ComputeTerminalDynSizedDynamicSymbols;
    use crate::verifier::eval_arg::compute_terminal_from_digest::ComputeTerminalFromDigestInitialIsOne;

    impl<const N: usize> Closure for ComputeTerminalConstSizedStaticSymbols<N> {
        type Args = XFieldElement;

        fn rust_shadow(&self, stack: &mut Vec<BFieldElement>) {
            let challenge = pop_encodable::<Self::Args>(stack);
            let terminal = EvalArg::compute_terminal(&self.symbols, self.initial, challenge);
            push_encodable(stack, &terminal);
        }

        fn pseudorandom_args(
            &self,
            seed: [u8; 32],
            _bench_case: Option<BenchmarkCase>,
        ) -> Self::Args {
            StdRng::from_seed(seed).random()
        }
    }

    #[test]
    fn eval_compute_unit_test_1() {
        ShadowedClosure::new(ComputeTerminalConstSizedStaticSymbols::<1> {
            symbols: [BFieldElement::new(77)],
            initial: XFieldElement::new_const(BFieldElement::new(2)),
        })
        .test();
    }

    #[test]
    fn eval_compute_unit_test_2() {
        ShadowedClosure::new(ComputeTerminalConstSizedStaticSymbols::<2> {
            symbols: [BFieldElement::new(77), BFieldElement::new(177)],
            initial: XFieldElement::new_const(BFieldElement::new(2)),
        })
        .test();
    }

    #[proptest(cases = 3)]
    fn eval_compute_terminal_test_1(
        #[strategy(arb())] symbol: BFieldElement,
        #[strategy(arb())] initial: XFieldElement,
    ) {
        ShadowedClosure::new(ComputeTerminalConstSizedStaticSymbols::<1> {
            symbols: [symbol],
            initial,
        })
        .test();
    }

    #[proptest(cases = 3)]
    fn eval_compute_terminal_test_2(
        #[strategy(arb())] symbols: [BFieldElement; 2],
        #[strategy(arb())] initial: XFieldElement,
    ) {
        ShadowedClosure::new(ComputeTerminalConstSizedStaticSymbols::<2> { symbols, initial })
            .test();
    }

    #[proptest(cases = 3)]
    fn eval_compute_terminal_test_3(
        #[strategy(arb())] symbols: [BFieldElement; 3],
        #[strategy(arb())] initial: XFieldElement,
    ) {
        ShadowedClosure::new(ComputeTerminalConstSizedStaticSymbols::<3> { symbols, initial })
            .test();
    }

    #[proptest(cases = 3)]
    fn eval_compute_terminal_test_4(
        #[strategy(arb())] symbols: [BFieldElement; 4],
        #[strategy(arb())] initial: XFieldElement,
    ) {
        ShadowedClosure::new(ComputeTerminalConstSizedStaticSymbols::<4> { symbols, initial })
            .test();
    }

    #[proptest(cases = 3)]
    fn eval_compute_terminal_test_5(
        #[strategy(arb())] symbols: [BFieldElement; 5],
        #[strategy(arb())] initial: XFieldElement,
    ) {
        ShadowedClosure::new(ComputeTerminalConstSizedStaticSymbols::<5> { symbols, initial })
            .test();
    }

    #[proptest(cases = 3)]
    fn eval_compute_terminal_test_256(
        #[strategy(arb())] symbols: [BFieldElement; 256],
        #[strategy(arb())] initial: XFieldElement,
    ) {
        ShadowedClosure::new(ComputeTerminalConstSizedStaticSymbols::<256> { symbols, initial })
            .test();
    }

    #[proptest(cases = 3)]
    fn compare_to_other_implementations_length_1(
        #[strategy(arb())] initial: XFieldElement,
        #[strategy(arb())] challenge: XFieldElement,
        #[strategy(arb())] symbols: [BFieldElement; 1],
    ) {
        let stat_res_const_sized =
            result_from_stat_snippet_const_sized(initial, challenge, symbols);
        let dyn_res_const_sized = result_from_dyn_snippet_const_sized(initial, challenge, symbols);
        let dyn_res_dyn_sized = result_from_dyn_snippet_dyn_sized(initial, challenge, symbols);
        assert_eq!(stat_res_const_sized, dyn_res_const_sized);
        assert_eq!(stat_res_const_sized, dyn_res_dyn_sized);
    }

    #[proptest(cases = 3)]
    fn compare_to_other_implementations_length_2(
        #[strategy(arb())] initial: XFieldElement,
        #[strategy(arb())] challenge: XFieldElement,
        #[strategy(arb())] symbols: [BFieldElement; 2],
    ) {
        let stat_res_const_sized =
            result_from_stat_snippet_const_sized(initial, challenge, symbols);
        let dyn_res_const_sized = result_from_dyn_snippet_const_sized(initial, challenge, symbols);
        let dyn_res_dyn_sized = result_from_dyn_snippet_dyn_sized(initial, challenge, symbols);
        assert_eq!(stat_res_const_sized, dyn_res_const_sized);
        assert_eq!(stat_res_const_sized, dyn_res_dyn_sized);
    }

    #[proptest(cases = 3)]
    fn compare_to_other_implementations_length_3(
        #[strategy(arb())] initial: XFieldElement,
        #[strategy(arb())] challenge: XFieldElement,
        #[strategy(arb())] symbols: [BFieldElement; 3],
    ) {
        let stat_res_const_sized =
            result_from_stat_snippet_const_sized(initial, challenge, symbols);
        let dyn_res_const_sized = result_from_dyn_snippet_const_sized(initial, challenge, symbols);
        let dyn_res_dyn_sized = result_from_dyn_snippet_dyn_sized(initial, challenge, symbols);
        assert_eq!(stat_res_const_sized, dyn_res_const_sized);
        assert_eq!(stat_res_const_sized, dyn_res_dyn_sized);
    }

    #[proptest(cases = 3)]
    fn compare_to_other_implementations_length_4(
        #[strategy(arb())] initial: XFieldElement,
        #[strategy(arb())] challenge: XFieldElement,
        #[strategy(arb())] symbols: [BFieldElement; 4],
    ) {
        let stat_res_const_sized =
            result_from_stat_snippet_const_sized(initial, challenge, symbols);
        let dyn_res_const_sized = result_from_dyn_snippet_const_sized(initial, challenge, symbols);
        let dyn_res_dyn_sized = result_from_dyn_snippet_dyn_sized(initial, challenge, symbols);
        assert_eq!(stat_res_const_sized, dyn_res_const_sized);
        assert_eq!(stat_res_const_sized, dyn_res_dyn_sized);
    }

    #[proptest(cases = 3)]
    fn compare_to_other_implementations_length_5(
        #[strategy(arb())] initial: XFieldElement,
        #[strategy(arb())] challenge: XFieldElement,
        #[strategy(arb())] symbols: [BFieldElement; 5],
    ) {
        let stat_res_const_sized =
            result_from_stat_snippet_const_sized(initial, challenge, symbols);
        let dyn_res_const_sized = result_from_dyn_snippet_const_sized(initial, challenge, symbols);
        let dyn_res_dyn_sized = result_from_dyn_snippet_dyn_sized(initial, challenge, symbols);
        assert_eq!(stat_res_const_sized, dyn_res_const_sized);
        assert_eq!(stat_res_const_sized, dyn_res_dyn_sized);
    }

    #[proptest(cases = 3)]
    fn compare_to_other_implementations_length_5_initial_is_one(
        #[strategy(arb())] challenge: XFieldElement,
        #[strategy(arb())] symbols: [BFieldElement; 5],
    ) {
        fn result_from_compute_terminal_from_digest(
            challenge: XFieldElement,
            digest: Digest,
        ) -> XFieldElement {
            let snippet = ComputeTerminalFromDigestInitialIsOne;
            let challenge = challenge.coefficients.iter().cloned().rev().collect_vec();
            let digest = digest.values().into_iter().rev().collect_vec();
            let init_stack = [snippet.init_stack_for_isolated_run(), challenge, digest].concat();
            let mut final_state = tasm_final_state(
                &ShadowedClosure::new(snippet),
                &init_stack,
                &[],
                NonDeterminism::default(),
                &None,
            );
            let terminal = Literal::pop_from_stack(DataType::Xfe, &mut final_state.op_stack.stack);

            terminal.as_xfe()
        }

        let initial = XFieldElement::one();
        let stat_res_const_sized =
            result_from_stat_snippet_const_sized(initial, challenge, symbols);
        let dyn_res_const_sized = result_from_dyn_snippet_const_sized(initial, challenge, symbols);
        let dyn_res_dyn_sized = result_from_dyn_snippet_dyn_sized(initial, challenge, symbols);
        let result_from_digest_snippet =
            result_from_compute_terminal_from_digest(challenge, Digest::new(symbols));
        assert_eq!(stat_res_const_sized, dyn_res_const_sized);
        assert_eq!(stat_res_const_sized, dyn_res_dyn_sized);
        assert_eq!(stat_res_const_sized, result_from_digest_snippet);
    }

    fn result_from_stat_snippet_const_sized<const N: usize>(
        initial: XFieldElement,
        challenge: XFieldElement,
        symbols: [BFieldElement; N],
    ) -> XFieldElement {
        let snippet = ComputeTerminalConstSizedStaticSymbols::<N> { symbols, initial };
        let challenge = challenge.coefficients.iter().cloned().rev().collect_vec();
        let init_stack = [snippet.init_stack_for_isolated_run(), challenge].concat();
        let mut final_state = tasm_final_state(
            &ShadowedClosure::new(snippet),
            &init_stack,
            &[],
            NonDeterminism::default(),
            &None,
        );
        let terminal = Literal::pop_from_stack(DataType::Xfe, &mut final_state.op_stack.stack);

        terminal.as_xfe()
    }

    fn result_from_dyn_snippet_const_sized<const N: usize>(
        initial: XFieldElement,
        challenge: XFieldElement,
        symbols: [BFieldElement; N],
    ) -> XFieldElement {
        let snippet = ComputeTerminalConstSizedDynamicSymbolsReversed::<N>;
        let challenge = challenge.coefficients.iter().cloned().rev().collect_vec();
        let initial = initial.coefficients.iter().cloned().rev().collect_vec();
        let mut memory = HashMap::default();
        let symbols_pointer = BFieldElement::zero();
        insert_as_array(
            symbols_pointer,
            &mut memory,
            symbols.into_iter().rev().collect_vec(),
        );
        let init_stack = [
            snippet.init_stack_for_isolated_run(),
            challenge,
            initial,
            vec![symbols_pointer],
        ]
        .concat();
        let mut final_state = tasm_final_state(
            &ShadowedFunction::new(snippet),
            &init_stack,
            &[],
            NonDeterminism::default().with_ram(memory),
            &None,
        );

        let terminal = Literal::pop_from_stack(DataType::Xfe, &mut final_state.op_stack.stack);

        terminal.as_xfe()
    }

    fn result_from_dyn_snippet_dyn_sized<const N: usize>(
        initial: XFieldElement,
        challenge: XFieldElement,
        symbols: [BFieldElement; N],
    ) -> XFieldElement {
        let snippet = ComputeTerminalDynSizedDynamicSymbols;
        let challenge = challenge.coefficients.iter().cloned().rev().collect_vec();
        let initial = initial.coefficients.iter().cloned().rev().collect_vec();
        let mut memory = HashMap::default();
        let symbols_pointer = BFieldElement::zero();
        list_insert(symbols_pointer, symbols.to_vec(), &mut memory);
        let init_stack = [
            snippet.init_stack_for_isolated_run(),
            challenge,
            initial,
            vec![symbols_pointer],
        ]
        .concat();
        let mut final_state = tasm_final_state(
            &ShadowedFunction::new(snippet),
            &init_stack,
            &[],
            NonDeterminism::default().with_ram(memory),
            &None,
        );

        let terminal = Literal::pop_from_stack(DataType::Xfe, &mut final_state.op_stack.stack);

        terminal.as_xfe()
    }
}

#[cfg(test)]
mod benches {
    use super::*;
    use crate::test_prelude::*;

    #[test]
    fn benchmark() {
        ShadowedClosure::new(ComputeTerminalConstSizedStaticSymbols::<256> {
            symbols: bfe_array![100; 256],
            initial: xfe!([1_u64 << 35, 1 << 4, 1_u64 << 44]),
        })
        .bench()
    }
}
