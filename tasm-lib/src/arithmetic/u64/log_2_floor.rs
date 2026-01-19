use std::collections::HashMap;

use triton_vm::prelude::*;

use crate::prelude::*;
use crate::traits::basic_snippet::Reviewer;
use crate::traits::basic_snippet::SignOffFingerprint;

/// The base 2 logarithm of the input, rounded down. See also: [u64::ilog2].
///
/// ### Behavior
///
/// ```text
/// BEFORE: _ [x: u64]
/// AFTER:  _ [y: u32]
/// ```
///
/// ### Preconditions
///
/// - the input `x` is properly [`BFieldCodec`] encoded
/// - the input `x` is not 0
///
/// ### Postconditions
///
/// - `y` is the [base-2 integer logarithm](u64::ilog2) of `x`
/// - `y` is properly [`BFieldCodec`] encoded
#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub struct Log2Floor;

impl BasicSnippet for Log2Floor {
    fn parameters(&self) -> Vec<(DataType, String)> {
        vec![(DataType::U64, "x".to_string())]
    }

    fn return_values(&self) -> Vec<(DataType, String)> {
        vec![(DataType::U32, "log_2_floor(x)".to_string())]
    }

    fn entrypoint(&self) -> String {
        "tasmlib_arithmetic_u64_log_2_floor".to_string()
    }

    fn code(&self, _: &mut Library) -> Vec<LabelledInstruction> {
        let entrypoint = self.entrypoint();

        let hi_neq_zero = format!("{entrypoint}_hi_neq_zero");
        let hi_eq_zero = format!("{entrypoint}_hi_eq_zero");
        triton_asm!(
            // BEFORE: _ x_hi x_lo
            // AFTER:  _ log2_floor(x)
            {entrypoint}:
                push 1
                dup 2
                // _ x_hi x_lo 1 x_hi

                skiz call {hi_neq_zero}
                skiz call {hi_eq_zero}
                // _ log2_floor(x)

                return

            {hi_neq_zero}:
                // x_hi != 0
                // _ x_hi x_lo 1

                pop 1
                // _ x_hi x_lo

                /* assert valid encoding */
                pop_count
                pop 1
                // _ x_hi

                log_2_floor
                addi 32
                // _ (log2_floor(x_hi) + 32)

                push 0
                // _ (log2_floor(x_hi) + 32) 0

                return

            {hi_eq_zero}:
                // x_hi == 0
                // _ 0 x_lo
                pick 1
                pop 1
                log_2_floor
                // _ log_2_floor(x_lo)

                return
        )
    }

    fn sign_offs(&self) -> HashMap<Reviewer, SignOffFingerprint> {
        let mut sign_offs = HashMap::new();
        sign_offs.insert(Reviewer("ferdinand"), 0xc89c4b19e24a3ba1.into());
        sign_offs
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_helpers::negative_test;
    use crate::test_prelude::*;

    impl Closure for Log2Floor {
        type Args = u64;

        fn rust_shadow(&self, stack: &mut Vec<BFieldElement>) {
            let x = pop_encodable::<Self::Args>(stack);
            push_encodable(stack, &x.ilog2());
        }

        fn pseudorandom_args(
            &self,
            seed: [u8; 32],
            bench_case: Option<BenchmarkCase>,
        ) -> Self::Args {
            match bench_case {
                Some(BenchmarkCase::CommonCase) => u64::from(u32::MAX),
                Some(BenchmarkCase::WorstCase) => u64::MAX,
                None => StdRng::from_seed(seed).random(),
            }
        }

        fn corner_case_args(&self) -> Vec<Self::Args> {
            (0..63)
                .map(|pow| 1_u64 << pow)
                .flat_map(|x| [x.checked_sub(1), Some(x), x.checked_add(1)])
                .flatten()
                .filter(|&x| x != 0)
                .collect()
        }
    }

    #[test]
    fn rust_shadow_test() {
        ShadowedClosure::new(Log2Floor).test();
    }

    #[proptest]
    fn hi_is_u32_but_lo_is_not(
        #[strategy(0_u32..)]
        #[map(BFieldElement::from)]
        x_hi: BFieldElement,
        #[strategy(1_u64 << 32..)]
        #[map(BFieldElement::new)]
        x_lo: BFieldElement,
    ) {
        let mut init_stack = Log2Floor.init_stack_for_isolated_run();
        init_stack.push(x_hi);
        init_stack.push(x_lo);

        let expected_err = InstructionError::OpStackError(OpStackError::FailedU32Conversion(x_lo));
        negative_test(
            &ShadowedClosure::new(Log2Floor),
            InitVmState::with_stack(init_stack),
            &[expected_err],
        );
    }

    #[proptest]
    fn hi_is_not_u32_but_lo_is(
        #[strategy(1_u64 << 32..)]
        #[map(BFieldElement::new)]
        x_hi: BFieldElement,
        #[strategy(0_u32..)]
        #[map(BFieldElement::from)]
        x_lo: BFieldElement,
    ) {
        let mut init_stack = Log2Floor.init_stack_for_isolated_run();
        init_stack.push(x_hi);
        init_stack.push(x_lo);

        let expected_err = InstructionError::OpStackError(OpStackError::FailedU32Conversion(x_hi));
        negative_test(
            &ShadowedClosure::new(Log2Floor),
            InitVmState::with_stack(init_stack),
            &[expected_err],
        );
    }

    #[test]
    fn crash_on_zero() {
        negative_test(
            &ShadowedClosure::new(Log2Floor),
            InitVmState::with_stack(Log2Floor.set_up_test_stack(0)),
            &[InstructionError::LogarithmOfZero],
        );
    }

    #[test]
    fn unit_test() {
        fn assert_terminal_stack_is_as_expected(x: u64, expected: u32) {
            let mut expected_stack = Log2Floor.init_stack_for_isolated_run();
            push_encodable(&mut expected_stack, &expected);

            test_rust_equivalence_given_complete_state(
                &ShadowedClosure::new(Log2Floor),
                &Log2Floor.set_up_test_stack(x),
                &[],
                &NonDeterminism::default(),
                &None,
                Some(&expected_stack),
            );
        }

        // many of the following are already covered by the edge cases declared in
        // “corner_case_initial_states” but repeated here as a sanity check
        assert_terminal_stack_is_as_expected(1, 0);
        assert_terminal_stack_is_as_expected(2, 1);
        assert_terminal_stack_is_as_expected(3, 1);
        assert_terminal_stack_is_as_expected(4, 2);
        assert_terminal_stack_is_as_expected(5, 2);
        assert_terminal_stack_is_as_expected(6, 2);
        assert_terminal_stack_is_as_expected(7, 2);
        assert_terminal_stack_is_as_expected(8, 3);
        assert_terminal_stack_is_as_expected((1 << 32) - 20_000, 31);
        assert_terminal_stack_is_as_expected((1 << 32) + 800, 32);
    }
}

#[cfg(test)]
mod benches {
    use super::*;
    use crate::test_prelude::*;

    #[test]
    fn benchmark() {
        ShadowedClosure::new(Log2Floor).bench();
    }
}
