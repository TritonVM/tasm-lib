use std::collections::HashMap;

use triton_vm::prelude::*;

use crate::prelude::*;
use crate::traits::basic_snippet::Reviewer;
use crate::traits::basic_snippet::SignOffFingerprint;

/// Integer-divide the argument by 2.
///
/// ### Behavior
///
/// ```text
/// BEFORE: _ [arg: u64]
/// AFTER:  _ [arg/2: u64]
/// ```
///
/// ### Preconditions
///
/// - the input is properly [`BFieldCodec`] encoded
///
/// ### Postconditions
///
/// - the output is properly [`BFieldCodec`] encoded
#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub struct Div2;

impl BasicSnippet for Div2 {
    fn parameters(&self) -> Vec<(DataType, String)> {
        vec![(DataType::U64, "arg".to_string())]
    }

    fn return_values(&self) -> Vec<(DataType, String)> {
        vec![(DataType::U64, "(arg/2)".to_string())]
    }

    fn entrypoint(&self) -> String {
        "tasmlib_arithmetic_u64_div2".to_string()
    }

    fn code(&self, _: &mut Library) -> Vec<LabelledInstruction> {
        triton_asm!(
            // BEFORE: _ arg_hi arg_lo
            // AFTER:  _ (arg / 2)_hi (arg / 2)_lo
            {self.entrypoint()}:
                /* divide low part */
                push 2
                pick 1
                div_mod
                pop 1
                // _ arg_hi (arg_lo / 2)

                /* divide high part, carry its least significant bit into the low part */
                push 2
                pick 2
                div_mod
                // _ (arg_lo / 2) (arg_hi / 2) (arg_hi % 2)
                // _ (arg_lo / 2) (arg / 2)_hi (arg_hi % 2)

                push {1_u32 << 31}
                hint two_pow_31: u32 = stack[0]
                mul
                hint carry: u32 = stack[0]
                // _ (arg_lo / 2) (arg / 2)_hi carry

                pick 2
                add
                // _ (arg / 2)_hi (arg / 2)_lo

                return
        )
    }

    fn sign_offs(&self) -> HashMap<Reviewer, SignOffFingerprint> {
        let mut sign_offs = HashMap::new();
        sign_offs.insert(Reviewer("ferdinand"), 0x23322a6d8af4bb90.into());
        sign_offs
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_helpers::negative_test;
    use crate::test_prelude::*;

    impl Div2 {
        fn assert_expected_behavior(&self, arg: u64) {
            let initial_stack = self.set_up_test_stack(arg);

            let mut expected_stack = initial_stack.clone();
            self.rust_shadow(&mut expected_stack);

            test_rust_equivalence_given_complete_state(
                &ShadowedClosure::new(Self),
                &initial_stack,
                &[],
                &NonDeterminism::default(),
                &None,
                Some(&expected_stack),
            );
        }
    }

    impl Closure for Div2 {
        type Args = u64;

        fn rust_shadow(&self, stack: &mut Vec<BFieldElement>) {
            let arg = pop_encodable::<Self::Args>(stack);
            push_encodable(stack, &(arg / 2));
        }

        fn pseudorandom_args(
            &self,
            seed: [u8; 32],
            bench_case: Option<BenchmarkCase>,
        ) -> Self::Args {
            match bench_case {
                Some(BenchmarkCase::CommonCase) => 0x8000_0000,
                Some(BenchmarkCase::WorstCase) => 0xf000_0001_f000_0000,
                None => StdRng::from_seed(seed).random(),
            }
        }
    }

    #[test]
    fn rust_shadow() {
        ShadowedClosure::new(Div2).test();
    }

    #[proptest]
    fn lo_is_not_u32(hi: u32, #[strategy(1_u64 << 32..)] lo: u64) {
        let stack = [Div2.init_stack_for_isolated_run(), bfe_vec![hi, lo]].concat();

        let error = InstructionError::OpStackError(OpStackError::FailedU32Conversion(bfe!(lo)));
        negative_test(
            &ShadowedClosure::new(Div2),
            InitVmState::with_stack(stack),
            &[error],
        );
    }

    #[proptest]
    fn hi_is_not_u32(#[strategy(1_u64 << 32..)] hi: u64, lo: u32) {
        let stack = [Div2.init_stack_for_isolated_run(), bfe_vec![hi, lo]].concat();

        let error = InstructionError::OpStackError(OpStackError::FailedU32Conversion(bfe!(hi)));
        negative_test(
            &ShadowedClosure::new(Div2),
            InitVmState::with_stack(stack),
            &[error],
        );
    }

    #[test]
    fn div_2_test() {
        let small_args = 0..9;
        let mid_args = (0..9).map(|offset| (1 << 32) + offset);
        let large_args = [0, 4, 1 << 31, 0b111 << 31].map(|offset| (1 << 63) + offset);

        for arg in small_args.chain(mid_args).chain(large_args) {
            Div2.assert_expected_behavior(arg);
        }
    }
}

#[cfg(test)]
mod benches {
    use super::*;
    use crate::test_prelude::*;

    #[test]
    fn benchmark() {
        ShadowedClosure::new(Div2).bench();
    }
}
