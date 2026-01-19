use std::collections::HashMap;

use triton_vm::prelude::*;

use crate::prelude::*;
use crate::traits::basic_snippet::Reviewer;
use crate::traits::basic_snippet::SignOffFingerprint;

/// Returns the smallest power of two greater than or equal to argument `arg`.
/// Behaves like the [`rustc` method in debug mode][rustc_pow] for all inputs
/// of type `u32`.
///
/// ### Behavior
///
/// ```text
/// BEFORE: _ arg
/// AFTER:  _ u32::next_power_of_two(arg)
/// ```
///
/// ### Pre-conditions
///
/// - `arg` is a valid u32
/// - `arg` is smaller than or equal to 2^31
///
/// ### Post-conditions
///
/// - the output is the smallest power of two greater than or equal to `arg`
///
/// [rustc_pow]: u32::next_power_of_two
#[derive(Debug, Clone, Copy)]
pub struct NextPowerOfTwo;

impl NextPowerOfTwo {
    pub const INPUT_TOO_LARGE_ERROR_ID: i128 = 130;
}

impl BasicSnippet for NextPowerOfTwo {
    fn parameters(&self) -> Vec<(DataType, String)> {
        vec![(DataType::U32, "self".to_owned())]
    }

    fn return_values(&self) -> Vec<(DataType, String)> {
        vec![(DataType::U32, "next_power_of_two".to_owned())]
    }

    fn entrypoint(&self) -> String {
        "tasmlib_arithmetic_u32_next_power_of_two".to_string()
    }

    fn code(&self, _: &mut Library) -> Vec<LabelledInstruction> {
        let entrypoint = self.entrypoint();
        let zero_or_one_label = format!("{entrypoint}_zero_or_one_label");
        let greater_than_one_label = format!("{entrypoint}_greater_than_one");
        triton_asm!(
            {entrypoint}:
                // _ arg

                push 1
                push 2
                dup 2
                lt
                // _ arg 1 (arg < 2)

                skiz
                    call {zero_or_one_label}
                // if arg < 2:  _ 1   0
                // if arg >= 2: _ arg 1

                skiz
                    call {greater_than_one_label}
                // _

                return

            {zero_or_one_label}:
                // _ arg 1

                pop 2
                push 1
                push 0
                // _ 1 0

                return

            {greater_than_one_label}:
                // _ arg

                addi -1
                log_2_floor
                addi 1
                // _ log₂(⌊value - 1⌋ + 1)

                push 2
                pow
                // _ 2^log₂(⌊value - 1⌋ + 1)

                /* Assert result *not* 2^{32} */
                dup 0
                push {1u64 << 32}
                eq
                push 0
                eq
                assert error_id {Self::INPUT_TOO_LARGE_ERROR_ID}

                return
        )
    }

    fn sign_offs(&self) -> HashMap<Reviewer, SignOffFingerprint> {
        let mut sign_offs = HashMap::new();
        sign_offs.insert(Reviewer("ferdinand"), 0x6ee79243e0c65303.into());
        sign_offs
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_prelude::*;

    impl Closure for NextPowerOfTwo {
        type Args = u32;

        fn rust_shadow(&self, stack: &mut Vec<BFieldElement>) {
            let arg = pop_encodable::<Self::Args>(stack);
            push_encodable(stack, &arg.next_power_of_two());
        }

        fn pseudorandom_args(
            &self,
            seed: [u8; 32],
            bench_case: Option<BenchmarkCase>,
        ) -> Self::Args {
            match bench_case {
                Some(BenchmarkCase::CommonCase) => (1 << 27) - 1,
                Some(BenchmarkCase::WorstCase) => (1 << 31) - 1,
                None => StdRng::from_seed(seed).next_u32() / 2,
            }
        }

        fn corner_case_args(&self) -> Vec<Self::Args> {
            let small_inputs = 0..=66;
            let big_valid_inputs = (0..=5).map(|i| (1 << 31) - i);

            small_inputs.chain(big_valid_inputs).collect()
        }
    }

    impl NextPowerOfTwo {
        fn prepare_vm_stack(&self, arg: u32) -> Vec<BFieldElement> {
            [self.init_stack_for_isolated_run(), bfe_vec![arg]].concat()
        }
    }

    #[test]
    fn next_power_of_two_pbt() {
        ShadowedClosure::new(NextPowerOfTwo).test()
    }

    #[test]
    fn npo2_overflow_negative_test() {
        let greater_two_pow_31 = (1..=5).map(|i| (1 << 31) + i);
        let smaller_equal_u32_max = (0..=2).map(|i| u32::MAX - i);

        for arg in greater_two_pow_31.chain(smaller_equal_u32_max) {
            test_assertion_failure(
                &ShadowedClosure::new(NextPowerOfTwo),
                InitVmState::with_stack(NextPowerOfTwo.prepare_vm_stack(arg)),
                &[NextPowerOfTwo::INPUT_TOO_LARGE_ERROR_ID],
            );
        }
    }
}

#[cfg(test)]
mod benches {
    use super::*;
    use crate::test_prelude::*;

    #[test]
    fn benchmark() {
        ShadowedClosure::new(NextPowerOfTwo).bench()
    }
}
