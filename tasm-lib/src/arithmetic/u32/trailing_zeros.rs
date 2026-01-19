use std::collections::HashMap;

use triton_vm::prelude::*;

use crate::prelude::*;
use crate::traits::basic_snippet::Reviewer;
use crate::traits::basic_snippet::SignOffFingerprint;

/// Returns the number of trailing zeros in the binary representation of the
/// input argument. Behaves like [`u32::trailing_zeros`].
///
/// ### Behavior
///
/// ```text
/// BEFORE: _ arg
/// AFTER:  _ u32::trailing_zeros(arg)
/// ```
///
/// ### Preconditions
///
/// - `arg` is a valid `u32`
///
/// ### Postconditions
///
/// - the output is the number of trailing zeros in the binary representation
///   of `arg`
#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub struct TrailingZeros;

impl BasicSnippet for TrailingZeros {
    fn parameters(&self) -> Vec<(DataType, String)> {
        vec![(DataType::U32, "arg".to_string())]
    }

    fn return_values(&self) -> Vec<(DataType, String)> {
        vec![(DataType::U32, "trailing_zeros(arg)".to_string())]
    }

    fn entrypoint(&self) -> String {
        "tasmlib_arithmetic_u32_trailing_zeros".to_string()
    }

    // The basic idea for the algorithm below is taken from “Count the consecutive
    // zero bits (trailing) on the right in parallel” [0]. For example, consider
    // input 1010100₂:
    //
    // input:                         1010100₂
    // bitwise negation:         11…110101011₂
    // (wrapping) add one:       11…110101100₂
    // bitwise `and` with input:          100₂
    // base-2 integer logarithm:            2
    //
    // By handling the edge case “arg == 0” early, the bitwise negation of the input
    // can never be 11…11₂, meaning the subsequent addition of 1 can never overflow.
    // This, in turn, implies that the instruction `log_2_floor` will never cause a
    // crash.
    //
    // [0] https://graphics.stanford.edu/~seander/bithacks.html#ZerosOnRightParallel
    fn code(&self, _: &mut Library) -> Vec<LabelledInstruction> {
        let entrypoint = self.entrypoint();
        let arg_eq_0 = format!("{entrypoint}_arg_eq_0");
        let arg_neq_0 = format!("{entrypoint}_arg_neq_0");

        triton_asm! {
            // BEFORE: _ arg
            // AFTER:  _ trailing_zeros(arg)
            {entrypoint}:
                push 1
                dup 1
                push 0
                eq
                // _ arg 1 (arg == 0)

                skiz call {arg_eq_0}
                skiz call {arg_neq_0}
                // _ trailing_zeros(arg)

                return

            // BEFORE: _ 0 1
            // AFTER:  _ 32 0
            {arg_eq_0}:
                pop 2
                push 32
                push 0
                return

            // BEFORE: _ arg
            // AFTER:  _ trailing_zeros(arg)
            // where arg != 0
            {arg_neq_0}:
                dup 0
                push {u32::MAX}
                    hint u32_max: u32 = stack[0]
                xor
                    hint bitwise_negated_arg: u32 = stack[0]
                // _ arg bitwise_negated_arg

                addi 1
                and
                log_2_floor

                return
        }
    }

    fn sign_offs(&self) -> HashMap<Reviewer, SignOffFingerprint> {
        let mut sign_offs = HashMap::new();
        sign_offs.insert(Reviewer("ferdinand"), 0x2ec8110d7af9b4e9.into());
        sign_offs
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_prelude::*;

    impl Closure for TrailingZeros {
        type Args = u32;

        fn rust_shadow(&self, stack: &mut Vec<BFieldElement>) {
            let arg = pop_encodable::<Self::Args>(stack);
            push_encodable(stack, &arg.trailing_zeros());
        }

        fn pseudorandom_args(
            &self,
            seed: [u8; 32],
            bench_case: Option<BenchmarkCase>,
        ) -> Self::Args {
            match bench_case {
                Some(BenchmarkCase::CommonCase) => 0b1111_1111 << 3,
                Some(BenchmarkCase::WorstCase) => 1 << 31,
                None => StdRng::from_seed(seed).random(),
            }
        }

        fn corner_case_args(&self) -> Vec<Self::Args> {
            [1, 1 << 31, u32::MAX - 1]
                .into_iter()
                .flat_map(|i| [i - 1, i, i + 1])
                .collect()
        }
    }

    #[test]
    fn unit() {
        ShadowedClosure::new(TrailingZeros).test();
    }
}

#[cfg(test)]
mod benches {
    use super::*;
    use crate::test_prelude::*;

    #[test]
    fn benchmark() {
        ShadowedClosure::new(TrailingZeros).bench()
    }
}
