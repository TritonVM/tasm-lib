use std::collections::HashMap;

use triton_vm::prelude::*;

use crate::arithmetic::u32::trailing_zeros::TrailingZeros as U32TrailingZeros;
use crate::prelude::*;
use crate::traits::basic_snippet::Reviewer;
use crate::traits::basic_snippet::SignOffFingerprint;

/// Returns the number of trailing zeros in the binary representation of the
/// input argument. Behaves like [`u64::trailing_zeros`].
///
/// ### Behavior
///
/// ```text
/// BEFORE: _ [arg: u64]
/// AFTER:  _ [u64::trailing_zeros(arg): u32]
/// ```
///
/// ### Preconditions
///
/// - the input argument is properly [`BFieldCodec`] encoded
///
/// ### Postconditions
///
/// - the output is the number of trailing zeros in the binary representation
///   of `arg`
#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub struct TrailingZeros;

impl BasicSnippet for TrailingZeros {
    fn parameters(&self) -> Vec<(DataType, String)> {
        vec![(DataType::U64, "arg".to_string())]
    }

    fn return_values(&self) -> Vec<(DataType, String)> {
        vec![(DataType::U32, "trailing_zeros(arg)".to_string())]
    }

    fn entrypoint(&self) -> String {
        "tasmlib_arithmetic_u64_trailing_zeros".to_string()
    }

    fn code(&self, library: &mut Library) -> Vec<LabelledInstruction> {
        let u32_trailing_zeros = library.import(Box::new(U32TrailingZeros));

        let entrypoint = self.entrypoint();
        let hi_bits_dominate = format!("{entrypoint}_hi_bits_dominate");
        let lo_bits_dominate = format!("{entrypoint}_lo_bits_dominate");

        triton_asm! {
            // BEFORE: _ arg_hi arg_lo
            // AFTER:  _ trailing_zeros(arg)
            {entrypoint}:
                push 1
                dup 1
                push 0
                eq
                // _ arg_hi arg_lo 1 (arg_lo == 0)

                skiz call {hi_bits_dominate}
                skiz call {lo_bits_dominate}
                // _ trailing_zeros(arg)

                return

            // BEFORE: _ arg_hi arg_lo 1
            // AFTER:  _ trailing_zeros(arg) 0
            // where arg_lo == 0
            {hi_bits_dominate}:
                pop 2
                call {u32_trailing_zeros}
                addi 32
                push 0
                return

            // BEFORE: _ arg_hi arg_lo
            // AFTER:  _ trailing_zeros(arg)
            // where arg_lo != 0
            {lo_bits_dominate}:
                pick 1
                pop 1
                call {u32_trailing_zeros}
                return
        }
    }

    fn sign_offs(&self) -> HashMap<Reviewer, SignOffFingerprint> {
        let mut sign_offs = HashMap::new();
        sign_offs.insert(Reviewer("ferdinand"), 0xf8428d5ca58ed29f.into());
        sign_offs
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_prelude::*;

    impl Closure for TrailingZeros {
        type Args = u64;

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
                Some(BenchmarkCase::CommonCase) => 0b1111_1111 << 15,
                Some(BenchmarkCase::WorstCase) => 1 << 63,
                None => StdRng::from_seed(seed).random(),
            }
        }

        fn corner_case_args(&self) -> Vec<Self::Args> {
            [1, 1 << 31, 1 << 32, 1 << 33, 1 << 63, u64::MAX - 1]
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
