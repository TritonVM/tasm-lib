use std::collections::HashMap;

use triton_vm::prelude::*;

use crate::prelude::*;
use crate::traits::basic_snippet::Reviewer;
use crate::traits::basic_snippet::SignOffFingerprint;

/// [Decrement][dec] the given argument by 1.
///
/// ### Behavior
///
/// ```text
/// BEFORE: _ [arg: u64]
/// AFTER:  _ [arg-1: u64]
/// ```
///
/// ### Preconditions
///
/// - the input `arg` is greater than 0
/// - the input `arg` is properly [`BFieldCodec`] encoded
///
/// ### Postconditions
///
/// - the output is properly [`BFieldCodec`] encoded
///
/// [dec]: num::Integer::dec
#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
pub struct Decr;

impl Decr {
    pub const OVERFLOW_ERROR_ID: i128 = 110;
}

impl BasicSnippet for Decr {
    fn parameters(&self) -> Vec<(DataType, String)> {
        vec![(DataType::U64, "arg".to_string())]
    }

    fn return_values(&self) -> Vec<(DataType, String)> {
        vec![(DataType::U64, "(arg-1)".to_string())]
    }

    fn entrypoint(&self) -> String {
        "tasmlib_arithmetic_u64_decr".to_string()
    }

    fn code(&self, _: &mut Library) -> Vec<LabelledInstruction> {
        let entrypoint = self.entrypoint();
        let propagate_carry = format!("{entrypoint}_propagate_carry");

        triton_asm!(
            {entrypoint}:
                addi -1
                // _ arg_hi (arg_lo - 1)

                dup 0
                push -1
                eq
                skiz
                    call {propagate_carry}
                return

            // BEFORE: _ arg_hi -1
            // AFTER:  _ (arg_hi - 1) u32::MAX
            {propagate_carry}:
                add
                // _ (arg_hi - 1)

                dup 0
                push -1
                eq
                // _ (arg_hi - 1) (arg_hi - 1 == -1)
                // _ (arg_hi - 1) (arg_hi == 0)

                push 0
                eq
                // _ (arg_hi - 1) (arg_hi != 0)

                assert error_id {Self::OVERFLOW_ERROR_ID}

                push {u32::MAX}
                return
        )
    }

    fn sign_offs(&self) -> HashMap<Reviewer, SignOffFingerprint> {
        let mut sign_offs = HashMap::new();
        sign_offs.insert(Reviewer("ferdinand"), 0xc5b5d50e60281819.into());
        sign_offs
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_prelude::*;

    impl Closure for Decr {
        type Args = u64;

        fn rust_shadow(&self, stack: &mut Vec<BFieldElement>) {
            let arg = pop_encodable::<Self::Args>(stack);
            push_encodable(stack, &arg.checked_sub(1).unwrap());
        }

        fn pseudorandom_args(
            &self,
            seed: [u8; 32],
            bench_case: Option<BenchmarkCase>,
        ) -> Self::Args {
            match bench_case {
                Some(BenchmarkCase::CommonCase) => 7,
                Some(BenchmarkCase::WorstCase) => 1 << 35,
                None => StdRng::from_seed(seed).random_range(1..=u64::MAX),
            }
        }
    }

    #[test]
    fn rust_shadow() {
        ShadowedClosure::new(Decr).test();
    }

    #[test]
    fn negative_test() {
        test_assertion_failure(
            &ShadowedClosure::new(Decr),
            InitVmState::with_stack(Decr.set_up_test_stack(0)),
            &[Decr::OVERFLOW_ERROR_ID],
        );
    }
}

#[cfg(test)]
mod benches {
    use super::*;
    use crate::test_prelude::*;

    #[test]
    fn benchmark() {
        ShadowedClosure::new(Decr).bench();
    }
}
