use std::collections::HashMap;

use triton_vm::prelude::*;

use crate::prelude::*;
use crate::traits::basic_snippet::Reviewer;
use crate::traits::basic_snippet::SignOffFingerprint;

/// Increment a `u64` by 1.
///
/// Crashes the VM if the input is [`u64::MAX`].
///
/// ### Behavior
///
/// ```text
/// BEFORE: _ v
/// AFTER:  _ (v+1)
/// ```
///
/// ### Preconditions
///
/// - all input arguments are properly [`BFieldCodec`] encoded
///
/// ### Postconditions
///
/// - the output is properly [`BFieldCodec`] encoded
#[derive(Debug, Default, Copy, Clone, Eq, PartialEq, Hash)]
pub struct Incr;

impl Incr {
    pub const OVERFLOW_ERROR_ID: i128 = 440;
}

impl BasicSnippet for Incr {
    fn parameters(&self) -> Vec<(DataType, String)> {
        vec![(DataType::U64, "value".to_string())]
    }

    fn return_values(&self) -> Vec<(DataType, String)> {
        vec![(DataType::U64, "value + 1".to_string())]
    }

    fn entrypoint(&self) -> String {
        "tasmlib_arithmetic_u64_incr".to_string()
    }

    fn code(&self, _: &mut Library) -> Vec<LabelledInstruction> {
        let entrypoint = self.entrypoint();
        let carry = format!("{entrypoint}_carry");
        triton_asm!(
            // BEFORE: _ [value: u64]
            // AFTER:  _ [value + 1: u64]
            {entrypoint}:
                addi 1
                dup 0
                push {1_u64 << 32}
                eq
                skiz
                    call {carry}
                return

            {carry}:
                pop 1
                addi 1
                dup 0
                push {1_u64 << 32}
                eq
                push 0
                eq
                assert error_id {Self::OVERFLOW_ERROR_ID}
                push 0
                return
        )
    }

    fn sign_offs(&self) -> HashMap<Reviewer, SignOffFingerprint> {
        let mut sign_offs = HashMap::new();
        sign_offs.insert(Reviewer("ferdinand"), 0x9b758bb656258905.into());
        sign_offs
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_prelude::*;

    impl Closure for Incr {
        type Args = u64;

        fn rust_shadow(&self, stack: &mut Vec<BFieldElement>) {
            let v = pop_encodable::<Self::Args>(stack);
            let incr = v.checked_add(1).unwrap();
            push_encodable(stack, &incr);
        }

        fn pseudorandom_args(
            &self,
            seed: [u8; 32],
            bench_case: Option<BenchmarkCase>,
        ) -> Self::Args {
            match bench_case {
                Some(BenchmarkCase::CommonCase) => (1000 << 32) + 7, // no carry
                Some(BenchmarkCase::WorstCase) => (1000 << 32) + u64::from(u32::MAX), // carry
                None => StdRng::from_seed(seed).random(),
            }
        }

        fn corner_case_args(&self) -> Vec<Self::Args> {
            vec![0, u32::MAX.into(), u64::MAX - 1]
        }
    }

    #[test]
    fn rust_shadow() {
        ShadowedClosure::new(Incr).test();
    }

    #[test]
    fn u64_max_crashes_vm() {
        test_assertion_failure(
            &ShadowedClosure::new(Incr),
            InitVmState::with_stack(Incr.set_up_test_stack(u64::MAX)),
            &[Incr::OVERFLOW_ERROR_ID],
        );
    }
}

#[cfg(test)]
mod benches {
    use super::*;
    use crate::test_prelude::*;

    #[test]
    fn benchmark() {
        ShadowedClosure::new(Incr).bench();
    }
}
