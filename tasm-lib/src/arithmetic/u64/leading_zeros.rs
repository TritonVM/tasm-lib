use triton_vm::prelude::*;

use crate::arithmetic::u32::leading_zeros::LeadingZeros as U32LeadingZeroes;
use crate::prelude::*;

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub struct LeadingZeros;

impl BasicSnippet for LeadingZeros {
    fn parameters(&self) -> Vec<(DataType, String)> {
        vec![(DataType::U64, "arg".to_string())]
    }

    fn return_values(&self) -> Vec<(DataType, String)> {
        vec![(DataType::U32, "leading_zeros(arg)".to_string())]
    }

    fn entrypoint(&self) -> String {
        "tasmlib_arithmetic_u64_leading_zeros".to_string()
    }

    fn code(&self, library: &mut Library) -> Vec<LabelledInstruction> {
        let leading_zeros_u32 = library.import(Box::new(U32LeadingZeroes));

        let entrypoint = self.entrypoint();
        let hi_is_zero_label = format!("{entrypoint}_hi_is_zero");

        triton_asm!(
            // BEFORE: _ value_hi value_lo
            // AFTER:  _ (leading_zeros as u32)
            {entrypoint}:
                pick 1
                call {leading_zeros_u32}
                // _ value_lo leading_zeros_value_hi

                dup 0
                push 32
                eq
                skiz
                    call {hi_is_zero_label}

                // _ temp leading_zeros

                pick 1
                pop 1
                return

            {hi_is_zero_label}:
                // _ value_lo 32

                pick 1
                call {leading_zeros_u32}
                // _ 32 leading_zeros_value_lo

                addi 32
                // _ 32 leading_zeros
                return
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_prelude::*;

    impl Closure for LeadingZeros {
        type Args = u64;

        fn rust_shadow(&self, stack: &mut Vec<BFieldElement>) {
            let arg = pop_encodable::<Self::Args>(stack);
            push_encodable(stack, &arg.leading_zeros());
        }

        fn pseudorandom_args(
            &self,
            seed: [u8; 32],
            bench_case: Option<BenchmarkCase>,
        ) -> Self::Args {
            match bench_case {
                Some(BenchmarkCase::CommonCase) => 1 << 31,
                Some(BenchmarkCase::WorstCase) => 1 << 62,
                None => StdRng::from_seed(seed).random(),
            }
        }

        fn corner_case_args(&self) -> Vec<Self::Args> {
            let small = 0..10;
            let medium = (27..35).map(|i| 1 << i);
            let large = (0..10).map(|i| u64::MAX - i);

            small.chain(medium).chain(large).collect()
        }
    }

    #[test]
    fn unit() {
        ShadowedClosure::new(LeadingZeros).test();
    }
}

#[cfg(test)]
mod benches {
    use super::*;
    use crate::test_prelude::*;

    #[test]
    fn benchmark() {
        ShadowedClosure::new(LeadingZeros).bench();
    }
}
