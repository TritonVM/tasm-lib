use triton_vm::prelude::*;

use crate::prelude::*;

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub struct LeadingZeros;

impl BasicSnippet for LeadingZeros {
    fn parameters(&self) -> Vec<(DataType, String)> {
        vec![(DataType::U32, "arg".to_string())]
    }

    fn return_values(&self) -> Vec<(DataType, String)> {
        vec![(DataType::U32, "leading_zeros(arg)".to_string())]
    }

    fn entrypoint(&self) -> String {
        "tasmlib_arithmetic_u32_leading_zeros".to_string()
    }

    fn code(&self, _: &mut Library) -> Vec<LabelledInstruction> {
        let entrypoint = self.entrypoint();
        let non_zero_label = format!("{entrypoint}_non_zero");

        triton_asm! {
            // BEFORE: _ value
            // AFTER:  _ (leading zeros in value)
            {entrypoint}:
                dup 0
                skiz
                  call {non_zero_label}

                push -1
                mul
                addi 32

                return

            {non_zero_label}:
                log_2_floor
                addi 1
                return
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_prelude::*;

    impl Closure for LeadingZeros {
        type Args = u32;

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
                Some(BenchmarkCase::CommonCase) => 1 << 15,
                Some(BenchmarkCase::WorstCase) => u32::MAX,
                None => StdRng::from_seed(seed).random(),
            }
        }

        fn corner_case_args(&self) -> Vec<Self::Args> {
            vec![0, 1, 2, 3, 1 << 28, 1 << 29, 1 << 30, 1 << 31, u32::MAX]
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
