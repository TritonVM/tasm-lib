use std::collections::HashMap;

use triton_vm::prelude::*;

use crate::prelude::*;
use crate::traits::basic_snippet::Reviewer;
use crate::traits::basic_snippet::SignOffFingerprint;

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub struct WrappingMul;

impl BasicSnippet for WrappingMul {
    fn parameters(&self) -> Vec<(DataType, String)> {
        ["rhs", "lhs"]
            .map(|side| (DataType::U64, side.to_string()))
            .to_vec()
    }

    fn return_values(&self) -> Vec<(DataType, String)> {
        vec![(DataType::U64, "product".to_string())]
    }

    fn entrypoint(&self) -> String {
        "tasmlib_arithmetic_u64_wrapping_mul".to_string()
    }

    fn code(&self, _: &mut Library) -> Vec<LabelledInstruction> {
        triton_asm!(
            // BEFORE: _ right_hi right_lo left_hi left_lo
            // AFTER:  _ prod_hi prod_lo
            {self.entrypoint()}:
                /* left_lo · right_lo */
                dup 0
                dup 3
                mul
                // _ right_hi right_lo left_hi left_lo (left_lo · right_lo)

                /* left_lo · right_hi (consume both) */
                swap 4
                mul
                // _ (left_lo · right_lo) right_lo left_hi (left_lo · right_hi)

                /* left_hi · right_lo (consume both) */
                swap 2
                mul
                // _ (left_lo · right_lo) (left_lo · right_hi) (left_hi · right_lo)
                // _ lolo                 lohi                 hilo

                /* prod_hi = lolo_hi + lohi_lo + hilo_lo */
                split
                pick 1
                pop 1
                // _ lolo lohi hilo_lo

                pick 1
                split
                pick 1
                pop 1
                // _ lolo hilo_lo lohi_lo

                pick 2
                split
                // _ hilo_lo lohi_lo lolo_hi lolo_lo
                // _ hilo_lo lohi_lo lolo_hi prod_lo

                place 3
                add
                add
                // _ prod_lo (hilo_lo + lohi_lo + lolo_hi)

                split
                pick 1
                pop 1
                // _ prod_lo (hilo_lo + lohi_lo + lolo_hi)_lo
                // _ prod_lo prod_hi

                place 1
                return
        )
    }

    fn sign_offs(&self) -> HashMap<Reviewer, SignOffFingerprint> {
        let mut sign_offs = HashMap::new();
        sign_offs.insert(Reviewer("ferdinand"), 0xa50ce435d2381aca.into());
        sign_offs
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_prelude::*;

    impl Closure for WrappingMul {
        type Args = (u64, u64);

        fn rust_shadow(&self, stack: &mut Vec<BFieldElement>) {
            let (right, left) = pop_encodable::<Self::Args>(stack);
            push_encodable(stack, &left.wrapping_mul(right));
        }

        fn pseudorandom_args(
            &self,
            seed: [u8; 32],
            bench_case: Option<BenchmarkCase>,
        ) -> Self::Args {
            match bench_case {
                Some(BenchmarkCase::CommonCase) => (1 << 31, (1 << 25) - 1),
                Some(BenchmarkCase::WorstCase) => (1 << 53, (1 << 33) - 1),
                None => StdRng::from_seed(seed).random(),
            }
        }
    }

    #[test]
    fn rust_shadow() {
        ShadowedClosure::new(WrappingMul).test();
    }
}

#[cfg(test)]
mod benches {
    use super::*;
    use crate::test_prelude::*;

    #[test]
    fn benchmark() {
        ShadowedClosure::new(WrappingMul).bench();
    }
}
