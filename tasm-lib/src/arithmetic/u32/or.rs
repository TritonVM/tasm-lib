use std::collections::HashMap;

use triton_vm::prelude::*;

use crate::prelude::*;
use crate::traits::basic_snippet::Reviewer;
use crate::traits::basic_snippet::SignOffFingerprint;

/// [Bitwise “or”][bitor] (“`|`”) for `u32`s.
///
/// ### Behavior
///
/// ```text
/// BEFORE: _ [right: u32] [left: u32]
/// AFTER:  _ [left | right: u32]
/// ```
///
/// ### Preconditions
///
/// - all input arguments are properly [`BFieldCodec`] encoded
///
/// ### Postconditions
///
/// - the output is the bitwise “or” of the input
/// - the output is properly [`BFieldCodec`] encoded
///
/// [bitor]: core::ops::BitOr
#[derive(Debug, Default, Copy, Clone, Eq, PartialEq, Hash)]
pub struct Or;

impl BasicSnippet for Or {
    fn parameters(&self) -> Vec<(DataType, String)> {
        ["right", "left"]
            .map(|s| (DataType::U32, s.to_string()))
            .to_vec()
    }

    fn return_values(&self) -> Vec<(DataType, String)> {
        vec![(DataType::U32, "left | right".to_string())]
    }

    fn entrypoint(&self) -> String {
        "tasmlib_arithmetic_u32_or".to_string()
    }

    fn code(&self, _: &mut Library) -> Vec<LabelledInstruction> {
        triton_asm!(
            // BEFORE: _ right left
            // AFTER:  _ (left | right)
            {self.entrypoint()}:
                dup 1
                dup 1       // _ right left right left
                xor         // _ right left (right ^ left)
                place 2
                and         // _ (right ^ left) (right & left)
                add
                return
        )
    }

    fn sign_offs(&self) -> HashMap<Reviewer, SignOffFingerprint> {
        let mut sign_offs = HashMap::new();
        sign_offs.insert(Reviewer("ferdinand"), 0x7e5a9b6cfb38b388.into());
        sign_offs
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_prelude::*;

    impl Closure for Or {
        type Args = (u32, u32);

        fn rust_shadow(&self, stack: &mut Vec<BFieldElement>) {
            let (right, left) = pop_encodable::<Self::Args>(stack);
            let or = left | right;
            push_encodable(stack, &or);
        }

        fn pseudorandom_args(
            &self,
            seed: [u8; 32],
            bench_case: Option<BenchmarkCase>,
        ) -> Self::Args {
            match bench_case {
                Some(BenchmarkCase::CommonCase) => (1 << 15, 1 << 16),
                Some(BenchmarkCase::WorstCase) => (u32::MAX, u32::MAX),
                None => StdRng::from_seed(seed).random(),
            }
        }

        fn corner_case_args(&self) -> Vec<Self::Args> {
            let edges = [0, 1, 2, 0b1110, 0b11110, 1 << 30, 1 << 31, u32::MAX];

            edges.into_iter().cartesian_product(edges).collect()
        }
    }

    #[test]
    fn rust_shadow() {
        ShadowedClosure::new(Or).test();
    }
}

#[cfg(test)]
mod benches {
    use super::*;
    use crate::test_prelude::*;

    #[test]
    fn benchmark() {
        ShadowedClosure::new(Or).bench();
    }
}
