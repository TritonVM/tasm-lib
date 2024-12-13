use std::collections::HashMap;

use triton_vm::prelude::*;

use crate::data_type::DataType;
use crate::prelude::*;
use crate::traits::basic_snippet::Reviewer;
use crate::traits::basic_snippet::SignOffFingerprint;

/// Count [the number of ones](u64::count_ones) in the binary representation of
/// the argument, also known as its population count.
///
/// ### Behavior
///
/// ```text
/// BEFORE: _ [x: u64]
/// AFTER:  _ [result: u32]
/// ```
///
/// ### Preconditions
///
/// - the input argument is properly [`BFieldCodec`] encoded
///
/// ### Postconditions
///
/// - the output equals the number of 1s in the binary representation of the
///   input argument
/// - the output is properly [`BFieldCodec`] encoded
#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub struct PopCount;

impl BasicSnippet for PopCount {
    fn inputs(&self) -> Vec<(DataType, String)> {
        vec![(DataType::U64, "x".to_string())]
    }

    fn outputs(&self) -> Vec<(DataType, String)> {
        vec![(DataType::U32, "pop_count(x)".to_string())]
    }

    fn entrypoint(&self) -> String {
        "tasmlib_arithmetic_u64_popcount".to_string()
    }

    fn code(&self, _: &mut Library) -> Vec<LabelledInstruction> {
        triton_asm!(
            {self.entrypoint()}:
                pop_count
                pick 1
                pop_count
                add
                return
        )
    }

    fn sign_offs(&self) -> HashMap<Reviewer, SignOffFingerprint> {
        let mut sign_offs = HashMap::new();
        sign_offs.insert(Reviewer("ferdinand"), 0xeaae261c6df2bcbb.into());
        sign_offs
    }
}

#[cfg(test)]
mod tests {
    use rand::prelude::*;

    use super::*;
    use crate::pop_encodable;
    use crate::push_encodable;
    use crate::snippet_bencher::BenchmarkCase;
    use crate::traits::closure::Closure;
    use crate::traits::closure::ShadowedClosure;
    use crate::traits::rust_shadow::RustShadow;

    impl PopCount {
        pub fn set_up_initial_stack(&self, x: u64) -> Vec<BFieldElement> {
            let mut stack = self.init_stack_for_isolated_run();
            push_encodable(&mut stack, &x);
            stack
        }
    }

    impl Closure for PopCount {
        fn rust_shadow(&self, stack: &mut Vec<BFieldElement>) {
            let x = pop_encodable::<u64>(stack);
            push_encodable(stack, &x.count_ones());
        }

        fn pseudorandom_initial_state(
            &self,
            seed: [u8; 32],
            bench_case: Option<BenchmarkCase>,
        ) -> Vec<BFieldElement> {
            let x = match bench_case {
                Some(BenchmarkCase::CommonCase) => 1 << 25,
                Some(BenchmarkCase::WorstCase) => u64::MAX,
                None => StdRng::from_seed(seed).gen(),
            };

            self.set_up_initial_stack(x)
        }

        fn corner_case_initial_states(&self) -> Vec<Vec<BFieldElement>> {
            [1, 1 << 32, u64::MAX]
                .into_iter()
                .flat_map(|x| [x.checked_sub(1), Some(x), x.checked_add(1)])
                .flatten()
                .map(|x| self.set_up_initial_stack(x))
                .collect()
        }
    }

    #[test]
    fn rust_shadow_test() {
        ShadowedClosure::new(PopCount).test()
    }
}

#[cfg(test)]
mod benches {
    use super::*;
    use crate::traits::closure::ShadowedClosure;
    use crate::traits::rust_shadow::RustShadow;

    #[test]
    fn benchmark() {
        ShadowedClosure::new(PopCount).bench();
    }
}
