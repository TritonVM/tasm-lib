use triton_vm::prelude::*;

use crate::data_type::DataType;
use crate::library::Library;
use crate::traits::basic_snippet::BasicSnippet;

/// Returns the smallest power of two greater than or equal to self.
/// Behaves like the `rustc` method for all inputs of type `u32`.
#[derive(Debug, Clone, Copy)]
pub struct NextPowerOfTwo;

impl BasicSnippet for NextPowerOfTwo {
    fn inputs(&self) -> Vec<(DataType, String)> {
        vec![(DataType::U32, "self".to_owned())]
    }

    fn outputs(&self) -> Vec<(DataType, String)> {
        vec![(DataType::U32, "next_power_of_two".to_owned())]
    }

    fn entrypoint(&self) -> String {
        "tasmlib_arithmetic_u32_next_power_of_two".to_string()
    }

    fn code(&self, _library: &mut Library) -> Vec<LabelledInstruction> {
        let entrypoint = self.entrypoint();
        let zero_or_one_label = format!("{entrypoint}_zero_or_one_label");
        let greater_than_one_label = format!("{entrypoint}_greater_than_one");
        triton_asm!(
            {entrypoint}:
                // _ self_

                push 1
                push 2
                dup 2
                lt
                skiz
                    call {zero_or_one_label}
                skiz
                    call {greater_than_one_label}

                return

                {zero_or_one_label}:
                    // _ self_ 1

                    pop 2
                    // _

                    push 1
                    // _ result

                    push 0
                    // _ result 0

                    return

                {greater_than_one_label}:
                    // _ self_ 1

                    push -1
                    add
                    // _ (value - 1)

                    log_2_floor
                    push 1
                    add
                    // _ (log_2_floor(value - 1) + 1)

                    push 2
                    // _ (log_2_floor(value - 1) + 1) 2

                    pow
                    // _ npo2(self_)

                    // Assert result *not* 1^{32}
                    dup 0
                    push {1u64 << 32}
                    eq
                    push 0
                    eq
                    assert

                    return
        )
    }
}

#[cfg(test)]
mod tests {
    use itertools::Itertools;
    use rand::prelude::*;
    use triton_vm::isa::error::AssertionError;

    use super::*;
    use crate::test_helpers::negative_test;
    use crate::traits::closure::Closure;
    use crate::traits::closure::ShadowedClosure;
    use crate::traits::rust_shadow::RustShadow;
    use crate::InitVmState;

    impl Closure for NextPowerOfTwo {
        fn rust_shadow(&self, stack: &mut Vec<BFieldElement>) {
            let self_: u32 = stack.pop().unwrap().try_into().unwrap();
            let npo2: u32 = self_.next_power_of_two();
            stack.push(BFieldElement::new(npo2 as u64));
        }

        fn pseudorandom_initial_state(
            &self,
            seed: [u8; 32],
            bench_case: Option<crate::snippet_bencher::BenchmarkCase>,
        ) -> Vec<BFieldElement> {
            let self_: u32 = match bench_case {
                Some(crate::snippet_bencher::BenchmarkCase::CommonCase) => (1 << 27) - 1,
                Some(crate::snippet_bencher::BenchmarkCase::WorstCase) => (1 << 31) - 1,
                None => {
                    let mut rng = StdRng::from_seed(seed);
                    rng.next_u32() / 2
                }
            };

            self.prepare_vm_stack(self_)
        }

        fn corner_case_initial_states(&self) -> Vec<Vec<BFieldElement>> {
            let small_inputs = (0u32..=66).map(|i| self.prepare_vm_stack(i)).collect_vec();
            let big_but_valid_inputs = [
                1u32 << 31,
                (1u32 << 31) - 1,
                (1u32 << 31) - 2,
                (1u32 << 31) - 3,
                (1u32 << 31) - 4,
                (1u32 << 31) - 5,
            ]
            .into_iter()
            .map(|i| self.prepare_vm_stack(i))
            .collect_vec();

            [small_inputs, big_but_valid_inputs].concat()
        }
    }

    impl NextPowerOfTwo {
        fn prepare_vm_stack(&self, self_: u32) -> Vec<BFieldElement> {
            [self.init_stack_for_isolated_run(), bfe_vec![self_]].concat()
        }
    }

    #[test]
    fn next_power_of_two_pbt() {
        ShadowedClosure::new(NextPowerOfTwo).test()
    }

    #[test]
    fn npo2_overflow_negative_test() {
        for self_ in [
            (1u32 << 31) + 1,
            (1u32 << 31) + 2,
            (1u32 << 31) + 10,
            u32::MAX - 2,
            u32::MAX - 1,
            u32::MAX,
        ] {
            let init_stack = NextPowerOfTwo.prepare_vm_stack(self_);
            let init_state = InitVmState::with_stack(init_stack);

            let assertion_failure = InstructionError::AssertionFailed(AssertionError::new(1, 0));
            negative_test(
                &ShadowedClosure::new(NextPowerOfTwo),
                init_state,
                &[assertion_failure],
            );
        }
    }
}

#[cfg(test)]
mod benches {
    use super::*;
    use crate::traits::closure::ShadowedClosure;
    use crate::traits::rust_shadow::RustShadow;

    #[test]
    fn npo2_u32_bench() {
        ShadowedClosure::new(NextPowerOfTwo).bench()
    }
}
