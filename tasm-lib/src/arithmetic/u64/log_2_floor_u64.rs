use triton_vm::prelude::*;

use crate::data_type::DataType;
use crate::library::Library;
use crate::traits::basic_snippet::BasicSnippet;

#[derive(Clone, Debug, Copy)]
pub struct Log2FloorU64;

impl BasicSnippet for Log2FloorU64 {
    fn inputs(&self) -> Vec<(DataType, String)> {
        vec![(DataType::U64, "x".to_owned())]
    }

    fn outputs(&self) -> Vec<(DataType, String)> {
        vec![(DataType::U32, "log_2_floor(x)".to_owned())]
    }

    fn entrypoint(&self) -> String {
        "tasmlib_arithmetic_u64_log_2_floor".to_string()
    }

    fn code(&self, _library: &mut Library) -> Vec<LabelledInstruction> {
        let entrypoint = self.entrypoint();

        // assumes that top of stack is a valid u32s<2>
        let hi_not_zero = format!("{entrypoint}_hi_not_zero");
        let hi_is_zero = format!("{entrypoint}_hi_is_zero");
        triton_asm!(
                // BEFORE: _ value_hi value_lo
                // AFTER:  _ log2_floor(value)
                {entrypoint}:
                    push 1
                    dup 2
                    // _ value_hi value_lo 1 value_hi

                    skiz call {hi_not_zero}
                    skiz call {hi_is_zero}
                    // stack: _ log2_floor(value)

                    return

                {hi_not_zero}:
                    // value_hi != 0
                    // stack: // stack: _ value_hi value_lo 1

                    pop 2
                    // stack: _ value_hi

                    log_2_floor
                    push 32
                    add
                    // stack: _ (log2_floor(value_hi) + 32)

                    push 0
                    // stack: _ (log2_floor(value_hi) + 32) 0

                    return

                {hi_is_zero}:
                    // value_hi == 0
                    // stack: _ 0 value_lo
                    swap 1
                    pop 1
                    log_2_floor
                    // stack: _ log_2_floor(value_lo)

                    return
        )
    }
}

#[cfg(test)]
mod tests {
    use num::Zero;
    use rand::rngs::StdRng;
    use rand::RngCore;
    use rand::SeedableRng;

    use crate::empty_stack;
    use crate::snippet_bencher::BenchmarkCase;
    use crate::test_helpers::negative_test;
    use crate::test_helpers::test_rust_equivalence_given_complete_state;
    use crate::traits::closure::Closure;
    use crate::traits::closure::ShadowedClosure;
    use crate::traits::rust_shadow::RustShadow;
    use crate::InitVmState;

    use super::*;

    impl Log2FloorU64 {
        fn init_state(&self, x: u64) -> Vec<BFieldElement> {
            [
                self.init_stack_for_isolated_run(),
                vec![bfe!(x >> 32), bfe!(x & u32::MAX as u64)],
            ]
            .concat()
        }
    }

    impl Closure for Log2FloorU64 {
        fn rust_shadow(&self, stack: &mut Vec<BFieldElement>) {
            // Get input value as a u64
            let lo: u32 = stack.pop().unwrap().try_into().unwrap();
            let hi: u32 = stack.pop().unwrap().try_into().unwrap();
            let value_u64: u64 = lo as u64 + (1 << 32) * (hi as u64);

            let log_2_floor = value_u64.ilog2() as u64;
            stack.push(BFieldElement::new(log_2_floor));
        }

        fn pseudorandom_initial_state(
            &self,
            seed: [u8; 32],
            bench_case: Option<crate::snippet_bencher::BenchmarkCase>,
        ) -> Vec<BFieldElement> {
            match bench_case {
                Some(BenchmarkCase::CommonCase) => self.init_state(u32::MAX as u64),
                Some(BenchmarkCase::WorstCase) => self.init_state(u64::MAX),
                None => {
                    let mut rng: StdRng = SeedableRng::from_seed(seed);
                    self.init_state(rng.next_u64())
                }
            }
        }

        fn corner_case_initial_states(&self) -> Vec<Vec<BFieldElement>> {
            let mut all_powers_of_two = vec![];
            for i in 0..63 {
                all_powers_of_two.push(self.init_state(1 << i));
            }

            all_powers_of_two
        }
    }

    #[test]
    fn log_2_floor_u64_test() {
        ShadowedClosure::new(Log2FloorU64).test();
    }

    #[test]
    fn lo_is_not_u32_hi_is_zero() {
        let not_u32 = BFieldElement::new(u32::MAX as u64 + 1);
        let mut init_stack = Log2FloorU64.init_stack_for_isolated_run();
        init_stack.push(BFieldElement::new(0));
        init_stack.push(not_u32);

        let init_state = InitVmState::with_stack(init_stack);
        negative_test(
            &ShadowedClosure::new(Log2FloorU64),
            init_state,
            &[InstructionError::FailedU32Conversion(not_u32)],
        );
    }

    #[test]
    fn hi_is_not_u32() {
        let not_u32 = BFieldElement::new(u32::MAX as u64 + 1);
        let mut init_stack = Log2FloorU64.init_stack_for_isolated_run();
        init_stack.push(not_u32);
        init_stack.push(BFieldElement::new(16));

        let init_state = InitVmState::with_stack(init_stack);
        negative_test(
            &ShadowedClosure::new(Log2FloorU64),
            init_state,
            &[InstructionError::FailedU32Conversion(not_u32)],
        );
    }

    #[test]
    fn hi_is_not_u32_alt() {
        let n: u64 = rand::thread_rng().next_u32() as u64;
        let not_u32 = BFieldElement::new(u32::MAX as u64 + 1 + n);
        let mut init_stack = Log2FloorU64.init_stack_for_isolated_run();
        init_stack.push(not_u32);
        init_stack.push(BFieldElement::new(16));

        let init_state = InitVmState::with_stack(init_stack);
        negative_test(
            &ShadowedClosure::new(Log2FloorU64),
            init_state,
            &[InstructionError::FailedU32Conversion(not_u32)],
        );
    }

    #[test]
    fn crash_on_zero() {
        let init_stack = [
            Log2FloorU64.init_stack_for_isolated_run(),
            vec![BFieldElement::zero(), BFieldElement::zero()],
        ]
        .concat();
        let init_state = InitVmState::with_stack(init_stack);
        negative_test(
            &ShadowedClosure::new(Log2FloorU64),
            init_state,
            &[InstructionError::LogarithmOfZero],
        );
    }

    #[test]
    fn u32s_2_log2_floor() {
        let mut expected = Log2FloorU64.init_stack_for_isolated_run();
        expected.push(BFieldElement::new(0));
        prop_log2_floor_u32s_2(1, &expected);

        expected = Log2FloorU64.init_stack_for_isolated_run();
        expected.push(BFieldElement::new(1));
        prop_log2_floor_u32s_2(2, &expected);
        prop_log2_floor_u32s_2(3, &expected);

        expected = Log2FloorU64.init_stack_for_isolated_run();
        expected.push(BFieldElement::new(2));
        prop_log2_floor_u32s_2(4, &expected);
        prop_log2_floor_u32s_2(5, &expected);
        prop_log2_floor_u32s_2(6, &expected);
        prop_log2_floor_u32s_2(7, &expected);

        expected = Log2FloorU64.init_stack_for_isolated_run();
        expected.push(BFieldElement::new(31));
        prop_log2_floor_u32s_2(u32::MAX as u64 - 20000, &expected);
        prop_log2_floor_u32s_2(u32::MAX as u64 - 1, &expected);
        prop_log2_floor_u32s_2(u32::MAX as u64, &expected);

        expected = Log2FloorU64.init_stack_for_isolated_run();
        expected.push(BFieldElement::new(32));
        prop_log2_floor_u32s_2(u32::MAX as u64 + 1, &expected);
        prop_log2_floor_u32s_2(u32::MAX as u64 + 2, &expected);
        prop_log2_floor_u32s_2(u32::MAX as u64 + 800, &expected);
        prop_log2_floor_u32s_2(u32::MAX as u64 + u32::MAX as u64, &expected);
        prop_log2_floor_u32s_2(u32::MAX as u64 + u32::MAX as u64 + 1, &expected);

        for i in 0..64 {
            expected = Log2FloorU64.init_stack_for_isolated_run();
            expected.push(BFieldElement::new(i));
            prop_log2_floor_u32s_2(1 << i, &expected);
            if i > 0 {
                prop_log2_floor_u32s_2((1 << i) + 1, &expected);
            }
            if i > 1 {
                prop_log2_floor_u32s_2((1 << i) + 2, &expected);
                prop_log2_floor_u32s_2((1 << i) + 3, &expected);

                expected = empty_stack();
                expected.push(BFieldElement::new(i - 1));
                prop_log2_floor_u32s_2((1 << i) - 1, &expected);
            }
        }
    }

    fn prop_log2_floor_u32s_2(value: u64, expected: &[BFieldElement]) {
        let mut init_stack = empty_stack();
        for elem in value.encode().into_iter().rev() {
            init_stack.push(elem);
        }

        test_rust_equivalence_given_complete_state(
            &ShadowedClosure::new(Log2FloorU64),
            &init_stack,
            &[],
            &NonDeterminism::default(),
            &None,
            Some(expected),
        );
    }
}

#[cfg(test)]
mod benches {
    use crate::traits::closure::ShadowedClosure;
    use crate::traits::rust_shadow::RustShadow;

    use super::*;

    #[test]
    fn log_2_floor_u64_benchmark() {
        ShadowedClosure::new(Log2FloorU64).bench();
    }
}
