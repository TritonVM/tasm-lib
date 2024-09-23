use std::collections::HashMap;

use itertools::Itertools;
use num::One;
use num::Zero;
use rand::prelude::*;
use triton_vm::prelude::*;
use triton_vm::twenty_first::prelude::U32s;

use crate::data_type::DataType;
use crate::empty_stack;
use crate::library::Library;
use crate::push_encodable;
use crate::traits::basic_snippet::BasicSnippet;
use crate::traits::deprecated_snippet::DeprecatedSnippet;
use crate::InitVmState;

#[derive(Clone, Debug)]
pub struct LtU64ConsumeArgs;

impl BasicSnippet for LtU64ConsumeArgs {
    fn inputs(&self) -> Vec<(DataType, String)> {
        vec![
            (DataType::U64, "rhs".to_owned()),
            (DataType::U64, "lhs".to_owned()),
        ]
    }

    fn outputs(&self) -> Vec<(DataType, String)> {
        vec![(DataType::Bool, "lhs < rhs".to_owned())]
    }

    fn entrypoint(&self) -> String {
        "tasmlib_arithmetic_u64_lt".to_owned()
    }

    fn code(&self, _library: &mut Library) -> Vec<LabelledInstruction> {
        let entrypoint = self.entrypoint();

        triton_asm!(
            {entrypoint}:
                // _ rhs_hi rhs_lo lhs_hi lhs_lo

                /* calculate lhs_hi < rhs_hi || lhs_hi == rhs_hi && lhs_lo < rhs_lo */
                swap 3
                swap 2
                // _ lhs_lo rhs_hi lhs_hi rhs_lo

                dup 2
                dup 2
                lt
                // _ lhs_lo rhs_hi lhs_hi rhs_lo (rhs_hi > lhs_hi)

                swap 4
                lt
                // _ (rhs_hi > lhs_hi) rhs_hi lhs_hi (rhs_lo > lhs_lo)

                swap 2
                eq
                // _ (rhs_hi > lhs_hi) (rhs_lo > lhs_lo) (lhs_hi == rhs_hi)

                mul
                add
                // _ (rhs_hi > lhs_hi || rhs_lo > lhs_lo && lhs_hi == rhs_hi)

                return
        )
    }
}

/// This `LtU64PreserveArgs` does not consume its arguments, which is the norm
/// for tasm functions.
///
/// See `LtU64ConsumeArgs` for a variant that does.
#[derive(Clone, Debug)]
pub struct LtU64PreserveArgs;

impl DeprecatedSnippet for LtU64PreserveArgs {
    fn entrypoint_name(&self) -> String {
        "tasmlib_arithmetic_u64_lt_preserve_args".to_string()
    }

    fn input_field_names(&self) -> Vec<String> {
        vec![
            "rhs_hi".to_string(),
            "rhs_lo".to_string(),
            "lhs_hi".to_string(),
            "lhs_lo".to_string(),
        ]
    }

    fn input_types(&self) -> Vec<DataType> {
        vec![DataType::U64, DataType::U64]
    }

    fn output_field_names(&self) -> Vec<String> {
        vec![
            "rhs_hi".to_string(),
            "rhs_lo".to_string(),
            "lhs_hi".to_string(),
            "lhs_lo".to_string(),
            "(lhs < rhs)".to_string(),
        ]
    }

    fn output_types(&self) -> Vec<DataType> {
        vec![DataType::U64, DataType::U64, DataType::Bool]
    }

    fn stack_diff(&self) -> isize {
        1
    }

    /// BEFORE: _ rhs_hi rhs_lo lhs_hi lhs_lo
    /// AFTER:  _ rhs_hi rhs_lo lhs_hi lhs_lo  (lhs < rhs)
    fn function_code(&self, _library: &mut Library) -> String {
        let entrypoint = self.entrypoint_name();
        triton_asm!(
            // Before: _ rhs_hi rhs_lo lhs_hi lhs_lo
            // After:  _ rhs_hi rhs_lo lhs_hi lhs_lo (lhs < rhs)
            {entrypoint}:
                dup 3
                dup 2
                lt
                // _ rhs_hi rhs_lo lhs_hi lhs_lo (rhs_hi > lhs_hi)

                dup 4
                dup 3
                eq
                // _ rhs_hi rhs_lo lhs_hi lhs_lo (rhs_hi > lhs_hi) (rhs_hi == lhs_hi)

                dup 4
                dup 3
                lt
                // _ rhs_hi rhs_lo lhs_hi lhs_lo (rhs_hi > lhs_hi) (rhs_hi == lhs_hi) (rhs_lo > lhs_lo)

                mul
                add
                // _ rhs_hi rhs_lo lhs_hi lhs_lo ((rhs_hi > lhs_hi) || (rhs_hi == lhs_hi) && (rhs_lo > lhs_lo))

                return
        )
        .iter()
        .join("\n")
    }

    fn crash_conditions(&self) -> Vec<String> {
        vec!["if inputs are not u32".to_string()]
    }

    fn gen_input_states(&self) -> Vec<crate::InitVmState> {
        let mut ret: Vec<InitVmState> = vec![];

        for _ in 0..30 {
            let n: u64 = rand::thread_rng().next_u64();
            let n: U32s<2> = n.try_into().unwrap();
            let m: u64 = rand::thread_rng().next_u64();
            let m: U32s<2> = m.try_into().unwrap();
            let mut input_stack = empty_stack();

            push_encodable(&mut input_stack, &n);
            push_encodable(&mut input_stack, &m);

            ret.push(InitVmState::with_stack(input_stack))
        }

        ret
    }

    fn common_case_input_state(&self) -> InitVmState {
        InitVmState::with_stack(
            [
                empty_stack(),
                vec![BFieldElement::zero(), BFieldElement::new(1 << 31)],
                vec![BFieldElement::one(), BFieldElement::new(1 << 30)],
            ]
            .concat(),
        )
    }

    fn worst_case_input_state(&self) -> InitVmState {
        InitVmState::with_stack(
            [
                empty_stack(),
                vec![BFieldElement::new(8), BFieldElement::new(1 << 31)],
                vec![BFieldElement::new(8), BFieldElement::new(1 << 30)],
            ]
            .concat(),
        )
    }

    fn rust_shadowing(
        &self,
        stack: &mut Vec<BFieldElement>,
        _std_in: Vec<BFieldElement>,
        _secret_in: Vec<BFieldElement>,
        _memory: &mut HashMap<BFieldElement, BFieldElement>,
    ) {
        // top element on stack
        let a0: u32 = stack[stack.len() - 1].try_into().unwrap();
        let b0: u32 = stack[stack.len() - 2].try_into().unwrap();
        let lhs = U32s::<2>::new([a0, b0]);

        // second element on stack
        let a1: u32 = stack[stack.len() - 3].try_into().unwrap();
        let b1: u32 = stack[stack.len() - 4].try_into().unwrap();
        let rhs = U32s::<2>::new([a1, b1]);

        stack.push(if lhs < rhs {
            BFieldElement::one()
        } else {
            BFieldElement::zero()
        });
    }
}

#[cfg(test)]
mod tests {
    use rand::prelude::*;

    use super::*;
    use crate::empty_stack;
    use crate::snippet_bencher::BenchmarkCase;
    use crate::test_helpers::test_rust_equivalence_given_complete_state;
    use crate::test_helpers::test_rust_equivalence_given_input_values_deprecated;
    use crate::test_helpers::test_rust_equivalence_multiple_deprecated;
    use crate::traits::closure::Closure;
    use crate::traits::closure::ShadowedClosure;
    use crate::traits::rust_shadow::RustShadow;

    // FIXME: Use `rng.gen()` after this is released:
    // https://github.com/Neptune-Crypto/twenty-first/pull/80
    fn random_gen() -> U32s<2> {
        let mut rng = rand::thread_rng();
        U32s::new([rng.gen(), rng.gen()])
    }

    #[test]
    fn lt_u64_test_new_snippet() {
        test_rust_equivalence_multiple_deprecated(&LtU64PreserveArgs, true);
    }

    #[test]
    fn u32s_lt_true_with_hi() {
        // 15 * 2^32 > 11 * 2^32
        let expected_end_stack = [
            empty_stack(),
            vec![BFieldElement::new(15), BFieldElement::new(0)],
            vec![BFieldElement::new(11), BFieldElement::new(0)],
            vec![BFieldElement::one()],
        ]
        .concat();

        let lhs = U32s::try_from(11 * (1u64 << 32)).unwrap();
        let rhs = U32s::try_from(15 * (1u64 << 32)).unwrap();
        prop_lt_preserve(lhs, rhs, Some(&expected_end_stack));
        prop_lt_consume(lhs, rhs);
    }

    #[test]
    fn u32s_lt_false_with_zero() {
        // eval(0 < 0) = false
        let expected_end_stack = [
            empty_stack(),
            vec![BFieldElement::new(0), BFieldElement::new(0)],
            vec![BFieldElement::new(0), BFieldElement::new(0)],
            vec![BFieldElement::zero()],
        ]
        .concat();
        let zero = U32s::zero();
        prop_lt_preserve(zero, zero, Some(&expected_end_stack));
        prop_lt_consume(zero, zero);
    }

    #[test]
    fn u32s_lt_pbt() {
        for _ in 0..100 {
            let lhs: U32s<2> = random_gen();
            let rhs: U32s<2> = random_gen();
            prop_lt_preserve(lhs, rhs, None);
            prop_lt_consume(lhs, rhs);
        }
    }

    #[test]
    fn u32s_lt_true_pbt() {
        let mut rng = rand::thread_rng();
        let init_stack = empty_stack();
        for _ in 0..100 {
            let rhs: u64 = rng.gen();
            let lhs: u64 = rng.gen_range(0..rhs);
            let rhs: U32s<2> = U32s::try_from(rhs).unwrap();
            let lhs: U32s<2> = U32s::try_from(lhs).unwrap();
            let expected = [
                init_stack.clone(),
                rhs.encode().into_iter().rev().collect(),
                lhs.encode().into_iter().rev().collect(),
                vec![BFieldElement::one()],
            ]
            .concat();

            prop_lt_preserve(lhs, rhs, Some(&expected));
            prop_lt_consume(lhs, rhs);
        }
    }

    #[test]
    fn u32s_lt_false_pbt() {
        let mut rng = rand::thread_rng();
        let init_stack = empty_stack();
        for _ in 0..100 {
            let lhs: u64 = rng.gen();
            let rhs: u64 = rng.gen_range(0..=lhs);
            let rhs: U32s<2> = U32s::try_from(rhs).unwrap();
            let lhs: U32s<2> = U32s::try_from(lhs).unwrap();
            let expected = [
                init_stack.clone(),
                rhs.encode().into_iter().rev().collect(),
                lhs.encode().into_iter().rev().collect(),
                vec![BFieldElement::zero()],
            ]
            .concat();

            prop_lt_preserve(lhs, rhs, Some(&expected));
            prop_lt_consume(lhs, rhs);
        }
    }

    #[test]
    fn u32s_lt_false_because_equal_pbt() {
        let mut rng = rand::thread_rng();
        let init_stack = empty_stack();
        for _ in 0..100 {
            let lhs: u64 = rng.gen();
            let rhs: u64 = lhs;
            let rhs: U32s<2> = U32s::try_from(rhs).unwrap();
            let lhs: U32s<2> = U32s::try_from(lhs).unwrap();
            let expected = [
                init_stack.clone(),
                rhs.encode().into_iter().rev().collect(),
                lhs.encode().into_iter().rev().collect(),
                vec![BFieldElement::zero()],
            ]
            .concat();

            prop_lt_preserve(lhs, rhs, Some(&expected));
            prop_lt_consume(lhs, rhs);
        }
    }

    fn prop_lt_preserve(lhs: U32s<2>, rhs: U32s<2>, expected: Option<&[BFieldElement]>) {
        let mut init_stack = empty_stack();
        init_stack.append(&mut rhs.encode().into_iter().rev().collect());
        init_stack.append(&mut lhs.encode().into_iter().rev().collect());

        let stdin = &[];
        test_rust_equivalence_given_input_values_deprecated(
            &LtU64PreserveArgs,
            &init_stack,
            stdin,
            HashMap::default(),
            expected,
        );
    }

    fn prop_lt_consume(lhs: U32s<2>, rhs: U32s<2>) {
        let mut init_stack = empty_stack();
        init_stack.append(&mut rhs.encode().into_iter().rev().collect());
        init_stack.append(&mut lhs.encode().into_iter().rev().collect());

        let stdin = &[];
        let expected = None;
        test_rust_equivalence_given_complete_state(
            &ShadowedClosure::new(LtU64ConsumeArgs),
            &init_stack,
            stdin,
            &NonDeterminism::default(),
            &None,
            expected,
        );
    }

    #[test]
    fn lt_u64_pbt() {
        ShadowedClosure::new(LtU64ConsumeArgs).test()
    }

    impl LtU64ConsumeArgs {
        fn init_state(&self, rhs: u64, lhs: u64) -> Vec<BFieldElement> {
            [
                self.init_stack_for_isolated_run(),
                rhs.encode(),
                lhs.encode(),
            ]
            .concat()
        }
    }

    impl Closure for LtU64ConsumeArgs {
        fn rust_shadow(&self, stack: &mut Vec<BFieldElement>) {
            let lhs = U32s::new([
                stack.pop().unwrap().try_into().unwrap(),
                stack.pop().unwrap().try_into().unwrap(),
            ]);
            let rhs = U32s::new([
                stack.pop().unwrap().try_into().unwrap(),
                stack.pop().unwrap().try_into().unwrap(),
            ]);

            stack.push(bfe!((lhs < rhs) as u64));
        }

        fn pseudorandom_initial_state(
            &self,
            seed: [u8; 32],
            bench_case: Option<BenchmarkCase>,
        ) -> Vec<BFieldElement> {
            match bench_case {
                Some(BenchmarkCase::CommonCase) => self.init_state(
                    (1u64 << 40) + u32::MAX as u64 - 1,
                    (1u64 << 40) + u32::MAX as u64,
                ),
                Some(BenchmarkCase::WorstCase) => self.init_state(u64::MAX, u64::MAX - 1),
                None => {
                    let mut rng: StdRng = SeedableRng::from_seed(seed);
                    self.init_state(rng.gen(), rng.gen())
                }
            }
        }

        fn corner_case_initial_states(&self) -> Vec<Vec<BFieldElement>> {
            let zero_zero = self.init_state(0, 0);
            let max_max = self.init_state(u64::MAX, u64::MAX);
            let same_hi0_same_lo = self.init_state(1 << 31, 1 << 31);
            let same_hi0_greater_lo = self.init_state(1 << 29, 1 << 31);
            let same_hi0_less_lo = self.init_state(1 << 31, 1 << 29);
            let same_hi1_same_lo = self.init_state(1 << 32, 1 << 32);
            let same_hi1_greater_lo = self.init_state(1 << 32, (1 << 32) + 5);
            let same_hi1_less_lo = self.init_state((1 << 32) + 5, 1 << 32);

            vec![
                zero_zero,
                max_max,
                same_hi0_same_lo,
                same_hi0_greater_lo,
                same_hi0_less_lo,
                same_hi1_same_lo,
                same_hi1_greater_lo,
                same_hi1_less_lo,
            ]
        }
    }
}

#[cfg(test)]
mod benches {
    use super::*;
    use crate::snippet_bencher::bench_and_write;
    use crate::traits::closure::ShadowedClosure;
    use crate::traits::rust_shadow::RustShadow;

    #[test]
    fn lt_u64_benchmark_other() {
        bench_and_write(LtU64PreserveArgs);
    }

    #[test]
    fn lt_u64_benchmark() {
        ShadowedClosure::new(LtU64ConsumeArgs).bench();
    }
}
