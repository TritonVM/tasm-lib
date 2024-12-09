use triton_vm::isa::triton_asm;
use triton_vm::prelude::LabelledInstruction;

use crate::arithmetic;
use crate::data_type::DataType;
use crate::library::Library;
use crate::prelude::BasicSnippet;

/// Less-than operator for `i128`
///
/// # Behavior
///
/// BEFORE: `_ [rhs: i128] [lhs: i128]`
///
/// AFTER: `_ (lhs < rhs)`
///
/// # Preconditions
///
///  - `rhs` and `lhs` consists of 4 `u32`s.
///
/// # Postconditions
///
///  - `result = lhs < rhs` is `true` or `false`.
///
/// # Panics
///
///  - If preconditions are not met.
pub struct Lt;

impl BasicSnippet for Lt {
    fn inputs(&self) -> Vec<(DataType, String)> {
        ["rhs", "lhs"]
            .map(|s| (DataType::I128, s.to_string()))
            .to_vec()
    }

    fn outputs(&self) -> Vec<(DataType, String)> {
        vec![(DataType::Bool, "result".to_string())]
    }

    fn entrypoint(&self) -> String {
        "tasmlib_arithmetic_i128_lt".to_owned()
    }

    fn code(&self, library: &mut Library) -> Vec<LabelledInstruction> {
        let entrypoint = self.entrypoint();
        let u128_lt = library.import(Box::new(arithmetic::u128::lt_u128::LtU128));

        const TWO_POW_31: u32 = 1 << 31;
        let flip_msbs = triton_asm!(
            // _ [rhs: i128] [lhs: i128]
            swap 3
            push {TWO_POW_31}
            xor
            swap 3

            swap 7
            push {TWO_POW_31}
            xor
            swap 7
            // _ [rhs+(1<<127): u128] [rhs+(1<<127): u128]
        );

        triton_asm!(
            {entrypoint}:
                // _ [rhs] [lhs]

                /* Flip most-significant bit, then do u128-lt. */

                {&flip_msbs}
                // _ [rhs'] [lhs']

                call {u128_lt}
                // _ (rhs > lhs)

                return
        )
    }
}

#[cfg(test)]
mod tests {
    use itertools::Itertools;
    use proptest_arbitrary_interop::arb;
    use rand::random;
    use rand::rngs::StdRng;
    use rand::Rng;
    use rand::SeedableRng;
    use test_strategy::proptest;
    use triton_vm::error::InstructionError;
    use triton_vm::error::OpStackError;
    use triton_vm::prelude::bfe;
    use triton_vm::prelude::BFieldElement;
    use triton_vm::vm::NonDeterminism;

    use super::*;
    use crate::push_encodable;
    use crate::snippet_bencher::BenchmarkCase;
    use crate::test_helpers::negative_test;
    use crate::test_helpers::test_rust_equivalence_given_complete_state;
    use crate::traits::closure::Closure;
    use crate::traits::closure::ShadowedClosure;
    use crate::traits::rust_shadow::RustShadow;
    use crate::InitVmState;

    #[test]
    fn i128_lt_proptest() {
        ShadowedClosure::new(Lt).test();
    }

    #[proptest]
    fn input_not_u32_words_negative_test(
        #[strategy(0usize..8)] stack_index_bad_word: usize,
        #[strategy(arb())]
        #[filter(#bad_word.value() > u32::MAX as u64)]
        bad_word: BFieldElement,
    ) {
        fn init_state_not_u32_words(
            stack_index_bad_word: usize,
            bad_word: BFieldElement,
        ) -> InitVmState {
            let mut stack = Lt.init_state(random(), random());
            let last_elem_index = stack.len() - 1;
            stack[last_elem_index - stack_index_bad_word] = bad_word;

            InitVmState::with_stack(stack)
        }

        negative_test(
            &ShadowedClosure::new(Lt),
            init_state_not_u32_words(stack_index_bad_word, bad_word),
            &[InstructionError::OpStackError(
                OpStackError::FailedU32Conversion(bad_word),
            )],
        );
    }

    #[test]
    fn i128_lt_unit_test_min_max() {
        let init_stack = Lt.init_state(i128::MIN, i128::MAX);
        let mut expected_end_stack = Lt.init_stack_for_isolated_run();
        expected_end_stack.push(bfe!(true as u64));

        let stdin = &[];
        test_rust_equivalence_given_complete_state(
            &ShadowedClosure::new(Lt),
            &init_stack,
            stdin,
            &NonDeterminism::default(),
            &None,
            Some(&expected_end_stack),
        );
    }

    #[test]
    fn i128_lt_unit_test_zero_zero() {
        let init_stack = Lt.init_state(0, 0);
        let mut expected_end_stack = Lt.init_stack_for_isolated_run();
        expected_end_stack.push(bfe!(false as u64));

        let stdin = &[];
        test_rust_equivalence_given_complete_state(
            &ShadowedClosure::new(Lt),
            &init_stack,
            stdin,
            &NonDeterminism::default(),
            &None,
            Some(&expected_end_stack),
        );
    }

    impl Lt {
        fn init_state(&self, lhs: i128, rhs: i128) -> Vec<BFieldElement> {
            let mut stack = self.init_stack_for_isolated_run();
            push_encodable(&mut stack, &(rhs as u128));
            push_encodable(&mut stack, &(lhs as u128));

            stack
        }
    }

    impl Closure for Lt {
        fn rust_shadow(&self, stack: &mut Vec<BFieldElement>) {
            fn pop_i128(stack: &mut Vec<BFieldElement>) -> i128 {
                let num_limbs = 4;
                let limbs: Vec<u32> = (0..num_limbs)
                    .map(|_| stack.pop().unwrap().try_into().unwrap())
                    .collect_vec();

                let mut acc = ((limbs[3] as i32) as i128) << 96;
                for (i, limb) in limbs[0..3].iter().enumerate() {
                    acc += (*limb as i128) << (i * 32);
                }

                acc
            }
            let lhs = pop_i128(stack);
            let rhs = pop_i128(stack);

            stack.push(bfe!((lhs < rhs) as u64));
        }

        fn pseudorandom_initial_state(
            &self,
            seed: [u8; 32],
            _bench_case: Option<BenchmarkCase>,
        ) -> Vec<BFieldElement> {
            let mut rng = StdRng::from_seed(seed);
            self.init_state(rng.gen(), rng.gen())
        }

        fn corner_case_initial_states(&self) -> Vec<Vec<BFieldElement>> {
            let points_with_plus_minus_one = [
                i128::MIN,
                -(1 << 96),
                -(1 << 64),
                -(1 << 32),
                -1,
                0,
                1,
                1 << 32,
                1 << 64,
                1 << 96,
                1 << 126,
                i128::MAX,
            ]
            .into_iter()
            .flat_map(|p| [p.checked_sub(1), Some(p), p.checked_add(1)])
            .flatten()
            .collect_vec();

            points_with_plus_minus_one
                .iter()
                .cartesian_product(&points_with_plus_minus_one)
                .map(|(&l, &r)| self.init_state(l, r))
                .collect()
        }
    }
}

#[cfg(test)]
mod benches {
    use super::*;
    use crate::traits::closure::ShadowedClosure;
    use crate::traits::rust_shadow::RustShadow;

    #[test]
    fn lt_i128_benchmark() {
        ShadowedClosure::new(Lt).bench();
    }
}
