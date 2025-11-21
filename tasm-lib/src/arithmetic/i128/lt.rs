use triton_vm::prelude::*;

use crate::arithmetic;
use crate::prelude::*;

/// Less-than operator for `i128`
///
/// # Behavior
///
/// ```text
/// BEFORE: _ [rhs: i128] [lhs: i128]
/// AFTER:  _ (lhs < rhs)
/// ```
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
#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub struct Lt;

impl BasicSnippet for Lt {
    fn parameters(&self) -> Vec<(DataType, String)> {
        ["rhs", "lhs"]
            .map(|s| (DataType::I128, s.to_string()))
            .to_vec()
    }

    fn return_values(&self) -> Vec<(DataType, String)> {
        vec![(DataType::Bool, "result".to_string())]
    }

    fn entrypoint(&self) -> String {
        "tasmlib_arithmetic_i128_lt".to_owned()
    }

    fn code(&self, library: &mut Library) -> Vec<LabelledInstruction> {
        let entrypoint = self.entrypoint();
        let u128_lt = library.import(Box::new(arithmetic::u128::lt::Lt));

        const TWO_POW_31: u32 = 1 << 31;
        let flip_msbs = triton_asm!(
            // _ [rhs: i128] [lhs: i128]
            pick 3
            push {TWO_POW_31}
            xor
            place 3

            pick 7
            push {TWO_POW_31}
            xor
            place 7
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
    use std::collections::HashSet;

    use triton_vm::error::InstructionError;
    use triton_vm::error::OpStackError;

    use super::*;
    use crate::test_helpers::negative_test;
    use crate::test_prelude::*;

    impl Closure for Lt {
        type Args = (i128, i128);

        fn rust_shadow(&self, stack: &mut Vec<BFieldElement>) {
            let (rhs, lhs) = pop_encodable::<Self::Args>(stack);
            push_encodable(stack, &(lhs < rhs));
        }

        fn pseudorandom_args(&self, seed: [u8; 32], _: Option<BenchmarkCase>) -> Self::Args {
            StdRng::from_seed(seed).random()
        }

        fn corner_case_args(&self) -> Vec<Self::Args> {
            let points_with_plus_minus_one = [
                i128::MIN,
                -(1 << 96),
                -(1 << 64),
                -(1 << 32),
                -1,
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
            .collect::<HashSet<_>>()
            .into_iter()
            .collect_vec();

            points_with_plus_minus_one
                .iter()
                .cartesian_product(&points_with_plus_minus_one)
                .map(|(&l, &r)| (l, r))
                .collect()
        }
    }

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
            let mut stack = Lt.set_up_test_stack(rand::random());
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
        let init_stack = Lt.set_up_test_stack((i128::MAX, i128::MIN));
        let mut expected_end_stack = Lt.init_stack_for_isolated_run();
        push_encodable(&mut expected_end_stack, &true);

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
        let init_stack = Lt.set_up_test_stack((0, 0));
        let mut expected_end_stack = Lt.init_stack_for_isolated_run();
        push_encodable(&mut expected_end_stack, &false);

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

    #[proptest]
    fn equal_elements_are_not_lt_prop(value: i128) {
        let init_stack = Lt.set_up_test_stack((value, value));
        let mut expected_end_stack = Lt.init_stack_for_isolated_run();
        push_encodable(&mut expected_end_stack, &false);

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
}

#[cfg(test)]
mod benches {
    use super::*;
    use crate::test_prelude::*;

    #[test]
    fn benchmark() {
        ShadowedClosure::new(Lt).bench();
    }
}
