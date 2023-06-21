use std::collections::HashMap;

use num::{One, Zero};
use rand::RngCore;
use twenty_first::amount::u32s::U32s;
use twenty_first::shared_math::b_field_element::BFieldElement;

use crate::snippet::{DataType, Snippet};
use crate::snippet_state::SnippetState;
use crate::{get_init_tvm_stack, push_encodable, ExecutionState};

#[derive(Clone, Debug)]
pub struct EqU64;

impl Snippet for EqU64 {
    fn inputs(&self) -> Vec<String> {
        vec![
            "rhs_hi".to_string(),
            "rhs_lo".to_string(),
            "lhs_hi".to_string(),
            "lhs_lo".to_string(),
        ]
    }

    fn outputs(&self) -> Vec<String> {
        vec!["rhs_hi == lhs_hi && rhs_lo == rhs_lo".to_string()]
    }

    fn input_types(&self) -> Vec<crate::snippet::DataType> {
        vec![DataType::U64, DataType::U64]
    }

    fn output_types(&self) -> Vec<crate::snippet::DataType> {
        vec![DataType::Bool]
    }

    fn crash_conditions(&self) -> Vec<String> {
        vec![]
    }

    fn gen_input_states(&self) -> Vec<ExecutionState> {
        let mut rng = rand::thread_rng();
        let rhs = U32s::<2>::try_from(rng.next_u64()).unwrap();
        let lhs = U32s::<2>::try_from(rng.next_u64()).unwrap();

        let mut stack = get_init_tvm_stack();
        push_encodable(&mut stack, &rhs);
        push_encodable(&mut stack, &lhs);

        vec![ExecutionState::with_stack(stack)]
    }

    fn stack_diff(&self) -> isize {
        -3
    }

    fn entrypoint(&self) -> String {
        "tasm_arithmetic_u64_eq".to_string()
    }

    fn function_code(&self, _library: &mut SnippetState) -> String {
        let entrypoint = self.entrypoint();
        format!(
            "
            // Before: _ hi_r lo_r hi_l lo_l
            // After: _ (r == l)
            {entrypoint}:
                swap 3
                eq
                swap 2
                eq
                add

                // The sum of the two equality operations must be 2
                push 2
                eq
                return
            "
        )
    }

    fn rust_shadowing(
        &self,
        stack: &mut Vec<BFieldElement>,
        _std_in: Vec<BFieldElement>,
        _secret_in: Vec<BFieldElement>,
        _memory: &mut HashMap<BFieldElement, BFieldElement>,
    ) {
        let a_lo: u32 = stack.pop().unwrap().try_into().unwrap();
        let a_hi: u32 = stack.pop().unwrap().try_into().unwrap();
        let a = U32s::<2>::new([a_lo, a_hi]);

        let b_lo: u32 = stack.pop().unwrap().try_into().unwrap();
        let b_hi: u32 = stack.pop().unwrap().try_into().unwrap();
        let b = U32s::<2>::new([b_lo, b_hi]);

        stack.push(if a == b {
            BFieldElement::one()
        } else {
            BFieldElement::zero()
        })
    }

    fn common_case_input_state(&self) -> ExecutionState {
        ExecutionState::with_stack(
            vec![
                get_init_tvm_stack(),
                vec![BFieldElement::zero(), BFieldElement::new((1 << 31) - 1)],
                vec![BFieldElement::zero(), BFieldElement::new((1 << 10) - 1)],
            ]
            .concat(),
        )
    }

    fn worst_case_input_state(&self) -> ExecutionState {
        ExecutionState::with_stack(
            vec![
                get_init_tvm_stack(),
                vec![BFieldElement::new(1 << 31), BFieldElement::new(1 << 31)],
                vec![
                    BFieldElement::new(1 << 30),
                    BFieldElement::new((1 << 31) + 10),
                ],
            ]
            .concat(),
        )
    }
}

#[cfg(test)]
mod tests {
    use rand::RngCore;
    use twenty_first::shared_math::b_field_element::BFieldElement;
    use twenty_first::shared_math::bfield_codec::BFieldCodec;

    use crate::get_init_tvm_stack;

    use crate::test_helpers::{
        test_rust_equivalence_given_input_values, test_rust_equivalence_multiple,
    };

    use super::*;

    #[test]
    fn eq_u64_test() {
        test_rust_equivalence_multiple(&EqU64, true);
    }

    #[test]
    fn u32s_2_eq_false() {
        // Should return false
        let expected_end_stack = vec![get_init_tvm_stack(), vec![BFieldElement::zero()]].concat();
        prop_eq(
            U32s::new([239, 16]),
            U32s::new([239, 17]),
            Some(&expected_end_stack),
        );
        prop_eq(
            U32s::new([239, 16]),
            U32s::new([128, 16]),
            Some(&expected_end_stack),
        );
        prop_eq(
            U32s::new([239, 16]),
            U32s::new([128, 17]),
            Some(&expected_end_stack),
        );
        prop_eq(
            U32s::new([0, 16]),
            U32s::new([0, 17]),
            Some(&expected_end_stack),
        );
        prop_eq(
            U32s::new([239, 0]),
            U32s::new([128, 0]),
            Some(&expected_end_stack),
        );
        prop_eq(
            U32s::new([0, 1]),
            U32s::new([0, 0]),
            Some(&expected_end_stack),
        );
        prop_eq(
            U32s::new([0, 0]),
            U32s::new([0, 1]),
            Some(&expected_end_stack),
        );
        prop_eq(
            U32s::new([0, 0]),
            U32s::new([0, 1]),
            Some(&expected_end_stack),
        );
        prop_eq(
            U32s::new([1, 0]),
            U32s::new([0, 0]),
            Some(&expected_end_stack),
        );
        prop_eq(
            U32s::new([0, 0]),
            U32s::new([1, 0]),
            Some(&expected_end_stack),
        );
    }

    #[test]
    fn u32s_2_eq_true() {
        // Should return true
        let expected_end_stack = vec![get_init_tvm_stack(), vec![BFieldElement::one()]].concat();
        prop_eq(
            U32s::new([239, 17]),
            U32s::new([239, 17]),
            Some(&expected_end_stack),
        );
        prop_eq(
            U32s::new([u32::MAX, 16]),
            U32s::new([u32::MAX, 16]),
            Some(&expected_end_stack),
        );
        prop_eq(
            U32s::new([239, u32::MAX]),
            U32s::new([239, u32::MAX]),
            Some(&expected_end_stack),
        );
        prop_eq(
            U32s::new([u32::MAX, u32::MAX]),
            U32s::new([u32::MAX, u32::MAX]),
            Some(&expected_end_stack),
        );
        prop_eq(
            U32s::new([0, 0]),
            U32s::new([0, 0]),
            Some(&expected_end_stack),
        );
        prop_eq(
            U32s::new([1, 0]),
            U32s::new([1, 0]),
            Some(&expected_end_stack),
        );
        prop_eq(
            U32s::new([0, 1]),
            U32s::new([0, 1]),
            Some(&expected_end_stack),
        );
        prop_eq(
            U32s::new([1, 1]),
            U32s::new([1, 1]),
            Some(&expected_end_stack),
        );
    }

    #[test]
    fn u32s_2_eq_pbt_true() {
        let expected_end_stack = vec![get_init_tvm_stack(), vec![BFieldElement::one()]].concat();
        let mut rng = rand::thread_rng();
        for _ in 0..10 {
            let lhs = U32s::new([rng.next_u32(), rng.next_u32()]);
            let rhs = lhs;
            prop_eq(lhs, rhs, Some(&expected_end_stack));
        }
    }

    fn prop_eq(lhs: U32s<2>, rhs: U32s<2>, expected: Option<&[BFieldElement]>) {
        let mut init_stack = get_init_tvm_stack();
        for elem in rhs.encode().into_iter().rev() {
            init_stack.push(elem);
        }
        for elem in lhs.encode().into_iter().rev() {
            init_stack.push(elem);
        }

        test_rust_equivalence_given_input_values(
            &EqU64,
            &init_stack,
            &[],
            &[],
            &mut HashMap::default(),
            0,
            expected,
        );
    }
}

#[cfg(test)]
mod benches {
    use super::*;
    use crate::snippet_bencher::bench_and_write;

    #[test]
    fn eq_u64_benchmark() {
        bench_and_write(EqU64);
    }
}
