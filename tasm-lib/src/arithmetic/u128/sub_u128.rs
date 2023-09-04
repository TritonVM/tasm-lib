use std::collections::HashMap;

use rand::Rng;
use twenty_first::{
    amount::u32s::U32s,
    shared_math::{b_field_element::BFieldElement, bfield_codec::BFieldCodec},
};

use crate::{
    get_init_tvm_stack,
    library::Library,
    snippet::{DataType, DeprecatedSnippet},
    ExecutionState,
};

#[derive(Clone, Debug)]
pub struct SubU128;

impl DeprecatedSnippet for SubU128 {
    fn input_field_names(&self) -> Vec<String> {
        vec![
            "rhs_3".to_string(),
            "rhs_2".to_string(),
            "rhs_1".to_string(),
            "rhs_0".to_string(),
            "lhs_3".to_string(),
            "lhs_2".to_string(),
            "lhs_1".to_string(),
            "lhs_0".to_string(),
        ]
    }

    fn output_field_names(&self) -> Vec<String> {
        vec![
            "(lhs - rhs)_3".to_string(),
            "(lhs - rhs)_2".to_string(),
            "(lhs - rhs)_1".to_string(),
            "(lhs - rhs)_0".to_string(),
        ]
    }

    fn input_types(&self) -> Vec<crate::snippet::DataType> {
        vec![DataType::U128, DataType::U128]
    }

    fn output_types(&self) -> Vec<crate::snippet::DataType> {
        vec![DataType::U128]
    }

    fn crash_conditions(&self) -> Vec<String> {
        vec!["(lhs - rhs) overflows u128".to_string()]
    }

    fn entrypoint_name(&self) -> String {
        "tasm_arithmetic_u128_sub".to_string()
    }

    fn stack_diff(&self) -> isize {
        -4
    }

    fn function_code(&self, _library: &mut Library) -> String {
        let entrypoint = self.entrypoint_name();
        const TWO_POW_32: &str = "4294967296";

        format!(
            "
            // BEFORE: _ rhs_3 rhs_2 rhs_1 rhs_0 lhs_3 lhs_2 lhs_1 lhs_0
            // AFTER: _ sum_3 sum_2 sum_1 sum_0
            {entrypoint}:
                swap 1 swap 4
                push -1
                mul
                add
                // _ rhs_3 rhs_2 rhs_1 lhs_1 lhs_3 lhs_2 (lhs_0 - rhs_0)

                push {TWO_POW_32}
                add

                split
                // _ rhs_3 rhs_2 rhs_1 lhs_1 lhs_3 lhs_2 !carry diff_0

                swap 5 swap 1
                // _ rhs_3 rhs_2 diff_0 lhs_1 lhs_3 lhs_2 rhs_1 !carry

                push 0
                eq
                // _ rhs_3 rhs_2 diff_0 lhs_1 lhs_3 lhs_2 rhs_1 carry

                add
                // _ rhs_3 rhs_2 diff_0 lhs_1 lhs_3 lhs_2 rhs_1'

                push -1
                mul
                // _ rhs_3 rhs_2 diff_0 lhs_1 lhs_3 lhs_2 -rhs_1'

                swap 1 swap 3
                // _ rhs_3 rhs_2 diff_0 lhs_2 lhs_3 -rhs_1' lhs_1

                add
                // _ rhs_3 rhs_2 diff_0 lhs_2 lhs_3 (lhs_1 - rhs_1')

                push {TWO_POW_32}
                add
                split
                // _ rhs_3 rhs_2 diff_0 lhs_2 lhs_3 !carry diff_1

                swap 5 swap 1
                // _ rhs_3 diff_1 diff_0 lhs_2 lhs_3 rhs_2 !carry

                push 0
                eq
                // _ rhs_3 diff_1 diff_0 lhs_2 lhs_3 rhs_2 carry

                add
                // _ rhs_3 diff_1 diff_0 lhs_2 lhs_3 rhs_2'

                push -1
                mul
                // _ rhs_3 diff_1 diff_0 lhs_2 lhs_3 -rhs_2'

                swap 1 swap 2
                // _ rhs_3 diff_1 diff_0 lhs_3 -rhs_2' lhs_2

                add
                // _ rhs_3 diff_1 diff_0 lhs_3 (lhs_2 - rhs_2')

                push {TWO_POW_32}
                add
                split
                // _ rhs_3 diff_1 diff_0 lhs_3 !carry diff_2

                swap 5 swap 1
                // _ diff_2 diff_1 diff_0 lhs_3 rhs_3 !carry

                push 0
                eq
                // _ diff_2 diff_1 diff_0 lhs_3 rhs_3 carry

                add
                // _ diff_2 diff_1 diff_0 lhs_3 rhs_3'

                push -1
                mul
                add
                // _ diff_2 diff_1 diff_0 (lhs_3 -rhs_3')

                split
                // _ diff_2 diff_1 diff_0 overflow diff_3

                // Assert that there is no overflow
                swap 1
                push 0
                eq
                assert
                // _ diff_2 diff_1 diff_0 diff_3

                swap 3 swap 2 swap 1
                // _ diff_3 diff_2 diff_1 diff_0

                return
            "
        )
    }

    fn gen_input_states(&self) -> Vec<ExecutionState> {
        let mut rng = rand::thread_rng();

        let mut ret = vec![];
        for _ in 0..30 {
            {
                let a: u128 = rng.gen();
                let b: u128 = rng.gen_range(0..=a);
                ret.push(prepare_state(a, b));
            }

            {
                let a: u128 = rng.gen();
                let b: u128 = rng.gen_range(0..=a) >> 32;
                ret.push(prepare_state(a, b));
            }

            {
                let a: u128 = rng.gen();
                let b: u128 = rng.gen_range(0..=a) >> 64;
                ret.push(prepare_state(a, b));
            }

            {
                let a: u128 = rng.gen();
                let b: u128 = rng.gen_range(0..=a) >> 96;
                ret.push(prepare_state(a, b));
            }
        }

        ret
    }

    fn common_case_input_state(&self) -> ExecutionState {
        prepare_state(1u128 << 127, 1u128 << 126)
    }

    fn worst_case_input_state(&self) -> ExecutionState {
        prepare_state(
            (1u128 << 127) + (1u128 << 64),
            (1u128 << 126) + (1u128 << 56),
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
        let a0: u32 = stack.pop().unwrap().try_into().unwrap();
        let b0: u32 = stack.pop().unwrap().try_into().unwrap();
        let c0: u32 = stack.pop().unwrap().try_into().unwrap();
        let d0: u32 = stack.pop().unwrap().try_into().unwrap();
        let ab0 = U32s::<4>::new([a0, b0, c0, d0]);

        // second element on stack
        let a1: u32 = stack.pop().unwrap().try_into().unwrap();
        let b1: u32 = stack.pop().unwrap().try_into().unwrap();
        let c1: u32 = stack.pop().unwrap().try_into().unwrap();
        let d1: u32 = stack.pop().unwrap().try_into().unwrap();
        let ab1 = U32s::<4>::new([a1, b1, c1, d1]);
        let ab0_minus_ab1 = ab0 - ab1;
        let mut res = ab0_minus_ab1.encode();
        for _ in 0..res.len() {
            stack.push(res.pop().unwrap());
        }
    }
}

fn prepare_state(lhs: u128, rhs: u128) -> ExecutionState {
    let mut init_stack = get_init_tvm_stack();
    for elem in rhs.encode().into_iter().rev() {
        init_stack.push(elem);
    }
    for elem in lhs.encode().into_iter().rev() {
        init_stack.push(elem);
    }

    ExecutionState::with_stack(init_stack)
}

#[cfg(test)]
mod tests {
    use num::{One, Zero};
    use twenty_first::shared_math::b_field_element::BFieldElement;

    use crate::get_init_tvm_stack;

    use crate::test_helpers::{
        test_rust_equivalence_given_input_values_deprecated,
        test_rust_equivalence_multiple_deprecated,
    };

    use super::*;

    #[test]
    fn sub_u128_test() {
        test_rust_equivalence_multiple_deprecated(&SubU128, true);
    }

    #[test]
    fn subtraction_involving_zeros() {
        // 0 - 0 = 0
        let mut expected_end_stack = [
            get_init_tvm_stack(),
            vec![
                BFieldElement::zero(),
                BFieldElement::zero(),
                BFieldElement::zero(),
                BFieldElement::zero(),
            ],
        ]
        .concat();
        prop_sub(U32s::from(0), U32s::from(0), Some(&expected_end_stack));

        // 1 - 0 = 1
        expected_end_stack = [
            get_init_tvm_stack(),
            vec![
                BFieldElement::zero(),
                BFieldElement::zero(),
                BFieldElement::zero(),
                BFieldElement::one(),
            ],
        ]
        .concat();
        prop_sub(U32s::from(1), U32s::from(0), Some(&expected_end_stack));

        // 1 - 1 = 0
        expected_end_stack = [
            get_init_tvm_stack(),
            vec![
                BFieldElement::zero(),
                BFieldElement::zero(),
                BFieldElement::zero(),
                BFieldElement::zero(),
            ],
        ]
        .concat();
        prop_sub(U32s::from(1), U32s::from(1), Some(&expected_end_stack));

        // u64::MAX - u64::MAX = 0
        expected_end_stack = [
            get_init_tvm_stack(),
            vec![
                BFieldElement::zero(),
                BFieldElement::zero(),
                BFieldElement::zero(),
                BFieldElement::zero(),
            ],
        ]
        .concat();
        prop_sub(
            U32s::try_from(u64::MAX).unwrap(),
            U32s::try_from(u64::MAX).unwrap(),
            Some(&expected_end_stack),
        );

        // u128::MAX - u128::MAX = 0
        expected_end_stack = [
            get_init_tvm_stack(),
            vec![
                BFieldElement::zero(),
                BFieldElement::zero(),
                BFieldElement::zero(),
                BFieldElement::zero(),
            ],
        ]
        .concat();
        prop_sub(
            U32s::new([u32::MAX, u32::MAX, u32::MAX, u32::MAX]),
            U32s::new([u32::MAX, u32::MAX, u32::MAX, u32::MAX]),
            Some(&expected_end_stack),
        );
    }

    #[test]
    fn sub_u128_cascading_carry() {
        let expected_end_stack = [
            get_init_tvm_stack(),
            vec![
                BFieldElement::zero(),
                BFieldElement::zero(),
                BFieldElement::zero(),
                BFieldElement::one(),
            ],
        ]
        .concat();
        prop_sub(
            U32s::new([0, 0, 0, 1]),
            U32s::new([u32::MAX, u32::MAX, u32::MAX, 0]),
            Some(&expected_end_stack),
        );
    }

    fn prop_sub(lhs: U32s<4>, rhs: U32s<4>, expected: Option<&[BFieldElement]>) {
        let mut init_stack = get_init_tvm_stack();
        for elem in rhs.encode().into_iter().rev() {
            init_stack.push(elem);
        }
        for elem in lhs.encode().into_iter().rev() {
            init_stack.push(elem);
        }

        test_rust_equivalence_given_input_values_deprecated(
            &SubU128,
            &init_stack,
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
    fn sub_u128_benchmark() {
        bench_and_write(SubU128);
    }
}
