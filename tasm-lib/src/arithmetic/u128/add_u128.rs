use std::collections::HashMap;

use num::Zero;
use rand::Rng;
use triton_vm::prelude::*;
use triton_vm::twenty_first::prelude::U32s;

use crate::data_type::DataType;
use crate::empty_stack;
use crate::library::Library;
use crate::push_encodable;
use crate::traits::deprecated_snippet::DeprecatedSnippet;
use crate::ExecutionState;

#[derive(Clone, Debug)]
pub struct AddU128;

impl DeprecatedSnippet for AddU128 {
    fn entrypoint_name(&self) -> String {
        "tasm_arithmetic_u128_add".to_string()
    }

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

    fn input_types(&self) -> Vec<DataType> {
        vec![DataType::U128, DataType::U128]
    }

    fn output_field_names(&self) -> Vec<String> {
        vec![
            "(lhs + rhs)_3".to_string(),
            "(lhs + rhs)_2".to_string(),
            "(lhs + rhs)_1".to_string(),
            "(lhs + rhs)_0".to_string(),
        ]
    }

    fn output_types(&self) -> Vec<DataType> {
        vec![DataType::U128]
    }

    fn stack_diff(&self) -> isize {
        -4
    }

    /// Four top elements of stack are assumed to be valid u32s. So to have
    /// a value that's less than 2^32.
    fn function_code(&self, _library: &mut Library) -> String {
        let entrypoint = self.entrypoint_name();
        format!(
            "
            // BEFORE: _ rhs_3 rhs_2 rhs_1 rhs_0 lhs_3 lhs_2 lhs_1 lhs_0
            // AFTER:  _ sum_3 sum_2 sum_1 sum_0
            {entrypoint}:
                swap 1 swap 4
                add
                // _ rhs_3 rhs_2 rhs_1 lhs_1 lhs_3 lhs_2 (lhs_0 + rhs_0)

                split
                // _ rhs_3 rhs_2 rhs_1 lhs_1 lhs_3 lhs_2 (lhs_0 + rhs_0)_hi (lhs_0 + rhs_0)_lo

                // rename:
                // _ rhs_3 rhs_2 rhs_1 lhs_1 lhs_3 lhs_2 carry_1 sum_0

                swap 4
                // _ rhs_3 rhs_2 rhs_1 sum_0 lhs_3 lhs_2 carry_1 lhs_1

                add
                // _ rhs_3 rhs_2 rhs_1 sum_0 lhs_3 lhs_2 lhs_1'

                swap 1 swap 4
                // _ rhs_3 rhs_2 lhs_2 sum_0 lhs_3 lhs_1' rhs_1

                add
                // _ rhs_3 rhs_2 lhs_2 sum_0 lhs_3 (lhs_1' + rhs_1)

                split
                // _ rhs_3 rhs_2 lhs_2 sum_0 lhs_3 carry_2 sum_1

                swap 4
                // _ rhs_3 rhs_2 sum_1 sum_0 lhs_3 carry_2 lhs_2

                add
                // _ rhs_3 rhs_2 sum_1 sum_0 lhs_3 lhs_2'

                swap 1 swap 4
                // _ rhs_3 lhs_3 sum_1 sum_0 lhs_2' rhs_2

                add
                // _ rhs_3 lhs_3 sum_1 sum_0 (lhs_2' + rhs_2)

                split
                // _ rhs_3 lhs_3 sum_1 sum_0 carry_3 sum_2

                swap 4
                // _ rhs_3 sum_2 sum_1 sum_0 carry_3 lhs_3

                add
                // _ rhs_3 sum_2 sum_1 sum_0 lhs_3'

                dup 4
                // _ rhs_3 sum_2 sum_1 sum_0 lhs_3' rhs_3

                add
                // _ rhs_3 sum_2 sum_1 sum_0 (lhs_3' + rhs_3)

                split
                // _ rhs_3 sum_2 sum_1 sum_0 overflow sum_3

                swap 5
                pop 1
                // _ sum_3 sum_2 sum_1 sum_0 overflow

                push 0
                eq
                assert
                // _ sum_3 sum_2 sum_1 sum_0

                return
            "
        )
    }

    fn crash_conditions(&self) -> Vec<String> {
        vec!["if (lhs + rhs) overflows u128".to_string()]
    }

    fn gen_input_states(&self) -> Vec<ExecutionState> {
        let mut rng = rand::thread_rng();

        let mut states = vec![];
        let zero = U32s::<4>::zero();

        for _ in 0..20 {
            let small_a = U32s::<4>::try_from(rng.gen::<u64>()).unwrap();
            let small_b = U32s::<4>::try_from(rng.gen::<u64>()).unwrap();
            let mut random_bytes: [u32; 4] = [0, 0, 0, 0];
            rng.fill(&mut random_bytes);
            let large_a = U32s::<4>::new(random_bytes);

            // 0. one zero, one large
            states.push({
                let mut stack = empty_stack();
                push_encodable(&mut stack, &zero);
                push_encodable(&mut stack, &large_a);
                ExecutionState::with_stack(stack)
            });

            // 1. two small
            states.push({
                let mut stack = empty_stack();
                push_encodable(&mut stack, &small_a);
                push_encodable(&mut stack, &small_b);
                ExecutionState::with_stack(stack)
            });
        }

        states
    }

    fn common_case_input_state(&self) -> ExecutionState {
        ExecutionState::with_stack(
            [
                empty_stack(),
                vec![BFieldElement::zero(), BFieldElement::new(1 << 31)],
                vec![BFieldElement::zero(), BFieldElement::new(1 << 30)],
                vec![BFieldElement::zero(), BFieldElement::new(1 << 30)],
                vec![BFieldElement::zero(), BFieldElement::new(1 << 30)],
            ]
            .concat(),
        )
    }

    fn worst_case_input_state(&self) -> ExecutionState {
        ExecutionState::with_stack(
            [
                empty_stack(),
                vec![BFieldElement::zero(), BFieldElement::new(1 << 31)],
                vec![BFieldElement::zero(), BFieldElement::new(1 << 30)],
                vec![BFieldElement::zero(), BFieldElement::new(1 << 30)],
                vec![BFieldElement::zero(), BFieldElement::new(1 << 30)],
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
        let ab0_plus_ab1 = ab0 + ab1;
        let mut res = ab0_plus_ab1.encode();
        for _ in 0..res.len() {
            stack.push(res.pop().unwrap());
        }
    }
}

#[cfg(test)]
mod tests {
    use triton_vm::prelude::BFieldCodec;

    use crate::test_helpers::test_rust_equivalence_given_input_values_deprecated;
    use crate::test_helpers::test_rust_equivalence_multiple_deprecated;

    use super::*;

    #[test]
    fn add_u128_test() {
        test_rust_equivalence_multiple_deprecated(&AddU128, true);
    }

    #[test]
    fn add_u128_unit_test() {
        let mut expected = empty_stack();
        expected.push(BFieldElement::new(0));
        expected.push(BFieldElement::new(1 << 4));
        expected.push(BFieldElement::new(0));
        expected.push(BFieldElement::new(0));
        prop_add(1u128 << 67, 1u128 << 67, Some(&expected))
    }

    fn prop_add(lhs: u128, rhs: u128, expected: Option<&[BFieldElement]>) {
        let mut init_stack = empty_stack();
        for elem in rhs.encode().into_iter().rev() {
            init_stack.push(elem);
        }
        for elem in lhs.encode().into_iter().rev() {
            init_stack.push(elem);
        }

        test_rust_equivalence_given_input_values_deprecated::<AddU128>(
            &AddU128,
            &init_stack,
            &[],
            HashMap::default(),
            expected,
        );
    }
}

#[cfg(test)]
mod benches {
    use super::*;
    use crate::snippet_bencher::bench_and_write;

    #[test]
    fn add_u128_benchmark() {
        bench_and_write(AddU128);
    }
}
