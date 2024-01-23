use std::collections::HashMap;

use num::Zero;
use rand::RngCore;
use triton_vm::prelude::*;
use twenty_first::amount::u32s::U32s;

use crate::data_type::DataType;
use crate::empty_stack;
use crate::library::Library;
use crate::push_encodable;
use crate::traits::deprecated_snippet::DeprecatedSnippet;
use crate::ExecutionState;

#[derive(Clone, Debug)]
pub struct XorU64;

impl DeprecatedSnippet for XorU64 {
    fn entrypoint_name(&self) -> String {
        "tasm_arithmetic_u64_xor".to_string()
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
        vec!["(lhs ^ rhs)_hi".to_string(), "(lhs ^ rhs)_lo".to_string()]
    }

    fn output_types(&self) -> Vec<DataType> {
        vec![DataType::U64]
    }

    fn stack_diff(&self) -> isize {
        -2
    }

    fn function_code(&self, _library: &mut Library) -> String {
        let entrypoint = self.entrypoint_name();
        format!(
            "
            // BEFORE: rhs_hi rhs_lo lhs_hi lhs_lo
            // AFTER: (rhs ^ lhs)_hi (rhs ^ lhs)_lo
            {entrypoint}:
                swap 3
                xor
                // stack: _ lhs_lo rhs_lo (lhs_hi ^ rhs_hi)

                swap 2
                xor
                // stack: _ (lhs_hi ^ rhs_hi) (rhs_lo ^ lhs_lo)

                return
            "
        )
    }

    fn crash_conditions(&self) -> Vec<String> {
        vec!["any input is not u32".to_string()]
    }

    fn gen_input_states(&self) -> Vec<ExecutionState> {
        let mut rng = rand::thread_rng();
        let lhs = U32s::<2>::try_from(rng.next_u64()).unwrap();
        let rhs = U32s::<2>::try_from(rng.next_u64()).unwrap();
        let mut stack = empty_stack();
        push_encodable(&mut stack, &lhs);
        push_encodable(&mut stack, &rhs);
        vec![ExecutionState::with_stack(stack)]
    }

    fn common_case_input_state(&self) -> ExecutionState {
        ExecutionState::with_stack(
            [
                empty_stack(),
                vec![BFieldElement::zero(), BFieldElement::new((1 << 31) - 1)],
                vec![BFieldElement::zero(), BFieldElement::new((1 << 10) - 1)],
            ]
            .concat(),
        )
    }

    fn worst_case_input_state(&self) -> ExecutionState {
        ExecutionState::with_stack(
            [
                empty_stack(),
                vec![BFieldElement::new(1 << 31), BFieldElement::new(1 << 31)],
                vec![
                    BFieldElement::new(1 << 30),
                    BFieldElement::new((1 << 31) + 10),
                ],
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
        let a_lo: u32 = stack.pop().unwrap().try_into().unwrap();
        let a_hi: u32 = stack.pop().unwrap().try_into().unwrap();

        // second element on stack
        let b_lo: u32 = stack.pop().unwrap().try_into().unwrap();
        let b_hi: u32 = stack.pop().unwrap().try_into().unwrap();

        // Perform calculation and write the result back to the stack
        let xor_res = U32s::<2>::new([a_lo ^ b_lo, a_hi ^ b_hi]);
        let mut res = xor_res.encode();
        for _ in 0..res.len() {
            stack.push(res.pop().unwrap());
        }
    }
}

#[cfg(test)]
mod tests {
    use num::BigUint;
    use rand::thread_rng;
    use rand::RngCore;

    use crate::empty_stack;
    use crate::test_helpers::test_rust_equivalence_given_input_values_deprecated;
    use crate::test_helpers::test_rust_equivalence_multiple_deprecated;

    use super::*;

    #[test]
    fn xor_u64_test() {
        test_rust_equivalence_multiple_deprecated(&XorU64, true);
    }

    #[test]
    fn xor_test_simple() {
        prop_xor(4, 3);
        prop_xor(4, 4);
        prop_xor(u64::MAX, u64::MAX);
        prop_xor(0, u64::MAX);
        prop_xor(u64::MAX, 0);
    }

    #[test]
    fn xor_test_pbt() {
        let mut rng = thread_rng();
        for _ in 0..100 {
            let lhs = rng.next_u64();
            let rhs = rng.next_u64();
            prop_xor(lhs, rhs);
        }
    }

    fn prop_xor(lhs: u64, rhs: u64) {
        let mut init_stack = empty_stack();

        let rhs_u32_2 = U32s::<2>::new([(rhs & u32::MAX as u64) as u32, (rhs >> 32) as u32]);
        for elem in rhs_u32_2.encode().into_iter().rev() {
            init_stack.push(elem);
        }

        let lhs_u32_2 = U32s::<2>::new([(lhs & u32::MAX as u64) as u32, (lhs >> 32) as u32]);
        for elem in lhs_u32_2.encode().into_iter().rev() {
            init_stack.push(elem);
        }

        let expected_res: BigUint = (lhs ^ rhs).into();
        let expected_u32_2: U32s<2> = expected_res.into();
        let mut expected_end_stack = empty_stack();
        for elem in expected_u32_2.encode().into_iter().rev() {
            expected_end_stack.push(elem);
        }

        test_rust_equivalence_given_input_values_deprecated(
            &XorU64,
            &init_stack,
            &[],
            HashMap::default(),
            0,
            Some(&expected_end_stack),
        );
    }
}

#[cfg(test)]
mod benches {
    use crate::snippet_bencher::bench_and_write;

    use super::*;

    #[test]
    fn xor_u64_benchmark() {
        bench_and_write(XorU64);
    }
}
