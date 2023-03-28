use std::collections::HashMap;

use rand::RngCore;
use twenty_first::amount::u32s::U32s;
use twenty_first::shared_math::b_field_element::BFieldElement;
use twenty_first::util_types::algebraic_hasher::Hashable;

use crate::library::Library;
use crate::snippet::{DataType, Snippet};
use crate::{get_init_tvm_stack, push_hashable, ExecutionState};

#[derive(Clone)]
pub struct DivModU64;

impl Snippet for DivModU64 {
    fn entrypoint(&self) -> String {
        "tasm_arithmetic_u64_div_mod".to_string()
    }

    fn inputs(&self) -> Vec<String>
    where
        Self: Sized,
    {
        vec![
            "rhs_hi".to_string(),
            "rhs_lo".to_string(),
            "lhs_hi".to_string(),
            "lhs_lo".to_string(),
        ]
    }

    fn input_types(&self) -> Vec<crate::snippet::DataType> {
        vec![DataType::U64, DataType::U64]
    }

    fn output_types(&self) -> Vec<crate::snippet::DataType> {
        vec![DataType::U64]
    }

    fn outputs(&self) -> Vec<String>
    where
        Self: Sized,
    {
        vec!["(lhs / rhs)_hi".to_string(), "(lhs / rhs)_lo".to_string()]
    }

    fn stack_diff(&self) -> isize
    where
        Self: Sized,
    {
        -2
    }

    fn function_body(&self, library: &mut Library) -> String {
        let entrypoint = self.entrypoint();

        format!(
            "
            // BEFORE: _ numerator_hi numerator_lo divisor_hi divisor_lo
            // AFTER: _ quotient_hi quotient_lo remainder_hi remainder_lo
            {entrypoint}:

                // divisor > numerator => return (0, numerator)
                


            "
        )
    }

    fn crash_conditions() -> Vec<String>
    where
        Self: Sized,
    {
        vec![]
    }

    fn gen_input_states(&self) -> Vec<ExecutionState>
    where
        Self: Sized,
    {
        let mut rng = rand::thread_rng();
        let lhs = U32s::<2>::try_from(rng.next_u64()).unwrap();
        let rhs = U32s::<2>::try_from(rng.next_u64()).unwrap();
        let mut stack = get_init_tvm_stack();
        push_hashable(&mut stack, &lhs);
        push_hashable(&mut stack, &rhs);
        vec![ExecutionState::with_stack(stack)]
    }

    fn common_case_input_state(&self) -> ExecutionState
    where
        Self: Sized,
    {
        todo!()
    }

    fn worst_case_input_state(&self) -> ExecutionState
    where
        Self: Sized,
    {
        todo!()
    }

    fn rust_shadowing(
        &self,
        stack: &mut Vec<BFieldElement>,
        std_in: Vec<BFieldElement>,
        secret_in: Vec<BFieldElement>,
        memory: &mut HashMap<BFieldElement, BFieldElement>,
    ) where
        Self: Sized,
    {
        // top element on stack
        let numerator_lo: u32 = stack.pop().unwrap().try_into().unwrap();
        let numerator_hi: u32 = stack.pop().unwrap().try_into().unwrap();
        let numerator: u64 = numerator_lo as u64 + (numerator_hi as u64) << 32;

        // second element on stack
        let denominator_lo: u32 = stack.pop().unwrap().try_into().unwrap();
        let denominator_hi: u32 = stack.pop().unwrap().try_into().unwrap();
        let denominator: u64 = denominator_lo as u64 + (denominator_hi as u64) << 32;
        let quotient = numerator / denominator;
        let remainder = numerator % denominator;

        let quotient = U32s::<2>::try_from(quotient).unwrap();
        let remainder = U32s::<2>::try_from(remainder).unwrap();
        let mut quotient = quotient.to_sequence();
        for _ in 0..quotient.len() {
            stack.push(quotient.pop().unwrap());
        }
        let mut remainder = remainder.to_sequence();
        for _ in 0..remainder.len() {
            stack.push(remainder.pop().unwrap());
        }
    }
}
