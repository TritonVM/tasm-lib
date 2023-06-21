use rand::RngCore;
use twenty_first::amount::u32s::U32s;
use twenty_first::shared_math::b_field_element::BFieldElement;

use crate::snippet::{DataType, Snippet};
use crate::snippet_state::SnippetState;
use crate::{get_init_tvm_stack, push_encodable, ExecutionState};

#[derive(Clone, Debug)]
pub struct WrappingMulU64;

impl Snippet for WrappingMulU64 {
    fn entrypoint(&self) -> String {
        "tasm_arithmetic_u64_wrapping_mul".to_string()
    }

    fn inputs(&self) -> Vec<String> {
        vec![
            "lhs_hi".to_string(),
            "lhs_lo".to_string(),
            "rhs_hi".to_string(),
            "rhs_lo".to_string(),
        ]
    }

    fn input_types(&self) -> Vec<DataType> {
        vec![DataType::U64, DataType::U64]
    }

    fn output_types(&self) -> Vec<DataType> {
        vec![DataType::U64]
    }

    fn outputs(&self) -> Vec<String> {
        vec!["prod_hi".to_string(), "prod_lo".to_string()]
    }

    fn stack_diff(&self) -> isize {
        -2
    }

    fn function_code(&self, _library: &mut SnippetState) -> String {
        let entrypoint = self.entrypoint();

        format!(
            "
                // BEFORE: _ rhs_hi rhs_lo lhs_hi lhs_lo
                // AFTER: _ prod_hi prod_lo
                {entrypoint}:
                    // `lhs_lo * rhs_lo`:
                    dup 0 dup 3 mul
                    // _ rhs_hi rhs_lo lhs_hi lhs_lo (lhs_lo * rhs_lo)

                    // `rhs_hi * lhs_lo` (consume `rhs_hi` and `lhs_lo`):
                    swap 4
                    mul
                    // _ (lhs_lo * rhs_lo) rhs_lo lhs_hi (lhs_lo * rhs_hi)

                    // `rhs_lo * lhs_hi` (consume `rhs_lo` and `lhs_hi`):
                    swap 2
                    mul
                    // _ (lhs_lo * rhs_lo) (lhs_lo * rhs_hi) (lhs_hi * rhs_lo)

                    // rename to: a, b, c:
                    // _ a b c

                    // Calculate `prod_hi = a_hi + b_lo + c_lo`:
                    split
                    swap 1
                    pop
                    // _ a b c_lo

                    swap 1
                    split
                    swap 1
                    pop
                    // _ a c_lo b_lo

                    swap 2
                    split
                    // _ b_lo c_lo a_hi a_lo

                    swap 3
                    // _ a_lo c_lo a_hi b_lo

                    add
                    add
                    // _ a_lo (c_lo + a_hi + b_lo)

                    split
                    swap 1
                    pop
                    // _ a_lo (c_lo + a_hi + b_lo)_lo

                    swap 1
                    // _ (c_lo + a_hi + b_lo)_lo a_lo

                    // _ prod_hi prod_lo

                    return
                    "
        )
    }

    fn crash_conditions(&self) -> Vec<String> {
        todo!()
    }

    fn gen_input_states(&self) -> Vec<ExecutionState> {
        let mut rng = rand::thread_rng();

        let mut ret = vec![];
        for _ in 0..10 {
            ret.push(prepare_state(rng.next_u64(), rng.next_u64()));
        }

        ret
    }

    fn common_case_input_state(&self) -> ExecutionState {
        prepare_state(1 << 60, (1 << 42) - 1)
    }

    fn worst_case_input_state(&self) -> ExecutionState {
        prepare_state(1 << 60, (1 << 42) - 1)
    }

    fn rust_shadowing(
        &self,
        stack: &mut Vec<BFieldElement>,
        _std_in: Vec<BFieldElement>,
        _secret_in: Vec<BFieldElement>,
        _memory: &mut std::collections::HashMap<BFieldElement, BFieldElement>,
    ) {
        // top element on stack
        let a_lo: u32 = stack.pop().unwrap().try_into().unwrap();
        let a_hi: u32 = stack.pop().unwrap().try_into().unwrap();
        let a = ((a_hi as u64) << 32) + a_lo as u64;

        let b_lo: u32 = stack.pop().unwrap().try_into().unwrap();
        let b_hi: u32 = stack.pop().unwrap().try_into().unwrap();
        let b = ((b_hi as u64) << 32) + b_lo as u64;

        let prod = a.wrapping_mul(b);

        stack.push(BFieldElement::new(prod >> 32));
        stack.push(BFieldElement::new(prod & u32::MAX as u64));
    }
}

fn prepare_state(a: u64, b: u64) -> ExecutionState {
    let a = U32s::<2>::try_from(a).unwrap();
    let b = U32s::<2>::try_from(b).unwrap();
    let mut init_stack = get_init_tvm_stack();
    push_encodable(&mut init_stack, &a);
    push_encodable(&mut init_stack, &b);
    ExecutionState::with_stack(init_stack)
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use num::Zero;

    use crate::test_helpers::{
        test_rust_equivalence_given_input_values, test_rust_equivalence_multiple,
    };

    use super::*;

    #[test]
    fn wrapping_mul_u64_test() {
        test_rust_equivalence_multiple(&WrappingMulU64, true);
    }

    #[test]
    fn wrapping_mul_u64_simple() {
        let mut init_stack = get_init_tvm_stack();
        init_stack.push(BFieldElement::zero());
        init_stack.push(BFieldElement::new(100));
        init_stack.push(BFieldElement::zero());
        init_stack.push(BFieldElement::new(200));

        let mut expected = get_init_tvm_stack();
        expected.push(BFieldElement::zero());
        expected.push(BFieldElement::new(20_000));
        test_rust_equivalence_given_input_values(
            &WrappingMulU64,
            &init_stack,
            &[],
            &[],
            &mut HashMap::default(),
            1,
            Some(&expected),
        );
    }
}

#[cfg(test)]
mod benches {
    use super::*;
    use crate::snippet_bencher::bench_and_write;

    #[test]
    fn wrappingmul_u64_benchmark() {
        bench_and_write(WrappingMulU64);
    }
}
