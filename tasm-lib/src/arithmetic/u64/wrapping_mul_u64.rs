use rand::RngCore;
use triton_vm::prelude::*;
use triton_vm::twenty_first::prelude::U32s;

use crate::data_type::DataType;
use crate::empty_stack;
use crate::library::Library;
use crate::push_encodable;
use crate::traits::deprecated_snippet::DeprecatedSnippet;
use crate::ExecutionState;

#[derive(Clone, Debug)]
pub struct WrappingMulU64;

impl DeprecatedSnippet for WrappingMulU64 {
    fn entrypoint_name(&self) -> String {
        "tasm_arithmetic_u64_wrapping_mul".to_string()
    }

    fn input_field_names(&self) -> Vec<String> {
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

    fn output_field_names(&self) -> Vec<String> {
        vec!["prod_hi".to_string(), "prod_lo".to_string()]
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
                // BEFORE: _ rhs_hi rhs_lo lhs_hi lhs_lo
                // AFTER:  _ prod_hi prod_lo
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
                    pop 1
                    // _ a b c_lo

                    swap 1
                    split
                    swap 1
                    pop 1
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
                    pop 1
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
    let mut init_stack = empty_stack();
    push_encodable(&mut init_stack, &a);
    push_encodable(&mut init_stack, &b);
    ExecutionState::with_stack(init_stack)
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use num::Zero;

    use crate::test_helpers::test_rust_equivalence_given_input_values_deprecated;
    use crate::test_helpers::test_rust_equivalence_multiple_deprecated;

    use super::*;

    #[test]
    fn wrapping_mul_u64_test() {
        test_rust_equivalence_multiple_deprecated(&WrappingMulU64, true);
    }

    #[test]
    fn wrapping_mul_u64_simple() {
        let mut init_stack = empty_stack();
        init_stack.push(BFieldElement::zero());
        init_stack.push(BFieldElement::new(100));
        init_stack.push(BFieldElement::zero());
        init_stack.push(BFieldElement::new(200));

        let mut expected = empty_stack();
        expected.push(BFieldElement::zero());
        expected.push(BFieldElement::new(20_000));
        test_rust_equivalence_given_input_values_deprecated(
            &WrappingMulU64,
            &init_stack,
            &[],
            HashMap::default(),
            Some(&expected),
        );
    }
}

#[cfg(test)]
mod benches {
    use crate::snippet_bencher::bench_and_write;

    use super::*;

    #[test]
    fn wrappingmul_u64_benchmark() {
        bench_and_write(WrappingMulU64);
    }
}
