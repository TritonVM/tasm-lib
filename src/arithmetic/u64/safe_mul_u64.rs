use rand::RngCore;
use twenty_first::amount::u32s::U32s;
use twenty_first::shared_math::b_field_element::BFieldElement;

use crate::library::Library;
use crate::snippet::{DataType, Snippet};
use crate::{get_init_tvm_stack, push_hashable, ExecutionState};

#[derive(Clone)]
pub struct SafeMulU64;

impl Snippet for SafeMulU64 {
    fn entrypoint(&self) -> String {
        "tasm_arithmetic_u64_safe_mul".to_string()
    }

    fn inputs(&self) -> Vec<String>
    where
        Self: Sized,
    {
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

    fn outputs(&self) -> Vec<String>
    where
        Self: Sized,
    {
        vec!["prod_hi".to_string(), "prod_lo".to_string()]
    }

    fn stack_diff(&self) -> isize
    where
        Self: Sized,
    {
        -2
    }

    fn function_body(&self, _library: &mut Library) -> String {
        let entrypoint = self.entrypoint();

        format!(
            "
                // BEFORE: _ rhs_hi rhs_lo lhs_hi lhs_lo
                // AFTER: _ prod_hi prod_lo
                {entrypoint}:
                    // `lhs_lo * rhs_lo`:
                    dup0 dup3 mul
                    // _ rhs_hi rhs_lo lhs_hi lhs_lo (lhs_lo * rhs_lo)

                    // `rhs_hi * lhs_lo` (consume and `lhs_lo`):
                    swap4
                    swap1
                    dup1
                    mul
                    // _ (lhs_lo * rhs_lo) rhs_lo lhs_hi rhs_hi (lhs_lo * rhs_hi)

                    // `rhs_lo * lhs_hi` (consume `rhs_lo`):
                    swap3
                    dup2
                    mul
                    // _ (lhs_lo * rhs_lo) (lhs_lo * rhs_hi) lhs_hi rhs_hi (rhs_lo * lhs_hi)

                    // `lhs_hi * rhs_hi` (consume `lhs_hi` and `rhs_hi`):
                    swap2
                    mul
                    // _ (lhs_lo * rhs_lo) (lhs_lo * rhs_hi) (rhs_lo * lhs_hi) (lhs_hi * rhs_hi)

                    // crash if `lhs_hi * rhs_hi != 0`
                    push 0
                    eq
                    assert
                    // _ (lhs_lo * rhs_lo) (lhs_lo * rhs_hi) (rhs_lo * lhs_hi)

                    // rename to: a, b, c:
                    // _ a b c

                    // Calculate `prod_hi = a_hi + b_lo + c_lo`:
                    split
                    swap1
                    push 0
                    eq
                    assert
                    // _ a b c_lo

                    swap1
                    split
                    swap1
                    push 0
                    eq
                    assert
                    // _ a c_lo b_lo

                    swap2
                    split
                    // _ b_lo c_lo a_hi a_lo

                    swap3
                    // _ a_lo c_lo a_hi b_lo

                    add
                    add
                    // _ a_lo (c_lo + a_hi + b_lo)

                    split
                    swap1
                    push 0
                    eq
                    assert
                    // _ a_lo (c_lo + a_hi + b_lo)_lo

                    swap1
                    // _ (c_lo + a_hi + b_lo)_lo a_lo

                    // _ prod_hi prod_lo

                    return
                    "
        )
    }

    fn crash_conditions() -> Vec<String>
    where
        Self: Sized,
    {
        vec!["Product is greater than u64::MAX".to_string()]
    }

    fn gen_input_states(&self) -> Vec<ExecutionState>
    where
        Self: Sized,
    {
        let mut rng = rand::thread_rng();

        let mut ret = vec![];
        for _ in 0..10 {
            ret.push(prepare_state(rng.next_u32() as u64, rng.next_u32() as u64));
        }

        ret
    }

    fn common_case_input_state(&self) -> ExecutionState
    where
        Self: Sized,
    {
        prepare_state(1 << 31, (1 << 25) - 1)
    }

    fn worst_case_input_state(&self) -> ExecutionState
    where
        Self: Sized,
    {
        prepare_state(1 << 31, (1 << 31) - 1)
    }

    fn rust_shadowing(
        &self,
        stack: &mut Vec<BFieldElement>,
        _std_in: Vec<BFieldElement>,
        _secret_in: Vec<BFieldElement>,
        _memory: &mut std::collections::HashMap<BFieldElement, BFieldElement>,
    ) where
        Self: Sized,
    {
        // top element on stack
        let a_lo: u32 = stack.pop().unwrap().try_into().unwrap();
        let a_hi: u32 = stack.pop().unwrap().try_into().unwrap();
        let a = ((a_hi as u64) << 32) + a_lo as u64;

        let b_lo: u32 = stack.pop().unwrap().try_into().unwrap();
        let b_hi: u32 = stack.pop().unwrap().try_into().unwrap();
        let b = ((b_hi as u64) << 32) + b_lo as u64;

        // let prod = a.wrapping_mul(b);
        let (safe_mul_prod, overflow) = a.overflowing_mul(b);
        assert!(!overflow, "u64 mul result overflowed");

        stack.push(BFieldElement::new(safe_mul_prod >> 32));
        stack.push(BFieldElement::new(safe_mul_prod & u32::MAX as u64));
    }
}

fn prepare_state(a: u64, b: u64) -> ExecutionState {
    let a = U32s::<2>::try_from(a).unwrap();
    let b = U32s::<2>::try_from(b).unwrap();
    let mut init_stack = get_init_tvm_stack();
    push_hashable(&mut init_stack, &a);
    push_hashable(&mut init_stack, &b);
    ExecutionState::with_stack(init_stack)
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use num::Zero;
    use twenty_first::util_types::algebraic_hasher::Hashable;

    use crate::snippet_bencher::bench_and_write;
    use crate::test_helpers::{rust_tasm_equivalence_prop, rust_tasm_equivalence_prop_new};

    use super::*;

    #[test]
    fn safe_mul_u64_test() {
        rust_tasm_equivalence_prop_new(SafeMulU64);
    }

    #[test]
    fn safe_u64_benchmark() {
        bench_and_write(SafeMulU64);
    }

    #[should_panic]
    #[test]
    fn overflow_test_1() {
        // Crash because (rhs_hi * lhs_hi) != 0
        let lhs: U32s<2> = U32s::try_from(1u64 << 32).unwrap();
        let rhs: U32s<2> = U32s::try_from(1u64 << 32).unwrap();
        let mut init_stack = get_init_tvm_stack();
        for elem in rhs.to_sequence().into_iter().rev() {
            init_stack.push(elem);
        }
        for elem in lhs.to_sequence().into_iter().rev() {
            init_stack.push(elem);
        }

        SafeMulU64.run_tasm(&mut ExecutionState::with_stack(init_stack));
    }

    #[should_panic]
    #[test]
    fn overflow_test_2() {
        // Crash because (rhs_lo * lhs_hi)_hi != 0
        let lhs: U32s<2> = U32s::try_from(1u64 << 31).unwrap();
        let rhs: U32s<2> = U32s::try_from(1u64 << 33).unwrap();
        let mut init_stack = get_init_tvm_stack();
        for elem in rhs.to_sequence().into_iter().rev() {
            init_stack.push(elem);
        }
        for elem in lhs.to_sequence().into_iter().rev() {
            init_stack.push(elem);
        }

        SafeMulU64.run_tasm(&mut ExecutionState::with_stack(init_stack));
    }

    #[should_panic]
    #[test]
    fn overflow_test_3() {
        // Crash because (lhs_lo * rhs_hi)_hi != 0
        let lhs: U32s<2> = U32s::try_from(1u64 << 33).unwrap();
        let rhs: U32s<2> = U32s::try_from(1u64 << 31).unwrap();
        let mut init_stack = get_init_tvm_stack();
        for elem in rhs.to_sequence().into_iter().rev() {
            init_stack.push(elem);
        }
        for elem in lhs.to_sequence().into_iter().rev() {
            init_stack.push(elem);
        }

        SafeMulU64.run_tasm(&mut ExecutionState::with_stack(init_stack));
    }

    #[should_panic]
    #[test]
    fn overflow_test_4() {
        // Crash because (c_lo + a_hi + b_lo)_hi != 0
        let lhs: U32s<2> = U32s::try_from((1u64 << 33) + 5).unwrap();
        let rhs: U32s<2> = U32s::try_from((1u64 << 31) - 1).unwrap();
        let mut init_stack = get_init_tvm_stack();
        for elem in rhs.to_sequence().into_iter().rev() {
            init_stack.push(elem);
        }
        for elem in lhs.to_sequence().into_iter().rev() {
            init_stack.push(elem);
        }

        SafeMulU64.run_tasm(&mut ExecutionState::with_stack(init_stack));
    }

    #[test]
    fn safe_mul_u64_simple() {
        let mut init_stack = get_init_tvm_stack();
        init_stack.push(BFieldElement::zero());
        init_stack.push(BFieldElement::new(100));
        init_stack.push(BFieldElement::zero());
        init_stack.push(BFieldElement::new(200));

        let mut expected = get_init_tvm_stack();
        expected.push(BFieldElement::zero());
        expected.push(BFieldElement::new(20_000));
        rust_tasm_equivalence_prop(
            SafeMulU64,
            &init_stack,
            &[],
            &[],
            &mut HashMap::default(),
            1,
            Some(&expected),
        );
    }
}