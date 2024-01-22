use crate::twenty_first::amount::u32s::U32s;
use crate::twenty_first::shared_math::b_field_element::BFieldElement;
use rand::RngCore;

use crate::data_type::DataType;
use crate::library::Library;
use crate::traits::deprecated_snippet::DeprecatedSnippet;
use crate::{empty_stack, push_encodable, ExecutionState};

#[derive(Clone, Debug)]
pub struct SafeMulU64;

impl DeprecatedSnippet for SafeMulU64 {
    fn entrypoint_name(&self) -> String {
        "tasm_arithmetic_u64_safe_mul".to_string()
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

    fn output_types(&self) -> Vec<DataType> {
        vec![DataType::U64]
    }

    fn output_field_names(&self) -> Vec<String> {
        vec!["prod_hi".to_string(), "prod_lo".to_string()]
    }

    fn stack_diff(&self) -> isize {
        -2
    }

    fn function_code(&self, _library: &mut Library) -> String {
        let entrypoint = self.entrypoint_name();

        format!(
            "
                // BEFORE: _ rhs_hi rhs_lo lhs_hi lhs_lo
                // AFTER: _ prod_hi prod_lo
                {entrypoint}:
                    // `lhs_lo * rhs_lo`:
                    dup 0 dup 3 mul
                    // _ rhs_hi rhs_lo lhs_hi lhs_lo (lhs_lo * rhs_lo)

                    // `rhs_hi * lhs_lo` (consume and `lhs_lo`):
                    swap 4
                    swap 1
                    dup 1
                    mul
                    // _ (lhs_lo * rhs_lo) rhs_lo lhs_hi rhs_hi (lhs_lo * rhs_hi)

                    // `rhs_lo * lhs_hi` (consume `rhs_lo`):
                    swap 3
                    dup 2
                    mul
                    // _ (lhs_lo * rhs_lo) (lhs_lo * rhs_hi) lhs_hi rhs_hi (rhs_lo * lhs_hi)

                    // `lhs_hi * rhs_hi` (consume `lhs_hi` and `rhs_hi`):
                    swap 2
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
                    swap 1
                    push 0
                    eq
                    assert
                    // _ a b c_lo

                    swap 1
                    split
                    swap 1
                    push 0
                    eq
                    assert
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
                    push 0
                    eq
                    assert
                    // _ a_lo (c_lo + a_hi + b_lo)_lo

                    swap 1
                    // _ (c_lo + a_hi + b_lo)_lo a_lo

                    // _ prod_hi prod_lo

                    return
                    "
        )
    }

    fn crash_conditions(&self) -> Vec<String> {
        vec!["Product is greater than u64::MAX".to_string()]
    }

    fn gen_input_states(&self) -> Vec<ExecutionState> {
        let mut rng = rand::thread_rng();

        let mut ret = vec![];
        for _ in 0..10 {
            ret.push(prepare_state(rng.next_u32() as u64, rng.next_u32() as u64));
        }

        ret.push(prepare_state(u32::MAX as u64, u32::MAX as u64));
        ret.push(prepare_state(u32::MAX as u64 - 1, u32::MAX as u64));
        ret.push(prepare_state(u32::MAX as u64, u32::MAX as u64 - 1));
        ret.push(prepare_state(u32::MAX as u64 - 1, u32::MAX as u64 - 1));
        ret.push(prepare_state(u32::MAX as u64 - 2, u32::MAX as u64));
        ret.push(prepare_state(u32::MAX as u64, u32::MAX as u64 - 2));

        ret
    }

    fn common_case_input_state(&self) -> ExecutionState {
        prepare_state(1 << 31, (1 << 25) - 1)
    }

    fn worst_case_input_state(&self) -> ExecutionState {
        prepare_state(1 << 31, (1 << 31) - 1)
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
    let mut init_stack = empty_stack();
    push_encodable(&mut init_stack, &a);
    push_encodable(&mut init_stack, &b);
    ExecutionState::with_stack(init_stack)
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use crate::twenty_first::shared_math::bfield_codec::BFieldCodec;
    use num::Zero;

    use crate::test_helpers::{
        test_rust_equivalence_given_input_values_deprecated,
        test_rust_equivalence_multiple_deprecated,
    };

    use super::*;

    #[test]
    fn safe_mul_u64_test() {
        test_rust_equivalence_multiple_deprecated(&SafeMulU64, true);
    }

    #[should_panic]
    #[test]
    fn overflow_test_1() {
        // Crash because (rhs_hi * lhs_hi) != 0
        let lhs: U32s<2> = U32s::try_from(1u64 << 32).unwrap();
        let rhs: U32s<2> = U32s::try_from(1u64 << 32).unwrap();
        let mut init_stack = empty_stack();
        for elem in rhs.encode().into_iter().rev() {
            init_stack.push(elem);
        }
        for elem in lhs.encode().into_iter().rev() {
            init_stack.push(elem);
        }

        SafeMulU64
            .link_and_run_tasm_from_state_for_test(&mut ExecutionState::with_stack(init_stack));
    }

    #[should_panic]
    #[test]
    fn overflow_test_2() {
        // Crash because (rhs_lo * lhs_hi)_hi != 0
        let lhs: U32s<2> = U32s::try_from(1u64 << 31).unwrap();
        let rhs: U32s<2> = U32s::try_from(1u64 << 33).unwrap();
        let mut init_stack = empty_stack();
        for elem in rhs.encode().into_iter().rev() {
            init_stack.push(elem);
        }
        for elem in lhs.encode().into_iter().rev() {
            init_stack.push(elem);
        }

        SafeMulU64
            .link_and_run_tasm_from_state_for_test(&mut ExecutionState::with_stack(init_stack));
    }

    #[should_panic]
    #[test]
    fn overflow_test_3() {
        // Crash because (lhs_lo * rhs_hi)_hi != 0
        let lhs: U32s<2> = U32s::try_from(1u64 << 33).unwrap();
        let rhs: U32s<2> = U32s::try_from(1u64 << 31).unwrap();
        let mut init_stack = empty_stack();
        for elem in rhs.encode().into_iter().rev() {
            init_stack.push(elem);
        }
        for elem in lhs.encode().into_iter().rev() {
            init_stack.push(elem);
        }

        SafeMulU64
            .link_and_run_tasm_from_state_for_test(&mut ExecutionState::with_stack(init_stack));
    }

    #[should_panic]
    #[test]
    fn overflow_test_4() {
        // Crash because (c_lo + a_hi + b_lo)_hi != 0
        let lhs: U32s<2> = U32s::try_from((1u64 << 33) + 5).unwrap();
        let rhs: U32s<2> = U32s::try_from((1u64 << 31) - 1).unwrap();
        let mut init_stack = empty_stack();
        for elem in rhs.encode().into_iter().rev() {
            init_stack.push(elem);
        }
        for elem in lhs.encode().into_iter().rev() {
            init_stack.push(elem);
        }

        SafeMulU64
            .link_and_run_tasm_from_state_for_test(&mut ExecutionState::with_stack(init_stack));
    }

    #[test]
    fn safe_mul_u64_simple() {
        let mut init_stack = empty_stack();
        init_stack.push(BFieldElement::zero());
        init_stack.push(BFieldElement::new(100));
        init_stack.push(BFieldElement::zero());
        init_stack.push(BFieldElement::new(200));

        let mut expected = empty_stack();
        expected.push(BFieldElement::zero());
        expected.push(BFieldElement::new(20_000));
        test_rust_equivalence_given_input_values_deprecated(
            &SafeMulU64,
            &init_stack,
            &[],
            HashMap::default(),
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
    fn safe_u64_benchmark() {
        bench_and_write(SafeMulU64);
    }
}
