use rand::{thread_rng, RngCore};
use twenty_first::{amount::u32s::U32s, shared_math::b_field_element::BFieldElement};

use crate::{
    get_init_tvm_stack, push_hashable,
    snippet::{DataType, Snippet},
    ExecutionState,
};

#[derive(Clone)]
pub struct OrU64;

impl Snippet for OrU64 {
    fn entrypoint(&self) -> String {
        "tasm_arithmetic_u64_or_u64".to_string()
    }

    fn inputs(&self) -> Vec<String> {
        vec![
            "rhs_hi".to_string(),
            "rhs_lo".to_string(),
            "lhs_hi".to_string(),
            "lhs_lo".to_string(),
        ]
    }

    fn outputs(&self) -> Vec<String> {
        vec!["(lhs | rhs)_hi".to_string(), "(lhs | rhs)_lo".to_string()]
    }

    fn input_types(&self) -> Vec<crate::snippet::DataType> {
        vec![DataType::U64, DataType::U64]
    }

    fn output_types(&self) -> Vec<crate::snippet::DataType> {
        vec![DataType::U64]
    }

    fn stack_diff(&self) -> isize {
        -2
    }

    fn function_body(&self, _library: &mut crate::library::Library) -> String {
        let entrypoint = self.entrypoint();
        format!(
            "
                // BEFORE: _ rhs_hi rhs_lo lhs_hi lhs_lo
                // AFTER: _ (lhs | rhs)_hi (lhs | rhs)_lo
                {entrypoint}:
                    dup 2
                    dup 1
                    xor
                    // _ rhs_hi rhs_lo lhs_hi lhs_lo (rhs_lo ^ lhs_lo)

                    swap 3
                    and
                    // _ rhs_hi (rhs_lo ^ lhs_lo) lhs_hi (lhs_lo & rhs_lo)

                    swap 3
                    // _ (lhs_lo & rhs_lo) (rhs_lo ^ lhs_lo) lhs_hi rhs_hi

                    dup 1
                    dup 1
                    xor
                    // _ (lhs_lo & rhs_lo) (rhs_lo ^ lhs_lo) lhs_hi rhs_hi (lhs_hi ^ rhs_hi)

                    swap 2
                    and
                    // _ (lhs_lo & rhs_lo) (rhs_lo ^ lhs_lo) (lhs_hi ^ rhs_hi) (rhs_hi & lhs_hi)

                    add
                    // _ (lhs_lo & rhs_lo) (rhs_lo ^ lhs_lo) (lhs_hi | rhs_hi)

                    swap 2
                    add
                    // _ (lhs_hi | rhs_hi) (rhs_lo | lhs_lo)

                    return
                    "
        )
    }

    fn crash_conditions() -> Vec<String> {
        vec!["Inputs are not u32".to_owned()]
    }

    fn gen_input_states(&self) -> Vec<crate::ExecutionState> {
        let mut ret: Vec<ExecutionState> = vec![];
        for _ in 0..100 {
            ret.push(prepare_state(
                thread_rng().next_u64(),
                thread_rng().next_u64(),
            ));
        }

        ret
    }

    fn rust_shadowing(
        &self,
        stack: &mut Vec<twenty_first::shared_math::b_field_element::BFieldElement>,
        _std_in: Vec<twenty_first::shared_math::b_field_element::BFieldElement>,
        _secret_in: Vec<twenty_first::shared_math::b_field_element::BFieldElement>,
        _memory: &mut std::collections::HashMap<
            twenty_first::shared_math::b_field_element::BFieldElement,
            twenty_first::shared_math::b_field_element::BFieldElement,
        >,
    ) {
        let lhs_lo: u32 = stack.pop().unwrap().try_into().unwrap();
        let lhs_hi: u32 = stack.pop().unwrap().try_into().unwrap();
        let rhs_lo: u32 = stack.pop().unwrap().try_into().unwrap();
        let rhs_hi: u32 = stack.pop().unwrap().try_into().unwrap();

        let lhs = ((lhs_hi as u64) << 32) + lhs_lo as u64;
        let rhs = ((rhs_hi as u64) << 32) + rhs_lo as u64;

        let or = lhs | rhs;
        stack.push(BFieldElement::new(or >> 32));
        stack.push(BFieldElement::new(or & u32::MAX as u64));
    }

    fn common_case_input_state(&self) -> ExecutionState
    where
        Self: Sized,
    {
        prepare_state(u32::MAX as u64, u32::MAX as u64)
    }

    fn worst_case_input_state(&self) -> ExecutionState
    where
        Self: Sized,
    {
        prepare_state(u64::MAX, u64::MAX)
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

    use crate::{
        snippet_bencher::bench_and_write,
        test_helpers::{rust_tasm_equivalence_prop, rust_tasm_equivalence_prop_new},
    };

    use super::*;

    #[test]
    fn snippet_test() {
        rust_tasm_equivalence_prop_new(OrU64);
    }

    #[test]
    fn u64_or_benchmark() {
        bench_and_write(OrU64);
    }

    #[test]
    fn or_simple_test() {
        prop_safe_or(10, 1, Some(11));
        prop_safe_or(10, (1 << 36) + 1, Some(11 + (1 << 36)));
        prop_safe_or(256, 30, Some(286));
        prop_safe_or(123, 0, Some(123));
        prop_safe_or(0, 123, Some(123));
        prop_safe_or(1 << 31, 1 << 30, Some((1 << 30) + (1 << 31)));
        prop_safe_or(0, 0, Some(0));
        prop_safe_or(14, 0, Some(14));
        prop_safe_or(u32::MAX as u64, 0, Some(u32::MAX as u64));
        prop_safe_or(0, u32::MAX as u64, Some(u32::MAX as u64));
        prop_safe_or(u32::MAX as u64, u32::MAX as u64, Some(u32::MAX as u64));
        prop_safe_or(u64::MAX, u64::MAX, Some(u64::MAX));
        prop_safe_or(u64::MAX, 0, Some(u64::MAX));
        prop_safe_or(0, u64::MAX, Some(u64::MAX));
    }

    fn prop_safe_or(lhs: u64, rhs: u64, expected: Option<u64>) {
        let rhs = U32s::<2>::try_from(rhs).unwrap();
        let lhs = U32s::<2>::try_from(lhs).unwrap();
        let mut init_stack = get_init_tvm_stack();
        push_hashable(&mut init_stack, &rhs);
        push_hashable(&mut init_stack, &lhs);

        let execution_result = rust_tasm_equivalence_prop(
            OrU64,
            &init_stack,
            &[],
            &[],
            &mut HashMap::default(),
            0,
            None,
        );

        let mut final_stack = execution_result.final_stack;
        if let Some(res) = expected {
            let lo: u32 = final_stack.pop().unwrap().try_into().unwrap();
            let hi: u32 = final_stack.pop().unwrap().try_into().unwrap();
            assert_eq!(res, lo as u64 + ((hi as u64) << 32));
        };
    }
}