use rand::thread_rng;
use rand::RngCore;
use triton_vm::prelude::*;
use triton_vm::twenty_first::prelude::U32s;

use crate::data_type::DataType;
use crate::empty_stack;
use crate::push_encodable;
use crate::traits::deprecated_snippet::DeprecatedSnippet;
use crate::ExecutionState;

#[derive(Clone, Debug)]
pub struct OrU64;

impl DeprecatedSnippet for OrU64 {
    fn entrypoint_name(&self) -> String {
        "tasmlib_arithmetic_u64_or_u64".to_string()
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
        vec!["(lhs | rhs)_hi".to_string(), "(lhs | rhs)_lo".to_string()]
    }

    fn output_types(&self) -> Vec<DataType> {
        vec![DataType::U64]
    }

    fn stack_diff(&self) -> isize {
        -2
    }

    fn function_code(&self, _library: &mut crate::library::Library) -> String {
        let entrypoint = self.entrypoint_name();
        format!(
            "
                // BEFORE: _ rhs_hi rhs_lo lhs_hi lhs_lo
                // AFTER:  _ (lhs | rhs)_hi (lhs | rhs)_lo
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

    fn crash_conditions(&self) -> Vec<String> {
        vec!["Inputs are not u32".to_owned()]
    }

    fn gen_input_states(&self) -> Vec<ExecutionState> {
        let mut ret: Vec<ExecutionState> = vec![];
        for _ in 0..100 {
            ret.push(prepare_state(
                thread_rng().next_u64(),
                thread_rng().next_u64(),
            ));
        }

        ret
    }

    fn common_case_input_state(&self) -> ExecutionState {
        prepare_state(u32::MAX as u64, u32::MAX as u64)
    }

    fn worst_case_input_state(&self) -> ExecutionState {
        prepare_state(u64::MAX, u64::MAX)
    }

    fn rust_shadowing(
        &self,
        stack: &mut Vec<BFieldElement>,
        _std_in: Vec<BFieldElement>,
        _secret_in: Vec<BFieldElement>,
        _memory: &mut std::collections::HashMap<BFieldElement, BFieldElement>,
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

    use crate::test_helpers::test_rust_equivalence_given_input_values_deprecated;
    use crate::test_helpers::test_rust_equivalence_multiple_deprecated;

    use super::*;

    #[test]
    fn snippet_test() {
        test_rust_equivalence_multiple_deprecated(&OrU64, true);
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
        let res = lhs | rhs;
        if let Some(exp) = expected {
            assert_eq!(exp, { res });
        }

        let rhs = U32s::<2>::try_from(rhs).unwrap();
        let lhs = U32s::<2>::try_from(lhs).unwrap();
        let mut init_stack = empty_stack();
        push_encodable(&mut init_stack, &rhs);
        push_encodable(&mut init_stack, &lhs);

        let mut expected = empty_stack();
        expected.push(BFieldElement::new(res >> 32));
        expected.push(BFieldElement::new(res & u32::MAX as u64));

        test_rust_equivalence_given_input_values_deprecated(
            &OrU64,
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
    fn u64_or_benchmark() {
        bench_and_write(OrU64);
    }
}
