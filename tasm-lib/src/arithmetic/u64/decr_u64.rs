use std::collections::HashMap;

use num::{One, Zero};
use twenty_first::amount::u32s::U32s;
use twenty_first::shared_math::b_field_element::BFieldElement;
use twenty_first::shared_math::bfield_codec::BFieldCodec;

use crate::data_type::DataType;
use crate::library::Library;
use crate::traits::deprecated_snippet::DeprecatedSnippet;
use crate::{empty_stack, push_encodable, ExecutionState};

#[derive(Clone, Debug)]
pub struct DecrU64;

impl DeprecatedSnippet for DecrU64 {
    fn entrypoint_name(&self) -> String {
        "tasm_arithmetic_u64_decr".to_string()
    }

    fn input_field_names(&self) -> Vec<String> {
        vec!["value_hi".to_string(), "value_lo".to_string()]
    }

    fn input_types(&self) -> Vec<crate::data_type::DataType> {
        vec![DataType::U64]
    }

    fn output_field_names(&self) -> Vec<String> {
        vec!["(value - 1)_hi".to_string(), "(value - 1)_lo".to_string()]
    }

    fn output_types(&self) -> Vec<crate::data_type::DataType> {
        vec![DataType::U64]
    }

    fn stack_diff(&self) -> isize {
        0
    }

    fn function_code(&self, _library: &mut Library) -> String {
        let entrypoint = self.entrypoint_name();
        const U32_MAX: u32 = u32::MAX;

        format!(
            "
            {entrypoint}:
                push -1
                add
                dup 0
                push -1
                eq
                skiz
                    call {entrypoint}_carry
                return

            {entrypoint}_carry:
                pop 1
                push -1
                add
                dup 0
                push -1
                eq
                push 0
                eq
                assert
                push {U32_MAX}
                return
            "
        )
    }

    fn crash_conditions(&self) -> Vec<String> {
        vec!["value == 0".to_string()]
    }

    fn gen_input_states(&self) -> Vec<ExecutionState> {
        let values = vec![
            // U32s::<2>::zero(),
            U32s::<2>::new([0, 14]),
            U32s::<2>::new([u32::MAX, 13]),
        ];
        values
            .into_iter()
            .map(|value| {
                let mut stack = empty_stack();
                push_encodable(&mut stack, &value);
                ExecutionState::with_stack(stack)
            })
            .collect()
    }

    fn common_case_input_state(&self) -> ExecutionState {
        // no carry
        ExecutionState::with_stack(
            [
                empty_stack(),
                vec![BFieldElement::zero(), BFieldElement::new(7)],
            ]
            .concat(),
        )
    }

    fn worst_case_input_state(&self) -> ExecutionState {
        // with carry
        ExecutionState::with_stack(
            [
                empty_stack(),
                vec![BFieldElement::new(1000), BFieldElement::new(0)],
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
        let a: u32 = stack.pop().unwrap().try_into().unwrap();
        let b: u32 = stack.pop().unwrap().try_into().unwrap();
        let ab = U32s::<2>::new([a, b]);
        let ab_incr = ab - U32s::one();
        let mut res = ab_incr.encode();
        for _ in 0..res.len() {
            stack.push(res.pop().unwrap());
        }
    }
}

#[cfg(test)]
mod tests {
    use num::Zero;
    use rand::Rng;
    use triton_vm::NonDeterminism;

    use crate::test_helpers::{
        test_rust_equivalence_given_input_values_deprecated,
        test_rust_equivalence_multiple_deprecated,
    };
    use crate::{empty_stack, push_encodable};

    use super::*;

    #[test]
    fn decr_u64_test() {
        test_rust_equivalence_multiple_deprecated(&DecrU64, true);
    }

    #[test]
    fn decr_u64_negative_tasm_test() {
        let mut stack = empty_stack();
        push_encodable(&mut stack, &U32s::<2>::zero());
        assert!(DecrU64
            .link_and_run_tasm_for_test(&mut stack, vec![], NonDeterminism::default(), None)
            .is_err());
    }

    #[test]
    #[should_panic]
    fn decr_u64_negative_rust_test() {
        let mut stack = empty_stack();
        push_encodable(&mut stack, &U32s::<2>::zero());
        DecrU64::rust_shadowing(
            &DecrU64,
            &mut stack,
            vec![],
            vec![],
            &mut HashMap::default(),
        );
    }

    #[test]
    fn decr_u64_pbt() {
        prop_decr_u64(U32s::new([u32::MAX, 0]));
        prop_decr_u64(U32s::new([0, u32::MAX]));
        prop_decr_u64(U32s::new([u32::MAX, u32::MAX - 1]));
        prop_decr_u64(U32s::new([0, 1]));

        let mut rng = rand::thread_rng();
        for _ in 0..10 {
            prop_decr_u64(U32s::new([0, rng.gen()]));
            prop_decr_u64(U32s::new([rng.gen(), rng.gen()]));
        }
    }

    fn prop_decr_u64(value: U32s<2>) {
        let mut stack = empty_stack();
        push_encodable(&mut stack, &value);
        test_rust_equivalence_given_input_values_deprecated(
            &DecrU64,
            &stack,
            &[],
            HashMap::default(),
            0,
            None,
        );
    }
}

#[cfg(test)]
mod benches {
    use super::*;
    use crate::snippet_bencher::bench_and_write;

    #[test]
    fn decr_u64_benchmark() {
        bench_and_write(DecrU64);
    }
}
