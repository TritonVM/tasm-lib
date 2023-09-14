use std::collections::HashMap;

use num::One;
use rand::RngCore;
use twenty_first::amount::u32s::U32s;
use twenty_first::shared_math::b_field_element::BFieldElement;
use twenty_first::shared_math::bfield_codec::BFieldCodec;

use crate::library::Library;
use crate::snippet::{DataType, DeprecatedSnippet};
use crate::{get_init_tvm_stack, push_encodable, ExecutionState};

#[derive(Clone, Debug)]
pub struct IncrU64;

impl DeprecatedSnippet for IncrU64 {
    fn input_field_names(&self) -> Vec<String> {
        vec!["value_hi".to_string(), "value_lo".to_string()]
    }

    fn output_field_names(&self) -> Vec<String> {
        vec!["(value + 1)_hi".to_string(), "(value + 1)_lo".to_string()]
    }

    fn input_types(&self) -> Vec<crate::snippet::DataType> {
        vec![DataType::U64]
    }

    fn output_types(&self) -> Vec<crate::snippet::DataType> {
        vec![DataType::U64]
    }

    fn crash_conditions(&self) -> Vec<String> {
        vec!["value == u64::MAX".to_string()]
    }

    fn gen_input_states(&self) -> Vec<ExecutionState> {
        let mut rng = rand::thread_rng();
        let values = vec![
            U32s::new([u32::MAX, 0]),
            U32s::new([0, u32::MAX]),
            U32s::new([u32::MAX, u32::MAX - 1]),
            // U32s::new([u32::MAX, u32::MAX])
            U32s::<2>::try_from(rng.next_u32()).unwrap(),
            U32s::<2>::try_from(rng.next_u64()).unwrap(),
        ];
        values
            .into_iter()
            .map(|value| {
                let mut stack = get_init_tvm_stack();
                push_encodable(&mut stack, &value);
                ExecutionState::with_stack(stack)
            })
            .collect()
    }

    fn stack_diff(&self) -> isize {
        0
    }

    fn entrypoint_name(&self) -> String {
        "tasm_arithmetic_u64_incr".to_string()
    }

    fn function_code(&self, _library: &mut Library) -> String {
        let entrypoint = self.entrypoint_name();
        const TWO_POW_32: &str = "4294967296";
        format!(
            "
            // Before: _ value_hi value_lo
            // After: _ (value + 1)_hi (value + 1)_lo
            {entrypoint}_carry:
                pop
                push 1
                add
                dup 0
                push {TWO_POW_32}
                eq
                push 0
                eq
                assert
                push 0
                return

            {entrypoint}:
                push 1
                add
                dup 0
                push {TWO_POW_32}
                eq
                skiz
                    call {entrypoint}_carry
                return
            ",
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
        let ab_incr = ab + U32s::one();
        let mut res = ab_incr.encode();
        for _ in 0..res.len() {
            stack.push(res.pop().unwrap());
        }
    }

    fn common_case_input_state(&self) -> ExecutionState {
        // no carry
        ExecutionState::with_stack(
            [
                get_init_tvm_stack(),
                vec![BFieldElement::new(1000), BFieldElement::new(7)],
            ]
            .concat(),
        )
    }

    fn worst_case_input_state(&self) -> ExecutionState {
        // with carry
        ExecutionState::with_stack(
            [
                get_init_tvm_stack(),
                vec![BFieldElement::new(1000), BFieldElement::new((1 << 32) - 1)],
            ]
            .concat(),
        )
    }
}

#[cfg(test)]
mod tests {

    use crate::test_helpers::test_rust_equivalence_multiple_deprecated;
    use crate::{get_init_tvm_stack, push_encodable};

    use super::*;

    #[test]
    fn incr_u64_test() {
        test_rust_equivalence_multiple_deprecated(&IncrU64, true);
    }

    #[test]
    fn incr_u64_negative_tasm_test() {
        let mut stack = get_init_tvm_stack();
        let u64_max = U32s::<2>::try_from(u64::MAX).unwrap();
        push_encodable(&mut stack, &u64_max);
        assert!(IncrU64
            .link_and_run_tasm_for_test(&mut stack, vec![], vec![], &mut HashMap::default(), None)
            .is_err());
    }

    #[test]
    #[should_panic]
    fn incr_u64_negative_rust_test() {
        let mut stack = get_init_tvm_stack();
        let u64_max = U32s::<2>::try_from(u64::MAX).unwrap();
        push_encodable(&mut stack, &u64_max);
        IncrU64::rust_shadowing(
            &IncrU64,
            &mut stack,
            vec![],
            vec![],
            &mut HashMap::default(),
        );
    }
}

#[cfg(test)]
mod benches {
    use super::*;
    use crate::snippet_bencher::bench_and_write;

    #[test]
    fn incr_u64_benchmark() {
        bench_and_write(IncrU64);
    }
}
