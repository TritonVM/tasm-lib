use std::collections::HashMap;

use num::One;
use rand::prelude::*;
use triton_vm::prelude::*;
use triton_vm::twenty_first::prelude::U32s;

use crate::data_type::DataType;
use crate::empty_stack;
use crate::library::Library;
use crate::push_encodable;
use crate::traits::deprecated_snippet::DeprecatedSnippet;
use crate::InitVmState;

#[derive(Clone, Debug)]
pub struct IncrU64;

impl DeprecatedSnippet for IncrU64 {
    fn entrypoint_name(&self) -> String {
        "tasmlib_arithmetic_u64_incr".to_string()
    }

    fn input_field_names(&self) -> Vec<String> {
        vec!["value_hi".to_string(), "value_lo".to_string()]
    }

    fn input_types(&self) -> Vec<DataType> {
        vec![DataType::U64]
    }

    fn output_field_names(&self) -> Vec<String> {
        vec!["(value + 1)_hi".to_string(), "(value + 1)_lo".to_string()]
    }

    fn output_types(&self) -> Vec<DataType> {
        vec![DataType::U64]
    }

    fn stack_diff(&self) -> isize {
        0
    }

    fn function_code(&self, _library: &mut Library) -> String {
        let entrypoint = self.entrypoint_name();
        const TWO_POW_32: u64 = 1 << 32;
        format!(
            "
            // Before: _ value_hi value_lo
            // After:  _ (value + 1)_hi (value + 1)_lo
            {entrypoint}_carry:
                pop 1
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

    fn crash_conditions(&self) -> Vec<String> {
        vec!["value == u64::MAX".to_string()]
    }

    fn gen_input_states(&self) -> Vec<InitVmState> {
        let mut rng = rand::thread_rng();
        let values = vec![
            U32s::new([u32::MAX, 0]),
            U32s::new([0, u32::MAX]),
            U32s::new([u32::MAX, u32::MAX - 1]),
            // U32s::new([u32::MAX, u32::MAX])
            rng.next_u32().into(),
            U32s::<2>::try_from(rng.next_u64()).unwrap(),
        ];
        values
            .into_iter()
            .map(|value| {
                let mut stack = empty_stack();
                push_encodable(&mut stack, &value);
                InitVmState::with_stack(stack)
            })
            .collect()
    }

    fn common_case_input_state(&self) -> InitVmState {
        // no carry
        InitVmState::with_stack(
            [
                empty_stack(),
                vec![BFieldElement::new(1000), BFieldElement::new(7)],
            ]
            .concat(),
        )
    }

    fn worst_case_input_state(&self) -> InitVmState {
        // with carry
        InitVmState::with_stack(
            [
                empty_stack(),
                vec![BFieldElement::new(1000), BFieldElement::new((1 << 32) - 1)],
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
        let ab_incr = ab + U32s::one();
        let mut res = ab_incr.encode();
        for _ in 0..res.len() {
            stack.push(res.pop().unwrap());
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::empty_stack;
    use crate::push_encodable;
    use crate::test_helpers::test_rust_equivalence_multiple_deprecated;

    use super::*;

    #[test]
    fn incr_u64_test() {
        test_rust_equivalence_multiple_deprecated(&IncrU64, true);
    }

    #[test]
    fn incr_u64_negative_tasm_test() {
        let mut stack = empty_stack();
        let u64_max = U32s::<2>::try_from(u64::MAX).unwrap();
        push_encodable(&mut stack, &u64_max);
        assert!(IncrU64
            .link_and_run_tasm_for_test(&mut stack, vec![], NonDeterminism::default())
            .is_err());
    }

    #[test]
    #[should_panic]
    fn incr_u64_negative_rust_test() {
        let mut stack = empty_stack();
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
    use crate::snippet_bencher::bench_and_write;

    use super::*;

    #[test]
    fn incr_u64_benchmark() {
        bench_and_write(IncrU64);
    }
}
