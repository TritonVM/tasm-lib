use std::collections::HashMap;

use itertools::Itertools;
use num::One;
use num::Zero;
use triton_vm::prelude::*;
use triton_vm::twenty_first::prelude::U32s;

use crate::data_type::DataType;
use crate::empty_stack;
use crate::library::Library;
use crate::push_encodable;
use crate::traits::deprecated_snippet::DeprecatedSnippet;
use crate::InitVmState;

#[derive(Clone, Debug)]
pub struct DecrU64;

impl DeprecatedSnippet for DecrU64 {
    fn entrypoint_name(&self) -> String {
        "tasmlib_arithmetic_u64_decr".to_string()
    }

    fn input_field_names(&self) -> Vec<String> {
        vec!["value_hi".to_string(), "value_lo".to_string()]
    }

    fn input_types(&self) -> Vec<DataType> {
        vec![DataType::U64]
    }

    fn output_field_names(&self) -> Vec<String> {
        vec!["(value - 1)_hi".to_string(), "(value - 1)_lo".to_string()]
    }

    fn output_types(&self) -> Vec<DataType> {
        vec![DataType::U64]
    }

    fn stack_diff(&self) -> isize {
        0
    }

    fn function_code(&self, _library: &mut Library) -> String {
        let entrypoint = self.entrypoint_name();
        let carry_entrypoint = format!("{entrypoint}_carry");
        const U32_MAX: u32 = u32::MAX;

        triton_asm!(
            {entrypoint}:
                push -1
                add
                dup 0
                push -1
                eq
                skiz
                    call {carry_entrypoint}
                return

            {carry_entrypoint}:
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
        )
        .iter()
        .join("\n")
    }

    fn crash_conditions(&self) -> Vec<String> {
        vec!["value == 0".to_string()]
    }

    fn gen_input_states(&self) -> Vec<InitVmState> {
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
                InitVmState::with_stack(stack)
            })
            .collect()
    }

    fn common_case_input_state(&self) -> InitVmState {
        // no carry
        InitVmState::with_stack(
            [
                empty_stack(),
                vec![BFieldElement::zero(), BFieldElement::new(7)],
            ]
            .concat(),
        )
    }

    fn worst_case_input_state(&self) -> InitVmState {
        // with carry
        InitVmState::with_stack(
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
    use rand::prelude::*;

    use super::*;
    use crate::empty_stack;
    use crate::push_encodable;
    use crate::test_helpers::negative_test;
    use crate::test_helpers::test_rust_equivalence_given_input_values_deprecated;
    use crate::test_helpers::test_rust_equivalence_multiple_deprecated;
    use crate::traits::basic_snippet::BasicSnippet;
    use crate::traits::deprecated_snippet::tests::DeprecatedSnippetWrapper;

    #[test]
    fn decr_u64_test() {
        test_rust_equivalence_multiple_deprecated(&DecrU64, true);
    }

    #[test]
    fn decr_u64_negative_tasm_test() {
        let mut stack = empty_stack();
        push_encodable(&mut stack, &U32s::<2>::zero());
        assert!(DecrU64
            .link_and_run_tasm_for_test(&mut stack, vec![], NonDeterminism::default())
            .is_err());
    }

    #[test]
    fn decr_u64_negative_rust_test() {
        let snippet = DecrU64;
        let mut stack = DecrU64.init_stack_for_isolated_run();
        push_encodable(&mut stack, &U32s::<2>::zero());
        let snippet = DeprecatedSnippetWrapper::new(snippet);
        negative_test(
            &snippet,
            InitVmState::with_stack(stack),
            &[InstructionError::AssertionFailed],
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
