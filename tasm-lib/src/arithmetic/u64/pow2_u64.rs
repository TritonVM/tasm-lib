use std::collections::HashMap;

use num::One;
use triton_vm::prelude::*;
use triton_vm::twenty_first::prelude::U32s;

use crate::data_type::DataType;
use crate::empty_stack;
use crate::library::Library;
use crate::push_encodable;
use crate::traits::deprecated_snippet::DeprecatedSnippet;
use crate::ExecutionState;

/// Consumes top element which is interpreted as exponent. Pushes a
/// U32<2> to the top of the stack. So grows the stack by 1.
#[derive(Clone, Debug)]
pub struct Pow2U64;

impl DeprecatedSnippet for Pow2U64 {
    fn entrypoint_name(&self) -> String {
        "tasm_arithmetic_u64_pow2".to_string()
    }

    fn input_field_names(&self) -> Vec<String> {
        vec!["i".to_string()]
    }

    fn input_types(&self) -> Vec<DataType> {
        vec![DataType::U32]
    }

    fn output_field_names(&self) -> Vec<String> {
        vec!["(2^i)_hi".to_string(), "(2^i)_lo".to_string()]
    }

    fn output_types(&self) -> Vec<DataType> {
        vec![DataType::U64]
    }

    fn stack_diff(&self) -> isize {
        1
    }

    fn function_code(&self, _library: &mut Library) -> String {
        let entrypoint = self.entrypoint_name();

        format!(
            "{entrypoint}:
                push 2
                pow
                split
                return
            "
        )
    }

    fn crash_conditions(&self) -> Vec<String> {
        vec![]
    }

    fn gen_input_states(&self) -> Vec<ExecutionState> {
        (0..64)
            .map(|i: u32| {
                let mut stack = empty_stack();
                push_encodable(&mut stack, &i);
                ExecutionState::with_stack(stack)
            })
            .collect()
    }

    fn common_case_input_state(&self) -> ExecutionState {
        ExecutionState::with_stack([empty_stack(), vec![BFieldElement::new(31)]].concat())
    }

    fn worst_case_input_state(&self) -> ExecutionState {
        ExecutionState::with_stack([empty_stack(), vec![BFieldElement::new(63)]].concat())
    }

    fn rust_shadowing(
        &self,
        stack: &mut Vec<BFieldElement>,
        _std_in: Vec<BFieldElement>,
        _secret_in: Vec<BFieldElement>,
        _memory: &mut HashMap<BFieldElement, BFieldElement>,
    ) {
        // Find exponent
        let mut exponent: u32 = stack.pop().unwrap().try_into().unwrap();
        let mut res = U32s::<2>::one();

        while exponent > 0 {
            res.mul_two();
            exponent -= 1;
        }

        let mut res = res.encode();
        for _ in 0..res.len() {
            stack.push(res.pop().unwrap());
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::empty_stack;
    use crate::test_helpers::test_rust_equivalence_given_input_values_deprecated;
    use crate::test_helpers::test_rust_equivalence_multiple_deprecated;

    use super::*;

    #[test]
    fn pow2_static_test() {
        test_rust_equivalence_multiple_deprecated(&Pow2U64, true);
    }

    fn prop_exp_static(exponent: u8) {
        let mut init_stack = empty_stack();
        init_stack.push(BFieldElement::new(exponent as u64));

        // let expected = None;
        let mut expected = empty_stack();
        let res = 2u64.pow(exponent as u32);
        expected.push(BFieldElement::new(res >> 32));
        expected.push(BFieldElement::new(res & u32::MAX as u64));
        test_rust_equivalence_given_input_values_deprecated(
            &Pow2U64,
            &init_stack,
            &[],
            HashMap::default(),
            Some(&expected),
        );
    }

    #[test]
    fn all_exponents_static() {
        for i in 0..64 {
            prop_exp_static(i);
        }
    }
}

#[cfg(test)]
mod benches {
    use crate::snippet_bencher::bench_and_write;

    use super::*;

    #[test]
    fn pow2_static_benchmark() {
        bench_and_write(Pow2U64);
    }
}
