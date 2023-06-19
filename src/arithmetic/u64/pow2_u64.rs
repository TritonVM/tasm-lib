use std::collections::HashMap;

use num::One;
use twenty_first::amount::u32s::U32s;
use twenty_first::shared_math::b_field_element::BFieldElement;
use twenty_first::shared_math::bfield_codec::BFieldCodec;

use crate::snippet::{DataType, Snippet};
use crate::snippet_state::SnippetState;
use crate::{get_init_tvm_stack, push_encodable, ExecutionState};

/// Consumes top element which is interpreted as exponent. Pushes a
/// U32<2> to the top of the stack. So grows the stack by 1.
#[derive(Clone, Debug)]
pub struct Pow2U64;

impl Snippet for Pow2U64 {
    fn inputs(&self) -> Vec<String> {
        vec!["i".to_string()]
    }

    fn outputs(&self) -> Vec<String> {
        vec!["(2^i)_hi".to_string(), "(2^i)_lo".to_string()]
    }

    fn input_types(&self) -> Vec<crate::snippet::DataType> {
        vec![DataType::U32]
    }

    fn output_types(&self) -> Vec<crate::snippet::DataType> {
        vec![DataType::U64]
    }

    fn crash_conditions(&self) -> Vec<String> {
        vec![]
    }

    fn gen_input_states(&self) -> Vec<ExecutionState> {
        (0..64)
            .map(|i: u32| {
                let mut stack = get_init_tvm_stack();
                push_encodable(&mut stack, &i);
                ExecutionState::with_stack(stack)
            })
            .collect()
    }

    fn stack_diff(&self) -> isize {
        1
    }

    fn entrypoint(&self) -> String {
        "tasm_arithmetic_u64_pow2".to_string()
    }

    fn function_code(&self, _library: &mut SnippetState) -> String {
        let entrypoint = self.entrypoint();

        format!(
            "{entrypoint}:
                push 2
                pow
                split
                return
            "
        )
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

    fn common_case_input_state(&self) -> ExecutionState {
        ExecutionState::with_stack(
            vec![get_init_tvm_stack(), vec![BFieldElement::new(31)]].concat(),
        )
    }

    fn worst_case_input_state(&self) -> ExecutionState {
        ExecutionState::with_stack(
            vec![get_init_tvm_stack(), vec![BFieldElement::new(63)]].concat(),
        )
    }
}

#[cfg(test)]
mod tests {

    use crate::get_init_tvm_stack;

    use crate::test_helpers::{
        test_rust_equivalence_given_input_state, test_rust_equivalence_multiple,
    };

    use super::*;

    #[test]
    fn pow2_static_test() {
        test_rust_equivalence_multiple(&Pow2U64, true);
    }

    fn prop_exp_static(exponent: u8) {
        let mut init_stack = get_init_tvm_stack();
        init_stack.push(BFieldElement::new(exponent as u64));

        // let expected = None;
        let mut expected = get_init_tvm_stack();
        let res = 2u64.pow(exponent as u32);
        expected.push(BFieldElement::new(res >> 32));
        expected.push(BFieldElement::new(res & u32::MAX as u64));
        let mut execution_result = test_rust_equivalence_given_input_state(
            &Pow2U64,
            &init_stack,
            &[],
            &[],
            &mut HashMap::default(),
            0,
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
    use super::*;
    use crate::snippet_bencher::bench_and_write;

    #[test]
    fn pow2_static_benchmark() {
        bench_and_write(Pow2U64);
    }
}
