use num::Zero;
use twenty_first::{amount::u32s::U32s, shared_math::b_field_element::BFieldElement};

use crate::{
    get_init_tvm_stack, push_encodable,
    snippet::{DataType, Snippet},
    ExecutionState,
};

#[derive(Clone)]
pub struct DoublePow2U64;

impl Snippet for DoublePow2U64 {
    fn entrypoint(&self) -> String {
        "tasm_arithmetic_u64_pow2_double".to_string()
    }

    fn inputs(&self) -> Vec<String>
    where
        Self: Sized,
    {
        vec!["value_hi".to_string(), "value_lo".to_string()]
    }

    fn input_types(&self) -> Vec<crate::snippet::DataType> {
        vec![DataType::U64]
    }

    fn output_types(&self) -> Vec<crate::snippet::DataType> {
        vec![DataType::U64]
    }

    fn outputs(&self) -> Vec<String>
    where
        Self: Sized,
    {
        vec!["(value * 2)_hi".to_string(), "(value * 2)_lo".to_string()]
    }

    fn stack_diff(&self) -> isize
    where
        Self: Sized,
    {
        0
    }

    fn function_body(&self, _library: &mut crate::snippet_state::SnippetState) -> String {
        let entrypoint = self.entrypoint();
        const TWO_POW_31: &str = "2147483648";
        const ONE_HALF: &str = "9223372034707292161";

        // Double the value of a power of 2. Does *not* check if input is valid.
        format!(
            "
            // BEFORE: _ value_hi value_lo
            // AFTER: _ (value * 2)_hi (value * 2)_lo
            {entrypoint}:
                dup 0
                push {TWO_POW_31}
                eq
                skiz
                    call {entrypoint}_carry
                // _ value_hi value_lo

                push 2
                mul
                // _ value_hi (value_lo * 2)

                swap 1
                push 2
                mul
                swap 1
                // _ (value_hi * 2) (value_lo * 2)

                return


                {entrypoint}_carry:
                    // _ value_hi (value_lo = `2 ** 31`)

                    pop
                    pop
                    push {ONE_HALF} // sue me
                    push 0

                    return
                "
        )
    }

    fn crash_conditions() -> Vec<String>
    where
        Self: Sized,
    {
        vec![]
    }

    fn gen_input_states(&self) -> Vec<crate::ExecutionState>
    where
        Self: Sized,
    {
        let mut ret = vec![];
        for n in 0..63 {
            let n: U32s<2> = (1u64 >> n).try_into().unwrap();
            let mut input_stack = get_init_tvm_stack();

            push_encodable(&mut input_stack, &n);

            ret.push(ExecutionState::with_stack(input_stack))
        }

        ret
    }

    fn common_case_input_state(&self) -> ExecutionState
    where
        Self: Sized,
    {
        ExecutionState::with_stack(
            vec![
                get_init_tvm_stack(),
                vec![BFieldElement::zero(), BFieldElement::new(1 << 12)],
            ]
            .concat(),
        )
    }

    fn worst_case_input_state(&self) -> ExecutionState
    where
        Self: Sized,
    {
        // worst-case has carry from lower-bits to higher-bits
        ExecutionState::with_stack(
            vec![
                get_init_tvm_stack(),
                vec![BFieldElement::zero(), BFieldElement::new(1 << 31)],
            ]
            .concat(),
        )
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
    ) where
        Self: Sized,
    {
        let value_lo: u32 = stack.pop().unwrap().try_into().unwrap();
        let value_hi: u32 = stack.pop().unwrap().try_into().unwrap();
        let value: u64 = ((value_hi as u64) << 32) + value_lo as u64;
        let result: u64 = value * 2;

        stack.push(BFieldElement::new(result >> 32));
        stack.push(BFieldElement::new(result & u32::MAX as u64));
    }
}

#[cfg(test)]
mod tests {
    use crate::snippet_bencher::bench_and_write;
    use crate::test_helpers::rust_tasm_equivalence_prop_new;

    use super::*;

    #[test]
    fn double_pow2_u64_test() {
        rust_tasm_equivalence_prop_new(DoublePow2U64);
    }

    #[test]
    fn double_pow2_u64_benchmark() {
        bench_and_write(DoublePow2U64);
    }
}
