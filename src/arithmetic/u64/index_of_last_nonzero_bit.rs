use rand::{thread_rng, RngCore};
use twenty_first::shared_math::b_field_element::BFieldElement;

use crate::{
    get_init_tvm_stack,
    snippet::{DataType, Snippet},
    ExecutionState,
};

use super::log_2_floor_u64::Log2FloorU64;

#[derive(Clone)]
pub struct IndexOfLastNonZeroBitU64;

impl Snippet for IndexOfLastNonZeroBitU64 {
    fn entrypoint(&self) -> String {
        "tasm_arithmetic_u64_index_of_last_nonzero_bit".to_string()
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
        vec![DataType::U32]
    }

    fn outputs(&self) -> Vec<String>
    where
        Self: Sized,
    {
        vec!["index_of_last_nonzero_bit(value)".to_string()]
    }

    fn stack_diff(&self) -> isize
    where
        Self: Sized,
    {
        -1
    }

    fn function_body(&self, library: &mut crate::library::Library) -> String {
        let entrypoint = self.entrypoint();
        let log_2_floor_u64 = library.import(Box::new(Log2FloorU64));
        format!(
            "
            // BEFORE: _ value_hi value_lo
            // AFTER: _ log2_floor(value)
            {entrypoint}:
                dup1 dup1
                call {log_2_floor_u64}
                // _ value_hi value_lo i

                dup0
                dup0
                // rename: i -> ret
                // _ value_hi value_lo ret i i

                call {entrypoint}_while

                // _ value_hi value_lo ret i i
                pop
                pop
                swap2
                pop
                pop
                return

                // Start/end stack: // stack: _ value_hi value_lo ret i i
                {entrypoint}_while:

                    // return if i == 0
                    dup0
                    push -1
                    eq
                    skiz
                        return
                    // _ value_hi value_lo ret i i

                    // TODO: U32 table height can probably be reduced if
                    // we update the pow2 value with division instead of pow/split here.
                    dup0
                    push 2
                    pow
                    split
                    // _ value_hi value_lo ret i i pow2_hi pow2_lo

                    dup5
                    and
                    // _ value_hi value_lo ret i i pow2_hi (pow2_lo & value_lo)

                    swap1
                    dup6
                    and
                    // _ value_hi value_lo ret i i (pow2_lo & value_lo) (pow2_hi & value_hi)

                    push 0
                    eq
                    swap1
                    push 0
                    eq
                    // _ value_hi value_lo ret i i (pow2_hi & value_hi == 0) (pow2_lo & value_lo == 0)

                    mul
                    // _ value_hi value_lo ret i i (pow2_hi & value_hi == 0) && (pow2_lo & value_lo == 0)

                    push 0
                    eq
                    // _ value_hi value_lo ret i i !((pow2_hi & value_hi == 0) && (pow2_lo & value_lo == 0))

                    // rename: !((pow2_hi & value_hi == 0) && (pow2_lo & value_lo == 0)) -> nonzero
                    // _ value_hi value_lo ret i i nonzero
                    skiz
                        // _ value_hi value_lo ret i i
                        swap2
                        // _ value_hi value_lo i i ret
                    pop
                    // _ value_hi value_lo ret i

                    push -1
                    add
                    // _ value_hi value_lo ret (i - 1)

                    dup0
                    // _ value_hi value_lo ret (i - 1) (i - 1)

                    recurse
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
        let mut rng = thread_rng();
        let mut ret = vec![];
        for _ in 0..10 {
            ret.push(prepare_state(rng.next_u64()));
        }

        ret
    }

    fn common_case_input_state(&self) -> crate::ExecutionState
    where
        Self: Sized,
    {
        prepare_state(1 << 31)
    }

    fn worst_case_input_state(&self) -> crate::ExecutionState
    where
        Self: Sized,
    {
        prepare_state(1 << 62)
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

        let mut ret = 0;
        for i in (0..64).rev() {
            if value & (1 << i) != 0 {
                ret = i;
            }
        }

        stack.push(BFieldElement::new(ret as u64));
    }
}

fn prepare_state(value: u64) -> ExecutionState {
    let value_hi: u32 = (value >> 32) as u32;
    let value_lo: u32 = (value & u32::MAX as u64) as u32;
    let mut stack = get_init_tvm_stack();
    stack.push(BFieldElement::new(value_hi as u64));
    stack.push(BFieldElement::new(value_lo as u64));
    ExecutionState::with_stack(stack)
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use num::{One, Zero};

    use crate::{
        snippet_bencher::bench_and_write,
        test_helpers::{rust_tasm_equivalence_prop, rust_tasm_equivalence_prop_new},
    };

    use super::*;

    #[test]
    fn index_of_last_nonzero_bit_test() {
        rust_tasm_equivalence_prop_new(IndexOfLastNonZeroBitU64);
    }

    #[test]
    fn index_of_last_nonzero_bit_benchmark() {
        bench_and_write(IndexOfLastNonZeroBitU64);
    }

    #[test]
    fn ten_to_one() {
        let mut init_stack = get_init_tvm_stack();
        init_stack.push(BFieldElement::zero());
        init_stack.push(BFieldElement::new(10));

        let mut expected_output = get_init_tvm_stack();
        expected_output.push(BFieldElement::one());

        let _execution_result = rust_tasm_equivalence_prop(
            IndexOfLastNonZeroBitU64,
            &init_stack,
            &[],
            &[],
            &mut HashMap::default(),
            0,
            Some(&expected_output),
        );
    }

    #[test]
    fn eleven_to_zero() {
        let mut init_stack = get_init_tvm_stack();
        init_stack.push(BFieldElement::zero());
        init_stack.push(BFieldElement::new(11));

        let mut expected_output = get_init_tvm_stack();
        expected_output.push(BFieldElement::zero());

        let _execution_result = rust_tasm_equivalence_prop(
            IndexOfLastNonZeroBitU64,
            &init_stack,
            &[],
            &[],
            &mut HashMap::default(),
            0,
            Some(&expected_output),
        );
    }

    #[test]
    fn twelve_to_two() {
        let mut init_stack = get_init_tvm_stack();
        init_stack.push(BFieldElement::zero());
        init_stack.push(BFieldElement::new(12));

        let mut expected_output = get_init_tvm_stack();
        expected_output.push(BFieldElement::new(2));

        let _execution_result = rust_tasm_equivalence_prop(
            IndexOfLastNonZeroBitU64,
            &init_stack,
            &[],
            &[],
            &mut HashMap::default(),
            0,
            Some(&expected_output),
        );
    }

    #[test]
    fn three_to_zero() {
        let mut init_stack = get_init_tvm_stack();
        init_stack.push(BFieldElement::zero());
        init_stack.push(BFieldElement::new(3));

        let mut expected_output = get_init_tvm_stack();
        expected_output.push(BFieldElement::zero());

        let _execution_result = rust_tasm_equivalence_prop(
            IndexOfLastNonZeroBitU64,
            &init_stack,
            &[],
            &[],
            &mut HashMap::default(),
            0,
            Some(&expected_output),
        );
    }

    #[test]
    fn two_to_one() {
        let mut init_stack = get_init_tvm_stack();
        init_stack.push(BFieldElement::zero());
        init_stack.push(BFieldElement::new(2));

        let mut expected_output = get_init_tvm_stack();
        expected_output.push(BFieldElement::one());

        let _execution_result = rust_tasm_equivalence_prop(
            IndexOfLastNonZeroBitU64,
            &init_stack,
            &[],
            &[],
            &mut HashMap::default(),
            0,
            Some(&expected_output),
        );
    }

    #[test]
    fn one_to_zero() {
        let mut init_stack = get_init_tvm_stack();
        init_stack.push(BFieldElement::zero());
        init_stack.push(BFieldElement::one());

        let mut expected_output = get_init_tvm_stack();
        expected_output.push(BFieldElement::zero());

        let _execution_result = rust_tasm_equivalence_prop(
            IndexOfLastNonZeroBitU64,
            &init_stack,
            &[],
            &[],
            &mut HashMap::default(),
            0,
            Some(&expected_output),
        );
    }
}
