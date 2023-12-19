use rand::{thread_rng, RngCore};
use twenty_first::shared_math::b_field_element::BFieldElement;

use crate::data_type::DataType;
use crate::{
    arithmetic::u64::{and_u64::AndU64, log_2_floor_u64::Log2FloorU64},
    empty_stack,
    snippet::DeprecatedSnippet,
    ExecutionState,
};

use super::{decr_u64::DecrU64, xor_u64::XorU64};

#[derive(Clone, Debug)]
pub struct IndexOfLastNonZeroBitU64;

impl DeprecatedSnippet for IndexOfLastNonZeroBitU64 {
    fn entrypoint_name(&self) -> String {
        "tasm_arithmetic_u64_index_of_last_nonzero_bit".to_string()
    }

    fn input_field_names(&self) -> Vec<String> {
        vec!["value_hi".to_string(), "value_lo".to_string()]
    }

    fn input_types(&self) -> Vec<crate::data_type::DataType> {
        vec![DataType::U64]
    }

    fn output_types(&self) -> Vec<crate::data_type::DataType> {
        vec![DataType::U32]
    }

    fn output_field_names(&self) -> Vec<String> {
        vec!["index_of_last_nonzero_bit(value)".to_string()]
    }

    fn stack_diff(&self) -> isize {
        -1
    }

    fn function_code(&self, library: &mut crate::library::Library) -> String {
        let entrypoint = self.entrypoint_name();
        let decr = library.import(Box::new(DecrU64));
        let xor = library.import(Box::new(XorU64));
        let and = library.import(Box::new(AndU64));
        let log_2_floor_u64 = library.import(Box::new(Log2FloorU64));

        const U32MAX: u32 = u32::MAX;
        // Finds the least significant set bit using `x & ~(x - 1)` where
        // ~ denotes bit-inversion. Bit-inversion is achieved using
        // `x ^ (0xFFFFFFFF)`.
        format!(
            "
            // BEFORE: _ value_hi value_lo
            // AFTER: _ index_of_last_non-zero_bit
            {entrypoint}:
                dup 1
                dup 1
                // _ value_hi value_lo value_hi value_lo

                call {decr}
                // _ value_hi value_lo (value - 1)_hi (value - 1)_lo

                push {U32MAX}
                push {U32MAX}
                // _ value_hi value_lo (value - 1)_hi (value - 1)_lo 0xFFFFFFFF 0xFFFFFFFF

                call {xor}
                // _ value_hi value_lo ~(value - 1)_hi ~(value - 1)_lo

                call {and}
                // _ (value & ~(value - 1))_hi (value & ~(value - 1))_lo

                // The above value is now a power of two in u64. Calling log2_floor on this
                // value gives us the index we are looking for.
                call {log_2_floor_u64}

                return

            "
        )
    }

    fn crash_conditions(&self) -> Vec<String> {
        vec![
            "input is zero".to_owned(),
            "any input is not a u32".to_owned(),
        ]
    }

    fn gen_input_states(&self) -> Vec<crate::ExecutionState> {
        let mut rng = thread_rng();
        let mut ret = vec![];
        for _ in 0..10 {
            ret.push(prepare_state(rng.next_u64()));
        }

        ret
    }

    fn common_case_input_state(&self) -> crate::ExecutionState {
        prepare_state(1 << 31)
    }

    fn worst_case_input_state(&self) -> crate::ExecutionState {
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
    ) {
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
    let mut stack = empty_stack();
    stack.push(BFieldElement::new(value_hi as u64));
    stack.push(BFieldElement::new(value_lo as u64));
    ExecutionState::with_stack(stack)
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use num::Zero;

    use crate::test_helpers::{
        test_rust_equivalence_given_input_values_deprecated,
        test_rust_equivalence_multiple_deprecated,
    };

    use super::*;

    #[test]
    fn index_of_last_nonzero_bit_test() {
        test_rust_equivalence_multiple_deprecated(&IndexOfLastNonZeroBitU64, true);
    }

    fn index_of_last_nonzero_bit_prop(value: u64, expected: u32) {
        println!("value: {value}");
        let mut init_stack = empty_stack();
        init_stack.push(BFieldElement::new(value >> 32));
        init_stack.push(BFieldElement::new(value & u32::MAX as u64));

        let mut expected_output = empty_stack();
        expected_output.push(BFieldElement::new(expected as u64));

        test_rust_equivalence_given_input_values_deprecated(
            &IndexOfLastNonZeroBitU64,
            &init_stack,
            &[],
            HashMap::default(),
            0,
            Some(&expected_output),
        );
    }

    #[should_panic]
    #[test]
    fn disallow_non_u32_input_hi() {
        let mut init_stack = empty_stack();
        init_stack.push(BFieldElement::new(1 << 32));
        init_stack.push(BFieldElement::zero());

        test_rust_equivalence_given_input_values_deprecated(
            &IndexOfLastNonZeroBitU64,
            &init_stack,
            &[],
            HashMap::default(),
            0,
            None,
        );
    }

    #[should_panic]
    #[test]
    fn disallow_non_u32_input_lo() {
        let mut init_stack = empty_stack();
        init_stack.push(BFieldElement::zero());
        init_stack.push(BFieldElement::new(1 << 32));

        test_rust_equivalence_given_input_values_deprecated(
            &IndexOfLastNonZeroBitU64,
            &init_stack,
            &[],
            HashMap::default(),
            0,
            None,
        );
    }

    #[should_panic]
    #[test]
    fn disallow_zero_input() {
        let mut init_stack = empty_stack();
        init_stack.push(BFieldElement::zero());
        init_stack.push(BFieldElement::zero());

        test_rust_equivalence_given_input_values_deprecated(
            &IndexOfLastNonZeroBitU64,
            &init_stack,
            &[],
            HashMap::default(),
            0,
            None,
        );
    }

    #[test]
    fn unit_tests() {
        index_of_last_nonzero_bit_prop(1, 0);
        index_of_last_nonzero_bit_prop(2, 1);
        index_of_last_nonzero_bit_prop(3, 0);
        index_of_last_nonzero_bit_prop(10, 1);
        index_of_last_nonzero_bit_prop(11, 0);
        index_of_last_nonzero_bit_prop(12, 2);
        index_of_last_nonzero_bit_prop(64, 6);
        index_of_last_nonzero_bit_prop(65, 0);
        index_of_last_nonzero_bit_prop(66, 1);
        index_of_last_nonzero_bit_prop(67, 0);
        index_of_last_nonzero_bit_prop(68, 2);
        index_of_last_nonzero_bit_prop(u32::MAX as u64, 0);
        index_of_last_nonzero_bit_prop(1 << 32, 32);
        index_of_last_nonzero_bit_prop((1 << 32) + (1 << 17), 17);
        index_of_last_nonzero_bit_prop((1 << 62) + (1 << 17), 17);
        index_of_last_nonzero_bit_prop((1 << 63) + (1 << 17), 17);
        index_of_last_nonzero_bit_prop((1 << 63) + (1 << 41), 41);
        index_of_last_nonzero_bit_prop((1 << 63) + (1 << 60), 60);
        index_of_last_nonzero_bit_prop(1 << 63, 63);
    }
}

#[cfg(test)]
mod benches {
    use super::*;
    use crate::snippet_bencher::bench_and_write;

    #[test]
    fn index_of_last_nonzero_bit_benchmark() {
        bench_and_write(IndexOfLastNonZeroBitU64);
    }
}
