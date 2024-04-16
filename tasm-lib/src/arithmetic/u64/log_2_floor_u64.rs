use std::collections::HashMap;

use num::Zero;
use rand::RngCore;
use triton_vm::prelude::*;
use triton_vm::twenty_first::prelude::U32s;

use crate::data_type::DataType;
use crate::empty_stack;
use crate::library::Library;
use crate::push_encodable;
use crate::traits::deprecated_snippet::DeprecatedSnippet;
use crate::ExecutionState;

#[derive(Clone, Debug)]
pub struct Log2FloorU64;

impl DeprecatedSnippet for Log2FloorU64 {
    fn entrypoint_name(&self) -> String {
        "tasmlib_arithmetic_u64_log_2_floor".to_string()
    }

    fn input_field_names(&self) -> Vec<String> {
        vec!["value_hi".to_string(), "value_lo".to_string()]
    }

    fn input_types(&self) -> Vec<DataType> {
        vec![DataType::U64]
    }

    fn output_field_names(&self) -> Vec<String> {
        vec!["log2_floor(value)".to_string()]
    }

    fn output_types(&self) -> Vec<DataType> {
        vec![DataType::U32]
    }

    fn stack_diff(&self) -> isize {
        -1
    }

    fn function_code(&self, _library: &mut Library) -> String {
        let entrypoint = self.entrypoint_name();

        // assumes that top of stack is a valid u32s<2>
        format!(
            "
                // BEFORE: _ value_hi value_lo
                // AFTER:  _ log2_floor(value)
                {entrypoint}:
                    swap 1
                    push 1
                    dup 1
                    // stack: _ value_lo value_hi 1 value_hi

                    skiz call {entrypoint}_then
                    skiz call {entrypoint}_else
                    // stack: _ log2_floor(value)

                    return

                {entrypoint}_then:
                    // value_hi != 0
                    // stack: // stack: _ value_lo value_hi 1
                    pop 1
                    swap 1
                    pop 1     // stack: _ value_hi

                    log_2_floor
                    push 32
                    add
                    // stack: _ (log2_floor(value_hi) + 32)

                    push 0
                    // stack: _ (log2_floor(value_hi) + 32) 0

                    return

                {entrypoint}_else:
                    // value_hi == 0
                    // stack: _ value_lo value_hi
                    pop 1
                    log_2_floor
                    return
                "
        )
    }

    fn crash_conditions(&self) -> Vec<String> {
        vec![
            "if value_hi is not a u32".to_string(),
            "if value_lo is not a u32".to_string(),
        ]
    }

    fn gen_input_states(&self) -> Vec<ExecutionState> {
        let n: u64 = rand::thread_rng().next_u64();
        let n: U32s<2> = n.try_into().unwrap();
        let mut input_stack = empty_stack();

        push_encodable(&mut input_stack, &n);

        vec![ExecutionState::with_stack(input_stack)]
    }

    fn common_case_input_state(&self) -> ExecutionState {
        ExecutionState::with_stack(
            [
                empty_stack(),
                vec![BFieldElement::zero(), BFieldElement::new(1 << 31)],
            ]
            .concat(),
        )
    }

    fn worst_case_input_state(&self) -> ExecutionState {
        ExecutionState::with_stack(
            [
                empty_stack(),
                vec![
                    BFieldElement::new((1 << 31) + 1),
                    BFieldElement::new(1 << 31),
                ],
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
        // Get input value as a u64
        let lo: u32 = stack.pop().unwrap().try_into().unwrap();
        let hi: u32 = stack.pop().unwrap().try_into().unwrap();
        let value_u64: u64 = lo as u64 + (1 << 32) * (hi as u64);

        let log_2_floor = value_u64.ilog2() as u64;
        stack.push(BFieldElement::new(log_2_floor));
    }
}

#[cfg(test)]
mod tests {
    use triton_vm::prelude::BFieldCodec;
    use triton_vm::twenty_first::prelude::U32s;

    use crate::empty_stack;
    use crate::test_helpers::test_rust_equivalence_given_input_values_deprecated;
    use crate::test_helpers::test_rust_equivalence_multiple_deprecated;

    use super::*;

    #[test]
    fn log_2_floor_u64_test() {
        test_rust_equivalence_multiple_deprecated(&Log2FloorU64, true);
    }

    #[should_panic]
    #[test]
    fn lo_is_not_u32() {
        let mut init_stack = empty_stack();
        init_stack.push(BFieldElement::new(16));
        init_stack.push(BFieldElement::new(u32::MAX as u64 + 1));

        test_rust_equivalence_given_input_values_deprecated(
            &Log2FloorU64,
            &init_stack,
            &[],
            HashMap::default(),
            None,
        );
    }

    #[should_panic]
    #[test]
    fn hi_is_not_u32() {
        let mut init_stack = empty_stack();
        init_stack.push(BFieldElement::new(u32::MAX as u64 + 1));
        init_stack.push(BFieldElement::new(16));

        test_rust_equivalence_given_input_values_deprecated(
            &Log2FloorU64,
            &init_stack,
            &[],
            HashMap::default(),
            None,
        );
    }

    #[should_panic]
    #[test]
    fn hi_is_not_u32_alt() {
        let n: u64 = rand::thread_rng().next_u32() as u64;
        let mut init_stack = empty_stack();
        init_stack.push(BFieldElement::new(u32::MAX as u64 + 1 + n));
        init_stack.push(BFieldElement::new(16));

        test_rust_equivalence_given_input_values_deprecated(
            &Log2FloorU64,
            &init_stack,
            &[],
            HashMap::default(),
            None,
        );
    }

    #[test]
    fn u32s_2_log2_floor() {
        let mut expected = empty_stack();
        expected.push(BFieldElement::new(0));
        prop_log2_floor_u32s_2(1, &expected);

        expected = empty_stack();
        expected.push(BFieldElement::new(1));
        prop_log2_floor_u32s_2(2, &expected);
        prop_log2_floor_u32s_2(3, &expected);

        expected = empty_stack();
        expected.push(BFieldElement::new(2));
        prop_log2_floor_u32s_2(4, &expected);
        prop_log2_floor_u32s_2(5, &expected);
        prop_log2_floor_u32s_2(6, &expected);
        prop_log2_floor_u32s_2(7, &expected);

        expected = empty_stack();
        expected.push(BFieldElement::new(31));
        prop_log2_floor_u32s_2(u32::MAX as u64 - 20000, &expected);
        prop_log2_floor_u32s_2(u32::MAX as u64 - 1, &expected);
        prop_log2_floor_u32s_2(u32::MAX as u64, &expected);

        expected = empty_stack();
        expected.push(BFieldElement::new(32));
        prop_log2_floor_u32s_2(u32::MAX as u64 + 1, &expected);
        prop_log2_floor_u32s_2(u32::MAX as u64 + 2, &expected);
        prop_log2_floor_u32s_2(u32::MAX as u64 + 800, &expected);
        prop_log2_floor_u32s_2(u32::MAX as u64 + u32::MAX as u64, &expected);
        prop_log2_floor_u32s_2(u32::MAX as u64 + u32::MAX as u64 + 1, &expected);

        for i in 0..64 {
            expected = empty_stack();
            expected.push(BFieldElement::new(i));
            prop_log2_floor_u32s_2(1 << i, &expected);
            if i > 0 {
                prop_log2_floor_u32s_2((1 << i) + 1, &expected);
            }
            if i > 1 {
                prop_log2_floor_u32s_2((1 << i) + 2, &expected);
                prop_log2_floor_u32s_2((1 << i) + 3, &expected);

                expected = empty_stack();
                expected.push(BFieldElement::new(i - 1));
                prop_log2_floor_u32s_2((1 << i) - 1, &expected);
            }
        }
    }

    fn prop_log2_floor_u32s_2(value: u64, expected: &[BFieldElement]) {
        let mut init_stack = empty_stack();
        let value_as_u32_2 =
            U32s::new([(value & 0xFFFFFFFFu32 as u64) as u32, (value >> 32) as u32]);
        for elem in value_as_u32_2.encode().into_iter().rev() {
            init_stack.push(elem);
        }

        test_rust_equivalence_given_input_values_deprecated(
            &Log2FloorU64,
            &init_stack,
            &[],
            HashMap::default(),
            Some(expected),
        );
    }
}

#[cfg(test)]
mod benches {
    use crate::snippet_bencher::bench_and_write;

    use super::*;

    #[test]
    fn log_2_floor_u64_benchmark() {
        bench_and_write(Log2FloorU64);
    }
}
