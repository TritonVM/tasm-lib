use rand::{thread_rng, Rng};
use twenty_first::{shared_math::b_field_element::BFieldElement, util_types::mmr};

use crate::arithmetic::u64::decr_u64::DecrU64;
use crate::arithmetic::u64::incr_u64::IncrU64;
use crate::arithmetic::u64::log_2_floor_u64::Log2FloorU64;
use crate::arithmetic::u64::pow2_u64::Pow2U64;
use crate::arithmetic::u64::sub_u64::SubU64;
use crate::snippet::{DataType, Snippet};
use crate::{get_init_tvm_stack, ExecutionState};

#[derive(Clone, Debug)]
pub struct MmrRightLineageLength;

impl Snippet for MmrRightLineageLength {
    fn inputs(&self) -> Vec<String> {
        vec!["node_index_hi".to_string(), "node_index_lo".to_string()]
    }

    fn outputs(&self) -> Vec<String> {
        vec!["right_lineage_length".to_string()]
    }

    fn input_types(&self) -> Vec<crate::snippet::DataType> {
        vec![DataType::U64]
    }

    fn output_types(&self) -> Vec<crate::snippet::DataType> {
        vec![DataType::U32]
    }

    fn crash_conditions() -> Vec<String> {
        vec!["node index exceeds 2^63".to_string()]
    }

    fn gen_input_states(&self) -> Vec<crate::ExecutionState> {
        let mut ret: Vec<ExecutionState> = vec![];
        for _ in 0..10 {
            let node_index = thread_rng().gen_range(0..u64::MAX / 2);
            ret.push(prepare_state(node_index));
        }

        ret
    }

    fn stack_diff(&self) -> isize {
        -1
    }

    fn entrypoint(&self) -> String {
        "tasm_mmr_right_lineage_length".to_string()
    }

    fn function_body(&self, library: &mut crate::snippet_state::SnippetState) -> String {
        let entrypoint = self.entrypoint();
        let log_2_floor_u64 = library.import(Box::new(Log2FloorU64));
        let pow2_u64 = library.import(Box::new(Pow2U64));
        let sub_u64 = library.import(Box::new(SubU64));
        let decr = library.import(Box::new(DecrU64));
        let incr = library.import(Box::new(IncrU64));
        format!(
            "// Before: _ ni_hi ni_lo
            // After: _ right_lineage_count
            {entrypoint}:
                dup 1 dup 1 call {log_2_floor_u64}
                // _ ni_hi ni_lo log_2_floor

                push 1
                add
                // _ ni_hi ni_lo log_2_floor_plus_one

                // rename: log_2_floor_plus_one -> bit_width
                // _ ni_hi ni_lo bit_width

                dup 2 dup 2
                // _ ni_hi ni_lo bit_width ni_hi ni_lo

                dup 2
                call {pow2_u64}
                // _ ni_hi ni_lo bit_width ni_hi ni_lo npo2_hi npo2_lo

                call {sub_u64}
                // _ ni_hi ni_lo bit_width dist_hi dist_lo

                dup 1 push 0 eq push 0 eq
                // _ ni_hi ni_lo bit_width dist_hi dist_lo (dist_hi != 0)

                dup 1 dup 4 lt
                // _ ni_hi ni_lo bit_width dist_hi dist_lo (dist_hi != 0) (bit_width < dist_lo)

                add
                // _ ni_hi ni_lo bit_width dist_hi dist_lo (dist_hi != 0 || bit_width < dist_lo)

                push 1 swap 1
                skiz call {entrypoint}_then
                skiz call {entrypoint}_else

                // _ ni_hi ni_lo bit_width dist_hi ret

                swap 4 pop pop pop pop
                // _ ret

                return

                {entrypoint}_then:
                    // _ ni_hi ni_lo bit_width dist_hi dist_lo 1

                    pop
                    // _ ni_hi ni_lo bit_width dist_hi dist_lo

                    swap 1 swap 2
                    // _ ni_hi ni_lo dist_hi dist_lo bit_width

                    call {decr}
                    // _ ni_hi ni_lo dist_hi dist_lo (bit_width - 1)

                    call {pow2_u64}
                    // _ ni_hi ni_lo dist_hi dist_lo (npo2 >> 1)_hi (npo2 >> 1)_lo

                    dup 5 dup 5 call {sub_u64}
                    // _ ni_hi ni_lo dist_hi dist_lo (node_index - npo2 >> 1)_hi (node_index - npo2 >> 1)_lo

                    call {incr}
                    // _ ni_hi ni_lo dist_hi dist_lo (node_index - npo2 >> 1 + 1)_hi (node_index - npo2 >> 1 + 1)_lo

                    call {entrypoint}
                    // _ ni_hi ni_lo dist_hi dist_lo ret

                    push 0
                    return
                {entrypoint}_else:
                    // _ ni_hi ni_lo bit_width dist_hi dist_lo

                    push -1 add
                    // _ ni_hi ni_lo bit_width dist_hi (dist_lo - 1)

                    // rename: dist_lo - 1 -> ret
                    // _ ni_hi ni_lo bit_width dist_hi ret

                    return

            "
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
    ) {
        let node_index_lo: u32 = stack.pop().unwrap().try_into().unwrap();
        let node_index_hi: u32 = stack.pop().unwrap().try_into().unwrap();
        let node_index: u64 = ((node_index_hi as u64) << 32) + node_index_lo as u64;

        let right_lineage_length =
            mmr::shared_advanced::right_lineage_length_from_node_index(node_index);

        stack.push(BFieldElement::new(right_lineage_length as u64));
    }

    fn common_case_input_state(&self) -> ExecutionState
    where
        Self: Sized,
    {
        prepare_state((1 << 31) - 32)
    }

    fn worst_case_input_state(&self) -> ExecutionState
    where
        Self: Sized,
    {
        prepare_state((1 << 62) - 63)
    }
}

fn prepare_state(node_index: u64) -> ExecutionState {
    let mut stack = get_init_tvm_stack();
    let node_index_hi = BFieldElement::new(node_index >> 32);
    let node_index_lo = BFieldElement::new(node_index & u32::MAX as u64);
    stack.push(node_index_hi);
    stack.push(node_index_lo);
    ExecutionState::with_stack(stack)
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use crate::snippet_bencher::bench_and_write;
    use crate::test_helpers::{rust_tasm_equivalence_prop, rust_tasm_equivalence_prop_new};

    use super::*;

    #[test]
    fn right_lineage_length_test() {
        rust_tasm_equivalence_prop_new(MmrRightLineageLength);
    }

    #[test]
    fn right_lineage_length_benchmark() {
        bench_and_write(MmrRightLineageLength);
    }

    #[test]
    fn right_lineage_count_test() {
        prop_right_lineage_count(1, 0);
        prop_right_lineage_count(2, 1);
        prop_right_lineage_count(3, 0);
        prop_right_lineage_count(4, 0);
        prop_right_lineage_count(5, 2);
        prop_right_lineage_count(6, 1);
        prop_right_lineage_count(7, 0);
        prop_right_lineage_count(8, 0);
        prop_right_lineage_count(9, 1);

        prop_right_lineage_count(10, 0);
        prop_right_lineage_count(11, 0);
        prop_right_lineage_count(12, 3);
        prop_right_lineage_count(13, 2);
        prop_right_lineage_count(14, 1);
        prop_right_lineage_count(15, 0);
        prop_right_lineage_count(16, 0);
        prop_right_lineage_count(17, 1);
        prop_right_lineage_count(18, 0);
        prop_right_lineage_count(19, 0);

        prop_right_lineage_count(20, 2);
        prop_right_lineage_count(21, 1);
        prop_right_lineage_count(22, 0);
        prop_right_lineage_count(23, 0);
        prop_right_lineage_count(24, 1);
        prop_right_lineage_count(25, 0);
        prop_right_lineage_count(26, 0);
        prop_right_lineage_count(27, 4);
        prop_right_lineage_count(28, 3);
        prop_right_lineage_count(29, 2);

        prop_right_lineage_count(30, 1);
        prop_right_lineage_count(31, 0);
        prop_right_lineage_count(32, 0);
        prop_right_lineage_count(33, 1);
        prop_right_lineage_count(34, 0);
        prop_right_lineage_count(35, 0);
        prop_right_lineage_count(36, 2);
        prop_right_lineage_count(37, 1);
        prop_right_lineage_count(38, 0);
        prop_right_lineage_count(39, 0);

        prop_right_lineage_count(40, 1);
        prop_right_lineage_count(41, 0);

        // Run test for big numbers
        for i in 0..32 {
            prop_right_lineage_count(u32::MAX as u64 - i, i as u32);
        }

        // Run test for very big numbers
        for i in 0..63 {
            prop_right_lineage_count((u64::MAX >> 1) - i, i as u32);
        }
    }

    fn prop_right_lineage_count(node_index: u64, expected_count: u32) {
        println!("node_index = {node_index}");
        let mut init_stack = get_init_tvm_stack();
        let node_index_hi = BFieldElement::new(node_index >> 32);
        let node_index_lo = BFieldElement::new(node_index & u32::MAX as u64);
        init_stack.push(node_index_hi);
        init_stack.push(node_index_lo);

        // _ (right_lineage_count:u32)
        let expected = vec![
            get_init_tvm_stack(),
            vec![BFieldElement::new(expected_count as u64)],
        ]
        .concat();
        let _execution_result = rust_tasm_equivalence_prop(
            MmrRightLineageLength,
            &init_stack,
            &[],
            &[],
            &mut HashMap::default(),
            0,
            Some(&expected),
        );
    }
}
