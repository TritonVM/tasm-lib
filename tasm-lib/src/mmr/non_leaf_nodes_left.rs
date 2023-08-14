use rand::{thread_rng, Rng};
use std::collections::HashMap;
use twenty_first::amount::u32s::U32s;
use twenty_first::shared_math::b_field_element::BFieldElement;

use crate::arithmetic::u64::add_u64::AddU64;
use crate::arithmetic::u64::and_u64::AndU64;
use crate::arithmetic::u64::decr_u64::DecrU64;
use crate::arithmetic::u64::eq_u64::EqU64;
use crate::arithmetic::u64::incr_u64::IncrU64;
use crate::arithmetic::u64::log_2_floor_u64::Log2FloorU64;
use crate::arithmetic::u64::pow2_u64::Pow2U64;
use crate::library::Library;
use crate::rust_shadowing_helper_functions::non_leaf_nodes_left;
use crate::snippet::{DataType, DepracatedSnippet};
use crate::{get_init_tvm_stack, push_encodable, ExecutionState};

#[derive(Clone, Debug)]
pub struct MmrNonLeafNodesLeftUsingAnd;

impl DepracatedSnippet for MmrNonLeafNodesLeftUsingAnd {
    fn input_field_names(&self) -> Vec<String> {
        vec!["leaf_index_hi".to_string(), "leaf_index_lo".to_string()]
    }

    fn output_field_names(&self) -> Vec<String> {
        vec!["node_count_hi".to_string(), "node_count_lo".to_string()]
    }

    fn input_types(&self) -> Vec<crate::snippet::DataType> {
        vec![DataType::U64]
    }

    fn output_types(&self) -> Vec<crate::snippet::DataType> {
        vec![DataType::U64]
    }

    fn crash_conditions(&self) -> Vec<String> {
        vec!["Input values are not u32s".to_string()]
    }

    fn gen_input_states(&self) -> Vec<crate::ExecutionState> {
        let mut ret: Vec<ExecutionState> = vec![];
        for _ in 0..30 {
            let leaf_index = thread_rng().gen_range(0..u64::MAX / 2);
            ret.push(prepare_state(leaf_index));
        }

        // Ensure that we also test for leaf_index == 0
        ret.push(prepare_state(0));

        ret
    }

    fn stack_diff(&self) -> isize {
        0
    }

    fn entrypoint_name(&self) -> String {
        "tasm_mmr_non_leaf_nodes_left".to_string()
    }

    fn function_code(&self, library: &mut Library) -> String {
        let entrypoint = self.entrypoint_name();
        let log_2_floor_u64 = library.import(Box::new(Log2FloorU64));
        let pow2_u64 = library.import(Box::new(Pow2U64));
        let and_u64 = library.import(Box::new(AndU64));
        let eq_u64 = library.import(Box::new(EqU64));
        let decr_u64 = library.import(Box::new(DecrU64));
        let incr_u64 = library.import(Box::new(IncrU64));
        let add_u64 = library.import(Box::new(AddU64));

        format!(
            "
        // BEFORE: _ leaf_index_hi leaf_index_lo
        // AFTER: _ node_count_hi node_count_lo
        {entrypoint}:
            // Handle leaf_index == 0: if leaf_index == 0 => return leaf_index
            dup 1 dup 1 push 0 push 0 call {eq_u64}
            // _ leaf_index_hi leaf_index_lo (leaf_index == 0)

            skiz return
            // _ leaf_index_hi leaf_index_lo

            dup 1 dup 1
            call {log_2_floor_u64}
            call {incr_u64}
            // stack: _ di_hi di_lo log2_floor

            push 0
            push 0 push 0
            // stack: _ di_hi di_lo log2_floor h ret_hi ret_lo

            call {entrypoint}_while
            // stack: _ di_hi di_lo log2_floor h ret_hi ret_lo

            swap 4 pop
            // stack: _ di_hi ret_lo log2_floor h ret_hi

            swap 4 pop
            // stack: _ ret_hi ret_lo log2_floor h

            pop pop
            // stack: _ ret_hi ret_lo

            return

        // Start/end stack: _ di_hi di_lo log2_floor h ret_hi ret_lo
        {entrypoint}_while:
            dup 3 dup 3 eq
            // stack: _ di_hi di_lo log2_floor h ret_hi ret_lo (h == log2_floor)

            skiz return
            // stack: _ di_hi di_lo log2_floor h ret_hi ret_lo

            dup 2
            call {pow2_u64}
            // stack: _ di_hi di_lo log2_floor h ret_hi ret_lo pow_hi pow_lo

            dup 1 dup 1
            dup 9 dup 9
            // stack: _ di_hi di_lo log2_floor h ret_hi ret_lo pow_hi pow_lo pow_hi pow_lo di_hi di_lo

            call {and_u64}
            // stack: _ di_hi di_lo log2_floor h ret_hi ret_lo pow_hi pow_lo and_hi and_lo

            push 0 push 0
            call {eq_u64}
            // stack: _ di_hi di_lo log2_floor h ret_hi ret_lo pow_hi pow_lo (and_expr == 0)

            push 0
            eq
            skiz call {entrypoint}_if_then
            // stack: _ di_hi di_lo log2_floor h ret_hi ret_lo pow_hi pow_lo

            pop pop
            // stack: _ di_hi di_lo log2_floor h ret_hi ret_lo

            swap 2 push 1 add swap 2
            // stack: _ di_hi di_lo log2_floor (h + 1) ret_hi ret_lo

            recurse

            // Start/end stack: _ di_hi di_lo log2_floor h ret_hi ret_lo pow_hi pow_lo
        {entrypoint}_if_then:
            call {decr_u64}
            // stack: _ di_hi di_lo log2_floor h ret_hi ret_lo (pow - 1)_hi (pow - 1)_lo

            call {add_u64}
            // stack: _ di_hi di_lo log2_floor h (ret + 2^h - 1)_hi (ret + 2^h - 1)_lo

            push 0 push 0
            // rename: ret expression to `ret`
            // stack: _ di_hi di_lo log2_floor h ret_hi ret_lo 0 0
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
        let leaf_index_lo: u32 = stack.pop().unwrap().try_into().unwrap();
        let leaf_index_hi: u32 = stack.pop().unwrap().try_into().unwrap();
        let leaf_index: u64 = (leaf_index_hi as u64) * (1u64 << 32) + leaf_index_lo as u64;

        let result = non_leaf_nodes_left(leaf_index);
        let result = U32s::<2>::try_from(result).unwrap();
        push_encodable(stack, &result);
    }

    fn common_case_input_state(&self) -> ExecutionState {
        prepare_state((1 << 31) - 1)
    }

    fn worst_case_input_state(&self) -> ExecutionState {
        prepare_state((1 << 63) - 1)
    }
}

fn prepare_state(leaf_index: u64) -> ExecutionState {
    let mut stack = get_init_tvm_stack();
    let leaf_index_hi = BFieldElement::new(leaf_index >> 32);
    let leaf_index_lo = BFieldElement::new(leaf_index & u32::MAX as u64);
    stack.push(leaf_index_hi);
    stack.push(leaf_index_lo);
    ExecutionState::with_stack(stack)
}

#[cfg(test)]
mod tests {
    use rand::{thread_rng, RngCore};
    use twenty_first::amount::u32s::U32s;
    use twenty_first::shared_math::b_field_element::BFieldElement;
    use twenty_first::shared_math::bfield_codec::BFieldCodec;

    use crate::get_init_tvm_stack;

    use crate::test_helpers::{
        test_rust_equivalence_given_input_values, test_rust_equivalence_multiple,
    };

    use super::*;

    #[test]
    fn non_leaf_nodes_left_test() {
        test_rust_equivalence_multiple(&MmrNonLeafNodesLeftUsingAnd, true);
    }

    #[test]
    fn non_leaf_nodes_left_using_and_test() {
        let mut expected = get_init_tvm_stack();
        expected.push(BFieldElement::new(0));
        expected.push(BFieldElement::new(0));
        prop_non_leaf_nodes_left_using_and(0, Some(&expected));
        prop_non_leaf_nodes_left_using_and(1, Some(&expected));

        expected = get_init_tvm_stack();
        expected.push(BFieldElement::new(0));
        expected.push(BFieldElement::new(1));
        prop_non_leaf_nodes_left_using_and(2, Some(&expected));
        prop_non_leaf_nodes_left_using_and(3, Some(&expected));

        expected = get_init_tvm_stack();
        expected.push(BFieldElement::new(0));
        expected.push(BFieldElement::new(3));
        prop_non_leaf_nodes_left_using_and(4, Some(&expected));
        prop_non_leaf_nodes_left_using_and(5, Some(&expected));

        expected = get_init_tvm_stack();
        expected.push(BFieldElement::new(0));
        expected.push(BFieldElement::new(4));
        prop_non_leaf_nodes_left_using_and(6, Some(&expected));
        prop_non_leaf_nodes_left_using_and(7, Some(&expected));

        expected = get_init_tvm_stack();
        expected.push(BFieldElement::new(0));
        expected.push(BFieldElement::new(7));
        prop_non_leaf_nodes_left_using_and(8, Some(&expected));
        prop_non_leaf_nodes_left_using_and(9, Some(&expected));

        expected = get_init_tvm_stack();
        expected.push(BFieldElement::new(0));
        expected.push(BFieldElement::new(8));
        prop_non_leaf_nodes_left_using_and(10, Some(&expected));
        prop_non_leaf_nodes_left_using_and(11, Some(&expected));

        expected = get_init_tvm_stack();
        expected.push(BFieldElement::new(0));
        expected.push(BFieldElement::new(10));
        prop_non_leaf_nodes_left_using_and(12, Some(&expected));
        prop_non_leaf_nodes_left_using_and(13, Some(&expected));

        prop_non_leaf_nodes_left_using_and(u32::MAX as u64, None);
        prop_non_leaf_nodes_left_using_and(u64::MAX / 2, None);
    }

    #[test]
    fn non_leaf_nodes_using_and_pbt() {
        let mut rng = thread_rng();
        for _ in 0..10 {
            prop_non_leaf_nodes_left_using_and(rng.next_u64(), None);
        }
    }

    fn prop_non_leaf_nodes_left_using_and(leaf_index: u64, expected: Option<&[BFieldElement]>) {
        println!("leaf_index = {leaf_index}");
        let mut init_stack = get_init_tvm_stack();
        let value_as_u32_2 = U32s::new([
            (leaf_index & 0xFFFFFFFFu32 as u64) as u32,
            (leaf_index >> 32) as u32,
        ]);
        for elem in value_as_u32_2.encode().into_iter().rev() {
            init_stack.push(elem);
        }

        test_rust_equivalence_given_input_values(
            &MmrNonLeafNodesLeftUsingAnd,
            &init_stack,
            &[],
            &mut HashMap::default(),
            0,
            expected,
        );
    }
}

#[cfg(test)]
mod benches {
    use super::*;
    use crate::snippet_bencher::bench_and_write;

    #[test]
    fn non_leaf_nodes_left_benchmark() {
        bench_and_write(MmrNonLeafNodesLeftUsingAnd);
    }
}
