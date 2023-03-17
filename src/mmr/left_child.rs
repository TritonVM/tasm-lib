use std::collections::HashMap;

use num::BigUint;
use rand::{thread_rng, Rng};
use twenty_first::amount::u32s::U32s;
use twenty_first::shared_math::b_field_element::BFieldElement;
use twenty_first::util_types::algebraic_hasher::Hashable;
use twenty_first::util_types::mmr;

use crate::arithmetic::u64::pow2_u64::Pow2U64;
use crate::arithmetic::u64::sub_u64::SubU64;
use crate::library::Library;
use crate::snippet::{DataType, Snippet};
use crate::{get_init_tvm_stack, ExecutionState};

#[derive(Clone)]
pub struct MmrLeftChild;

impl Snippet for MmrLeftChild {
    fn inputs(&self) -> Vec<String> {
        vec![
            "node_index_hi".to_string(),
            "node_index_lo".to_string(),
            "height".to_string(),
        ]
    }

    fn outputs(&self) -> Vec<String> {
        vec!["left_child_hi".to_string(), "left_child_lo".to_string()]
    }

    fn input_types(&self) -> Vec<crate::snippet::DataType> {
        vec![DataType::U64, DataType::U32]
    }

    fn output_types(&self) -> Vec<crate::snippet::DataType> {
        vec![DataType::U64]
    }

    fn crash_conditions() -> Vec<String> {
        vec!["Input values are not u32s".to_string()]
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
        "tasm_mmr_left_child".to_string()
    }

    fn function_body(&self, library: &mut Library) -> String {
        let entrypoint = self.entrypoint();
        let pow2_u64 = library.import(Box::new(Pow2U64));
        let sub_u64 = library.import(Box::new(SubU64));
        format!(
            "
            // Before: _ ni_hi ni_lo height
            // After: _ left_child_hi left_child_lo
            {entrypoint}:
                call {pow2_u64} // -> _ ni_hi ni_lo (2^height)_hi (2^height)_lo
                swap 2
                swap 1
                swap 3
                swap 1                // -> _ (2^height)_hi (2^height)_lo ni_hi ni_lo
                call {sub_u64}    // -> _ left_child_hi left_child_lo
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
        let height: u32 = stack.pop().unwrap().try_into().unwrap();
        let node_index_lo: u32 = stack.pop().unwrap().try_into().unwrap();
        let node_index_hi: u32 = stack.pop().unwrap().try_into().unwrap();
        let node_index: u64 = (node_index_hi as u64) * (1u64 << 32) + node_index_lo as u64;
        let ret: u64 = mmr::shared_basic::left_child(node_index, height);
        let ret: U32s<2> = U32s::from(BigUint::from(ret));

        stack.append(&mut ret.to_sequence().into_iter().rev().collect());
    }

    fn common_case_input_state(&self) -> ExecutionState
    where
        Self: Sized,
    {
        prepare_state(1 << 32)
    }

    fn worst_case_input_state(&self) -> ExecutionState
    where
        Self: Sized,
    {
        prepare_state(1 << 62)
    }
}

fn prepare_state(node_index: u64) -> ExecutionState {
    let mut stack = get_init_tvm_stack();
    let node_index_hi = BFieldElement::new(node_index >> 32);
    let node_index_lo = BFieldElement::new(node_index & u32::MAX as u64);
    let (_, height) = mmr::shared_advanced::right_lineage_length_and_own_height(node_index);
    let height = BFieldElement::new(height as u64);
    stack.push(node_index_hi);
    stack.push(node_index_lo);
    stack.push(height);
    ExecutionState::with_stack(stack)
}

#[cfg(test)]
mod tests {
    use num::Zero;
    use twenty_first::shared_math::b_field_element::BFieldElement;
    use twenty_first::util_types::algebraic_hasher::Hashable;

    use crate::get_init_tvm_stack;
    use crate::snippet_bencher::bench_and_write;
    use crate::test_helpers::{rust_tasm_equivalence_prop, rust_tasm_equivalence_prop_new};

    use super::*;

    #[test]
    fn left_child_test() {
        rust_tasm_equivalence_prop_new(MmrLeftChild);
    }

    #[test]
    fn left_child_benchmark() {
        bench_and_write(MmrLeftChild);
    }

    #[test]
    fn u32s_left_child_simple() {
        let mut expected_stack = get_init_tvm_stack();
        expected_stack.push(BFieldElement::zero());
        expected_stack.push(BFieldElement::new(3));
        prop_left_child(U32s::<2>::from(7), 2, Some(&expected_stack));

        expected_stack = get_init_tvm_stack();
        expected_stack.push(BFieldElement::zero());
        expected_stack.push(BFieldElement::new(10));
        prop_left_child(U32s::<2>::from(14), 2, Some(&expected_stack));

        expected_stack = get_init_tvm_stack();
        expected_stack.push(BFieldElement::zero());
        expected_stack.push(BFieldElement::new(4));
        prop_left_child(U32s::<2>::from(6), 1, Some(&expected_stack));

        expected_stack = get_init_tvm_stack();
        expected_stack.push(BFieldElement::zero());
        expected_stack.push(BFieldElement::new(16));
        prop_left_child(U32s::<2>::from(18), 1, Some(&expected_stack));
    }

    fn prop_left_child(node_index: U32s<2>, height: u32, expected: Option<&[BFieldElement]>) {
        let mut init_stack = get_init_tvm_stack();
        for elem in node_index.to_sequence().into_iter().rev() {
            init_stack.push(elem);
        }
        init_stack.push(BFieldElement::new(height as u64));

        let _execution_result = rust_tasm_equivalence_prop::<MmrLeftChild>(
            MmrLeftChild,
            &init_stack,
            &[],
            &[],
            &mut HashMap::default(),
            0,
            expected,
        );
    }
}
