use std::collections::HashMap;

use num::BigUint;
use rand::{thread_rng, Rng};
use twenty_first::amount::u32s::U32s;
use twenty_first::shared_math::b_field_element::BFieldElement;
use twenty_first::shared_math::bfield_codec::BFieldCodec;
use twenty_first::util_types::mmr;

use crate::arithmetic::u64::decr_u64::DecrU64;
use crate::library::Library;
use crate::snippet::{DataType, DeprecatedSnippet};
use crate::{empty_stack, ExecutionState};

#[derive(Clone, Debug)]
pub struct MmrRightChild;

impl DeprecatedSnippet for MmrRightChild {
    fn input_field_names(&self) -> Vec<String> {
        vec!["node_index_hi".to_string(), "node_index_lo".to_string()]
    }

    fn output_field_names(&self) -> Vec<String> {
        vec!["right_child_hi".to_string(), "right_child_lo".to_string()]
    }

    fn input_types(&self) -> Vec<crate::snippet::DataType> {
        vec![DataType::U64]
    }

    fn output_types(&self) -> Vec<crate::snippet::DataType> {
        vec![DataType::U64]
    }

    fn crash_conditions(&self) -> Vec<String> {
        vec!["node_index == 0".to_string()]
    }

    fn gen_input_states(&self) -> Vec<crate::ExecutionState> {
        let mut ret: Vec<ExecutionState> = vec![];
        for _ in 0..10 {
            let node_index = thread_rng().gen_range(0..u64::MAX);
            ret.push(prepare_state(node_index));
        }

        ret
    }

    fn stack_diff(&self) -> isize {
        0
    }

    fn entrypoint_name(&self) -> String {
        "tasm_mmr_right_child".to_string()
    }

    /// Consider inlining this, instead of calling a function
    fn function_code(&self, library: &mut Library) -> String {
        let entrypoint = self.entrypoint_name();
        let decr_u64 = library.import(Box::new(DecrU64));
        format!(
            "
            // Before: _ nodex_index_hi node_index_lo
            // After: _ right_child_hi right_child_lo
            {entrypoint}:
                call {decr_u64}
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
        let node_index_lo: u32 = stack.pop().unwrap().try_into().unwrap();
        let node_index_hi: u32 = stack.pop().unwrap().try_into().unwrap();
        let node_index: u64 = (node_index_hi as u64) * (1u64 << 32) + node_index_lo as u64;
        let ret: u64 = mmr::shared_basic::right_child(node_index);
        let ret: U32s<2> = U32s::from(BigUint::from(ret));

        stack.append(&mut ret.encode().into_iter().rev().collect());
    }

    fn common_case_input_state(&self) -> ExecutionState {
        prepare_state(1 << 20)
    }

    fn worst_case_input_state(&self) -> ExecutionState {
        // Worst case is when there is a carry. Which happens when nodex_index % (1 << 32) == 0
        prepare_state(1 << 32)
    }
}

fn prepare_state(node_index: u64) -> ExecutionState {
    let mut stack = empty_stack();
    let node_index_hi = BFieldElement::new(node_index >> 32);
    let node_index_lo = BFieldElement::new(node_index & u32::MAX as u64);
    stack.push(node_index_hi);
    stack.push(node_index_lo);
    ExecutionState::with_stack(stack)
}

#[cfg(test)]
mod tests {
    use num::Zero;
    use twenty_first::shared_math::b_field_element::BFieldElement;
    use twenty_first::shared_math::bfield_codec::BFieldCodec;

    use crate::empty_stack;

    use crate::test_helpers::{
        test_rust_equivalence_given_input_values_deprecated,
        test_rust_equivalence_multiple_deprecated,
    };

    use super::*;

    #[test]
    fn right_child_test() {
        test_rust_equivalence_multiple_deprecated(&MmrRightChild, true);
    }

    #[test]
    fn u32s_right_child_simple() {
        let mut expected_stack = empty_stack();
        expected_stack.push(BFieldElement::zero());
        expected_stack.push(BFieldElement::new(2));
        prop_right_child(U32s::<2>::from(3), Some(&expected_stack));

        expected_stack = empty_stack();
        expected_stack.push(BFieldElement::zero());
        expected_stack.push(BFieldElement::new(9));
        prop_right_child(U32s::<2>::from(10), Some(&expected_stack));

        expected_stack = empty_stack();
        expected_stack.push(BFieldElement::zero());
        expected_stack.push(BFieldElement::new(14));
        prop_right_child(U32s::<2>::from(15), Some(&expected_stack));
    }

    fn prop_right_child(node_index: U32s<2>, expected: Option<&[BFieldElement]>) {
        let mut init_stack = empty_stack();
        for elem in node_index.encode().into_iter().rev() {
            init_stack.push(elem);
        }

        test_rust_equivalence_given_input_values_deprecated::<MmrRightChild>(
            &MmrRightChild,
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
    fn right_child_benchmark() {
        bench_and_write(MmrRightChild);
    }
}
