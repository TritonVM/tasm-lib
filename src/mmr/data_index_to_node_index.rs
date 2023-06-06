use rand::{thread_rng, Rng};
use std::collections::HashMap;
use twenty_first::shared_math::b_field_element::BFieldElement;
use twenty_first::util_types::mmr;

use crate::arithmetic::u64::add_u64::AddU64;
use crate::arithmetic::u64::incr_u64::IncrU64;
use crate::snippet::{DataType, Snippet};
use crate::snippet_state::SnippetState;
use crate::{get_init_tvm_stack, ExecutionState};

use super::non_leaf_nodes_left::MmrNonLeafNodesLeftUsingAnd;

#[derive(Clone, Debug)]
pub struct DataIndexToNodeIndex;

impl Snippet for DataIndexToNodeIndex {
    fn inputs(&self) -> Vec<String> {
        vec!["leaf_index_hi".to_string(), "leaf_index_lo".to_string()]
    }

    fn outputs(&self) -> Vec<String> {
        vec!["node_index_hi".to_string(), "node_index_lo".to_string()]
    }

    fn input_types(&self) -> Vec<crate::snippet::DataType> {
        vec![DataType::U64]
    }

    fn output_types(&self) -> Vec<crate::snippet::DataType> {
        vec![DataType::U64]
    }

    fn crash_conditions(&self) -> Vec<String> {
        vec!["leaf_index value is larger than 2^63".to_string()]
    }

    fn gen_input_states(&self) -> Vec<ExecutionState> {
        let mut ret: Vec<ExecutionState> = vec![];
        for _ in 0..40 {
            ret.push(prepare_state(thread_rng().gen_range(0..u64::MAX / 2)));
        }

        ret
    }

    fn stack_diff(&self) -> isize {
        // Pops a U32s<2> and pushes a U32s<2>
        0
    }

    fn entrypoint(&self) -> String {
        "tasm_mmr_data_index_to_node_index".to_string()
    }

    fn function_body(&self, library: &mut SnippetState) -> String {
        let entrypoint = self.entrypoint();
        let non_leaf_nodes_left = library.import(Box::new(MmrNonLeafNodesLeftUsingAnd));
        let incr_u64 = library.import(Box::new(IncrU64));
        let add_u64 = library.import(Box::new(AddU64));
        format!("
                // BEFORE: _ leaf_index_hi leaf_index_lo
                // AFTER: _ node_index_hi node_index_lo
                {entrypoint}:
                    dup 1 dup 1
                    // stack: _ leaf_index_hi leaf_index_lo leaf_index_hi leaf_index_lo

                    call {non_leaf_nodes_left}
                    // stack: _ leaf_index_hi leaf_index_lo non_leaf_nodes_left_hi non_leaf_nodes_left_lo

                    call {add_u64}
                    call {incr_u64}
                    // stack: _ node_index_hi node_index_lo

                    return
                    ")
    }

    fn rust_shadowing(
        &self,
        stack: &mut Vec<BFieldElement>,
        _std_in: Vec<BFieldElement>,
        _secret_in: Vec<BFieldElement>,
        _memory: &mut HashMap<BFieldElement, BFieldElement>,
    ) {
        let data_index_lo: u32 = stack.pop().unwrap().try_into().unwrap();
        let data_index_hi: u32 = stack.pop().unwrap().try_into().unwrap();
        let data_index: u64 = (data_index_hi as u64) * (1u64 << 32) + data_index_lo as u64;

        let node_index: u64 = mmr::shared_advanced::leaf_index_to_node_index(data_index);
        stack.push(BFieldElement::new(node_index >> 32));
        stack.push(BFieldElement::new(node_index & 0xFFFFFFFFu32 as u64));
    }

    fn common_case_input_state(&self) -> ExecutionState
    where
        Self: Sized,
    {
        prepare_state((1 << 32) - 1)
    }

    fn worst_case_input_state(&self) -> ExecutionState
    where
        Self: Sized,
    {
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
    use crate::snippet_bencher::bench_and_write;
    use crate::test_helpers::{rust_tasm_equivalence_prop, rust_tasm_equivalence_prop_new};

    use super::*;

    #[test]
    fn data_index_to_node_index_test() {
        rust_tasm_equivalence_prop_new(&DataIndexToNodeIndex, true);
    }

    #[test]
    fn data_index_to_node_index_benchmark() {
        bench_and_write(DataIndexToNodeIndex);
    }

    #[test]
    fn data_index_to_node_index_simple_test() {
        let mut expected = get_init_tvm_stack();
        expected.push(BFieldElement::new(0));
        expected.push(BFieldElement::new(1));
        prop_data_index_to_node_index(0, Some(&expected));

        expected = get_init_tvm_stack();
        expected.push(BFieldElement::new(0));
        expected.push(BFieldElement::new(2));
        prop_data_index_to_node_index(1, Some(&expected));

        expected = get_init_tvm_stack();
        expected.push(BFieldElement::new(0));
        expected.push(BFieldElement::new(4));
        prop_data_index_to_node_index(2, Some(&expected));

        expected = get_init_tvm_stack();
        expected.push(BFieldElement::new(0));
        expected.push(BFieldElement::new(5));
        prop_data_index_to_node_index(3, Some(&expected));

        expected = get_init_tvm_stack();
        expected.push(BFieldElement::new(0));
        expected.push(BFieldElement::new(8));
        prop_data_index_to_node_index(4, Some(&expected));

        expected = get_init_tvm_stack();
        expected.push(BFieldElement::new(0));
        expected.push(BFieldElement::new(9));
        prop_data_index_to_node_index(5, Some(&expected));

        expected = get_init_tvm_stack();
        expected.push(BFieldElement::new(0));
        expected.push(BFieldElement::new(11));
        prop_data_index_to_node_index(6, Some(&expected));

        expected = get_init_tvm_stack();
        expected.push(BFieldElement::new(0));
        expected.push(BFieldElement::new(12));
        prop_data_index_to_node_index(7, Some(&expected));

        expected = get_init_tvm_stack();
        expected.push(BFieldElement::new(0));
        expected.push(BFieldElement::new(16));
        prop_data_index_to_node_index(8, Some(&expected));

        expected = get_init_tvm_stack();
        expected.push(BFieldElement::new(0));
        expected.push(BFieldElement::new(17));
        prop_data_index_to_node_index(9, Some(&expected));

        expected = get_init_tvm_stack();
        expected.push(BFieldElement::new(0));
        expected.push(BFieldElement::new(19));
        prop_data_index_to_node_index(10, Some(&expected));

        expected = get_init_tvm_stack();
        expected.push(BFieldElement::new(0));
        expected.push(BFieldElement::new(20));
        prop_data_index_to_node_index(11, Some(&expected));

        expected = get_init_tvm_stack();
        expected.push(BFieldElement::new(0));
        expected.push(BFieldElement::new(23));
        prop_data_index_to_node_index(12, Some(&expected));

        expected = get_init_tvm_stack();
        expected.push(BFieldElement::new(0));
        expected.push(BFieldElement::new(24));
        prop_data_index_to_node_index(13, Some(&expected));

        expected = get_init_tvm_stack();
        expected.push(BFieldElement::new(0));
        expected.push(BFieldElement::new(199_999_989));
        prop_data_index_to_node_index(100_000_000, Some(&expected));

        expected = get_init_tvm_stack();
        expected.push(BFieldElement::new(1));
        expected.push(BFieldElement::new(4_294_967_263));
        prop_data_index_to_node_index(u32::MAX as u64, Some(&expected));

        expected = get_init_tvm_stack();
        expected.push(BFieldElement::new(16777216));
        expected.push(BFieldElement::new(0));
        prop_data_index_to_node_index(1u64 << 55, Some(&expected));

        expected = get_init_tvm_stack();
        expected.push(BFieldElement::new(2147483648));
        expected.push(BFieldElement::new(0));
        prop_data_index_to_node_index(1u64 << 62, Some(&expected));

        expected = get_init_tvm_stack();
        expected.push(BFieldElement::new(2147483648));
        expected.push(BFieldElement::new(38));
        prop_data_index_to_node_index((1u64 << 62) + 20, Some(&expected));

        expected = get_init_tvm_stack();
        expected.push(BFieldElement::new(2147483648));
        expected.push(BFieldElement::new(797));
        prop_data_index_to_node_index((1u64 << 62) + 400, Some(&expected));

        expected = get_init_tvm_stack();
        expected.push(BFieldElement::new(2147483647));
        expected.push(BFieldElement::new(4294857690));
        prop_data_index_to_node_index((1u64 << 62) - 54778, Some(&expected));

        println!("2^32");
        prop_data_index_to_node_index((1u64 << 32) - 1, None);
        prop_data_index_to_node_index(1u64 << 32, None);
        prop_data_index_to_node_index((1u64 << 32) + 1, None);

        println!("2^33");
        prop_data_index_to_node_index((1u64 << 33) - 1, None);
        prop_data_index_to_node_index(1u64 << 33, None);
        prop_data_index_to_node_index((1u64 << 33) + 1, None);
    }

    #[test]
    fn data_index_to_node_index_pbt() {
        let mut rng = thread_rng();
        for _ in 0..20 {
            let rand = rng.next_u64() / 2;
            prop_data_index_to_node_index(rand, None);
        }
    }

    fn prop_data_index_to_node_index(data_index: u64, expected: Option<&[BFieldElement]>) {
        let mut init_stack = get_init_tvm_stack();
        let value_as_u32_2 = U32s::new([
            (data_index & 0xFFFFFFFFu32 as u64) as u32,
            (data_index >> 32) as u32,
        ]);
        for elem in value_as_u32_2.encode().into_iter().rev() {
            init_stack.push(elem);
        }

        let _execution_result = rust_tasm_equivalence_prop::<DataIndexToNodeIndex>(
            &DataIndexToNodeIndex,
            &init_stack,
            &[],
            &[],
            &mut HashMap::default(),
            0,
            expected,
        );
    }
}
