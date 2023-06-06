use std::collections::HashMap;

use num::One;
use rand::{random, thread_rng, Rng};
use twenty_first::shared_math::b_field_element::BFieldElement;

use crate::rust_shadowing_helper_functions::unsafe_list::unsafe_insert_random_list;
use crate::snippet::{DataType, Snippet};
use crate::snippet_state::SnippetState;
use crate::{get_init_tvm_stack, ExecutionState};

// Called "Long" because this logic can be shortened
#[derive(Clone, Debug)]
pub struct UnsafeLength(pub DataType);

impl Snippet for UnsafeLength {
    fn inputs(&self) -> Vec<String> {
        vec!["*list".to_string()]
    }

    fn outputs(&self) -> Vec<String> {
        vec!["list_length".to_string()]
    }

    fn input_types(&self) -> Vec<crate::snippet::DataType> {
        vec![DataType::List(Box::new(self.0.clone()))]
    }

    fn output_types(&self) -> Vec<crate::snippet::DataType> {
        vec![DataType::U32]
    }

    fn crash_conditions() -> Vec<String> {
        vec![]
    }

    fn gen_input_states(&self) -> Vec<ExecutionState> {
        let mut ret: Vec<ExecutionState> = vec![];
        let mut rng = thread_rng();
        let mut stack = get_init_tvm_stack();
        let list_address: BFieldElement = random();
        let list_length: usize = rng.gen_range(0..100);
        stack.push(list_address);

        // Test for various values of `N` (list-element size)
        let mut memory = HashMap::default();
        unsafe_insert_random_list(list_address, list_length, &mut memory, 1);
        ret.push(ExecutionState::with_stack_and_memory(
            stack.clone(),
            memory,
            0,
        ));
        memory = HashMap::default();
        unsafe_insert_random_list(list_address, list_length, &mut memory, 2);
        ret.push(ExecutionState::with_stack_and_memory(
            stack.clone(),
            memory,
            0,
        ));
        memory = HashMap::default();
        unsafe_insert_random_list(list_address, list_length, &mut memory, 3);
        ret.push(ExecutionState::with_stack_and_memory(
            stack.clone(),
            memory,
            0,
        ));
        memory = HashMap::default();
        unsafe_insert_random_list(list_address, list_length, &mut memory, 4);
        ret.push(ExecutionState::with_stack_and_memory(
            stack.clone(),
            memory,
            0,
        ));
        memory = HashMap::default();
        unsafe_insert_random_list(list_address, list_length, &mut memory, 11);
        ret.push(ExecutionState::with_stack_and_memory(stack, memory, 0));

        ret
    }

    fn stack_diff(&self) -> isize {
        // Consumes a memory address and returns a length in the form of a u32
        0
    }

    fn entrypoint(&self) -> String {
        format!("tasm_list_unsafe_u32_length_long_{}", self.0)
    }

    fn function_body(&self, _library: &mut SnippetState) -> String {
        let entry_point = self.entrypoint();
        // Before: _ *list
        // After: _ list_length_u32
        format!(
            "
            {entry_point}:
                read_mem
                swap 1
                pop
                return
                "
        )
    }

    fn rust_shadowing(
        &self,
        stack: &mut Vec<BFieldElement>,
        _std_in: Vec<BFieldElement>,
        _secret_in: Vec<BFieldElement>,
        memory: &mut HashMap<BFieldElement, BFieldElement>,
    ) {
        // Find the list in memory and push its length to the top of the stack
        let list_address = stack.pop().unwrap();
        let list_length = memory[&list_address];
        stack.push(list_length);
    }

    fn common_case_input_state(&self) -> ExecutionState
    where
        Self: Sized,
    {
        let mut stack = get_init_tvm_stack();
        let list_address: u32 = random();
        let list_address = BFieldElement::from(list_address as u64);
        stack.push(list_address);
        let mut memory = HashMap::default();
        unsafe_insert_random_list(BFieldElement::one(), 1 << 5, &mut memory, 1);
        ExecutionState::with_stack_and_memory(stack.clone(), memory, 0)
    }

    fn worst_case_input_state(&self) -> ExecutionState
    where
        Self: Sized,
    {
        let mut stack = get_init_tvm_stack();
        let list_address: u32 = random();
        let list_address = BFieldElement::from(list_address as u64);
        stack.push(list_address);
        let mut memory = HashMap::default();
        unsafe_insert_random_list(BFieldElement::one(), 1 << 6, &mut memory, 1);
        ExecutionState::with_stack_and_memory(stack.clone(), memory, 0)
    }
}

#[cfg(test)]
mod tests {
    use num::One;
    use rand::{thread_rng, RngCore};
    use twenty_first::shared_math::b_field_element::BFieldElement;

    use crate::get_init_tvm_stack;
    use crate::snippet_bencher::bench_and_write;
    use crate::test_helpers::{rust_tasm_equivalence_prop, rust_tasm_equivalence_prop_new};

    use super::*;

    #[test]
    fn new_snippet_test_long() {
        rust_tasm_equivalence_prop_new(UnsafeLength(DataType::BFE), true);
        rust_tasm_equivalence_prop_new(UnsafeLength(DataType::U64), true);
        rust_tasm_equivalence_prop_new(UnsafeLength(DataType::Digest), true);
    }

    #[test]
    fn unsafe_length_long_benchmark() {
        bench_and_write(UnsafeLength(DataType::Digest));
    }

    #[test]
    fn list_u32_simple_long() {
        let expected_end_stack = vec![get_init_tvm_stack(), vec![BFieldElement::new(42)]].concat();
        prop_length_long(BFieldElement::one(), 42, Some(&expected_end_stack));

        let expected_end_stack = vec![get_init_tvm_stack(), vec![BFieldElement::new(588)]].concat();
        prop_length_long(BFieldElement::one(), 588, Some(&expected_end_stack));
    }

    // Note that the *actual list* of length `list_length` is *actually constructed in the VM in this test. So you may not
    // want to exaggerate that number.
    fn prop_length_long(
        list_address: BFieldElement,
        list_length: u32,
        expected: Option<&[BFieldElement]>,
    ) {
        let mut init_stack = get_init_tvm_stack();
        init_stack.push(list_address);

        let mut init_memory = HashMap::default();

        // Insert length indicator of list, lives on offset = 0 from `list_address`
        init_memory.insert(list_address, BFieldElement::new(list_length as u64));

        // Insert random values for the elements in the list
        let mut rng = thread_rng();
        for i in 0..list_length {
            init_memory.insert(
                list_address + BFieldElement::new((i + 1) as u64),
                BFieldElement::new(rng.next_u64()),
            );
        }

        let _execution_result = rust_tasm_equivalence_prop::<UnsafeLength>(
            UnsafeLength(DataType::BFE),
            &init_stack,
            &[],
            &[],
            &mut init_memory,
            0,
            expected,
        );
    }
}
