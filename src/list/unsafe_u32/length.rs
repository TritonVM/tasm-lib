use std::collections::HashMap;

use rand::{random, thread_rng, Rng};
use twenty_first::shared_math::b_field_element::BFieldElement;

use crate::library::Library;
use crate::rust_shadowing_helper_functions::unsafe_insert_random_list;
use crate::snippet::{DataType, Snippet};
use crate::{get_init_tvm_stack, ExecutionState};

// Called "Long" because this logic can be shortened
#[derive(Clone)]
pub struct LengthLong(pub DataType);

impl Snippet for LengthLong {
    fn inputs() -> Vec<&'static str> {
        vec!["*list"]
    }

    fn outputs() -> Vec<&'static str> {
        vec!["list_length"]
    }

    fn input_types(&self) -> Vec<crate::snippet::DataType> {
        vec![DataType::List(Box::new(self.0.clone()))]
    }

    fn output_types(&self) -> Vec<crate::snippet::DataType> {
        vec![DataType::U32]
    }

    fn crash_conditions() -> Vec<&'static str> {
        vec![]
    }

    fn gen_input_states() -> Vec<ExecutionState> {
        let mut ret: Vec<ExecutionState> = vec![];
        let mut rng = thread_rng();
        let mut stack = get_init_tvm_stack();
        let list_address: BFieldElement = random();
        let list_length: usize = rng.gen_range(0..100);
        stack.push(list_address);

        // Test for various values of `N` (list-element size)
        let mut memory = HashMap::default();
        unsafe_insert_random_list::<1>(list_address, list_length, &mut memory);
        ret.push(ExecutionState::with_stack_and_memory(
            stack.clone(),
            memory,
            0,
        ));
        memory = HashMap::default();
        unsafe_insert_random_list::<2>(list_address, list_length, &mut memory);
        ret.push(ExecutionState::with_stack_and_memory(
            stack.clone(),
            memory,
            0,
        ));
        memory = HashMap::default();
        unsafe_insert_random_list::<3>(list_address, list_length, &mut memory);
        ret.push(ExecutionState::with_stack_and_memory(
            stack.clone(),
            memory,
            0,
        ));
        memory = HashMap::default();
        unsafe_insert_random_list::<4>(list_address, list_length, &mut memory);
        ret.push(ExecutionState::with_stack_and_memory(
            stack.clone(),
            memory,
            0,
        ));
        memory = HashMap::default();
        unsafe_insert_random_list::<11>(list_address, list_length, &mut memory);
        ret.push(ExecutionState::with_stack_and_memory(stack, memory, 0));

        ret
    }

    fn stack_diff() -> isize {
        // Consumes a memory address and returns a length in the form of a u32
        0
    }

    fn entrypoint(&self) -> &'static str {
        "list_u32_length_long"
    }

    fn function_body(&self, _library: &mut Library) -> String {
        let entry_point = self.entrypoint();
        // Before: _ *list
        // After: _ list_length_u32
        format!(
            "
            {entry_point}:
                push 0
                read_mem
                swap1
                pop
                return
                "
        )
    }

    fn rust_shadowing(
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
}

// Called "Short" because it's efficient code
#[derive(Clone)]
pub struct LengthShort(pub DataType);

impl Snippet for LengthShort {
    fn inputs() -> Vec<&'static str> {
        vec!["*list"]
    }

    fn outputs() -> Vec<&'static str> {
        vec!["*list", "list_length"]
    }

    fn input_types(&self) -> Vec<crate::snippet::DataType> {
        vec![DataType::List(Box::new(self.0.clone()))]
    }

    fn output_types(&self) -> Vec<crate::snippet::DataType> {
        vec![DataType::List(Box::new(self.0.clone())), DataType::U32]
    }

    fn crash_conditions() -> Vec<&'static str> {
        vec![]
    }

    fn gen_input_states() -> Vec<ExecutionState> {
        LengthLong::gen_input_states()
    }

    fn stack_diff() -> isize {
        // Adds the length of a vector in the form of a u32 to the top of the stack
        1
    }

    fn entrypoint(&self) -> &'static str {
        "list_u32_length_short"
    }

    fn function_body(&self, _library: &mut Library) -> String {
        let entry_point = self.entrypoint();
        // Before: _ *list
        // After: _ *list list_length_u32
        format!(
            "
            {entry_point}:
                push 0
                read_mem
                return
                "
        )
    }

    fn rust_shadowing(
        stack: &mut Vec<BFieldElement>,
        _std_in: Vec<BFieldElement>,
        _secret_in: Vec<BFieldElement>,
        memory: &mut HashMap<BFieldElement, BFieldElement>,
    ) {
        // Find the list in memory and push its length to the top of the stack
        let list_address = *stack.last().as_ref().unwrap();
        let list_length = memory[list_address];
        stack.push(list_length);
    }
}

#[cfg(test)]
mod tests_long {
    use num::One;
    use rand::{thread_rng, RngCore};
    use twenty_first::shared_math::b_field_element::BFieldElement;

    use crate::get_init_tvm_stack;
    use crate::test_helpers::{rust_tasm_equivalence_prop, rust_tasm_equivalence_prop_new};

    use super::*;

    #[test]
    fn new_snippet_test_long() {
        rust_tasm_equivalence_prop_new::<LengthLong>(LengthLong(DataType::U64));
    }

    #[test]
    fn new_snippet_test_short() {
        rust_tasm_equivalence_prop_new::<LengthShort>(LengthShort(DataType::XFE));
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

        let _execution_result = rust_tasm_equivalence_prop::<LengthLong>(
            LengthLong(DataType::BFE),
            &init_stack,
            &[],
            &[],
            &mut init_memory,
            0,
            expected,
        );
    }
}

#[cfg(test)]
mod tests_short {
    use rand::{thread_rng, RngCore};
    use twenty_first::shared_math::b_field_element::BFieldElement;

    use crate::get_init_tvm_stack;
    use crate::test_helpers::rust_tasm_equivalence_prop;

    use super::*;

    #[test]
    fn list_u32_simple_short() {
        let expected_end_stack = vec![
            get_init_tvm_stack(),
            vec![BFieldElement::new(48), BFieldElement::new(42)],
        ]
        .concat();
        prop_length_long(BFieldElement::new(48), 42, Some(&expected_end_stack));

        let expected_end_stack = vec![
            get_init_tvm_stack(),
            vec![BFieldElement::new(8888906), BFieldElement::new(588)],
        ]
        .concat();
        prop_length_long(BFieldElement::new(8888906), 588, Some(&expected_end_stack));
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

        let _execution_result = rust_tasm_equivalence_prop::<LengthShort>(
            LengthShort(DataType::Bool),
            &init_stack,
            &[],
            &[],
            &mut init_memory,
            0,
            expected,
        );
    }
}
