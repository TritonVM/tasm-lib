use std::collections::HashMap;

use itertools::Itertools;
use num::One;
use rand::prelude::*;
use triton_vm::prelude::*;

use crate::empty_stack;
use crate::prelude::*;
use crate::rust_shadowing_helper_functions::list::untyped_insert_random_list;
use crate::traits::deprecated_snippet::DeprecatedSnippet;
use crate::InitVmState;

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct Length {
    pub element_type: DataType,
}

impl Length {
    pub fn new(data_type: DataType) -> Self {
        Self {
            element_type: data_type,
        }
    }
}

impl DeprecatedSnippet for Length {
    fn entrypoint_name(&self) -> String {
        format!(
            "tasmlib_list_length___{}",
            self.element_type.label_friendly_name()
        )
    }

    fn input_field_names(&self) -> Vec<String> {
        vec!["*list".to_string()]
    }

    fn input_types(&self) -> Vec<DataType> {
        vec![DataType::List(Box::new(self.element_type.clone()))]
    }

    fn output_field_names(&self) -> Vec<String> {
        vec!["list_length".to_string()]
    }

    fn output_types(&self) -> Vec<DataType> {
        vec![DataType::U32]
    }

    fn stack_diff(&self) -> isize {
        // Consumes a memory address and returns a length in the form of a u32
        0
    }

    fn function_code(&self, _: &mut Library) -> String {
        let entry_point = self.entrypoint_name();
        // BEFORE: _ *list
        // AFTER:  _ list_length_u32
        triton_asm!(
            {entry_point}:
                read_mem 1
                pop 1
                return
        )
        .iter()
        .join("\n")
    }

    fn crash_conditions(&self) -> Vec<String> {
        vec![]
    }

    fn gen_input_states(&self) -> Vec<InitVmState> {
        let mut ret: Vec<InitVmState> = vec![];
        let mut rng = thread_rng();
        let mut stack = empty_stack();
        let list_address: BFieldElement = random();
        let list_length: usize = rng.gen_range(0..100);
        stack.push(list_address);

        // Test for various values of `N` (list-element size)
        let mut memory = HashMap::default();
        untyped_insert_random_list(list_address, list_length, &mut memory, 1);
        ret.push(InitVmState::with_stack_and_memory(stack.clone(), memory));
        memory = HashMap::default();
        untyped_insert_random_list(list_address, list_length, &mut memory, 2);
        ret.push(InitVmState::with_stack_and_memory(stack.clone(), memory));
        memory = HashMap::default();
        untyped_insert_random_list(list_address, list_length, &mut memory, 3);
        ret.push(InitVmState::with_stack_and_memory(stack.clone(), memory));
        memory = HashMap::default();
        untyped_insert_random_list(list_address, list_length, &mut memory, 4);
        ret.push(InitVmState::with_stack_and_memory(stack.clone(), memory));
        memory = HashMap::default();
        untyped_insert_random_list(list_address, list_length, &mut memory, 11);
        ret.push(InitVmState::with_stack_and_memory(stack, memory));

        ret
    }

    fn common_case_input_state(&self) -> InitVmState {
        let mut stack = empty_stack();
        let list_address: u32 = random();
        let list_address = BFieldElement::from(list_address as u64);
        stack.push(list_address);
        let mut memory = HashMap::default();
        untyped_insert_random_list(BFieldElement::one(), 1 << 5, &mut memory, 1);
        InitVmState::with_stack_and_memory(stack.clone(), memory)
    }

    fn worst_case_input_state(&self) -> InitVmState {
        let mut stack = empty_stack();
        let list_address: u32 = random();
        let list_address = BFieldElement::from(list_address as u64);
        stack.push(list_address);
        let mut memory = HashMap::default();
        untyped_insert_random_list(BFieldElement::one(), 1 << 6, &mut memory, 1);
        InitVmState::with_stack_and_memory(stack.clone(), memory)
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
}

#[cfg(test)]
mod tests {
    use rand::prelude::*;

    use super::*;
    use crate::test_helpers::test_rust_equivalence_given_input_values_deprecated;
    use crate::test_helpers::test_rust_equivalence_multiple_deprecated;

    #[test]
    fn new_snippet_test_long() {
        fn test_rust_equivalence_and_export(data_type: DataType) {
            test_rust_equivalence_multiple_deprecated(
                &Length {
                    element_type: data_type,
                },
                true,
            );
        }

        test_rust_equivalence_and_export(DataType::Bfe);
        test_rust_equivalence_and_export(DataType::U64);
        test_rust_equivalence_and_export(DataType::Digest);
    }

    #[test]
    fn list_u32_simple_long() {
        let expected_end_stack = [empty_stack(), vec![BFieldElement::new(42)]].concat();
        prop_length_long(BFieldElement::one(), 42, Some(&expected_end_stack));

        let expected_end_stack = [empty_stack(), vec![BFieldElement::new(588)]].concat();
        prop_length_long(BFieldElement::one(), 588, Some(&expected_end_stack));
    }

    // Note that the *actual list* of length `list_length` is *actually constructed in the VM in this test. So you may not
    // want to exaggerate that number.
    fn prop_length_long(
        list_address: BFieldElement,
        list_length: u32,
        expected: Option<&[BFieldElement]>,
    ) {
        let mut init_stack = empty_stack();
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

        let data_type = DataType::Bfe;
        test_rust_equivalence_given_input_values_deprecated(
            &Length {
                element_type: data_type,
            },
            &init_stack,
            &[],
            init_memory,
            expected,
        );
    }
}

#[cfg(test)]
mod benches {
    use super::*;
    use crate::snippet_bencher::bench_and_write;

    #[test]
    fn length_long_benchmark() {
        bench_and_write(Length::new(DataType::Digest));
    }
}
