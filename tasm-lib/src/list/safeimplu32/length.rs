use std::collections::HashMap;

use itertools::Itertools;
use num::One;
use rand::random;
use rand::thread_rng;
use rand::Rng;
use triton_vm::prelude::*;

use crate::data_type::DataType;
use crate::empty_stack;
use crate::library::Library;
use crate::rust_shadowing_helper_functions::safe_list::safe_insert_random_list;
use crate::traits::deprecated_snippet::DeprecatedSnippet;
use crate::ExecutionState;

#[derive(Clone, Debug)]
pub struct Length {
    pub data_type: DataType,
}

impl DeprecatedSnippet for Length {
    fn entrypoint_name(&self) -> String {
        format!(
            "tasm_list_safeimplu32_length___{}",
            self.data_type.label_friendly_name()
        )
    }

    fn input_field_names(&self) -> Vec<String> {
        vec!["*list".to_string()]
    }

    fn input_types(&self) -> Vec<DataType> {
        vec![DataType::List(Box::new(self.data_type.clone()))]
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

    fn function_code(&self, _library: &mut Library) -> String {
        // BEFORE: _ *list
        // AFTER:  _ list_length_u32
        triton_asm!(
            {self.entrypoint_name()}:
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

    fn gen_input_states(&self) -> Vec<ExecutionState> {
        let mut ret: Vec<ExecutionState> = vec![];
        let mut rng = thread_rng();
        let mut stack = empty_stack();
        let list_pointer: BFieldElement = random();
        let capacity: u32 = 100;
        let list_length: usize = rng.gen_range(0..=capacity as usize);
        stack.push(list_pointer);

        let mut memory = HashMap::default();
        safe_insert_random_list(
            &self.data_type,
            list_pointer,
            capacity,
            list_length,
            &mut memory,
        );
        ret.push(ExecutionState::with_stack_and_memory(
            stack.clone(),
            memory,
            0,
        ));
        memory = HashMap::default();
        safe_insert_random_list(
            &self.data_type,
            list_pointer,
            capacity,
            list_length,
            &mut memory,
        );
        ret.push(ExecutionState::with_stack_and_memory(
            stack.clone(),
            memory,
            0,
        ));
        memory = HashMap::default();
        safe_insert_random_list(
            &self.data_type,
            list_pointer,
            capacity,
            list_length,
            &mut memory,
        );
        ret.push(ExecutionState::with_stack_and_memory(
            stack.clone(),
            memory,
            0,
        ));
        memory = HashMap::default();
        safe_insert_random_list(
            &self.data_type,
            list_pointer,
            capacity,
            list_length,
            &mut memory,
        );
        ret.push(ExecutionState::with_stack_and_memory(
            stack.clone(),
            memory,
            0,
        ));
        memory = HashMap::default();
        safe_insert_random_list(
            &self.data_type,
            list_pointer,
            capacity,
            list_length,
            &mut memory,
        );
        ret.push(ExecutionState::with_stack_and_memory(stack, memory, 0));

        ret
    }

    fn common_case_input_state(&self) -> ExecutionState {
        const COMMON_LENGTH: usize = 1 << 5;
        get_benchmark_input_state(COMMON_LENGTH, &self.data_type)
    }

    fn worst_case_input_state(&self) -> ExecutionState {
        const COMMON_LENGTH: usize = 1 << 6;
        get_benchmark_input_state(COMMON_LENGTH, &self.data_type)
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

fn get_benchmark_input_state(list_length: usize, data_type: &DataType) -> ExecutionState {
    let mut memory = HashMap::default();
    let list_pointer: BFieldElement = BFieldElement::one();

    let capacity = list_length * 2;
    safe_insert_random_list(
        data_type,
        list_pointer,
        capacity as u32,
        list_length,
        &mut memory,
    );

    let mut stack = empty_stack();
    stack.push(list_pointer);

    let nondeterminism = NonDeterminism::default().with_ram(memory);
    ExecutionState {
        stack,
        std_in: vec![],
        nondeterminism,
        words_allocated: 1,
    }
}

#[cfg(test)]
mod tests {
    use num::One;
    use BFieldElement;

    use crate::empty_stack;
    use crate::test_helpers::{
        test_rust_equivalence_given_input_values_deprecated,
        test_rust_equivalence_multiple_deprecated,
    };

    use super::*;

    #[test]
    fn new_snippet_test_long() {
        fn test_rust_equivalence_and_export(data_type: DataType) {
            test_rust_equivalence_multiple_deprecated(&Length { data_type }, true);
        }

        test_rust_equivalence_and_export(DataType::Bool);
        test_rust_equivalence_and_export(DataType::U32);
        test_rust_equivalence_and_export(DataType::U64);
        test_rust_equivalence_and_export(DataType::Bfe);
        test_rust_equivalence_and_export(DataType::Xfe);
        test_rust_equivalence_and_export(DataType::Digest);
    }

    #[test]
    fn list_u32_simple() {
        let expected_end_stack = [empty_stack(), vec![BFieldElement::new(42)]].concat();
        prop_length(DataType::U64, 42, &expected_end_stack);

        let expected_end_stack = [empty_stack(), vec![BFieldElement::new(588)]].concat();
        prop_length(DataType::Xfe, 588, &expected_end_stack);

        let expected_end_stack = [empty_stack(), vec![BFieldElement::new(4)]].concat();
        prop_length(DataType::Digest, 4, &expected_end_stack);

        let expected_end_stack = [empty_stack(), vec![BFieldElement::new(7)]].concat();
        prop_length(DataType::U32, 7, &expected_end_stack);
    }

    // Note that the *actual list* of length `list_length` is *actually* constructed in the VM in
    // this test. So you may not want to exaggerate that number.
    fn prop_length(element_type: DataType, list_length: usize, expected_stack: &[BFieldElement]) {
        let list_pointer = BFieldElement::one();
        let capacity = 1000;
        let mut init_stack = empty_stack();
        init_stack.push(list_pointer);

        let mut memory = HashMap::default();

        safe_insert_random_list(
            &element_type,
            list_pointer,
            capacity,
            list_length,
            &mut memory,
        );

        let data_type = DataType::Bfe;
        test_rust_equivalence_given_input_values_deprecated(
            &Length { data_type },
            &init_stack,
            &[],
            memory,
            0,
            Some(expected_stack),
        );
    }
}

#[cfg(test)]
mod benches {
    use crate::snippet_bencher::bench_and_write;

    use super::*;

    #[test]
    fn safe_length_benchmark() {
        let data_type = DataType::Digest;
        bench_and_write(Length { data_type });
    }
}
