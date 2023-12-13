use itertools::Itertools;
use num::{One, Zero};
use triton_vm::triton_asm;
use twenty_first::shared_math::b_field_element::BFieldElement;

use crate::data_type::DataType;
use crate::{
    dyn_malloc, empty_stack, rust_shadowing_helper_functions::safe_list::safe_list_new,
    snippet::DeprecatedSnippet, ExecutionState,
};

#[derive(Clone, Debug)]
pub struct SafeNew {
    pub data_type: DataType,
}

impl DeprecatedSnippet for SafeNew {
    fn entrypoint_name(&self) -> String {
        format!(
            "tasm_list_safeimplu32_new___{}",
            self.data_type.label_friendly_name()
        )
    }

    fn input_field_names(&self) -> Vec<String> {
        vec!["capacity".to_string()]
    }

    fn input_types(&self) -> Vec<DataType> {
        vec![DataType::U32]
    }

    fn output_field_names(&self) -> Vec<String> {
        vec!["*list".to_string()]
    }

    fn output_types(&self) -> Vec<DataType> {
        // List pointers are considered u32
        vec![DataType::List(Box::new(self.data_type.clone()))]
    }

    fn stack_diff(&self) -> isize {
        0
    }

    fn function_code(&self, library: &mut crate::library::Library) -> String {
        let dyn_alloc = library.import(Box::new(dyn_malloc::DynMalloc));

        let element_size = self.data_type.stack_size();
        let mul_with_size = match element_size {
            1 => vec![],
            _ => triton_asm!(push {element_size} mul),
        };

        triton_asm!(
            // BEFORE: _ capacity
            // AFTER:  _ *list
            {self.entrypoint_name()}:
                // convert capacity in number of elements to number of required VM words
                dup 0
                {&mul_with_size}        // _ capacity (capacity_in_bfes)
                push 2
                add                     // _ capacity (words to allocate)
                call {dyn_alloc}        // _ capacity *list

                // write initial length = 0 to `*list`, capacity to `*list + 1`
                push 0                  // _ capacity *list 0
                swap 1                  // _ capacity 0 *list
                write_mem 2             // _ (*list + 2)
                push -2
                add                     // _ *list
                return
        )
        .iter()
        .join("\n")
    }

    fn crash_conditions(&self) -> Vec<String> {
        vec![]
    }

    fn gen_input_states(&self) -> Vec<ExecutionState> {
        let mut stack = empty_stack();

        // push capacity to stack
        stack.push(BFieldElement::new(1000));
        vec![ExecutionState::with_stack(stack)]
    }

    fn common_case_input_state(&self) -> ExecutionState {
        ExecutionState::with_stack([empty_stack(), vec![BFieldElement::new(1 << 5)]].concat())
    }

    fn worst_case_input_state(&self) -> ExecutionState {
        ExecutionState::with_stack([empty_stack(), vec![BFieldElement::new(1 << 6)]].concat())
    }

    fn rust_shadowing(
        &self,
        stack: &mut Vec<BFieldElement>,
        _std_in: Vec<BFieldElement>,
        _secret_in: Vec<BFieldElement>,
        memory: &mut std::collections::HashMap<BFieldElement, BFieldElement>,
    ) {
        let capacity: usize = stack.pop().unwrap().value().try_into().unwrap();
        let allocator_addr = BFieldElement::zero();
        let used_memory = memory
            .entry(allocator_addr)
            .and_modify(|e| {
                *e = if e.is_zero() {
                    BFieldElement::one()
                } else {
                    *e
                }
            })
            .or_insert_with(BFieldElement::one);
        let list_pointer = *used_memory;
        *used_memory += BFieldElement::new(capacity as u64)
            * BFieldElement::new(self.data_type.stack_size() as u64)
            + BFieldElement::new(2);
        safe_list_new(list_pointer, capacity as u32, memory);
        stack.push(list_pointer);
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use rand::random;

    use crate::{
        list::safeimplu32::push::SafePush, rust_shadowing_helper_functions,
        test_helpers::test_rust_equivalence_multiple_deprecated, Digest, DIGEST_LENGTH,
    };

    use super::*;

    #[test]
    fn new_snippet_test() {
        fn test_rust_equivalence_and_export(data_type: DataType) {
            test_rust_equivalence_multiple_deprecated(&SafeNew { data_type }, true);
        }

        test_rust_equivalence_and_export(DataType::Bool);
        test_rust_equivalence_and_export(DataType::U32);
        test_rust_equivalence_and_export(DataType::U64);
        test_rust_equivalence_and_export(DataType::Bfe);
        test_rust_equivalence_and_export(DataType::Xfe);
        test_rust_equivalence_and_export(DataType::Digest);
    }

    #[test]
    fn allow_multiple_lists_in_memory() {
        let data_type = DataType::Digest;

        // Verify that one list does not overwrite another list in memory
        let capacity_as_bfe = BFieldElement::new(100);
        let mut stack = empty_stack();
        let memory = HashMap::default();
        stack.push(capacity_as_bfe);
        let memory = SafeNew {
            data_type: data_type.clone(),
        }
        .link_and_run_tasm_for_test(&mut stack, vec![], vec![], memory, None)
        .unwrap()
        .final_ram;
        let first_list = stack.pop().unwrap();

        // Prepare stack for push to 1st list
        stack.push(first_list);
        let digest1: Digest = random();
        for elem in digest1.values().iter().rev() {
            stack.push(elem.to_owned());
        }
        let memory = SafePush {
            data_type: data_type.clone(),
        }
        .link_and_run_tasm_for_test(&mut stack, vec![], vec![], memory, None)
        .unwrap()
        .final_ram;
        assert_eq!(
            empty_stack()[DIGEST_LENGTH..],
            stack[DIGEST_LENGTH..],
            "Stack must be empty after call to push, except for program hash"
        );

        // Get another list in memory
        stack.push(capacity_as_bfe);
        let memory = SafeNew {
            data_type: data_type.clone(),
        }
        .link_and_run_tasm_for_test(&mut stack, vec![], vec![], memory, None)
        .unwrap()
        .final_ram;
        let second_list = stack.pop().unwrap();

        // Verify that expected number of VM words were allocated for the first list
        assert_eq!(
            first_list
                + BFieldElement::new(2)
                + capacity_as_bfe * BFieldElement::new(data_type.stack_size() as u64),
            second_list
        );

        // Prepare stack for push to 2nd list
        stack.push(second_list);
        let digest2: Digest = random();
        for elem in digest2.values().iter().rev() {
            stack.push(elem.to_owned());
        }
        let memory = SafePush { data_type }
            .link_and_run_tasm_for_test(&mut stack, vec![], vec![], memory, None)
            .unwrap()
            .final_ram;
        assert_eq!(
            empty_stack()[DIGEST_LENGTH..],
            stack[DIGEST_LENGTH..],
            "Stack must be empty after call to push, except for program hash"
        );

        // Verify that digest1 was not overwritten by digest2 in memory
        let digest1_fetched = rust_shadowing_helper_functions::safe_list::safe_list_get(
            first_list,
            0,
            &memory,
            DataType::Digest.stack_size(),
        );
        assert_eq!(
            digest1.values().to_vec(),
            digest1_fetched,
            "Memory-fetched value must match expectation for digest 1"
        );

        let digest2_fetched = rust_shadowing_helper_functions::safe_list::safe_list_get(
            second_list,
            0,
            &memory,
            DataType::Digest.stack_size(),
        );
        assert_eq!(
            digest2.values().to_vec(),
            digest2_fetched,
            "Memory-fetched value must match expectation for digest 2"
        );

        // Because why not?
        assert_ne!(
            digest1, digest2,
            "Randomly generated digests must be different"
        );
    }
}

#[cfg(test)]
mod benches {
    use super::*;
    use crate::snippet_bencher::bench_and_write;

    #[test]
    fn safe_new_benchmark() {
        let data_type = DataType::Digest;
        bench_and_write(SafeNew { data_type });
    }
}
