use num::{One, Zero};
use twenty_first::shared_math::b_field_element::BFieldElement;

use crate::{
    dyn_malloc, get_init_tvm_stack,
    rust_shadowing_helper_functions::safe_list::safe_list_new,
    snippet::{DataType, Snippet},
    ExecutionState,
};

#[derive(Clone)]
pub struct SafeNew(pub DataType);

impl Snippet for SafeNew {
    fn entrypoint(&self) -> String {
        format!("tasm_list_safe_u32_new_{}", self.0)
    }

    fn inputs(&self) -> Vec<String>
    where
        Self: Sized,
    {
        vec!["capacity".to_string()]
    }

    fn input_types(&self) -> Vec<DataType> {
        vec![DataType::U32]
    }

    fn output_types(&self) -> Vec<DataType> {
        // List pointers are considered u32
        vec![DataType::U32]
    }

    fn outputs(&self) -> Vec<String>
    where
        Self: Sized,
    {
        vec!["*list".to_string()]
    }

    fn stack_diff(&self) -> isize
    where
        Self: Sized,
    {
        0
    }

    fn function_body(&self, library: &mut crate::library::Library) -> String {
        let entrypoint = self.entrypoint();

        // Data structure for `list::safe_u32` is: [length, capacity, element0, element1, ...]
        let element_size = self.0.get_size();
        let dyn_alloc = library.import(Box::new(dyn_malloc::DynMalloc));
        // input capacity is given in terms of `element_size`. So `element_size * capacity` words
        // need to be allocated
        format!(
            "
            {entrypoint}:
                // _ capacity

                // Convert capacity in number of elements to number of VM words required for that list
                dup0
                push {element_size}
                mul
                push 2
                add
                // _ capacity (words to allocate)

                call {dyn_alloc}
                // _ capacity *list

                // Write initial length = 0 to `*list`
                push 0
                write_mem
                // _ capacity *list 0

                // Write capactiy to memory location `*list + 1`
                pop
                push 1
                add
                // _ capacity (*list + 1)

                swap1
                write_mem
                // _ (*list + 1) capacity

                pop
                push -1
                add
                // _ *list

                return
            "
        )
    }

    fn crash_conditions() -> Vec<String>
    where
        Self: Sized,
    {
        vec![]
    }

    fn gen_input_states(&self) -> Vec<ExecutionState>
    where
        Self: Sized,
    {
        let mut stack = get_init_tvm_stack();

        // push capacity to stack
        stack.push(BFieldElement::new(1000));
        vec![ExecutionState::with_stack(stack)]
    }

    fn rust_shadowing(
        &self,
        stack: &mut Vec<BFieldElement>,
        _std_in: Vec<BFieldElement>,
        _secret_in: Vec<BFieldElement>,
        memory: &mut std::collections::HashMap<BFieldElement, BFieldElement>,
    ) where
        Self: Sized,
    {
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
            .or_insert(BFieldElement::one());
        let list_pointer = *used_memory;
        *used_memory += BFieldElement::new(capacity as u64)
            * BFieldElement::new(self.0.get_size() as u64)
            + BFieldElement::new(2);
        safe_list_new(list_pointer, capacity as u32, memory);
        stack.push(list_pointer);
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use rand::random;
    use twenty_first::shared_math::rescue_prime_digest::Digest;

    use crate::{
        list::safe_u32::push::SafePush, rust_shadowing_helper_functions,
        test_helpers::rust_tasm_equivalence_prop_new,
    };

    use super::*;

    #[test]
    fn new_snippet_test() {
        rust_tasm_equivalence_prop_new(SafeNew(DataType::Bool));
        rust_tasm_equivalence_prop_new(SafeNew(DataType::U32));
        rust_tasm_equivalence_prop_new(SafeNew(DataType::U64));
        rust_tasm_equivalence_prop_new(SafeNew(DataType::BFE));
        rust_tasm_equivalence_prop_new(SafeNew(DataType::XFE));
        rust_tasm_equivalence_prop_new(SafeNew(DataType::Digest));
    }

    #[test]
    fn allow_multiple_lists_in_memory() {
        let data_type = DataType::Digest;

        // Verify that one list does not overwrite another list in memory
        let capacity_as_bfe = BFieldElement::new(100);
        let mut stack = get_init_tvm_stack();
        let mut memory = HashMap::default();
        stack.push(capacity_as_bfe);
        SafeNew(data_type.clone()).run_tasm_old(&mut stack, vec![], vec![], &mut memory, 0);
        let first_list = stack.pop().unwrap();

        // Prepare stack for push to 1st list
        stack.push(first_list);
        let digest1: Digest = random();
        for elem in digest1.values().iter().rev() {
            stack.push(elem.to_owned());
        }
        SafePush(data_type.clone()).run_tasm_old(&mut stack, vec![], vec![], &mut memory, 0);
        assert_eq!(
            get_init_tvm_stack(),
            stack,
            "Stack must be empty after call to push"
        );

        // Get another list in memory
        stack.push(capacity_as_bfe);
        SafeNew(data_type.clone()).run_tasm_old(&mut stack, vec![], vec![], &mut memory, 0);
        let second_list = stack.pop().unwrap();

        // Verify that expected number of VM words were allocated for the first list
        assert_eq!(
            first_list
                + BFieldElement::new(2)
                + capacity_as_bfe * BFieldElement::new(data_type.get_size() as u64),
            second_list
        );

        // Prepare stack for push to 2nd list
        stack.push(second_list);
        let digest2: Digest = random();
        for elem in digest2.values().iter().rev() {
            stack.push(elem.to_owned());
        }
        SafePush(data_type.clone()).run_tasm_old(&mut stack, vec![], vec![], &mut memory, 0);
        assert_eq!(
            get_init_tvm_stack(),
            stack,
            "Stack must be empty after call to push"
        );

        // Verify that digest1 was not overwritten by digest2 in memory
        let digest1_fetched = rust_shadowing_helper_functions::safe_list::safe_list_read(
            first_list,
            0,
            &memory,
            DataType::Digest.get_size(),
        );
        assert_eq!(
            digest1.values().to_vec(),
            digest1_fetched,
            "Memory-fetched value must match expectation for digest 1"
        );

        let digest2_fetched = rust_shadowing_helper_functions::safe_list::safe_list_read(
            second_list,
            0,
            &memory,
            DataType::Digest.get_size(),
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
