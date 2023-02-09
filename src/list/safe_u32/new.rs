use num::Zero;
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

                dup0
                push {element_size}
                mul
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
        let list_pointer = BFieldElement::zero();
        safe_list_new(list_pointer, capacity as u32, memory);
        stack.push(list_pointer);
    }
}

#[cfg(test)]
mod tests {
    use crate::test_helpers::rust_tasm_equivalence_prop_new;

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
}
