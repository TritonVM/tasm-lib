use std::collections::HashMap;

use num::{One, Zero};
use rand::Rng;
use twenty_first::shared_math::b_field_element::{BFieldElement, BFIELD_ZERO};

use crate::{
    get_init_tvm_stack,
    library::Library,
    snippet::{DataType, Snippet},
    ExecutionState,
};

#[derive(Clone)]
pub struct DynMalloc;

impl Snippet for DynMalloc {
    fn entrypoint(&self) -> String {
        "dyn_malloc".to_string()
    }

    fn inputs(&self) -> Vec<String>
    where
        Self: Sized,
    {
        vec!["size".to_string()]
    }

    fn input_types(&self) -> Vec<DataType> {
        vec![DataType::BFE]
    }

    fn output_types(&self) -> Vec<DataType> {
        vec![DataType::BFE]
    }

    fn outputs(&self) -> Vec<String>
    where
        Self: Sized,
    {
        vec!["*addr".to_string()]
    }

    fn stack_diff(&self) -> isize
    where
        Self: Sized,
    {
        0
    }

    fn function_body(&self, library: &mut Library) -> String {
        let entrypoint = self.entrypoint();
        let free_pointer_addr = library.kmalloc(1);
        format!(
            "
            // Before: _ size
            // After: _ *next_addr
            {entrypoint}:
                push {free_pointer_addr}  // _ size *free_pointer
                push 0                    // _ size *free_pointer 0
                read_mem                  // _ size *free_pointer *next_addr'

                // add 1 iff `next_addr` was 0, i.e. uninitialized.
                dup0                      // _ size *free_pointer *next_addr' *next_addr'
                push 0                    // _ size *free_pointer *next_addr' *next_addr' 0
                eq                        // _ size *free_pointer *next_addr' (*next_addr' == 0)
                add                       // _ size *free_pointer *next_addr

                dup0                      // _ size *free_pointer *next_addr *next_addr
                dup3                      // _ size *free_pointer *next_addr *next_addr size
                add                       // _ size *free_pointer *next_addr *(next_addr + size)
                swap1                     // _ size *free_pointer *(next_addr + size) *next_addr
                swap3                     // _ *next_addr *free_pointer *(next_addr + size) size
                pop                       // _ *next_addr *free_pointer *(next_addr + size)
                write_mem
                pop pop                   // _ next_addr
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
        let mut rng = rand::thread_rng();

        let mut stack = get_init_tvm_stack();
        stack.push(BFieldElement::new(rng.gen_range(0..10_000)));

        // FIXME: Static allocator not in scope.
        let free_pointer_addr = BFIELD_ZERO;
        let first_free_addr = free_pointer_addr.value() + 1;
        let free_pointer = BFieldElement::new(rng.gen_range(first_free_addr..10_000));
        let mut memory = HashMap::<BFieldElement, BFieldElement>::new();
        memory.insert(free_pointer_addr, free_pointer);

        let ret: Vec<ExecutionState> = vec![
            ExecutionState::with_stack_and_memory(stack, memory, 0),
            // Add test case for empty memory. `next_addr` should return 1.
            ExecutionState::with_stack(get_init_tvm_stack()),
        ];

        ret
    }

    fn rust_shadowing(
        &self,
        stack: &mut Vec<BFieldElement>,
        _std_in: Vec<BFieldElement>,
        _secret_in: Vec<BFieldElement>,
        memory: &mut HashMap<BFieldElement, BFieldElement>,
    ) where
        Self: Sized,
    {
        // FIXME: Static allocator not in scope.
        let allocator_addr = BFIELD_ZERO;
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

        let size = stack.pop().unwrap();
        let next_addr = *used_memory;
        stack.push(next_addr);
        *used_memory += size;
    }
}

#[cfg(test)]
mod tests {
    use crate::test_helpers::rust_tasm_equivalence_prop_new;

    use super::*;

    #[test]
    fn dyn_malloc_test() {
        rust_tasm_equivalence_prop_new(DynMalloc);
    }
}
