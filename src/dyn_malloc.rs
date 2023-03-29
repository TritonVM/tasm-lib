use std::collections::HashMap;

use num::{One, Zero};
use rand::Rng;
use twenty_first::shared_math::b_field_element::{BFieldElement, BFIELD_ZERO};

pub const DYN_MALLOC_ADDRESS: u32 = 0;

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
        vec![DataType::U32]
    }

    fn output_types(&self) -> Vec<DataType> {
        vec![DataType::U32]
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

    fn function_body(&self, _library: &mut Library) -> String {
        let entrypoint = self.entrypoint();
        format!(
            "
            // Return a pointer to a free address and allocate `size` words for this pointer

            // Before: _ size
            // After: _ *next_addr
            {entrypoint}:
                push {DYN_MALLOC_ADDRESS}  // _ size *free_pointer
                read_mem                   // _ size *free_pointer *next_addr'

                // add 1 iff `next_addr` was 0, i.e. uninitialized.
                dup 0                      // _ size *free_pointer *next_addr' *next_addr'
                push 0                     // _ size *free_pointer *next_addr' *next_addr' 0
                eq                         // _ size *free_pointer *next_addr' (*next_addr' == 0)
                add                        // _ size *free_pointer *next_addr

                dup 0                      // _ size *free_pointer *next_addr *next_addr
                dup 3                      // _ size *free_pointer *next_addr *next_addr size

                // Ensure that `size` does not exceed 2^32
                split
                swap 1
                push 0
                eq
                assert

                add                        // _ size *free_pointer *next_addr *(next_addr + size)

                // Ensure that no more than 2^32 words are allocated, because I don't want a wrap-around
                // in the address space
                split
                swap 1
                push 0
                eq
                assert

                swap 1                     // _ size *free_pointer *(next_addr + size) *next_addr
                swap 3                     // _ *next_addr *free_pointer *(next_addr + size) size
                pop                        // _ *next_addr *free_pointer *(next_addr + size)
                write_mem
                pop                        // _ next_addr
                return
            "
        )
    }

    fn crash_conditions() -> Vec<String>
    where
        Self: Sized,
    {
        vec![
            "Caller attempts to allocate more than 2^32 words".to_owned(),
            "More than 2^32 words allocated to memory".to_owned(),
        ]
    }

    fn gen_input_states(&self) -> Vec<ExecutionState>
    where
        Self: Sized,
    {
        let mut rng = rand::thread_rng();

        let mut stack = get_init_tvm_stack();
        stack.push(BFieldElement::new(rng.gen_range(0..10_000)));

        let free_pointer_addr = DYN_MALLOC_ADDRESS;
        let first_free_addr = free_pointer_addr + 1;
        let free_pointer = BFieldElement::new(rng.gen_range(first_free_addr..10_000) as u64);
        let mut memory = HashMap::<BFieldElement, BFieldElement>::new();
        memory.insert(BFieldElement::new(free_pointer_addr as u64), free_pointer);

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
            .or_insert_with(BFieldElement::one);

        let size = stack.pop().unwrap();
        assert!(size.value() < (1u64 << 32));

        let next_addr = *used_memory;

        stack.push(next_addr);
        *used_memory += size;

        assert!(used_memory.value() < (1u64 << 32));
    }

    fn common_case_input_state(&self) -> ExecutionState
    where
        Self: Sized,
    {
        let mut init_stack = get_init_tvm_stack();
        init_stack.push(BFieldElement::new(10));
        ExecutionState::with_stack(init_stack)
    }

    fn worst_case_input_state(&self) -> ExecutionState
    where
        Self: Sized,
    {
        let mut init_stack = get_init_tvm_stack();
        init_stack.push(BFieldElement::new(1 << 31));
        ExecutionState::with_stack(init_stack)
    }
}

#[cfg(test)]
mod tests {
    use crate::{snippet_bencher::bench_and_write, test_helpers::rust_tasm_equivalence_prop_new};

    use super::*;

    #[test]
    fn dyn_malloc_test() {
        rust_tasm_equivalence_prop_new(DynMalloc);
    }

    #[test]
    fn dyn_malloc_benchmark() {
        bench_and_write(DynMalloc);
    }
}
