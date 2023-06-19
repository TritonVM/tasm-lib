use std::collections::HashMap;

use num::{One, Zero};
use rand::Rng;
use triton_opcodes::{
    instruction::LabelledInstruction,
    parser::{parse, to_labelled},
};
use twenty_first::shared_math::b_field_element::{BFieldElement, BFIELD_ZERO};

pub const DYN_MALLOC_ADDRESS: u32 = 0;

use crate::{
    get_init_tvm_stack,
    snippet::{DataType, Snippet},
    snippet_state::SnippetState,
    ExecutionState,
};

#[derive(Clone, Debug)]
pub struct DynMalloc;

impl DynMalloc {
    pub fn get_initialization_code(malloc_init_value: u32) -> String {
        let mut ret = String::default();

        if malloc_init_value > 0 {
            ret.push_str(&format!("push {DYN_MALLOC_ADDRESS}\n"));
            ret.push_str(&format!("push {malloc_init_value}\n"));
            ret.push_str("write_mem\n");
            ret.push_str("pop\n");
        }

        ret
    }

    pub fn get_initialization_code_as_instructions(
        words_statically_allocated: u32,
    ) -> Vec<LabelledInstruction> {
        let code = Self::get_initialization_code(words_statically_allocated);
        to_labelled(&parse(&code).unwrap())
    }
}

impl Snippet for DynMalloc {
    fn entrypoint(&self) -> String {
        "tasm_memory_dyn_malloc".to_string()
    }

    fn inputs(&self) -> Vec<String> {
        vec!["size".to_string()]
    }

    fn input_types(&self) -> Vec<DataType> {
        vec![DataType::U32]
    }

    fn output_types(&self) -> Vec<DataType> {
        vec![DataType::U32]
    }

    fn outputs(&self) -> Vec<String> {
        vec!["*addr".to_string()]
    }

    fn stack_diff(&self) -> isize {
        0
    }

    fn function_code(&self, _library: &mut SnippetState) -> String {
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

    fn crash_conditions(&self) -> Vec<String> {
        vec![
            "Caller attempts to allocate more than 2^32 words".to_owned(),
            "More than 2^32 words allocated to memory".to_owned(),
        ]
    }

    fn gen_input_states(&self) -> Vec<ExecutionState> {
        let mut rng = rand::thread_rng();

        let mut stack = get_init_tvm_stack();
        stack.push(BFieldElement::new(rng.gen_range(0..10_000)));

        let allocation_size = rng.gen_range(0..10_000);
        let memory = HashMap::<BFieldElement, BFieldElement>::new();

        let ret: Vec<ExecutionState> = vec![
            ExecutionState::with_stack_and_memory(stack, memory, allocation_size),
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
    ) {
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

    fn common_case_input_state(&self) -> ExecutionState {
        let mut init_stack = get_init_tvm_stack();
        init_stack.push(BFieldElement::new(10));
        ExecutionState::with_stack(init_stack)
    }

    fn worst_case_input_state(&self) -> ExecutionState {
        let mut init_stack = get_init_tvm_stack();
        init_stack.push(BFieldElement::new(1 << 31));
        ExecutionState::with_stack(init_stack)
    }
}

#[cfg(test)]
mod tests {
    use crate::test_helpers::rust_tasm_equivalence_prop_new;

    use super::*;

    #[test]
    fn sane_address_chosen_for_dyn_malloc() {
        // It's probably a really bad idea to use any other value than 0.
        assert_eq!(0, DYN_MALLOC_ADDRESS);
    }

    #[test]
    fn dyn_malloc_test() {
        rust_tasm_equivalence_prop_new(&DynMalloc, true);
    }

    #[test]
    fn get_initialization_code_equivalence() {
        // Verify that code returning `Vec<LabelledInstruction>` and `String` agree
        let init_code_string = DynMalloc::get_initialization_code(4);
        let init_code_vec = DynMalloc::get_initialization_code_as_instructions(4);
        let string_parsed = to_labelled(&parse(&init_code_string).unwrap());
        assert_eq!(string_parsed, init_code_vec);
    }
}

#[cfg(test)]
mod benches {
    use super::*;
    use crate::snippet_bencher::bench_and_write;

    #[test]
    fn dyn_malloc_benchmark() {
        bench_and_write(DynMalloc);
    }
}
