use itertools::Itertools;
use std::collections::HashMap;

use num::{One, Zero};
use rand::Rng;
use triton_vm::instruction::LabelledInstruction;
use triton_vm::triton_asm;
use twenty_first::shared_math::b_field_element::BFieldElement;

pub const DYN_MALLOC_ADDRESS: BFieldElement = BFieldElement::new(1 << 32);

use crate::data_type::DataType;
use crate::{empty_stack, library::Library, snippet::DeprecatedSnippet, ExecutionState};

#[derive(Clone, Debug)]
pub struct DynMalloc;

impl DynMalloc {
    pub fn get_initialization_code(malloc_init_value: u32) -> Vec<LabelledInstruction> {
        triton_asm!(push {malloc_init_value} push {DYN_MALLOC_ADDRESS} write_mem 1 pop 1)
    }
}

impl DeprecatedSnippet for DynMalloc {
    fn entrypoint_name(&self) -> String {
        "tasm_memory_dyn_malloc".to_string()
    }

    fn input_field_names(&self) -> Vec<String> {
        vec!["size".to_string()]
    }

    fn input_types(&self) -> Vec<DataType> {
        vec![DataType::U32]
    }

    fn output_field_names(&self) -> Vec<String> {
        vec!["*addr".to_string()]
    }

    fn output_types(&self) -> Vec<DataType> {
        vec![DataType::U32]
    }

    fn stack_diff(&self) -> isize {
        0
    }

    fn function_code(&self, _library: &mut Library) -> String {
        let entrypoint = self.entrypoint_name();
        let first_dynamically_allocated_address: BFieldElement =
            DYN_MALLOC_ADDRESS + BFieldElement::new(1);
        triton_asm!(
        // Return a pointer to a free address and allocate `size` words for this pointer

        // BEFORE: _ size
        // AFTER:  _ *next_addr
        {entrypoint}:
            push {DYN_MALLOC_ADDRESS}                  // _ size *dyn_malloc_state
            read_mem 1                                 // _ size *next_addr' *dyn_malloc_state-1
            pop 1                                      // _ size *next_addr'

            dup 0                                      // _ size *next_addr' *next_addr'
            push 0                                     // _ size *next_addr' *next_addr' 0
            eq                                         // _ size *next_addr' (*next_addr' == 0)
            push {first_dynamically_allocated_address}
            mul                                        // _ size *next_addr' (*next_addr' == 0) * (2^{32} + 1)
            add                                        // _ size *next_addr

            dup 0                                      // _ size *next_addr *next_addr
            swap 2                                     // _ *next_addr *next_addr size

            // Ensure that `size` does not exceed 2^32
            split
            swap 1
            push 0
            eq
            assert
            add                         // _ *next_addr *(next_addr + size)

            // ensure that all dynamic allocations take place in the $(2^{32}, 2^{33})$ range.
            dup 0
            split
            pop 1
            assert
            // _ *next_addr *(next_addr + size)

            // write the address of unallocated memory back to memory
            push {DYN_MALLOC_ADDRESS}   // _ *next_addr *(next_addr + size) *dyn_malloc_state
            write_mem 1                 // _ *next_addr (*dyn_malloc_state+1)
            pop 1                       // _ *next_addr
            return
        )
        .iter()
        .join("\n")
    }

    fn crash_conditions(&self) -> Vec<String> {
        vec![
            "Caller attempts to allocate more than 2^32 words".to_owned(),
            "More than 2^32 words allocated to memory".to_owned(),
        ]
    }

    fn gen_input_states(&self) -> Vec<ExecutionState> {
        let mut rng = rand::thread_rng();

        let mut stack = empty_stack();
        stack.push(BFieldElement::new(rng.gen_range(0..10_000)));

        let static_allocation_size = rng.gen_range(0..10_000);
        let memory = HashMap::<BFieldElement, BFieldElement>::new();

        let ret: Vec<ExecutionState> = vec![
            ExecutionState::with_stack_and_memory(stack, memory, static_allocation_size),
            ExecutionState::with_stack(empty_stack()),
        ];

        ret
    }

    fn common_case_input_state(&self) -> ExecutionState {
        let mut init_stack = empty_stack();
        init_stack.push(BFieldElement::new(10));
        ExecutionState::with_stack(init_stack)
    }

    fn worst_case_input_state(&self) -> ExecutionState {
        let mut init_stack = empty_stack();
        init_stack.push(BFieldElement::new(1 << 31));
        ExecutionState::with_stack(init_stack)
    }

    fn rust_shadowing(
        &self,
        stack: &mut Vec<BFieldElement>,
        _std_in: Vec<BFieldElement>,
        _secret_in: Vec<BFieldElement>,
        memory: &mut HashMap<BFieldElement, BFieldElement>,
    ) {
        let size = stack.pop().unwrap();
        assert!(size.value() < (1u64 << 32));

        let used_memory = memory
            .entry(DYN_MALLOC_ADDRESS)
            .and_modify(|e| {
                *e = if e.is_zero() {
                    DYN_MALLOC_ADDRESS + BFieldElement::one()
                } else {
                    *e
                }
            })
            .or_insert_with(|| DYN_MALLOC_ADDRESS + BFieldElement::one());

        let next_addr = *used_memory;

        stack.push(next_addr);
        *used_memory += size;

        assert!(
            used_memory.value() > (1u64 << 32),
            "New dyn malloc state is {used_memory}"
        );
        assert!(
            used_memory.value() < (1u64 << 33),
            "New dyn malloc state is {used_memory}"
        );
    }
}

#[cfg(test)]
mod tests {
    use crate::test_helpers::test_rust_equivalence_multiple_deprecated;

    use super::*;

    #[test]
    fn expected_address_chosen_for_dyn_malloc() {
        assert_eq!(1 << 32, DYN_MALLOC_ADDRESS.value());
    }

    #[test]
    fn dyn_malloc_test() {
        test_rust_equivalence_multiple_deprecated(&DynMalloc, true);
    }

    #[test]
    fn unit_test() {
        let mut init_stack = empty_stack();
        init_stack.push(BFieldElement::new(10));
        let mut empty_memory_state = ExecutionState::with_stack(init_stack.clone());
        let mut final_state_1 =
            DynMalloc.link_and_run_tasm_from_state_for_test(&mut empty_memory_state);
        assert_eq!(
            DYN_MALLOC_ADDRESS + BFieldElement::one(),
            final_state_1.final_stack.pop().unwrap()
        );

        let mut non_empty_memory_state =
            ExecutionState::with_stack_and_memory(init_stack, final_state_1.final_ram, 0);
        let mut final_state_2 =
            DynMalloc.link_and_run_tasm_from_state_for_test(&mut non_empty_memory_state);
        assert_eq!(
            DYN_MALLOC_ADDRESS + BFieldElement::new(11),
            final_state_2.final_stack.pop().unwrap()
        );
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
