use std::collections::HashMap;

use num::One;
use num::Zero;
use rand::prelude::StdRng;
use rand::Rng;
use rand::SeedableRng;
use triton_vm::prelude::*;

use crate::data_type::DataType;
use crate::empty_stack;
use crate::library::Library;
use crate::prelude::BasicSnippet;
use crate::snippet_bencher::BenchmarkCase;
use crate::traits::function::Function;
use crate::traits::function::FunctionInitialState;

/// The location of the dynamic allocator state in memory.
///
/// See the [memory convention][super] for details.
pub const DYN_MALLOC_ADDRESS: BFieldElement = BFieldElement::new(BFieldElement::MAX);

/// The address of the first page that can be dynamically allocated.
pub const DYN_MALLOC_FIRST_PAGE: u64 = 1;

/// The size of one dynamically allocated page.
pub const DYN_MALLOC_PAGE_SIZE: u64 = 1 << 32;

pub const DYN_MALLOC_FIRST_ADDRESS: BFieldElement =
    BFieldElement::new(DYN_MALLOC_FIRST_PAGE * DYN_MALLOC_PAGE_SIZE);

/// Return a pointer to the next free page of memory.
#[derive(Debug, Default, Copy, Clone, Eq, PartialEq)]
pub struct DynMalloc;

impl BasicSnippet for DynMalloc {
    fn inputs(&self) -> Vec<(DataType, String)> {
        vec![]
    }

    fn outputs(&self) -> Vec<(DataType, String)> {
        vec![(DataType::Bfe, "*addr".to_string())]
    }

    fn entrypoint(&self) -> String {
        "tasm_memory_dyn_malloc".to_string()
    }

    fn code(&self, _: &mut Library) -> Vec<LabelledInstruction> {
        let entrypoint = self.entrypoint();
        let dyn_malloc_init = format!("{entrypoint}_initialize");

        triton_asm! {
        // BEFORE: _
        // AFTER:  _ *addr
        {entrypoint}:
            push {DYN_MALLOC_ADDRESS}       // _ *dyn_malloc_state
            hint dyn_malloc_state: Pointer = stack[0]
            read_mem 1 pop 1                // _ page_idx

            dup 0 push 0 eq                 // _ page_idx (page_idx == 0)
            skiz call {dyn_malloc_init}     // _ page_idx

            // update dynamic allocator state
            dup 0                           // _ page_idx page_idx
            push 1                          // _ page_idx page_idx 1
            add                             // _ page_idx next_page_idx
            hint dyn_malloc_state: MemoryPageIdx = stack[0]
            push {DYN_MALLOC_ADDRESS}       // _ page_idx next_page_idx *dyn_malloc_state
            write_mem 1 pop 1               // _ page_idx

            // translate page number to address
            push {DYN_MALLOC_PAGE_SIZE}     // _ page_idx page_size
            hint page_size = stack[0]
            mul                             // _ *free_page
            hint free_page_address: Pointer = stack[0]
            return

        // BEFORE: _ 0
        // AFTER:  _ DYN_MALLOC_FIRST_PAGE
        {dyn_malloc_init}:
            pop 1
            push {DYN_MALLOC_FIRST_PAGE}
            return
        }
    }
}

impl Function for DynMalloc {
    fn rust_shadow(
        &self,
        stack: &mut Vec<BFieldElement>,
        memory: &mut HashMap<BFieldElement, BFieldElement>,
    ) {
        let mut page_idx = memory.get(&DYN_MALLOC_ADDRESS).copied().unwrap_or_default();
        if page_idx.is_zero() {
            page_idx = DYN_MALLOC_FIRST_PAGE.into();
        }
        let page_idx = page_idx;

        let next_page_idx = page_idx + BFieldElement::one();
        memory.insert(DYN_MALLOC_ADDRESS, next_page_idx);

        let page_address = page_idx * BFieldElement::new(DYN_MALLOC_PAGE_SIZE);
        stack.push(page_address);
    }

    fn pseudorandom_initial_state(
        &self,
        seed: [u8; 32],
        _: Option<BenchmarkCase>,
    ) -> FunctionInitialState {
        let mut rng = StdRng::from_seed(seed);

        let stack = empty_stack();

        let mut memory = HashMap::new();
        let page_number = rng.gen_range(0..(1u64 << 32));
        memory.insert(DYN_MALLOC_ADDRESS, page_number.into());

        FunctionInitialState { stack, memory }
    }

    fn corner_case_initial_states(&self) -> Vec<FunctionInitialState> {
        let stack = empty_stack();
        let memory = HashMap::new();
        vec![FunctionInitialState { stack, memory }]
    }
}

#[cfg(test)]
mod tests {
    use test_strategy::proptest;

    use crate::traits::function::ShadowedFunction;
    use crate::traits::rust_shadow::RustShadow;

    use super::*;

    #[test]
    fn expected_address_chosen_for_dyn_malloc() {
        let negative_one = -BFieldElement::one();
        assert_eq!(negative_one, DYN_MALLOC_ADDRESS);
    }

    #[test]
    fn dynamic_allocator_behaves_like_rust_shadowing() {
        ShadowedFunction::new(DynMalloc).test();
    }

    #[derive(Debug, Default, Copy, Clone, Eq, PartialEq)]
    struct MultipleDynMallocCalls {
        num_calls: usize,
    }

    impl BasicSnippet for MultipleDynMallocCalls {
        fn inputs(&self) -> Vec<(DataType, String)> {
            vec![]
        }

        fn outputs(&self) -> Vec<(DataType, String)> {
            vec![(DataType::Bfe, "*addr".to_string()); self.num_calls]
        }

        fn entrypoint(&self) -> String {
            "tasm_memory_dyn_malloc_multiple_calls".to_string()
        }

        fn code(&self, lib: &mut Library) -> Vec<LabelledInstruction> {
            let dyn_malloc = lib.import(Box::new(DynMalloc));

            let single_call = triton_asm!(call { dyn_malloc });
            let multiple_calls = vec![single_call; self.num_calls].concat();

            triton_asm!( {self.entrypoint()}: {&multiple_calls} return )
        }
    }

    impl Function for MultipleDynMallocCalls {
        fn rust_shadow(
            &self,
            stack: &mut Vec<BFieldElement>,
            memory: &mut HashMap<BFieldElement, BFieldElement>,
        ) {
            for _ in 0..self.num_calls {
                DynMalloc.rust_shadow(stack, memory);
            }
        }

        fn pseudorandom_initial_state(
            &self,
            seed: [u8; 32],
            _: Option<BenchmarkCase>,
        ) -> FunctionInitialState {
            DynMalloc.pseudorandom_initial_state(seed, None)
        }
    }

    #[proptest(cases = 20)]
    fn dynamic_allocator_can_be_called_multiple_times(
        #[strategy(0_usize..1_000)] num_calls: usize,
    ) {
        let multiple_calls = MultipleDynMallocCalls { num_calls };
        ShadowedFunction::new(multiple_calls).test();
    }
}

#[cfg(test)]
mod benches {
    use crate::traits::function::ShadowedFunction;
    use crate::traits::rust_shadow::RustShadow;

    use super::*;

    #[test]
    fn dyn_malloc_benchmark() {
        ShadowedFunction::new(DynMalloc).bench();
    }
}
