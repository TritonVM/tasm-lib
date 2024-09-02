use std::collections::HashMap;

use num::One;
use num::Zero;
use rand::prelude::*;
use triton_vm::air::memory_layout::MemoryRegion;
use triton_vm::prelude::*;

use crate::data_type::DataType;
use crate::empty_stack;
use crate::library::Library;
use crate::prelude::BasicSnippet;
use crate::snippet_bencher::BenchmarkCase;
use crate::traits::function::Function;
use crate::traits::function::FunctionInitialState;

use super::NUM_PAGES_FOR_DYN_MALLOC;

/// The location of the dynamic allocator state in memory.
///
/// See the [memory convention][super] for details.
pub const DYN_MALLOC_ADDRESS: BFieldElement = BFieldElement::new(BFieldElement::MAX);

/// Initial counter-value if the dynamic allocator state is not initialized.
pub const DYN_MALLOC_INIT_COUNTER: u64 = 1;

/// Constant that adds extra space between each allocated memory page. Since no
/// program needs all available [NUM_PAGES_FOR_DYN_MALLOC], only every N pages
/// need to be available for the dynamic allocator. If this constant is set to
/// 1, then all pages available for dynamic allocation can be returned.
pub const INTERSPERCE_PARAMETER: u64 = 1 << 3;

/// The number of different pages this snippet can allocate. If the state of
/// the dynamic allocator is initialized to zero, this is also the number of
/// times this memory allocator can be called before crashing due to running
/// out of memory.
pub const NUM_ALLOCATABLE_PAGES: u64 = NUM_PAGES_FOR_DYN_MALLOC / INTERSPERCE_PARAMETER;

/// The size of one dynamically allocated page.
pub const DYN_MALLOC_PAGE_SIZE: u64 = 1 << 32;

/// The number of words seperating each subsequent page allocated by this
/// snippet.
pub const NUM_WORDS_BETWEEN_ALLOCATED_PAGES: u64 = INTERSPERCE_PARAMETER * DYN_MALLOC_PAGE_SIZE;

/// The address of the first page that can be dynamically allocated.
pub const DYN_MALLOC_FIRST_ADDRESS: BFieldElement =
    BFieldElement::new(DYN_MALLOC_INIT_COUNTER * NUM_WORDS_BETWEEN_ALLOCATED_PAGES);

/// Return a pointer to the next free page of memory. Updates the dyn malloc state
/// accordingly
#[derive(Debug, Default, Copy, Clone, Eq, PartialEq)]
pub struct DynMalloc;

impl DynMalloc {
    pub fn memory_region() -> MemoryRegion {
        MemoryRegion::new(
            DYN_MALLOC_FIRST_ADDRESS,
            (NUM_PAGES_FOR_DYN_MALLOC * DYN_MALLOC_PAGE_SIZE)
                .try_into()
                .unwrap(),
        )
    }
}

impl BasicSnippet for DynMalloc {
    fn inputs(&self) -> Vec<(DataType, String)> {
        vec![]
    }

    fn outputs(&self) -> Vec<(DataType, String)> {
        vec![(DataType::Bfe, "*addr".to_string())]
    }

    fn entrypoint(&self) -> String {
        "tasmlib_memory_dyn_malloc".to_string()
    }

    fn code(&self, _: &mut Library) -> Vec<LabelledInstruction> {
        let entrypoint = self.entrypoint();
        let dyn_malloc_init = format!("{entrypoint}_initialize");

        triton_asm! {
        // BEFORE: _
        // AFTER:  _ *addr
        {entrypoint}:
            push {DYN_MALLOC_ADDRESS}       // _ *dyn_malloc_state
            read_mem 1 pop 1                // _ page_idx

            dup 0 push 0 eq                 // _ page_idx (page_idx == 0)
            skiz call {dyn_malloc_init}     // _ page_idx

            // Verify that we are mapping inside allowed region
            push {NUM_ALLOCATABLE_PAGES}
            dup 1
            lt                              // _ page_idx (page_idx < NUM_ALLOCATABLE_PAGES)
            assert

            // update dynamic allocator state
            dup 0                           // _ page_idx page_idx
            push 1                          // _ page_idx page_idx 1
            add                             // _ page_idx next_page_idx
            push {DYN_MALLOC_ADDRESS}       // _ page_idx next_page_idx *dyn_malloc_state
            write_mem 1 pop 1               // _ page_idx

            // translate page number to address
            push {NUM_WORDS_BETWEEN_ALLOCATED_PAGES}
            mul
            // _ *free_page

            return

        // BEFORE: _ 0
        // AFTER:  _ DYN_MALLOC_FIRST_PAGE
        {dyn_malloc_init}:
            pop 1
            push {DYN_MALLOC_INIT_COUNTER}
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
            page_idx = DYN_MALLOC_INIT_COUNTER.into();
        }
        let page_idx = page_idx;

        assert!(
            page_idx.value() < NUM_ALLOCATABLE_PAGES,
            "All allocations must happen inside dyn malloc's region"
        );

        let next_page_idx = page_idx + BFieldElement::one();

        memory.insert(DYN_MALLOC_ADDRESS, next_page_idx);

        let page_address = page_idx * bfe!(NUM_WORDS_BETWEEN_ALLOCATED_PAGES);
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
        let page_number = rng.gen_range(0..NUM_ALLOCATABLE_PAGES);
        memory.insert(DYN_MALLOC_ADDRESS, page_number.into());

        FunctionInitialState { stack, memory }
    }

    fn corner_case_initial_states(&self) -> Vec<FunctionInitialState> {
        let empty_vm_state = {
            let stack = self.init_stack_for_isolated_run();
            let memory = HashMap::new();

            FunctionInitialState { stack, memory }
        };

        let one_page_has_been_allocated = {
            let stack = self.init_stack_for_isolated_run();
            let memory: HashMap<_, _> = [(DYN_MALLOC_ADDRESS, bfe!(1))].into_iter().collect();

            FunctionInitialState { stack, memory }
        };

        let second_to_last_page_has_been_allocated = {
            let stack = self.init_stack_for_isolated_run();
            let memory: HashMap<_, _> = [(DYN_MALLOC_ADDRESS, bfe!(NUM_ALLOCATABLE_PAGES - 1))]
                .into_iter()
                .collect();

            FunctionInitialState { stack, memory }
        };

        let third_to_last_page_has_been_allocated = {
            let stack = self.init_stack_for_isolated_run();
            let memory: HashMap<_, _> = [(DYN_MALLOC_ADDRESS, bfe!(NUM_ALLOCATABLE_PAGES - 2))]
                .into_iter()
                .collect();

            FunctionInitialState { stack, memory }
        };

        vec![
            empty_vm_state,
            one_page_has_been_allocated,
            second_to_last_page_has_been_allocated,
            third_to_last_page_has_been_allocated,
        ]
    }
}

#[cfg(test)]
mod tests {
    use proptest_arbitrary_interop::arb;
    use test_strategy::proptest;

    use crate::test_helpers::negative_test;
    use crate::traits::function::ShadowedFunction;
    use crate::traits::rust_shadow::RustShadow;
    use crate::InitVmState;

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

    #[test]
    fn disallow_allocation_outside_of_region_unit_test() {
        fn negative_prop_disallow_allocation_outside_of_region(memory_page_index: BFieldElement) {
            let snippet = DynMalloc;
            let init_stack = snippet.init_stack_for_isolated_run();
            let shadowed_snippet = ShadowedFunction::new(snippet);

            let memory_filled_dyn_malloc: HashMap<_, _> = [(DYN_MALLOC_ADDRESS, memory_page_index)]
                .into_iter()
                .collect();
            let init_state =
                InitVmState::with_stack_and_memory(init_stack, memory_filled_dyn_malloc);
            negative_test(
                &shadowed_snippet,
                init_state,
                &[InstructionError::AssertionFailed],
            );
        }

        // Last page has been allocated, next call must fail
        negative_prop_disallow_allocation_outside_of_region(bfe!(NUM_ALLOCATABLE_PAGES));
        negative_prop_disallow_allocation_outside_of_region(bfe!(NUM_ALLOCATABLE_PAGES + 1));
        negative_prop_disallow_allocation_outside_of_region(bfe!(NUM_ALLOCATABLE_PAGES + 2));
        negative_prop_disallow_allocation_outside_of_region(bfe!(NUM_ALLOCATABLE_PAGES * 2));
        negative_prop_disallow_allocation_outside_of_region(bfe!(u32::MAX / 2 - 1));
        negative_prop_disallow_allocation_outside_of_region(bfe!(u32::MAX / 2));
        negative_prop_disallow_allocation_outside_of_region(bfe!(u32::MAX - 1));
        negative_prop_disallow_allocation_outside_of_region(bfe!(u32::MAX));
    }

    #[proptest]
    fn disallow_allocation_if_page_counter_is_not_a_u32(
        #[strategy(arb())]
        #[filter(#address.value() > u32::MAX as u64)]
        address: BFieldElement,
    ) {
        fn negative_prop_disallow_allocation_with_non_u32_page_counter(
            memory_page_index: BFieldElement,
        ) {
            let snippet = DynMalloc;
            let init_stack = snippet.init_stack_for_isolated_run();
            let shadowed_snippet = ShadowedFunction::new(snippet);

            let memory_filled_dyn_malloc: HashMap<_, _> = [(DYN_MALLOC_ADDRESS, memory_page_index)]
                .into_iter()
                .collect();
            let init_state =
                InitVmState::with_stack_and_memory(init_stack, memory_filled_dyn_malloc);
            negative_test(
                &shadowed_snippet,
                init_state,
                &[InstructionError::FailedU32Conversion(memory_page_index)],
            );
        }

        negative_prop_disallow_allocation_with_non_u32_page_counter(address);
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
            "tasmlib_memory_dyn_malloc_multiple_calls".to_string()
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
