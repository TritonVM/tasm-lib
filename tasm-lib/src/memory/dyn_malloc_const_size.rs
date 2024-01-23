use std::collections::HashMap;

use num_traits::Zero;
use rand::rngs::StdRng;
use rand::Rng;
use rand::SeedableRng;
use triton_vm::prelude::*;

use crate::data_type::DataType;
use crate::empty_stack;
use crate::library::Library;
use crate::memory::dyn_malloc::DYN_MALLOC_ADDRESS;
use crate::traits::basic_snippet::BasicSnippet;
use crate::traits::function::Function;
use crate::traits::function::FunctionInitialState;

const CONST_SIZE_MALLOCS_FIRST_DYNAMICALLY_ALLOCATED_ADDRESS: BFieldElement =
    BFieldElement::new(1 << 33);
const TWO_POW_32: u64 = 1 << 32;

/// Output a new dynamically allocated address. Should *not* be used in combination with `DynMalloc`!
#[derive(Clone, Debug)]
pub struct DynMallocConstSize;

impl BasicSnippet for DynMallocConstSize {
    fn inputs(&self) -> Vec<(DataType, String)> {
        vec![]
    }

    fn outputs(&self) -> Vec<(DataType, String)> {
        vec![(DataType::Bfe, "*free_address".to_owned())]
    }

    fn entrypoint(&self) -> String {
        "tasm_memory_dyn_malloc_const_size".to_string()
    }

    fn code(&self, _library: &mut Library) -> Vec<LabelledInstruction> {
        let entrypoint = self.entrypoint();

        triton_asm!(
        // BEFORE: _
        // AFTER:  _ *next_addr
        {entrypoint}:
            push {DYN_MALLOC_ADDRESS}                  // _ *dyn_malloc_state
            read_mem 1                                 // _ *next_addr' *dyn_malloc_state-1
            pop 1                                      // _ *next_addr'

            dup 0                                      // _ *next_addr' *next_addr'
            push 0                                     // _ *next_addr' *next_addr' 0
            eq                                         // _ *next_addr' (*next_addr' == 0)
            push {CONST_SIZE_MALLOCS_FIRST_DYNAMICALLY_ALLOCATED_ADDRESS}
            mul                                        // _ *next_addr' (*next_addr' == 0) * 2^{33}
            add                                        // _ *next_addr

            // Calculate next available address
            dup 0
            push {TWO_POW_32}
            add
            // _ *next_addr *next_next_addr

            // Verify that *next_next_addr does not live in the $[0, 2^{32})$ range.
            dup 0
            split
            pop 1
            push 0
            eq
            push 0
            eq
            assert
            // _ *next_addr *next_next_addr

            push {DYN_MALLOC_ADDRESS}   // _ *next_addr *next_next_addr *dyn_malloc_state
            write_mem 1                 // _ *next_addr (*dyn_malloc_state+1)
            pop 1                       // _ *next_addr

            return
        )
    }
}

impl Function for DynMallocConstSize {
    fn rust_shadow(
        &self,
        stack: &mut Vec<BFieldElement>,
        memory: &mut HashMap<BFieldElement, BFieldElement>,
    ) {
        let used_memory = memory
            .entry(DYN_MALLOC_ADDRESS)
            .and_modify(|e| {
                *e = if e.is_zero() {
                    CONST_SIZE_MALLOCS_FIRST_DYNAMICALLY_ALLOCATED_ADDRESS
                } else {
                    *e
                }
            })
            .or_insert_with(|| CONST_SIZE_MALLOCS_FIRST_DYNAMICALLY_ALLOCATED_ADDRESS);

        let next_addr = *used_memory;

        stack.push(next_addr);
        *used_memory += TWO_POW_32.into();

        assert!(used_memory.value() > (1u64 << 32));
    }

    fn pseudorandom_initial_state(
        &self,
        seed: [u8; 32],
        _bench_case: Option<crate::snippet_bencher::BenchmarkCase>,
    ) -> crate::traits::function::FunctionInitialState {
        let mut rng: StdRng = StdRng::from_seed(seed);
        let uses = rng.gen_range(0..(1u64 << 31));
        let memory: HashMap<BFieldElement, BFieldElement> =
            [(DYN_MALLOC_ADDRESS, BFieldElement::new(TWO_POW_32 * uses))]
                .into_iter()
                .collect();

        FunctionInitialState {
            stack: empty_stack(),
            memory,
        }
    }

    fn corner_case_initial_states(&self) -> Vec<FunctionInitialState> {
        let unused_memory_allocator = FunctionInitialState {
            stack: empty_stack(),
            memory: HashMap::default(),
        };

        let last_legal_allocated_address: BFieldElement =
            (((1u128 << 64) - (1 << 33)) as u64).into();
        let last_legal_allocated_address = FunctionInitialState {
            stack: empty_stack(),
            memory: [(DYN_MALLOC_ADDRESS, last_legal_allocated_address)]
                .into_iter()
                .collect(),
        };

        vec![unused_memory_allocator, last_legal_allocated_address]
    }
}

#[cfg(test)]
mod tests {
    use std::cell::RefCell;
    use std::rc::Rc;

    use crate::execute_with_terminal_state;
    use crate::linker::link_for_isolated_run;
    use crate::test_helpers::tasm_final_state;
    use crate::traits::function::ShadowedFunction;
    use crate::traits::rust_shadow::RustShadow;

    use super::*;

    #[test]
    fn expected_address_chosen_for_dyn_malloc() {
        assert_eq!(1 << 32, DYN_MALLOC_ADDRESS.value());
    }

    #[test]
    fn expected_address_chosen_for_first_address() {
        assert_eq!(
            1 << 33,
            CONST_SIZE_MALLOCS_FIRST_DYNAMICALLY_ALLOCATED_ADDRESS.value()
        );
    }

    #[test]
    fn first_free_address() {
        let mut final_state = tasm_final_state(
            &ShadowedFunction::new(DynMallocConstSize),
            &empty_stack(),
            &[],
            NonDeterminism::default(),
            &None,
            0,
        );

        let allocated_address = final_state.final_stack.pop().unwrap();
        assert_eq!(
            CONST_SIZE_MALLOCS_FIRST_DYNAMICALLY_ALLOCATED_ADDRESS,
            allocated_address
        );
        assert_eq!(1u64 << 33, allocated_address.value());
    }

    #[test]
    fn pbt() {
        ShadowedFunction::new(DynMallocConstSize).test();
    }

    #[test]
    fn negative_test() {
        // run rust shadow
        let init_stack = empty_stack();
        let last_legal_allocated_address: BFieldElement =
            (((1u128 << 64) - (1 << 32)) as u64).into();
        let init_memory: HashMap<BFieldElement, BFieldElement> =
            [(DYN_MALLOC_ADDRESS, last_legal_allocated_address)]
                .into_iter()
                .collect();
        let rust_result = std::panic::catch_unwind(|| {
            let mut rust_stack = init_stack.clone();
            ShadowedFunction::new(DynMallocConstSize).rust_shadow_wrapper(
                &[],
                &NonDeterminism::new(vec![]),
                &mut rust_stack,
                &mut init_memory.clone(),
                &mut None,
            )
        });

        // Run on Triton
        let code = link_for_isolated_run(Rc::new(RefCell::new(DynMallocConstSize)), 0);

        let program = Program::new(&code);
        let tvm_result = execute_with_terminal_state(
            &program,
            &[],
            &init_stack,
            &NonDeterminism::default().with_ram(init_memory),
            None,
        );

        assert!(
            rust_result.is_err() && tvm_result.is_err(),
            "Test case: dyn malloc const size must fail when "
        );
    }
}

#[cfg(test)]
mod benches {
    use crate::traits::function::ShadowedFunction;
    use crate::traits::rust_shadow::RustShadow;

    use super::*;

    #[test]
    fn push_ram_to_stack_bench() {
        ShadowedFunction::new(DynMallocConstSize).bench();
    }
}
