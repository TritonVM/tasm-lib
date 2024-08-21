use triton_vm::prelude::*;

use crate::data_type::DataType;
use crate::library::Library;
use crate::traits::basic_snippet::BasicSnippet;
use crate::verifier::vm_proof_iter::shared::vm_proof_iter_type;

pub struct ReadCurrentItemPointer;

impl BasicSnippet for ReadCurrentItemPointer {
    fn inputs(&self) -> Vec<(DataType, String)> {
        vec![(
            DataType::StructRef(vm_proof_iter_type()),
            "vm_proof_iter".to_owned(),
        )]
    }

    fn outputs(&self) -> Vec<(DataType, String)> {
        vec![(DataType::VoidPointer, "current_value".to_owned())]
    }

    fn entrypoint(&self) -> String {
        "tasmlib_verifier_vm_proof_iter_read_current_item_pointer".to_owned()
    }

    fn code(&self, _library: &mut Library) -> Vec<LabelledInstruction> {
        let entrypoint = self.entrypoint();

        triton_asm!(
            {entrypoint}:
                // _ *proof_iter

                read_mem 1

                pop 1
                // _ *current_item_pointer

                return
        )
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use num::Zero;
    use rand::rngs::StdRng;
    use rand::Rng;
    use rand::SeedableRng;

    use crate::memory::dyn_malloc::DYN_MALLOC_FIRST_ADDRESS;
    use crate::memory::FIRST_NON_DETERMINISTICALLY_INITIALIZED_MEMORY_ADDRESS;
    use crate::memory::LAST_ADDRESS_AVAILABLE_FOR_NON_DETERMINISTICALLY_ALLOCATED_MEMORY;
    use crate::snippet_bencher::BenchmarkCase;
    use crate::traits::function::Function;
    use crate::traits::function::FunctionInitialState;
    use crate::traits::function::ShadowedFunction;
    use crate::traits::rust_shadow::RustShadow;

    use super::*;

    const PROOF_ITER_POINTER_FOR_TEST: BFieldElement = DYN_MALLOC_FIRST_ADDRESS;

    #[test]
    fn pbt() {
        ShadowedFunction::new(ReadCurrentItemPointer).test()
    }

    impl Function for ReadCurrentItemPointer {
        fn rust_shadow(
            &self,
            stack: &mut Vec<BFieldElement>,
            memory: &mut HashMap<BFieldElement, BFieldElement>,
        ) {
            let proof_iter_pointer = stack.pop().unwrap();
            let current_item_pointer = *memory
                .get(&proof_iter_pointer)
                .unwrap_or(&BFieldElement::zero());
            stack.push(current_item_pointer);
        }

        fn pseudorandom_initial_state(
            &self,
            seed: [u8; 32],
            _bench_case: Option<BenchmarkCase>,
        ) -> FunctionInitialState {
            let mut rng: StdRng = SeedableRng::from_seed(seed);

            let current_item_pointer: BFieldElement = bfe!(rng.gen_range(
                FIRST_NON_DETERMINISTICALLY_INITIALIZED_MEMORY_ADDRESS.value()
                    ..LAST_ADDRESS_AVAILABLE_FOR_NON_DETERMINISTICALLY_ALLOCATED_MEMORY.value()
            ));

            let memory: HashMap<_, _> = [(PROOF_ITER_POINTER_FOR_TEST, current_item_pointer)]
                .into_iter()
                .collect();
            FunctionInitialState {
                stack: [
                    self.init_stack_for_isolated_run(),
                    vec![PROOF_ITER_POINTER_FOR_TEST],
                ]
                .concat(),
                memory,
            }
        }

        fn corner_case_initial_states(&self) -> Vec<FunctionInitialState> {
            let empty_memory_state = FunctionInitialState {
                stack: [
                    self.init_stack_for_isolated_run(),
                    vec![PROOF_ITER_POINTER_FOR_TEST],
                ]
                .concat(),
                memory: HashMap::default(),
            };

            vec![empty_memory_state]
        }
    }
}
