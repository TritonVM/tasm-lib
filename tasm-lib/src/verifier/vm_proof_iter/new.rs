use triton_vm::prelude::*;

use crate::data_type::DataType;
use crate::memory::dyn_malloc::DynMalloc;
use crate::traits::basic_snippet::BasicSnippet;
use crate::verifier::vm_proof_iter::shared::vm_proof_iter_type;

pub struct New;

impl New {
    pub const FIRST_PROOF_ITEM_OFFSET: u64 = 2;
}

impl BasicSnippet for New {
    fn inputs(&self) -> Vec<(DataType, String)> {
        vec![(DataType::VoidPointer, "*proof".to_string())]
    }

    fn outputs(&self) -> Vec<(DataType, String)> {
        vec![(
            DataType::StructRef(vm_proof_iter_type()),
            "vm_proof_iter".to_owned(),
        )]
    }

    fn entrypoint(&self) -> String {
        "tasmlib_verifier_vm_proof_iter_new".to_owned()
    }

    fn code(&self, library: &mut crate::library::Library) -> Vec<LabelledInstruction> {
        let entrypoint = self.entrypoint();
        let dyn_malloc = library.import(Box::new(DynMalloc));

        triton_asm!(
            {entrypoint}:
                // *proof

                push {Self::FIRST_PROOF_ITEM_OFFSET} add
                // _ *first_proof_item

                call {dyn_malloc}
                // _ *first_proof_item *vm_proof_iter

                write_mem 1
                // _ (*vm_proof_iter + 1)

                push -1
                add
                // _ *vm_proof_iter

                return
        )
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use crate::rust_shadowing_helper_functions;
    use crate::snippet_bencher::BenchmarkCase;
    use crate::traits::function::Function;
    use crate::traits::function::FunctionInitialState;
    use crate::traits::function::ShadowedFunction;
    use crate::traits::rust_shadow::RustShadow;

    use super::*;

    #[test]
    fn vm_proof_iter_new_pbt() {
        ShadowedFunction::new(New).test()
    }

    impl Function for New {
        fn rust_shadow(
            &self,
            stack: &mut Vec<BFieldElement>,
            memory: &mut HashMap<BFieldElement, BFieldElement>,
        ) {
            let pointer_to_proof = stack.pop().unwrap();
            let pointer_to_first_element = pointer_to_proof + bfe!(Self::FIRST_PROOF_ITEM_OFFSET);
            let pointer_to_vm_proof_iter =
                rust_shadowing_helper_functions::dyn_malloc::dynamic_allocator(memory);
            memory.insert(pointer_to_vm_proof_iter, pointer_to_first_element);
            stack.push(pointer_to_vm_proof_iter);
        }

        fn pseudorandom_initial_state(
            &self,
            _seed: [u8; 32],
            _bench_case: Option<BenchmarkCase>,
        ) -> FunctionInitialState {
            FunctionInitialState {
                stack: self.init_stack_for_isolated_run(),
                memory: HashMap::default(),
            }
        }
    }
}
