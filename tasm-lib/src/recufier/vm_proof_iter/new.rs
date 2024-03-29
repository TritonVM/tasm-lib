use triton_vm::prelude::*;

use crate::data_type::DataType;
use crate::data_type::StructType;
use crate::memory::dyn_malloc::DynMalloc;
use crate::traits::basic_snippet::BasicSnippet;

pub const FIRST_PROOF_ITEM_ELEMENT_POINTER: BFieldElement = BFieldElement::new(2);

pub fn vm_proof_iter_type() -> StructType {
    let name = "VmProofIter".to_string();
    let fields = vec![("current_item_pointer".to_string(), DataType::Bfe)];

    StructType { name, fields }
}

pub struct New;

impl BasicSnippet for New {
    fn inputs(&self) -> Vec<(DataType, String)> {
        vec![]
    }

    fn outputs(&self) -> Vec<(DataType, String)> {
        vec![(
            DataType::StructRef(vm_proof_iter_type()),
            "vm_proof_iter".to_owned(),
        )]
    }

    fn entrypoint(&self) -> String {
        "tasm_recufier_vm_proof_iter_new".to_owned()
    }

    fn code(&self, library: &mut crate::library::Library) -> Vec<LabelledInstruction> {
        let entrypoint = self.entrypoint();
        let dyn_malloc = library.import(Box::new(DynMalloc));

        triton_asm!(
            {entrypoint}:
                // _

                push {FIRST_PROOF_ITEM_ELEMENT_POINTER}

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
            let pointer = rust_shadowing_helper_functions::dyn_malloc::dynamic_allocator(memory);
            memory.insert(pointer, FIRST_PROOF_ITEM_ELEMENT_POINTER);
            stack.push(pointer);
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
