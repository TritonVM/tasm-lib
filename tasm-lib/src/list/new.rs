use std::collections::HashMap;

use triton_vm::prelude::*;

use crate::prelude::*;
use crate::rust_shadowing_helper_functions::list::list_new;
use crate::snippet_bencher::BenchmarkCase;
use crate::traits::function::Function;
use crate::traits::function::FunctionInitialState;

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct New {
    pub element_type: DataType,
}

impl New {
    #[allow(clippy::self_named_constructors)] // 🤷
    pub fn new(data_type: DataType) -> Self {
        Self {
            element_type: data_type,
        }
    }
}

impl BasicSnippet for New {
    fn inputs(&self) -> Vec<(DataType, String)> {
        vec![]
    }

    fn outputs(&self) -> Vec<(DataType, String)> {
        let list_type = DataType::List(Box::new(self.element_type.clone()));
        vec![(list_type, "*list".to_string())]
    }

    fn entrypoint(&self) -> String {
        let element_type = self.element_type.label_friendly_name();
        format!("tasmlib_list_new___{element_type}")
    }

    fn code(&self, library: &mut Library) -> Vec<LabelledInstruction> {
        let dyn_malloc = library.import(Box::new(DynMalloc));

        triton_asm!(
            // BEFORE: _
            // AFTER:  _ *list
            {self.entrypoint()}:
                call {dyn_malloc}
                            // _ *list

                /* write initial length = 0 to `*list` */
                push 0
                pick 1
                write_mem 1
                addi -1
                            // _ *list

                return
        )
    }
}

impl Function for New {
    fn rust_shadow(
        &self,
        stack: &mut Vec<BFieldElement>,
        memory: &mut HashMap<BFieldElement, BFieldElement>,
    ) {
        DynMalloc.rust_shadow(stack, memory);

        let &list_pointer = stack.last().unwrap();
        list_new(list_pointer, memory);
    }

    fn pseudorandom_initial_state(
        &self,
        _: [u8; 32],
        _: Option<BenchmarkCase>,
    ) -> FunctionInitialState {
        FunctionInitialState {
            stack: self.init_stack_for_isolated_run(),
            ..Default::default()
        }
    }
}

#[cfg(test)]
pub(crate) mod tests {
    use super::*;
    use crate::test_prelude::*;

    #[test]
    fn rust_shadow() {
        for ty in [
            DataType::Bool,
            DataType::Bfe,
            DataType::U32,
            DataType::U64,
            DataType::Xfe,
            DataType::Digest,
        ] {
            dbg!(&ty);
            ShadowedFunction::new(New::new(ty)).test();
        }
    }
}

#[cfg(test)]
mod benches {
    use super::*;
    use crate::test_prelude::*;

    #[test]
    fn benchmark() {
        ShadowedFunction::new(New::new(DataType::Digest)).bench();
    }
}
