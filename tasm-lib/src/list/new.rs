use std::collections::HashMap;

use triton_vm::prelude::*;

use crate::prelude::*;
use crate::rust_shadowing_helper_functions::list::list_new;
use crate::snippet_bencher::BenchmarkCase;
use crate::traits::function::Function;
use crate::traits::function::FunctionInitialState;

/// Creates a new list and returns a pointer to it.
///
/// Lists like these do not have an explicit capacity. Instead, they have
/// access to a full [memory page][crate::memory]. Snippets like
/// [`Push`][super::push::Push] and [`Set`][super::set::Set] ensure that all
/// list access is in bounds.
#[derive(Debug, Default, Copy, Clone, Eq, PartialEq, Hash)]
pub struct New;

impl BasicSnippet for New {
    fn parameters(&self) -> Vec<(DataType, String)> {
        vec![]
    }

    fn return_values(&self) -> Vec<(DataType, String)> {
        vec![(DataType::VoidPointer, "*list".to_string())]
    }

    fn entrypoint(&self) -> String {
        "tasmlib_list_new".to_string()
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
        ShadowedFunction::new(New).test();
    }
}

#[cfg(test)]
mod benches {
    use super::*;
    use crate::test_prelude::*;

    #[test]
    fn benchmark() {
        ShadowedFunction::new(New).bench();
    }
}
