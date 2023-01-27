use std::collections::HashMap;

use rand::Rng;
use twenty_first::shared_math::b_field_element::BFieldElement;

use crate::library::Library;
use crate::snippet::{NewSnippet, Snippet};
use crate::{get_init_tvm_stack, ExecutionState};

pub struct BfeAdd();

impl NewSnippet for BfeAdd {
    fn inputs() -> Vec<&'static str> {
        vec!["b", "a"]
    }

    fn outputs() -> Vec<&'static str> {
        vec!["a + b"]
    }

    fn crash_conditions() -> Vec<&'static str> {
        vec![]
    }

    fn gen_input_states() -> Vec<ExecutionState> {
        let mut rng = rand::thread_rng();
        let mut stack = get_init_tvm_stack();
        stack.push(rng.gen());
        stack.push(rng.gen());
        vec![ExecutionState::with_stack(stack)]
    }
}

impl Snippet for BfeAdd {
    fn stack_diff() -> isize {
        -1
    }

    fn entrypoint() -> &'static str {
        "bfe_add"
    }

    fn function_body(_library: &mut Library) -> String {
        let entrypoint = Self::entrypoint();
        format!(
            "
            {entrypoint}:
                add
                return
            "
        )
    }

    fn rust_shadowing(
        stack: &mut Vec<BFieldElement>,
        _std_in: Vec<BFieldElement>,
        _secret_in: Vec<BFieldElement>,
        _memory: &mut HashMap<BFieldElement, BFieldElement>,
    ) {
        let a = stack.pop().unwrap();
        let b = stack.pop().unwrap();
        stack.push(a + b);
    }
}

#[cfg(test)]
mod tests {
    use crate::snippet_bencher::bench_and_write;
    use crate::test_helpers::rust_tasm_equivalence_prop_new;

    use super::*;

    #[test]
    fn bfe_add_test() {
        rust_tasm_equivalence_prop_new::<BfeAdd>();
    }

    #[test]
    fn bfe_add_benchmark() {
        bench_and_write::<BfeAdd>();
    }
}
