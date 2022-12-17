use std::collections::HashMap;

use twenty_first::shared_math::b_field_element::BFieldElement;

use crate::library::Library;
use crate::snippet_trait::Snippet;

pub struct BfeAdd();

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
    use crate::{get_init_tvm_stack, snippet_trait::rust_tasm_equivalence_prop};

    use super::*;

    #[test]
    fn bfe_add_simple() {
        // TASM
        let init_stack = vec![
            get_init_tvm_stack(),
            vec![BFieldElement::new(200), BFieldElement::new(300)],
        ]
        .concat();

        let expected = None;
        let _execution_result = rust_tasm_equivalence_prop::<BfeAdd>(
            &init_stack,
            &[],
            &[],
            &mut HashMap::default(),
            expected,
        );
    }
}
