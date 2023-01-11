use std::collections::HashMap;

use twenty_first::shared_math::b_field_element::BFieldElement;

use crate::{library::Library, snippet::Snippet};

pub struct MmrVerifyFromSecretInputThroughMemory();

impl Snippet for MmrVerifyFromSecretInputThroughMemory {
    fn stack_diff() -> isize {
        todo!()
    }

    fn entrypoint() -> &'static str {
        "verify_from_secret_input_through_memory"
    }

    fn function_body(_library: &mut Library) -> String {
        let entrypoint = Self::entrypoint();
        format!(
            "
            // BEFORE: _ *peaks [digest (leaf_digest)] leaf_count_hi leaf_count_lo
            // AFTER: _ validation_result
                {entrypoint}:
                    return
                "
        )
    }

    fn rust_shadowing(
        _stack: &mut Vec<BFieldElement>,
        _std_in: Vec<BFieldElement>,
        _secret_in: Vec<BFieldElement>,
        _memory: &mut HashMap<BFieldElement, BFieldElement>,
    ) {
        todo!()
    }
}
