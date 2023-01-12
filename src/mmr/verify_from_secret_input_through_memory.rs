use std::collections::HashMap;

use twenty_first::shared_math::{
    b_field_element::BFieldElement, rescue_prime_regular::DIGEST_LENGTH,
};

use crate::library::Library;
use crate::list::u32::set_length::SetLength;
use crate::snippet::{NewSnippet, Snippet};

use super::MAX_MMR_HEIGHT;

pub struct MmrVerifyFromSecretInputThroughMemory();

impl NewSnippet for MmrVerifyFromSecretInputThroughMemory {
    fn inputs() -> Vec<&'static str> {
        vec![
            "peaks_pointer",
            "leaf_digest4",
            "leaf_digest3",
            "leaf_digest2",
            "leaf_digest1",
            "leaf_digest0",
            "leaf_count_hi",
            "leaf_count_lo",
        ]
    }

    fn outputs() -> Vec<&'static str> {
        vec!["auth_path_pointer", "validation_result"]
    }

    fn crash_conditions() -> Vec<&'static str> {
        vec![]
    }

    fn gen_input_states() -> Vec<crate::ExecutionState> {
        todo!()
    }
}

impl Snippet for MmrVerifyFromSecretInputThroughMemory {
    fn stack_diff() -> isize {
        -7
    }

    fn entrypoint() -> &'static str {
        "verify_from_secret_input_through_memory"
    }

    fn function_body(library: &mut Library) -> String {
        let set_length = library.import::<SetLength>();

        // Allocate memory for the returned auth path for the newly inserted element
        // Warning: This auth path is only allocated *once* even though the code is called multiple times.
        // So if this function is called multiple times, the auth_paths will be overwritten.
        let static_auth_path_pointer = library.kmalloc(DIGEST_LENGTH * MAX_MMR_HEIGHT);
        let entrypoint = Self::entrypoint();
        format!(
            "
            // BEFORE: _ *peaks leaf_count_hi leaf_count_lo [digest (leaf_digest)]
            // AFTER: _ *auth_path leaf_index_hi leaf_index_lo validation_result
                {entrypoint}:
                    // allocate space in memory for the `auth_path`
                    push {static_auth_path_pointer}
                    push 0
                    call {set_length}
                    // _ *peaks [digest (leaf_digest)] leaf_count_hi leaf_count_lo *auth_path

                    swap8
                    // _ *auth_path [digest (leaf_digest)] leaf_count_hi leaf_count_lo *peaks

                    // Read the leaf index from secret input
                    divine
                    divine
                    // _ *peaks [digest (leaf_digest)] leaf_count_hi leaf_count_lo *auth_path leaf_index_hi leaf_index_lo


                    swap8
                    // _ *auth_path [digest (leaf_digest)] leaf_count_hi leaf_count_lo *peaks

                    swap2 swap1
                    // _ *auth_path [digest (leaf_digest)] *peaks leaf_count_hi leaf_count_lo

                    // _ *auth_path [digest (leaf_digest)] *peaks leaf_count_hi leaf_count_lo leaf_index_hi leaf_index_lo

                    // BEFORE: _ *auth_path [digest (leaf_digest)] *peaks leaf_count_hi leaf_count_lo leaf_index_hi leaf_index_lo
                    // AFTER: _ validation_result

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
