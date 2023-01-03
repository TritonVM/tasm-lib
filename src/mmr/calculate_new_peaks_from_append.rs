use std::collections::HashMap;

use num::BigUint;
use twenty_first::amount::u32s::U32s;
use twenty_first::shared_math::b_field_element::BFieldElement;
use twenty_first::shared_math::rescue_prime_regular::DIGEST_LENGTH;
use twenty_first::util_types::algebraic_hasher::Hashable;
use twenty_first::util_types::mmr;

use crate::arithmetic::u32s_2::sub::U32s2Sub;
use crate::library::Library;
use crate::list::u32::pop::Pop;
use crate::list::u32::set_length::{self, SetLength};
use crate::snippet_trait::Snippet;
use crate::{arithmetic::u32s_2::powers_of_two::U32s2PowersOfTwoStatic, list::u32::push::Push};

use super::{
    data_index_to_node_index::{self, DataIndexToNodeIndex},
    right_child_and_height::MmrRightChildAndHeight,
};

pub const MAX_MMR_HEIGHT: usize = 64;

pub struct CalculateNewPeaksFromAppend;

impl Snippet for CalculateNewPeaksFromAppend {
    fn stack_diff() -> isize {
        // pops: `old_leaf_count` (u32s<2>); old_peaks (*list); new_leaf (Digest)
        // pushes: *list (new peaks); *auth_path_of_newly_added_leaf
        -6
    }

    fn entrypoint() -> &'static str {
        "calculate_new_peaks_from_append"
    }

    fn function_body(library: &mut Library) -> String {
        let entrypoint = Self::entrypoint();
        let data_index_to_node_index = library.import::<DataIndexToNodeIndex>();
        let right_child_and_height = library.import::<MmrRightChildAndHeight>();

        let push = library.import::<Push<DIGEST_LENGTH>>();
        let pop = library.import::<Pop<DIGEST_LENGTH>>();
        let set_length = library.import::<SetLength>();
        let dup_peaks = format!("dup{DIGEST_LENGTH}");
        let pop_digest = str::repeat("pop", DIGEST_LENGTH);
        let dup_top_digest = str::repeat(&format!("dup{}", DIGEST_LENGTH - 1), DIGEST_LENGTH);

        // Allocate memory for the returned auth path for the newly inserted element
        // Warning: This auth path is only allocated *once* even though the code is called multiple times
        let static_auth_path_pointer = library.kmalloc(DIGEST_LENGTH * MAX_MMR_HEIGHT);

        format!(
            "
                // BEFORE: _ old_leaf_count_hi old_leaf_count_lo *peaks [digests (new_leaf)]
                // AFTER: _ *new_peaks data_index_hi data_index_lo *auth_path
                {entrypoint}:
                    call {push}
                    // stack: _ old_leaf_count_hi old_leaf_count_lo *peaks

                    // Create auth_path return value (vector living in RAM)
                    // Notice that this will always allocate to the same pointer in memory, even
                    // if this function is called multiple times.
                    push {static_auth_path_pointer}
                    push 0
                    call {set_length}
                    // stack: _ old_leaf_count_hi old_leaf_count_lo *peaks *auth_path

                    swap1
                    // stack: _ old_leaf_count_hi old_leaf_count_lo *auth_path *peaks

                    dup3 dup3
                    // stack: _ old_leaf_count_hi old_leaf_count_lo *auth_path *peaks old_leaf_count_hi old_leaf_count_lo

                    call {data_index_to_node_index}
                    // stack: _ old_leaf_count_hi old_leaf_count_lo *auth_path *peaks new_ni_hi new_ni_lo

                    call {right_child_and_height}
                    // stack: _ old_leaf_count_hi old_leaf_count_lo *auth_path *peaks is_right_child height
                    pop

                    call {entrypoint}_while

                    return

                // Stack start and end: _ old_leaf_count_hi old_leaf_count_lo *auth_path *peaks is_right_child
                {entrypoint}_while:
                    push 0
                    eq
                    skiz
                        return
                    // Stack: _ old_leaf_count_hi old_leaf_count_lo *auth_path *peaks

                    dup0
                    call {pop}
                    // Stack: _ old_leaf_count_hi old_leaf_count_lo *auth_path *peaks [digest (new_hash)]

                    {dup_peaks}
                    // Stack: _ old_leaf_count_hi old_leaf_count_lo *auth_path *peaks [digest (new_hash)] *peaks

                    call {pop}
                    // Stack: _ old_leaf_count_hi old_leaf_count_lo *auth_path *peaks [digest (new_hash)] [digests (previous_peak)]

                    // Update authentication path with latest previous_peak
                    // These instructions assume digest length of 5!
                    dup11
                    dup5 dup5 dup5 dup5 dup5
                    // Stack: _ old_leaf_count_hi old_leaf_count_lo *auth_path *peaks [digest (new_hash)] [digests (previous_peak)] *auth_path [digests (previous_peak)]

                    call {push}
                    pop
                    // Stack: _ old_leaf_count_hi old_leaf_count_lo *auth_path *peaks [digest (new_hash)] [digests (previous_peak)]

                    hash
                    {pop_digest}
                    // Stack: _ old_leaf_count_hi old_leaf_count_lo *auth_path *peaks [digests (new_peak)]

                    call {push}
                    // Stack: _ old_leaf_count_hi old_leaf_count_lo *auth_path *peaks




                "
        )
    }

    fn rust_shadowing(
        stack: &mut Vec<BFieldElement>,
        std_in: Vec<BFieldElement>,
        secret_in: Vec<BFieldElement>,
        memory: &mut HashMap<BFieldElement, BFieldElement>,
    ) {
        todo!()
    }
}
