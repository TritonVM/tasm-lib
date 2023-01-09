use std::collections::HashMap;

use twenty_first::{
    amount::u32s::U32s, shared_math::b_field_element::BFieldElement,
    util_types::algebraic_hasher::Hashable,
};

use crate::{
    arithmetic::u32::{is_odd::U32IsOdd, is_u32::IsU32},
    library::Library,
    list::u32::length::LengthLong,
    snippet_trait::Snippet,
};

use super::leaf_index_to_mt_index::{self, MmrLeafIndexToMtIndexAndPeakIndex};

/// Calculate new MMR peaks from a leaf mutation using Merkle tree indices walk up the tree
pub struct MmrCalculateNewPeaksFromLeafMutationMtIndices();

impl Snippet for MmrCalculateNewPeaksFromLeafMutationMtIndices {
    fn stack_diff() -> isize {
        -11
    }

    fn entrypoint() -> &'static str {
        "calculate_new_peaks_from_leaf_mutation"
    }

    fn function_body(library: &mut Library) -> String {
        let entrypoint = Self::entrypoint();
        let leaf_index_to_mt_index = library.import::<MmrLeafIndexToMtIndexAndPeakIndex>();
        let list_length = library.import::<LengthLong>();
        let u32_is_odd = library.import::<U32IsOdd>();

        format!(
            "
            // BEFORE: _ *peaks [digest (new_leaf)] *auth_paths leaf_count_hi leaf_count_lo leaf_index_hi leaf_index_lo
            // AFTER: _
            {entrypoint}:
                call {leaf_index_to_mt_index}
                // stack: _ *peaks [digest (new_leaf)] *auth_path peak_index mt_index_hi mt_index_lo

                // Get the length of `auth_path`, length is measured in number of digests
                dup3
                call {list_length}
                // stack: _ *peaks [digest (new_leaf)] *auth_path peak_index mt_index_hi mt_index_lo auth_path_length

                push 0
                // stack: _ *peaks [digest (new_leaf)] *auth_path peak_index mt_index_hi mt_index_lo ap_length ap_index

                call {entrypoint}_while


                return

            // start/end stack: _ *peaks [digest (new_leaf)] *auth_path peak_index mt_index_hi mt_index_lo ap_length ap_index
            {entrypoint}_while:
                // Loop condition: break loop if ap_index == ap_length
                dup1 dup1 eq
                skiz return
                // stack: _ *peaks [digest (new_leaf)] *auth_path peak_index mt_index_hi mt_index_lo ap_length ap_index

                dup2
                call {u32_is_odd}
                push 1
                swap1
                // stack: _ *peaks [digest (new_leaf)] *auth_path peak_index mt_index_hi mt_index_lo ap_length ap_index 1 (is_right_child)
                skiz call {entrypoint}_right_child
                skiz call {entrypoint}_left_child

                recurse

            {entrypoint}_right_child:
                // stack: _ *peaks [digest (new_leaf)] *auth_path peak_index mt_index_hi mt_index_lo ap_length ap_index 1
                pop

                // push 0 to prevent `left_child` branch from being taken
                push 0
                return
            {entrypoint}_left_child:
                return

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
