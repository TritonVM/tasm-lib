use std::collections::HashMap;

use num::BigUint;
use twenty_first::amount::u32s::U32s;
use twenty_first::shared_math::b_field_element::BFieldElement;
use twenty_first::util_types::algebraic_hasher::Hashable;
use twenty_first::util_types::mmr;

use crate::arithmetic::u32s_2::powers_of_two::U32s2PowersOfTwoStatic;
use crate::arithmetic::u32s_2::sub::U32s2Sub;
use crate::library::Library;
use crate::snippet_trait::Snippet;

use super::data_index_to_node_index::{self, DataIndexToNodeIndex};

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

        format!(
            "
                // BEFORE: _ [digests] *old_peaks old_leaf_count_hi old_leaf_count_lo
                // AFTER: _ *new_peaks data_index_hi data_index_lo *auth_path
                {entrypoint}:
                    call {data_index_to_node_index}
                    // stack: _ [digests] *old_peaks new_node_index_hi new_node_index_lo


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
