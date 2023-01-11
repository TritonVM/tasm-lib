use std::collections::HashMap;

use twenty_first::shared_math::b_field_element::BFieldElement;

use crate::library::Library;
use crate::snippet::Snippet;

pub struct CountLeaves();

impl Snippet for CountLeaves {
    fn stack_diff() -> isize {
        // Pops the MMR, three words, and pushes the leaf count, two words
        -1
    }

    fn entrypoint() -> &'static str {
        "count_leaves"
    }

    // Before: _ leaf_count peaks
    // After: _ leaf_count
    fn function_body(_library: &mut Library) -> String {
        // If you already have an MMR you might want to get the leaf count directly from the object without calling this function
        let entrypoint = Self::entrypoint();
        format!(
            "{entrypoint}:
                pop
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
        stack.pop().unwrap();
    }
}

#[cfg(test)]
mod tests {
    use num::{One, Zero};
    use twenty_first::shared_math::b_field_element::BFieldElement;
    use twenty_first::shared_math::rescue_prime_digest::Digest;
    use twenty_first::shared_math::rescue_prime_regular::RescuePrimeRegular;
    use twenty_first::util_types::mmr::mmr_accumulator::MmrAccumulator;
    use twenty_first::util_types::mmr::mmr_trait::Mmr;

    use crate::{get_init_tvm_stack, snippet::rust_tasm_equivalence_prop};

    use super::*;

    #[test]
    fn mmr_count_leaves() {
        let mut expected_stack = get_init_tvm_stack();

        // Test with an MMR with 3 leaves
        expected_stack.push(BFieldElement::zero());
        expected_stack.push(BFieldElement::new(3));
        prop_count_leaves(
            &mut MmrAccumulator::new(vec![
                Digest::default(),
                Digest::default(),
                Digest::default(),
            ]),
            &expected_stack,
        );

        // Test with an MMR with 9 leaves
        expected_stack = get_init_tvm_stack();
        expected_stack.push(BFieldElement::zero());
        expected_stack.push(BFieldElement::new(9));
        prop_count_leaves(
            &mut MmrAccumulator::new(vec![
                Digest::default(),
                Digest::default(),
                Digest::default(),
                Digest::default(),
                Digest::default(),
                Digest::default(),
                Digest::default(),
                Digest::default(),
                Digest::default(),
            ]),
            &expected_stack,
        );

        // Test with an MMR with 9 _ u32::MAX leaves
        expected_stack = get_init_tvm_stack();
        expected_stack.push(BFieldElement::one());
        expected_stack.push(BFieldElement::new(8));
        prop_count_leaves(
            &mut MmrAccumulator::init(
                vec![
                    Digest::default(),
                    Digest::default(),
                    Digest::default(),
                    Digest::default(),
                    Digest::default(),
                    Digest::default(),
                ],
                (u32::MAX as u64 + 9) as u128,
            ),
            &expected_stack,
        );
    }

    fn prop_count_leaves(
        mmra: &mut MmrAccumulator<RescuePrimeRegular>,
        expected: &[BFieldElement],
    ) {
        let mut init_stack = get_init_tvm_stack();
        let leaf_count_u64: u64 = mmra.count_leaves() as u64;

        // most significant bits
        init_stack.push(BFieldElement::new(leaf_count_u64 >> 32));

        // leasts significant bits
        init_stack.push(BFieldElement::new(leaf_count_u64 & (u32::MAX as u64)));

        // Just fake the list pointer by picking something arbitrary
        init_stack.push(BFieldElement::new(13337));

        let _execution_result = rust_tasm_equivalence_prop::<CountLeaves>(
            &init_stack,
            &[],
            &[],
            &mut HashMap::default(),
            0,
            Some(expected),
        );
    }
}
