use std::collections::HashMap;

use triton_vm::prelude::*;

use crate::prelude::*;
use crate::traits::basic_snippet::Reviewer;
use crate::traits::basic_snippet::SignOffFingerprint;

/// Verify membership in a [Merkle tree](twenty_first::prelude::MerkleTree).
///
/// Verify that a leaf lives in a Merkle tree, given the tree's root, its
/// height, the leaf's index, and the leaf itself. The authentication path is
/// non-deterministically divined. This algorithm asserts that the leaf is a
/// member of the tree; phrased differently, if membership could not be
/// established, it crashes the VM.
///
/// ### Behavior
///
/// ```text
/// BEFORE: _ [root: Digest] tree_height leaf_index [leaf: Digest]
/// AFTER:  _
/// ```
///
/// ### Preconditions
///
/// - all input arguments are properly [`BFieldCodec`] encoded
///
/// ### Postconditions
///
/// None.
#[derive(Clone, Debug)]
pub struct MerkleVerify;

impl MerkleVerify {
    pub const TREE_TOO_HIGH_ERROR_ID: i128 = 0;
    pub const OUT_OF_BOUNDS_LEAF_ERROR_ID: i128 = 1;
    pub const ROOT_MISMATCH_ERROR_ID: i128 = 2;
}

impl BasicSnippet for MerkleVerify {
    fn parameters(&self) -> Vec<(DataType, String)> {
        vec![
            (DataType::Digest, "root".to_string()),
            (DataType::U32, "tree_height".to_string()),
            (DataType::U32, "leaf_index".to_string()),
            (DataType::Digest, "leaf".to_string()),
        ]
    }

    fn return_values(&self) -> Vec<(DataType, String)> {
        vec![]
    }

    fn entrypoint(&self) -> String {
        "tasmlib_hashing_merkle_verify".to_string()
    }

    fn code(&self, _: &mut Library) -> Vec<LabelledInstruction> {
        let entrypoint = self.entrypoint();
        let traverse_tree = format!("{entrypoint}_traverse_tree");
        let tree_height_is_not_zero = format!("{entrypoint}_tree_height_is_not_zero");
        triton_asm!(
            // BEFORE: _ [root; 5] tree_height leaf_index [leaf; 5]
            // AFTER:  _
            {entrypoint}:
                /* Assert reasonable tree height
                 *
                 * Don't rely only on
                 * 1. `pow`'s implicit check that the exponent is a u32,
                 * 2. `assert leaf_index < num_leaves`.
                 * Since bfe!(2)^192 == 1 and 192 < u32::MAX, weird things are possible. For
                 * example, the number of leafs for a tree of height 193 would incorrectly be
                 * computed as 2.
                 * Any attack would probably still require a hash collision to work, but there's
                 * no point in leaving a potential attack vector open.
                 */
                push 32
                dup 7
                lt
                assert error_id {Self::TREE_TOO_HIGH_ERROR_ID}

                /* Calculate node index from tree height and leaf index */
                dup 6
                push 2
                pow
                // _ [root; 5] tree_height leaf_index [leaf; 5] num_leaves

                dup 0 dup 7 lt
                // _ [root; 5] tree_height leaf_index [leaf; 5] num_leaves (leaf_index < num_leaves)

                assert error_id {Self::OUT_OF_BOUNDS_LEAF_ERROR_ID}
                // _ [root; 5] tree_height leaf_index [leaf; 5] num_leaves

                pick 6
                add
                // _ [root; 5] tree_height [leaf; 5] node_index

                place 5
                // _ [root; 5] tree_height node_index [leaf; 5]

                pick 6
                skiz
                    call {tree_height_is_not_zero}
                // _ [root; 5] [0|1] [calculated_root; 5]

                /* compare calculated and provided root */
                pick 5
                pop 1
                assert_vector error_id {Self::ROOT_MISMATCH_ERROR_ID}
                pop 5

                return

            // BEFORE: _ node_index [leaf; 5]
            {tree_height_is_not_zero}:
                push 1
                place 6
                // _ 1 node_index [leaf; 5]

                call {traverse_tree}
                // _ 1 1 [calculated_root; 5]

                pick 6
                pop 1

                return

            {traverse_tree}:
                merkle_step
                recurse_or_return
        )
    }

    fn sign_offs(&self) -> HashMap<Reviewer, SignOffFingerprint> {
        let mut sign_offs = HashMap::new();
        sign_offs.insert(Reviewer("ferdinand"), 0xc5311e6579152b58.into());
        sign_offs
    }
}

#[cfg(test)]
mod tests {
    use std::collections::VecDeque;

    use proptest::collection::vec;

    use super::*;
    use crate::test_helpers::negative_test;
    use crate::test_prelude::*;

    impl ReadOnlyAlgorithm for MerkleVerify {
        fn rust_shadow(
            &self,
            stack: &mut Vec<BFieldElement>,
            _: &HashMap<BFieldElement, BFieldElement>,
            _: VecDeque<BFieldElement>,
            mut nd_digests: VecDeque<Digest>,
        ) {
            // BEFORE: _ [root; 5] tree_height leaf_index [leaf; 5]
            // AFTER:  _
            let leaf = pop_encodable(stack);
            let leaf_index = pop_encodable::<u32>(stack);
            let tree_height = pop_encodable::<u32>(stack);
            let root = pop_encodable(stack);

            let num_leaves = 1 << tree_height;
            assert!(leaf_index < num_leaves);

            let mut node_digest = leaf;
            let mut node_index = leaf_index + num_leaves;
            while node_index != 1 {
                let sibling = nd_digests.pop_front().unwrap();
                let node_is_left_sibling = node_index.is_multiple_of(2);
                node_digest = if node_is_left_sibling {
                    Tip5::hash_pair(node_digest, sibling)
                } else {
                    Tip5::hash_pair(sibling, node_digest)
                };
                node_index /= 2;
            }
            assert_eq!(node_digest, root);
        }

        fn pseudorandom_initial_state(
            &self,
            seed: [u8; 32],
            maybe_bench_case: Option<BenchmarkCase>,
        ) -> ReadOnlyAlgorithmInitialState {
            // BEFORE: _ [root; 5] tree_height leaf_index [leaf; 5]
            let mut rng = StdRng::from_seed(seed);
            let tree_height = match maybe_bench_case {
                Some(BenchmarkCase::CommonCase) => 6,
                Some(BenchmarkCase::WorstCase) => 20,
                None => rng.random_range(1..20),
            };

            // sample unconstrained inputs directly
            let num_leaves = 1 << tree_height;
            let leaf_index = rng.random_range(0..num_leaves);
            let path = (0..tree_height).map(|_| rng.random()).collect_vec();
            let leaf = rng.random();

            // walk up tree to calculate root
            let mut current_node = leaf;
            let mut node_index = leaf_index + num_leaves;
            for &sibling in &path {
                let node_is_left_sibling = node_index % 2 == 0;
                current_node = if node_is_left_sibling {
                    Tip5::hash_pair(current_node, sibling)
                } else {
                    Tip5::hash_pair(sibling, current_node)
                };
                node_index /= 2;
            }
            let root = current_node;

            let mut stack = Self.init_stack_for_isolated_run();
            push_encodable(&mut stack, &root);
            push_encodable(&mut stack, &tree_height);
            push_encodable(&mut stack, &leaf_index);
            push_encodable(&mut stack, &leaf);

            let nondeterminism = NonDeterminism::default().with_digests(path);
            ReadOnlyAlgorithmInitialState {
                stack,
                nondeterminism,
            }
        }
    }

    #[test]
    fn merkle_verify_test() {
        ShadowedReadOnlyAlgorithm::new(MerkleVerify).test()
    }

    #[proptest]
    fn merkle_tree_verification_fails_if_leaf_is_disturbed_slightly(
        seed: [u8; 32],
        #[strategy(0_usize..5)] perturbation_index: usize,
        #[filter(#perturbation != 0)] perturbation: i8,
    ) {
        let mut initial_state = MerkleVerify.pseudorandom_initial_state(seed, None);
        let top_of_stack = initial_state.stack.len() - 1;
        initial_state.stack[top_of_stack - perturbation_index] += bfe!(perturbation);

        test_assertion_failure(
            &ShadowedReadOnlyAlgorithm::new(MerkleVerify),
            initial_state.into(),
            &[2],
        );
    }

    #[proptest]
    fn merkle_tree_verification_fails_if_leaf_index_is_disturbed_slightly(
        seed: [u8; 32],
        #[filter(#perturbation != 0)] perturbation: i8,
    ) {
        let mut initial_state = MerkleVerify.pseudorandom_initial_state(seed, None);
        let top_of_stack = initial_state.stack.len() - 1;
        let leaf_index_index = top_of_stack - 5;
        initial_state.stack[leaf_index_index] += bfe!(perturbation);

        // out-of-range leaf indices are tested separately
        let leaf_index = initial_state.stack[leaf_index_index];
        prop_assume!(u32::try_from(leaf_index).is_ok());

        test_assertion_failure(
            &ShadowedReadOnlyAlgorithm::new(MerkleVerify),
            initial_state.into(),
            &[1, 2],
        );
    }

    #[proptest]
    fn merkle_tree_verification_fails_if_leaf_index_is_out_of_range(
        seed: [u8; 32],
        #[strategy(u64::from(u32::MAX)..=BFieldElement::MAX)]
        #[map(BFieldElement::new)]
        leaf_index: BFieldElement,
    ) {
        let mut initial_state = MerkleVerify.pseudorandom_initial_state(seed, None);
        let top_of_stack = initial_state.stack.len() - 1;
        let leaf_index_index = top_of_stack - 5;
        initial_state.stack[leaf_index_index] = leaf_index;

        negative_test(
            &ShadowedReadOnlyAlgorithm::new(MerkleVerify),
            initial_state.into(),
            &[OpStackError::FailedU32Conversion(leaf_index).into()],
        );
    }

    #[proptest]
    fn merkle_tree_verification_fails_if_tree_height_is_disturbed_slightly(
        seed: [u8; 32],
        #[strategy(-32_i8..32)]
        #[filter(#perturbation != 0)]
        perturbation: i8,
        #[strategy(vec(arb(), #perturbation.clamp(0, 32) as usize))]
        additional_bogus_tree_nodes: Vec<Digest>,
    ) {
        let mut initial_state = MerkleVerify.pseudorandom_initial_state(seed, None);
        let top_of_stack = initial_state.stack.len() - 1;
        let tree_height_index = top_of_stack - 6;
        initial_state.stack[tree_height_index] += bfe!(perturbation);

        // out-of-range tree heights are tested separately
        let perturbed_tree_height = initial_state.stack[tree_height_index];
        prop_assume!(u32::try_from(perturbed_tree_height).is_ok());
        prop_assume!(perturbed_tree_height.value() < 32);

        // if the expected tree height is increased, additional internal nodes are needed
        initial_state
            .nondeterminism
            .digests
            .extend(additional_bogus_tree_nodes);

        let expected_errors = [
            MerkleVerify::OUT_OF_BOUNDS_LEAF_ERROR_ID,
            MerkleVerify::ROOT_MISMATCH_ERROR_ID,
        ];
        test_assertion_failure(
            &ShadowedReadOnlyAlgorithm::new(MerkleVerify),
            initial_state.into(),
            &expected_errors,
        );
    }

    #[proptest]
    fn merkle_tree_verification_fails_if_tree_height_is_too_large(
        seed: [u8; 32],
        #[strategy(32_u32..)] tree_height: u32,
    ) {
        let mut initial_state = MerkleVerify.pseudorandom_initial_state(seed, None);
        let top_of_stack = initial_state.stack.len() - 1;
        let tree_height_index = top_of_stack - 6;
        initial_state.stack[tree_height_index] = bfe!(tree_height);

        test_assertion_failure(
            &ShadowedReadOnlyAlgorithm::new(MerkleVerify),
            initial_state.into(),
            &[MerkleVerify::TREE_TOO_HIGH_ERROR_ID],
        );
    }

    #[proptest]
    fn merkle_tree_verification_fails_if_tree_height_is_way_too_large(
        seed: [u8; 32],
        #[strategy(u64::from(u32::MAX)..=BFieldElement::MAX)]
        #[map(BFieldElement::new)]
        tree_height: BFieldElement,
    ) {
        let mut initial_state = MerkleVerify.pseudorandom_initial_state(seed, None);
        let top_of_stack = initial_state.stack.len() - 1;
        let tree_height_index = top_of_stack - 6;
        initial_state.stack[tree_height_index] = tree_height;

        negative_test(
            &ShadowedReadOnlyAlgorithm::new(MerkleVerify),
            initial_state.into(),
            &[OpStackError::FailedU32Conversion(tree_height).into()],
        );
    }

    #[proptest]
    fn merkle_tree_verification_fails_if_root_is_disturbed_slightly(
        seed: [u8; 32],
        #[strategy(7_usize..12)] perturbation_index: usize,
        #[filter(#perturbation != 0)] perturbation: i8,
    ) {
        let mut initial_state = MerkleVerify.pseudorandom_initial_state(seed, None);
        let top_of_stack = initial_state.stack.len() - 1;
        initial_state.stack[top_of_stack - perturbation_index] += bfe!(perturbation);

        test_assertion_failure(
            &ShadowedReadOnlyAlgorithm::new(MerkleVerify),
            initial_state.into(),
            &[MerkleVerify::ROOT_MISMATCH_ERROR_ID],
        );
    }
}

#[cfg(test)]
mod bench {
    use super::*;
    use crate::test_prelude::*;

    #[test]
    fn benchmark() {
        ShadowedReadOnlyAlgorithm::new(MerkleVerify).bench()
    }
}
