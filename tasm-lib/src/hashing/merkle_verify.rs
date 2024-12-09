use triton_vm::prelude::*;

use crate::data_type::DataType;
use crate::library::Library;
use crate::traits::basic_snippet::BasicSnippet;

pub const MERKLE_AUTHENTICATION_ROOT_MISMATCH_ERROR: i128 = 2;

/// Verify membership in a Merkle tree.
///
/// MerkleVerify -- verify that a leaf lives in a Merkle tree,
/// given the root, leaf index, and leaf. The authentication path
/// is non-deterministically divined. This algorithm asserts
/// that the path is valid; phrased differently, it crashes the
/// VM if it is not.
#[derive(Clone, Debug)]
pub struct MerkleVerify;

impl BasicSnippet for MerkleVerify {
    fn inputs(&self) -> Vec<(DataType, String)> {
        vec![
            (DataType::Digest, "root".to_string()),
            (DataType::U32, "tree_height".to_string()),
            (DataType::U32, "leaf_index".to_string()),
            (DataType::Digest, "leaf".to_string()),
        ]
    }

    fn outputs(&self) -> Vec<(DataType, String)> {
        vec![]
    }

    fn entrypoint(&self) -> String {
        "tasmlib_hashing_merkle_verify".to_string()
    }

    fn code(&self, _library: &mut Library) -> Vec<LabelledInstruction> {
        let entrypoint = self.entrypoint();
        let traverse_tree = format!("{entrypoint}_traverse_tree");
        let tree_height_is_not_zero = format!("{entrypoint}_tree_height_is_not_zero");
        triton_asm!(
            // BEFORE: _ [root; 5] tree_height leaf_index [leaf; 5]
            // AFTER:  _
            {entrypoint}:
                /* Assert reasonable tree height
                 *
                 * Don't rely on `pow`'s `is_u32` and `assert leaf_index < num_leaves` only:
                 * Since bfe!(2)^3784253760 == 1 ∧ 3784253760 < 4294967295 == u32::MAX, weird
                 * things are possible. It is not immediately obvious how this translates into
                 * an attack, but there's no point in leaving a potential attack vector open.
                 */
                push 32
                dup 7
                lt
                assert error_id 0

                /* Calculate node index from tree height and leaf index */
                dup 6
                push 2
                pow
                // _ [root; 5] tree_height leaf_index [leaf; 5] num_leaves

                dup 0 dup 7 lt
                // _ [root; 5] tree_height leaf_index [leaf; 5] num_leaves (leaf_index < num_leaves)

                assert error_id 1
                // _ [root; 5] tree_height leaf_index [leaf; 5] num_leaves

                pick 6
                add
                // _ [root; 5] tree_height [leaf; 5] node_index

                place 5
                // _ [root; 5] tree_height node_index [leaf; 5]

                dup 6
                skiz
                    call {tree_height_is_not_zero}

                // _ [root; 5] [0|1] [0|1] [calculated_root; 5]

                /* compare calculated and provided root */

                pick 5
                pick 6
                pop 2
                // _ [root; 5] [calculated_root; 5]

                assert_vector error_id {MERKLE_AUTHENTICATION_ROOT_MISMATCH_ERROR}
                pop 5

                return

            {tree_height_is_not_zero}:
                // _ [root; 5] tree_height node_index [leaf; 5]

                push 1
                swap 7
                pop 1
                // _ [root; 5] 1 node_index [leaf; 5]

                call {traverse_tree}
                // _ [root; 5] 1 1 [calculated_root; 5]

                return

            {traverse_tree}:
                merkle_step
                recurse_or_return

        )
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;
    use std::collections::VecDeque;

    use itertools::Itertools;
    use proptest::collection::vec;
    use proptest::prop_assume;
    use proptest_arbitrary_interop::arb;
    use rand::prelude::*;
    use test_strategy::proptest;

    use super::*;
    use crate::prelude::Tip5;
    use crate::snippet_bencher::BenchmarkCase;
    use crate::test_helpers::negative_test;
    use crate::test_helpers::test_assertion_failure;
    use crate::traits::read_only_algorithm::ReadOnlyAlgorithm;
    use crate::traits::read_only_algorithm::ReadOnlyAlgorithmInitialState;
    use crate::traits::read_only_algorithm::ShadowedReadOnlyAlgorithm;
    use crate::traits::rust_shadow::RustShadow;

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

        test_assertion_failure(
            &ShadowedReadOnlyAlgorithm::new(MerkleVerify),
            initial_state.into(),
            &[1, 2],
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
            &[0],
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
            &[2],
        );
    }

    impl ReadOnlyAlgorithm for MerkleVerify {
        fn rust_shadow(
            &self,
            stack: &mut Vec<BFieldElement>,
            _memory: &HashMap<BFieldElement, BFieldElement>,
            _nd_tokens: VecDeque<BFieldElement>,
            nd_digests: VecDeque<Digest>,
        ) {
            // BEFORE: _ [root; 5] tree_height leaf_index [leaf; 5]
            // AFTER:  _
            let pop_digest_from = |stack: &mut Vec<BFieldElement>| {
                Digest::new([
                    stack.pop().unwrap(),
                    stack.pop().unwrap(),
                    stack.pop().unwrap(),
                    stack.pop().unwrap(),
                    stack.pop().unwrap(),
                ])
            };

            let leaf = pop_digest_from(stack);
            let leaf_index: u32 = stack.pop().unwrap().try_into().unwrap();
            let tree_height: u32 = stack.pop().unwrap().try_into().unwrap();
            let root = pop_digest_from(stack);

            let num_leaves = 1 << tree_height;
            assert!(leaf_index < num_leaves);

            let mut node_digest = leaf;
            let mut sibling_height = 0;
            let mut node_index = leaf_index + num_leaves;
            while node_index != 1 {
                let sibling = nd_digests[sibling_height];
                let node_is_left_sibling = node_index % 2 == 0;
                node_digest = if node_is_left_sibling {
                    Tip5::hash_pair(node_digest, sibling)
                } else {
                    Tip5::hash_pair(sibling, node_digest)
                };
                sibling_height += 1;
                node_index /= 2;
            }
            assert_eq!(node_digest, root);
        }

        fn pseudorandom_initial_state(
            &self,
            seed: [u8; 32],
            maybe_bench_case: Option<BenchmarkCase>,
        ) -> ReadOnlyAlgorithmInitialState {
            {
                // BEFORE: _ [root; 5] tree_height leaf_index [leaf; 5]
                let mut rng = StdRng::from_seed(seed);
                let tree_height = match maybe_bench_case {
                    Some(BenchmarkCase::CommonCase) => 6,
                    Some(BenchmarkCase::WorstCase) => 20,
                    None => rng.gen_range(1..20),
                };

                // sample unconstrained inputs directly
                let num_leaves = 1 << tree_height;
                let leaf_index = rng.gen_range(0..num_leaves);
                let path: Vec<Digest> = (0..tree_height).map(|_| rng.gen()).collect_vec();
                let leaf: Digest = rng.gen();

                // walk up tree to calculate root
                let mut node_digest = leaf;
                let mut node_index = leaf_index + num_leaves;
                for &sibling in path.iter() {
                    let node_is_left_sibling = node_index % 2 == 0;
                    node_digest = match node_is_left_sibling {
                        true => Tip5::hash_pair(node_digest, sibling),
                        false => Tip5::hash_pair(sibling, node_digest),
                    };
                    node_index /= 2;
                }
                let root = node_digest;

                // prepare stack
                let mut stack = MerkleVerify.init_stack_for_isolated_run();
                for r in root.reversed().values().into_iter() {
                    stack.push(r);
                }
                stack.push(BFieldElement::new(tree_height));
                stack.push(BFieldElement::new(leaf_index));
                for l in leaf.reversed().values().into_iter() {
                    stack.push(l);
                }

                let nondeterminism = NonDeterminism::default().with_digests(path);
                ReadOnlyAlgorithmInitialState {
                    stack,
                    nondeterminism,
                }
            }
        }
    }
}

#[cfg(test)]
mod bench {
    use super::MerkleVerify;
    use crate::traits::read_only_algorithm::ShadowedReadOnlyAlgorithm;
    use crate::traits::rust_shadow::RustShadow;

    #[test]
    fn merkle_verify_bench() {
        ShadowedReadOnlyAlgorithm::new(MerkleVerify).bench()
    }
}
