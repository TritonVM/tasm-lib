use crate::data_type::DataType;
use crate::library::Library;
use crate::traits::basic_snippet::BasicSnippet;
use triton_vm::prelude::*;

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

                /* Calculate node index from tree height and leaf index */
                dup 6
                push 2
                pow
                // _ [root; 5] tree_height leaf_index [leaf; 5] num_leaves

                dup 0 dup 7 lt
                // _ [root; 5] tree_height leaf_index [leaf; 5] num_leaves (leaf_index < num_leaves)

                assert
                // _ [root; 5] tree_height leaf_index [leaf; 5] num_leaves

                dup 6
                add
                // _ [root; 5] tree_height leaf_index [leaf; 5] node_index

                swap 6
                pop 1
                // _ [root; 5] tree_height node_index [leaf; 5]

                dup 6
                skiz
                    call {tree_height_is_not_zero}

                // _ [root; 5] [0|1] [0|1] [calculated_root; 5]

                /* compare calculated and provided root */
                // _ [root; 5] g1 g0 cr4 cr3 cr2 cr1 cr0

                swap 2
                swap 4
                swap 6
                // _ [root; 5] cr4 g0 cr2 cr3 cr0 cr1 g1

                pop 1
                // _ [root; 5] cr4 g0 cr2 cr3 cr0 cr1

                swap 2
                swap 4
                // _ [root; 5] cr4 cr3 cr2 cr1 cr0 g0

                pop 1
                // _ [root; 5] cr4 cr3 cr2 cr1 cr0
                // _ [root; 5] [calculated_root; 5]

                assert_vector
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

    use itertools::Itertools;
    use rand::random;
    use rand::rngs::StdRng;
    use rand::thread_rng;
    use rand::Rng;
    use rand::SeedableRng;
    use triton_vm::twenty_first::util_types::algebraic_hasher::AlgebraicHasher;

    use crate::snippet_bencher::BenchmarkCase;
    use crate::test_helpers::negative_test;
    use crate::traits::algorithm::Algorithm;
    use crate::traits::algorithm::AlgorithmInitialState;
    use crate::traits::algorithm::ShadowedAlgorithm;
    use crate::traits::rust_shadow::RustShadow;
    use crate::VmHasher;

    use super::*;

    #[test]
    fn merkle_verify_test() {
        ShadowedAlgorithm::new(MerkleVerify).test()
    }

    #[test]
    fn merkle_verify_negative_test() {
        let seed: [u8; 32] = thread_rng().gen();
        for i in 0..=4 {
            let mut init_state = MerkleVerify.pseudorandom_initial_state(seed, None);
            let len = init_state.stack.len();
            let height: usize = len - 7;
            let leaf: usize = len - 1;
            let leaf_index: usize = len - 6;
            let root: usize = len - 8;

            // BEFORE: _ [root; 5] tree_height leaf_index [leaf; 5]
            let allowed_error_codes = match i {
                0 => {
                    println!("now testing: too high height");
                    init_state.nondeterminism.digests.push(random());
                    init_state.stack[height].increment();
                    vec![
                        InstructionError::VectorAssertionFailed(0),
                        InstructionError::AssertionFailed,
                    ]
                }
                1 => {
                    println!("now testing: too small height");
                    init_state.nondeterminism.digests.push(random());
                    init_state.stack[height].decrement();
                    vec![
                        InstructionError::VectorAssertionFailed(0),
                        InstructionError::AssertionFailed,
                    ]
                }
                2 => {
                    println!("now testing: corrupt leaf");
                    init_state.stack[leaf].increment();
                    vec![InstructionError::VectorAssertionFailed(0)]
                }
                3 => {
                    println!("now testing: wrong leaf index");
                    init_state.stack[leaf_index] =
                        BFieldElement::new(init_state.stack[leaf_index].value() ^ 1);
                    vec![InstructionError::VectorAssertionFailed(0)]
                }
                4 => {
                    println!("now testing: corrupt root");
                    init_state.stack[root].increment();
                    vec![InstructionError::VectorAssertionFailed(0)]
                }
                _ => unreachable!(),
            };

            negative_test(
                &ShadowedAlgorithm::new(MerkleVerify),
                init_state.into(),
                &allowed_error_codes,
            );
        }
    }

    impl Algorithm for MerkleVerify {
        fn rust_shadow(
            &self,
            stack: &mut Vec<BFieldElement>,
            _memory: &mut HashMap<BFieldElement, BFieldElement>,
            nondeterminism: &NonDeterminism,
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
                let sibling = nondeterminism.digests[sibling_height];
                let node_is_left_sibling = node_index % 2 == 0;
                node_digest = match node_is_left_sibling {
                    true => VmHasher::hash_pair(node_digest, sibling),
                    false => VmHasher::hash_pair(sibling, node_digest),
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
        ) -> AlgorithmInitialState {
            {
                // BEFORE: _ [root; 5] tree_height leaf_index [leaf; 5]
                let mut rng: StdRng = SeedableRng::from_seed(seed);
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
                        true => VmHasher::hash_pair(node_digest, sibling),
                        false => VmHasher::hash_pair(sibling, node_digest),
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
                AlgorithmInitialState {
                    stack,
                    nondeterminism,
                }
            }
        }
    }
}

#[cfg(test)]
mod bench {
    use crate::traits::algorithm::ShadowedAlgorithm;
    use crate::traits::rust_shadow::RustShadow;

    use super::MerkleVerify;

    #[test]
    fn merkle_verify_bench() {
        ShadowedAlgorithm::new(MerkleVerify).bench()
    }
}
