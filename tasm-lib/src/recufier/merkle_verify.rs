use std::collections::HashMap;

use itertools::Itertools;
use rand::rngs::StdRng;
use rand::{Rng, SeedableRng};
use triton_vm::instruction::LabelledInstruction;
use triton_vm::{triton_asm, NonDeterminism};
use twenty_first::shared_math::b_field_element::BFieldElement;
use twenty_first::util_types::algebraic_hasher::AlgebraicHasher;

use crate::data_type::DataType;
use crate::library::Library;
use crate::snippet_bencher::BenchmarkCase;
use crate::traits::algorithm::{Algorithm, AlgorithmInitialState};
use crate::traits::basic_snippet::BasicSnippet;
use crate::{empty_stack, Digest, VmHasher};

/// MerkleVerify -- verify that a leaf lives in a Merkle tree,
/// given the root, leaf index, and leaf. The authentication path
/// is non-deterministically divined. This algorithm asserts
/// that the path is valid; phrased differently, it crashes the
/// VM if it is not.
///
/// inputs:
///
///  - root : Digest
///  - leaf index: u32
///  - leaf : Digest
///  - tree height: u32
///
/// outputs:
///
///  - (none)
#[derive(Clone, Debug)]
pub struct MerkleVerify;

impl BasicSnippet for MerkleVerify {
    fn inputs(&self) -> Vec<(DataType, String)> {
        vec![
            (DataType::Digest, "root".to_string()),
            (DataType::U32, "leaf_index".to_string()),
            (DataType::Digest, "leaf".to_string()),
            (DataType::U32, "tree_height".to_string()),
        ]
    }

    fn outputs(&self) -> Vec<(DataType, String)> {
        vec![]
    }

    fn entrypoint(&self) -> String {
        "tasm_recufier_merkle_verify".to_string()
    }

    fn code(&self, _library: &mut Library) -> Vec<LabelledInstruction> {
        let entrypoint = self.entrypoint();
        let traverse_tree = format!("{entrypoint}_traverse_tree");
        triton_asm!(
            // BEFORE: _ [root; 5] leaf_index [leaf; 5] tree_height
            // AFTER:  _
            {entrypoint}:
                // calculate node index from tree height and leaf index:
                push 2 pow              // _ [root; 5] leaf_index [leaf; 5] num_leaves
                dup 0 dup 7 lt          // _ [root; 5] leaf_index [leaf; 5] num_leaves (leaf_index < num_leaves)
                assert                  // _ [root; 5] leaf_index [leaf; 5] num_leaves
                dup 6 add               // _ [root; 5] leaf_index [leaf; 5] node_index
                swap 6 pop 1            // _ [root; 5] node_index [leaf; 5]
                call {traverse_tree}    // _ [root; 5] 1 [root'; 5]
                swap 1 swap 2 swap 3
                swap 4 swap 5           // _ [root; 5] [root'; 5] 1
                pop 1                   // _ [root; 5] [root'; 5]
                assert_vector           // _ [root; 5]
                pop 5                   // _
                return

            // BEFORE:    _ node_index [leaf; 5]
            // INVARIANT: _ (node_index >> i) [some_digest; 5]
            // AFTER:     _ 1 [root'; 5]
            {traverse_tree}:
                dup 5 push 1 eq skiz return         // break loop if node_index is 1
                divine_sibling hash recurse         // move up one level in the Merkle tree
        )
    }
}

impl Algorithm for MerkleVerify {
    fn rust_shadow(
        &self,
        stack: &mut Vec<BFieldElement>,
        _memory: &mut HashMap<BFieldElement, BFieldElement>,
        nondeterminism: &NonDeterminism<BFieldElement>,
    ) {
        let pop_digest_from = |stack: &mut Vec<BFieldElement>| {
            Digest::new([
                stack.pop().unwrap(),
                stack.pop().unwrap(),
                stack.pop().unwrap(),
                stack.pop().unwrap(),
                stack.pop().unwrap(),
            ])
        };

        let tree_height: u32 = stack.pop().unwrap().try_into().unwrap();
        let leaf = pop_digest_from(stack);
        let leaf_index: u32 = stack.pop().unwrap().try_into().unwrap();
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
            let mut rng: StdRng = SeedableRng::from_seed(seed);
            let tree_height = match maybe_bench_case {
                Some(BenchmarkCase::CommonCase) => 6,
                Some(BenchmarkCase::WorstCase) => 20,
                None => rng.gen_range(0..20),
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
            let mut stack = empty_stack();
            for r in root.reversed().values().into_iter() {
                stack.push(r);
            }
            stack.push(BFieldElement::new(leaf_index));
            for l in leaf.reversed().values().into_iter() {
                stack.push(l);
            }
            stack.push(BFieldElement::new(tree_height));

            let nondeterminism = NonDeterminism::default().with_digests(path);
            AlgorithmInitialState {
                stack,
                nondeterminism,
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use std::cell::RefCell;
    use std::rc::Rc;

    use rand::thread_rng;
    use triton_vm::Program;

    use crate::execute_with_terminal_state;
    use crate::linker::link_for_isolated_run;
    use crate::traits::algorithm::ShadowedAlgorithm;
    use crate::traits::rust_shadow::RustShadow;

    use super::MerkleVerify;
    use super::*;

    #[test]
    fn merkle_verify_test() {
        ShadowedAlgorithm::new(MerkleVerify).test()
    }

    #[test]
    fn negative_test() {
        let seed: [u8; 32] = thread_rng().gen();
        for i in 0..6 {
            let AlgorithmInitialState {
                mut stack,
                nondeterminism,
            } = MerkleVerify.pseudorandom_initial_state(seed, None);
            let len = stack.len();

            match i {
                0 => stack[len - 1].increment(), // height
                1 => stack[len - 1].decrement(),
                2 => stack[len - 2].increment(), // leaf
                3 => stack[len - 6].increment(), // leaf index
                4 => stack[len - 6].decrement(),
                5 => stack[len - 7].increment(), // root
                _ => unreachable!(),
            }

            // test rust/tasm equivalence
            // in this case: verify that they both fail

            let stdin = vec![];

            // run rust shadow
            let rust_result = std::panic::catch_unwind(|| {
                let mut rust_stack = stack.clone();
                let mut rust_memory = nondeterminism.ram.clone();
                ShadowedAlgorithm::new(MerkleVerify.clone()).rust_shadow_wrapper(
                    &stdin,
                    &nondeterminism,
                    &mut rust_stack,
                    &mut rust_memory,
                    &mut None,
                )
            });

            if let Ok(result) = &rust_result {
                println!("rust result: {:?}\ni: {}", result, i);
            }

            // run tvm
            let code = link_for_isolated_run(Rc::new(RefCell::new(MerkleVerify)), 0);
            let program = Program::new(&code);
            let tvm_result =
                execute_with_terminal_state(&program, &stdin, &stack, &nondeterminism, None);
            if let Ok(result) = &tvm_result {
                println!("tasm result: {result}\ni: {i}");
            }

            assert!(rust_result.is_err());
            assert!(tvm_result.is_err());
        }
    }
}

#[cfg(test)]
mod bench {
    use super::MerkleVerify;
    use crate::traits::algorithm::ShadowedAlgorithm;
    use crate::traits::rust_shadow::RustShadow;

    #[test]
    fn merkle_verify_bench() {
        ShadowedAlgorithm::new(MerkleVerify).bench()
    }
}
