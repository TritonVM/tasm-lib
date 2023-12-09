use std::collections::HashMap;

use itertools::Itertools;
use rand::rngs::StdRng;
use rand::{Rng, SeedableRng};
use triton_vm::{triton_asm, NonDeterminism};
use twenty_first::shared_math::b_field_element::BFieldElement;
use twenty_first::util_types::algebraic_hasher::AlgebraicHasher;

use crate::algorithm::Algorithm;
use crate::library::Library;
use crate::snippet::{BasicSnippet, DataType};
use crate::snippet_bencher::BenchmarkCase;
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
    fn entrypoint(&self) -> String {
        "tasm_recufier_merkle_verify".to_string()
    }

    fn inputs(&self) -> Vec<(crate::snippet::DataType, String)> {
        vec![
            (DataType::Digest, "root".to_string()),
            (DataType::U32, "leaf_index".to_string()),
            (DataType::Digest, "leaf".to_string()),
            (DataType::U32, "tree_height".to_string()),
        ]
    }

    fn outputs(&self) -> Vec<(crate::snippet::DataType, String)> {
        vec![]
    }

    fn code(&self, _library: &mut Library) -> Vec<triton_vm::instruction::LabelledInstruction> {
        let entrypoint = self.entrypoint();
        let traverse_tree = format!("{entrypoint}_traverse_tree");
        let assert_tree_top = format!("{entrypoint}_assert_tree_top");
        triton_asm!(
            // BEFORE: _ r4 r3 r2 r1 r0 leaf_index l4 l3 l2 l1 l0 tree_height
            // AFTER: _
            {entrypoint}:
                // calculate node index from tree height and leaf index:
                push 2 pow                               // stack: [* r4 r3 r2 r1 r0 leaf_index l4 l3 l2 l1 l0 2^tree_height]
                dup 6 add                                // stack: [* r4 r3 r2 r1 r0 leaf_index l4 l3 l2 l1 l0 (2^tree_height + leaf_index)]
                swap 6 pop                               // stack: [* r4 r3 r2 r1 r0 node_index l4 l3 l2 l1 l0]

                // walt up tree, to the root
                push 0 push 0 push 0 push 0 push 0       // stack: [* r4 r3 r2 r1 f0 node_index l4 l3 l2 l1 l0 0 0 0 0 0]
                call {traverse_tree}                     //
                                                         // stack: [* r4 r3 r2 r1 r0 idx>>1 - - - - - - - - - -]
                call {assert_tree_top}                   //
                                                         // stack: [* r4 r3 r2 r1 r0]
                pop pop pop pop pop                      // _
                return

            // subroutine: go up tree
            // stack before: [* node_index - - - - - - - - - -]
            // stack after: [* node_index>>1 - - - - - - - - - -]
            {traverse_tree}:                             // start function description:
                dup 10 push 1 eq skiz return             // break loop if node leaf_index is 1
                divine_sibling hash recurse              // move up one level in the Merkle tree

            // subroutine: compare digests
            // stack before: [* r4 r3 r2 r1 r0 idx a b c d e - - - - -]
            // stack after: [* r4 r3 r2 r1 r0]
            {assert_tree_top}:                           // start function description:
                pop pop pop pop pop                      // remove unnecessary “0”s from hashing
                                                         // stack: [* r4 r3 r2 r1 r0 idx a b c d e]
                swap 1 swap 2 swap 3 swap 4 swap 5
                                                         // stack: [* r4 r3 r2 r1 r0 a b c d e idx]
                assert                                   // idx=1 for iff current node is root
                                                         // stack: [* r4 r3 r2 r1 r0 a b c d e]
                assert_vector                            // actually compare to root of tree
                pop pop pop pop pop                      // clean up stack, leave only one root
                return
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
        // read inputs:
        //
        //  - root : Digest
        //  - leaf index: u32
        //  - leaf : Digest
        //  - tree height: u32
        let tree_height: u32 = stack.pop().unwrap().value().try_into().unwrap();
        let leaf = Digest::new([
            stack.pop().unwrap(),
            stack.pop().unwrap(),
            stack.pop().unwrap(),
            stack.pop().unwrap(),
            stack.pop().unwrap(),
        ]);
        let leaf_index: u32 = stack.pop().unwrap().value().try_into().unwrap();
        let root = Digest::new([
            stack.pop().unwrap(),
            stack.pop().unwrap(),
            stack.pop().unwrap(),
            stack.pop().unwrap(),
            stack.pop().unwrap(),
        ]);

        let mut node_digest = leaf;
        let mut sibling_height = 0;
        let mut node_index = leaf_index + (1 << tree_height);
        while node_index != 1 {
            let sibling = nondeterminism.digests[sibling_height];
            if node_index & 1 == 0 {
                node_digest = VmHasher::hash_pair(node_digest, sibling);
            } else {
                node_digest = VmHasher::hash_pair(sibling, node_digest);
            }
            sibling_height += 1;
            node_index /= 2;
        }
        assert_eq!(node_digest, root);
    }

    fn pseudorandom_initial_state(
        &self,
        seed: [u8; 32],
        maybe_bench_case: Option<BenchmarkCase>,
    ) -> (
        Vec<BFieldElement>,
        HashMap<BFieldElement, BFieldElement>,
        NonDeterminism<BFieldElement>,
    ) {
        {
            let mut rng: StdRng = SeedableRng::from_seed(seed);
            let tree_height = if let Some(bench_case) = maybe_bench_case {
                match bench_case {
                    BenchmarkCase::CommonCase => 6,
                    BenchmarkCase::WorstCase => 20,
                }
            } else {
                rng.gen_range(0..20)
            };

            // sample unconstrained inputs directly
            let leaf_index = rng.gen_range(0..(1 << tree_height));
            let path: Vec<Digest> = (0..tree_height).map(|_| rng.gen()).collect_vec();
            let leaf: Digest = rng.gen();

            // walk up tree to calculate root
            let mut node_digest = leaf;
            let mut node_index = leaf_index + (1 << tree_height);
            let mut sibling_height = 0;
            while node_index != 1 {
                let sibling = path[sibling_height];
                if node_index & 1 == 0 {
                    // Node is a left child
                    node_digest = VmHasher::hash_pair(node_digest, sibling);
                } else {
                    // Node is a right child
                    node_digest = VmHasher::hash_pair(sibling, node_digest);
                }
                sibling_height += 1;
                node_index /= 2;
            }
            let root = node_digest;

            // prepare stack
            let mut stack = empty_stack();
            for r in root.0.into_iter().rev() {
                stack.push(r);
            }
            stack.push(BFieldElement::new(leaf_index as u64));
            for l in leaf.0.into_iter().rev() {
                stack.push(l);
            }

            stack.push(BFieldElement::new(tree_height as u64));

            // prepare non-determinism
            let nondeterminism = NonDeterminism::new(vec![]).with_digests(path);

            // prepare memory
            let memory = HashMap::<BFieldElement, BFieldElement>::new();

            (stack, memory, nondeterminism)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::cell::RefCell;
    use std::rc::Rc;

    use rand::thread_rng;
    use twenty_first::util_types::algebraic_hasher::Domain;

    use crate::algorithm::ShadowedAlgorithm;
    use crate::linker::link_for_isolated_run;
    use crate::snippet::RustShadow;
    use crate::{execute_with_terminal_state, prepend_state_preparation, VmHasherState};

    use super::MerkleVerify;

    #[test]
    fn merkle_verify_test() {
        ShadowedAlgorithm::new(MerkleVerify).test()
    }

    #[test]
    fn negative_test() {
        let seed: [u8; 32] = thread_rng().gen();
        for i in 0..6 {
            let (mut stack, memory, mut nondeterminism) =
                MerkleVerify.pseudorandom_initial_state(seed, None);
            let len = stack.len();

            match i {
                0 => {
                    // change height; should fail
                    stack[len - 1].increment();
                }
                1 => {
                    // change height; should fail
                    stack[len - 1].decrement();
                }
                2 => {
                    // change leaf; should fail
                    stack[len - 2].increment();
                }
                3 => {
                    // change index; should fail
                    stack[len - 6].increment()
                }
                4 => {
                    // change index; should fail
                    stack[len - 6].decrement()
                }
                5 => {
                    // change root; should fail
                    stack[len - 7].increment()
                }
                _ => {}
            }

            // test rust/tasm equivalence
            // in this case: verify that they both fail

            let stdin = vec![];

            // run rust shadow
            let rust_result = std::panic::catch_unwind(|| {
                let mut rust_stack = stack.clone();
                let mut rust_memory = memory.clone();
                ShadowedAlgorithm::new(MerkleVerify.clone()).rust_shadow_wrapper(
                    &stdin,
                    &nondeterminism,
                    &mut rust_stack,
                    &mut rust_memory,
                    &mut VmHasherState::new(Domain::VariableLength),
                )
            });

            if let Ok(result) = &rust_result {
                println!("rust result: {:?}\ni: {}", result, i);
            }

            // run tvm
            let code = link_for_isolated_run(Rc::new(RefCell::new(MerkleVerify)), 0);
            let program = prepend_state_preparation(&code, &stack);
            let tvm_result =
                execute_with_terminal_state(&program, &stdin, &mut nondeterminism, None);
            if let Ok(result) = &tvm_result {
                println!("tasm result: {}\ni: {}", result, i);
            }

            assert!(rust_result.is_err());
            assert!(tvm_result.is_err());
        }
    }
}

#[cfg(test)]
mod bench {
    use crate::{algorithm::ShadowedAlgorithm, snippet::RustShadow};

    use super::MerkleVerify;

    #[test]
    fn merkle_verify_bench() {
        ShadowedAlgorithm::new(MerkleVerify).bench()
    }
}
