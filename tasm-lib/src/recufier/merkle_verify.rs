use std::collections::HashMap;

use itertools::Itertools;
use num::Zero;
use rand::rngs::StdRng;
use rand::{Rng, RngCore, SeedableRng};
use triton_vm::{triton_asm, NonDeterminism};
use twenty_first::shared_math::b_field_element::BFieldElement;
use twenty_first::shared_math::other::random_elements;
use twenty_first::util_types::algebraic_hasher::AlgebraicHasher;
use twenty_first::util_types::merkle_tree::{CpuParallel, MerkleTree};
use twenty_first::util_types::merkle_tree_maker::MerkleTreeMaker;

use crate::algorithm::Algorithm;
use crate::library::Library;
use crate::snippet::BasicSnippet;
use crate::{get_init_tvm_stack, Digest, VmHasher};

/// MerkleVerify -- verify that a leaf lives in a Merkle tree,
/// given the root, leaf index, and leaf. The authentication path
/// is non-deterministically divinded. This algorithm asserts
/// that the path is valid; phrased differently, it crashes the
/// VM if it is not.
///
/// inputs:
///
///  - root : Digest
///  - leaf index: u64
///  - leaf : Digest
///
/// outputs:
///
///  - (none)
#[derive(Clone, Debug)]
pub struct MerkleVerify;

impl BasicSnippet for MerkleVerify {
    fn entrypoint() -> String {
        "tasm_recufier_merkle_verify".to_string()
    }

    fn inputs() -> Vec<(crate::snippet::DataType, String)> {
        vec![]
    }

    fn outputs() -> Vec<(crate::snippet::DataType, String)> {
        vec![]
    }

    fn code(library: &mut Library) -> Vec<triton_vm::instruction::LabelledInstruction> {
        let entrypoint = Self::entrypoint();
        triton_asm!(
            {entrypoint}:
                read_io                                  // number of authentication paths to test
                                                         // stack: [num]
                push 0 swap 1 write_mem pop               // store number of APs at RAM address 0
                                                         // stack: []
                read_io read_io read_io read_io read_io  // read Merkle root
                                                         // stack: [r4 r3 r2 r1 r0]
                call {entrypoint}_check_aps              //
                pop pop pop pop pop pop pop              // leave clean stack: Merkle root
                                                         // stack: []
                return                                   // done – should be “return”

                                                         // subroutine: check AP one at a time
                                                         // stack before: [* r4 r3 r2 r1 r0]
                                                         // stack after: [* r4 r3 r2 r1 r0]
            {entrypoint}_check_aps:                      // start function description:
                push 0 read_mem dup 0                     // get number of APs left to check
                                                         // stack: [* r4 r3 r2 r1 r0 0 num_left num_left]
                push 0 eq                                // see if there are authentication paths left
                                                         // stack: [* r4 r3 r2 r1 r0 0 num_left num_left==0]
                skiz return                              // return if no authentication paths left
                push -1 add write_mem pop                // decrease number of authentication paths left to check
                                                         // stack: [* r4 r3 r2 r1 r0]
                call {entrypoint}_get_idx_and_leaf       //
                                                         // stack: [* r4 r3 r2 r1 r0 idx d4 d3 d2 d1 d0 0 0 0 0 0]
                call {entrypoint}_traverse_tree          //
                                                         // stack: [* r4 r3 r2 r1 r0 idx>>2 - - - - - - - - - -]
                call {entrypoint}_assert_tree_top        //
                                                         // stack: [* r4 r3 r2 r1 r0]
                recurse                                  // check next AP

                                                         // subroutine: read index & leaf
                                                         // stack before: [*]
                                                         // stack afterwards: [* idx d4 d3 d2 d1 d0 0 0 0 0 0]
            {entrypoint}_get_idx_and_leaf:               // start function description:
                read_io                                  // read node index
                read_io read_io read_io read_io read_io  // read leaf's value
                push 0 push 0 push 0 push 0 push 0       // add zeroes as preparation for divine sibling
                return                                   //

                                                         // subroutine: go up tree
                                                         // stack before: [* idx - - - - - - - - - -]
                                                         // stack after: [* idx>>2 - - - - - - - - - -]
            {entrypoint}_traverse_tree:                  // start function description:
                dup 10 push 1 eq skiz return              // break loop if node index is 1
                divine_sibling hash recurse              // move up one level in the Merkle tree

                                                         // subroutine: compare digests
                                                         // stack before: [* r4 r3 r2 r1 r0 idx a b c d e - - - - -]
                                                         // stack after: [* r4 r3 r2 r1 r0]
            {entrypoint}_assert_tree_top:                // start function description:
                pop pop pop pop pop                      // remove unnecessary “0”s from hashing
                                                         // stack: [* r4 r3 r2 r1 r0 idx a b c d e]
                swap 1 swap 2 swap 3 swap 4 swap 5
                                                         // stack: [* r4 r3 r2 r1 r0 a b c d e idx]
                assert                                   //
                                                         // stack: [* r4 r3 r2 r1 r0 a b c d e]
                assert_vector                            // actually compare to root of tree
                pop pop pop pop pop                      // clean up stack, leave only one root
                return
        )
    }
}

/// TVM assembly to verify Merkle authentication paths
///
/// input: number of authentication paths, merkle root, authentication paths,
/// each one preceded by the corresponding node index in the merkle tree,
/// where the authentication path starts
///
/// output: Result<(), VMFail>
///
/// uses RAM at address 0 to store the number of authentication paths
impl Algorithm for MerkleVerify {
    fn rust_shadow(
        stack: &mut Vec<BFieldElement>,
        memory: &mut HashMap<BFieldElement, BFieldElement>,
        nondeterminism: triton_vm::NonDeterminism<BFieldElement>,
    ) {
        // read inputs:
        //
        //  - root : Digest
        //  - leaf index: u64
        //  - leaf : Digest
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

        let mut input_index: usize = 6;
        let mut sibling_index: usize = 0;
        input_index += 1;

        let mut node_digest = leaf;
        while leaf_index != 1 {
            let sibling = nondeterminism.digests[sibling_index];
            if leaf_index & 1 == 0 {
                node_digest = VmHasher::hash_pair(&node_digest, &sibling);
            } else {
                node_digest = VmHasher::hash_pair(&sibling, &node_digest);
            }
            sibling_index += 1;
            leaf_index /= 2;
        }
        assert_eq!(node_digest, root);
    }

    fn pseudorandom_initial_state(
        seed: [u8; 32],
        bench_case: crate::snippet_bencher::BenchmarkCase,
    ) -> (
        Vec<BFieldElement>,
        HashMap<BFieldElement, BFieldElement>,
        NonDeterminism<BFieldElement>,
    ) {
        {
            let tree_height = match bench_case {
                CommonCase => 6,
                WorstCase => 20,
            };

            let mut rng: StdRng = SeedableRng::from_seed(seed);

            // sample unconstrained inputs directly
            let index = rng.next_u32() % (1 << tree_height);
            let path: Vec<Digest> = (0..tree_height).map(|_| rng.gen()).collect_vec();
            let leaf: Digest = rng.gen();

            // walk up tree to calculate root
            let mut leaf_index = index;
            let mut sibling_index = 0;
            let mut node_digest = leaf;
            while leaf_index != 1 {
                let sibling = path[sibling_index];
                if leaf_index & 1 == 0 {
                    node_digest = VmHasher::hash_pair(&node_digest, &sibling);
                } else {
                    node_digest = VmHasher::hash_pair(&sibling, &node_digest);
                }
                sibling_index += 1;
                leaf_index /= 2;
            }
            let root = node_digest;

            // prepare stack
            let mut stack = get_init_tvm_stack();
            for r in root.0.into_iter().rev() {
                stack.push(r);
            }
            stack.push(BFieldElement::new(index as u64));
            for l in leaf.0.into_iter().rev() {
                stack.push(l);
            }

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
    use std::collections::HashMap;

    use rand::random;
    use rand::thread_rng;
    use twenty_first::shared_math::b_field_element::BFieldElement;
    use twenty_first::util_types::algebraic_hasher::AlgebraicHasher;
    use twenty_first::util_types::merkle_tree::CpuParallel;
    use twenty_first::util_types::merkle_tree::MerkleTree;
    use twenty_first::util_types::merkle_tree_maker::MerkleTreeMaker;

    use crate::get_init_tvm_stack;
    use crate::snippet_bencher::BenchmarkCase;
    use crate::test_helpers::test_rust_equivalence_given_input_values;
    use crate::test_helpers::test_rust_equivalence_multiple;
    use crate::VmHasher;

    use super::MerkleVerify;

    #[test]
    fn merkle_verify() {
        test_rust_equivalence_multiple(&MerkleVerify, true);
    }

    #[should_panic]
    #[test]
    fn merkle_tree_ap_verify_negative_test() {
        let (mut stack, memory, nondeterminism) =
            MerkleVerify::pseudorandom_initial_state(thread_rng().gen(), BenchmarkCase::CommonCase);

        // modify index so as to make it invalid
        stack[5] = thread_rng().gen();

        test_rust_equivalence_given_input_values(
            &MerkleVerify,
            stack,
            &standard_input,
            &bad_secret_input,
            &mut HashMap::default(),
            0,
            None,
        );
    }
}
