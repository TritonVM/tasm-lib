use std::collections::HashMap;

use crate::library::Library;
use crate::snippet_trait::Snippet;
use num::Zero;
use twenty_first::shared_math::rescue_prime_regular::RescuePrimeRegular;
use twenty_first::shared_math::{b_field_element::BFieldElement, rescue_prime_digest::Digest};
use twenty_first::util_types::algebraic_hasher::AlgebraicHasher;

pub struct MtApVerify();
/// TVM assembly to verify Merkle authentication paths
///
/// input: number of authentication paths, merkle root, authentication paths,
/// each one preceded by the corresponding node index in the merkle tree,
/// where the authentication path starts
///
/// output: Result<(), VMFail>
///
/// uses RAM at address 0 to store the number of authentication paths
impl Snippet for MtApVerify {
    fn stack_diff() -> isize {
        0
    }

    fn entrypoint() -> &'static str {
        "mt_ap_verify"
    }

    fn function_body(_library: &mut Library) -> String {
        let entrypoint = Self::entrypoint();
        format!(
            "
            {entrypoint}:
                read_io                                  // number of authentication paths to test
                                                         // stack: [num]
                push 0 swap1 write_mem pop pop           // store number of APs at RAM address 0
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
                push 0 push 0 read_mem dup0              // get number of APs left to check
                                                         // stack: [* r4 r3 r2 r1 r0 0 num_left num_left]
                push 0 eq                                // see if there are authentication paths left
                                                         // stack: [* r4 r3 r2 r1 r0 0 num_left num_left==0]
                skiz return                              // return if no authentication paths left
                push -1 add write_mem pop pop            // decrease number of authentication paths left to check
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
                dup10 push 1 eq skiz return              // break loop if node index is 1
                divine_sibling hash recurse              // move up one level in the Merkle tree

                                                         // subroutine: compare digests
                                                         // stack before: [* r4 r3 r2 r1 r0 idx a b c d e - - - - -]
                                                         // stack after: [* r4 r3 r2 r1 r0]
            {entrypoint}_assert_tree_top:                // start function description:
                pop pop pop pop pop                      // remove unnecessary “0”s from hashing
                                                         // stack: [* r4 r3 r2 r1 r0 idx a b c d e]
                swap1 swap2 swap3 swap4 swap5
                                                         // stack: [* r4 r3 r2 r1 r0 a b c d e idx]
                assert                                   //
                                                         // stack: [* r4 r3 r2 r1 r0 a b c d e]
                assert_vector                            // actually compare to root of tree
                pop pop pop pop pop                      // clean up stack, leave only one root
                return
        ")
    }

    //needs secret input with merkle siblings along all authentication paths
    //will need to update code when we switch to a new hash function
    fn rust_shadowing(
        _stack: &mut Vec<BFieldElement>,
        std_in: Vec<BFieldElement>,
        secret_in: Vec<BFieldElement>,
        memory: &mut HashMap<BFieldElement, BFieldElement>,
    ) {
        // Leave memory as TASM leaves it:
        memory.insert(BFieldElement::zero(), BFieldElement::zero());

        let num_of_aps: u32 = std_in[0].try_into().unwrap();
        let merkle_root = Digest::new([std_in[5], std_in[4], std_in[3], std_in[2], std_in[1]]);
        let mut input_index: usize = 6;
        let mut secret_index: usize = 0;
        for _i in 0..num_of_aps {
            let mut node_index: u32 = std_in[input_index].try_into().unwrap();
            input_index += 1;
            //reversing the field elements of the leaf nodes
            let mut vector: Vec<BFieldElement> = std_in[input_index..input_index + 5].to_vec();
            vector.reverse();
            let elems: [BFieldElement; 5] = vector.try_into().unwrap();
            input_index += 5;
            let leaf: Digest = Digest::new(elems);
            let mut node_digest = leaf;
            while node_index != 1 {
                //reversing the field elements of the siblings
                let mut vector: Vec<BFieldElement> =
                    secret_in[secret_index..(secret_index + 5)].to_vec();
                vector.reverse();
                let sibling_elems: [BFieldElement; 5] = vector.try_into().unwrap();
                let sibling: Digest = Digest::new(sibling_elems);
                if node_index & 1 == 0 {
                    node_digest = RescuePrimeRegular::hash_pair(&node_digest, &sibling);
                } else {
                    node_digest = RescuePrimeRegular::hash_pair(&sibling, &node_digest);
                }
                secret_index += 5;
                node_index /= 2;
            }
            assert_eq!(node_digest, merkle_root);
        }
    }
}

#[cfg(test)]
mod merkle_authentication_verify_test {
    use std::collections::HashMap;

    use twenty_first::shared_math::b_field_element::BFieldElement;
    use twenty_first::shared_math::rescue_prime_digest::Digest;
    use twenty_first::shared_math::rescue_prime_regular::RescuePrimeRegular;
    use twenty_first::util_types::algebraic_hasher::AlgebraicHasher;
    use twenty_first::util_types::merkle_tree::CpuParallel;
    use twenty_first::util_types::merkle_tree::MerkleTree;
    use twenty_first::util_types::merkle_tree::PartialAuthenticationPath;
    use twenty_first::util_types::merkle_tree_maker::MerkleTreeMaker;

    use crate::get_init_tvm_stack;
    use crate::snippet_trait::rust_tasm_equivalence_prop;

    use super::MtApVerify;

    fn generate_leafs() -> Vec<Digest> {
        let two: u32 = 2;
        let num_leafs: u32 = two.pow(get_tree_height());
        //generate merkle leafs
        let number_of_leaves: usize = num_leafs.try_into().unwrap();
        let mut leafs: Vec<Digest> = Vec::with_capacity(number_of_leaves);
        for i in 0..num_leafs {
            leafs.push(RescuePrimeRegular::hash(&BFieldElement::from(i * i + 3)));
            // use digest.values() to access the field elements
        }
        //build other nodes
        leafs
    }

    const TREE_HEIGHT: u32 = 5;

    fn get_tree_height() -> u32 {
        TREE_HEIGHT
    }

    fn get_tree_height_usize() -> usize {
        get_tree_height().try_into().unwrap()
    }

    const NUMBER_OF_AUTHENTICATION_PATHS: usize = 4;

    fn choose_indices() -> [usize; NUMBER_OF_AUTHENTICATION_PATHS] {
        [2, 3, 7, 20]
    }

    fn unwrap_partial_authentication_path(
        partial_auth_path: &PartialAuthenticationPath<Digest>,
    ) -> Vec<Option<Digest>> {
        let path = partial_auth_path.clone();
        path.0
    }

    //generate standard (public) input for verifier
    fn generate_input(
        indices: [usize; NUMBER_OF_AUTHENTICATION_PATHS],
        leafs: &Vec<Digest>,
    ) -> Vec<BFieldElement> {
        let number_of_authentication_paths = indices.len();
        let number_of_leaves = leafs.len();
        let number_of_aps: u32 = number_of_authentication_paths.try_into().unwrap();
        let mt: &MerkleTree<RescuePrimeRegular, CpuParallel> =
            &MerkleTreeMaker::from_digests(leafs);
        let merkle_root = mt.nodes[1];
        let mut reverse_merkle_root = merkle_root.values().to_vec();
        reverse_merkle_root.reverse();
        let mut input: Vec<BFieldElement> = Vec::with_capacity(number_of_leaves + 5);
        input.push(BFieldElement::from(number_of_aps));
        input.append(&mut reverse_merkle_root);
        for leaf_index_usize in indices {
            let leaf_index: u32 = (leaf_index_usize + number_of_leaves).try_into().unwrap();
            input.push(BFieldElement::from(leaf_index));
            let mut vector = leafs[leaf_index_usize].clone().values().to_vec();
            vector.reverse();
            input.append(&mut vector);
        }

        input
    }

    //generate secret input for verifier
    fn generate_siblings_as_vector(
        mt: MerkleTree<RescuePrimeRegular, CpuParallel>,
        indices: [usize; NUMBER_OF_AUTHENTICATION_PATHS],
    ) -> Vec<BFieldElement> {
        let tree_height = get_tree_height_usize();
        let number_of_aps = indices.len();
        let number_of_leaves = mt.get_leaf_count();
        let mut bfield_vector: Vec<BFieldElement> =
            Vec::with_capacity(number_of_aps * tree_height * 5);
        for leaf_index in indices {
            let mut node_index = leaf_index + number_of_leaves;
            for _j in 0..tree_height {
                let node_parity = node_index & 1;
                let sibling_index = node_index + 1 - 2 * node_parity;
                let sibling = mt.nodes[sibling_index];
                let mut vector = sibling.values().to_vec();
                vector.reverse();
                bfield_vector.append(&mut vector);
                node_index /= 2;
            }
        }
        bfield_vector
    }

    #[test]
    fn merkle_tree_ap_verify_test1() {
        let leafs = generate_leafs();
        let mt: &MerkleTree<RescuePrimeRegular, CpuParallel> =
            &MerkleTreeMaker::from_digests(&leafs);
        let indices = choose_indices();
        let merkle_root = mt.nodes[1];
        for index in indices {
            let leaf: Digest = leafs[index];
            let mut node_index = index;
            let mut node_digest = leaf;
            let authentication_path = mt.get_authentication_structure(&[index])[0].clone();
            for i in 0..get_tree_height_usize() {
                let sibling: Digest =
                    unwrap_partial_authentication_path(&authentication_path)[i].unwrap();
                if node_index & 1 == 0 {
                    node_digest = RescuePrimeRegular::hash_pair(&node_digest, &sibling);
                } else {
                    node_digest = RescuePrimeRegular::hash_pair(&sibling, &node_digest);
                }
                node_index /= 2;
            }
            assert_eq!(node_digest, merkle_root);
        }
    }

    #[test]
    fn merkle_tree_ap_verify_test2() {
        let leafs = generate_leafs();
        let mt: &MerkleTree<RescuePrimeRegular, CpuParallel> =
            &MerkleTreeMaker::from_digests(&leafs);
        let indices = choose_indices();
        let secret_input: Vec<BFieldElement> = generate_siblings_as_vector(mt.clone(), indices);
        let stack: &mut Vec<BFieldElement> = &mut get_init_tvm_stack();
        let standard_input: Vec<BFieldElement> = generate_input(indices, &leafs);

        rust_tasm_equivalence_prop::<MtApVerify>(
            stack,
            &standard_input,
            &secret_input,
            &mut HashMap::default(),
            0,
            None,
        );
    }
}
