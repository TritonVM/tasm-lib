use std::collections::HashMap;
use std::marker::PhantomData;

use num::Zero;
use rand::Rng;
use twenty_first::shared_math::b_field_element::BFieldElement;
use twenty_first::util_types::algebraic_hasher::AlgebraicHasher;
use twenty_first::util_types::merkle_tree::{CpuParallel, MerkleTree};
use twenty_first::util_types::merkle_tree_maker::MerkleTreeMaker;

use crate::snippet::Snippet;
use crate::snippet_state::SnippetState;
use crate::{get_init_tvm_stack, Digest, ExecutionState};

#[derive(Clone, Debug)]
pub struct MtApVerifyFromSecretInput<H: AlgebraicHasher + std::fmt::Debug>(pub PhantomData<H>);

/// TVM assembly to verify Merkle authentication paths
///
/// input: number of authentication paths, merkle root, authentication paths,
/// each one preceded by the corresponding node index in the merkle tree,
/// where the authentication path starts
///
/// output: Result<(), VMFail>
///
/// uses RAM at address 0 to store the number of authentication paths
impl<H: AlgebraicHasher + std::fmt::Debug> Snippet for MtApVerifyFromSecretInput<H> {
    fn inputs(&self) -> Vec<String> {
        vec![]
    }

    fn outputs(&self) -> Vec<String> {
        vec![]
    }

    fn input_types(&self) -> Vec<crate::snippet::DataType> {
        vec![]
    }

    fn output_types(&self) -> Vec<crate::snippet::DataType> {
        vec![]
    }

    fn crash_conditions(&self) -> Vec<String> {
        vec!["auth path in public input is invalid".to_string()]
    }

    fn gen_input_states(&self) -> Vec<ExecutionState> {
        let mt_height = 5;
        vec![prepare_state::<H>(mt_height)]
    }

    fn stack_diff(&self) -> isize {
        0
    }

    fn entrypoint(&self) -> String {
        "tasm_recufier_mt_ap_verify".to_string()
    }

    fn function_code(&self, _library: &mut SnippetState) -> String {
        let entrypoint = self.entrypoint();
        format!(
            "
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
        ")
    }

    //needs secret input with merkle siblings along all authentication paths
    //will need to update code when we switch to a new hash function
    fn rust_shadowing(
        &self,
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
                    node_digest = H::hash_pair(&node_digest, &sibling);
                } else {
                    node_digest = H::hash_pair(&sibling, &node_digest);
                }
                secret_index += 5;
                node_index /= 2;
            }
            assert_eq!(node_digest, merkle_root);
        }
    }

    fn common_case_input_state(&self) -> ExecutionState {
        let mt_height = 6;
        prepare_state::<H>(mt_height)
    }

    fn worst_case_input_state(&self) -> ExecutionState {
        // `mt_height` should probably be 20 here, but that execution takes very long to run.
        let mt_height = 12;
        prepare_state::<H>(mt_height)
    }
}

fn prepare_state<H: AlgebraicHasher>(tree_height: usize) -> ExecutionState {
    // fn gen_input_states(&self) -> Vec<ExecutionState> {
    let leafs = generate_leafs::<H>(tree_height);
    let mt: &MerkleTree<H> = &CpuParallel::from_digests(&leafs);
    let indices = choose_indices(tree_height);
    let secret_in: Vec<BFieldElement> =
        generate_siblings_as_vector(mt.clone(), indices, tree_height);
    let stack: Vec<BFieldElement> = get_init_tvm_stack();
    let std_in: Vec<BFieldElement> = generate_input::<H>(indices, &leafs);

    ExecutionState {
        stack,
        std_in,
        secret_in,
        memory: HashMap::default(),
        words_allocated: 0,
    }
}

const NUMBER_OF_AUTHENTICATION_PATHS: usize = 7;
fn choose_indices(tree_height: usize) -> [usize; NUMBER_OF_AUTHENTICATION_PATHS] {
    let mut indices = [0usize; NUMBER_OF_AUTHENTICATION_PATHS];
    let mut i = 0;
    while i < NUMBER_OF_AUTHENTICATION_PATHS {
        let index = rand::thread_rng().gen_range(0..2u32.pow(tree_height as u32));
        if !indices.contains(&(index as usize)) {
            indices[i] = index as usize;
            i += 1;
        }
    }

    indices
}

//generate secret input for verifier
fn generate_siblings_as_vector<H: AlgebraicHasher>(
    mt: MerkleTree<H>,
    indices: [usize; NUMBER_OF_AUTHENTICATION_PATHS],
    tree_height: usize,
) -> Vec<BFieldElement> {
    let number_of_aps = indices.len();
    let number_of_leaves = mt.get_leaf_count();
    let mut bfield_vector: Vec<BFieldElement> = Vec::with_capacity(number_of_aps * tree_height * 5);
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

//generate standard (public) input for verifier
fn generate_input<H: AlgebraicHasher>(
    indices: [usize; NUMBER_OF_AUTHENTICATION_PATHS],
    leafs: &Vec<Digest>,
) -> Vec<BFieldElement> {
    let number_of_authentication_paths = indices.len();
    let number_of_leaves = leafs.len();
    let number_of_aps: u32 = number_of_authentication_paths.try_into().unwrap();
    let mt: &MerkleTree<H> = &CpuParallel::from_digests(leafs);
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

fn generate_leafs<H: AlgebraicHasher>(height: usize) -> Vec<Digest> {
    let num_leafs: u32 = 2u32.pow(height as u32);
    //generate merkle leafs
    let number_of_leaves: usize = num_leafs.try_into().unwrap();
    let mut leafs: Vec<Digest> = Vec::with_capacity(number_of_leaves);
    for i in 0..num_leafs {
        leafs.push(H::hash(&BFieldElement::from(i * i + 3)));
        // use digest.values() to access the field elements
    }
    //build other nodes
    leafs
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::collections::HashMap;

    use rand::random;
    use twenty_first::shared_math::b_field_element::BFieldElement;
    use twenty_first::util_types::algebraic_hasher::AlgebraicHasher;
    use twenty_first::util_types::merkle_tree::CpuParallel;
    use twenty_first::util_types::merkle_tree::MerkleTree;
    use twenty_first::util_types::merkle_tree::PartialAuthenticationPath;
    use twenty_first::util_types::merkle_tree_maker::MerkleTreeMaker;

    use crate::get_init_tvm_stack;
    use crate::test_helpers::test_rust_equivalence_given_input_values;
    use crate::test_helpers::test_rust_equivalence_multiple;
    use crate::VmHasher;

    use super::MtApVerifyFromSecretInput;

    fn unwrap_partial_authentication_path(
        partial_auth_path: &PartialAuthenticationPath<Digest>,
    ) -> Vec<Option<Digest>> {
        let path = partial_auth_path.clone();
        path.0
    }

    #[test]
    fn merkle_tree_ap_verify_from_secret_input_test() {
        test_rust_equivalence_multiple(&MtApVerifyFromSecretInput(PhantomData::<VmHasher>), true);
    }

    #[test]
    fn merkle_tree_ap_verify_test1() {
        let mt_height = 5;
        let leafs = generate_leafs::<VmHasher>(mt_height);
        let mt: &MerkleTree<VmHasher> = &CpuParallel::from_digests(&leafs);
        let indices = choose_indices(mt_height);
        let merkle_root = mt.nodes[1];
        for index in indices {
            let leaf: Digest = leafs[index];
            let mut node_index = index;
            let mut node_digest = leaf;
            let authentication_path = mt.get_authentication_structure(&[index])[0].clone();
            for i in 0..mt_height {
                let sibling: Digest =
                    unwrap_partial_authentication_path(&authentication_path)[i].unwrap();
                if node_index & 1 == 0 {
                    node_digest = VmHasher::hash_pair(&node_digest, &sibling);
                } else {
                    node_digest = VmHasher::hash_pair(&sibling, &node_digest);
                }
                node_index /= 2;
            }
            assert_eq!(node_digest, merkle_root);
        }
    }

    #[test]
    fn merkle_tree_ap_verify_test2() {
        let mt_height = 5;
        let leafs = generate_leafs::<VmHasher>(mt_height);
        let mt: &MerkleTree<VmHasher> = &CpuParallel::from_digests(&leafs);
        let indices = choose_indices(mt_height);
        let secret_input: Vec<BFieldElement> =
            generate_siblings_as_vector(mt.clone(), indices, mt_height);
        let stack: &mut Vec<BFieldElement> = &mut get_init_tvm_stack();
        let standard_input: Vec<BFieldElement> = generate_input::<VmHasher>(indices, &leafs);

        test_rust_equivalence_given_input_values(
            &MtApVerifyFromSecretInput(PhantomData::<VmHasher>),
            stack,
            &standard_input,
            &secret_input,
            &mut HashMap::default(),
            0,
            None,
        );
    }

    #[should_panic]
    #[test]
    fn merkle_tree_ap_verify_negative_test() {
        let mt_height = 5;
        let leafs = generate_leafs::<VmHasher>(mt_height);
        let mt: &MerkleTree<VmHasher> = &CpuParallel::from_digests(&leafs);
        let indices = choose_indices(mt_height);

        // Generate invalid secret input
        let mut bad_secret_input: Vec<BFieldElement> =
            generate_siblings_as_vector(mt.clone(), indices, mt_height);
        bad_secret_input[0] = random();

        let stack: &mut Vec<BFieldElement> = &mut get_init_tvm_stack();
        let standard_input: Vec<BFieldElement> = generate_input::<VmHasher>(indices, &leafs);

        test_rust_equivalence_given_input_values(
            &MtApVerifyFromSecretInput(PhantomData::<VmHasher>),
            stack,
            &standard_input,
            &bad_secret_input,
            &mut HashMap::default(),
            0,
            None,
        );
    }
}

#[cfg(test)]
mod benches {
    use super::*;
    use crate::{snippet_bencher::bench_and_write, VmHasher};

    #[test]
    fn merkle_tree_ap_verify_from_secret_input_benchmark() {
        bench_and_write(MtApVerifyFromSecretInput(PhantomData::<VmHasher>));
    }
}
