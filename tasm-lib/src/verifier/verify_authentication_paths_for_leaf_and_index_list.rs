use std::collections::HashMap;

use itertools::Itertools;
use rand::rngs::StdRng;
use rand::Rng;
use rand::RngCore;
use rand::SeedableRng;
use triton_vm::prelude::*;
use triton_vm::twenty_first::math::x_field_element::EXTENSION_DEGREE;
use triton_vm::twenty_first::prelude::*;

use crate::data_type::DataType;
use crate::rust_shadowing_helper_functions;
use crate::snippet_bencher::BenchmarkCase;
use crate::traits::algorithm::Algorithm;
use crate::traits::algorithm::AlgorithmInitialState;
use crate::traits::basic_snippet::BasicSnippet;
use crate::Digest;
use crate::VmHasher;

/// Verify a batch of Merkle membership claims.
/// Behavior: crashes the VM if even one of the authentication paths
/// is invalid. Goes into an infinite loop that crashes VM if height == 0.
#[derive(Debug, Default, Copy, Clone, Eq, PartialEq, Hash)]
pub struct VerifyAuthenticationPathForLeafAndIndexList;

impl BasicSnippet for VerifyAuthenticationPathForLeafAndIndexList {
    fn inputs(&self) -> Vec<(DataType, String)> {
        vec![
            (DataType::U32, "dom_len_minus_one".to_owned()),
            (DataType::U32, "dom_len_plus_half_dom_len".to_owned()),
            (
                DataType::List(Box::new(DataType::Xfe)),
                "*leaf_last_word".to_owned(),
            ),
            (
                DataType::List(Box::new(DataType::U32)),
                "*a_indices".to_owned(),
            ),
            (
                DataType::List(Box::new(DataType::U32)),
                "*a_indices_last_word".to_owned(),
            ),
            (DataType::Digest, "root".to_string()),
        ]
    }

    fn outputs(&self) -> Vec<(DataType, String)> {
        vec![]
    }

    fn entrypoint(&self) -> String {
        "tasmlib_verifier_verify_authentication_paths_for_leaf_and_index_list".into()
    }

    fn code(&self, _library: &mut crate::library::Library) -> Vec<LabelledInstruction> {
        let entrypoint = self.entrypoint();
        let main_loop = format!("{entrypoint}_main_loop");

        let loop_over_auth_paths_label = format!("{entrypoint}_loop_over_auth_path_elements");
        let loop_over_auth_paths_code = triton_asm!(
            {loop_over_auth_paths_label}:
                merkle_step                         // move up one level in the Merkle tree
                recurse_or_return                   // break loop if node_index is 1
        );

        triton_asm!(
            // BEFORE: _ dom_len_minus_one (dom_len + half_dom_len) *leaf *idx_end_cond *a_indices_last_word [root]
            // AFTER : _

            {entrypoint}:
                call {main_loop}
                // _ dom_len_minus_one (dom_len + half_dom_len) *leaf_last_word *a_indices *a_indices_last_word [root]

                /* Cleanup stack */
                pop 5
                pop 5
                // _

                return


            // Invariant: _ dom_len_minus_one (dom_len + half_dom_len) *leaf[n]_last_word *a_indices *a_indices[n] [root]
            {main_loop}:
                // _ dom_len_minus_one (dom_len + half_dom_len) *leaf[n] *a_indices *a_indices[n] [root]

                push 1
                // _ dom_len_minus_one (dom_len + half_dom_len) *leaf *a_indices *a_indices[n] [root] 1

                dup 6
                read_mem 1
                swap 8
                pop 1
                // _ dom_len_minus_one (dom_len + half_dom_len) *leaf *a_indices *a_indices[n]' [root] 1 ia_0[n]

                dup 11
                and
                dup 10
                xor
                // _ dom_len_minus_one (dom_len + half_dom_len) *leaf *a_indices *a_indices[n]' [root] 1 ((ia_0[n] & dom_len_minus_one) ^ (dom_len + half_dom_len))
                // _ dom_len_minus_one (dom_len + half_dom_len) *leaf *a_indices *a_indices[n]' [root] 1 (ib_r[n] + dom_len)
                // _ dom_len_minus_one (dom_len + half_dom_len) *leaf *a_indices *a_indices[n]' [root] 1 node_index_ib_r[n]

                push 0
                push 0
                // _ dom_len_minus_one (dom_len + half_dom_len) *leaf *a_indices *a_indices[n]' [root] 1 ib_r[n] 0 0

                dup 11
                read_mem {EXTENSION_DEGREE}
                swap 15
                pop 1
                // _ dom_len_minus_one (dom_len + half_dom_len) *leaf' *a_indices *a_indices[n]' [root] 1 ib_r[n] 0 0 [xfe]

                call {loop_over_auth_paths_label}
                // _ dom_len_minus_one (dom_len + half_dom_len) *leaf' *a_indices *a_indices[n]' [root] 1 1 [calculated_root]
                // _ dom_len_minus_one (dom_len + half_dom_len) *leaf' *a_indices *a_indices[n]' [root] 1 1 cr4 cr3 cr2 cr1 cr0

                swap 2
                swap 4
                swap 6
                pop 1
                swap 2
                swap 4
                pop 1
                // _ dom_len_minus_one (dom_len + half_dom_len) *leaf' *a_indices *a_indices[n]' [root] cr4 cr3 cr2 cr1 cr0
                // _ dom_len_minus_one (dom_len + half_dom_len) *leaf' *a_indices *a_indices[n]' [root] [calculated_root]

                assert_vector
                // _ dom_len_minus_one (dom_len + half_dom_len) *leaf *a_indices *a_indices[n]' [root]

                recurse_or_return

            {&loop_over_auth_paths_code}
        )
    }
}

impl Algorithm for VerifyAuthenticationPathForLeafAndIndexList {
    fn rust_shadow(
        &self,
        stack: &mut Vec<BFieldElement>,
        memory: &mut HashMap<BFieldElement, BFieldElement>,
        nondeterminism: &NonDeterminism,
    ) {
        // read arguments from stack
        let root = Digest::new([
            stack.pop().unwrap(),
            stack.pop().unwrap(),
            stack.pop().unwrap(),
            stack.pop().unwrap(),
            stack.pop().unwrap(),
        ]);
        let idx_last_elem = stack.pop().unwrap();
        let idx_end_condition = stack.pop().unwrap();
        let leaf_last_element_pointer = stack.pop().unwrap();
        let half_dom_len_plus_dom_len: u32 = stack.pop().unwrap().try_into().unwrap();
        let dom_len_minus_one: u32 = stack.pop().unwrap().try_into().unwrap();

        let tree_height: usize = (dom_len_minus_one + 1).ilog2().try_into().unwrap();

        let mut auth_path_counter = 0;
        let mut idx_element_pointer = idx_last_elem;
        let mut leaf_pointer = leaf_last_element_pointer;
        while idx_element_pointer != idx_end_condition {
            println!("idx_element_pointer: {idx_element_pointer}");
            println!("idx_end_condition: {idx_end_condition}");
            let authentication_path = nondeterminism.digests
                [auth_path_counter * tree_height..(auth_path_counter + 1) * tree_height]
                .to_vec();

            let leaf_index_a_round_0: u32 = memory
                .get(&idx_element_pointer)
                .map(|x| x.value())
                .unwrap_or_default()
                .try_into()
                .unwrap();
            let leaf_index_b_this_round: u32 =
                (leaf_index_a_round_0 & dom_len_minus_one) ^ half_dom_len_plus_dom_len;
            let read_word_from_mem =
                |pointer: BFieldElement| memory.get(&pointer).copied().unwrap_or_default();
            let leaf = XFieldElement::new([
                read_word_from_mem(leaf_pointer - bfe!(2)),
                read_word_from_mem(leaf_pointer - bfe!(1)),
                read_word_from_mem(leaf_pointer),
            ]);
            let inclusion_proof = MerkleTreeInclusionProof::<Tip5> {
                tree_height,
                indexed_leaves: vec![(leaf_index_b_this_round as usize, leaf.into())],
                authentication_structure: authentication_path,
                ..Default::default()
            };
            // assert!(inclusion_proof.verify(root));

            idx_element_pointer.decrement();
            auth_path_counter += 1;
            leaf_pointer -= bfe!(EXTENSION_DEGREE as u64);
        }
    }

    fn pseudorandom_initial_state(
        &self,
        seed: [u8; 32],
        bench_case: Option<crate::snippet_bencher::BenchmarkCase>,
    ) -> AlgorithmInitialState {
        let mut rng: StdRng = SeedableRng::from_seed(seed);

        // determine sizes
        let (height, num_indices) = match bench_case {
            Some(BenchmarkCase::CommonCase) => (10, 80),
            Some(BenchmarkCase::WorstCase) => (20, 80),
            None => (rng.gen_range(6..=15), rng.gen_range(2..10) as usize),
        };

        self.prepare_state(&mut rng, height, num_indices)
    }

    fn corner_case_initial_states(&self) -> Vec<AlgorithmInitialState> {
        let mut rng: StdRng = SeedableRng::from_seed([42u8; 32]);
        let two_leaves_reveal_0 = self.prepare_state(&mut rng, 1, 1);
        let two_leaves_reveal_1 = self.prepare_state(&mut rng, 1, 1);
        let two_leaves_reveal_2 = self.prepare_state(&mut rng, 1, 2);
        let four_leaves_reveal_0 = self.prepare_state(&mut rng, 2, 0);
        let four_leaves_reveal_1 = self.prepare_state(&mut rng, 2, 1);
        let four_leaves_reveal_2 = self.prepare_state(&mut rng, 2, 2);
        let four_leaves_reveal_3 = self.prepare_state(&mut rng, 2, 3);
        let four_leaves_reveal_4 = self.prepare_state(&mut rng, 2, 4);
        vec![
            two_leaves_reveal_0,
            two_leaves_reveal_1,
            two_leaves_reveal_2,
            four_leaves_reveal_0,
            four_leaves_reveal_1,
            four_leaves_reveal_2,
            four_leaves_reveal_3,
            four_leaves_reveal_4,
        ]
    }
}

impl VerifyAuthenticationPathForLeafAndIndexList {
    fn prepare_state(
        &self,
        rng: &mut StdRng,
        height: u32,
        num_indices: usize,
    ) -> AlgorithmInitialState {
        // generate data structure
        let dom_len = 1 << height;

        let xfe_leafs = (0..dom_len)
            .map(|_| rng.gen::<XFieldElement>())
            .collect_vec();
        let leafs_as_digest: Vec<Digest> = xfe_leafs.iter().map(|&xfe| xfe.into()).collect_vec();
        let tree =
            <CpuParallel as MerkleTreeMaker<VmHasher>>::from_digests(&leafs_as_digest).unwrap();
        let root = tree.root();

        let indices = (0..num_indices)
            .map(|_| rng.gen_range(0..dom_len) as usize)
            .collect_vec();
        let indicated_leafs = indices.iter().map(|i| xfe_leafs[*i]).collect_vec();
        let authentication_paths = indices
            .iter()
            .rev()
            .map(|i| tree.authentication_structure(&[*i]).unwrap())
            .collect_vec();
        let indices: Vec<u32> = indices.into_iter().map(|idx| idx as u32).collect_vec();

        // prepare memory + stack + nondeterminism
        let mut memory: HashMap<BFieldElement, BFieldElement> = HashMap::default();

        let a_indices_pointer = BFieldElement::new(rng.next_u64() % (1 << 20));
        rust_shadowing_helper_functions::list::list_insert(a_indices_pointer, indices, &mut memory);

        let leaf_pointer = BFieldElement::new(rng.next_u64() % (1 << 20) + (1 << 32));
        rust_shadowing_helper_functions::list::list_insert(
            leaf_pointer,
            indicated_leafs,
            &mut memory,
        );

        // vec![
        //     (DataType::U32, "dom_len_minus_one".to_owned()),
        //     (DataType::U32, "half_dom_len".to_owned()),
        //     (DataType::List(Box::new(DataType::Xfe)), "*leaf".to_owned()),
        //     (
        //         DataType::List(Box::new(DataType::U32)),
        //         "*idx_end_condition".to_owned(),
        //     ),
        //     (DataType::List(Box::new(DataType::U32)), "*idx".to_owned()),
        //     (DataType::Digest, "root".to_string()),
        // ]
        let a_indices_last_word = a_indices_pointer + bfe!(num_indices as u64);
        let leaf_pointer_last_word = leaf_pointer + bfe!((EXTENSION_DEGREE * num_indices) as u64);
        let dom_len_minus_one: u32 = dom_len - 1;
        let half_dom_len: u32 = dom_len / 2;
        println!("dom_len: {dom_len}");
        println!("dom_len_minus_one: {dom_len_minus_one}");
        println!("half_dom_len: {half_dom_len}");

        let mut stack = self.init_stack_for_isolated_run();
        stack.push(bfe!(dom_len_minus_one));
        stack.push(bfe!(half_dom_len + dom_len));
        stack.push(leaf_pointer_last_word);
        stack.push(a_indices_pointer);
        stack.push(a_indices_last_word);
        stack.push(root.0[4]);
        stack.push(root.0[3]);
        stack.push(root.0[2]);
        stack.push(root.0[1]);
        stack.push(root.0[0]);
        let nondeterminism = NonDeterminism::default()
            .with_digests(authentication_paths.into_iter().flatten().collect_vec())
            .with_ram(memory);

        AlgorithmInitialState {
            stack,
            nondeterminism,
        }
    }
}

#[cfg(test)]
mod test {
    use rand::thread_rng;
    use rand::Rng;

    use crate::test_helpers::negative_test;
    use crate::traits::algorithm::Algorithm;
    use crate::traits::algorithm::ShadowedAlgorithm;
    use crate::traits::rust_shadow::RustShadow;

    use super::*;

    #[test]
    fn test() {
        ShadowedAlgorithm::new(VerifyAuthenticationPathForLeafAndIndexList).test();
    }

    #[test]
    fn leaf_index_indices_auth_path_negative_test() {
        let mut rng = thread_rng();
        let seed: [u8; 32] = rng.gen();
        let vap4lail = VerifyAuthenticationPathForLeafAndIndexList;
        for i in 0..4 {
            let mut initial_state = vap4lail.pseudorandom_initial_state(seed, None);
            let stack_size = initial_state.stack.len();

            match i {
                0 => {
                    // change height; should fail
                    initial_state.stack[stack_size - 1] += BFieldElement::new(1);
                }
                1 => {
                    // change height; should fail
                    initial_state.stack[stack_size - 1] -= BFieldElement::new(1);
                }
                2 => {
                    // change root; should fail
                    initial_state.stack[stack_size - 2].increment();
                }
                3 => {
                    // change authentication path; should fail
                    initial_state.nondeterminism.digests[0].0[0].increment();
                }
                _ => unreachable!(), // no change; should be valid
            }

            negative_test(
                &ShadowedAlgorithm::new(vap4lail),
                initial_state.into(),
                &[InstructionError::VectorAssertionFailed(0)],
            );
        }
    }
}

#[cfg(test)]
mod benches {
    use crate::traits::algorithm::ShadowedAlgorithm;
    use crate::traits::rust_shadow::RustShadow;

    use super::*;

    #[test]
    fn vap4lail_benchmark() {
        ShadowedAlgorithm::new(VerifyAuthenticationPathForLeafAndIndexList).bench();
    }
}
