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
use crate::structure::tasm_object::TasmObject;
use crate::traits::algorithm::Algorithm;
use crate::traits::algorithm::AlgorithmInitialState;
use crate::traits::basic_snippet::BasicSnippet;
use crate::Digest;
use crate::VmHasher;

/// Verify a batch of Merkle membership claims.
///
/// Arguments:
///
///   - leafs_and_index_list : [(Xfe,U32)]
///   - root : Digest
///   - height : U32
///
/// Behavior: crashes the VM if even one of the authentication paths
/// is invalid.
#[derive(Debug, Default, Copy, Clone, Eq, PartialEq, Hash)]
pub struct VerifyAuthenticationPathForLeafAndIndexList;

impl BasicSnippet for VerifyAuthenticationPathForLeafAndIndexList {
    fn inputs(&self) -> Vec<(DataType, String)> {
        let indexed_leaf_type = DataType::Tuple(vec![DataType::U32, DataType::Xfe]);
        vec![
            (
                DataType::List(Box::new(indexed_leaf_type)),
                "leaf_and_index_list".to_string(),
            ),
            (DataType::Digest, "root".to_string()),
            (DataType::U32, "height".to_string()),
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
                dup 5 push 1 eq skiz return         // break loop if node_index is 1
                divine_sibling hash recurse         // move up one level in the Merkle tree
        );

        const LIST_ELEMENT_SIZE: usize = EXTENSION_DEGREE + 1;

        triton_asm!(
            // BEFORE: _ *leaf_and_index_list root4 root3 root2 root1 root0 height

            {entrypoint}:
                /* Calculate `num_leafs` */

                push 2
                pow
                // _ *leaf_and_index_list root4 root3 root2 root1 root0 num_leafs


                /* Rearrange */
                swap 5
                swap 4
                swap 3
                swap 2
                swap 1
                // _ *leaf_and_index_list num_leafs root4 root3 root2 root1 root0
                // _ *leaf_and_index_list num_leafs [root] <-- rename


                /* Calculate end-condition for main-loop */
                dup 6          // _ *leaf_and_index_list num_leafs [root] *leaf_and_index_list
                read_mem 1     // _ *leaf_and_index_list num_leafs [root] len (*leaf_and_index_list-1)

                swap 1
                push {LIST_ELEMENT_SIZE}
                // _ *leaf_and_index_list num_leafs [root] (*leaf_and_index_list-1) len ELEM_SIZE

                mul
                add
                push {1 + LIST_ELEMENT_SIZE}
                add
                // _ *leaf_and_index_list num_leafs [root] (*leaf_and_index_list_last_word + `LIST_ELEMENT_SIZE`)
                // _ *leaf_and_index_list num_leafs [root] *end_condition

                swap 7
                // _ *end_condition  num_leafs [root] *leaf_and_index_list

                push {LIST_ELEMENT_SIZE}
                add
                // _ *end_condition  num_leafs [root] *leaf_and_index_list_first_idx

                swap 6
                swap 5
                swap 4
                swap 3
                swap 2
                swap 1
                // _ *end_condition *leaf_and_index_list_first_idx num_leafs [root]

                call {main_loop}
                // _ *leaf_and_index_list_end_condition *leaf_and_index_list_end_condition num_leafs [root]


                /* Cleanup stack */
                pop 5
                pop 3
                // _

                return


            // Invariant: _ *leaf_and_index_list_end_condition *leaf_and_index_element num_leafs [root]
            {main_loop}:
                dup 7
                dup 7
                eq
                skiz return

                // _ *leaf_and_index_list_end_condition *leaf_and_index_element_idx num_leafs [root]
                dup 6
                read_mem 1
                swap 8
                pop 1
                // _ *leaf_and_index_list_end_condition *leaf_and_index_element_val num_leafs [root] leaf_index

                dup 6
                add
                // _ *leaf_and_index_list_end_condition *leaf_and_index_element_val num_leafs [root] node_index

                // Get indicated digest onto stack
                push 0
                push 0
                // _ *leaf_and_index_list_end_condition *leaf_and_index_element_val num_leafs [root] node_index 0 0

                dup 9
                read_mem {EXTENSION_DEGREE}
                push {2 * LIST_ELEMENT_SIZE}
                add
                swap 13
                pop 1
                // _ *leaf_and_index_list_end_condition *leaf_and_index_element_idx_next num_leafs [root] node_index 0 0 xfe2 xfe1 xfe0
                // _ *leaf_and_index_list_end_condition *leaf_and_index_element_idx_next num_leafs [root] node_index [leaf]  <-- rename

                call {loop_over_auth_paths_label}
                // _ *leaf_and_index_list_end_condition *leaf_and_index_element_idx_next num_leafs [root] 1 [calculated_root]  <-- rename

                swap 1
                swap 2
                swap 3
                swap 4
                swap 5
                pop 1
                // _ *leaf_and_index_list_end_condition *leaf_and_index_element_idx_next num_leafs [root] [calculated_root]

                assert_vector
                // _ *leaf_and_index_list_end_condition *leaf_and_index_element_idx_next num_leafs [root]

                recurse

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
        let height = stack.pop().unwrap().value() as usize;
        let root = Digest::new([
            stack.pop().unwrap(),
            stack.pop().unwrap(),
            stack.pop().unwrap(),
            stack.pop().unwrap(),
            stack.pop().unwrap(),
        ]);
        let address = stack.pop().unwrap();

        let indices_and_leafs =
            *Vec::<(u32, XFieldElement)>::decode_from_memory(memory, address).unwrap();

        // iterate and verify
        for (i, (leaf_index, leaf_xfe)) in indices_and_leafs.into_iter().enumerate() {
            let authentication_path = nondeterminism.digests[i * height..(i + 1) * height].to_vec();
            let inclusion_proof = MerkleTreeInclusionProof::<Tip5> {
                tree_height: height,
                indexed_leaves: vec![(leaf_index as usize, leaf_xfe.into())],
                authentication_structure: authentication_path,
                ..Default::default()
            };
            assert!(inclusion_proof.verify(root));
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
        let one_leaf_reveal_0 = self.prepare_state(&mut rng, 0, 0);
        let one_leaf_reveal_1 = self.prepare_state(&mut rng, 0, 1);
        let two_leaves_reveal_0 = self.prepare_state(&mut rng, 1, 1);
        let two_leaves_reveal_1 = self.prepare_state(&mut rng, 1, 1);
        let two_leaves_reveal_2 = self.prepare_state(&mut rng, 1, 2);
        let four_leaves_reveal_0 = self.prepare_state(&mut rng, 2, 0);
        let four_leaves_reveal_1 = self.prepare_state(&mut rng, 2, 1);
        let four_leaves_reveal_2 = self.prepare_state(&mut rng, 2, 2);
        let four_leaves_reveal_3 = self.prepare_state(&mut rng, 2, 3);
        let four_leaves_reveal_4 = self.prepare_state(&mut rng, 2, 4);
        vec![
            one_leaf_reveal_0,
            one_leaf_reveal_1,
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
        let n = 1 << height;

        let xfe_leafs = (0..n).map(|_| rng.gen::<XFieldElement>()).collect_vec();
        let leafs_as_digest: Vec<Digest> = xfe_leafs.iter().map(|&xfe| xfe.into()).collect_vec();
        let tree =
            <CpuParallel as MerkleTreeMaker<VmHasher>>::from_digests(&leafs_as_digest).unwrap();
        let root = tree.root();

        let indices = (0..num_indices)
            .map(|_| rng.gen_range(0..n) as usize)
            .collect_vec();
        let indicated_leafs = indices.iter().map(|i| xfe_leafs[*i]).collect_vec();
        let leafs_and_indices = indices
            .iter()
            .map(|i| *i as u32)
            .zip(indicated_leafs)
            .collect_vec();
        let authentication_paths = indices
            .iter()
            .map(|i| tree.authentication_structure(&[*i]).unwrap())
            .collect_vec();

        // prepare memory + stack + nondeterminism
        let mut memory: HashMap<BFieldElement, BFieldElement> = HashMap::new();
        let address = BFieldElement::new(rng.next_u64() % (1 << 20));
        rust_shadowing_helper_functions::list::list_insert(address, leafs_and_indices, &mut memory);
        let mut stack = self.init_stack_for_isolated_run();
        stack.push(address);
        stack.push(root.0[4]);
        stack.push(root.0[3]);
        stack.push(root.0[2]);
        stack.push(root.0[1]);
        stack.push(root.0[0]);
        stack.push(BFieldElement::new(height as u64));
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
    use std::cell::RefCell;
    use std::rc::Rc;

    use rand::thread_rng;
    use rand::Rng;

    use crate::execute_with_terminal_state;
    use crate::linker::link_for_isolated_run;
    use crate::traits::algorithm::Algorithm;
    use crate::traits::algorithm::AlgorithmInitialState;
    use crate::traits::algorithm::ShadowedAlgorithm;
    use crate::traits::rust_shadow::RustShadow;

    use super::*;

    #[test]
    fn test() {
        ShadowedAlgorithm::new(VerifyAuthenticationPathForLeafAndIndexList).test();
    }

    #[test]
    fn negative_test() {
        let mut rng = thread_rng();
        let seed: [u8; 32] = rng.gen();
        let vap4lail = VerifyAuthenticationPathForLeafAndIndexList;
        for i in 0..4 {
            let AlgorithmInitialState {
                mut stack,
                mut nondeterminism,
            } = vap4lail.pseudorandom_initial_state(seed, None);
            let len = stack.len();

            match i {
                0 => {
                    // change height; should fail
                    stack[len - 1] += BFieldElement::new(1);
                }
                1 => {
                    // change height; should fail
                    stack[len - 1] -= BFieldElement::new(1);
                }
                2 => {
                    // change root; should fail
                    stack[len - 2].increment();
                }
                3 => {
                    // change authentication path; should fail
                    nondeterminism.digests[0].0[0].increment();
                }
                _ => {} // no change; should be valid
            }

            // test rust/tasm equivalence
            // in this case: verify that they both fail

            let stdin = vec![];

            // run rust shadow
            let rust_result = std::panic::catch_unwind(|| {
                let mut rust_stack = stack.clone();
                let mut rust_memory = nondeterminism.ram.clone();
                ShadowedAlgorithm::new(vap4lail).rust_shadow_wrapper(
                    &stdin,
                    &nondeterminism,
                    &mut rust_stack,
                    &mut rust_memory,
                    &mut None,
                )
            });

            if let Ok(result) = &rust_result {
                println!(
                    "rust result: {:?}\ni: {}\nstack: {:?}",
                    result,
                    i,
                    stack.clone()
                );
            }

            // run tvm
            let code = link_for_isolated_run(Rc::new(RefCell::new(vap4lail)));
            let program = Program::new(&code);
            let tvm_result =
                execute_with_terminal_state(&program, &stdin, &stack, &nondeterminism, None);
            if let Ok(result) = &tvm_result {
                println!("tasm result: {}\ni: {}", result, i);
            }

            assert!(rust_result.is_err());
            assert!(tvm_result.is_err());
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
