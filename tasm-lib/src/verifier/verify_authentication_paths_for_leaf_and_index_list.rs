use std::collections::HashMap;

use itertools::Itertools;
use rand::rngs::StdRng;
use rand::Rng;
use rand::RngCore;
use rand::SeedableRng;
use triton_vm::prelude::*;
use triton_vm::twenty_first::prelude::*;

use crate::data_type::DataType;
use crate::empty_stack;
use crate::hashing::merkle_verify::MerkleVerify;
use crate::list::get::Get;
use crate::list::length::Length;
use crate::rust_shadowing_helper_functions;
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
///   - leafs_and_index_list : [(Digest,U32)]
///   - root : Digest
///   - height : U32
///
/// Behavior: crashes the VM if even one of the authentication paths
/// is invalid.
#[derive(Debug, Default, Copy, Clone, Eq, PartialEq, Hash)]
pub struct VerifyAuthenticationPathForLeafAndIndexList;

impl BasicSnippet for VerifyAuthenticationPathForLeafAndIndexList {
    fn inputs(&self) -> Vec<(DataType, String)> {
        let indexed_leaf_type = DataType::Tuple(vec![DataType::U32, DataType::Digest]);
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
        self.inputs()
    }

    fn entrypoint(&self) -> String {
        "tasmlib_verifier_verify_authentication_paths_for_leaf_and_index_list".into()
    }

    fn code(&self, library: &mut crate::library::Library) -> Vec<LabelledInstruction> {
        let entrypoint = self.entrypoint();
        let main_loop = format!("{entrypoint}_main_loop");
        let indices_and_leaves = DataType::Tuple(vec![DataType::U32, DataType::Digest]);

        let len_subroutine_label =
            library.import(Box::new(Length::new(indices_and_leaves.clone())));
        let get_element_subroutine_label = library.import(Box::new(Get::new(indices_and_leaves)));
        let merkle_verify_subroutine_label = library.import(Box::new(MerkleVerify));

        triton_asm! {
            // BEFORE: _ *leaf_and_index_list root4 root3 root2 root1 root0 height
            // AFTER:  _ *leaf_and_index_list root4 root3 root2 root1 root0 height
            {entrypoint}:
                dup 6               // _ *leaf_and_index_list root4 root3 root2 root1 root0 height *leaf_and_index_list
                call {len_subroutine_label}
                                    // _ *leaf_and_index_list root4 root3 root2 root1 root0 height length
                push 0              // _ *leaf_and_index_list root4 root3 root2 root1 root0 height length 0

                call {main_loop}    // _ *leaf_and_index_list root4 root3 root2 root1 root0 height length length

                pop 2
                return

            // INVARIANT: _ *leaf_and_index_list root4 root3 root2 root1 root0 height length iteration
            {main_loop}:
                // evaluate termination criterion
                dup 1 dup 1     // _ *leaf_and_index_list root4 root3 root2 root1 root0 height length iteration length iteration
                eq              // _ *leaf_and_index_list root4 root3 root2 root1 root0 height length iteration length==iteration
                skiz return     // _ *leaf_and_index_list root4 root3 root2 root1 root0 height length iteration

                // duplicate root
                dup 7 dup 7 dup 7 dup 7 dup 7
                                // _ *leaf_and_index_list root4 root3 root2 root1 root0 height length iteration root4 root3 root2 root1 root0

                // read leaf and index
                dup 13          // _ *leaf_and_index_list root4 root3 root2 root1 root0 height length iteration root4 root3 root2 root1 root0 lai_list
                dup 6           // _ *leaf_and_index_list root4 root3 root2 root1 root0 height length iteration root4 root3 root2 root1 root0 lai_list iteration
                call {get_element_subroutine_label}
                                // _ *leaf_and_index_list root4 root3 root2 root1 root0 height length iteration root4 root3 root2 root1 root0 index leaf4 leaf3 leaf2 leaf1 leaf0

                // format and call merkle_verify
                dup 13          // _ *leaf_and_index_list root4 root3 root2 root1 root0 height length iteration root4 root3 root2 root1 root0 index leaf4 leaf3 leaf2 leaf1 leaf0 height
                call {merkle_verify_subroutine_label}

                // prepare for next iteration
                push 1 add      // _ *leaf_and_index_list root4 root3 root2 root1 root0 height length iteration+1
                recurse
        }
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

        let indices_and_leafs = *Vec::<(u32, Digest)>::decode_from_memory(memory, address).unwrap();

        // iterate and verify
        for (i, (leaf_index, leaf_digest)) in indices_and_leafs.into_iter().enumerate() {
            let authentication_path = nondeterminism.digests[i * height..(i + 1) * height].to_vec();
            let inclusion_proof = MerkleTreeInclusionProof::<Tip5> {
                tree_height: height,
                indexed_leaves: vec![(leaf_index as usize, leaf_digest)],
                authentication_structure: authentication_path,
                ..Default::default()
            };
            assert!(inclusion_proof.verify(root));
        }

        // restore stack
        stack.push(address);
        stack.push(root.0[4]);
        stack.push(root.0[3]);
        stack.push(root.0[2]);
        stack.push(root.0[1]);
        stack.push(root.0[0]);
        stack.push(BFieldElement::new(height as u64));
    }

    fn pseudorandom_initial_state(
        &self,
        seed: [u8; 32],
        bench_case: Option<crate::snippet_bencher::BenchmarkCase>,
    ) -> AlgorithmInitialState {
        let mut rng: StdRng = SeedableRng::from_seed(seed);

        // determine sizes
        let height = if let Some(case) = bench_case {
            match case {
                crate::snippet_bencher::BenchmarkCase::CommonCase => 15,
                crate::snippet_bencher::BenchmarkCase::WorstCase => 25,
            }
        } else {
            rng.gen_range(6..=15)
        };
        let num_indices = rng.gen_range(2..5) as usize;
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

        let leafs = (0..n).map(|_| rng.gen::<Digest>()).collect_vec();
        let tree = <CpuParallel as MerkleTreeMaker<VmHasher>>::from_digests(&leafs).unwrap();
        let root = tree.root();

        let indices = (0..num_indices)
            .map(|_| rng.gen_range(0..n) as usize)
            .collect_vec();
        let indicated_leafs = indices.iter().map(|i| leafs[*i]).collect_vec();
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
        let mut stack = empty_stack();
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

    #[ignore = "Very slow, about 280s on my powerful laptop"]
    #[test]
    fn vap4lail_benchmark() {
        ShadowedAlgorithm::new(VerifyAuthenticationPathForLeafAndIndexList).bench();
    }
}
