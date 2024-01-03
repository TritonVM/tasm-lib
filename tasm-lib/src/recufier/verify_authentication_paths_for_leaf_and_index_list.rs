use std::collections::HashMap;

use itertools::Itertools;
use rand::{rngs::StdRng, Rng, RngCore, SeedableRng};
use triton_vm::{triton_asm, BFieldElement, NonDeterminism};
use twenty_first::util_types::{
    merkle_tree::{CpuParallel, MerkleTree},
    merkle_tree_maker::MerkleTreeMaker,
};

use crate::{
    data_type::DataType,
    traits::{
        algorithm::{Algorithm, AlgorithmInitialState},
        basic_snippet::BasicSnippet,
    },
};
use crate::{
    empty_stack, list::ListType, recufier::merkle_verify::MerkleVerify,
    rust_shadowing_helper_functions, structure::tasm_object::TasmObject, Digest, VmHasher,
};

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
#[derive(Debug, Clone)]
pub struct VerifyAuthenticationPathForLeafAndIndexList {
    pub list_type: ListType,
}

impl BasicSnippet for VerifyAuthenticationPathForLeafAndIndexList {
    fn inputs(&self) -> Vec<(DataType, String)> {
        vec![
            (
                DataType::List(Box::new(DataType::Tuple(vec![
                    DataType::U32,
                    DataType::Digest,
                ]))),
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
        format!(
            "tasm_recufier_verify_authentication_paths_for_leaf_and_index_list_{}",
            self.list_type
        )
    }

    fn code(
        &self,
        library: &mut crate::library::Library,
    ) -> Vec<triton_vm::instruction::LabelledInstruction> {
        let entrypoint = self.entrypoint();
        let main_loop = format!("{entrypoint}_main_loop");
        let data_type = DataType::Tuple(vec![DataType::U32, DataType::Digest]);
        let lai_length = library.import(self.list_type.length_snippet(data_type.clone()));
        let lai_get = library.import(self.list_type.get_snippet(data_type.clone()));
        let merkle_verify = library.import(Box::new(MerkleVerify));
        triton_asm! {
            // BEFORE: _ leaf_and_index_list root4 root3 root2 root1 root0 height
            // AFTER:  _ leaf_and_index_list root4 root3 root2 root1 root0 height
            {entrypoint}:
                dup 6               // _ leaf_and_index_list root4 root3 root2 root1 root0 height leaf_and_index_list
                call {lai_length}   // _ leaf_and_index_list root4 root3 root2 root1 root0 height length
                push 0              // _ leaf_and_index_list root4 root3 root2 root1 root0 height length 0

                call {main_loop}    // _ leaf_and_index_list root4 root3 root2 root1 root0 height length length

                pop 2
                return

            // INVARIANT: _ leaf_and_index_list root4 root3 root2 root1 root0 height length iteration
            {main_loop}:
                // evaluate termination criterion
                dup 1 dup 1     // _ leaf_and_index_list root4 root3 root2 root1 root0 height length iteration length iteration
                eq              // _ leaf_and_index_list root4 root3 root2 root1 root0 height length iteration length==iteration
                skiz return     // _ leaf_and_index_list root4 root3 root2 root1 root0 height length iteration

                // duplicate root
                dup 7 dup 7 dup 7 dup 7 dup 7
                // _ leaf_and_index_list root4 root3 root2 root1 root0 height length iteration root4 root3 root2 root1 root0

                // read leaf and index
                dup 13          // _ leaf_and_index_list root4 root3 root2 root1 root0 height length iteration root4 root3 root2 root1 root0 lai_list
                dup 6           // _ leaf_and_index_list root4 root3 root2 root1 root0 height length iteration root4 root3 root2 root1 root0 lai_list iteration
                call {lai_get}  // _ leaf_and_index_list root4 root3 root2 root1 root0 height length iteration root4 root3 root2 root1 root0 index leaf4 leaf3 leaf2 leaf1 leaf0

                // format and call merkle_verify
                dup 13          // _ leaf_and_index_list root4 root3 root2 root1 root0 height length iteration root4 root3 root2 root1 root0 index leaf4 leaf3 leaf2 leaf1 leaf0 height
                call {merkle_verify}

                // prepare for next iteration
                push 1 add      // _ leaf_and_index_list root4 root3 root2 root1 root0 height length iteration+1
                recurse
        }
    }
}

impl Algorithm for VerifyAuthenticationPathForLeafAndIndexList {
    fn rust_shadow(
        &self,
        stack: &mut Vec<triton_vm::BFieldElement>,
        memory: &mut std::collections::HashMap<triton_vm::BFieldElement, triton_vm::BFieldElement>,
        nondeterminism: &triton_vm::NonDeterminism<triton_vm::BFieldElement>,
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

        // read object from memory
        let safety_offset = match self.list_type {
            ListType::Safe => 1,
            ListType::Unsafe => 0,
        };
        let indices_and_leafs = *Vec::<(u32, Digest)>::decode_from_memory(
            memory,
            address + BFieldElement::new(safety_offset),
        )
        .unwrap();

        // iterate and verify
        let mut digest_index = 0;
        for (index, leaf) in indices_and_leafs {
            let authentication_path = (0..height)
                .map(|_| {
                    let node = nondeterminism.digests[digest_index];
                    digest_index += 1;
                    node
                })
                .collect_vec();
            assert!(MerkleTree::<VmHasher>::verify_authentication_structure(
                root,
                height,
                &[index as usize],
                &[leaf],
                &authentication_path
            ));
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
            10 + rng.gen_range(1..10) // random number between 10 and 19
        };
        let n = 1 << height;
        let num_indices = rng.gen_range(2..5) as usize;

        // generate data structure
        let leafs = (0..n).map(|_| rng.gen::<Digest>()).collect_vec();
        let tree = <CpuParallel as MerkleTreeMaker<VmHasher>>::from_digests(&leafs);
        let root = tree.get_root();

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
            .map(|i| tree.get_authentication_structure(&[*i]))
            .collect_vec();

        // prepare memory + stack + nondeterminism
        let mut memory: HashMap<BFieldElement, BFieldElement> = HashMap::new();
        let address = BFieldElement::new(rng.next_u64() % (1 << 20));
        match self.list_type {
            ListType::Safe => rust_shadowing_helper_functions::safe_list::safe_list_insert(
                address,
                num_indices as u32,
                leafs_and_indices,
                &mut memory,
            ),
            ListType::Unsafe => rust_shadowing_helper_functions::unsafe_list::unsafe_list_insert(
                address,
                leafs_and_indices,
                &mut memory,
            ),
        };
        let mut stack = empty_stack();
        stack.push(address);
        stack.push(root.0[4]);
        stack.push(root.0[3]);
        stack.push(root.0[2]);
        stack.push(root.0[1]);
        stack.push(root.0[0]);
        stack.push(BFieldElement::new(height as u64));
        let nondeterminism = NonDeterminism::<BFieldElement>::default()
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
    use super::VerifyAuthenticationPathForLeafAndIndexList;
    use crate::traits::algorithm::{Algorithm, AlgorithmInitialState};
    use crate::traits::rust_shadow::RustShadow;
    use crate::{
        execute_with_terminal_state, linker::link_for_isolated_run, list::ListType,
        traits::algorithm::ShadowedAlgorithm,
    };
    use rand::{thread_rng, Rng};
    use std::{cell::RefCell, rc::Rc};
    use triton_vm::{BFieldElement, Program};

    #[test]
    fn test() {
        ShadowedAlgorithm::new(VerifyAuthenticationPathForLeafAndIndexList {
            list_type: ListType::Unsafe,
        })
        .test();
    }

    #[test]
    fn negative_test() {
        let mut rng = thread_rng();
        let seed: [u8; 32] = rng.gen();
        let vap4lail = VerifyAuthenticationPathForLeafAndIndexList {
            list_type: ListType::Unsafe,
        };
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
                ShadowedAlgorithm::new(vap4lail.clone()).rust_shadow_wrapper(
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
            let code = link_for_isolated_run(Rc::new(RefCell::new(vap4lail.clone())), 0);
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
    use super::*;
    use crate::traits::algorithm::ShadowedAlgorithm;
    use crate::traits::rust_shadow::RustShadow;

    #[test]
    fn vap4lail_benchmark() {
        ShadowedAlgorithm::new(VerifyAuthenticationPathForLeafAndIndexList {
            list_type: ListType::Unsafe,
        })
        .bench();
    }
}
