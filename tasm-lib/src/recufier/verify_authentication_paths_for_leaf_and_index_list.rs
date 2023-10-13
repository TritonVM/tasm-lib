use std::collections::HashMap;

use itertools::Itertools;
use rand::{rngs::StdRng, Rng, SeedableRng};
use triton_vm::{triton_asm, BFieldElement, NonDeterminism};
use twenty_first::util_types::{
    merkle_tree::{CpuParallel, MerkleTree},
    merkle_tree_maker::MerkleTreeMaker,
};

use crate::{
    algorithm::Algorithm,
    empty_stack,
    list::{
        safe_u32::{get::SafeGet, length::SafeLength},
        unsafe_u32::{get::UnsafeGet, length::UnsafeLength},
        ListType,
    },
    recufier::merkle_verify::MerkleVerify,
    snippet::{BasicSnippet, DataType},
    structure::tasm_object::{load_to_memory, TasmObject},
    Digest, VmHasher,
};

#[derive(Debug, Clone)]
pub struct VerifyAuthenticationPathForLeafAndIndexList {
    list_type: ListType,
}

impl BasicSnippet for VerifyAuthenticationPathForLeafAndIndexList {
    fn inputs(&self) -> Vec<(crate::snippet::DataType, String)> {
        vec![
            (
                DataType::List(Box::new(DataType::Tuple(vec![
                    DataType::Digest,
                    DataType::U32,
                ]))),
                "leaf_and_index_list".to_string(),
            ),
            (DataType::Digest, "root".to_string()),
            (DataType::U32, "height".to_string()),
        ]
    }

    fn outputs(&self) -> Vec<(crate::snippet::DataType, String)> {
        self.inputs()
    }

    fn entrypoint(&self) -> String {
        "tasm_recufier_verify_authentication_paths_for_leaf_and_index_list".to_string()
    }

    fn code(
        &self,
        library: &mut crate::library::Library,
    ) -> Vec<triton_vm::instruction::LabelledInstruction> {
        let entrypoint = self.entrypoint();
        let main_loop = format!("{entrypoint}_main_loop");
        let data_type = DataType::Tuple(vec![DataType::Digest, DataType::U32]);
        let lai_length = match self.list_type {
            ListType::Safe => library.import(Box::new(SafeLength(data_type.clone()))),
            ListType::Unsafe => library.import(Box::new(UnsafeLength(data_type.clone()))),
        };
        let lai_get = match self.list_type {
            ListType::Safe => library.import(Box::new(SafeGet(data_type))),
            ListType::Unsafe => library.import(Box::new(UnsafeGet(data_type))),
        };
        let merkle_verify = library.import(Box::new(MerkleVerify));
        triton_asm! {
            // BEFORE: _ leaf_and_index_list root4 root3 root2 root1 root0 height
            // AFTER: _ leaf_and_index_list root4 root3 root2 root1 root0 height
            {entrypoint}:
                dup 6               // _ leaf_and_index_list root4 root3 root2 root1 root0 height leaf_and_index_list
                call {lai_length}   // _ leaf_and_index_list root4 root3 root2 root1 root0 height length
                push 0              // _ leaf_and_index_list root4 root3 root2 root1 root0 height length 0

                call {main_loop}    // _ leaf_and_index_list root4 root3 root2 root1 root0 height length length

                pop pop
                return

            // INVARIANT: _ leaf_and_index_list root4 root3 root2 root1 root0 height length iteration
            {main_loop}:
                // evaluate termination criterion
                dup 1 dup 1     // _ leaf_and_index_list root4 root3 root2 root1 root0 height length iteration length iteration
                eq              // _ leaf_and_index_list root4 root3 root2 root1 root0 height length iteration length==iteration
                skiz return     // _ leaf_and_index_list root4 root3 root2 root1 root0 height length iteration

                // duplicate root
                dup 7  dup 7 dup 7 dup 7 dup 7
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
        println!("height at shadow: {}", height);
        let root = Digest::new([
            stack.pop().unwrap(),
            stack.pop().unwrap(),
            stack.pop().unwrap(),
            stack.pop().unwrap(),
            stack.pop().unwrap(),
        ]);
        let address = stack.pop().unwrap();

        // read object from memory
        let indices_and_leafs = *Vec::<(Digest, u32)>::decode_from_memory(memory, address).unwrap();

        // iterate and verify
        let mut digest_index = 0;
        for (leaf, index) in indices_and_leafs {
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
    ) -> (
        Vec<triton_vm::BFieldElement>,
        std::collections::HashMap<triton_vm::BFieldElement, triton_vm::BFieldElement>,
        triton_vm::NonDeterminism<triton_vm::BFieldElement>,
    ) {
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
        let num_indices = rng.gen_range(0..5) as usize;

        // generate data structure
        let leafs = (0..n).map(|_| rng.gen::<Digest>()).collect_vec();
        let tree = <CpuParallel as MerkleTreeMaker<VmHasher>>::from_digests(&leafs);
        let root = tree.get_root();

        let indices = (0..num_indices)
            .map(|_| rng.gen_range(0..n) as usize)
            .collect_vec();
        let indicated_leafs = indices.iter().map(|i| leafs[*i]).collect_vec();
        let leafs_and_indices = indicated_leafs
            .into_iter()
            .zip(indices.iter().map(|i| *i as u32))
            .collect_vec();
        let authentication_paths = indices
            .iter()
            .map(|i| tree.get_authentication_structure(&[*i]))
            .collect_vec();

        // prepare memory + stack + nondeterminism
        let mut memory: HashMap<BFieldElement, BFieldElement> = HashMap::new();
        let address = load_to_memory(&mut memory, leafs_and_indices);
        let mut stack = empty_stack();
        stack.push(address);
        stack.push(root.0[4]);
        stack.push(root.0[3]);
        stack.push(root.0[2]);
        stack.push(root.0[1]);
        stack.push(root.0[0]);
        stack.push(BFieldElement::new(height as u64));
        let nondeterminism = NonDeterminism::<BFieldElement>::new(vec![])
            .with_digests(authentication_paths.into_iter().flatten().collect_vec());

        (stack, memory, nondeterminism)
    }
}

#[cfg(test)]
mod test {
    use std::{cell::RefCell, rc::Rc};

    use rand::{thread_rng, Rng};
    use triton_vm::BFieldElement;
    use twenty_first::util_types::algebraic_hasher::Domain;

    use crate::{
        algorithm::{Algorithm, ShadowedAlgorithm},
        execute_with_terminal_state,
        linker::link_for_isolated_run,
        list::ListType,
        program_with_state_preparation,
        snippet::RustShadow,
        VmHasherState,
    };

    use super::VerifyAuthenticationPathForLeafAndIndexList;

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
            let (mut stack, memory, mut nondeterminism) =
                vap4lail.pseudorandom_initial_state(seed, None);
            println!("height after generation: {}", stack.last().unwrap());
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
                let mut rust_memory = memory.clone();
                ShadowedAlgorithm::new(vap4lail.clone()).rust_shadow_wrapper(
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
            let code = link_for_isolated_run(Rc::new(RefCell::new(vap4lail.clone())), 0);
            let program = program_with_state_preparation(&code, &stack, &mut nondeterminism, None);
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
