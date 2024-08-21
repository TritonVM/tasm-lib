use tip5::Digest;
use triton_vm::prelude::*;

use crate::data_type::DataType;
use crate::hashing::merkle_root_from_xfes_static_size::MerkleRootFromXfesStaticSize;
use crate::traits::basic_snippet::BasicSnippet;

/// Calculate a Merkle root from a list of X-field elements. List must have
/// length 256, 512, or 1024.
pub struct MerkleRootFromXfesWrapper;

const MAX_LENGTH_SUPPORTED: u32 = 1024;

impl BasicSnippet for MerkleRootFromXfesWrapper {
    fn inputs(&self) -> Vec<(DataType, String)> {
        vec![(DataType::List(Box::new(DataType::Xfe)), "*xfes".to_owned())]
    }

    fn outputs(&self) -> Vec<(DataType, String)> {
        vec![(DataType::Digest, "root".to_owned())]
    }

    fn entrypoint(&self) -> String {
        "tasmlib_hashing_merkle_root_from_xfes_wrapper".to_owned()
    }

    fn code(&self, library: &mut crate::library::Library) -> Vec<LabelledInstruction> {
        let entrypoint = self.entrypoint();
        let length_pointer = library.kmalloc(1);

        let pointer_for_node_memory = library.kmalloc(MAX_LENGTH_SUPPORTED * (Digest::LEN as u32));

        let snippet_for_length_256 = MerkleRootFromXfesStaticSize {
            log2_length: 8,
            static_memory_pointer: pointer_for_node_memory,
        };
        let snippet_for_length_256 = library.import(Box::new(snippet_for_length_256));
        let snippet_for_length_512 = MerkleRootFromXfesStaticSize {
            log2_length: 9,
            static_memory_pointer: pointer_for_node_memory,
        };
        let snippet_for_length_512 = library.import(Box::new(snippet_for_length_512));
        let snippet_for_length_1024 = MerkleRootFromXfesStaticSize {
            log2_length: 10,
            static_memory_pointer: pointer_for_node_memory,
        };
        let snippet_for_length_1024 = library.import(Box::new(snippet_for_length_1024));

        triton_asm!(
            {entrypoint}:
                // _ *xfes
                read_mem 1
                push 2
                add
                swap 1
                // _ (*xfes + 1) len

                /* Check if length is supported */
                dup 0
                push 256
                eq
                dup 1
                push 512
                eq
                dup 2
                push 1024
                eq
                // _ (*xfes + 1) len (len == 256) (len == 512) (len == 1024)

                add
                add
                // _ (*xfes + 1) len (len == 256 || len == 512 || len == 1024)

                assert
                // _ (*xfes + 1) len

                dup 0
                push {length_pointer}
                write_mem 1
                pop 1
                // _ (*xfes + 1) len

                push 256
                eq
                skiz
                    call {snippet_for_length_256}
                // _ ((*xfes + 1)|[root])

                push {length_pointer}
                read_mem 1
                pop 1
                push 512
                eq
                skiz
                    call {snippet_for_length_512}
                // _ ((*xfes + 1)|[root])

                push {length_pointer}
                read_mem 1
                pop 1
                push 1024
                eq
                skiz
                    call {snippet_for_length_1024}
                // _ [root]

                return
        )
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use itertools::Itertools;
    use rand::prelude::*;
    use test_strategy::proptest;
    use triton_vm::twenty_first::prelude::*;

    use crate::library::STATIC_MEMORY_FIRST_ADDRESS;
    use crate::memory::encode_to_memory;
    use crate::prelude::TasmObject;
    use crate::snippet_bencher::BenchmarkCase;
    use crate::traits::function::Function;
    use crate::traits::function::FunctionInitialState;
    use crate::traits::function::ShadowedFunction;
    use crate::traits::rust_shadow::RustShadow;

    use super::*;

    #[proptest(cases = 4)]
    fn merkle_root_from_xfes_wrapper_pbt_pbt() {
        ShadowedFunction::new(MerkleRootFromXfesWrapper).test();
    }

    impl Function for MerkleRootFromXfesWrapper {
        fn rust_shadow(
            &self,
            stack: &mut Vec<BFieldElement>,
            memory: &mut std::collections::HashMap<BFieldElement, BFieldElement>,
        ) {
            let leafs_pointer = stack.pop().unwrap();
            let xfes = *Vec::<XFieldElement>::decode_from_memory(memory, leafs_pointer).unwrap();
            let xfes_len = xfes.len();
            assert!(
                xfes_len == 256 || xfes_len == 512 || xfes_len == 1024,
                "This algorithm currently can currently only handle lengths of 256, 512, and 1024."
            );
            let as_digests: Vec<Digest> = xfes.into_iter().map(|x| x.into()).collect_vec();
            let mt = MerkleTree::new::<CpuParallel>(&as_digests).unwrap();
            let num_not_leaf_nodes = xfes_len as u32;

            // Modify statically allocated memory as the above snippet does.
            // `.skip(2)`: dummy-digest at index 0, root at index 1
            let num_skips = 2;
            for (node_index, &node) in (0..num_not_leaf_nodes).zip(mt.nodes()).skip(num_skips) {
                let node_address = Self::static_memory_address_for_isolated_run_nodes()
                    + bfe!(node_index) * bfe!(Digest::LEN as u32);
                encode_to_memory(memory, node_address, &node);
            }

            memory.insert(
                Self::static_memory_address_for_isolated_run_length(),
                bfe!(xfes_len as u32),
            );

            stack.extend(mt.root().reversed().values());
        }

        fn pseudorandom_initial_state(
            &self,
            seed: [u8; 32],
            bench_case: Option<BenchmarkCase>,
        ) -> FunctionInitialState {
            let mut rng: StdRng = SeedableRng::from_seed(seed);
            let num_leafs = match bench_case {
                Some(BenchmarkCase::CommonCase) => 512,
                Some(BenchmarkCase::WorstCase) => 1024,
                None => 1 << rng.gen_range(8..=10),
            };

            let digests_pointer = rng.gen();

            let leafs = (0..num_leafs)
                .map(|_| rng.gen::<XFieldElement>())
                .collect_vec();

            self.init_state(leafs, digests_pointer)
        }
    }

    impl MerkleRootFromXfesWrapper {
        fn static_memory_address_for_isolated_run_nodes() -> BFieldElement {
            STATIC_MEMORY_FIRST_ADDRESS - bfe!(MAX_LENGTH_SUPPORTED * Digest::LEN as u32)
        }

        fn static_memory_address_for_isolated_run_length() -> BFieldElement {
            STATIC_MEMORY_FIRST_ADDRESS
        }

        fn init_state(
            &self,
            xfes: Vec<XFieldElement>,
            xfe_pointer: BFieldElement,
        ) -> FunctionInitialState {
            let mut memory = HashMap::<BFieldElement, BFieldElement>::new();
            encode_to_memory(&mut memory, xfe_pointer, &xfes);
            let mut stack = self.init_stack_for_isolated_run();
            stack.push(xfe_pointer);

            FunctionInitialState { stack, memory }
        }
    }
}

#[cfg(test)]
mod benches {
    use crate::traits::function::ShadowedFunction;
    use crate::traits::rust_shadow::RustShadow;

    use super::*;

    #[test]
    fn merkle_root_wrapper_bench() {
        ShadowedFunction::new(MerkleRootFromXfesWrapper).bench();
    }
}
