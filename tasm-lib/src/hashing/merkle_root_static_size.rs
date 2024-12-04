use itertools::Itertools;
use triton_vm::prelude::*;

use crate::data_type::DataType;
use crate::library::Library;
use crate::prelude::BasicSnippet;

/// Compute the Merkle root of a slice of `Digest`s
#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
pub struct MerkleRootStaticSize {
    /// Aka `height` of the Merkle tree
    pub log2_length: u8,

    /// Points to the dummy-digest at node index 0. The remaining elements are the
    /// nodes in the Merkle tree.
    pub nodes_pointer: BFieldElement,
}

impl MerkleRootStaticSize {
    fn num_leaves(&self) -> usize {
        1 << self.log2_length
    }

    fn num_nodes(&self) -> usize {
        2 * self.num_leaves()
    }
}

impl BasicSnippet for MerkleRootStaticSize {
    fn inputs(&self) -> Vec<(DataType, String)> {
        vec![]
    }

    fn outputs(&self) -> Vec<(DataType, String)> {
        vec![(DataType::Digest, "root".to_owned())]
    }

    fn entrypoint(&self) -> String {
        format!(
            "tasmlib_hashing_merkle_root_static_size_{}_at_{}",
            self.num_leaves(),
            self.nodes_pointer,
        )
    }

    fn code(&self, library: &mut Library) -> Vec<LabelledInstruction> {
        let entrypoint = self.entrypoint();

        let leaves_last_word = self.nodes_pointer
            + bfe!(self.num_nodes() as u32) * bfe!(Digest::LEN as u32) // last element
            - bfe!(1); // last word in last element
        let read_all_leaves = triton_asm![read_mem 5; self.num_leaves()];
        let read_leaf_layer = triton_asm!(
            push {leaves_last_word}
            {&read_all_leaves}
            pop 1
            // _ [leaf_{n-1}] … [leaf_1] [leaf_0]
        );

        if self.log2_length == 0 {
            // the (only) leaf _is_ the root
            return triton_asm!({entrypoint}: {&read_leaf_layer} return);
        } else if self.log2_length == 1 {
            // root is never stored, only returned
            return triton_asm!(
                {entrypoint}:
                    {&read_leaf_layer}
                    // _ [right] [left]
                    hash
                    return
            );
        }

        let hash_one_leaf_and_write_to_ram = |left_child_node_index: u32| {
            let parent_node_index = left_child_node_index / 2;
            triton_asm!(
                hash
                push {self.nodes_pointer + bfe!(parent_node_index) * bfe!(Digest::LEN as u32)}
                write_mem {Digest::LEN}
                pop 1
            )
        };
        let hash_leaves_and_write_first_layer = (0..self.num_leaves())
            .step_by(2)
            .map(|left_leaf_index| (left_leaf_index + self.num_leaves()) as u32)
            .map(hash_one_leaf_and_write_to_ram)
            .collect_vec()
            .concat();

        let build_smaller_tree = MerkleRootStaticSize {
            log2_length: self.log2_length - 1,
            nodes_pointer: self.nodes_pointer,
        };
        let build_smaller_tree_label = library.import(Box::new(build_smaller_tree));

        triton_asm!(
            {entrypoint}:
                {&read_leaf_layer}
                {&hash_leaves_and_write_first_layer}
                call {build_smaller_tree_label}
                return
        )
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use proptest_arbitrary_interop::arb;
    use rand::prelude::*;
    use test_strategy::proptest;
    use triton_vm::twenty_first::prelude::*;

    use super::*;
    use crate::memory::encode_to_memory;
    use crate::rust_shadowing_helper_functions::array::array_from_memory;
    use crate::rust_shadowing_helper_functions::array::insert_as_array;
    use crate::snippet_bencher::BenchmarkCase;
    use crate::traits::function::Function;
    use crate::traits::function::FunctionInitialState;
    use crate::traits::function::ShadowedFunction;
    use crate::traits::rust_shadow::RustShadow;

    impl Function for MerkleRootStaticSize {
        fn rust_shadow(
            &self,
            stack: &mut Vec<BFieldElement>,
            memory: &mut HashMap<BFieldElement, BFieldElement>,
        ) {
            let leafs_pointer =
                self.nodes_pointer + bfe!(self.num_leaves() as u32) * bfe!(Digest::LEN as u32);
            let leafs = array_from_memory::<Digest>(leafs_pointer, self.num_leaves(), memory);
            let mt = MerkleTree::new::<CpuParallel>(&leafs).unwrap();

            let num_not_leaf_nodes = self.num_leaves() as u32;

            // `.skip(2)`: dummy-digest at index 0, root at index 1
            for (node_index, node) in (0..num_not_leaf_nodes).zip(mt.nodes()).skip(2) {
                let node_address = self.nodes_pointer + bfe!(node_index) * bfe!(Digest::LEN as u32);
                encode_to_memory(memory, node_address, node);
            }

            stack.extend(mt.root().reversed().values());
        }

        fn pseudorandom_initial_state(
            &self,
            seed: [u8; 32],
            _: Option<BenchmarkCase>,
        ) -> FunctionInitialState {
            let mut rng = StdRng::from_seed(seed);
            let leafs = (0..self.num_leaves()).map(|_| rng.gen()).collect_vec();
            self.init_state(leafs)
        }
    }

    impl MerkleRootStaticSize {
        fn init_state(&self, leafs: Vec<Digest>) -> FunctionInitialState {
            assert_eq!(self.num_leaves(), leafs.len());

            let stack = self.init_stack_for_isolated_run();
            let mut memory = HashMap::new();
            let leafs_pointer =
                self.nodes_pointer + bfe!(self.num_leaves() as u32) * bfe!(Digest::LEN as u32);
            insert_as_array(leafs_pointer, &mut memory, leafs);

            FunctionInitialState { stack, memory }
        }
    }

    #[test]
    fn merkle_root_for_tree_of_height_0_can_be_computed() {
        let shadowed_function = MerkleRootStaticSize {
            log2_length: 0,
            nodes_pointer: bfe!(42),
        };
        ShadowedFunction::new(shadowed_function).test();
    }

    #[test]
    fn merkle_root_for_tree_of_height_1_can_be_computed() {
        let shadowed_function = MerkleRootStaticSize {
            log2_length: 1,
            nodes_pointer: bfe!(42),
        };
        ShadowedFunction::new(shadowed_function).test();
    }

    #[proptest(cases = 10)]
    fn test(#[strategy(0u8..=8)] log2_length: u8, #[strategy(arb())] nodes_pointer: BFieldElement) {
        let shadowed_function = MerkleRootStaticSize {
            log2_length,
            nodes_pointer,
        };
        ShadowedFunction::new(shadowed_function).test();
    }
}

#[cfg(test)]
mod benches {
    use super::*;
    use crate::traits::function::ShadowedFunction;
    use crate::traits::rust_shadow::RustShadow;

    fn bench_case(log2_length: u8) {
        let shadowed_function = MerkleRootStaticSize {
            log2_length,
            nodes_pointer: bfe!(0),
        };
        ShadowedFunction::new(shadowed_function).bench();
    }

    #[test]
    fn merkle_root_bench_512() {
        bench_case(9);
    }

    #[test]
    fn merkle_root_bench_1024() {
        bench_case(10);
    }
}
