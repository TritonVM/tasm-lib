use itertools::Itertools;
use triton_vm::instruction::LabelledInstruction;
use triton_vm::prelude::*;
use twenty_first::prelude::*;
use x_field_element::EXTENSION_DEGREE;

use crate::data_type::ArrayType;
use crate::data_type::DataType;
use crate::hashing::merkle_root_static_size::MerkleRootStaticSize;
use crate::library::Library;
use crate::traits::basic_snippet::BasicSnippet;

pub struct MerkleRootFromXfesStaticSize {
    /// Aka `height` of the Merkle tree
    pub log2_length: u8,

    /// A section of memory this snippet can use to store intermediate node
    /// digests. It's the caller's responsibility to allocate enough memory.
    pub static_memory_pointer: BFieldElement,
}

impl MerkleRootFromXfesStaticSize {
    fn num_leaves(&self) -> usize {
        1 << self.log2_length
    }
}

impl BasicSnippet for MerkleRootFromXfesStaticSize {
    fn inputs(&self) -> Vec<(DataType, String)> {
        vec![(
            DataType::Array(Box::new(ArrayType {
                element_type: DataType::Xfe,
                length: self.num_leaves(),
            })),
            "*xfes".to_owned(),
        )]
    }

    fn outputs(&self) -> Vec<(DataType, String)> {
        vec![(DataType::Digest, "root".to_owned())]
    }

    fn entrypoint(&self) -> String {
        format!(
            "tasmlib_hashing_merkle_root_from_xfes_static_size_{}_at_{}",
            self.num_leaves(),
            self.static_memory_pointer,
        )
    }

    fn code(&self, library: &mut Library) -> Vec<LabelledInstruction> {
        assert!(self.log2_length >= 1, "need at least 1 actual layer");
        let entrypoint = self.entrypoint();

        let xfe_to_digest_on_stack = triton_asm!(
            push 0
            push 0
            swap 2
            read_mem {EXTENSION_DEGREE}
        );
        let all_xfes_as_digests_on_stack = vec![xfe_to_digest_on_stack; self.num_leaves()].concat();
        let xfes_pointer_last_word_offset = self.num_leaves() * EXTENSION_DEGREE - 1;

        let hash_one_leaf_and_write_to_ram = |left_child_node_index: u32| {
            let parent_index = left_child_node_index / 2;
            triton_asm!(
                hash
                push {self.static_memory_pointer + bfe!(parent_index) * bfe!(Digest::LEN as u32)}
                write_mem {Digest::LEN}
                pop 1
            )
        };
        let hash_leaves_and_write_first_layer = (0..self.num_leaves())
            .step_by(2)
            .map(|left_child_leaf_index| (left_child_leaf_index + self.num_leaves()) as u32)
            .map(hash_one_leaf_and_write_to_ram)
            .collect_vec()
            .concat();

        let merke_root_snippet = MerkleRootStaticSize {
            log2_length: self.log2_length - 1,
            nodes_pointer: self.static_memory_pointer,
        };
        let merkle_root_snippet_label = library.import(Box::new(merke_root_snippet));

        triton_asm!(
            {entrypoint}:
                // _ *xfes

                push {xfes_pointer_last_word_offset}
                add
                // _ *xfes_last_elem_last_word

                {&all_xfes_as_digests_on_stack}
                pop 1
                // _ [[leaf]; self.num_leaves()]

                {&hash_leaves_and_write_first_layer}
                // _

                call {merkle_root_snippet_label}

                return
        )
    }
}

#[cfg(test)]
mod test {
    use std::collections::HashMap;

    use num::Zero;
    use proptest_arbitrary_interop::arb;
    use rand::prelude::*;
    use test_strategy::proptest;

    use crate::memory::encode_to_memory;
    use crate::rust_shadowing_helper_functions::array::array_from_memory;
    use crate::rust_shadowing_helper_functions::array::insert_as_array;
    use crate::snippet_bencher::BenchmarkCase;
    use crate::traits::function::Function;
    use crate::traits::function::FunctionInitialState;
    use crate::traits::function::ShadowedFunction;
    use crate::traits::rust_shadow::RustShadow;

    use super::*;

    impl Function for MerkleRootFromXfesStaticSize {
        fn rust_shadow(
            &self,
            stack: &mut Vec<BFieldElement>,
            memory: &mut HashMap<BFieldElement, BFieldElement>,
        ) {
            let xfes_pointer = stack.pop().unwrap();
            let xfes = array_from_memory::<XFieldElement>(xfes_pointer, self.num_leaves(), memory);
            let as_digests: Vec<Digest> = xfes.into_iter().map(|x| x.into()).collect_vec();
            let mt = MerkleTree::new::<CpuParallel>(&as_digests).unwrap();

            let num_not_leaf_nodes = self.num_leaves() as u32;

            // `.skip(2)`: dummy-digest at index 0, root at index 1
            // In the case where the input leaf length is one, the root
            // is written to memory. Otherwise, it is not.
            let num_skips = if self.log2_length == 1 { 1 } else { 2 };
            for (node_index, &node) in (0..num_not_leaf_nodes).zip(mt.nodes()).skip(num_skips) {
                let node_address =
                    self.static_memory_pointer + bfe!(node_index) * bfe!(Digest::LEN as u32);
                encode_to_memory(memory, node_address, &node);
            }

            stack.extend(mt.root().reversed().values());
        }

        fn pseudorandom_initial_state(
            &self,
            seed: [u8; 32],
            _bench_case: Option<BenchmarkCase>,
        ) -> FunctionInitialState {
            let mut rng: StdRng = SeedableRng::from_seed(seed);
            let leafs = (0..self.num_leaves()).map(|_| rng.gen()).collect_vec();
            self.init_state(leafs, BFieldElement::zero())
        }
    }

    impl MerkleRootFromXfesStaticSize {
        fn init_state(
            &self,
            leafs: Vec<XFieldElement>,
            leafs_pointer: BFieldElement,
        ) -> FunctionInitialState {
            assert_eq!(self.num_leaves(), leafs.len());

            let stack = [self.init_stack_for_isolated_run(), vec![leafs_pointer]].concat();
            let mut memory = HashMap::new();
            insert_as_array(leafs_pointer, &mut memory, leafs);

            FunctionInitialState { stack, memory }
        }
    }

    pub(super) const STATIC_MEMORY_POINTER_FOR_TESTS: BFieldElement =
        BFieldElement::new(BFieldElement::MAX - (1 << 30));

    #[test]
    fn merkle_root_from_xfes_height_1() {
        let shadowed_function = MerkleRootFromXfesStaticSize {
            log2_length: 1,
            static_memory_pointer: STATIC_MEMORY_POINTER_FOR_TESTS,
        };
        ShadowedFunction::new(shadowed_function).test();
    }

    #[test]
    fn merkle_root_from_xfes_height_2() {
        let shadowed_function = MerkleRootFromXfesStaticSize {
            log2_length: 2,
            static_memory_pointer: STATIC_MEMORY_POINTER_FOR_TESTS,
        };
        ShadowedFunction::new(shadowed_function).test();
    }

    #[proptest(cases = 10)]
    fn merkle_root_from_xfes_static_size_pbt_pbt(
        #[strategy(1u8..=8)] log2_length: u8,
        #[strategy(arb())] static_memory_pointer: BFieldElement,
    ) {
        let shadowed_function = MerkleRootFromXfesStaticSize {
            log2_length,
            static_memory_pointer,
        };
        ShadowedFunction::new(shadowed_function).test();
    }
}

#[cfg(test)]
mod benches {
    use test::STATIC_MEMORY_POINTER_FOR_TESTS;

    use crate::traits::function::ShadowedFunction;
    use crate::traits::rust_shadow::RustShadow;

    use super::*;

    fn bench_case(log2_length: u8) {
        let shadowed_function = MerkleRootFromXfesStaticSize {
            log2_length,
            static_memory_pointer: STATIC_MEMORY_POINTER_FOR_TESTS,
        };
        ShadowedFunction::new(shadowed_function).bench();
    }

    #[test]
    fn merkle_root_from_xfes_bench_512() {
        bench_case(9);
    }

    #[test]
    fn merkle_root_from_xfes_bench_1024() {
        bench_case(10);
    }
}
