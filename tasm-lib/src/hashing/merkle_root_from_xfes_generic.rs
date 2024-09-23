use triton_vm::prelude::*;
use twenty_first::math::x_field_element::EXTENSION_DEGREE;

use crate::data_type::DataType;
use crate::hashing::merkle_root::MerkleRoot;
use crate::library::Library;
use crate::memory::dyn_malloc::DynMalloc;
use crate::traits::basic_snippet::BasicSnippet;

/// Calculate a Merkle root from a list of X-field elements.
/// The input list must have a length that is a power of two
/// and is not one. Can otherwise handle any length.
pub struct MerkleRootFromXfesGeneric;

impl BasicSnippet for MerkleRootFromXfesGeneric {
    fn inputs(&self) -> Vec<(DataType, String)> {
        vec![(
            DataType::List(Box::new(DataType::Xfe)),
            "*leafs".to_string(),
        )]
    }

    fn outputs(&self) -> Vec<(DataType, String)> {
        vec![(DataType::Digest, "root".to_string())]
    }

    fn entrypoint(&self) -> String {
        "tasmlib_hashing_merkle_root_from_xfes_generic".to_string()
    }

    fn code(&self, library: &mut Library) -> Vec<LabelledInstruction> {
        let entrypoint = self.entrypoint();
        let dyn_malloc = library.import(Box::new(DynMalloc));
        let merkle_root = library.import(Box::new(MerkleRoot));

        let build_1st_layer = format!("{entrypoint}_build_parent_layer");
        let build_1st_layer_code = triton_asm!(
            // INVARIANT: _ (*parent_nodes - 4) *parent_digests[n] 0 0 0 0 *xfes[2*n]_last_word
            {build_1st_layer}:
                push 0
                push 0
                dup 2
                read_mem {EXTENSION_DEGREE}
                // _ (*parent_nodes - 4) *parent_digests[n] 0 0 0 0 *xfes[2*n] [0 0 right_xfe] *xfes[2*n - 1]

                push 0
                push 0
                swap 2
                // _ (*parent_nodes - 4) *parent_digests[n] 0 0 0 0 *xfes[2*n] [0 0 right_xfe] 0 0 *xfes[2*n - 1]

                read_mem {EXTENSION_DEGREE}
                // _ (*parent_nodes - 4) *parent_digests[n] 0 0 0 0 *xfes[2*n] [0 0 right_xfe] [0 0 left_xfe] *xfes[2*(n-1)]
                // _ (*parent_nodes - 4) *parent_digests[n] 0 0 0 0 *xfes[2*n] [0 0 right_xfe] [0 0 left_xfe] *xfes[2*n]'

                swap 11
                pop 1
                // _ (*parent_nodes - 4) *parent_digests[n] 0 0 0 0 *xfes[2*n]' [0 0 right_xfe] [0 0 left_xfe]

                hash
                // _ (*parent_nodes - 4) *parent_digests[n] 0 0 0 0 *xfes[2*n]' [parent_digest]

                dup 10
                write_mem {Digest::LEN}
                // _ (*parent_nodes - 4) *parent_digests[n] 0 0 0 0 *xfes[2*n]' *parent_digests[n+1]

                push -10
                add
                // _ (*parent_nodes - 4) *parent_digests[n] 0 0 0 0 *xfes[2*n]' *parent_digests[n-1]

                swap 6
                pop 1
                // _ (*parent_nodes - 4) *parent_digests[n-1] 0 0 0 0 *xfes[2*n]'
                // _ (*parent_nodes - 4) *parent_digests[n]'  0 0 0 0 *xfes[2*n]'

                recurse_or_return
        );

        triton_asm!(
                // BEGIN: _ *leafs
            {entrypoint}:
                // Strategy: Construct the 1st parent layer and store it as a
                // list in memory.

                read_mem 1
                push 1
                add
                swap 1
                // _ *xfes len

                /* Verify len != 1 and len is power of two */
                dup 0
                pop_count
                push 1
                eq
                assert
                dup 0
                push 1
                eq
                push 0
                eq
                assert
                // _ *xfes len

                push 2
                dup 1
                div_mod
                pop 1

                dup 0
                call {dyn_malloc}
                // _ *xfes len (len / 2) (len / 2) *parent_nodes

                write_mem 1
                // _ *xfes len (len / 2) *parent_nodes[0]

                swap 1
                // _ *xfes len *parent_nodes[0] (len / 2)

                push -1
                add
                // _ *xfes len *parent_nodes[0] (len / 2 - 1)

                push {Digest::LEN}
                mul
                // _ *xfes len *parent_nodes[0] parent_offset_last_element

                dup 1
                add
                // _ *xfes len *parent_nodes[0] *parent_nodes[last]

                swap 2
                swap 1
                // _ *xfes *parent_nodes[last] len *parent_nodes[0]

                push {-(Digest::LEN as isize)}
                add
                // _ *xfes *parent_nodes[last] len (*parent_nodes - 4)

                swap 3
                swap 1
                // _ (*parent_nodes - 4) *parent_nodes[last] *xfes len

                push {EXTENSION_DEGREE}
                mul
                // _ (*parent_nodes - 4) *parent_nodes[last] *xfes xfe_offset_last_word

                add
                // _ (*parent_nodes - 4) *parent_nodes[last] *xfes[last]_last_word

                push 0
                push 0
                push 0
                push 0
                swap 4
                // _ (*parent_nodes - 4) *parent_nodes[last] 0 0 0 0 *xfes[last]_last_word

                call {build_1st_layer}
                // _ (*parent_nodes - 4) *parent_digests[n] 0 0 0 0 *xfes[2*n]_last_word

                pop 5
                pop 1
                // _ (*parent_nodes - 4)

                push {Digest::LEN - 1}
                add
                // _ *parent_digests

                call {merkle_root}
                // _ [merkle_root]

                return

                {&build_1st_layer_code}
        )
    }
}

#[cfg(test)]
mod test {
    use std::collections::HashMap;

    use itertools::Itertools;
    use num::One;
    use num::Zero;
    use rand::prelude::*;
    use twenty_first::prelude::MerkleTreeMaker;
    use twenty_first::util_types::merkle_tree::CpuParallel;
    use twenty_first::util_types::merkle_tree::MerkleTree;

    use super::*;
    use crate::memory::encode_to_memory;
    use crate::prelude::TasmObject;
    use crate::rust_shadowing_helper_functions::dyn_malloc::dynamic_allocator;
    use crate::rust_shadowing_helper_functions::list::list_new;
    use crate::rust_shadowing_helper_functions::list::list_push;
    use crate::snippet_bencher::BenchmarkCase;
    use crate::test_helpers::negative_test;
    use crate::traits::function::Function;
    use crate::traits::function::FunctionInitialState;
    use crate::traits::function::ShadowedFunction;
    use crate::traits::rust_shadow::RustShadow;
    use crate::InitVmState;

    impl Function for MerkleRootFromXfesGeneric {
        fn rust_shadow(
            &self,
            stack: &mut Vec<BFieldElement>,
            memory: &mut std::collections::HashMap<BFieldElement, BFieldElement>,
        ) {
            let leafs_pointer = stack.pop().unwrap();
            let leafs = *Vec::<XFieldElement>::decode_from_memory(memory, leafs_pointer).unwrap();
            assert!(
                !leafs.len().is_one(),
                "This algorithm currently cannot handle an input length
                     of one. Feel free to add that support."
            );
            let leafs: Vec<Digest> = leafs.into_iter().map(|x| x.into()).collect();

            let mt: MerkleTree = CpuParallel::from_digests(&leafs).unwrap();
            let root = mt.root();

            // Write entire Merkle tree to memory, because that's what the VM does
            let digests_in_layer_one = dynamic_allocator(memory);
            list_new(digests_in_layer_one, memory);
            for node_count in 0..(leafs.len() >> 1) {
                let node_index = node_count + (1 << (mt.height() - 1));
                let node = mt.node(node_index).unwrap();
                list_push(
                    digests_in_layer_one,
                    node.values().to_vec(),
                    memory,
                    Digest::LEN,
                )
            }

            let pointer = dynamic_allocator(memory);
            for layer in 2..(mt.height() + 1) {
                for node_count in 0..(leafs.len() >> layer) {
                    let node_index = node_count + (1 << (mt.height() - layer));
                    let node = mt.node(node_index).unwrap();
                    let pointer = pointer + BFieldElement::new((node_index * Digest::LEN) as u64);
                    encode_to_memory(memory, pointer, &node);
                }
            }

            stack.push(root.0[4]);
            stack.push(root.0[3]);
            stack.push(root.0[2]);
            stack.push(root.0[1]);
            stack.push(root.0[0]);
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
                None => 1 << rng.gen_range(1..=10),
            };

            let digests_pointer = rng.gen();

            let leafs = (0..num_leafs)
                .map(|_| rng.gen::<XFieldElement>())
                .collect_vec();

            self.init_state(leafs, digests_pointer)
        }

        fn corner_case_initial_states(&self) -> Vec<FunctionInitialState> {
            let height_1 = self.init_state(vec![XFieldElement::one(); 2], BFieldElement::zero());
            let height_2 = self.init_state(vec![XFieldElement::one(); 4], BFieldElement::zero());
            let height_3 = self.init_state(vec![XFieldElement::one(); 8], BFieldElement::zero());

            vec![height_1, height_2, height_3]
        }
    }

    impl MerkleRootFromXfesGeneric {
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

    #[test]
    fn test() {
        ShadowedFunction::new(MerkleRootFromXfesGeneric).test()
    }

    #[test]
    fn cannot_handle_input_list_of_length_one() {
        let height_0 = MerkleRootFromXfesGeneric
            .init_state(vec![XFieldElement::one(); 1], BFieldElement::zero());
        negative_test(
            &ShadowedFunction::new(MerkleRootFromXfesGeneric),
            InitVmState::with_stack_and_memory(height_0.stack, height_0.memory),
            &[InstructionError::AssertionFailed],
        );
    }

    #[test]
    fn cannot_handle_input_list_of_length_not_pow2() {
        for bad_length in [3, 5, 6, 7, 9, 10, 11, 12, 13, 14, 15, 17] {
            let init_state = MerkleRootFromXfesGeneric.init_state(
                vec![XFieldElement::one(); bad_length],
                BFieldElement::zero(),
            );
            negative_test(
                &ShadowedFunction::new(MerkleRootFromXfesGeneric),
                InitVmState::with_stack_and_memory(init_state.stack, init_state.memory),
                &[InstructionError::AssertionFailed],
            );
        }
    }
}

#[cfg(test)]
mod benches {
    use super::*;
    use crate::traits::function::ShadowedFunction;
    use crate::traits::rust_shadow::RustShadow;

    #[test]
    fn merkle_root_bench() {
        ShadowedFunction::new(MerkleRootFromXfesGeneric).bench()
    }
}
