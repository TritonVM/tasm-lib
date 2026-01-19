use std::collections::HashMap;

use triton_vm::prelude::*;
use twenty_first::math::x_field_element::EXTENSION_DEGREE;

use crate::hashing::merkle_root::MerkleRoot;
use crate::prelude::*;
use crate::traits::basic_snippet::Reviewer;
use crate::traits::basic_snippet::SignOffFingerprint;

/// Calculate a Merkle root from a list of extension-field elements.
///
/// ### Behavior
///
/// ```text
/// BEFORE: _ *leafs
/// AFTER:  _ [root: Digest]
/// ```
///
/// ### Preconditions
///
/// - `*leafs` points to a list of [`XFieldElement`]s
/// - the length of the pointed-to list is greater than 1
/// - the length of the pointed-to list is a power of 2
/// - the length of the pointed-to list is a u32
///
/// ### Postconditions
///
/// None.
#[derive(Debug, Default, Copy, Clone, Eq, PartialEq, Hash)]
pub struct MerkleRootFromXfes;

impl MerkleRootFromXfes {
    pub const NUM_ELEMENTS_NOT_POWER_OF_2_ERROR_ID: i128 = 90;
}

impl BasicSnippet for MerkleRootFromXfes {
    fn parameters(&self) -> Vec<(DataType, String)> {
        let list_type = DataType::List(Box::new(DataType::Xfe));
        vec![(list_type, "*leafs".to_string())]
    }

    fn return_values(&self) -> Vec<(DataType, String)> {
        vec![(DataType::Digest, "root".to_string())]
    }

    fn entrypoint(&self) -> String {
        "tasmlib_hashing_merkle_root_from_xfes".to_string()
    }

    fn code(&self, library: &mut Library) -> Vec<LabelledInstruction> {
        let dyn_malloc = library.import(Box::new(DynMalloc));
        let merkle_root = library.import(Box::new(MerkleRoot));

        let entrypoint = self.entrypoint();
        let list_len_is_1 = format!("{entrypoint}_list_len_is_1");
        let build_1st_layer = format!("{entrypoint}_build_1st_layer");

        triton_asm!(
            // BEFORE: _ *leafs
            {entrypoint}:
                read_mem 1
                addi 1
                pick 1
                // _ *xfes len

                /* assert the number of elements is some power of 2 */
                dup 0
                pop_count
                push 1
                eq
                assert error_id {Self::NUM_ELEMENTS_NOT_POWER_OF_2_ERROR_ID}

                /* special case: list length is 1 */
                push 0      hint return_early: bool = stack[0]
                dup 1
                push 1
                eq
                skiz call {list_len_is_1}
                skiz return
                // _ *xfes len

                /* Strategy: Construct the 1st parent layer and store it as a list in memory. */
                push 2
                dup 1
                div_mod
                pop 1

                dup 0
                call {dyn_malloc}
                // _ *xfes len (len / 2) (len / 2) *parent_nodes

                write_mem 1
                // _ *xfes len (len / 2) *parent_nodes[0]

                pick 1
                // _ *xfes len *parent_nodes[0] (len / 2)

                addi -1
                // _ *xfes len *parent_nodes[0] (len / 2 - 1)

                push {Digest::LEN}
                mul
                // _ *xfes len *parent_nodes[0] parent_offset_last_element

                dup 1
                add
                // _ *xfes len *parent_nodes[0] *parent_nodes[last]

                place 2
                // _ *xfes *parent_nodes[last] len *parent_nodes[0]

                addi {-(Digest::LEN as isize)}
                // _ *xfes *parent_nodes[last] len (*parent_nodes - 4)

                place 3
                // _ (*parent_nodes - 4) *xfes *parent_nodes[last] len

                push {EXTENSION_DEGREE}
                mul
                // _ (*parent_nodes - 4) *xfes *parent_nodes[last] (len·3)

                pick 2
                add
                // _ (*parent_nodes - 4) *parent_nodes[last] *xfes[last]_last_word

                push 0
                push 0
                push 0
                push 0
                pick 4
                // _ (*parent_nodes - 4) *parent_nodes[last] 0 0 0 0 *xfes[last]_last_word

                call {build_1st_layer}
                // _ (*parent_nodes - 4) *parent_digests[n] 0 0 0 0 *xfes[2*n]_last_word

                pop 5
                pop 1
                // _ (*parent_nodes - 4)

                addi {Digest::LEN - 1}
                // _ *parent_digests

                call {merkle_root}
                // _ [merkle_root]

                return

            // BEFORE: _ *xfes 1 0
            // AFTER:  _ [0 0 xfes[0]] 1
            {list_len_is_1}:
                            hint filler = stack[0]
                            hint return_early: bool = stack[1]
                push 0      hint filler = stack[0]
                // _ *xfes 1 0 0

                pick 3
                addi {EXTENSION_DEGREE}
                read_mem {EXTENSION_DEGREE}
                            hint root: Digest = stack[1..6]
                pop 1
                // _ 1 [0 0 xfes[0]]

                pick 5
                return


            // INVARIANT: _ (*parent_nodes - 4) *parent_digests[n] 0 0 0 0 *xfes[2*n]_last_word
            {build_1st_layer}:
                push 0
                push 0
                pick 2
                read_mem {EXTENSION_DEGREE}
                // _ (*parent_nodes - 4) *parent_digests[n] 0 0 0 0 [0 0 right_xfe] *xfes[2*n-1]

                push 0
                push 0
                pick 2
                read_mem {EXTENSION_DEGREE}
                // _ (*parent_nodes - 4) *parent_digests[n] 0 0 0 0 [0 0 right_xfe] [0 0 left_xfe] *xfes[2*n-2]

                place 10
                // _ (*parent_nodes - 4) *parent_digests[n] 0 0 0 0 *xfes[2*n-2] [0 0 right_xfe] [0 0 left_xfe]

                hash
                // _ (*parent_nodes - 4) *parent_digests[n] 0 0 0 0 *xfes[2*n-2] [parent_digest]

                pick 10
                write_mem {Digest::LEN}
                // _ (*parent_nodes - 4) 0 0 0 0 *xfes[2*n-2] *parent_digests[n+1]

                addi -10
                // _ (*parent_nodes - 4) 0 0 0 0 *xfes[2*n-2] *parent_digests[n-1]

                place 5
                // _ (*parent_nodes - 4) *parent_digests[n-1] 0 0 0 0 *xfes[2*n-2]

                recurse_or_return
        )
    }

    fn sign_offs(&self) -> HashMap<Reviewer, SignOffFingerprint> {
        let mut sign_offs = HashMap::new();
        sign_offs.insert(Reviewer("ferdinand"), 0x57f2e812e29b71b8.into());

        sign_offs
    }
}

#[cfg(test)]
mod tests {
    use proptest::collection::vec;
    use twenty_first::util_types::merkle_tree::MerkleTree;

    use super::*;
    use crate::rust_shadowing_helper_functions::dyn_malloc::dynamic_allocator;
    use crate::rust_shadowing_helper_functions::list::list_new;
    use crate::rust_shadowing_helper_functions::list::list_push;
    use crate::test_helpers::test_assertion_failure;
    use crate::test_prelude::*;

    impl MerkleRootFromXfes {
        fn init_state(
            &self,
            leafs: Vec<XFieldElement>,
            leaf_pointer: BFieldElement,
        ) -> FunctionInitialState {
            let mut memory = HashMap::new();
            encode_to_memory(&mut memory, leaf_pointer, &leafs);

            let mut stack = self.init_stack_for_isolated_run();
            stack.push(leaf_pointer);

            FunctionInitialState { stack, memory }
        }
    }

    impl Function for MerkleRootFromXfes {
        fn rust_shadow(
            &self,
            stack: &mut Vec<BFieldElement>,
            memory: &mut HashMap<BFieldElement, BFieldElement>,
        ) {
            let leafs_pointer = stack.pop().unwrap();
            let leafs = *Vec::<XFieldElement>::decode_from_memory(memory, leafs_pointer).unwrap();
            let leafs = leafs.into_iter().map(Digest::from).collect_vec();
            let mt = MerkleTree::par_new(&leafs).unwrap();

            if leafs.len() == 1 {
                stack.extend(mt.root().reversed().values());
                return;
            }

            // Write entire Merkle tree to memory, because that's what the VM does
            let first_layer_pointer = dynamic_allocator(memory);
            list_new(first_layer_pointer, memory);
            for node_count in 0..(leafs.len() >> 1) {
                let node_index = node_count + (1 << (mt.height() - 1));
                let node = mt.node(node_index).unwrap();
                list_push(first_layer_pointer, node.values().to_vec(), memory)
            }

            let rest_of_tree_pointer = dynamic_allocator(memory);
            for layer in 2..=mt.height() {
                for node_count in 0..(leafs.len() >> layer) {
                    let node_index = node_count + (1 << (mt.height() - layer));
                    let node = mt.node(node_index).unwrap();
                    let pointer = rest_of_tree_pointer + bfe!(node_index * Digest::LEN);
                    encode_to_memory(memory, pointer, &node);
                }
            }

            stack.extend(mt.root().reversed().values());
        }

        fn pseudorandom_initial_state(
            &self,
            seed: [u8; 32],
            bench_case: Option<BenchmarkCase>,
        ) -> FunctionInitialState {
            let mut rng = StdRng::from_seed(seed);
            let num_leafs = match bench_case {
                Some(BenchmarkCase::CommonCase) => 1 << 9,
                Some(BenchmarkCase::WorstCase) => 1 << 10,
                None => 1 << rng.random_range(1..=10),
            };
            let list_pointer = rng.random();
            let leafs = (0..num_leafs).map(|_| rng.random()).collect_vec();

            self.init_state(leafs, list_pointer)
        }

        fn corner_case_initial_states(&self) -> Vec<FunctionInitialState> {
            [1, 2, 4, 8]
                .map(|len| self.init_state(xfe_vec![1; len], bfe!(0)))
                .to_vec()
        }
    }

    #[test]
    fn rust_shadow() {
        ShadowedFunction::new(MerkleRootFromXfes).test();
    }

    #[proptest(cases = 100)]
    fn cannot_handle_input_list_of_length_not_pow2(
        #[strategy(vec(arb(), 0..2048))]
        #[filter(!#leafs.len().is_power_of_two())]
        leafs: Vec<XFieldElement>,
        #[strategy(arb())] address: BFieldElement,
    ) {
        test_assertion_failure(
            &ShadowedFunction::new(MerkleRootFromXfes),
            MerkleRootFromXfes.init_state(leafs, address).into(),
            &[MerkleRootFromXfes::NUM_ELEMENTS_NOT_POWER_OF_2_ERROR_ID],
        );
    }
}

#[cfg(test)]
mod benches {
    use super::*;
    use crate::test_prelude::*;

    #[test]
    fn benchmark() {
        ShadowedFunction::new(MerkleRootFromXfes).bench();
    }
}
