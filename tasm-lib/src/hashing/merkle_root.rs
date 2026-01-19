use std::collections::HashMap;

use triton_vm::prelude::*;

use crate::prelude::*;
use crate::traits::basic_snippet::Reviewer;
use crate::traits::basic_snippet::SignOffFingerprint;

/// Compute the Merkle root of a slice of `Digest`s. Corresponds to
/// `MerkleTree::`[`sequential_new`][new]`(leafs).`[`root`][root]`()`.
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
/// - `*leafs` points to a list of Digests
/// - the length of the pointed-to list is greater than 0
/// - the length of the pointed-to list is a power of 2
/// - the length of the pointed-to list is a u32
///
/// ### Postconditions
///
/// None.
///
/// [new]: twenty_first::prelude::MerkleTree::sequential_new
/// [root]: twenty_first::prelude::MerkleTree::root
#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub struct MerkleRoot;

impl MerkleRoot {
    pub const NUM_LEAFS_NOT_POWER_OF_2_ERROR_ID: i128 = 431;
}

impl BasicSnippet for MerkleRoot {
    fn parameters(&self) -> Vec<(DataType, String)> {
        vec![(
            DataType::List(Box::new(DataType::Digest)),
            "*leafs".to_string(),
        )]
    }

    fn return_values(&self) -> Vec<(DataType, String)> {
        vec![(DataType::Digest, "root".to_string())]
    }

    fn entrypoint(&self) -> String {
        "tasmlib_hashing_merkle_root".to_string()
    }

    fn code(&self, library: &mut Library) -> Vec<LabelledInstruction> {
        let dyn_malloc = library.import(Box::new(DynMalloc));

        let entrypoint = self.entrypoint();
        let calculate_parent_digests = format!("{entrypoint}_calculate_parent_digests");
        let next_layer_loop = format!("{entrypoint}_next_layer_loop");

        triton_asm!(
            {entrypoint}:
                // _ *leafs

                read_mem 1
                addi 1
                // _ leafs_len *leafs

                /* assert the number of leafs is some power of 2 */
                dup 1
                pop_count
                push 1
                eq
                assert error_id {Self::NUM_LEAFS_NOT_POWER_OF_2_ERROR_ID}

                call {dyn_malloc}
                // _ leafs_len *leafs *parent_level

                /* adjust `*parent_level` to point to last element, first word */
                dup 2
                addi -1
                push {Digest::LEN}
                mul
                add
                // _ leafs_len *leafs (*parent_level + (leafs_len - 1) * Digest::LEN)
                // _ leafs_len *leafs *parent_level'

                /* adjust `*leafs` to point to last element, last word */
                pick 1
                dup 2
                push {Digest::LEN}
                mul
                add
                // _ leafs_len *parent_level' (*leafs + leafs_len * Digest::LEN)
                // _ leafs_len *parent_level' *leafs'

                call {next_layer_loop}
                // _ 1 *address (*root + Digest::LEN)

                place 2
                pop 2
                // _ (*root + Digest::LEN - 1)

                read_mem {Digest::LEN}
                // _ [root: Digest] (*root - 1)

                pop 1
                // _ [root: Digest]

               return

            // INVARIANT: _ current_len *next_level[last]_first_word *current_level[last]_last_word
            {next_layer_loop}:
                // _ current_len *next_level *current_level

                /* end loop if `current_len == 1` */
                dup 2
                push 1
                eq
                skiz
                    return
                // _ current_len *next_level *current_level

                /* update `current_len` */
                pick 2
                push {bfe!(2).inverse()}
                        hint one_half = stack[0]
                mul
                place 2
                // _ (current_len/2) *next_level *current_level

                /* set up termination condition for parent calculation loop:
                 * `*next_level - current_len / 2 * Digest::LEN`
                 */
                dup 1
                dup 3
                push {-(Digest::LEN as isize)}
                mul
                add
                // _ (current_len/2) *next_level *current_level *next_level_stop
                // _ (current_len/2) *next_level *current_elem  *next_elem_stop

                dup 2
                push 0
                push 0
                push 0
                push 0
                pick 6
                // _ (current_len/2) *next_level *next_elem_stop *next_level 0 0 0 0 *current_elem

                call {calculate_parent_digests}
                pop 5
                pop 1
                // _ (current_len/2) *next_level *next_elem_stop

                /* Update `*current_level` based on `*next_level` */
                pick 1
                // _ (current_len/2) *next_elem_stop *next_level

                addi {Digest::LEN - 1}
                // _ (current_len/2) *next_level' *current_level'

                recurse

            // Populate the `*next` digest list
            // INVARIANT: _ *next_elem_stop *next_elem 0 0 0 0 *curr_elem
            {calculate_parent_digests}:
                read_mem {Digest::LEN}
                read_mem {Digest::LEN}
                // _ *next_elem_stop *next_elem 0 0 0 0 [right] [left] (*curr_elem[n] - 10)
                // _ *next_elem_stop *next_elem 0 0 0 0 [right] [left] *curr_elem[n - 2]
                // _ *next_elem_stop *next_elem 0 0 0 0 [right] [left] *curr_elem'

                place 10
                // _ *next_elem_stop *next_elem 0 0 0 0 *curr_elem' [right] [left]

                hash
                // _ *next_elem_stop *next_elem 0 0 0 0 *curr_elem' [parent_digest]

                pick 10
                // _ *next_elem_stop 0 0 0 0 *curr_elem' [parent_digest] *next_elem

                write_mem {Digest::LEN}
                // _ *next_elem_stop 0 0 0 0 *curr_elem' (*next_elem + 5)

                addi -10
                // _ *next_elem_stop 0 0 0 0 *curr_elem' (*next_elem - 5)
                // _ *next_elem_stop 0 0 0 0 *curr_elem' *next_elem[n-1]
                // _ *next_elem_stop 0 0 0 0 *curr_elem' *next_elem'

                place 5
                // _ *next_elem_stop *next_elem' 0 0 0 0 *curr_elem'

                recurse_or_return
        )
    }

    fn sign_offs(&self) -> HashMap<Reviewer, SignOffFingerprint> {
        let mut sign_offs = HashMap::new();
        sign_offs.insert(Reviewer("ferdinand"), 0x19b073a58272366c.into());
        sign_offs
    }
}

#[cfg(test)]
mod tests {
    use proptest::collection::vec;
    use twenty_first::util_types::merkle_tree::MerkleTree;

    use super::*;
    use crate::rust_shadowing_helper_functions::dyn_malloc::dynamic_allocator;
    use crate::test_prelude::*;

    impl MerkleRoot {
        fn init_state(
            &self,
            leafs: Vec<Digest>,
            digests_pointer: BFieldElement,
        ) -> FunctionInitialState {
            let mut memory = HashMap::new();
            encode_to_memory(&mut memory, digests_pointer, &leafs);
            let mut stack = self.init_stack_for_isolated_run();
            stack.push(digests_pointer);

            FunctionInitialState { stack, memory }
        }
    }

    impl Function for MerkleRoot {
        fn rust_shadow(
            &self,
            stack: &mut Vec<BFieldElement>,
            memory: &mut HashMap<BFieldElement, BFieldElement>,
        ) {
            let leafs_pointer = stack.pop().unwrap();
            let leafs = *Vec::decode_from_memory(memory, leafs_pointer).unwrap();
            let mt = MerkleTree::par_new(&leafs).unwrap();

            // mimic snippet: write internal nodes to memory, skipping (dummy) node 0
            let tree_pointer = dynamic_allocator(memory);
            let num_internal_nodes = leafs.len();

            for node_index in 1..num_internal_nodes {
                let node = mt.node(node_index).unwrap();
                let node_address = tree_pointer + bfe!(node_index * Digest::LEN);
                encode_to_memory(memory, node_address, &node);
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
                Some(BenchmarkCase::CommonCase) => 512,
                Some(BenchmarkCase::WorstCase) => 1024,
                None => 1 << rng.random_range(0..=8),
            };
            let leafs = (0..num_leafs).map(|_| rng.random()).collect_vec();
            let digests_pointer = rng.random();

            self.init_state(leafs, digests_pointer)
        }

        fn corner_case_initial_states(&self) -> Vec<FunctionInitialState> {
            let height_0 = self.init_state(vec![Digest::default()], bfe!(0));
            let height_1 = self.init_state(vec![Digest::default(), Digest::default()], bfe!(0));

            vec![height_0, height_1]
        }
    }

    #[test]
    fn rust_shadow() {
        ShadowedFunction::new(MerkleRoot).test();
    }

    #[test]
    fn computing_root_of_tree_of_height_0_crashes_vm() {
        test_assertion_failure(
            &ShadowedFunction::new(MerkleRoot),
            MerkleRoot.init_state(vec![], bfe!(0)).into(),
            &[MerkleRoot::NUM_LEAFS_NOT_POWER_OF_2_ERROR_ID],
        );
    }

    #[proptest(cases = 100)]
    fn computing_root_of_tree_of_height_not_power_of_2_crashes_vm(
        #[strategy(vec(arb(), 0..2048))]
        #[filter(!#leafs.len().is_power_of_two())]
        leafs: Vec<Digest>,
        #[strategy(arb())] address: BFieldElement,
    ) {
        test_assertion_failure(
            &ShadowedFunction::new(MerkleRoot),
            MerkleRoot.init_state(leafs, address).into(),
            &[MerkleRoot::NUM_LEAFS_NOT_POWER_OF_2_ERROR_ID],
        );
    }
}

#[cfg(test)]
mod benches {
    use super::*;
    use crate::test_prelude::*;

    #[test]
    fn benchmark() {
        ShadowedFunction::new(MerkleRoot).bench();
    }
}
