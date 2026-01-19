use std::collections::HashMap;

use triton_vm::prelude::*;

use crate::hashing::merkle_step_u64_index::MerkleStepU64Index;
use crate::prelude::*;
use crate::traits::basic_snippet::Reviewer;
use crate::traits::basic_snippet::SignOffFingerprint;

/// Similar to instruction `merkle_step_mem`, but for index of type `u64`
/// instead of the native `u32`. The most notable difference is that the stack
/// element with index 6 (“`st6`”) will generally be modified.
///
/// ### Behavior
///
/// ```text
/// BEFORE: _ ram_ptr  [merkle_tree_node_index: u64] [node: Digest]
/// AFTER:  _ ram_ptr' [merkle_tree_parent_node_index: u64] [parent_node: Digest]
/// ```
///
/// ### Preconditions
///
/// - all input arguments are properly [`BFieldCodec`] encoded
///
/// ### Postconditions
///
/// - all output is properly [`BFieldCodec`] encoded
/// - the `ram_ptr` is incremented by [`Digest::LEN`]
#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub struct MerkleStepMemU64Index;

impl BasicSnippet for MerkleStepMemU64Index {
    fn parameters(&self) -> Vec<(DataType, String)> {
        vec![
            (DataType::VoidPointer, "ram pointer".to_owned()),
            (DataType::U64, "merkle tree node index".to_owned()),
            (DataType::Digest, "node".to_owned()),
        ]
    }

    fn return_values(&self) -> Vec<(DataType, String)> {
        vec![
            (DataType::VoidPointer, "ram pointer".to_owned()),
            (DataType::U64, "merkle tree parent node index".to_owned()),
            (DataType::Digest, "parent node".to_owned()),
        ]
    }

    fn entrypoint(&self) -> String {
        "tasmlib_hashing_merkle_step_mem_u64_index".to_owned()
    }

    fn code(&self, _: &mut Library) -> Vec<LabelledInstruction> {
        triton_asm!(
            {self.entrypoint()}:
                merkle_step_mem
                // _ ptr' mt_idx_hi (mt_idx_lo / 2) [parent_node: Digest]

                {&MerkleStepU64Index::make_u64_index_consistent()}
                // _ ptr' (mt_index / 2)_hi (mt_index / 2)_lo [parent_node: Digest]

                return
        )
    }

    fn sign_offs(&self) -> HashMap<Reviewer, SignOffFingerprint> {
        let mut sign_offs = HashMap::new();
        sign_offs.insert(Reviewer("ferdinand"), 0xb5f098c6c0c971c3.into());
        sign_offs
    }
}

#[cfg(test)]
mod tests {
    use std::collections::VecDeque;

    use super::*;
    use crate::test_prelude::*;

    impl MerkleStepMemU64Index {
        fn set_up_initial_state(
            &self,
            ram_ptr: BFieldElement,
            leaf_index: u64,
        ) -> ReadOnlyAlgorithmInitialState {
            let mut stack = self.init_stack_for_isolated_run();
            push_encodable(&mut stack, &ram_ptr);
            push_encodable(&mut stack, &leaf_index);
            push_encodable(&mut stack, &rand::random::<Digest>());

            let mut nondeterminism = NonDeterminism::default();
            encode_to_memory(&mut nondeterminism.ram, ram_ptr, &rand::random::<Digest>());

            ReadOnlyAlgorithmInitialState {
                stack,
                nondeterminism,
            }
        }
    }

    impl ReadOnlyAlgorithm for MerkleStepMemU64Index {
        fn rust_shadow(
            &self,
            stack: &mut Vec<BFieldElement>,
            memory: &HashMap<BFieldElement, BFieldElement>,
            _: VecDeque<BFieldElement>,
            _: VecDeque<Digest>,
        ) {
            let stack_digest = pop_encodable::<Digest>(stack);
            let leaf_index = pop_encodable::<u64>(stack);
            let ram_ptr = pop_encodable::<BFieldElement>(stack);

            let stack_digest_is_left_sibling = leaf_index.is_multiple_of(2);
            let sibling_digest = *Digest::decode_from_memory(memory, ram_ptr).unwrap();
            let (left_digest, right_digest) = if stack_digest_is_left_sibling {
                (stack_digest, sibling_digest)
            } else {
                (sibling_digest, stack_digest)
            };

            let parent_digest = Tip5::hash_pair(left_digest, right_digest);
            let parent_index = leaf_index / 2;

            push_encodable(stack, &(ram_ptr + bfe!(Digest::LEN)));
            push_encodable(stack, &parent_index);
            push_encodable(stack, &parent_digest);
        }

        fn pseudorandom_initial_state(
            &self,
            seed: [u8; 32],
            bench_case: Option<BenchmarkCase>,
        ) -> ReadOnlyAlgorithmInitialState {
            let mut rng = StdRng::from_seed(seed);
            let leaf_index = match bench_case {
                Some(BenchmarkCase::CommonCase) => 1 << 33,
                Some(BenchmarkCase::WorstCase) => 1 << 63,
                None => rng.random(),
            };

            self.set_up_initial_state(rng.random(), leaf_index)
        }
    }

    #[test]
    fn unit() {
        ShadowedReadOnlyAlgorithm::new(MerkleStepMemU64Index).test();
    }
}

#[cfg(test)]
mod benches {
    use super::*;
    use crate::test_prelude::*;

    #[test]
    fn benchmark() {
        ShadowedReadOnlyAlgorithm::new(MerkleStepMemU64Index).bench();
    }
}
