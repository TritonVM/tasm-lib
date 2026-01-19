use std::collections::HashMap;

use triton_vm::prelude::*;

use crate::prelude::*;
use crate::traits::basic_snippet::Reviewer;
use crate::traits::basic_snippet::SignOffFingerprint;

/// Like instruction `merkle_step`, but for index of type `u64` instead of the
/// native `u32`.
///
/// ### Behavior
///
/// ```text
/// BEFORE: _ [merkle_tree_node_index: u64] [node: Digest]
/// AFTER:  _ [merkle_tree_parent_node_index: u64] [parent_node: Digest]
/// ```
///
/// ### Preconditions
///
/// - all input arguments are properly [`BFieldCodec`] encoded
///
/// ### Postconditions
///
/// - all output is properly [`BFieldCodec`] encoded
#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub struct MerkleStepU64Index;

impl MerkleStepU64Index {
    /// Make a Merkle tree index of type `u64` consistent after having performed a
    /// Triton-VM-native `merkle_step` instruction.
    ///
    /// ```text
    /// BEFORE: _ mt_index_hi       (mt_index_lo / 2) [node: Digest]
    /// AFTER:  _ (mt_index / 2)_hi (mt_index / 2)_lo [node: Digest]
    /// ```
    pub(super) fn make_u64_index_consistent() -> Vec<LabelledInstruction> {
        triton_asm! {
            push 2      // _ mt_index_hi (mt_index_lo / 2) [digest; 5] 2
            pick 7      // _ (mt_index_lo / 2) [digest; 5] 2 mt_index_hi
            div_mod     // _ (mt_index_lo / 2) [digest; 5] (mt_index_hi / 2) (mt_index_hi % 2)
                        // _ (mt_index_lo / 2) [digest; 5] (mt_index / 2)_hi (mt_index_hi % 2)
            push {1u32 << 31}
            hint two_pow_31: u32 = stack[0]
            mul         // _ (mt_index_lo / 2) [digest; 5] (mt_index / 2)_hi carry
            hint carry: u32 = stack[0]
            pick 7      // _ [digest; 5] (mt_index / 2)_hi carry (mt_index_lo / 2)
            add         // _ [digest; 5] (mt_index / 2)_hi (mt_index / 2)_lo
            place 6     // _ (mt_index / 2)_lo [digest; 5] (mt_index / 2)_hi
            place 6     // _ (mt_index / 2)_hi (mt_index / 2)_lo [digest; 5]
        }
    }
}

impl BasicSnippet for MerkleStepU64Index {
    fn parameters(&self) -> Vec<(DataType, String)> {
        vec![
            (DataType::U64, "merkle tree node index".to_owned()),
            (DataType::Digest, "node".to_owned()),
        ]
    }

    fn return_values(&self) -> Vec<(DataType, String)> {
        vec![
            (DataType::U64, "merkle tree parent node index".to_owned()),
            (DataType::Digest, "parent node".to_owned()),
        ]
    }

    fn entrypoint(&self) -> String {
        "tasmlib_hashing_merkle_step_u64_index".to_owned()
    }

    fn code(&self, _: &mut Library) -> Vec<LabelledInstruction> {
        triton_asm!(
            {self.entrypoint()}:
                merkle_step
                // _ mt_index_hi (mt_index_lo / 2) [parent_node: Digest]

                {&Self::make_u64_index_consistent()}
                // _ (mt_index / 2)_hi (mt_index / 2)_lo [parent_node: Digest]

                return
        )
    }

    fn sign_offs(&self) -> HashMap<Reviewer, SignOffFingerprint> {
        let mut sign_offs = HashMap::new();
        sign_offs.insert(Reviewer("ferdinand"), 0x9895be8abf9a0ace.into());
        sign_offs
    }
}

#[cfg(test)]
mod tests {
    use std::collections::VecDeque;

    use super::*;
    use crate::test_prelude::*;

    impl MerkleStepU64Index {
        fn set_up_initial_state(&self, leaf_index: u64) -> ReadOnlyAlgorithmInitialState {
            let mut stack = self.init_stack_for_isolated_run();
            push_encodable(&mut stack, &leaf_index);
            push_encodable(&mut stack, &rand::random::<Digest>());

            ReadOnlyAlgorithmInitialState {
                stack,
                nondeterminism: NonDeterminism::default().with_digests(vec![rand::random()]),
            }
        }
    }

    impl ReadOnlyAlgorithm for MerkleStepU64Index {
        fn rust_shadow(
            &self,
            stack: &mut Vec<BFieldElement>,
            _: &HashMap<BFieldElement, BFieldElement>,
            _: VecDeque<BFieldElement>,
            nd_digests: VecDeque<Digest>,
        ) {
            let stack_digest = pop_encodable::<Digest>(stack);
            let leaf_index = pop_encodable::<u64>(stack);

            let stack_digest_is_left_sibling = leaf_index.is_multiple_of(2);
            let (left_digest, right_digest) = if stack_digest_is_left_sibling {
                (stack_digest, nd_digests[0])
            } else {
                (nd_digests[0], stack_digest)
            };

            let parent_digest = Tip5::hash_pair(left_digest, right_digest);
            let parent_index = leaf_index / 2;

            push_encodable(stack, &parent_index);
            push_encodable(stack, &parent_digest);
        }

        fn pseudorandom_initial_state(
            &self,
            seed: [u8; 32],
            bench_case: Option<BenchmarkCase>,
        ) -> ReadOnlyAlgorithmInitialState {
            let leaf_index = match bench_case {
                Some(BenchmarkCase::CommonCase) => 1 << 33,
                Some(BenchmarkCase::WorstCase) => 1 << 63,
                None => StdRng::from_seed(seed).random(),
            };

            self.set_up_initial_state(leaf_index)
        }
    }

    #[test]
    fn rust_shadow() {
        ShadowedReadOnlyAlgorithm::new(MerkleStepU64Index).test();
    }

    #[test]
    fn unit_test() {
        fn assert_expected_behavior(mt_index: u64, expected_parent_index: u64) {
            let initial_state = MerkleStepU64Index.set_up_initial_state(mt_index);
            let final_state = crate::test_helpers::tasm_final_state(
                &ShadowedReadOnlyAlgorithm::new(MerkleStepU64Index),
                &initial_state.stack,
                &[],
                initial_state.nondeterminism,
                &None,
            );

            let mut final_stack = final_state.op_stack.stack;
            let _ = pop_encodable::<Digest>(&mut final_stack);
            let calculated_parent_index = pop_encodable::<u64>(&mut final_stack);

            assert_eq!(expected_parent_index, calculated_parent_index);
            assert_eq!(mt_index / 2, calculated_parent_index);
        }

        assert_expected_behavior(0, 0);
        assert_expected_behavior(1, 0);
        assert_expected_behavior(2, 1);
        assert_expected_behavior(3, 1);
        assert_expected_behavior(4, 2);
        assert_expected_behavior(1 << 32, 1 << 31);
        assert_expected_behavior(1 << 33, 1 << 32);
        assert_expected_behavior(1 << 34, 1 << 33);
        assert_expected_behavior(0b101 << 32, 0b101 << 31);
        assert_expected_behavior(u64::MAX, (1 << 63) - 1);
    }
}

#[cfg(test)]
mod benches {
    use super::*;
    use crate::test_prelude::*;

    #[test]
    fn benchmark() {
        ShadowedReadOnlyAlgorithm::new(MerkleStepU64Index).bench();
    }
}
