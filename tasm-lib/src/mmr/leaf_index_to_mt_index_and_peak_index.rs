use std::collections::HashMap;

use triton_vm::prelude::*;

use crate::arithmetic::u64::add::Add;
use crate::arithmetic::u64::and::And;
use crate::arithmetic::u64::decr::Decr;
use crate::arithmetic::u64::log_2_floor::Log2Floor;
use crate::arithmetic::u64::lt_preserve_args::LtPreserveArgs;
use crate::arithmetic::u64::popcount::PopCount;
use crate::arithmetic::u64::pow2::Pow2;
use crate::prelude::*;
use crate::traits::basic_snippet::Reviewer;
use crate::traits::basic_snippet::SignOffFingerprint;

/// Compute both, merkle tree and peak index, for the given leaf index in an MMR
/// with the given number of leafs. [See also][rust].
///
/// ### Behavior
///
/// ```text
/// BEFORE: _ [num_leafs: u64] [leaf_index: u64]
/// AFTER:  _ [merkle_tree_index: 64] [peak_index: u32]
/// ```
///
/// ### Preconditions
///
/// - the `leaf_index` is smaller than `num_leafs`
/// - all input arguments are properly [`BFieldCodec`] encoded
///
/// ### Postconditions
///
/// - all output arguments are properly [`BFieldCodec`] encoded
///
/// [rust]: twenty_first::util_types::mmr::shared_basic::leaf_index_to_mt_index_and_peak_index
#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub struct MmrLeafIndexToMtIndexAndPeakIndex;

impl MmrLeafIndexToMtIndexAndPeakIndex {
    pub const LEAF_INDEX_GE_NUM_LEAFS_ERROR_ID: i128 = 350;
}

impl BasicSnippet for MmrLeafIndexToMtIndexAndPeakIndex {
    fn parameters(&self) -> Vec<(DataType, String)> {
        ["num_leafs", "leaf_index"]
            .map(|s| (DataType::U64, s.to_string()))
            .to_vec()
    }

    fn return_values(&self) -> Vec<(DataType, String)> {
        vec![
            (DataType::U64, "mt_index".to_string()),
            (DataType::U32, "peak_index".to_string()),
        ]
    }

    fn entrypoint(&self) -> String {
        "tasmlib_mmr_leaf_index_to_mt_index_and_peak_index".to_string()
    }

    fn code(&self, library: &mut Library) -> Vec<LabelledInstruction> {
        let entrypoint = self.entrypoint();
        let log_2_floor_u64 = library.import(Box::new(Log2Floor));
        let lt_u64 = library.import(Box::new(LtPreserveArgs));
        let add_u64 = library.import(Box::new(Add));
        let and_u64 = library.import(Box::new(And));
        let pow2_u64 = library.import(Box::new(Pow2));
        let decr_u64 = library.import(Box::new(Decr));
        let popcount_u64 = library.import(Box::new(PopCount));

        triton_asm!(
        // BEFORE: _ [num_leafs: u64] [leaf_index: u64]
        // AFTER:  _ [mt_index: u64] peak_index
        {entrypoint}:
            /* assert leaf_index < num_leafs */
            call {lt_u64}
            assert error_id {Self::LEAF_INDEX_GE_NUM_LEAFS_ERROR_ID}
            // _ [num_leafs: u64] [leaf_index: u64]

            dup 3
            dup 2
            xor
            // _ [num_leafs: u64] [leaf_index: u64] discrepancies_hi

            dup 3
            dup 2
            xor
            // _ [num_leafs: u64] [leaf_index: u64] [discrepancies: u64]

            call {log_2_floor_u64}
            // _ [num_leafs: u64] [leaf_index: u64] local_mt_height

            call {pow2_u64}
            // _ [num_leafs: u64] [leaf_index: u64] [local_mt_num_leafs: u64]

            dup 1 dup 1
            call {decr_u64}
            // _ [num_leafs: u64] [leaf_index: u64] [local_mt_num_leafs: u64] [remainder_bitmask: u64]

            dup 1 dup 1
            pick 7 pick 7
            call {and_u64}
            // _ [num_leafs: u64] [local_mt_num_leafs: u64] [remainder_bitmask: u64] [local_leaf_index: u64]

            pick 5 pick 5
            // _ [num_leafs: u64] [remainder_bitmask: u64] [local_leaf_index: u64] [local_mt_num_leafs: u64]

            call {add_u64}
            // _ [num_leafs: u64] [remainder_bitmask: u64] [merkle_tree_index: u64]

            place 5 place 5
            // _ [merkle_tree_index: u64] [num_leafs: u64] [remainder_bitmask: u64]

            dup 3 dup 3
            call {popcount_u64}
            // _ [merkle_tree_index: u64] [num_leafs: u64] [remainder_bitmask: u64] all_the_ones

            place 4
            // _ [merkle_tree_index: u64] all_the_ones [num_leafs: u64] [remainder_bitmask: u64]

            call {and_u64}
            call {popcount_u64}
            // _ [merkle_tree_index: u64] all_the_ones ones_to_subtract

            push -1
            mul
            add
            addi -1
            // _ [merkle_tree_index: u64] peak_index

            return
        )
    }

    fn sign_offs(&self) -> HashMap<Reviewer, SignOffFingerprint> {
        let mut sign_offs = HashMap::new();
        sign_offs.insert(Reviewer("ferdinand"), 0xfaeccefe8b0b120.into());
        sign_offs
    }
}

#[cfg(test)]
pub(crate) mod tests {
    use twenty_first::util_types::mmr::shared_basic::leaf_index_to_mt_index_and_peak_index;

    use super::*;
    use crate::test_prelude::*;

    impl MmrLeafIndexToMtIndexAndPeakIndex {
        pub fn assert_expected_behavior(&self, num_leafs: u64, leaf_index: u64) {
            let initial_stack = self.set_up_test_stack((num_leafs, leaf_index));

            let mut expected_stack = initial_stack.clone();
            self.rust_shadow(&mut expected_stack);

            test_rust_equivalence_given_complete_state(
                &ShadowedClosure::new(Self),
                &initial_stack,
                &[],
                &NonDeterminism::default(),
                &None,
                Some(&expected_stack),
            );
        }
    }

    impl Closure for MmrLeafIndexToMtIndexAndPeakIndex {
        type Args = (u64, u64);

        fn rust_shadow(&self, stack: &mut Vec<BFieldElement>) {
            let (num_leafs, leaf_index) = pop_encodable::<Self::Args>(stack);

            let (mt_index, peak_index) =
                leaf_index_to_mt_index_and_peak_index(leaf_index, num_leafs);

            push_encodable(stack, &mt_index);
            push_encodable(stack, &peak_index);
        }

        fn pseudorandom_args(
            &self,
            seed: [u8; 32],
            bench_case: Option<BenchmarkCase>,
        ) -> Self::Args {
            let Some(bench_case) = bench_case else {
                let mut rng = StdRng::from_seed(seed);
                let num_leafs = rng.random_range(1..1 << 63);
                let leaf_index = rng.random_range(0..num_leafs);
                return (num_leafs, leaf_index);
            };

            match bench_case {
                BenchmarkCase::WorstCase => ((1 << 63) - 1, (1 << 63) - 63),
                BenchmarkCase::CommonCase => ((1 << 32) - 1, (3 << 30) + 100_000),
            }
        }

        fn corner_case_args(&self) -> Vec<Self::Args> {
            // todo: can the collection of cases below be less
            //   “stab-in-the-dark” and more actual edge cases?

            let mut states = vec![];
            for num_leafs in (1..=5).chain([14]).chain(32..=37) {
                for leaf_index in 0..num_leafs {
                    states.push((num_leafs, leaf_index));
                }
            }

            let more_states = (10..20)
                .map(|pow| 1 << pow)
                .flat_map(|n| [(14, n), (n + 9, n + 11), (n + 10, n + 11)])
                .map(|(leaf_index, num_leafs)| (num_leafs, leaf_index));
            states.extend(more_states);

            states.push((1 << 31, 5_550_001));
            states.push(((1 << 31) + (1 << 20), 5_550_001));

            for num_leafs in [(1 << 31) + (1 << 20) - 1, (1 << 63) + (1 << 62) - 1] {
                states.push((num_leafs, num_leafs - 1));
            }

            states
        }
    }

    #[test]
    fn rust_shadow() {
        ShadowedClosure::new(MmrLeafIndexToMtIndexAndPeakIndex).test();
    }

    #[proptest]
    fn property_test(
        #[strategy(1_u64..)] num_leafs: u64,
        #[strategy(0_u64..#num_leafs)] leaf_index: u64,
    ) {
        MmrLeafIndexToMtIndexAndPeakIndex.assert_expected_behavior(num_leafs, leaf_index);
    }

    #[proptest]
    fn negative_property_test(num_leafs: u64, #[strategy(#num_leafs..)] leaf_index: u64) {
        let initial_stack =
            MmrLeafIndexToMtIndexAndPeakIndex.set_up_test_stack((num_leafs, leaf_index));

        test_assertion_failure(
            &ShadowedClosure::new(MmrLeafIndexToMtIndexAndPeakIndex),
            InitVmState::with_stack(initial_stack),
            &[MmrLeafIndexToMtIndexAndPeakIndex::LEAF_INDEX_GE_NUM_LEAFS_ERROR_ID],
        );
    }
}

#[cfg(test)]
mod benches {
    use super::*;
    use crate::test_prelude::*;

    #[test]
    fn benchmark() {
        ShadowedClosure::new(MmrLeafIndexToMtIndexAndPeakIndex).bench();
    }
}
