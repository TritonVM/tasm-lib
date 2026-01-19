use std::collections::HashMap;

use triton_vm::prelude::*;
use twenty_first::util_types::mmr::mmr_accumulator::MmrAccumulator;
use twenty_first::util_types::mmr::mmr_successor_proof::MmrSuccessorProof;

use crate::arithmetic::u64 as u64_lib;
use crate::hashing::merkle_step_mem_u64_index::MerkleStepMemU64Index;
use crate::hashing::merkle_step_u64_index::MerkleStepU64Index;
use crate::mmr::leaf_index_to_mt_index_and_peak_index::MmrLeafIndexToMtIndexAndPeakIndex;
use crate::prelude::*;
use crate::traits::basic_snippet::Reviewer;
use crate::traits::basic_snippet::SignOffFingerprint;

/// Verify that one MMR is a successor to another.
///
/// Verify the successorship relation between two MMRs. A [`MmrSuccessorProof`]
/// is necessary to demonstrate this relation, but it is not a *stack* argument
/// because this algorithm obtains the relevant info (authentication paths) from
/// nondeterminism. Accordingly, nondeterminism must be [initialized] correctly with
/// the `MmrSuccessorProof`.
///
/// The snippet crashes if the relation does not hold, or if the proof is invalid.
///
/// ### Behavior
///
/// ```text
/// BEFORE: _ *old_mmr *new_mmr
/// AFTER:  _
/// ```
///
/// ### Preconditions
///
/// None.
///
/// ### Postconditions
///
/// - the `new_mmr` is a successor of the `old_mmr`
///
/// [initialized]: VerifyMmrSuccessor::update_nondeterminism
#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub struct VerifyMmrSuccessor;

impl VerifyMmrSuccessor {
    pub(crate) const OLD_HAS_MORE_LEAFS_THAN_NEW_ERROR_ID: i128 = 150;
    pub(crate) const INCONSISTENT_OLD_MMR_ERROR_ID: i128 = 151;
    pub(crate) const INCONSISTENT_NEW_MMR_ERROR_ID: i128 = 152;
    pub(crate) const DIFFERING_SHARED_PEAK_ERROR_ID: i128 = 153;
    pub(crate) const DIFFERING_UNSHARED_PEAK_ERROR_ID: i128 = 154;
}

impl BasicSnippet for VerifyMmrSuccessor {
    fn parameters(&self) -> Vec<(DataType, String)> {
        ["*old_mmr", "*new_mmr"]
            .map(|ptr_name| (DataType::VoidPointer, ptr_name.to_string()))
            .to_vec()
    }

    fn return_values(&self) -> Vec<(DataType, String)> {
        vec![]
    }

    fn entrypoint(&self) -> String {
        "tasm_lib_mmr_verify_mmr_successor".to_string()
    }

    fn code(&self, library: &mut Library) -> Vec<LabelledInstruction> {
        let sub_u64 = library.import(Box::new(u64_lib::sub::Sub));
        let lt_u64 = library.import(Box::new(u64_lib::lt::Lt));
        let popcount_u64 = library.import(Box::new(u64_lib::popcount::PopCount));
        let trailing_zeros_u64 = library.import(Box::new(u64_lib::trailing_zeros::TrailingZeros));
        let shl_u64 = library.import(Box::new(u64_lib::shift_left::ShiftLeft));
        let shr_u64 = library.import(Box::new(u64_lib::shift_right::ShiftRight));

        let leaf_index_to_mti_and_pki = library.import(Box::new(MmrLeafIndexToMtIndexAndPeakIndex));
        let merkle_step_u64 = library.import(Box::new(MerkleStepU64Index));
        let merkle_step_mem_u64 = library.import(Box::new(MerkleStepMemU64Index));

        let entrypoint = self.entrypoint();
        let clean_up_because_old_mmr_has_0_leafs =
            format!("{entrypoint}_clean_up_because_old_mmr_has_0_leafs");
        let assert_mmr_equality = format!("{entrypoint}_assert_mmr_equality");
        let assert_unchanged_peaks_equality =
            format!("{entrypoint}_assert_unchanged_peaks_equality");
        let clean_up_because_new_leafs_dont_affect_old_peaks =
            format!("{entrypoint}_clean_up_because_new_leafs_dont_affect_old_peaks");
        let traverse_new_tree = format!("{entrypoint}_traverse_new_tree");
        let traverse_new_tree_right_sibling = format!("{traverse_new_tree}_right_sibling");

        triton_asm! {
            // BEFORE: _ *old *new
            // AFTER:  _
            {entrypoint}:
                /* consistent old MMR? */
                pick 1
                {&MmrAccumulator::destructure()}
                    hint old_num_leafs: Pointer = stack[0]
                    hint old_peaks: Pointer = stack[1]
                // _ *new *old_peaks *old_num_leafs

                addi 1
                read_mem 2 hint old_num_leafs: u64 = stack[1..3]
                pop 1
                // _ *new *old_peaks [old_num_leafs: u64]

                pick 2
                read_mem 1 hint old_peaks_len = stack[1]
                addi 1
                place 1
                // _ *new [old_num_leafs: u64] *old_peaks old_peaks_len

                dup 3 dup 3
                call {popcount_u64}
                // _ *new [old_num_leafs: u64] *old_peaks old_peaks_len (popcount of old_num_leafs)

                eq assert error_id {Self::INCONSISTENT_OLD_MMR_ERROR_ID}
                // _ *new [old_num_leafs: u64] *old_peaks

                /* consistent new MMR? */
                pick 3
                {&MmrAccumulator::destructure()}
                    hint new_num_leafs: Pointer = stack[0]
                    hint new_peaks: Pointer = stack[1]
                // _ [old_num_leafs: u64] *old_peaks *new_peaks *new_num_leafs

                addi 1
                read_mem 2 hint new_num_leafs: u64 = stack[1..3]
                pop 1
                // _ [old_num_leafs: u64] *old_peaks *new_peaks [new_num_leafs: u64]

                pick 2
                read_mem 1 hint new_peaks_len = stack[1]
                addi 1
                place 1
                // _ [old_num_leafs: u64] *old_peaks [new_num_leafs: u64] *new_peaks new_peaks_len

                dup 3 dup 3
                call {popcount_u64}
                // _ [old_num_leafs: u64] *old_peaks [new_num_leafs: u64] *new_peaks new_peaks_len (popcount of new_num_leafs)

                eq assert error_id {Self::INCONSISTENT_NEW_MMR_ERROR_ID}
                // _ [old_num_leafs: u64] *old_peaks [new_num_leafs: u64] *new_peaks

                /* edge case: old MMR has 0 leafs – nothing to verify */
                push 0
                dup 6 dup 6
                // _ [old_num_leafs: u64] *old_peaks [new_num_leafs: u64] *new_peaks 0 [old_num_leafs: u64]

                push 0 push 0 hint zero: u64 = stack[0..2]
                {&DataType::U64.compare()}
                // _ [old_num_leafs: u64] *old_peaks [new_num_leafs: u64] *new_peaks 0 (old_num_leafs == 0)

                skiz call {clean_up_because_old_mmr_has_0_leafs}
                skiz return
                // _ [old_num_leafs: u64] *old_peaks [new_num_leafs: u64] *new_peaks

                /* edge case: old and new MMRs have the same number of leafs
                 * Check equality of peaks. Treated separately to simplify nominal case.
                 */
                push 0
                dup 6 dup 6
                dup 5 dup 5
                {&DataType::U64.compare()}
                // _ [old_num_leafs: u64] *old_peaks [new_num_leafs: u64] *new_peaks 0 (old_num_leafs == new_num_leafs)

                skiz call {assert_mmr_equality}
                skiz return
                // _ [old_num_leafs: u64] *old_peaks [new_num_leafs: u64] *new_peaks

                /* crash if num_old_leafs > num_new_leafs */
                dup 5 dup 5
                dup 4 dup 4
                call {lt_u64}
                // _ [old_num_leafs: u64] *old_peaks [new_num_leafs: u64] *new_peaks (new_num_leafs < old_num_leafs)

                push 0 eq
                // _ [old_num_leafs: u64] *old_peaks [new_num_leafs: u64] *new_peaks (new_num_leafs >= old_num_leafs)

                assert error_id {Self::OLD_HAS_MORE_LEAFS_THAN_NEW_ERROR_ID}
                // _ [old_num_leafs: u64] *old_peaks [new_num_leafs: u64] *new_peaks

                /* nominal case */
                dup 2 dup 2
                dup 7 dup 7
                //  _ [old_num_leafs: u64] *old_peaks [new_num_leafs: u64] *new_peaks [new_num_leafs: u64] [old_num_leafs: u64]

                call {leaf_index_to_mti_and_pki}    hint num_unchanged_peaks = stack[0]
                                                    hint merkle_tree_idx: u64 = stack[1..3]
                // _ [old_num_leafs: u64] *old_peaks [new_num_leafs: u64] *new_peaks [mt_index: u64] num_unchanged_peaks

                pick 2 pick 2
                place 8 place 8
                // _ [mt_index: u64] [old_num_leafs: u64] *old_peaks [new_num_leafs: u64] *new_peaks num_unchanged_peaks

                pick 4
                read_mem 1
                addi 2
                // _ [mt_index: u64] [old_num_leafs: u64] [new_num_leafs: u64] *new_peaks num_unchanged_peaks old_num_peaks *old_peaks[0]

                pick 1
                addi -1
                push {Digest::LEN}
                mul
                // _ [mt_index: u64] [old_num_leafs: u64] [new_num_leafs: u64] *new_peaks num_unchanged_peaks *old_peaks[0] (old_num_peaks-1)*DIGEST_LENGTH

                dup 1
                add hint last_old_peak: Pointer = stack[0]
                place 1
                // _ [mt_index: u64] [old_num_leafs: u64] [new_num_leafs: u64] *new_peaks num_unchanged_peaks *old_peaks[-1] *old_peaks[0]

                pick 3
                addi 1
                pick 3
                // _ [mt_index: u64] [old_num_leafs: u64] [new_num_leafs: u64] *old_peaks[-1] *old_peaks[0] *new_peaks[0] num_unchanged_peaks

                call {assert_unchanged_peaks_equality}
                // _ [mt_index: u64] [old_num_leafs: u64] [new_num_leafs: u64] *old_peaks[-1] *old_peaks[i] *new_peaks[i] 0

                pick 2
                pop 2
                // _ [mt_index: u64] [old_num_leafs: u64] [new_num_leafs: u64] *old_peaks[-1] *new_peaks[i]

                place 5
                place 5
                // _ [mt_index: u64] *old_peaks[-1] *new_peaks[i] [old_num_leafs: u64] [new_num_leafs: u64]

                dup 3 dup 3
                call {trailing_zeros_u64} hint height_of_lowest_old_peak = stack[0]
                // _ [mt_index: u64] *old_peaks[-1] *new_peaks[i] [old_num_leafs: u64] [new_num_leafs: u64] height_of_lowest_old_peak

                push 0 push 1 hint one: u64 = stack[0..2]
                dup 2
                call {shl_u64} hint num_leafs_in_lowest_old_peak: u64 = stack[0..2]
                // _ [mt_index: u64] *old_peaks[-1] *new_peaks[i] [old_num_leafs: u64] [new_num_leafs: u64] height_of_lowest_old_peak [num_leafs_in_lowest_old_peak: u64]

                pick 6 pick 6
                pick 6 pick 6
                call {sub_u64}
                // _ [mt_index: u64] *old_peaks[-1] *new_peaks[i] height_of_lowest_old_peak [num_leafs_in_lowest_old_peak: u64] [num_new_leafs: u64]

                call {lt_u64}
                // _ [mt_index: u64] *old_peaks[-1] *new_peaks[i] height_of_lowest_old_peak (num_new_leafs < num_leafs_in_lowest_old_peak)

                push 0
                place 1
                skiz call {clean_up_because_new_leafs_dont_affect_old_peaks}
                skiz return
                // _ [mt_index: u64] *old_peaks[-1] *new_peaks[i] height_of_lowest_old_peak

                pick 4 pick 4
                pick 2
                // _ *old_peaks[-1] *new_peaks[i] [mt_index: u64] height_of_lowest_old_peak

                call {shr_u64} hint merkle_tree_idx: u64 = stack[0..2]
                // _ *old_peaks[-1] *new_peaks[i] [mt_index: u64]

                pick 2
                place 3
                divine 5
                // _ *new_peaks[i] *old_peaks[-1] [mt_index: u64] [current_node: Digest]

                call {traverse_new_tree}
                // _ *new_peaks[i] *old_peaks[j] [one: u64] [root: Digest]

                pick 8
                addi {Digest::LEN - 1}
                read_mem {Digest::LEN}
                pop 1
                // _ *old_peaks[j] [one: u64] [root: Digest] [new_peak[i]: Digest]

                assert_vector error_id {Self::DIFFERING_UNSHARED_PEAK_ERROR_ID}
                pop 5
                // _ *old_peaks[j] [one: u64]

                pop 3
                return

            // BEFORE: _ [old_num_leafs: u64] *old_peaks [new_num_leafs: u64] *new_peaks 0
            // AFTER:  _ 1
            {clean_up_because_old_mmr_has_0_leafs}:
                pop 5
                pop 2
                push 1
                return

            // BEFORE: _ [num_leafs: u64] *old_peaks [num_leafs: u64] *new_peaks 0
            // AFTER:  _ 1
            {assert_mmr_equality}:
                pick 4
                addi 1
                // _ [num_leafs: u64] [num_leafs: u64] *new_peaks 0 *old_peaks[0]

                pick 2
                addi 1
                // _ [num_leafs: u64] [num_leafs: u64] 0 *old_peaks[0] *new_peaks[0]

                pick 4 pick 4
                call {popcount_u64}
                // _ [num_leafs: u64] 0 *old_peaks[0] *new_peaks[0] num_peaks

                call {assert_unchanged_peaks_equality}
                // _ [num_leafs: u64] 0 *old_peaks[n] *new_peaks[n] 0

                pop 5
                pop 1
                push 1
                return

            // BEFORE:    _ *old_peaks[0] *new_peaks[0] n
            // INVARIANT: _ *old_peaks[i] *new_peaks[i] (n - i)
            // AFTER:     _ *old_peaks[n] *new_peaks[n] 0
            {assert_unchanged_peaks_equality}:
                dup 0
                push 0
                eq
                skiz return
                // _ *old_peaks[i] *new_peaks[i] (n - i)

                pick 2
                addi {Digest::LEN - 1}
                read_mem {Digest::LEN}
                addi {Digest::LEN + 1}
                place 7
                // _ *old_peaks[i + 1] *new_peaks[i] (n - i) [old_peak[i]: Digest]

                pick 6
                addi {Digest::LEN - 1}
                read_mem {Digest::LEN}
                addi {Digest::LEN + 1}
                place 11
                // _ *old_peaks[i + 1] *new_peaks[i + 1] (n - i) [old_peak[i]: Digest] [new_peak[i]: Digest]

                assert_vector error_id {Self::DIFFERING_SHARED_PEAK_ERROR_ID}
                pop 5
                // _ *old_peaks[i + 1] *new_peaks[i + 1] (n - i)

                addi -1
                recurse

            // BEFORE: _ [mt_index: u64] *old_peaks[-1] *new_peaks[i] height_of_lowest_old_peak 0
            // AFTER:  _ 1
            {clean_up_because_new_leafs_dont_affect_old_peaks}:
                pop 5
                pop 1
                push 1
                return

            // INVARIANT: _ *old_peaks[j] [mt_index >> i: u64] [current_node: Digest]
            {traverse_new_tree}:
                dup 6 dup 6
                push 0 push 1 hint merkle_tree_root_index: u64 = stack[0..2]
                {&DataType::U64.compare()}
                skiz return
                // _ *old_peaks[j] [mt_index >> i: u64] [current_node: Digest]

                push 1
                dup 6
                push 1
                and
                // _ *old_peaks[j] [mt_index >> i: u64] [current_node: Digest] 1 is_right_sibling

                skiz call {traverse_new_tree_right_sibling}
                skiz call {merkle_step_u64}
                // _ *old_peaks[j'] [mt_index >> (i + 1): u64] [next_node: Digest]

                recurse

            // BEFORE: _ *old_peaks[j]   [mt_index >> i: u64]       [current_node: Digest] 1
            // AFTER:  _ *old_peaks[j-1] [mt_index >> (i + 1): u64] [next_node: Digest]    0
            {traverse_new_tree_right_sibling}:
                pop 1
                call {merkle_step_mem_u64}
                // _ *old_peaks[j+1] [mt_index >> (i + 1): u64] [next_node: Digest]

                pick 7
                addi -10 // -(2 · Digest::LEN)
                place 7
                // _ *old_peaks[j-1] [mt_index >> (i + 1): u64] [next_node: Digest]

                push 0
                return
        }
    }

    fn sign_offs(&self) -> HashMap<Reviewer, SignOffFingerprint> {
        let mut sign_offs = HashMap::new();
        sign_offs.insert(Reviewer("ferdinand"), 0x6b56efd7dd2b9e4f.into());
        sign_offs.insert(Reviewer("alan"), 0x6b56efd7dd2b9e4f.into());
        sign_offs
    }
}

impl VerifyMmrSuccessor {
    /// Update a nondeterminism in accordance with verifying a given [`MmrSuccessorProof`]
    /// with this snippet.
    pub fn update_nondeterminism(
        nondeterminism: &mut NonDeterminism,
        mmr_successor_proof: &MmrSuccessorProof,
    ) {
        let mut auth_path = mmr_successor_proof.paths.iter();
        if let Some(&first) = auth_path.next() {
            nondeterminism
                .individual_tokens
                .extend(first.reversed().values());
            nondeterminism.digests.extend(auth_path);
        };
    }
}

#[cfg(test)]
mod tests {
    use std::collections::VecDeque;

    use twenty_first::util_types::mmr::mmr_accumulator::MmrAccumulator;
    use twenty_first::util_types::mmr::mmr_successor_proof::MmrSuccessorProof;

    use super::*;
    use crate::empty_stack;
    use crate::test_prelude::*;
    use crate::twenty_first::prelude::Mmr;

    impl ReadOnlyAlgorithm for VerifyMmrSuccessor {
        fn rust_shadow(
            &self,
            stack: &mut Vec<BFieldElement>,
            memory: &HashMap<BFieldElement, BFieldElement>,
            nd_tokens: VecDeque<BFieldElement>,
            nd_digests: VecDeque<Digest>,
        ) {
            let new_mmr_pointer = stack.pop().unwrap();
            let old_mmr_pointer = stack.pop().unwrap();

            let new_mmr = *MmrAccumulator::decode_from_memory(memory, new_mmr_pointer).unwrap();
            let old_mmr = *MmrAccumulator::decode_from_memory(memory, old_mmr_pointer).unwrap();

            // figure out the length of the authentication path
            let num_new_leafs = new_mmr.num_leafs() - old_mmr.num_leafs();
            let new_dummy_leafs = vec![Digest::default(); num_new_leafs.try_into().unwrap()];
            let dummy_proof = MmrSuccessorProof::new_from_batch_append(&old_mmr, &new_dummy_leafs);
            let auth_path_len = dummy_proof.paths.len();

            // grab first path element from nd tokens
            let mut path = vec![];
            if auth_path_len > 0 {
                let first_element = (0..Digest::LEN).rev().map(|i| nd_tokens[i]).collect_vec();
                path.push(Digest::new(first_element.try_into().unwrap()));
            }

            // grab remaining path elements from nd digests
            if auth_path_len > 1 {
                path.extend((0..auth_path_len - 1).map(|i| nd_digests[i]));
            }

            assert!(MmrSuccessorProof { paths: path }.verify(&old_mmr, &new_mmr));
        }

        fn pseudorandom_initial_state(
            &self,
            seed: [u8; 32],
            bench_case: Option<BenchmarkCase>,
        ) -> ReadOnlyAlgorithmInitialState {
            let mut rng = StdRng::from_seed(seed);
            let old_num_leafs = match bench_case {
                Some(BenchmarkCase::CommonCase) => u32::MAX.into(),
                Some(BenchmarkCase::WorstCase) => u64::MAX >> 2,
                None => rng.next_u64() >> 1,
            };
            let old_peaks = (0..old_num_leafs.count_ones())
                .map(|_| rng.random())
                .collect();
            let old = MmrAccumulator::init(old_peaks, old_num_leafs);

            let num_new_leafs = match bench_case {
                Some(BenchmarkCase::CommonCase) => 100,
                Some(BenchmarkCase::WorstCase) => 1000,
                None => 1 << rng.random_range(0..5),
            };
            let new_leafs = (0..num_new_leafs).map(|_| rng.random()).collect_vec();
            let proof = MmrSuccessorProof::new_from_batch_append(&old, &new_leafs);

            let mut new = old.clone();
            for leaf in new_leafs {
                new.append(leaf);
            }

            initial_state(&old, &new, &proof)
        }

        fn corner_case_initial_states(&self) -> Vec<ReadOnlyAlgorithmInitialState> {
            let mut rng = rand::rng();
            let mut initial_states = vec![];

            for (num_old_leafs, num_inserted_leafs) in [0_u64, 1, 2, 3, 4, 8]
                .into_iter()
                .cartesian_product([0, 1, 2, 3, 4, 8])
            {
                let old_peaks = (0..num_old_leafs.count_ones())
                    .map(|_| rng.random())
                    .collect();
                let old = MmrAccumulator::init(old_peaks, num_old_leafs);

                let mut new = old.clone();
                let new_leafs = (0..num_inserted_leafs).map(|_| rng.random()).collect_vec();
                for &leaf in &new_leafs {
                    new.append(leaf);
                }
                let proof = MmrSuccessorProof::new_from_batch_append(&old, &new_leafs);

                initial_states.push(initial_state(&old, &new, &proof));
            }

            initial_states
        }
    }

    fn initial_state(
        old: &MmrAccumulator,
        new: &MmrAccumulator,
        proof: &MmrSuccessorProof,
    ) -> ReadOnlyAlgorithmInitialState {
        let Digest(seed) = Tip5::hash_pair(Tip5::hash(old), Tip5::hash(new));
        let seed = seed
            .into_iter()
            .flat_map(|bfe| bfe.raw_bytes())
            .take(32)
            .collect_vec();
        let mut rng = StdRng::from_seed(seed.try_into().unwrap());

        let address_for_old = bfe!(rng.random_range(0_u32..1 << 30));
        let address_for_new =
            address_for_old + bfe!(old.encode().len()) + bfe!(rng.random_range(0_u32..1 << 28));

        let mut nondeterminism = NonDeterminism::default();
        VerifyMmrSuccessor::update_nondeterminism(&mut nondeterminism, proof);
        encode_to_memory(&mut nondeterminism.ram, address_for_old, old);
        encode_to_memory(&mut nondeterminism.ram, address_for_new, new);

        let mut stack = empty_stack();
        stack.push(address_for_old);
        stack.push(address_for_new);

        ReadOnlyAlgorithmInitialState {
            stack,
            nondeterminism,
        }
    }

    fn failing_initial_states() -> Vec<ReadOnlyAlgorithmInitialState> {
        let one_leaf = MmrAccumulator::new_from_leafs(vec![Digest::default()]);
        let empty = MmrAccumulator::new_from_leafs(vec![]);
        let bogus_proof = MmrSuccessorProof { paths: vec![] };
        let mut initial_states = vec![initial_state(&one_leaf, &empty, &bogus_proof)];

        let mut rng = StdRng::seed_from_u64(0x18c78fc35da66859);
        for (old_num_leafs, new_num_leafs) in
            [1_u64, 2, 3, 8].into_iter().cartesian_product([0, 1, 8])
        {
            let old_peaks = (0..old_num_leafs.count_ones())
                .map(|_| rng.random())
                .collect();
            let old = MmrAccumulator::init(old_peaks, old_num_leafs);

            let new_leafs = (0..new_num_leafs).map(|_| rng.random()).collect_vec();
            let mut new_mmr = old.clone();
            for &leaf in &new_leafs {
                new_mmr.append(leaf);
            }
            let new = new_mmr;
            let proof = MmrSuccessorProof::new_from_batch_append(&old, &new_leafs);

            let wrong_old = MmrAccumulator::init(old.peaks(), old.num_leafs().rotate_left(1));
            initial_states.push(initial_state(&wrong_old, &new, &proof));

            let wrong_new = MmrAccumulator::init(new.peaks(), new.num_leafs().rotate_left(1));
            initial_states.push(initial_state(&old, &wrong_new, &proof));

            let mut wrong_new_peaks = new.peaks();
            wrong_new_peaks.push(rng.random());
            let too_many_peaks_new = MmrAccumulator::init(wrong_new_peaks, new.num_leafs());
            initial_states.push(initial_state(&old, &too_many_peaks_new, &proof));

            for peak_idx in 0..old.peaks().len() {
                let mut wrong_old_peaks = old.peaks();
                let Digest(ref mut digest_to_disturb) = wrong_old_peaks[peak_idx];
                let digest_to_disturb_innards_idx = rng.random_range(0..Digest::LEN);
                digest_to_disturb[digest_to_disturb_innards_idx].increment();

                let wrong_old = MmrAccumulator::init(wrong_old_peaks, old.num_leafs());
                initial_states.push(initial_state(&wrong_old, &new, &proof));
            }

            for proof_path_idx in 0..proof.paths.len() {
                let mut wrong_proof = proof.clone();
                let proof_paths = &mut wrong_proof.paths;
                let Digest(ref mut digest_to_disturb) = proof_paths[proof_path_idx];
                let digest_to_disturb_innards_idx = rng.random_range(0..Digest::LEN);
                digest_to_disturb[digest_to_disturb_innards_idx].increment();

                initial_states.push(initial_state(&old, &new, &wrong_proof));
            }
        }

        // the secret input being underpopulated is no acceptable failure reason
        for state in &mut initial_states {
            let non_determinism = &mut state.nondeterminism;
            non_determinism.individual_tokens.extend(bfe_array![0; 5]);
            non_determinism.digests.extend([Digest::default(); 1000]);
        }

        initial_states
    }

    #[test]
    fn unit() {
        ShadowedReadOnlyAlgorithm::new(VerifyMmrSuccessor).test();
    }

    #[test]
    fn verify_mmr_successor_negative_test() {
        let all_error_ids = [
            VerifyMmrSuccessor::OLD_HAS_MORE_LEAFS_THAN_NEW_ERROR_ID,
            VerifyMmrSuccessor::INCONSISTENT_OLD_MMR_ERROR_ID,
            VerifyMmrSuccessor::INCONSISTENT_NEW_MMR_ERROR_ID,
            VerifyMmrSuccessor::DIFFERING_SHARED_PEAK_ERROR_ID,
            VerifyMmrSuccessor::DIFFERING_UNSHARED_PEAK_ERROR_ID,
        ];

        for (i, init_state) in failing_initial_states().into_iter().enumerate() {
            dbg!(i);
            test_assertion_failure(
                &ShadowedReadOnlyAlgorithm::new(VerifyMmrSuccessor),
                init_state.into(),
                &all_error_ids,
            );
        }
    }
}

#[cfg(test)]
mod bench {
    use super::*;
    use crate::test_prelude::*;

    #[test]
    fn benchmark() {
        ShadowedReadOnlyAlgorithm::new(VerifyMmrSuccessor).bench();
    }
}
