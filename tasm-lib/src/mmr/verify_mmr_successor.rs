use triton_vm::prelude::*;
use twenty_first::util_types::mmr::mmr_accumulator::MmrAccumulator;
use twenty_first::util_types::mmr::mmr_successor_proof::MmrSuccessorProof;

use crate::field;
use crate::hashing::merkle_step_u64_index::MerkleStepU64Index;
use crate::mmr::leaf_index_to_mt_index_and_peak_index::MmrLeafIndexToMtIndexAndPeakIndex;
use crate::prelude::*;

/// Verify that one MMR is a successor to another.
///
/// Verify the successorship relation between two MMRs. A `MmrSuccessorProof`
/// is necessary to demonstrate this relation, but it is not a *stack* argument
/// because this algorithm obtains the relevant info (authentication paths) from
/// nondeterministic digests. Accordingly, nondeterminism must be initialized
/// correctly with the `MmrSuccessorProof`.
///
/// This snippet crashes if the relation does not hold (or if the proof is invalid).
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
/// - todo
///
/// ### Postconditions
///
/// - todo
pub struct VerifyMmrSuccessor;

impl VerifyMmrSuccessor {
    pub(crate) const A_ERROR_ID: i128 = 150;
    pub(crate) const B_ERROR_ID: i128 = 151;
    pub(crate) const C_ERROR_ID: i128 = 152;
    pub(crate) const UNEQUAL_PEAK_DESPITE_EQUAL_NUM_LEAFS_ERROR_ID: i128 = 153;
}

impl BasicSnippet for VerifyMmrSuccessor {
    fn inputs(&self) -> Vec<(DataType, String)> {
        ["*old_mmr", "*new_mmr"]
            .map(|ptr_name| (DataType::VoidPointer, ptr_name.to_string()))
            .to_vec()
    }

    fn outputs(&self) -> Vec<(DataType, String)> {
        vec![]
    }

    fn entrypoint(&self) -> String {
        "tasm_lib_mmr_verify_mmr_successor".to_string()
    }

    fn code(&self, library: &mut Library) -> Vec<LabelledInstruction> {
        let num_leafs = triton_asm!(
            // _ *mmr
            {&field!(MmrAccumulator::leaf_count)}
            // _ *leaf_count
            addi 1 read_mem 2
            // _ [num_leafs] (*leaf_count-1)
            pop 1
        );

        let add_u64 = library.import(Box::new(crate::arithmetic::u64::add::Add));
        let ilog2_u64 = library.import(Box::new(crate::arithmetic::u64::log_2_floor::Log2Floor));
        let ltu64 = library.import(Box::new(crate::arithmetic::u64::lt::Lt));
        let popcount_u64 = library.import(Box::new(crate::arithmetic::u64::popcount::PopCount));
        let shr_u64 = library.import(Box::new(crate::arithmetic::u64::shift_right::ShiftRight));
        let sub_u64 = library.import(Box::new(crate::arithmetic::u64::sub::Sub));

        let bag_peaks = library.import(Box::new(crate::mmr::bag_peaks::BagPeaks));
        let leaf_index_to_mti_and_pki = library.import(Box::new(MmrLeafIndexToMtIndexAndPeakIndex));
        let merkle_step_u64 = library.import(Box::new(MerkleStepU64Index));

        let compare_u64 = DataType::U64.compare();

        let entrypoint = self.entrypoint();
        let main_loop = format!("{entrypoint}_main_loop");
        let traverse = format!("{entrypoint}_traverse_partial_auth_path");
        let cleanup_and_return = format!("{entrypoint}_cleanup_and_return");
        let assert_mmrs_equal = format!("{entrypoint}_assert_mmrs_equal");

        let strip_top_bit = triton_asm!(
            // BEFORE: [num_leafs_remaining] garbage
            // AFTER:  [num_leafs_remaining*] old_height
            pop 1
            dup 1 dup 1 call {ilog2_u64}
            // [num_leafs_remaining] old_height
            hint old_height = stack[0]

            dup 0 push 2 pow split
            // [num_leafs_remaining] old_height [1<<old_height]
            hint two_pow_old_height = stack[0..2]

            dup 4 dup 4
            // [num_leafs_remaining] old_height [1<<old_height] [num_leafs_remaining]
            call {sub_u64}
            // [num_leafs_remaining] old_height [num_leafs_remaining*]
            hint new_num_leafs_remaining = stack[0..2]

            swap 3 swap 1 swap 4
            // [num_leafs_remaining*] old_height [num_leafs_remaining]
            pop 2
            // [num_leafs_remaining*] old_height
            hint num_leafs_remaining = stack[1..3]
        );

        triton_asm! {
            // BEFORE: _ *old_mmr *new_mmr
            // AFTER:  _
            {entrypoint}:

            /* tests before preparing loop */

            // num old leafs == 0 ?
            push 0
            // _ *old_mmr *new_mmr 0

            dup 2 {&num_leafs}
            // _ *old_mmr *new_mmr 0 [old_num_leafs]

            push 0 push 0
            // _ *old_mmr *new_mmr 0 [old_num_leafs] [0]

            {&compare_u64}
            // _ *old_mmr *new_mmr 0 (old_num_leafs == 0)

            skiz call {cleanup_and_return}
            skiz return
            // _ *old_mmr *new_mmr


            // new num leafs == old num leafs
            push 0
            // _ *old_mmr *new_mmr 0

            dup 2 {&num_leafs}
            // _ *old_mmr *new_mmr 0 [old_num_leafs]

            dup 3 {&num_leafs}
            // _ *old_mmr *new_mmr 0 [old_num_leafs] [new_num_leafs]

            {&compare_u64}
            // _ *old_mmr *new_mmr 0 (old_num_leafs == new_num_leafs)

            skiz call {assert_mmrs_equal}
            skiz return
            // _ *old_mmr *new_mmr


            // new num leafs < old num leafs  ?
            dup 1 {&num_leafs}
            // _ *old_mmr *new_mmr [old_num_leafs: u64]

            dup 2 {&num_leafs}
            // _ *old_mmr *new_mmr [old_num_leafs: u64] [new_num_leafs: u64]

            call {ltu64}
            // _ *old_mmr *new_mmr (new_num_leafs < old_num_leafs)

            push 0 eq
            // _ *old_mmr *new_mmr (new_num_leafs >= old_num_leafs)

            assert error_id {Self::A_ERROR_ID}
            // _ *old_mmr *new_mmr


            // consistent new mmr?
            dup 0
            {&field!(MmrAccumulator::peaks)}
            // _ *old_mmr *new_mmr *new_mmr_peaks

            read_mem 1
            pop 1
            // _ *old_mmr *new_mmr new_num_peaks

            dup 1 {&num_leafs} call {popcount_u64}
            // _ *old_mmr *new_mmr new_num_peaks (popcount of new_num_leafs)

            eq assert error_id  {Self::B_ERROR_ID}
            // _ *old_mmr *new_mmr


            /* prepare and call loop */
            dup 1 {&field!(MmrAccumulator::peaks)}
            // _ *old_mmr *new_mmr *old_peaks

            read_mem 1 push 2 add
            // _ *old_mmr *new_mmr num_old_peaks *old_peaks[0]
            hint current_peak_ptr = stack[0]

            swap 1 push {Digest::LEN} mul
            // _ *old_mmr *new_mmr *old_peaks[0] (num_old_peaks*5)

            dup 1 add
            // _ *old_mmr *new_mmr *old_peaks[0] *end_of_memory
            hint end_of_memory = stack[0]

            push 0 push 0
            // _ *old_mmr *new_mmr *old_peaks[0] *end_of_memory [0]
            hint running_leaf_count = stack[0..2]

            dup 5 {&num_leafs}
            // _ *old_mmr *new_mmr *old_peaks[0] *end_of_memory [0] [old_num_leafs]
            hint num_leafs_remaining = stack[0..2]

            push {0x455b00b5}
            // _ *old_mmr *new_mmr *old_peaks[0] *end_of_memory [0] [old_num_leafs] garbage

            call {main_loop}
            // _ *old_mmr *new_mmr *end_of_memory *end_of_memory [old_num_leafs] [0] garbage

            /* clean up after loop */
            pop 5
            // _ *old_mmr *new_mmr *end_of_memory *end_of_memory

            pop 4
            // _

            return

            // INVARIANT: _ *new_mmr *current_peak *end_of_memory [running_leaf_count] [num_leafs_remaining] garbage
            {main_loop}:

                {&strip_top_bit}
                // _ *new_mmr *current_peak *end_of_memory [running_leaf_count] [num_leafs_remaining*] old_height

                dup 7 {&num_leafs}
                // _ *new_mmr *current_peak *end_of_memory [running_leaf_count] [num_leafs_remaining*] old_height [new_num_leafs]

                dup 6 dup 6
                // _ *new_mmr *current_peak *end_of_memory [running_leaf_count] [num_leafs_remaining*] old_height [new_num_leafs] [running_leaf_count]

                call {leaf_index_to_mti_and_pki}
                // _ *new_mmr *current_peak *end_of_memory [running_leaf_count] [num_leafs_remaining*] old_height [merkle_tree_index] peak_index
                hint merkle_tree_index = stack[1..3]
                hint peak_index = stack[0]

                swap 2 swap 1
                // _ *new_mmr *current_peak *end_of_memory [running_leaf_count] [num_leafs_remaining*] old_height peak_index [merkle_tree_index]

                dup 3
                // _ *new_mmr *current_peak *end_of_memory [running_leaf_count] [num_leafs_remaining*] old_height peak_index [merkle_tree_index] old_height

                call {shr_u64}
                // _ *new_mmr *current_peak *end_of_memory [running_leaf_count] [num_leafs_remaining*] old_height peak_index [merkle_tree_index >> old_height]


                /* prepare & traverse */
                dup 9 push {Digest::LEN-1} add read_mem {Digest::LEN} pop 1
                // _ *new_mmr *current_peak *end_of_memory [running_leaf_count] [num_leafs_remaining*] old_height peak_index [merkle_tree_index >> old_height] [current_old_peak]
                hint merkle_node = stack[0..5]

                call {traverse}
                // _ *new_mmr *current_peak *end_of_memory [running_leaf_count] [num_leafs_remaining*] old_height peak_index [1] [some_new_peak]
                hint landed_peak = stack[0..5]
                hint merkle_index = stack[5..7]
                hint peak_index = stack[7]

                dup 15 {&field!(MmrAccumulator::peaks)}
                // _ *new_mmr *current_peak *end_of_memory [running_leaf_count] [num_leafs_remaining*] old_height peak_index [1] [some_new_peak] *new_peaks

                push {1 + Digest::LEN - 1} dup 9
                hint peak_index = stack[0]

                push {Digest::LEN} mul add
                // _ *new_mmr *current_peak *end_of_memory [running_leaf_count] [num_leafs_remaining*] old_height peak_index [1] [some_new_peak] *new_peaks (5+peak_index*5)

                add read_mem {Digest::LEN} pop 1
                // _ *new_mmr *current_peak *end_of_memory [running_leaf_count] [num_leafs_remaining*] old_height peak_index [1] [some_new_peak] [new_peaks[peak_index]]

                assert_vector error_id {Self::C_ERROR_ID}
                pop 5
                // _ *new_mmr *current_peak *end_of_memory [running_leaf_count] [num_leafs_remaining*] old_height peak_index [1]


                /* prepare for next iteration */
                pop 3
                // _ *new_mmr *current_peak *end_of_memory [running_leaf_count] [num_leafs_remaining*] old_height

                push 2 pow split
                // _ *new_mmr *current_peak *end_of_memory [running_leaf_count] [num_leafs_remaining*] [1<<old_height]

                dup 5 dup 5 call {add_u64}
                // _ *new_mmr *current_peak *end_of_memory [running_leaf_count] [num_leafs_remaining*] [running_leaf_count*]
                hint running_leaf_count = stack[0..2]

                swap 4 swap 1 swap 5 pop 1
                // _ *new_mmr *current_peak *end_of_memory [running_leaf_count*] [num_leafs_remaining*] garbage

                swap 6 push {Digest::LEN} add swap 6
                // _ *new_mmr *next_peak *end_of_memory [running_leaf_count*] [num_leafs_remaining*] garbage
                hint next_peak_ptr = stack[6]

                recurse_or_return

            // INVARIANT: _ [merkle_tree_index] [current_node]
            {traverse}:
                // evaluate termination condition
                dup 6 dup 6 push 0 push 1
                // _ [merkle_tree_index] [current_node] [merkle_tree_index] [1]

                {&compare_u64}
                // _ [merkle_tree_index] [current_node] (merkle_tree_index == 1)

                skiz return
                // _ [merkle_tree_index] [current_node]

                call {merkle_step_u64}
                // _ [merkle_tree_index] [current_node*]

                recurse

            // BEFORE: _ *old_mmr *new_mmr 0
            // AFTER:  _ 1
            {cleanup_and_return}:
                pop 3
                push 1
                return

            // BEFORE: _ *old_mmr *new_mmr 0
            // AFTER:  _ 1
            {assert_mmrs_equal}:
                pick 1 {&field!(MmrAccumulator::peaks)}
                // _ *old_mmr 0 *new_mmr_peaks

                call {bag_peaks}
                // _ *old_mmr 0 [new_bagged: Digest]

                pick 6 {&field!(MmrAccumulator::peaks)}
                // _ 0 [new_bagged: Digest] *old_mmr_peaks

                call {bag_peaks}
                // _ 0 [new_bagged: Digest] [old_bagged: Digest]

                assert_vector error_id {Self::UNEQUAL_PEAK_DESPITE_EQUAL_NUM_LEAFS_ERROR_ID}
                pop 5
                // _ 0

                pop 1
                push 1
                // _ 1

                return
        }
    }
}

impl VerifyMmrSuccessor {
    /// Update a nondeterminism in accordance with verifying a given `MmrSuccessorProof`
    /// with this snippet.
    pub fn update_nondeterminism(
        nondeterminism: &mut NonDeterminism,
        mmr_successor_proof: &MmrSuccessorProof,
    ) {
        nondeterminism
            .digests
            .append(&mut mmr_successor_proof.paths.clone())
    }
}

#[cfg(test)]
mod tests {
    use std::collections::VecDeque;

    use twenty_first::util_types::mmr::mmr_accumulator::MmrAccumulator;
    use twenty_first::util_types::mmr::mmr_successor_proof::MmrSuccessorProof;
    use twenty_first::util_types::mmr::shared_advanced::get_peak_heights;
    use twenty_first::util_types::mmr::shared_basic::leaf_index_to_mt_index_and_peak_index;

    use super::*;
    use crate::empty_stack;
    use crate::test_prelude::*;
    use crate::traits::mem_preserver::MemPreserver;
    use crate::traits::mem_preserver::MemPreserverInitialState;
    use crate::twenty_first::prelude::Mmr;

    fn num_digests_to_read(old_mmr: &MmrAccumulator, new_mmr: &MmrAccumulator) -> usize {
        if new_mmr.num_leafs() <= old_mmr.num_leafs() {
            return 0;
        }

        let mut number = 0;
        let mut running_leaf_count = 0;
        let old_peak_heights = get_peak_heights(old_mmr.num_leafs());
        let mut new_merkle_tree_indices_of_old_peaks = vec![];
        for old_peak_height in old_peak_heights {
            let (mut merkle_tree_index, _peak_index) =
                leaf_index_to_mt_index_and_peak_index(running_leaf_count, new_mmr.num_leafs());
            running_leaf_count += 1 << old_peak_height;
            merkle_tree_index >>= old_peak_height;
            new_merkle_tree_indices_of_old_peaks.push(merkle_tree_index);
        }
        for mut merkle_tree_index in new_merkle_tree_indices_of_old_peaks {
            while merkle_tree_index != 1 {
                number += 1;
                merkle_tree_index >>= 1;
            }
        }
        number
    }

    fn initial_state_from_mmr_tuple(
        old_mmr: &MmrAccumulator,
        new_mmr: &MmrAccumulator,
        mmr_successor_proof: &MmrSuccessorProof,
    ) -> MemPreserverInitialState {
        let Digest(seed) = Tip5::hash_pair(Tip5::hash(old_mmr), Tip5::hash(new_mmr));
        let seed = seed
            .into_iter()
            .flat_map(|bfe| bfe.raw_bytes())
            .take(32)
            .collect_vec();
        let mut rng = StdRng::from_seed(seed.try_into().unwrap());

        let old_mmr_address = bfe!(rng.gen_range(0_u32..1 << 30));
        let new_mmr_address = old_mmr_address + bfe!(rng.gen_range(0_u32..1 << 28));

        let mut nondeterminism = NonDeterminism::default();
        VerifyMmrSuccessor::update_nondeterminism(&mut nondeterminism, mmr_successor_proof);
        encode_to_memory(&mut nondeterminism.ram, old_mmr_address, old_mmr);
        encode_to_memory(&mut nondeterminism.ram, new_mmr_address, new_mmr);

        let mut stack = empty_stack();
        stack.push(old_mmr_address);
        stack.push(new_mmr_address);

        MemPreserverInitialState {
            stack,
            nondeterminism,
            ..Default::default()
        }
    }

    fn failing_initial_states() -> Vec<MemPreserverInitialState> {
        let mut rng = thread_rng();
        let mut initial_states = vec![];

        for (old_num_leafs, new_num_leafs) in
            [1_u64, 2, 3, 8].into_iter().cartesian_product([0, 1, 8])
        {
            if (old_num_leafs, new_num_leafs) == (0, 0) {
                continue;
            }

            let old_peaks = (0..old_num_leafs.count_ones()).map(|_| rng.gen()).collect();
            let old_mmr = MmrAccumulator::init(old_peaks, old_num_leafs);

            let new_leafs = (0..new_num_leafs).map(|_| rng.gen()).collect_vec();
            let mut new_mmr = old_mmr.clone();
            for &leaf in &new_leafs {
                new_mmr.append(leaf);
            }
            let new_mmr = new_mmr;

            let mmr_successor_proof =
                MmrSuccessorProof::new_from_batch_append(&old_mmr, &new_leafs);

            let old_mmr_fake =
                MmrAccumulator::init(old_mmr.peaks(), old_mmr.num_leafs().rotate_right(1));
            let rotate_old_num_leafs =
                initial_state_from_mmr_tuple(&old_mmr_fake, &new_mmr, &mmr_successor_proof);
            initial_states.push(rotate_old_num_leafs);

            let new_mmr_fake =
                MmrAccumulator::init(new_mmr.peaks(), new_mmr.num_leafs().rotate_right(1));
            let rotate_new_num_leafs =
                initial_state_from_mmr_tuple(&old_mmr, &new_mmr_fake, &mmr_successor_proof);
            initial_states.push(rotate_new_num_leafs);

            let mut fake_new_peaks = new_mmr.peaks();
            fake_new_peaks.push(rng.gen());
            let new_mmr_fake_peak = MmrAccumulator::init(fake_new_peaks, new_mmr.num_leafs());
            let inconsistent_new_mmr =
                initial_state_from_mmr_tuple(&old_mmr, &new_mmr_fake_peak, &mmr_successor_proof);
            initial_states.push(inconsistent_new_mmr);

            if mmr_successor_proof.paths.is_empty() {
                continue;
            }

            let mut mmr_successor_proof_fake_1 = mmr_successor_proof.clone();
            let proof_paths = &mut mmr_successor_proof_fake_1.paths;
            let digest_to_disturb_idx = rng.gen_range(0..proof_paths.len());
            let digest_innard_to_disturb_idx = rng.gen_range(0..Digest::LEN);
            let Digest(ref mut digest_to_disturb) = &mut proof_paths[digest_to_disturb_idx];
            digest_to_disturb[digest_innard_to_disturb_idx].increment();
            let modify_path_element =
                initial_state_from_mmr_tuple(&old_mmr, &new_mmr, &mmr_successor_proof_fake_1);
            initial_states.push(modify_path_element);
        }

        initial_states
    }

    impl MemPreserver for VerifyMmrSuccessor {
        fn rust_shadow(
            &self,
            stack: &mut Vec<BFieldElement>,
            memory: &HashMap<BFieldElement, BFieldElement>,
            _: VecDeque<BFieldElement>,
            nd_digests: VecDeque<Digest>,
            _: VecDeque<BFieldElement>,
            _: &mut Option<Tip5>,
        ) -> Vec<BFieldElement> {
            let new_mmr_pointer = stack.pop().unwrap();
            let old_mmr_pointer = stack.pop().unwrap();

            let new_mmr = *MmrAccumulator::decode_from_memory(memory, new_mmr_pointer).unwrap();
            let old_mmr = *MmrAccumulator::decode_from_memory(memory, old_mmr_pointer).unwrap();

            let num_digests = num_digests_to_read(&old_mmr, &new_mmr);
            let paths = (0..num_digests).map(|i| nd_digests[i]).collect();
            let mmr_successor_proof = MmrSuccessorProof { paths };

            assert!(mmr_successor_proof.verify(&old_mmr, &new_mmr));

            vec![]
        }

        fn pseudorandom_initial_state(
            &self,
            seed: [u8; 32],
            bench_case: Option<BenchmarkCase>,
        ) -> MemPreserverInitialState {
            let mut rng = StdRng::from_seed(seed);
            let old_num_leafs = match bench_case {
                Some(BenchmarkCase::WorstCase) => u64::MAX >> 2,
                Some(BenchmarkCase::CommonCase) => u32::MAX as u64,
                None => rng.next_u64() & (u64::MAX >> 1),
            };
            let old_peaks = (0..old_num_leafs.count_ones()).map(|_| rng.gen()).collect();
            let old_mmr = MmrAccumulator::init(old_peaks, old_num_leafs);

            let num_new_leafs = match bench_case {
                Some(BenchmarkCase::CommonCase) => 100,
                Some(BenchmarkCase::WorstCase) => 1000,
                None => 1 << rng.gen_range(0..5),
            };
            let new_leafs = (0..num_new_leafs).map(|_| rng.gen()).collect_vec();
            let mmr_successor_proof =
                MmrSuccessorProof::new_from_batch_append(&old_mmr, &new_leafs);
            let mut new_mmr = old_mmr.clone();
            for leaf in new_leafs {
                new_mmr.append(leaf);
            }

            initial_state_from_mmr_tuple(&old_mmr, &new_mmr, &mmr_successor_proof)
        }

        fn corner_case_initial_states(&self) -> Vec<MemPreserverInitialState> {
            let mut rng = thread_rng();
            let mut initial_states = vec![];

            for (old_num_leafs, num_inserted_leafs) in [0_u64, 1, 2, 3, 4, 8]
                .into_iter()
                .cartesian_product([0, 1, 2, 3, 4, 8])
            {
                let old_peaks = (0..old_num_leafs.count_ones()).map(|_| rng.gen()).collect();
                let old_mmr = MmrAccumulator::init(old_peaks, old_num_leafs);

                let mut new_mmr = old_mmr.clone();
                let new_leafs = (0..num_inserted_leafs).map(|_| rng.gen()).collect_vec();
                for leaf in new_leafs.iter().copied() {
                    new_mmr.append(leaf);
                }
                let mmr_successor_proof =
                    MmrSuccessorProof::new_from_batch_append(&old_mmr, &new_leafs);

                let initial_state =
                    initial_state_from_mmr_tuple(&old_mmr, &new_mmr, &mmr_successor_proof);
                initial_states.push(initial_state);
            }

            initial_states
        }
    }

    #[test]
    fn verify_mmr_successor_simple_test() {
        ShadowedMemPreserver::new(VerifyMmrSuccessor).test();
    }

    #[test]
    fn verify_mmr_successor_negative_test() {
        for (i, init_state) in failing_initial_states().into_iter().enumerate() {
            dbg!(i);
            test_assertion_failure(
                &ShadowedMemPreserver::new(VerifyMmrSuccessor),
                init_state.into(),
                &[
                    VerifyMmrSuccessor::A_ERROR_ID,
                    VerifyMmrSuccessor::B_ERROR_ID,
                    VerifyMmrSuccessor::C_ERROR_ID,
                    VerifyMmrSuccessor::UNEQUAL_PEAK_DESPITE_EQUAL_NUM_LEAFS_ERROR_ID,
                ],
            );
        }
    }

    #[test]
    fn test_num_digests_in_proof() {
        for (old_num_leafs, new_num_leafs) in [(1, 0), (0, 1), (116, 182)] {
            num_digests_prop(old_num_leafs, new_num_leafs);
        }
    }

    fn num_digests_prop(old_mmr_num_leafs: u64, num_new_leafs: u64) {
        let mut rng = thread_rng();
        let old_num_peaks = old_mmr_num_leafs.count_ones();
        let old_peaks = (0..old_num_peaks).map(|_| rng.gen()).collect();
        let old_mmr = MmrAccumulator::init(old_peaks, old_mmr_num_leafs);

        let new_leafs = (0..num_new_leafs).map(|_| rng.gen()).collect_vec();
        let mut new_mmr = old_mmr.clone();
        for leaf in new_leafs.iter().copied() {
            new_mmr.append(leaf);
        }

        let mmr_successor_proof = MmrSuccessorProof::new_from_batch_append(&old_mmr, &new_leafs);

        let num_leafs_on_path = num_digests_to_read(&old_mmr, &new_mmr);
        assert_eq!(mmr_successor_proof.paths.len(), num_leafs_on_path);
    }
}

#[cfg(test)]
mod bench {
    use super::*;
    use crate::test_prelude::*;

    #[test]
    fn benchmark() {
        ShadowedMemPreserver::new(VerifyMmrSuccessor).bench();
    }
}
