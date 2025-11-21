use triton_vm::prelude::*;

use super::leaf_index_to_mt_index_and_peak_index::MmrLeafIndexToMtIndexAndPeakIndex;
use crate::arithmetic::u32::is_odd::IsOdd;
use crate::arithmetic::u64::div2::Div2;
use crate::list::get::Get;
use crate::list::set::Set;
use crate::prelude::*;

/// Calculate new MMR peaks from a leaf mutation using Merkle tree indices walk up the tree
#[derive(Debug, Default, Copy, Clone, Eq, PartialEq, Hash)]
pub struct MmrCalculateNewPeaksFromLeafMutationMtIndices;

impl BasicSnippet for MmrCalculateNewPeaksFromLeafMutationMtIndices {
    fn parameters(&self) -> Vec<(DataType, String)> {
        let list_type = DataType::List(Box::new(DataType::Digest));
        vec![
            (list_type.clone(), "*auth_path".to_string()),
            (DataType::U64, "leaf_index".to_string()),
            (list_type, "*peaks".to_string()),
            (DataType::Digest, "digest".to_string()),
            (DataType::U64, "leaf_count".to_string()),
        ]
    }

    fn return_values(&self) -> Vec<(DataType, String)> {
        let list_type = DataType::List(Box::new(DataType::Digest));
        vec![
            (list_type, "*auth_path".to_string()),
            (DataType::U64, "leaf_index".to_string()),
        ]
    }

    fn entrypoint(&self) -> String {
        "tasmlib_mmr_calculate_new_peaks_from_leaf_mutation".into()
    }

    fn code(&self, library: &mut Library) -> Vec<LabelledInstruction> {
        let leaf_index_to_mt_index = library.import(Box::new(MmrLeafIndexToMtIndexAndPeakIndex));
        let u32_is_odd = library.import(Box::new(IsOdd));
        let get = library.import(Box::new(Get::new(DataType::Digest)));
        let set = library.import(Box::new(Set::new(DataType::Digest)));
        let div_2 = library.import(Box::new(Div2));

        let entrypoint = self.entrypoint();
        let while_loop = format!("{entrypoint}_while");
        let swap_digests = format!("{entrypoint}_swap_digests");

        triton_asm!(
            // BEFORE: _ *auth_path leaf_index_hi leaf_index_lo *peaks [digest (leaf_digest)] leaf_count_hi leaf_count_lo
            // AFTER:  _ *auth_path leaf_index_hi leaf_index_lo
            {entrypoint}:
                dup 9 dup 9
                call {leaf_index_to_mt_index}
                // stack: _ *auth_path leaf_index_hi leaf_index_lo *peaks [digest (leaf_digest)] mt_index_hi mt_index_lo peak_index

                push 0
                // stack: _ *auth_path leaf_index_hi leaf_index_lo *peaks [digest (leaf_digest)] mt_index_hi mt_index_lo peak_index i

                swap 8 swap 4 swap 1 swap 7 swap 3 swap 6 swap 2 swap 5 swap 1
                // stack: _ *auth_path leaf_index_hi leaf_index_lo *peaks i peak_index mt_index_hi mt_index_lo [digest (leaf_digest)]
                // rename: leaf_digest -> acc_hash

                call {while_loop}
                // _ _ *auth_path leaf_index_hi leaf_index_lo *peaks i peak_index mt_index_hi mt_index_lo [digest (acc_hash)]

                dup 9 dup 8
                // _ *auth_path leaf_index_hi leaf_index_lo *peaks i peak_index mt_index_hi mt_index_lo [digest [digest (acc_hash)] *peaks peak_index

                call {set}
                // _ *auth_path leaf_index_hi leaf_index_lo *peaks i peak_index mt_index_hi mt_index_lo

                pop 5

                return

            // Note that this while loop is the same as one in `verify_from_memory`
            // INVARIANT: _ *auth_path leaf_index_hi leaf_index_lo *peaks i peak_index mt_index_hi mt_index_lo [digest (leaf_digest)]
            {while_loop}:
                dup 6 dup 6 push 0 push 1 {&DataType::U64.compare()}
                // _ *auth_path leaf_index_hi leaf_index_lo *peaks i peak_index mt_index_hi mt_index_lo [digest (leaf_digest)] (mt_index == 1)

                skiz return
                // _ *auth_path leaf_index_hi leaf_index_lo *peaks i peak_index mt_index_hi mt_index_lo [digest (leaf_digest)]

                // declare `ap_element = auth_path[i]`
                dup 12 dup 9 call {get}
                // _ *auth_path leaf_index_hi leaf_index_lo *peaks i peak_index mt_index_hi mt_index_lo [digest (leaf_digest)] [digest (ap_element)]

                dup 10 call {u32_is_odd} push 0 eq
                // _ *auth_path leaf_index_hi leaf_index_lo *peaks i peak_index mt_index_hi mt_index_lo [digest (acc_hash)] [digest (ap_element)] (mt_index % 2 == 0)

                skiz call {swap_digests}
                // _ *auth_path leaf_index_hi leaf_index_lo *peaks i peak_index mt_index_hi mt_index_lo [digest (right_node)] [digest (left_node)]

                hash
                // _ *auth_path leaf_index_hi leaf_index_lo *peaks i peak_index mt_index_hi mt_index_lo [digest (new_acc_hash)]

                // i -> i + 1
                swap 8 push 1 add swap 8
                // _ *auth_path leaf_index_hi leaf_index_lo *peaks (i + 1) peak_index mt_index_hi mt_index_lo [digest (new_acc_hash)]

                // mt_index -> mt_index / 2
                swap 6 swap 1 swap 5
                // _ *auth_path [digest (leaf_digest)] *peaks peak_index acc_hash_0 acc_hash_1 (i + 1) acc_hash_4 acc_hash_3 acc_hash_2 mt_index_hi mt_index_lo

                call {div_2}
                // _ *auth_path [digest (leaf_digest)] *peaks peak_index acc_hash_0 acc_hash_1 (i + 1) acc_hash_4 acc_hash_3 acc_hash_2 (mt_index / 2)_hi (mt_index / 2)_lo

                swap 5 swap 1 swap 6
                // _ *auth_path [digest (leaf_digest)] *peaks (mt_index / 2)_hi (mt_index / 2)_lo peak_index (i + 1) acc_hash_4 acc_hash_3 acc_hash_2 acc_hash_1 acc_hash_0

                recurse

            // purpose: swap the two digests `i` (node with `acc_hash`) is left child
            {swap_digests}:
                // _ *auth_path leaf_index_hi leaf_index_lo *peaks i peak_index mt_index_hi mt_index_lo [digest (acc_hash)] [digest (ap_element)]
                pick 9
                pick 9
                pick 9
                pick 9
                pick 9
                // _ *auth_path leaf_index_hi leaf_index_lo *peaks i peak_index mt_index_hi mt_index_lo [digest (ap_element)] [digest (acc_hash)]

                return

        )
    }
}

#[cfg(test)]
mod tests {
    use itertools::Itertools;

    use super::*;
    use crate::empty_stack;
    use crate::mmr::MAX_MMR_HEIGHT;
    use crate::test_prelude::*;
    use crate::twenty_first::math::other::random_elements;
    use crate::twenty_first::prelude::*;
    use crate::twenty_first::util_types::mmr;
    use crate::twenty_first::util_types::mmr::mmr_accumulator::MmrAccumulator;
    use crate::twenty_first::util_types::mmr::mmr_accumulator::util::mmra_with_mps;
    use crate::twenty_first::util_types::mmr::mmr_trait::LeafMutation;

    // These consts are an improvement to the previous situation.
    // I'll be the first to admit that this is not pretty, either.
    // At least they highlight that there is an issue that should be resolved.
    const AUTH_PATH_POINTER: BFieldElement =
        BFieldElement::new((MAX_MMR_HEIGHT * Digest::LEN + 2) as u64);
    const PEAKS_POINTER: BFieldElement = BFieldElement::new(1);

    impl MmrCalculateNewPeaksFromLeafMutationMtIndices {
        fn prepare_state_with_mmra(
            &self,
            start_mmr: &mut MmrAccumulator,
            leaf_index: u64,
            new_leaf: Digest,
            auth_path: Vec<Digest>,
        ) -> FunctionInitialState {
            let mut stack = empty_stack();
            stack.push(AUTH_PATH_POINTER);
            push_encodable(&mut stack, &leaf_index);
            stack.push(PEAKS_POINTER);
            push_encodable(&mut stack, &new_leaf);
            push_encodable(&mut stack, &start_mmr.num_leafs());

            let mut memory = HashMap::default();
            encode_to_memory(&mut memory, PEAKS_POINTER, &start_mmr.peaks());
            encode_to_memory(&mut memory, AUTH_PATH_POINTER, &auth_path);

            FunctionInitialState { stack, memory }
        }
    }

    impl Function for MmrCalculateNewPeaksFromLeafMutationMtIndices {
        fn rust_shadow(
            &self,
            stack: &mut Vec<BFieldElement>,
            memory: &mut HashMap<BFieldElement, BFieldElement>,
        ) {
            let leaf_count = pop_encodable(stack);
            let new_leaf = pop_encodable(stack);
            let peaks_pointer = stack.pop().unwrap();
            let leaf_index = pop_encodable(stack);
            let auth_path_pointer = stack.pop().unwrap();

            let peaks = *Vec::decode_from_memory(memory, peaks_pointer).unwrap();
            let auth_path = *Vec::decode_from_memory(memory, auth_path_pointer).unwrap();
            let mmr_mp = MmrMembershipProof::new(auth_path);
            let new_peaks = mmr::shared_basic::calculate_new_peaks_from_leaf_mutation(
                &peaks, leaf_count, new_leaf, leaf_index, &mmr_mp,
            );
            encode_to_memory(memory, peaks_pointer, &new_peaks);

            stack.push(auth_path_pointer);
            push_encodable(stack, &leaf_index);
        }

        fn pseudorandom_initial_state(
            &self,
            seed: [u8; 32],
            bench_case: Option<BenchmarkCase>,
        ) -> FunctionInitialState {
            let mut rng = StdRng::from_seed(seed);
            let (leaf_index, num_leafs) = match bench_case {
                Some(BenchmarkCase::CommonCase) => ((1 << 31) - 32, 1 << 31),
                Some(BenchmarkCase::WorstCase) => ((1 << 62) - 63, 1 << 62),
                None => {
                    let num_leafs = rng.random_range(1..=1 << 62);
                    let leaf_index = rng.random_range(0..num_leafs);
                    (leaf_index, num_leafs)
                }
            };

            let leaf = rng.random();
            let (mut mmra, mps) = mmra_with_mps(num_leafs, vec![(leaf_index, leaf)]);
            let auth_path = mps[0].clone();
            let new_leaf = rng.random();

            self.prepare_state_with_mmra(
                &mut mmra,
                leaf_index,
                new_leaf,
                auth_path.authentication_path,
            )
        }
    }

    #[test]
    fn rust_shadow() {
        ShadowedFunction::new(MmrCalculateNewPeaksFromLeafMutationMtIndices).test();
    }

    #[test]
    fn mmra_leaf_mutate_test_single() {
        let digest0 = Tip5::hash(&BFieldElement::new(4545));
        let digest1 = Tip5::hash(&BFieldElement::new(12345));
        let mut mmr = MmrAccumulator::new_from_leafs(vec![]);
        mmr.append(digest0);
        let expected_final_mmra = MmrAccumulator::new_from_leafs(vec![digest1]);
        let mutated_index = 0;
        prop_calculate_new_peaks_from_leaf_mutation(
            &mut mmr,
            digest1,
            mutated_index,
            expected_final_mmra,
            vec![],
        );
    }

    fn mmra_leaf_mutate_test_n_leafs(leaf_count: usize) {
        let init_leaf_digests: Vec<Digest> = random_elements(leaf_count);
        let new_leaf: Digest = rand::rng().random();

        let (mmra, mps) = mmra_with_mps(
            leaf_count as u64,
            init_leaf_digests
                .iter()
                .clone()
                .enumerate()
                .map(|(i, &d)| (i as u64, d))
                .collect_vec(),
        );

        for mutated_index in 0..leaf_count {
            let auth_path = mps[mutated_index].authentication_path.clone();
            let mut final_digests = init_leaf_digests.clone();
            final_digests[mutated_index] = new_leaf;
            let expected_final_mmra = MmrAccumulator::new_from_leafs(final_digests);
            prop_calculate_new_peaks_from_leaf_mutation(
                &mut mmra.clone(),
                new_leaf,
                mutated_index as u64,
                expected_final_mmra,
                auth_path,
            );
        }
    }

    #[test]
    fn mmra_leaf_mutate_test_many_leaf_sizes() {
        for leaf_count in 1..30 {
            mmra_leaf_mutate_test_n_leafs(leaf_count);
        }
    }

    #[test]
    fn mmra_leaf_mutate_test_other_leaf_sizes() {
        for leaf_count in [127, 128] {
            mmra_leaf_mutate_test_n_leafs(leaf_count);
        }
    }

    #[test]
    fn mmra_leaf_mutate_big() {
        for log_sizes in [15u64, 20, 25, 32, 35, 40, 45, 50, 55, 60, 62, 63] {
            println!("log_sizes = {log_sizes}");
            let init_peak_digests: Vec<Digest> = random_elements(log_sizes as usize);
            let new_leaf: Digest = rand::rng().random();
            let mut init_mmr =
                MmrAccumulator::init(init_peak_digests.clone(), (1u64 << log_sizes) - 1);

            let mut final_peaks = init_peak_digests.clone();
            final_peaks[log_sizes as usize - 1] = new_leaf;
            let expected_final_mmra = MmrAccumulator::init(final_peaks, (1u64 << log_sizes) - 1);
            prop_calculate_new_peaks_from_leaf_mutation(
                &mut init_mmr,
                new_leaf,
                (1u64 << log_sizes) - 2,
                expected_final_mmra,
                vec![],
            );
        }
    }

    #[test]
    fn mmra_leaf_mutate_advanced() {
        for log_size in [31, 63] {
            println!("log_sizes = {log_size}");
            let init_peak_digests: Vec<Digest> = random_elements(log_size as usize);
            let new_leaf: Digest = rand::rng().random();
            let before_insertion_mmr =
                MmrAccumulator::init(init_peak_digests.clone(), (1u64 << log_size) - 1);

            // Insert a leaf such that a very long (log_size long) auth path is returned
            let mut init_mmr = before_insertion_mmr.clone();
            let mp = init_mmr.append(rand::rng().random());

            let mut final_mmr = init_mmr.clone();
            let leaf_mutation =
                LeafMutation::new(before_insertion_mmr.num_leafs(), new_leaf, mp.clone());
            final_mmr.mutate_leaf(leaf_mutation);

            // Mutate the last element for which we just acquired an authentication path
            prop_calculate_new_peaks_from_leaf_mutation(
                &mut init_mmr,
                new_leaf,
                (1u64 << log_size) - 1,
                final_mmr,
                mp.authentication_path,
            );
        }
    }

    fn prop_calculate_new_peaks_from_leaf_mutation(
        start_mmr: &mut MmrAccumulator,
        new_leaf: Digest,
        new_leaf_index: u64,
        expected_mmr: MmrAccumulator,
        auth_path: Vec<Digest>,
    ) {
        let mmr_new_peaks = MmrCalculateNewPeaksFromLeafMutationMtIndices;
        let init_exec_state =
            mmr_new_peaks.prepare_state_with_mmra(start_mmr, new_leaf_index, new_leaf, auth_path);

        // AFTER: _ *auth_path leaf_index_hi leaf_index_lo
        let mut expected_final_stack = empty_stack();
        expected_final_stack.push(AUTH_PATH_POINTER);
        expected_final_stack.push(BFieldElement::new(new_leaf_index >> 32));
        expected_final_stack.push(BFieldElement::new(new_leaf_index & u32::MAX as u64));

        let vm_output = test_rust_equivalence_given_complete_state(
            &ShadowedFunction::new(MmrCalculateNewPeaksFromLeafMutationMtIndices),
            &init_exec_state.stack,
            &[],
            &NonDeterminism::default().with_ram(init_exec_state.memory),
            &None,
            Some(&expected_final_stack),
        );

        // Find produced MMR
        let final_memory = vm_output.ram;
        let produced_peaks = *Vec::decode_from_memory(&final_memory, PEAKS_POINTER).unwrap();
        let produced_mmr = MmrAccumulator::init(produced_peaks, start_mmr.num_leafs());

        // Verify that both code paths produce the same MMR
        assert_eq!(expected_mmr, produced_mmr);

        // Verify that auth paths is still value
        let auth_path = *Vec::decode_from_memory(&final_memory, AUTH_PATH_POINTER).unwrap();
        let mmr_mp = MmrMembershipProof::new(auth_path);
        assert!(
            mmr_mp.verify(
                new_leaf_index,
                new_leaf,
                &produced_mmr.peaks(),
                produced_mmr.num_leafs(),
            ),
            "TASM-produced authentication path must be valid"
        );

        // Extra checks because paranoia
        let mut expected_final_mmra_double_check = start_mmr.to_accumulator();
        expected_final_mmra_double_check.mutate_leaf(LeafMutation::new(
            new_leaf_index,
            new_leaf,
            mmr_mp.clone(),
        ));
        assert_eq!(expected_final_mmra_double_check, produced_mmr);
        assert!(mmr_mp.verify(
            new_leaf_index,
            new_leaf,
            &expected_final_mmra_double_check.peaks(),
            expected_final_mmra_double_check.num_leafs()
        ));
    }
}

#[cfg(test)]
mod benches {
    use super::*;
    use crate::test_prelude::*;

    #[test]
    fn benchmark() {
        ShadowedFunction::new(MmrCalculateNewPeaksFromLeafMutationMtIndices).bench();
    }
}
