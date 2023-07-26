use num::One;
use rand::{random, thread_rng, Rng};
use std::collections::HashMap;
use twenty_first::shared_math::b_field_element::BFieldElement;
use twenty_first::shared_math::other::random_elements;
use twenty_first::test_shared::mmr::get_rustyleveldb_ammr_from_digests;
use twenty_first::util_types::mmr::mmr_accumulator::MmrAccumulator;
use twenty_first::util_types::mmr::mmr_trait::Mmr;
use twenty_first::util_types::mmr::{self, mmr_membership_proof::MmrMembershipProof};

use super::leaf_index_to_mt_index::MmrLeafIndexToMtIndexAndPeakIndex;
use crate::arithmetic::u32::is_odd::U32IsOdd;
use crate::arithmetic::u64::div2_u64::Div2U64;
use crate::arithmetic::u64::eq_u64::EqU64;
use crate::library::Library;
use crate::list::safe_u32::get::SafeGet;
use crate::list::safe_u32::set::SafeSet;
use crate::list::unsafe_u32::get::UnsafeGet;
use crate::list::unsafe_u32::set::UnsafeSet;
use crate::list::ListType;
use crate::mmr::MAX_MMR_HEIGHT;
use crate::snippet::{DataType, Snippet};
use crate::{
    get_init_tvm_stack, rust_shadowing_helper_functions, Digest, ExecutionState, VmHasher,
    DIGEST_LENGTH,
};

/// Calculate new MMR peaks from a leaf mutation using Merkle tree indices walk up the tree
#[derive(Clone, Debug)]
pub struct MmrCalculateNewPeaksFromLeafMutationMtIndices {
    pub list_type: ListType,
}

impl MmrCalculateNewPeaksFromLeafMutationMtIndices {
    // Returns: (execution state, auth path pointer, peaks pointer)
    fn prepare_state_with_mmra(
        &self,
        start_mmr: &mut MmrAccumulator<VmHasher>,
        leaf_index: u64,
        new_leaf: Digest,
        auth_path: Vec<Digest>,
    ) -> (ExecutionState, BFieldElement, BFieldElement) {
        let mut stack = get_init_tvm_stack();

        // We assume that the auth paths can safely be stored in memory on this address
        let auth_path_pointer = BFieldElement::new((MAX_MMR_HEIGHT * DIGEST_LENGTH + 2) as u64);
        stack.push(auth_path_pointer);

        stack.push(BFieldElement::new(leaf_index >> 32));
        stack.push(BFieldElement::new(leaf_index & u32::MAX as u64));

        let peaks_pointer = BFieldElement::one();
        stack.push(peaks_pointer);

        // push digests such that element 0 of digest is on top of stack
        for value in new_leaf.values().iter().rev() {
            stack.push(*value);
        }

        let leaf_count: u64 = start_mmr.count_leaves();
        stack.push(BFieldElement::new(leaf_count >> 32));
        stack.push(BFieldElement::new(leaf_count & u32::MAX as u64));

        // Initialize memory
        let list_push = match self.list_type {
            ListType::Safe => rust_shadowing_helper_functions::safe_list::safe_list_push,
            ListType::Unsafe => rust_shadowing_helper_functions::unsafe_list::unsafe_list_push,
        };
        let mut memory: HashMap<BFieldElement, BFieldElement> = HashMap::default();
        match self.list_type {
            ListType::Safe => {
                rust_shadowing_helper_functions::safe_list::safe_list_new(
                    peaks_pointer,
                    MAX_MMR_HEIGHT as u32,
                    &mut memory,
                );
            }
            ListType::Unsafe => {
                rust_shadowing_helper_functions::unsafe_list::unsafe_list_new(
                    peaks_pointer,
                    &mut memory,
                );
            }
        }
        for peak in start_mmr.get_peaks() {
            list_push(
                peaks_pointer,
                peak.values().to_vec(),
                &mut memory,
                DIGEST_LENGTH,
            );
        }

        match self.list_type {
            ListType::Safe => {
                rust_shadowing_helper_functions::safe_list::safe_list_new(
                    auth_path_pointer,
                    MAX_MMR_HEIGHT as u32,
                    &mut memory,
                );
            }
            ListType::Unsafe => {
                rust_shadowing_helper_functions::unsafe_list::unsafe_list_new(
                    auth_path_pointer,
                    &mut memory,
                );
            }
        }
        for ap_element in auth_path.iter() {
            list_push(
                auth_path_pointer,
                ap_element.values().to_vec(),
                &mut memory,
                DIGEST_LENGTH,
            );
        }

        (
            ExecutionState::with_stack_and_memory(
                stack,
                memory,
                2 * (MAX_MMR_HEIGHT * DIGEST_LENGTH + 1),
            ),
            auth_path_pointer,
            peaks_pointer,
        )
    }
}

impl Snippet for MmrCalculateNewPeaksFromLeafMutationMtIndices {
    fn inputs(&self) -> Vec<String> {
        vec![
            "*auth_path".to_string(),
            "leaf_index_hi".to_string(),
            "leaf_index_lo".to_string(),
            "*peaks".to_string(),
            "digest_4".to_string(),
            "digest_3".to_string(),
            "digest_2".to_string(),
            "digest_1".to_string(),
            "digest_0".to_string(),
            "leaf_count_hi".to_string(),
            "leaf_count_lo".to_string(),
        ]
    }

    fn outputs(&self) -> Vec<String> {
        vec![
            "*auth_path".to_string(),
            "leaf_index_hi".to_string(),
            "leaf_index_lo".to_string(),
        ]
    }

    fn input_types(&self) -> Vec<crate::snippet::DataType> {
        vec![
            DataType::List(Box::new(DataType::Digest)),
            DataType::U64,
            DataType::List(Box::new(DataType::Digest)),
            DataType::Digest,
            DataType::U64,
        ]
    }

    fn output_types(&self) -> Vec<crate::snippet::DataType> {
        vec![DataType::List(Box::new(DataType::Digest)), DataType::U64]
    }

    fn crash_conditions(&self) -> Vec<String> {
        vec![]
    }

    fn gen_input_states(&self) -> Vec<ExecutionState> {
        let mmr_size: usize = 10;
        let digests: Vec<Digest> = random_elements(mmr_size);
        let leaf_index: usize = thread_rng().gen_range(0..mmr_size);
        let new_leaf: Digest = random();
        let ammr = get_rustyleveldb_ammr_from_digests::<VmHasher>(digests);
        let mut mmra = ammr.to_accumulator();
        let auth_path = ammr.prove_membership(leaf_index as u64);
        let ret0 = self.prepare_state_with_mmra(
            &mut mmra,
            leaf_index as u64,
            new_leaf,
            auth_path.0.authentication_path,
        );

        vec![ret0.0]
    }

    fn stack_diff(&self) -> isize {
        -8
    }

    fn entrypoint(&self) -> String {
        format!(
            "tasm_mmr_calculate_new_peaks_from_leaf_mutation_{}",
            self.list_type
        )
    }

    fn function_code(&self, library: &mut Library) -> String {
        let entrypoint = self.entrypoint();
        let leaf_index_to_mt_index = library.import(Box::new(MmrLeafIndexToMtIndexAndPeakIndex));
        let u32_is_odd = library.import(Box::new(U32IsOdd));
        let eq_u64 = library.import(Box::new(EqU64));
        let get = match self.list_type {
            ListType::Safe => library.import(Box::new(SafeGet(DataType::Digest))),
            ListType::Unsafe => library.import(Box::new(UnsafeGet(DataType::Digest))),
        };
        let set = match self.list_type {
            ListType::Safe => library.import(Box::new(SafeSet(DataType::Digest))),
            ListType::Unsafe => library.import(Box::new(UnsafeSet(DataType::Digest))),
        };
        let div_2 = library.import(Box::new(Div2U64));

        format!(
            "
            // BEFORE: _ *auth_path leaf_index_hi leaf_index_lo *peaks [digest (leaf_digest)] leaf_count_hi leaf_count_lo
            // AFTER: _ *auth_path leaf_index_hi leaf_index_lo
            {entrypoint}:
                dup 9 dup 9
                call {leaf_index_to_mt_index}
                // stack: _ *auth_path leaf_index_hi leaf_index_lo *peaks [digest (leaf_digest)] mt_index_hi mt_index_lo peak_index

                push 0
                /// stack: _ *auth_path leaf_index_hi leaf_index_lo *peaks [digest (leaf_digest)] mt_index_hi mt_index_lo peak_index i

                swap 8 swap 4 swap 1 swap 7 swap 3 swap 6 swap 2 swap 5 swap 1
                /// stack: _ *auth_path leaf_index_hi leaf_index_lo *peaks i peak_index mt_index_hi mt_index_lo [digest (leaf_digest)]
                // rename: leaf_digest -> acc_hash

                call {entrypoint}_while
                // _ _ *auth_path leaf_index_hi leaf_index_lo *peaks i peak_index mt_index_hi mt_index_lo [digest (acc_hash)]

                dup 9 dup 8
                // _ *auth_path leaf_index_hi leaf_index_lo *peaks i peak_index mt_index_hi mt_index_lo [digest [digest (acc_hash)] *peaks peak_index

                call {set}
                // _ *auth_path leaf_index_hi leaf_index_lo *peaks i peak_index mt_index_hi mt_index_lo

                pop pop pop pop pop

                return

            // Note that this while loop is the same as one in `verify_from_memory`
            // BEFORE/AFTER: _ *auth_path leaf_index_hi leaf_index_lo *peaks i peak_index mt_index_hi mt_index_lo [digest (leaf_digest)]
                {entrypoint}_while:
                    dup 6 dup 6 push 0 push 1 call {eq_u64}
                    // _ *auth_path leaf_index_hi leaf_index_lo *peaks i peak_index mt_index_hi mt_index_lo [digest (leaf_digest)] (mt_index == 1)

                    skiz return
                    // _ *auth_path leaf_index_hi leaf_index_lo *peaks i peak_index mt_index_hi mt_index_lo [digest (leaf_digest)]

                    // declare `ap_element = auth_path[i]`
                    dup 12 dup 9 call {get}
                    // _ *auth_path leaf_index_hi leaf_index_lo *peaks i peak_index mt_index_hi mt_index_lo [digest (leaf_digest)] [digest (ap_element)]

                    dup 10 call {u32_is_odd} push 0 eq
                    // _ *auth_path leaf_index_hi leaf_index_lo *peaks i peak_index mt_index_hi mt_index_lo [digest (acc_hash)] [digest (ap_element)] (mt_index % 2 == 0)

                    skiz call {entrypoint}_swap_digests
                    // _ *auth_path leaf_index_hi leaf_index_lo *peaks i peak_index mt_index_hi mt_index_lo [digest (right_node)] [digest (left_node)]

                    hash
                    pop pop pop pop pop
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
                {entrypoint}_swap_digests:
                // _ *auth_path leaf_index_hi leaf_index_lo *peaks i peak_index mt_index_hi mt_index_lo [digest (acc_hash)] [digest (ap_element)]
                        swap 4 swap 9 swap 4
                        swap 3 swap 8 swap 3
                        swap 2 swap 7 swap 2
                        swap 1 swap 6 swap 1
                        swap 5
                        // _ *auth_path leaf_index_hi leaf_index_lo *peaks i peak_index mt_index_hi mt_index_lo [digest (ap_element)] [digest (acc_hash)]

                        return
            "

        )
    }

    fn rust_shadowing(
        &self,
        stack: &mut Vec<BFieldElement>,
        _std_in: Vec<BFieldElement>,
        _secret_in: Vec<BFieldElement>,
        memory: &mut HashMap<BFieldElement, BFieldElement>,
    ) {
        // BEFORE: _ *auth_path leaf_index_hi leaf_index_lo *peaks [digest (leaf_digest)] leaf_count_hi leaf_count_lo
        // AFTER: _ *auth_path leaf_index_hi leaf_index_lo
        let leaf_count_lo: u32 = stack.pop().unwrap().try_into().unwrap();
        let leaf_count_hi: u32 = stack.pop().unwrap().try_into().unwrap();
        let leaf_count: u64 = ((leaf_count_hi as u64) << 32) + leaf_count_lo as u64;

        let mut new_leaf_digest_values = [BFieldElement::new(0); DIGEST_LENGTH];
        for elem in new_leaf_digest_values.iter_mut() {
            *elem = stack.pop().unwrap();
        }

        let new_leaf = Digest::new(new_leaf_digest_values);

        let peaks_pointer = stack.pop().unwrap();

        let leaf_index_lo: u32 = stack.pop().unwrap().try_into().unwrap();
        let leaf_index_hi: u32 = stack.pop().unwrap().try_into().unwrap();
        let leaf_index: u64 = ((leaf_index_hi as u64) << 32) + leaf_index_lo as u64;

        let auth_paths_pointer = stack.pop().unwrap();

        let list_get = match self.list_type {
            ListType::Safe => rust_shadowing_helper_functions::safe_list::safe_list_get,
            ListType::Unsafe => rust_shadowing_helper_functions::unsafe_list::unsafe_list_get,
        };
        let list_set = match self.list_type {
            ListType::Safe => rust_shadowing_helper_functions::safe_list::safe_list_set,
            ListType::Unsafe => rust_shadowing_helper_functions::unsafe_list::unsafe_list_set,
        };

        let peaks_count: u64 = memory[&peaks_pointer].value();
        let mut peaks: Vec<Digest> = vec![];
        for i in 0..peaks_count {
            let digest = Digest::new(
                list_get(peaks_pointer, i as usize, memory, DIGEST_LENGTH)
                    .try_into()
                    .unwrap(),
            );
            peaks.push(digest);
        }

        let auth_path_length = memory[&auth_paths_pointer].value();
        let mut auth_path: Vec<Digest> = vec![];
        for i in 0..auth_path_length {
            let digest = Digest::new(
                list_get(auth_paths_pointer, i as usize, memory, DIGEST_LENGTH)
                    .try_into()
                    .unwrap(),
            );
            auth_path.push(digest);
        }

        let mmr_mp = MmrMembershipProof::new(leaf_index, auth_path);
        let new_peaks = mmr::shared_basic::calculate_new_peaks_from_leaf_mutation::<VmHasher>(
            &peaks, &new_leaf, leaf_count, &mmr_mp,
        );

        // Write mutated peak back to memory
        // rust_shadowing_helper_functions::list_set(peaks_pointer, index, value, memory)
        for i in 0..peaks_count {
            list_set(
                peaks_pointer,
                i as usize,
                new_peaks[i as usize].values().to_vec(),
                memory,
                DIGEST_LENGTH,
            );
        }

        // AFTER: _ *auth_path leaf_index_hi leaf_index_lo
        stack.push(auth_paths_pointer);
        stack.push(BFieldElement::new(leaf_index_hi as u64));
        stack.push(BFieldElement::new(leaf_index_lo as u64));
    }

    fn common_case_input_state(&self) -> ExecutionState {
        let mmr_leaf_count_log2 = 31u64;
        let mmr_size = 1 << mmr_leaf_count_log2;
        let peaks: Vec<Digest> = random_elements(mmr_leaf_count_log2 as usize);
        let mut mmra = MmrAccumulator::<VmHasher>::init(peaks, mmr_size - 1);
        let inserted_leaf: Digest = random();
        let leaf_after_mutation: Digest = random();
        let auth_path = mmra.append(inserted_leaf).authentication_path;
        self.prepare_state_with_mmra(
            &mut mmra,
            mmr_size - mmr_leaf_count_log2 - 1,
            leaf_after_mutation,
            auth_path,
        )
        .0
    }

    fn worst_case_input_state(&self) -> ExecutionState {
        let mmr_leaf_count_log2 = 62u64;
        let mmr_size = 1 << mmr_leaf_count_log2;
        let peaks: Vec<Digest> = random_elements(mmr_leaf_count_log2 as usize);
        let mut mmra = MmrAccumulator::<VmHasher>::init(peaks, mmr_size - 1);
        let inserted_leaf: Digest = random();
        let leaf_after_mutation: Digest = random();
        let auth_path = mmra.append(inserted_leaf).authentication_path;
        self.prepare_state_with_mmra(
            &mut mmra,
            mmr_size - mmr_leaf_count_log2 - 1,
            leaf_after_mutation,
            auth_path,
        )
        .0
    }
}

#[cfg(test)]
mod tests {
    use rand::{thread_rng, Rng};
    use twenty_first::shared_math::b_field_element::BFieldElement;
    use twenty_first::shared_math::other::random_elements;
    use twenty_first::test_shared::mmr::get_empty_rustyleveldb_ammr;
    use twenty_first::util_types::algebraic_hasher::AlgebraicHasher;
    use twenty_first::util_types::mmr::archival_mmr::ArchivalMmr;
    use twenty_first::util_types::mmr::mmr_accumulator::MmrAccumulator;
    use twenty_first::util_types::mmr::mmr_membership_proof::MmrMembershipProof;
    use twenty_first::util_types::mmr::mmr_trait::Mmr;

    use crate::get_init_tvm_stack;
    use crate::mmr::MAX_MMR_HEIGHT;

    use crate::test_helpers::{
        test_rust_equivalence_given_input_values, test_rust_equivalence_multiple,
    };

    use super::*;

    #[test]
    fn calculate_new_peaks_from_leaf_mutation_test_unsafe_lists() {
        test_rust_equivalence_multiple(
            &MmrCalculateNewPeaksFromLeafMutationMtIndices {
                list_type: ListType::Unsafe,
            },
            true,
        );
    }

    #[test]
    fn calculate_new_peaks_from_leaf_mutation_test_safe_lists() {
        test_rust_equivalence_multiple(
            &MmrCalculateNewPeaksFromLeafMutationMtIndices {
                list_type: ListType::Safe,
            },
            true,
        );
    }

    #[test]
    fn mmra_leaf_mutate_test_single() {
        let digest0 = VmHasher::hash(&BFieldElement::new(4545));
        let digest1 = VmHasher::hash(&BFieldElement::new(12345));
        let mut archival_mmr = get_empty_rustyleveldb_ammr();
        archival_mmr.append(digest0);
        let expected_final_mmra = MmrAccumulator::new(vec![digest1]);
        let mutated_index = 0;
        prop_calculate_new_peaks_from_leaf_mutation(
            &mut archival_mmr.to_accumulator(),
            digest1,
            mutated_index,
            expected_final_mmra,
            vec![],
        );
    }

    #[test]
    fn mmra_leaf_mutate_test_two_leafs() {
        let digest0 = VmHasher::hash(&BFieldElement::new(4545));
        let digest1 = VmHasher::hash(&BFieldElement::new(12345));
        let digest2 = VmHasher::hash(&BFieldElement::new(55555555));
        let mut archival_mmr = get_empty_rustyleveldb_ammr();
        archival_mmr.append(digest0);
        archival_mmr.append(digest1);
        let expected_final_mmra = MmrAccumulator::new(vec![digest2, digest1]);
        let mutated_index = 0;
        let auth_path = archival_mmr
            .prove_membership(mutated_index)
            .0
            .authentication_path;
        prop_calculate_new_peaks_from_leaf_mutation(
            &mut archival_mmr.to_accumulator(),
            digest2,
            mutated_index,
            expected_final_mmra,
            auth_path,
        );
    }

    #[test]
    fn mmra_leaf_mutate_test_three_leafs() {
        let leaf_count = 3;
        let init_leaf_digests: Vec<Digest> = random_elements(leaf_count);
        let new_leaf: Digest = thread_rng().gen();
        let archival_mmr: ArchivalMmr<VmHasher, _> =
            get_rustyleveldb_ammr_from_digests(init_leaf_digests.clone());

        for mutated_index in 0..leaf_count {
            let auth_path = archival_mmr
                .prove_membership(mutated_index as u64)
                .0
                .authentication_path;
            let mut final_digests = init_leaf_digests.clone();
            final_digests[mutated_index] = new_leaf;
            let expected_final_mmra = MmrAccumulator::new(final_digests);
            prop_calculate_new_peaks_from_leaf_mutation(
                &mut archival_mmr.to_accumulator(),
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
            println!("leaf_count = {leaf_count}");
            let init_leaf_digests: Vec<Digest> = random_elements(leaf_count);
            let new_leaf: Digest = thread_rng().gen();
            let archival_mmr: ArchivalMmr<VmHasher, _> =
                get_rustyleveldb_ammr_from_digests(init_leaf_digests.clone());

            for mutated_index in 0..leaf_count {
                let auth_path = archival_mmr
                    .prove_membership(mutated_index as u64)
                    .0
                    .authentication_path;
                let mut final_digests = init_leaf_digests.clone();
                final_digests[mutated_index] = new_leaf;
                let expected_final_mmra = MmrAccumulator::new(final_digests);
                prop_calculate_new_peaks_from_leaf_mutation(
                    &mut archival_mmr.to_accumulator(),
                    new_leaf,
                    mutated_index as u64,
                    expected_final_mmra,
                    auth_path,
                );
            }
        }
    }

    #[test]
    fn mmra_leaf_mutate_test_other_leaf_sizes() {
        for leaf_count in [511, 1023] {
            println!("leaf_count = {leaf_count}");
            let init_leaf_digests: Vec<Digest> = random_elements(leaf_count);
            let new_leaf: Digest = thread_rng().gen();
            let archival_mmr: ArchivalMmr<VmHasher, _> =
                get_rustyleveldb_ammr_from_digests(init_leaf_digests.clone());

            for mutated_index in [0, leaf_count - 100, leaf_count - 2, leaf_count - 1] {
                let auth_path = archival_mmr
                    .prove_membership(mutated_index as u64)
                    .0
                    .authentication_path;
                let mut final_digests = init_leaf_digests.clone();
                final_digests[mutated_index] = new_leaf;
                let expected_final_mmra = MmrAccumulator::new(final_digests);
                prop_calculate_new_peaks_from_leaf_mutation(
                    &mut archival_mmr.to_accumulator(),
                    new_leaf,
                    mutated_index as u64,
                    expected_final_mmra,
                    auth_path,
                );
            }
        }
    }

    #[test]
    fn mmra_leaf_mutate_big() {
        for log_sizes in [15u64, 20, 25, 32, 35, 40, 45, 50, 55, 60, 62, 63] {
            println!("log_sizes = {log_sizes}");
            let init_peak_digests: Vec<Digest> = random_elements(log_sizes as usize);
            let new_leaf: Digest = thread_rng().gen();
            let mut init_mmr: MmrAccumulator<VmHasher> =
                MmrAccumulator::init(init_peak_digests.clone(), (1u64 << log_sizes) - 1);

            let mut final_peaks = init_peak_digests.clone();
            final_peaks[log_sizes as usize - 1] = new_leaf;
            let expected_final_mmra: MmrAccumulator<VmHasher> =
                MmrAccumulator::init(final_peaks, (1u64 << log_sizes) - 1);
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
            let new_leaf: Digest = thread_rng().gen();
            let before_insertion_mmr: MmrAccumulator<VmHasher> =
                MmrAccumulator::init(init_peak_digests.clone(), (1u64 << log_size) - 1);

            // Insert a leaf such that a very long (log_size long) auth path is returned
            let mut init_mmr = before_insertion_mmr.clone();
            let mp = init_mmr.append(thread_rng().gen());

            let mut final_mmr = init_mmr.clone();
            final_mmr.mutate_leaf(&mp, &new_leaf);

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
        start_mmr: &mut MmrAccumulator<VmHasher>,
        new_leaf: Digest,
        new_leaf_index: u64,
        expected_mmr: MmrAccumulator<VmHasher>,
        auth_path: Vec<Digest>,
    ) {
        let implementation_with_unsafe_lists = MmrCalculateNewPeaksFromLeafMutationMtIndices {
            list_type: ListType::Unsafe,
        };
        let (init_exec_state, auth_path_pointer, peaks_pointer) = implementation_with_unsafe_lists
            .prepare_state_with_mmra(start_mmr, new_leaf_index, new_leaf, auth_path);
        let init_stack = init_exec_state.stack;
        let mut memory = init_exec_state.memory;

        // AFTER: _ *auth_path leaf_index_hi leaf_index_lo
        let mut expected_final_stack = get_init_tvm_stack();
        expected_final_stack.push(auth_path_pointer);
        expected_final_stack.push(BFieldElement::new(new_leaf_index >> 32));
        expected_final_stack.push(BFieldElement::new(new_leaf_index & u32::MAX as u64));

        test_rust_equivalence_given_input_values(
            &MmrCalculateNewPeaksFromLeafMutationMtIndices {
                list_type: ListType::Unsafe,
            },
            &init_stack,
            &[],
            &[],
            &mut memory,
            MAX_MMR_HEIGHT * DIGEST_LENGTH + 1, // assume that 64 digests are allocated in memory when code starts to run
            Some(&expected_final_stack),
        );

        // Find produced MMR
        let peaks_count = memory[&peaks_pointer].value();
        let mut produced_peaks = vec![];
        for i in 0..peaks_count {
            let peak: Digest = Digest::new(
                rust_shadowing_helper_functions::unsafe_list::unsafe_list_get(
                    peaks_pointer,
                    i as usize,
                    &memory,
                    DIGEST_LENGTH,
                )
                .try_into()
                .unwrap(),
            );
            produced_peaks.push(peak);
        }

        let produced_mmr =
            MmrAccumulator::<VmHasher>::init(produced_peaks, start_mmr.count_leaves());

        // Verify that both code paths produce the same MMR
        assert_eq!(expected_mmr, produced_mmr);

        // Verify that auth paths is still value
        let auth_path_element_count = memory[&auth_path_pointer].value();
        let mut auth_path = vec![];
        for i in 0..auth_path_element_count {
            let auth_path_element: Digest = Digest::new(
                rust_shadowing_helper_functions::unsafe_list::unsafe_list_get(
                    auth_path_pointer,
                    i as usize,
                    &memory,
                    DIGEST_LENGTH,
                )
                .try_into()
                .unwrap(),
            );
            auth_path.push(auth_path_element);
        }

        let mmr_mp = MmrMembershipProof::<VmHasher> {
            leaf_index: new_leaf_index,
            authentication_path: auth_path,
            _hasher: std::marker::PhantomData,
        };
        assert!(
            mmr_mp
                .verify(
                    &produced_mmr.get_peaks(),
                    &new_leaf,
                    produced_mmr.count_leaves(),
                )
                .0,
            "TASM-produced authentication path must be valid"
        );

        // Extra checks because paranoia
        let mut expected_final_mmra_double_check = start_mmr.to_accumulator();
        expected_final_mmra_double_check.mutate_leaf(&mmr_mp, &new_leaf);
        assert_eq!(expected_final_mmra_double_check, produced_mmr);
        assert!(
            mmr_mp
                .verify(
                    &expected_final_mmra_double_check.get_peaks(),
                    &new_leaf,
                    expected_final_mmra_double_check.count_leaves()
                )
                .0
        );
    }
}

#[cfg(test)]
mod benches {
    use super::*;
    use crate::snippet_bencher::bench_and_write;

    #[test]
    fn calculate_new_peaks_from_leaf_mutation_benchmark_unsafe_lists() {
        bench_and_write(MmrCalculateNewPeaksFromLeafMutationMtIndices {
            list_type: ListType::Unsafe,
        });
    }

    #[test]
    fn calculate_new_peaks_from_leaf_mutation_benchmark_safe_lists() {
        bench_and_write(MmrCalculateNewPeaksFromLeafMutationMtIndices {
            list_type: ListType::Safe,
        });
    }
}
