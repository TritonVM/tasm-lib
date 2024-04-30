use std::collections::HashMap;

use num::One;
use rand::random;
use rand::thread_rng;
use rand::Rng;
use triton_vm::prelude::*;
use triton_vm::twenty_first::prelude::MmrMembershipProof;
use twenty_first::math::other::random_elements;
use twenty_first::util_types::mmr;
use twenty_first::util_types::mmr::mmr_accumulator::util::mmra_with_mps;
use twenty_first::util_types::mmr::mmr_accumulator::MmrAccumulator;
use twenty_first::util_types::mmr::mmr_trait::Mmr;

use crate::arithmetic::u32::isodd::Isodd;
use crate::arithmetic::u64::div2_u64::Div2U64;
use crate::arithmetic::u64::eq_u64::EqU64;
use crate::data_type::DataType;
use crate::empty_stack;
use crate::library::Library;
use crate::list::get::Get;
use crate::list::set::Set;
use crate::mmr::MAX_MMR_HEIGHT;
use crate::rust_shadowing_helper_functions;
use crate::traits::deprecated_snippet::DeprecatedSnippet;
use crate::InitVmState;
use crate::VmHasher;
use crate::DIGEST_LENGTH;

use super::leaf_index_to_mt_index_and_peak_index::MmrLeafIndexToMtIndexAndPeakIndex;

/// Calculate new MMR peaks from a leaf mutation using Merkle tree indices walk up the tree
#[derive(Debug, Default, Copy, Clone, Eq, PartialEq, Hash)]
pub struct MmrCalculateNewPeaksFromLeafMutationMtIndices;

impl MmrCalculateNewPeaksFromLeafMutationMtIndices {
    // Returns: (execution state, auth path pointer, peaks pointer)
    fn prepare_state_with_mmra(
        &self,
        start_mmr: &mut MmrAccumulator<VmHasher>,
        leaf_index: u64,
        new_leaf: Digest,
        auth_path: Vec<Digest>,
    ) -> (InitVmState, BFieldElement, BFieldElement) {
        let mut stack = empty_stack();

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
        let list_push = rust_shadowing_helper_functions::list::list_push;
        let mut memory: HashMap<BFieldElement, BFieldElement> = HashMap::default();
        rust_shadowing_helper_functions::list::list_new(peaks_pointer, &mut memory);

        for peak in start_mmr.get_peaks() {
            list_push(
                peaks_pointer,
                peak.values().to_vec(),
                &mut memory,
                DIGEST_LENGTH,
            );
        }

        rust_shadowing_helper_functions::list::list_new(auth_path_pointer, &mut memory);
        for ap_element in auth_path.iter() {
            list_push(
                auth_path_pointer,
                ap_element.values().to_vec(),
                &mut memory,
                DIGEST_LENGTH,
            );
        }

        (
            InitVmState::with_stack_and_memory(stack, memory),
            auth_path_pointer,
            peaks_pointer,
        )
    }
}

impl DeprecatedSnippet for MmrCalculateNewPeaksFromLeafMutationMtIndices {
    fn entrypoint_name(&self) -> String {
        "tasmlib_mmr_calculate_new_peaks_from_leaf_mutation".into()
    }

    fn input_field_names(&self) -> Vec<String> {
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

    fn input_types(&self) -> Vec<DataType> {
        vec![
            DataType::List(Box::new(DataType::Digest)),
            DataType::U64,
            DataType::List(Box::new(DataType::Digest)),
            DataType::Digest,
            DataType::U64,
        ]
    }

    fn output_field_names(&self) -> Vec<String> {
        vec![
            "*auth_path".to_string(),
            "leaf_index_hi".to_string(),
            "leaf_index_lo".to_string(),
        ]
    }

    fn output_types(&self) -> Vec<DataType> {
        vec![DataType::List(Box::new(DataType::Digest)), DataType::U64]
    }

    fn stack_diff(&self) -> isize {
        -8
    }

    fn function_code(&self, library: &mut Library) -> String {
        let entrypoint = self.entrypoint_name();
        let leaf_index_to_mt_index = library.import(Box::new(MmrLeafIndexToMtIndexAndPeakIndex));
        let u32_is_odd = library.import(Box::new(Isodd));
        let eq_u64 = library.import(Box::new(EqU64));
        let get = library.import(Box::new(Get::new(DataType::Digest)));
        let set = library.import(Box::new(Set::new(DataType::Digest)));
        let div_2 = library.import(Box::new(Div2U64));

        format!(
            "
            // BEFORE: _ *auth_path leaf_index_hi leaf_index_lo *peaks [digest (leaf_digest)] leaf_count_hi leaf_count_lo
            // AFTER:  _ *auth_path leaf_index_hi leaf_index_lo
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

                pop 5

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

    fn crash_conditions(&self) -> Vec<String> {
        vec![]
    }

    fn gen_input_states(&self) -> Vec<InitVmState> {
        let mmr_size: usize = 10;
        let digests: Vec<Digest> = random_elements(mmr_size);
        let leaf_index: usize = thread_rng().gen_range(0..mmr_size);
        let new_leaf: Digest = random();
        let (mut mmra, mps) = mmra_with_mps(
            mmr_size as u64,
            vec![(leaf_index as u64, digests[leaf_index as usize])],
        );
        let auth_path = mps[0].clone();
        let ret0 = self.prepare_state_with_mmra(
            &mut mmra,
            leaf_index as u64,
            new_leaf,
            auth_path.authentication_path,
        );

        vec![ret0.0]
    }

    fn common_case_input_state(&self) -> InitVmState {
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

    fn worst_case_input_state(&self) -> InitVmState {
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

    fn rust_shadowing(
        &self,
        stack: &mut Vec<BFieldElement>,
        _std_in: Vec<BFieldElement>,
        _secret_in: Vec<BFieldElement>,
        memory: &mut HashMap<BFieldElement, BFieldElement>,
    ) {
        // BEFORE: _ *auth_path leaf_index_hi leaf_index_lo *peaks [digest (leaf_digest)] leaf_count_hi leaf_count_lo
        // AFTER:  _ *auth_path leaf_index_hi leaf_index_lo
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

        let list_get = rust_shadowing_helper_functions::list::list_get;

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
            &peaks, new_leaf, leaf_count, &mmr_mp,
        );

        // Write mutated peak back to memory
        // rust_shadowing_helper_functions::list_set(peaks_pointer, index, value, memory)
        for i in 0..peaks_count {
            rust_shadowing_helper_functions::list::list_set(
                peaks_pointer,
                i as usize,
                new_peaks[i as usize].values().to_vec(),
                memory,
            );
        }

        // AFTER: _ *auth_path leaf_index_hi leaf_index_lo
        stack.push(auth_paths_pointer);
        stack.push(BFieldElement::new(leaf_index_hi as u64));
        stack.push(BFieldElement::new(leaf_index_lo as u64));
    }
}

#[cfg(test)]
mod tests {
    use itertools::Itertools;
    use twenty_first::util_types::algebraic_hasher::AlgebraicHasher;

    use crate::test_helpers::test_rust_equivalence_given_input_values_deprecated;
    use crate::test_helpers::test_rust_equivalence_multiple_deprecated;

    use super::*;

    #[test]
    fn calculate_new_peaks_from_leaf_mutation_test() {
        test_rust_equivalence_multiple_deprecated(
            &MmrCalculateNewPeaksFromLeafMutationMtIndices,
            true,
        );
    }

    #[test]
    fn mmra_leaf_mutate_test_single() {
        let digest0 = VmHasher::hash(&BFieldElement::new(4545));
        let digest1 = VmHasher::hash(&BFieldElement::new(12345));
        let mut mmr = MmrAccumulator::<Tip5>::new(vec![]);
        mmr.append(digest0);
        let expected_final_mmra = MmrAccumulator::new(vec![digest1]);
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
        let new_leaf: Digest = thread_rng().gen();

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
            let expected_final_mmra = MmrAccumulator::new(final_digests);
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
            final_mmr.mutate_leaf(&mp, new_leaf);

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
        let mmr_new_peaks = MmrCalculateNewPeaksFromLeafMutationMtIndices;
        let (init_exec_state, auth_path_pointer, peaks_pointer) =
            mmr_new_peaks.prepare_state_with_mmra(start_mmr, new_leaf_index, new_leaf, auth_path);
        let init_stack = init_exec_state.stack;
        let memory = init_exec_state.nondeterminism.ram;

        // AFTER: _ *auth_path leaf_index_hi leaf_index_lo
        let mut expected_final_stack = empty_stack();
        expected_final_stack.push(auth_path_pointer);
        expected_final_stack.push(BFieldElement::new(new_leaf_index >> 32));
        expected_final_stack.push(BFieldElement::new(new_leaf_index & u32::MAX as u64));

        let vm_output = test_rust_equivalence_given_input_values_deprecated(
            &MmrCalculateNewPeaksFromLeafMutationMtIndices,
            &init_stack,
            &[],
            memory,
            Some(&expected_final_stack),
        );

        // Find produced MMR
        let final_memory = vm_output.ram;
        let peaks_count = final_memory[&peaks_pointer].value();
        let mut produced_peaks = vec![];
        for i in 0..peaks_count {
            let peak: Digest = Digest::new(
                rust_shadowing_helper_functions::list::list_get(
                    peaks_pointer,
                    i as usize,
                    &final_memory,
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
        let auth_path_element_count = final_memory[&auth_path_pointer].value();
        let mut auth_path = vec![];
        for i in 0..auth_path_element_count {
            let auth_path_element: Digest = Digest::new(
                rust_shadowing_helper_functions::list::list_get(
                    auth_path_pointer,
                    i as usize,
                    &final_memory,
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
            mmr_mp.verify(
                &produced_mmr.get_peaks(),
                new_leaf,
                produced_mmr.count_leaves(),
            ),
            "TASM-produced authentication path must be valid"
        );

        // Extra checks because paranoia
        let mut expected_final_mmra_double_check = start_mmr.to_accumulator();
        expected_final_mmra_double_check.mutate_leaf(&mmr_mp, new_leaf);
        assert_eq!(expected_final_mmra_double_check, produced_mmr);
        assert!(mmr_mp.verify(
            &expected_final_mmra_double_check.get_peaks(),
            new_leaf,
            expected_final_mmra_double_check.count_leaves()
        ));
    }
}

#[cfg(test)]
mod benches {
    use crate::snippet_bencher::bench_and_write;

    use super::*;

    #[test]
    fn calculate_new_peaks_from_leaf_mutation_benchmark() {
        bench_and_write(MmrCalculateNewPeaksFromLeafMutationMtIndices);
    }
}
