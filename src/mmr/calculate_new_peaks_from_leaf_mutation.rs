use std::collections::HashMap;
use twenty_first::shared_math::b_field_element::BFieldElement;
use twenty_first::shared_math::rescue_prime_digest::Digest;
use twenty_first::shared_math::rescue_prime_regular::{RescuePrimeRegular, DIGEST_LENGTH};
use twenty_first::util_types::mmr::{self, mmr_membership_proof::MmrMembershipProof};

use super::leaf_index_to_mt_index::MmrLeafIndexToMtIndexAndPeakIndex;
use crate::arithmetic::u32::is_odd::U32IsOdd;
use crate::arithmetic::u64::div2_u64::Div2U64;
use crate::arithmetic::u64::eq_u64::EqU64;
use crate::library::Library;
use crate::list::u32::get::Get;
use crate::list::u32::set::Set;
use crate::rust_shadowing_helper_functions;
use crate::snippet::Snippet;

/// Calculate new MMR peaks from a leaf mutation using Merkle tree indices walk up the tree
pub struct MmrCalculateNewPeaksFromLeafMutationMtIndices();

impl Snippet for MmrCalculateNewPeaksFromLeafMutationMtIndices {
    fn stack_diff() -> isize {
        -11
    }

    fn entrypoint() -> &'static str {
        "calculate_new_peaks_from_leaf_mutation"
    }

    fn function_body(library: &mut Library) -> String {
        let entrypoint = Self::entrypoint();
        let leaf_index_to_mt_index = library.import::<MmrLeafIndexToMtIndexAndPeakIndex>();
        let u32_is_odd = library.import::<U32IsOdd>();
        let u64_eq = library.import::<EqU64>();
        let get = library.import::<Get<DIGEST_LENGTH>>();
        let set = library.import::<Set<DIGEST_LENGTH>>();
        let div_2 = library.import::<Div2U64>();

        format!(
            "
            // BEFORE: _ *peaks *auth_paths [digest (new_leaf)] leaf_count_hi leaf_count_lo leaf_index_hi leaf_index_lo
            // AFTER: _
            {entrypoint}:
                call {leaf_index_to_mt_index}
                // stack: _ *peaks *auth_paths [digest (new_leaf)] mt_index_hi mt_index_lo peak_index

                push 0
                // stack: _ *peaks *auth_paths [digest (new_leaf)] mt_index_hi mt_index_lo peak_index i

                dup8 dup8 dup8 dup8 dup8
                // stack: _ *peaks *auth_paths [digest (new_leaf)] mt_index_hi mt_index_lo peak_index i [digest (new_leaf)]

                call {entrypoint}_while
                // _ *peaks *auth_paths [digest (new_leaf)] mt_index_hi mt_index_lo peak_index i [digest (acc_hash)]

                dup15 dup7
                // _ *peaks *auth_paths [digest (new_leaf)] mt_index_hi mt_index_lo peak_index i [digest (acc_hash)] *peaks peak_index

                call {set}
                // _ *peaks *auth_paths [digest (new_leaf)] mt_index_hi mt_index_lo peak_index i

                pop pop pop pop pop pop pop pop pop pop pop
                // _

                return

            // Note that this while loop is the same as one in `verify_from_memory`
            // start/end stack: _ *peaks *auth_paths [digest (new_leaf)] mt_index_hi mt_index_lo peak_index i [digest (acc_hash)]
            {entrypoint}_while:
                dup8 dup8 push 0 push 1 call {u64_eq}
                // _ *peaks *auth_paths [digest (new_leaf)] mt_index_hi mt_index_lo peak_index i [digest (acc_hash)] (mt_index == 1)
                skiz return
                // _ *peaks *auth_paths [digest (new_leaf)] mt_index_hi mt_index_lo peak_index i [digest (acc_hash)]

                dup14 dup6 call {get}
                // _ *peaks *auth_paths [digest (new_leaf)] mt_index_hi mt_index_lo peak_index i [digest (acc_hash)] [digest (ap_element)]

                dup12 call {u32_is_odd} push 0 eq
                // _ *peaks *auth_paths [digest (new_leaf)] mt_index_hi mt_index_lo peak_index i [digest (acc_hash)] [digest (ap_element)] (mt_index % 2 == 0)

                skiz call {entrypoint}_swap_digests
                // _ *peaks *auth_paths [digest (new_leaf)] mt_index_hi mt_index_lo peak_index i [digest (right_child)] [digest (left_child)]

                hash
                pop pop pop pop pop
                // _ *peaks *auth_paths [digest (new_leaf)] mt_index_hi mt_index_lo peak_index i [digest (acc_hash)]

                // i -> i + 1
                swap5 push 1 add swap5
                // _ *peaks *auth_paths [digest (new_leaf)] mt_index_hi mt_index_lo peak_index (i + 1) [digest (acc_hash)]

                swap8 swap1 swap7
                // _ *peaks *auth_paths [digest (new_leaf)] acc_hash_0 acc_hash_1 peak_index (i + 1) acc_hash_4 acc_hash_3 acc_hash_2 mt_index_hi mt_index_lo

                call {div_2}
                // _ *peaks *auth_paths [digest (new_leaf)] acc_hash_0 acc_hash_1 peak_index (i + 1) acc_hash_4 acc_hash_3 acc_hash_2 (mt_index / 2)_hi (mt_index / 2)_lo

                swap7 swap1 swap8
                // _ *peaks *auth_paths [digest (new_leaf)] (mt_index / 2)_hi (mt_index / 2)_lo peak_index (i + 1) acc_hash_4 acc_hash_3 acc_hash_2 acc_hash_1 acc_hash_0

                recurse

            // purpose: swap the two digests `i` (node with `acc_hash`) is left child
            {entrypoint}_swap_digests:
            // _ *peaks *auth_paths [digest (new_leaf)] mt_index_hi mt_index_lo peak_index i [digest (acc_hash)] [digest (ap_element)]
                swap4 swap9 swap4
                swap3 swap8 swap3
                swap2 swap7 swap2
                swap1 swap6 swap1
                swap5
                // _ *peaks *auth_paths [digest (new_leaf)] mt_index_hi mt_index_lo peak_index i [digest (ap_element)] [digest (acc_hash)]

                return
            "

        )
    }

    fn rust_shadowing(
        stack: &mut Vec<BFieldElement>,
        _std_in: Vec<BFieldElement>,
        _secret_in: Vec<BFieldElement>,
        memory: &mut HashMap<BFieldElement, BFieldElement>,
    ) {
        // init stack: _ *peaks *auth_paths [digest (new_leaf)] leaf_count_hi leaf_count_lo leaf_index_hi leaf_index_lo
        let leaf_index_lo: u32 = stack.pop().unwrap().try_into().unwrap();
        let leaf_index_hi: u32 = stack.pop().unwrap().try_into().unwrap();
        let leaf_index: u64 = ((leaf_index_hi as u64) << 32) + leaf_index_lo as u64;

        let leaf_count_lo: u32 = stack.pop().unwrap().try_into().unwrap();
        let leaf_count_hi: u32 = stack.pop().unwrap().try_into().unwrap();
        let leaf_count: u64 = ((leaf_count_hi as u64) << 32) + leaf_count_lo as u64;

        let mut new_leaf_digest_values = [BFieldElement::new(0); DIGEST_LENGTH];
        for elem in new_leaf_digest_values.iter_mut() {
            *elem = stack.pop().unwrap();
        }

        let new_leaf = Digest::new(new_leaf_digest_values);

        let auth_paths_pointer = stack.pop().unwrap();
        let peaks_pointer = stack.pop().unwrap();

        let peaks_count: u64 = memory[&peaks_pointer].value();
        let mut peaks: Vec<Digest> = vec![];
        for i in 0..peaks_count {
            let digest = Digest::new(rust_shadowing_helper_functions::list_read(
                peaks_pointer,
                i as usize,
                memory,
            ));
            peaks.push(digest);
        }

        let auth_path_length = memory[&auth_paths_pointer].value();
        let mut auth_path: Vec<Digest> = vec![];
        for i in 0..auth_path_length {
            let digest = Digest::new(rust_shadowing_helper_functions::list_read(
                auth_paths_pointer,
                i as usize,
                memory,
            ));
            auth_path.push(digest);
        }

        let mmr_mp = MmrMembershipProof::new(leaf_index as u128, auth_path);
        let new_peaks = mmr::shared::calculate_new_peaks_from_leaf_mutation::<RescuePrimeRegular>(
            &peaks,
            &new_leaf,
            leaf_count as u128,
            &mmr_mp,
        )
        .unwrap();

        // Write mutated peak back to memory
        // rust_shadowing_helper_functions::list_set(peaks_pointer, index, value, memory)
        for i in 0..peaks_count {
            rust_shadowing_helper_functions::list_set(
                peaks_pointer,
                i as usize,
                new_peaks[i as usize].values(),
                memory,
            );
        }
    }
}

#[cfg(test)]
mod leaf_mutation_tests {
    use num::Zero;
    use rand::{thread_rng, Rng};
    use twenty_first::shared_math::b_field_element::BFieldElement;
    use twenty_first::shared_math::other::random_elements;
    use twenty_first::test_shared::mmr::{get_archival_mmr_from_digests, get_empty_archival_mmr};
    use twenty_first::util_types::algebraic_hasher::AlgebraicHasher;
    use twenty_first::util_types::mmr::archival_mmr::ArchivalMmr;
    use twenty_first::util_types::mmr::mmr_accumulator::MmrAccumulator;
    use twenty_first::util_types::mmr::mmr_membership_proof::MmrMembershipProof;
    use twenty_first::util_types::mmr::mmr_trait::Mmr;

    use crate::get_init_tvm_stack;
    use crate::mmr::calculate_new_peaks_from_append::MAX_MMR_HEIGHT;
    use crate::test_helpers::rust_tasm_equivalence_prop;

    use super::*;

    #[test]
    fn mmra_leaf_mutate_test_single() {
        type H = RescuePrimeRegular;
        let digest0 = H::hash(&BFieldElement::new(4545));
        let digest1 = H::hash(&BFieldElement::new(12345));
        let mut archival_mmr: ArchivalMmr<H> = get_empty_archival_mmr();
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
        type H = RescuePrimeRegular;
        let digest0 = H::hash(&BFieldElement::new(4545));
        let digest1 = H::hash(&BFieldElement::new(12345));
        let digest2 = H::hash(&BFieldElement::new(55555555));
        let mut archival_mmr: ArchivalMmr<H> = get_empty_archival_mmr();
        archival_mmr.append(digest0);
        archival_mmr.append(digest1);
        let expected_final_mmra = MmrAccumulator::new(vec![digest2, digest1]);
        let mutated_index = 0;
        let auth_path = archival_mmr
            .prove_membership(mutated_index as u128)
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
        type H = RescuePrimeRegular;
        let leaf_count = 3;
        let init_leaf_digests: Vec<Digest> = random_elements(leaf_count);
        let new_leaf: Digest = thread_rng().gen();
        let mut archival_mmr: ArchivalMmr<H> =
            get_archival_mmr_from_digests(init_leaf_digests.clone());

        for mutated_index in 0..leaf_count {
            let auth_path = archival_mmr
                .prove_membership(mutated_index as u128)
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
        type H = RescuePrimeRegular;

        for leaf_count in 1..30 {
            println!("leaf_count = {leaf_count}");
            let init_leaf_digests: Vec<Digest> = random_elements(leaf_count);
            let new_leaf: Digest = thread_rng().gen();
            let mut archival_mmr: ArchivalMmr<H> =
                get_archival_mmr_from_digests(init_leaf_digests.clone());

            for mutated_index in 0..leaf_count {
                let auth_path = archival_mmr
                    .prove_membership(mutated_index as u128)
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
        type H = RescuePrimeRegular;

        for leaf_count in [511, 1023] {
            println!("leaf_count = {leaf_count}");
            let init_leaf_digests: Vec<Digest> = random_elements(leaf_count);
            let new_leaf: Digest = thread_rng().gen();
            let mut archival_mmr: ArchivalMmr<H> =
                get_archival_mmr_from_digests(init_leaf_digests.clone());

            for mutated_index in [0, leaf_count - 100, leaf_count - 2, leaf_count - 1] {
                let auth_path = archival_mmr
                    .prove_membership(mutated_index as u128)
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
        type H = RescuePrimeRegular;

        for log_sizes in [15u64, 20, 25, 32, 35, 40, 45, 50, 55, 60, 62, 63] {
            println!("log_sizes = {log_sizes}");
            let init_peak_digests: Vec<Digest> = random_elements(log_sizes as usize);
            let new_leaf: Digest = thread_rng().gen();
            let mut init_mmr: MmrAccumulator<H> =
                MmrAccumulator::init(init_peak_digests.clone(), (1u64 << log_sizes) as u128 - 1);

            let mut final_peaks = init_peak_digests.clone();
            final_peaks[log_sizes as usize - 1] = new_leaf;
            let expected_final_mmra: MmrAccumulator<H> =
                MmrAccumulator::init(final_peaks, (1u64 << log_sizes) as u128 - 1);
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
        type H = RescuePrimeRegular;

        for log_size in [31, 63] {
            println!("log_sizes = {log_size}");
            let init_peak_digests: Vec<Digest> = random_elements(log_size as usize);
            let new_leaf: Digest = thread_rng().gen();
            let before_insertion_mmr: MmrAccumulator<H> =
                MmrAccumulator::init(init_peak_digests.clone(), (1u64 << log_size) as u128 - 1);

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

    fn prop_calculate_new_peaks_from_leaf_mutation<
        H: AlgebraicHasher + std::cmp::PartialEq + std::fmt::Debug,
    >(
        start_mmr: &mut MmrAccumulator<H>,
        new_leaf: Digest,
        new_leaf_index: u64,
        expected_mmr: MmrAccumulator<H>,
        auth_path: Vec<Digest>,
    ) {
        // Init stack: _ *peaks *auth_paths [digest (new_leaf)] leaf_count_hi leaf_count_lo leaf_index_hi leaf_index_lo
        let mut init_stack = get_init_tvm_stack();
        let peaks_pointer = BFieldElement::zero();
        init_stack.push(peaks_pointer);

        // We assume that the auth paths can safely be stored in memory on this address
        let auth_path_pointer = BFieldElement::new((MAX_MMR_HEIGHT * DIGEST_LENGTH + 1) as u64);
        init_stack.push(auth_path_pointer);

        // push digests such that element 0 of digest is on top of stack
        for value in new_leaf.values().iter().rev() {
            init_stack.push(*value);
        }

        let leaf_count: u64 = start_mmr.count_leaves() as u64;
        init_stack.push(BFieldElement::new(leaf_count >> 32));
        init_stack.push(BFieldElement::new(leaf_count & u32::MAX as u64));

        init_stack.push(BFieldElement::new(new_leaf_index >> 32));
        init_stack.push(BFieldElement::new(new_leaf_index & u32::MAX as u64));

        // Initialize memory
        let mut memory: HashMap<BFieldElement, BFieldElement> = HashMap::default();
        rust_shadowing_helper_functions::list_new(peaks_pointer, &mut memory);
        for peak in start_mmr.get_peaks() {
            rust_shadowing_helper_functions::list_push(peaks_pointer, peak.values(), &mut memory);
        }

        rust_shadowing_helper_functions::list_new(auth_path_pointer, &mut memory);
        for ap_element in auth_path.iter() {
            rust_shadowing_helper_functions::list_push(
                auth_path_pointer,
                ap_element.values(),
                &mut memory,
            );
        }

        let expected_final_stack = get_init_tvm_stack();

        let _execution_result =
            rust_tasm_equivalence_prop::<MmrCalculateNewPeaksFromLeafMutationMtIndices>(
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
            let peak: Digest = Digest::new(rust_shadowing_helper_functions::list_read(
                peaks_pointer,
                i as usize,
                &memory,
            ));
            produced_peaks.push(peak);
        }

        let mut produced_mmr = MmrAccumulator::<H>::init(produced_peaks, start_mmr.count_leaves());

        // Verify that both code paths produce the same MMR
        assert_eq!(expected_mmr, produced_mmr);

        // Verify that auth paths is still value
        let auth_path_element_count = memory[&auth_path_pointer].value();
        let mut auth_path = vec![];
        for i in 0..auth_path_element_count {
            let auth_path_element: Digest = Digest::new(
                rust_shadowing_helper_functions::list_read(auth_path_pointer, i as usize, &memory),
            );
            auth_path.push(auth_path_element);
        }

        let mmr_mp = MmrMembershipProof::<H> {
            data_index: new_leaf_index as u128,
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
