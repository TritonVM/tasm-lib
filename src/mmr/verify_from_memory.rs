use std::collections::HashMap;

use twenty_first::shared_math::b_field_element::BFieldElement;
use twenty_first::shared_math::rescue_prime_digest::Digest;
use twenty_first::shared_math::rescue_prime_regular::{RescuePrimeRegular, DIGEST_LENGTH};
use twenty_first::util_types::mmr::mmr_membership_proof::MmrMembershipProof;

use crate::arithmetic::u32::is_odd::U32IsOdd;
use crate::arithmetic::u64::div2_u64::Div2U64;
use crate::arithmetic::u64::eq_u64::EqU64;
use crate::library::Library;
use crate::list::u32::get::Get;
use crate::rust_shadowing_helper_functions;
use crate::snippet_trait::Snippet;

use super::leaf_index_to_mt_index::MmrLeafIndexToMtIndexAndPeakIndex;

pub struct MmrVerifyFromMemory();

impl Snippet for MmrVerifyFromMemory {
    fn stack_diff() -> isize {
        // pops *peaks, *auth_path, [digest (leaf_hash)], leaf_count_hi, leaf_count_hi, leaf_index_hi, leaf_index_lo
        // pushes a bool indicating validation result.
        -10
    }

    fn entrypoint() -> &'static str {
        "verify_from_memory"
    }

    fn function_body(library: &mut Library) -> String {
        let leaf_index_to_mt_index = library.import::<MmrLeafIndexToMtIndexAndPeakIndex>();
        let get = library.import::<Get<DIGEST_LENGTH>>();
        let u32_is_odd = library.import::<U32IsOdd>();
        let entrypoint = Self::entrypoint();
        let eq_u64 = library.import::<EqU64>();
        let div_2 = library.import::<Div2U64>();
        format!(
            "
                // BEFORE: _ *peaks *auth_path [digest (new_leaf)] leaf_count_hi leaf_count_lo leaf_index_hi leaf_index_lo
                // AFTER: _ validation_result
                // Will crash if `leaf_index >= leaf_count`
                {entrypoint}:
                    call {leaf_index_to_mt_index}
                    // stack: _ *peaks *auth_path [digest (new_leaf)] mt_index_hi mt_index_lo peak_index

                    push 0
                    // stack: _ *peaks *auth_path [digest (new_leaf)] mt_index_hi mt_index_lo peak_index i

                    dup8 dup8 dup8 dup8 dup8
                    // stack: _ *peaks *auth_path [digest (new_leaf)] mt_index_hi mt_index_lo peak_index i [digest (new_leaf)]
                    // rename: new_leaf -> acc_hash

                    call {entrypoint}_while
                    // _ *peaks *auth_path [digest (new_leaf)] mt_index_hi mt_index_lo peak_index i [digest (acc_hash)]

                    dup15 dup7
                    // _ *peaks *auth_path [digest (new_leaf)] mt_index_hi mt_index_lo peak_index i [digest (acc_hash)] *peaks peak_index

                    call {get}
                    // _ *peaks *auth_path [digest (new_leaf)] mt_index_hi mt_index_lo peak_index i [digest (acc_hash)] [digest (expected_peak)]

                    // Compare top two digests
                    // _ *peaks *auth_path [digest (new_leaf)] mt_index_hi mt_index_lo peak_index i b4 b3 b2 b1 b0 a4 a3 a2 a1 a0

                    swap6 eq
                    // _ *peaks *auth_path [digest (new_leaf)] mt_index_hi mt_index_lo peak_index i b4 b3 b2 a0 b0 a4 a3 a2 (a1 == b1)

                    swap6 eq
                    // _ *peaks *auth_path [digest (new_leaf)] mt_index_hi mt_index_lo peak_index i b4 b3 (a1 == b1) a0 b0 a4 a3 (a2 == b2)

                    swap6 eq
                    // _ *peaks *auth_path [digest (new_leaf)] mt_index_hi mt_index_lo peak_index i b4 (a2 == b2) (a1 == b1) a0 b0 a4 (a3 == b3)

                    swap6 eq
                    // _ *peaks *auth_path [digest (new_leaf)] mt_index_hi mt_index_lo peak_index i (a3 == b3) (a2 == b2) (a1 == b1) a0 b0 (a4 == b4)

                    swap2 eq
                    // _ *peaks *auth_path [digest (new_leaf)] mt_index_hi mt_index_lo peak_index i (a3 == b3) (a2 == b2) (a1 == b1) (a4 == b4) (a0 == b0)

                    mul mul mul mul
                    // _ *peaks *auth_path [digest (new_leaf)] mt_index_hi mt_index_lo peak_index i (a3 == b3)*(a2 == b2)*(a1 == b1)*(a4 == b4)*(a0 == b0)
                    // rename: (a3 == b3)*(a2 == b2)*(a1 == b1)*(a4 == b4)*(a0 == b0) -> ret

                    swap11
                    // _ ret *auth_path [digest (new_leaf)] mt_index_hi mt_index_lo peak_index i *peaks

                    pop pop pop pop pop
                    pop pop pop pop pop
                    pop
                    // _ ret

                    return

                // Note that this while loop is the same as one in `calculate_new_peaks_from_leaf_mutation`
                // BEFORE/AFTER: _ *peaks *auth_path [digest (new_leaf)] mt_index_hi mt_index_lo peak_index i [digest (acc_hash)]
                {entrypoint}_while:
                    dup8 dup8 push 0 push 1 call {eq_u64}
                    // _ *peaks *auth_path [digest (new_leaf)] mt_index_hi mt_index_lo peak_index i [digest (acc_hash)] (mt_index == 1)

                    skiz return
                    // _ *peaks *auth_path [digest (new_leaf)] mt_index_hi mt_index_lo peak_index i [digest (acc_hash)]

                    // declare `ap_element`
                    dup14 dup6 call {get}
                    // _ *peaks *auth_path [digest (new_leaf)] mt_index_hi mt_index_lo peak_index i [digest (acc_hash)] [digest (ap_element)]

                    dup12 call {u32_is_odd} push 0 eq
                    // _ *peaks *auth_paths [digest (new_leaf)] mt_index_hi mt_index_lo peak_index i [digest (acc_hash)] [digest (ap_element)] (mt_index % 2 == 0)

                    skiz call {entrypoint}_swap_digests
                    // _ *peaks *auth_paths [digest (new_leaf)] mt_index_hi mt_index_lo peak_index i [digest (right_node)] [digest (left_node)]

                    hash
                    pop pop pop pop pop
                    // _ *peaks *auth_paths [digest (new_leaf)] mt_index_hi mt_index_lo peak_index i [digest (new_acc_hash)]

                    // i -> i + 1
                    swap5 push 1 add swap5
                    // _ *peaks *auth_paths [digest (new_leaf)] mt_index_hi mt_index_lo peak_index (i + 1) [digest (new_acc_hash)]

                    // mt_index -> mt_index / 2
                    swap8 swap1 swap7
                    // _ *peaks *auth_paths [digest (new_leaf)] peak_index acc_hash_0 acc_hash_1 (i + 1) acc_hash_4 acc_hash_3 acc_hash_2 mt_index_hi mt_index_lo

                    call {div_2}
                    // _ *peaks *auth_paths [digest (new_leaf)] peak_index acc_hash_0 acc_hash_1 (i + 1) acc_hash_4 acc_hash_3 acc_hash_2 (mt_index / 2)_hi (mt_index / 2)_lo

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
        type H = RescuePrimeRegular;

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

        let leaf_hash = Digest::new(new_leaf_digest_values);

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

        let valid_mp = MmrMembershipProof::<H>::new(leaf_index as u128, auth_path)
            .verify(&peaks, &leaf_hash, leaf_count as u128)
            .0;

        stack.push(BFieldElement::new(valid_mp as u64));
    }
}

#[cfg(test)]
mod auth_path_verify_from_memory_tests {
    use num::Zero;
    use rand::{thread_rng, Rng};
    use twenty_first::{
        shared_math::{b_field_element::BFieldElement, other::random_elements},
        test_shared::mmr::{get_archival_mmr_from_digests, get_empty_archival_mmr},
        util_types::{
            algebraic_hasher::AlgebraicHasher,
            mmr::{
                archival_mmr::ArchivalMmr, mmr_accumulator::MmrAccumulator,
                mmr_membership_proof::MmrMembershipProof, mmr_trait::Mmr,
            },
        },
    };

    use crate::{
        get_init_tvm_stack, mmr::calculate_new_peaks_from_append::MAX_MMR_HEIGHT,
        snippet_trait::rust_tasm_equivalence_prop,
    };

    use super::*;

    // This will crash the VM because leaf?index is not strictly less than leaf_count
    #[test]
    #[should_panic]
    fn mmra_ap_verify_test_empty() {
        type H = RescuePrimeRegular;
        let digest0 = H::hash(&BFieldElement::new(4545));
        let mut archival_mmr: ArchivalMmr<H> = get_empty_archival_mmr();
        let mut mmr = archival_mmr.to_accumulator();
        let leaf_index = 0;
        prop_verify_from_memory(&mut mmr, digest0, leaf_index, vec![], false);
    }

    #[test]
    fn mmra_ap_verify_test_one() {
        type H = RescuePrimeRegular;
        let digest0 = H::hash(&BFieldElement::new(4545));
        let mut archival_mmr: ArchivalMmr<H> = get_empty_archival_mmr();
        archival_mmr.append(digest0);
        let mut mmr = archival_mmr.to_accumulator();
        let leaf_index = 0;
        prop_verify_from_memory(&mut mmr, digest0, leaf_index, vec![], true);
    }

    #[test]
    fn mmra_ap_verify_test_two() {
        type H = RescuePrimeRegular;
        let digest0 = H::hash(&BFieldElement::new(123));
        let digest1 = H::hash(&BFieldElement::new(456));

        let mut archival_mmr: ArchivalMmr<H> = get_empty_archival_mmr();
        archival_mmr.append(digest0);
        archival_mmr.append(digest1);
        let mut mmr = archival_mmr.to_accumulator();

        let leaf_index_0 = 0;
        prop_verify_from_memory(&mut mmr, digest0, leaf_index_0, vec![digest1], true);

        let leaf_index_1 = 1;
        prop_verify_from_memory(&mut mmr, digest1, leaf_index_1, vec![digest0], true);
    }

    #[test]
    fn mmra_ap_verify_test_pbt() {
        type H = RescuePrimeRegular;
        let max_size = 19;

        for leaf_count in 0..max_size {
            let digests: Vec<Digest> = random_elements(leaf_count);
            let mut archival_mmr: ArchivalMmr<H> = get_archival_mmr_from_digests(digests.clone());
            let mut mmr = archival_mmr.to_accumulator();

            let bad_leaf: Digest = thread_rng().gen();
            for (leaf_index, leaf_digest) in digests.into_iter().enumerate() {
                let (auth_path, _) = archival_mmr.prove_membership(leaf_index as u128);

                // Positive test
                prop_verify_from_memory(
                    &mut mmr,
                    leaf_digest,
                    leaf_index as u64,
                    auth_path.authentication_path.clone(),
                    true,
                );

                // Negative tests
                let bad_index = (leaf_index + 1) % leaf_count;
                if bad_index != leaf_index {
                    prop_verify_from_memory(
                        &mut mmr,
                        leaf_digest,
                        bad_index as u64,
                        auth_path.authentication_path.clone(),
                        false,
                    );
                }
                prop_verify_from_memory(
                    &mut mmr,
                    bad_leaf,
                    leaf_index as u64,
                    auth_path.authentication_path,
                    false,
                );
            }
        }
    }

    #[test]
    fn mmra_ap_verify_many_leafs() {
        type H = RescuePrimeRegular;

        for init_leaf_count in [
            (1u64 << 40) + (1 << 21) + 510,
            (1 << 32) - 1,
            (1 << 63) - 3,
            (1 << 63) - 2,
            (1 << 63) - 1,
            1 << 63,
        ] {
            // let init_peak_count = 10; // 1 + 1 + 8
            let init_peak_count = init_leaf_count.count_ones();
            println!("init_peak_count = {init_peak_count}");

            // We can't construct this large archival MMRs, so we have to handle it with an MMRA
            // and handle the membership proofs ourselves
            let fake_peaks: Vec<Digest> = random_elements(init_peak_count as usize);
            let mut mmr: MmrAccumulator<H> =
                MmrAccumulator::init(fake_peaks, init_leaf_count as u128);

            // Insert the 1st leaf
            let second_to_last_leaf: Digest = thread_rng().gen();
            let second_to_last_leaf_index = init_leaf_count;
            let mut real_membership_proof_second_to_last = mmr.append(second_to_last_leaf);
            assert_eq!(
                real_membership_proof_second_to_last.data_index,
                second_to_last_leaf_index as u128
            );

            // Insert one more leaf and update the existing membership proof
            let last_leaf: Digest = thread_rng().gen();
            let last_leaf_index = second_to_last_leaf_index + 1;
            MmrMembershipProof::update_from_append(
                &mut real_membership_proof_second_to_last,
                init_leaf_count as u128 + 1,
                &last_leaf,
                &mmr.get_peaks(),
            );
            let real_membership_proof_last = mmr.append(last_leaf);

            prop_verify_from_memory(
                &mut mmr,
                second_to_last_leaf,
                second_to_last_leaf_index,
                real_membership_proof_second_to_last.authentication_path,
                true,
            );
            prop_verify_from_memory(
                &mut mmr,
                last_leaf,
                last_leaf_index,
                real_membership_proof_last.authentication_path,
                true,
            );
        }
    }

    fn prop_verify_from_memory<H: AlgebraicHasher + std::cmp::PartialEq + std::fmt::Debug>(
        mmr: &mut MmrAccumulator<H>,
        leaf: Digest,
        leaf_index: u64,
        auth_path: Vec<Digest>,
        expect_validation_success: bool,
    ) {
        // Init stack: _ *peaks *auth_paths [digest (new_leaf)] leaf_count_hi leaf_count_lo leaf_index_hi leaf_index_lo
        let mut init_stack = get_init_tvm_stack();
        let peaks_pointer = BFieldElement::zero();
        init_stack.push(peaks_pointer);

        // We assume that the auth paths can safely be stored in memory on this address
        let auth_path_pointer = BFieldElement::new((MAX_MMR_HEIGHT * DIGEST_LENGTH + 1) as u64);
        init_stack.push(auth_path_pointer);

        // push digests such that element 0 of digest is on top of stack
        for value in leaf.values().iter().rev() {
            init_stack.push(*value);
        }

        let leaf_count: u64 = mmr.count_leaves() as u64;
        init_stack.push(BFieldElement::new(leaf_count >> 32));
        init_stack.push(BFieldElement::new(leaf_count & u32::MAX as u64));

        init_stack.push(BFieldElement::new(leaf_index >> 32));
        init_stack.push(BFieldElement::new(leaf_index & u32::MAX as u64));

        // Initialize memory
        let mut memory: HashMap<BFieldElement, BFieldElement> = HashMap::default();
        rust_shadowing_helper_functions::list_new(peaks_pointer, &mut memory);
        for peak in mmr.get_peaks() {
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

        rust_shadowing_helper_functions::list_new(auth_path_pointer, &mut memory);
        for ap_element in auth_path.iter() {
            rust_shadowing_helper_functions::list_push(
                auth_path_pointer,
                ap_element.values(),
                &mut memory,
            );
        }

        let mut expected_final_stack = get_init_tvm_stack();
        expected_final_stack.push(BFieldElement::new(expect_validation_success as u64));

        let _execution_result = rust_tasm_equivalence_prop::<MmrVerifyFromMemory>(
            &init_stack,
            &[],
            &[],
            &mut memory,
            MAX_MMR_HEIGHT * DIGEST_LENGTH + 1, // assume that 64 digests are allocated in memory when code starts to run
            Some(&expected_final_stack),
        );

        // Verify that auth path expectation was correct
        assert_eq!(
            expect_validation_success,
            MmrMembershipProof::<H>::new(leaf_index as u128, auth_path)
                .verify(&mmr.get_peaks(), &leaf, mmr.count_leaves())
                .0
        );
    }
}
