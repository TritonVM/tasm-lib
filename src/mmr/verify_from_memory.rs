use std::collections::HashMap;

use num::Zero;
use rand::{thread_rng, Rng};
use twenty_first::shared_math::b_field_element::BFieldElement;
use twenty_first::shared_math::other::random_elements;
use twenty_first::shared_math::rescue_prime_digest::{Digest, DIGEST_LENGTH};
use twenty_first::shared_math::rescue_prime_regular::RescuePrimeRegular;
use twenty_first::test_shared::mmr::get_archival_mmr_from_digests;
use twenty_first::util_types::algebraic_hasher::AlgebraicHasher;
use twenty_first::util_types::mmr::archival_mmr::ArchivalMmr;
use twenty_first::util_types::mmr::mmr_accumulator::MmrAccumulator;
use twenty_first::util_types::mmr::mmr_membership_proof::MmrMembershipProof;
use twenty_first::util_types::mmr::mmr_trait::Mmr;

use crate::arithmetic::u32::is_odd::U32IsOdd;
use crate::arithmetic::u64::div2_u64::Div2U64;
use crate::arithmetic::u64::eq_u64::EqU64;
use crate::hashing::eq_digest::EqDigest;
use crate::hashing::swap_digest::SwapDigest;
use crate::library::Library;
use crate::list::unsafe_u32::get::Get;
use crate::snippet::{DataType, Snippet};
use crate::{get_init_tvm_stack, rust_shadowing_helper_functions, ExecutionState};

use super::leaf_index_to_mt_index::MmrLeafIndexToMtIndexAndPeakIndex;
use super::MAX_MMR_HEIGHT;

#[derive(Clone)]
pub struct MmrVerifyFromMemory;

impl Snippet for MmrVerifyFromMemory {
    fn inputs() -> Vec<&'static str> {
        vec![
            "*peaks",
            "leaf_count_hi",
            "leaf_count_lo",
            "leaf_index_hi",
            "leaf_index_lo",
            "leaf_digest_4",
            "leaf_digest_3",
            "leaf_digest_2",
            "leaf_digest_1",
            "leaf_digest_0",
            "*auth_path",
        ]
    }

    fn outputs() -> Vec<&'static str> {
        vec![
            "*auth_path",
            "leaf_index_hi",
            "leaf_index_lo",
            "validation_result",
        ]
    }

    fn input_types(&self) -> Vec<crate::snippet::DataType> {
        vec![
            DataType::List(Box::new(DataType::Digest)),
            DataType::U64,
            DataType::U64,
            DataType::Digest,
            DataType::List(Box::new(DataType::Digest)),
        ]
    }

    fn output_types(&self) -> Vec<crate::snippet::DataType> {
        vec![
            DataType::List(Box::new(DataType::Digest)),
            DataType::U64,
            DataType::Bool,
        ]
    }

    fn crash_conditions() -> Vec<&'static str> {
        vec!["leaf_index >= leaf_count", "leaf_index values not u32s"]
    }

    fn gen_input_states() -> Vec<crate::ExecutionState> {
        type H = RescuePrimeRegular;
        let mut rng = thread_rng();
        let max_size = 100;
        let size = rng.gen_range(1..max_size);
        let digests: Vec<Digest> = random_elements(size);
        let leaf_index = rng.gen_range(0..size);
        let leaf = digests[leaf_index];
        let mut ammr: ArchivalMmr<H> = get_archival_mmr_from_digests(digests);
        let auth_path = ammr
            .prove_membership(leaf_index as u128)
            .0
            .authentication_path;
        let (ret0, _, _) = prepare_vm_state(
            &mut ammr.to_accumulator(),
            leaf,
            leaf_index as u64,
            auth_path,
        );

        vec![ret0]
    }

    fn stack_diff() -> isize {
        -7
    }

    fn entrypoint(&self) -> &'static str {
        "verify_from_memory"
    }

    fn function_body(&self, library: &mut Library) -> String {
        let leaf_index_to_mt_index = library.import(Box::new(MmrLeafIndexToMtIndexAndPeakIndex));
        let get = library.import(Box::new(Get::<DIGEST_LENGTH>(DataType::Digest)));
        let u32_is_odd = library.import(Box::new(U32IsOdd));
        let entrypoint = self.entrypoint();
        let eq_u64 = library.import(Box::new(EqU64));
        let div_2 = library.import(Box::new(Div2U64));
        let swap_digests = library.import(Box::new(SwapDigest));
        let eq_digest = library.import(Box::new(EqDigest));
        format!(
            "
                // BEFORE: _ *peaks leaf_count_hi leaf_count_lo leaf_index_hi leaf_index_lo [digest (leaf_digest)] *auth_path
                // AFTER: _ *auth_path leaf_index_hi leaf_index_lo validation_result
                // Will crash if `leaf_index >= leaf_count`
                {entrypoint}:
                    dup9 dup9 dup9 dup9
                    call {leaf_index_to_mt_index}
                    // stack: _ *peaks leaf_count_hi leaf_count_lo leaf_index_hi leaf_index_lo [digest (leaf_digest)] *auth_path mt_index_hi mt_index_lo peak_index

                    // Push index counter into memory, `i`, to stack
                    push 0
                    /// stack: _ *peaks leaf_count_hi leaf_count_lo leaf_index_hi leaf_index_lo [digest (leaf_digest)] *auth_path mt_index_hi mt_index_lo peak_index i

                    swap9 swap4 swap8 swap3 swap6 swap1 swap7 swap2 swap5
                    // stack: _ *peaks leaf_count_hi leaf_count_lo leaf_index_hi leaf_index_lo i *auth_path peak_index mt_index_hi mt_index_lo [digest (leaf_digest)]

                    // rename: leaf_digest -> acc_hash
                    // stack: _ *peaks leaf_count_hi leaf_count_lo leaf_index_hi leaf_index_lo i *auth_path peak_index mt_index_hi mt_index_lo [digest (acc_hash)]

                    call {entrypoint}_while
                    // _ *peaks leaf_count_hi leaf_count_lo leaf_index_hi leaf_index_lo i *auth_path peak_index mt_index_hi mt_index_lo [digest (acc_hash)]

                    // Compare `acc_hash` to the `expected_peak`, where `expected_peak = peaks[peak_index]`
                    dup14 dup8
                    // _ *peaks leaf_count_hi leaf_count_lo leaf_index_hi leaf_index_lo i *auth_path peak_index mt_index_hi mt_index_lo [digest (acc_hash)] *peaks peak_index

                    call {get}
                    // _ *peaks leaf_count_hi leaf_count_lo leaf_index_hi leaf_index_lo i *auth_path peak_index mt_index_hi mt_index_lo [digest (acc_hash)] [digest (expected_peak)]

                    // Compare top two digests
                    call {eq_digest}
                    // _ *peaks leaf_count_hi leaf_count_lo leaf_index_hi leaf_index_lo i *auth_path peak_index mt_index_hi mt_index_lo (expected_peak == acc_hash)

                    // Rename: expected_peak == acc_hash -> validation_result
                    // _ *peaks leaf_count_hi leaf_count_lo leaf_index_hi leaf_index_lo i *auth_path peak_index mt_index_hi mt_index_lo validation_result

                    // Cleanup stack
                    swap7
                    // _ *peaks leaf_count_hi leaf_count_lo validation_result leaf_index_lo i *auth_path peak_index mt_index_hi mt_index_lo leaf_index_hi

                    swap9 pop pop pop pop
                    // _ *peaks leaf_index_hi leaf_count_lo validation_result leaf_index_lo i *auth_path

                    swap6 pop pop
                    // _ *auth_path leaf_index_hi leaf_count_lo validation_result leaf_index_lo

                    swap2 pop
                    // _ *auth_path leaf_index_hi leaf_index_lo validation_result

                    return

                // Note that this while loop is the same as one in `calculate_new_peaks_from_leaf_mutation`
                // BEFORE/AFTER: _ *peaks leaf_count_hi leaf_count_lo leaf_index_hi leaf_index_lo i *auth_path peak_index mt_index_hi mt_index_lo [digest (acc_hash)]
                {entrypoint}_while:
                    dup6 dup6 push 0 push 1 call {eq_u64}
                    // _ *peaks leaf_count_hi leaf_count_lo leaf_index_hi leaf_index_lo i *auth_path peak_index mt_index_hi mt_index_lo [digest (acc_hash)] (mt_index == 1)

                    skiz return
                    // _ *peaks leaf_count_hi leaf_count_lo leaf_index_hi leaf_index_lo i *auth_path peak_index mt_index_hi mt_index_lo [digest (acc_hash)]

                    // declare `ap_element = auth_path[i]`
                    dup8 dup10
                    // _ *peaks leaf_count_hi leaf_count_lo leaf_index_hi leaf_index_lo i *auth_path peak_index mt_index_hi mt_index_lo *auth_path i

                    call {get}
                    // _ *peaks leaf_count_hi leaf_count_lo leaf_index_hi leaf_index_lo i *auth_path peak_index mt_index_hi mt_index_lo [digest (acc_hash)] [digest (ap_element)]

                    dup10 call {u32_is_odd} push 0 eq
                    // _ *peaks leaf_count_hi leaf_count_lo leaf_index_hi leaf_index_lo i *auth_path peak_index mt_index_hi mt_index_lo [digest (acc_hash)] [digest (ap_element)] (mt_index % 2 == 0)

                    skiz call {swap_digests}
                    // _ *peaks leaf_count_hi leaf_count_lo leaf_index_hi leaf_index_lo i *auth_path peak_index mt_index_hi mt_index_lo [digest (right_node)] [digest (left_node)]

                    hash
                    pop pop pop pop pop
                    // _ *peaks leaf_count_hi leaf_count_lo leaf_index_hi leaf_index_lo i *auth_path peak_index mt_index_hi mt_index_lo [digest (acc_hash)]

                    // i -> i + 1
                    swap9 push 1 add swap9
                    // _ *peaks leaf_count_hi leaf_count_lo leaf_index_hi leaf_index_lo (i + 1) *auth_path peak_index mt_index_hi mt_index_lo [digest (acc_hash)]

                    // mt_index -> mt_index / 2
                    swap6 swap1 swap5
                    call {div_2}
                    swap5 swap1 swap6
                    // _ *peaks leaf_count_hi leaf_count_lo leaf_index_hi leaf_index_lo (i + 1) *auth_path peak_index (mt_index / 2)_hi (mt_index / 2)_lo acc_hash_4 acc_hash_3 acc_hash_2 acc_hash_1 acc_hash_0

                    // Rename: i + 1 -> i; (mt_index / 2) -> mt_index
                    // _ *peaks leaf_count_hi leaf_count_lo leaf_index_hi leaf_index_lo i *auth_path peak_index mt_index_hi mt_index_lo [digest (acc_hash)]

                    recurse
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

        // BEFORE: _ *peaks leaf_count_hi leaf_count_lo leaf_index_hi leaf_index_lo [digest (leaf_digest)] *auth_path
        // AFTER: _ *auth_path leaf_index_hi leaf_index_lo validation_result
        let auth_path_pointer = stack.pop().unwrap();

        let auth_path_length = memory[&auth_path_pointer].value();
        let mut auth_path: Vec<Digest> = vec![];
        for i in 0..auth_path_length {
            let digest = Digest::new(rust_shadowing_helper_functions::unsafe_list_read(
                auth_path_pointer,
                i as usize,
                memory,
            ));
            auth_path.push(digest);
        }

        let mut new_leaf_digest_values = [BFieldElement::new(0); DIGEST_LENGTH];
        for elem in new_leaf_digest_values.iter_mut() {
            *elem = stack.pop().unwrap();
        }

        let leaf_digest = Digest::new(new_leaf_digest_values);

        let leaf_index_lo: u32 = stack.pop().unwrap().try_into().unwrap();
        let leaf_index_hi: u32 = stack.pop().unwrap().try_into().unwrap();
        let leaf_index: u64 = ((leaf_index_hi as u64) << 32) + leaf_index_lo as u64;

        let leaf_count_lo: u32 = stack.pop().unwrap().try_into().unwrap();
        let leaf_count_hi: u32 = stack.pop().unwrap().try_into().unwrap();
        let leaf_count: u64 = ((leaf_count_hi as u64) << 32) + leaf_count_lo as u64;

        let peaks_pointer = stack.pop().unwrap();
        let peaks_count: u64 = memory[&peaks_pointer].value();
        let mut peaks: Vec<Digest> = vec![];
        for i in 0..peaks_count {
            let digest = Digest::new(rust_shadowing_helper_functions::unsafe_list_read(
                peaks_pointer,
                i as usize,
                memory,
            ));
            peaks.push(digest);
        }

        let valid_mp = MmrMembershipProof::<H>::new(leaf_index as u128, auth_path)
            .verify(&peaks, &leaf_digest, leaf_count as u128)
            .0;

        stack.push(auth_path_pointer);
        stack.push(BFieldElement::new(leaf_index_hi as u64));
        stack.push(BFieldElement::new(leaf_index_lo as u64));
        stack.push(BFieldElement::new(valid_mp as u64));
    }
}

/// Returns (ExecutionState, auth_path_pointer, peaks_pointer)
fn prepare_vm_state<H: AlgebraicHasher + std::cmp::PartialEq + std::fmt::Debug>(
    mmr: &mut MmrAccumulator<H>,
    leaf: Digest,
    leaf_index: u64,
    auth_path: Vec<Digest>,
) -> (ExecutionState, BFieldElement, BFieldElement) {
    // BEFORE: _ *peaks leaf_count_hi leaf_count_lo leaf_index_hi leaf_index_lo [digest (leaf_digest)] *auth_path
    // AFTER: _ *auth_path leaf_index_hi leaf_index_lo validation_result
    let mut stack = get_init_tvm_stack();

    let peaks_pointer = BFieldElement::zero();
    stack.push(peaks_pointer);

    let leaf_count: u64 = mmr.count_leaves() as u64;
    stack.push(BFieldElement::new(leaf_count >> 32));
    stack.push(BFieldElement::new(leaf_count & u32::MAX as u64));

    let leaf_index_hi = BFieldElement::new(leaf_index >> 32);
    let leaf_index_lo = BFieldElement::new(leaf_index & u32::MAX as u64);
    stack.push(leaf_index_hi);
    stack.push(leaf_index_lo);

    // push digests such that element 0 of digest is on top of stack
    for value in leaf.values().iter().rev() {
        stack.push(*value);
    }

    // We assume that the auth paths can safely be stored in memory on this address
    let auth_path_pointer = BFieldElement::new((MAX_MMR_HEIGHT * DIGEST_LENGTH + 1) as u64);
    stack.push(auth_path_pointer);

    // Initialize memory
    let mut memory: HashMap<BFieldElement, BFieldElement> = HashMap::default();
    rust_shadowing_helper_functions::unsafe_list_new(peaks_pointer, &mut memory);
    for peak in mmr.get_peaks() {
        rust_shadowing_helper_functions::unsafe_list_push(
            peaks_pointer,
            peak.values(),
            &mut memory,
        );
    }

    rust_shadowing_helper_functions::unsafe_list_new(auth_path_pointer, &mut memory);
    for ap_element in auth_path.iter() {
        rust_shadowing_helper_functions::unsafe_list_push(
            auth_path_pointer,
            ap_element.values(),
            &mut memory,
        );
    }

    (
        ExecutionState::with_stack_and_memory(stack, memory, 2 * (MAX_MMR_HEIGHT * DIGEST_LENGTH)),
        auth_path_pointer,
        peaks_pointer,
    )
}

#[cfg(test)]
mod auth_path_verify_from_memory_tests {
    use rand::{thread_rng, Rng};

    use twenty_first::shared_math::{b_field_element::BFieldElement, other::random_elements};
    use twenty_first::test_shared::mmr::{get_archival_mmr_from_digests, get_empty_archival_mmr};
    use twenty_first::util_types::algebraic_hasher::AlgebraicHasher;
    use twenty_first::util_types::mmr::archival_mmr::ArchivalMmr;
    use twenty_first::util_types::mmr::mmr_accumulator::MmrAccumulator;
    use twenty_first::util_types::mmr::{mmr_membership_proof::MmrMembershipProof, mmr_trait::Mmr};

    use crate::snippet_bencher::bench_and_write;
    use crate::test_helpers::{rust_tasm_equivalence_prop, rust_tasm_equivalence_prop_new};
    use crate::{get_init_tvm_stack, mmr::MAX_MMR_HEIGHT};

    use super::*;

    #[test]
    fn verify_from_memory_test() {
        rust_tasm_equivalence_prop_new::<MmrVerifyFromMemory>(MmrVerifyFromMemory);
    }

    #[test]
    fn verify_from_memory_benchmark() {
        bench_and_write::<MmrVerifyFromMemory>(MmrVerifyFromMemory);
    }

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
                real_membership_proof_second_to_last.leaf_index,
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

            // Positive tests
            prop_verify_from_memory(
                &mut mmr,
                second_to_last_leaf,
                second_to_last_leaf_index,
                real_membership_proof_second_to_last
                    .authentication_path
                    .clone(),
                true,
            );
            prop_verify_from_memory(
                &mut mmr,
                last_leaf,
                last_leaf_index,
                real_membership_proof_last.authentication_path.clone(),
                true,
            );

            // Negative tests
            let bad_leaf: Digest = thread_rng().gen();
            prop_verify_from_memory(
                &mut mmr,
                bad_leaf,
                second_to_last_leaf_index,
                real_membership_proof_second_to_last.authentication_path,
                false,
            );
            prop_verify_from_memory(
                &mut mmr,
                bad_leaf,
                last_leaf_index,
                real_membership_proof_last.authentication_path,
                false,
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
        let (exec_state, auth_path_pointer, _peaks_pointer) =
            prepare_vm_state(mmr, leaf, leaf_index, auth_path.clone());

        let leaf_index_hi = BFieldElement::new(leaf_index >> 32);
        let leaf_index_lo = BFieldElement::new(leaf_index & u32::MAX as u64);
        let init_stack = exec_state.stack;
        let mut memory = exec_state.memory;

        // AFTER: _ *auth_path leaf_index_hi leaf_index_lo validation_result
        let mut expected_final_stack = get_init_tvm_stack();
        expected_final_stack.push(auth_path_pointer);
        expected_final_stack.push(leaf_index_hi);
        expected_final_stack.push(leaf_index_lo);
        expected_final_stack.push(BFieldElement::new(expect_validation_success as u64));

        let _execution_result = rust_tasm_equivalence_prop::<MmrVerifyFromMemory>(
            MmrVerifyFromMemory,
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
