use num::{One, Zero};
use rand::{random, thread_rng, Rng};
use std::collections::HashMap;
use twenty_first::shared_math::b_field_element::BFieldElement;
use twenty_first::shared_math::other::random_elements;
use twenty_first::test_shared::mmr::get_rustyleveldb_ammr_from_digests;
use twenty_first::util_types::mmr::archival_mmr::ArchivalMmr;
use twenty_first::util_types::mmr::mmr_accumulator::MmrAccumulator;
use twenty_first::util_types::mmr::mmr_membership_proof::MmrMembershipProof;
use twenty_first::util_types::mmr::mmr_trait::Mmr;
use twenty_first::util_types::mmr::shared_basic::leaf_index_to_mt_index_and_peak_index;

use super::leaf_index_to_mt_index::MmrLeafIndexToMtIndexAndPeakIndex;
use super::MAX_MMR_HEIGHT;
use crate::arithmetic::u32::is_odd::U32IsOdd;
use crate::arithmetic::u64::div2_u64::Div2U64;
use crate::arithmetic::u64::eq_u64::EqU64;
use crate::hashing::eq_digest::EqDigest;
use crate::hashing::swap_digest::SwapDigest;
use crate::list::safe_u32::get::SafeGet;
use crate::list::unsafe_u32::get::UnsafeGet;
use crate::list::ListType;
use crate::snippet::{DataType, Snippet};
use crate::snippet_state::SnippetState;
use crate::{
    get_init_tvm_stack, rust_shadowing_helper_functions, Digest, ExecutionState, VmHasher,
    DIGEST_LENGTH,
};

#[derive(Clone, Debug)]
pub struct MmrVerifyLeafMembershipFromSecretIn {
    pub list_type: ListType,
}

impl MmrVerifyLeafMembershipFromSecretIn {
    // BEFORE: _ *peaks [digest (leaf_digest)] leaf_count_hi leaf_count_lo
    // AFTER: _ *auth_path leaf_index_hi leaf_index_lo
    fn prepare_state_for_tests(
        &self,
        size: usize,
        leaf_index: u64,
        generate_valid_proof: bool,
    ) -> ExecutionState {
        let digests: Vec<Digest> = random_elements(size);
        let ammr: ArchivalMmr<VmHasher, _> = get_rustyleveldb_ammr_from_digests(digests.clone());
        let mut vm_init_state = self.mmr_to_init_vm_state(&ammr.to_accumulator());

        // Populate secret-in with the leaf index value, which is a u64
        vm_init_state
            .secret_in
            .push(BFieldElement::new(leaf_index >> 32));
        vm_init_state
            .secret_in
            .push(BFieldElement::new(leaf_index & u32::MAX as u64));

        // Populate secret-in with the correct authentication path
        let mmr_mp = ammr.prove_membership(leaf_index).0;
        let authentication_path = mmr_mp.authentication_path;
        for ap_element in authentication_path.iter() {
            let mut ap_element_values = ap_element.values().to_vec();
            for _ in 0..DIGEST_LENGTH {
                vm_init_state
                    .secret_in
                    .push(ap_element_values.pop().unwrap());
            }
        }

        if generate_valid_proof {
            let good_leaf = digests[leaf_index as usize];
            for value in good_leaf.values().iter().rev() {
                vm_init_state.stack.push(*value);
            }
        } else {
            let bad_leaf = digests[(leaf_index as usize + 1) % size];
            for value in bad_leaf.values().iter().rev() {
                vm_init_state.stack.push(*value);
            }
        }
        vm_init_state
    }

    fn prepare_state_for_benchmark(&self, log_2_leaf_count: u8, leaf_index: u64) -> ExecutionState {
        let leaf_count = 2u64.pow(log_2_leaf_count as u32);
        let peaks: Vec<Digest> = random_elements(log_2_leaf_count as usize);
        let mut mmra = MmrAccumulator::<VmHasher>::init(peaks, leaf_count - 1);
        let new_leaf: Digest = random();
        let authentication_path = mmra.append(new_leaf).authentication_path;

        let mut vm_init_state = self.mmr_to_init_vm_state(&mmra);

        // Populate secret-in with the leaf index value, which is a u64
        vm_init_state
            .secret_in
            .push(BFieldElement::new(leaf_index >> 32));
        vm_init_state
            .secret_in
            .push(BFieldElement::new(leaf_index & u32::MAX as u64));

        // Populate secret-in with the correct authentication path
        for ap_element in authentication_path.iter() {
            let mut ap_element_values = ap_element.values().to_vec();
            for _ in 0..DIGEST_LENGTH {
                vm_init_state
                    .secret_in
                    .push(ap_element_values.pop().unwrap());
            }
        }

        for value in new_leaf.values().iter().rev() {
            vm_init_state.stack.push(*value);
        }

        vm_init_state
    }

    /// Prepare the part of the state that can be derived from the MMR without
    /// knowing e.g. the leaf index of the leaf digest that you want to authenticate
    /// so this function does not populate e.g. `secret_in`. The caller has to do that.
    fn mmr_to_init_vm_state(&self, mmra: &MmrAccumulator<VmHasher>) -> ExecutionState {
        let mut stack: Vec<BFieldElement> = get_init_tvm_stack();
        let peaks_pointer = BFieldElement::one();
        stack.push(peaks_pointer);

        let leaf_count = mmra.count_leaves();
        let leaf_count_hi = BFieldElement::new(leaf_count >> 32);
        let leaf_count_lo = BFieldElement::new(leaf_count & u32::MAX as u64);
        stack.push(leaf_count_hi);
        stack.push(leaf_count_lo);

        // Write peaks to memory
        let mut memory: HashMap<BFieldElement, BFieldElement> = HashMap::default();
        match self.list_type {
            ListType::Unsafe => {
                rust_shadowing_helper_functions::unsafe_list::unsafe_list_new(
                    peaks_pointer,
                    &mut memory,
                );
            }
            ListType::Safe => {
                rust_shadowing_helper_functions::safe_list::safe_list_new(
                    peaks_pointer,
                    MAX_MMR_HEIGHT as u32,
                    &mut memory,
                );
            }
        }

        let list_push = match self.list_type {
            ListType::Unsafe => rust_shadowing_helper_functions::unsafe_list::unsafe_list_push,
            ListType::Safe => rust_shadowing_helper_functions::safe_list::safe_list_push,
        };
        for peak in mmra.get_peaks() {
            list_push(
                peaks_pointer,
                peak.values().to_vec(),
                &mut memory,
                DIGEST_LENGTH,
            );
        }

        let list_metadata_size = match self.list_type {
            ListType::Safe => 2,
            ListType::Unsafe => 1,
        };
        ExecutionState {
            stack,
            std_in: vec![],
            secret_in: vec![],
            memory,
            words_allocated: DIGEST_LENGTH * MAX_MMR_HEIGHT + 1 + list_metadata_size,
        }
    }
}

impl Snippet for MmrVerifyLeafMembershipFromSecretIn {
    fn inputs(&self) -> Vec<String> {
        vec![
            "peaks_pointer".to_string(),
            "leaf_count_hi".to_string(),
            "leaf_count_lo".to_string(),
            "leaf_digest_4".to_string(),
            "leaf_digest_3".to_string(),
            "leaf_digest_2".to_string(),
            "leaf_digest_1".to_string(),
            "leaf_digest_0".to_string(),
        ]
    }

    fn outputs(&self) -> Vec<String> {
        vec![
            "leaf_index_hi".to_string(),
            "leaf_index_lo".to_string(),
            "validation_result".to_string(),
        ]
    }

    fn input_types(&self) -> Vec<crate::snippet::DataType> {
        vec![
            DataType::List(Box::new(DataType::Digest)),
            DataType::U64,
            DataType::Digest,
        ]
    }

    fn output_types(&self) -> Vec<crate::snippet::DataType> {
        vec![DataType::U64, DataType::Bool]
    }

    fn crash_conditions(&self) -> Vec<String> {
        vec![
            "secret input is too short".to_string(),
            "leaf index is not strictly less than leaf count".to_string(),
        ]
    }

    // BEFORE: _ *peaks leaf_count_hi leaf_count_lo [digest (leaf_digest)]
    // AFTER:  _ leaf_index_hi leaf_index_lo validation_result
    fn gen_input_states(&self) -> Vec<ExecutionState> {
        let mut rng = thread_rng();
        let mut init_vm_states = vec![];

        // Positive tests
        for size in 1..20 {
            let leaf_index = rng.gen_range(0..size) as u64;
            init_vm_states.push(self.prepare_state_for_tests(size, leaf_index, true));

            // Negative test
            init_vm_states.push(self.prepare_state_for_tests(size, leaf_index, false));
        }

        init_vm_states
    }

    fn stack_diff(&self) -> isize {
        -5
    }

    fn entrypoint(&self) -> String {
        format!("tasm_mmr_verify_from_secret_in_{}", self.list_type)
    }

    // Already on stack (can be secret of public input): _ *peaks leaf_count_hi leaf_count_lo [digest (leaf)]
    // Secret input: _ (authentication_path: Vec<Digest>), (leaf_digest: Digest), (leaf_index: u64)
    fn function_code(&self, library: &mut SnippetState) -> String {
        let entrypoint = self.entrypoint();

        let leaf_index_to_mt_index = library.import(Box::new(MmrLeafIndexToMtIndexAndPeakIndex));
        let eq_u64 = library.import(Box::new(EqU64));
        let u32_is_odd = library.import(Box::new(U32IsOdd));
        let swap_digests = library.import(Box::new(SwapDigest));
        let compare_digest = library.import(Box::new(EqDigest));
        let div_2 = library.import(Box::new(Div2U64));
        let list_get = match self.list_type {
            ListType::Unsafe => library.import(Box::new(UnsafeGet(DataType::Digest))),
            ListType::Safe => library.import(Box::new(SafeGet(DataType::Digest))),
        };

        let divine_digest = "divine\n".repeat(DIGEST_LENGTH);

        // BEFORE: _ *peaks leaf_count_hi leaf_count_lo [digest (leaf_digest)]
        // AFTER:  _ leaf_index_hi leaf_index_lo validation_result
        format!(
            "
            {entrypoint}:
                // Read leaf index from secret in
                divine
                divine
                // _ *peaks leaf_count_hi leaf_count_lo [digest (leaf_digest)] leaf_index_hi leaf_index_lo
                swap 8 swap 1 swap 9 swap 7
                // _ leaf_index_hi leaf_index_lo *peaks [digest (leaf_digest)] leaf_count_hi leaf_count_lo

                dup 9 dup 9
                // _ leaf_index_hi leaf_index_lo *peaks [digest (leaf_digest)] leaf_count_hi leaf_count_lo leaf_index_hi leaf_index_lo

                call {leaf_index_to_mt_index}
                // _ leaf_index_hi leaf_index_lo *peaks [digest (leaf_digest)] mt_index_hi mt_index_lo peak_index

                swap 7 swap 4 swap 1 swap 5 swap 2 swap 6 swap 3
                // _ leaf_index_hi leaf_index_lo *peaks peak_index mt_index_hi mt_index_lo [digest (leaf_digest)]

                // We're reading the authentication path from secret in, so we don't need a counter variable for that. We
                // only need to stop the loop when `mt_index == 1`, since this indicates that we've hit a peak in the MMR.

                // Rename: `leaf_digest` -> `acc_hash`
                // _ leaf_index_hi leaf_index_lo *peaks peak_index mt_index_hi mt_index_lo [digest (acc_hash)]

                call {entrypoint}_while
                // _ leaf_index_hi leaf_index_lo *peaks peak_index mt_index_hi mt_index_lo [digest (acc_hash)]

                // Compare `acc_hash` with `peaks[peak_index]`
                dup 8 dup 8 call {list_get}
                // _ leaf_index_hi leaf_index_lo *peaks peak_index mt_index_hi mt_index_lo [digest (acc_hash)] [digest (peaks[peak_index])]

                call {compare_digest}
                // _ leaf_index_hi leaf_index_lo *peaks peak_index mt_index_hi mt_index_lo (acc_hash == peaks[peak_index])

                swap 4 pop
                // _ leaf_index_hi leaf_index_lo (acc_hash == peaks[peak_index]) peak_index mt_index_hi mt_index_lo

                pop pop pop
                // _ leaf_index_hi leaf_index_lo validation_result

                return

            // start/end: _ leaf_index_hi leaf_index_lo *peaks peak_index mt_index_hi mt_index_lo [digest (acc_hash)]
            {entrypoint}_while:
                dup 6 dup 6 push 0 push 1 call {eq_u64}
                // __ leaf_index_hi leaf_index_lo *peaks peak_index mt_index_hi mt_index_lo [digest (acc_hash)] (mt_index == 1)

                skiz return
                // __ leaf_index_hi leaf_index_lo *peaks peak_index mt_index_hi mt_index_lo [digest (acc_hash)]

                // declare `ap_element` by reading it from secret in
                {divine_digest}
                // __ leaf_index_hi leaf_index_lo *peaks peak_index mt_index_hi mt_index_lo [digest (acc_hash)] [digest (ap_element)]

                dup 10 call {u32_is_odd} push 0 eq
                // _ *auth_path leaf_index_hi leaf_index_lo *peaks i peak_index mt_index_hi mt_index_lo [digest (acc_hash)] [digest (ap_element)] (mt_index % 2 == 0)

                skiz call {swap_digests}
                // _ *auth_path leaf_index_hi leaf_index_lo *peaks i peak_index mt_index_hi mt_index_lo [digest (right_node)] [digest (left_node)]

                hash pop pop pop pop pop
                // _ *auth_path leaf_index_hi leaf_index_lo *peaks i peak_index mt_index_hi mt_index_lo [digest (acc_hash)]

                // mt_index -> mt_index / 2
                swap 6 swap 1 swap 5
                // _ *auth_path [digest (leaf_digest)] *peaks peak_index acc_hash_0 acc_hash_1 (i + 1) acc_hash_4 acc_hash_3 acc_hash_2 mt_index_hi mt_index_lo

                call {div_2}
                // _ *auth_path [digest (leaf_digest)] *peaks peak_index acc_hash_0 acc_hash_1 (i + 1) acc_hash_4 acc_hash_3 acc_hash_2 (mt_index / 2)_hi (mt_index / 2)_lo

                swap 5 swap 1 swap 6
                // _ *auth_path [digest (leaf_digest)] *peaks (mt_index / 2)_hi (mt_index / 2)_lo peak_index (i + 1) acc_hash_4 acc_hash_3 acc_hash_2 acc_hash_1 acc_hash_0

                recurse


        "
        )
    }

    fn rust_shadowing(
        &self,
        stack: &mut Vec<BFieldElement>,
        _std_in: Vec<BFieldElement>,
        secret_in: Vec<BFieldElement>,
        memory: &mut HashMap<BFieldElement, BFieldElement>,
    ) {
        // BEFORE: _ *peaks leaf_count_hi leaf_count_lo [digest (leaf_digest)]
        // AFTER:  _ leaf_index_hi leaf_index_lo validation_result
        let mut leaf_digest_values = [BFieldElement::new(0); DIGEST_LENGTH];
        for elem in leaf_digest_values.iter_mut() {
            *elem = stack.pop().unwrap();
        }

        let leaf_digest = Digest::new(leaf_digest_values);

        let leaf_count_lo: u32 = stack.pop().unwrap().try_into().unwrap();
        let leaf_count_hi: u32 = stack.pop().unwrap().try_into().unwrap();
        let leaf_count: u64 = ((leaf_count_hi as u64) << 32) + leaf_count_lo as u64;

        let list_get = match self.list_type {
            ListType::Safe => rust_shadowing_helper_functions::safe_list::safe_list_get,
            ListType::Unsafe => rust_shadowing_helper_functions::unsafe_list::unsafe_list_get,
        };
        let peaks_pointer = stack.pop().unwrap();
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

        let mut secret_in_cursor = 0;
        let leaf_index_hi: u32 = secret_in[secret_in_cursor].value().try_into().unwrap();
        secret_in_cursor += 1;
        let leaf_index_lo: u32 = secret_in[secret_in_cursor].value().try_into().unwrap();
        secret_in_cursor += 1;

        let leaf_index: u64 = ((leaf_index_hi as u64) << 32) + leaf_index_lo as u64;

        let (mut mt_index, _peak_index) =
            leaf_index_to_mt_index_and_peak_index(leaf_index, leaf_count);

        let mut auth_path: Vec<Digest> = vec![];
        while mt_index != 1 {
            let mut digest_values = [BFieldElement::zero(); DIGEST_LENGTH];
            for i in 0..DIGEST_LENGTH {
                digest_values[DIGEST_LENGTH - 1 - i] = secret_in[secret_in_cursor];
                secret_in_cursor += 1;
            }

            let digest = Digest::new(digest_values);
            auth_path.push(digest);
            mt_index /= 2;
        }

        let valid_mp = MmrMembershipProof::<VmHasher>::new(leaf_index, auth_path)
            .verify(&peaks, &leaf_digest, leaf_count)
            .0;

        // stack.push(BFieldElement::new(valid_mp as u64));
        // stack.push(auth_path_pointer);
        stack.push(BFieldElement::new(leaf_index_hi as u64));
        stack.push(BFieldElement::new(leaf_index_lo as u64));
        stack.push(BFieldElement::new(valid_mp as u64));
    }

    fn common_case_input_state(&self) -> ExecutionState {
        self.prepare_state_for_benchmark(31, 20)
    }

    fn worst_case_input_state(&self) -> ExecutionState {
        self.prepare_state_for_benchmark(62, 20)
    }
}

#[cfg(test)]
mod tests {
    use twenty_first::{
        test_shared::mmr::get_empty_rustyleveldb_ammr,
        util_types::algebraic_hasher::AlgebraicHasher,
    };

    use crate::{
        mmr::MAX_MMR_HEIGHT,
        test_helpers::{test_rust_equivalence_given_input_state, test_rust_equivalence_multiple},
        VmHasher,
    };

    use super::*;

    #[test]
    fn verify_from_secret_in_test_unsafe_lists() {
        test_rust_equivalence_multiple(
            &MmrVerifyLeafMembershipFromSecretIn {
                list_type: ListType::Unsafe,
            },
            true,
        );
    }

    #[test]
    fn verify_from_secret_in_test_safe_lists() {
        test_rust_equivalence_multiple(
            &MmrVerifyLeafMembershipFromSecretIn {
                list_type: ListType::Safe,
            },
            true,
        );
    }

    #[cfg(test)]
    mod benches {
        use super::*;
        use crate::snippet_bencher::bench_and_write;

        #[test]
        fn verify_from_secret_in_benchmark_unsafe_lists() {
            bench_and_write(MmrVerifyLeafMembershipFromSecretIn {
                list_type: ListType::Unsafe,
            });
        }

        #[test]
        fn verify_from_secret_in_benchmark_safe_lists() {
            bench_and_write(MmrVerifyLeafMembershipFromSecretIn {
                list_type: ListType::Safe,
            });
        }
    }

    #[test]
    fn mmra_ap_verify_test_one() {
        let digest0 = VmHasher::hash(&BFieldElement::new(4545));
        let mut archival_mmr: ArchivalMmr<VmHasher, _> = get_empty_rustyleveldb_ammr();
        archival_mmr.append(digest0);
        let mut mmr = archival_mmr.to_accumulator();
        let leaf_index = 0;
        prop_verify_from_secret_in(&mut mmr, digest0, leaf_index, vec![], true);
    }

    #[test]
    fn mmra_ap_verify_test_two() {
        let digest0 = VmHasher::hash(&BFieldElement::new(123));
        let digest1 = VmHasher::hash(&BFieldElement::new(456));

        let mut archival_mmr: ArchivalMmr<VmHasher, _> = get_empty_rustyleveldb_ammr();
        archival_mmr.append(digest0);
        archival_mmr.append(digest1);
        let mut mmr = archival_mmr.to_accumulator();

        let leaf_index_0 = 0;
        prop_verify_from_secret_in(&mut mmr, digest0, leaf_index_0, vec![digest1], true);

        let leaf_index_1 = 1;
        prop_verify_from_secret_in(&mut mmr, digest1, leaf_index_1, vec![digest0], true);
    }

    #[test]
    fn mmra_ap_verify_test_pbt() {
        let max_size = 19;

        for leaf_count in 0..max_size {
            let digests: Vec<Digest> = random_elements(leaf_count);
            let archival_mmr: ArchivalMmr<VmHasher, _> =
                get_rustyleveldb_ammr_from_digests(digests.clone());
            let mut mmr = archival_mmr.to_accumulator();

            let bad_leaf: Digest = thread_rng().gen();
            for (leaf_index, leaf_digest) in digests.into_iter().enumerate() {
                let (auth_path, _) = archival_mmr.prove_membership(leaf_index as u64);

                // Positive test
                prop_verify_from_secret_in(
                    &mut mmr,
                    leaf_digest,
                    leaf_index as u64,
                    auth_path.authentication_path.clone(),
                    true,
                );

                // Negative test
                prop_verify_from_secret_in(
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
        for init_leaf_count in [
            (1u64 << 40) + (1 << 21) + 510,
            (1 << 32) - 1,
            (1 << 61) - 3,
            (1 << 61) - 2,
            (1 << 61) - 1,
            1 << 61,
        ] {
            // let init_peak_count = 10; // 1 + 1 + 8
            let init_peak_count = init_leaf_count.count_ones();
            println!("init_peak_count = {init_peak_count}");

            // We can't construct this large archival MMRs, so we have to handle it with an MMRA
            // and handle the membership proofs ourselves
            let fake_peaks: Vec<Digest> = random_elements(init_peak_count as usize);
            let mut mmr: MmrAccumulator<VmHasher> =
                MmrAccumulator::init(fake_peaks, init_leaf_count);

            // Insert the 1st leaf
            let second_to_last_leaf: Digest = thread_rng().gen();
            let second_to_last_leaf_index = init_leaf_count;
            let mut real_membership_proof_second_to_last = mmr.append(second_to_last_leaf);
            assert_eq!(
                real_membership_proof_second_to_last.leaf_index,
                second_to_last_leaf_index
            );

            // Insert one more leaf and update the existing membership proof
            let last_leaf: Digest = thread_rng().gen();
            let last_leaf_index = second_to_last_leaf_index + 1;
            MmrMembershipProof::update_from_append(
                &mut real_membership_proof_second_to_last,
                init_leaf_count + 1,
                &last_leaf,
                &mmr.get_peaks(),
            );
            let real_membership_proof_last = mmr.append(last_leaf);

            // Positive tests
            prop_verify_from_secret_in(
                &mut mmr,
                second_to_last_leaf,
                second_to_last_leaf_index,
                real_membership_proof_second_to_last
                    .authentication_path
                    .clone(),
                true,
            );
            prop_verify_from_secret_in(
                &mut mmr,
                last_leaf,
                last_leaf_index,
                real_membership_proof_last.authentication_path.clone(),
                true,
            );

            // Negative tests
            let bad_leaf: Digest = thread_rng().gen();
            prop_verify_from_secret_in(
                &mut mmr,
                bad_leaf,
                second_to_last_leaf_index,
                real_membership_proof_second_to_last.authentication_path,
                false,
            );
            prop_verify_from_secret_in(
                &mut mmr,
                bad_leaf,
                last_leaf_index,
                real_membership_proof_last.authentication_path,
                false,
            );
        }
    }

    // BEFORE: _ *peaks leaf_count_hi leaf_count_lo [digest (leaf_digest)]
    // AFTER:  _ leaf_index_hi leaf_index_lo validation_result
    fn prop_verify_from_secret_in<H: AlgebraicHasher>(
        mmr: &mut MmrAccumulator<H>,
        leaf: Digest,
        leaf_index: u64,
        auth_path: Vec<Digest>,
        expect_validation_success: bool,
    ) {
        let mut init_stack = get_init_tvm_stack();

        let peaks_pointer = BFieldElement::one();
        init_stack.push(peaks_pointer);

        let leaf_count: u64 = mmr.count_leaves();
        let leaf_count_hi = BFieldElement::new(leaf_count >> 32);
        let leaf_count_lo = BFieldElement::new(leaf_count & u32::MAX as u64);
        init_stack.push(leaf_count_hi);
        init_stack.push(leaf_count_lo);

        println!("leaf_count = {leaf_count}");
        println!("leaf_index = {leaf_index}");

        // push digests such that element 0 of digest is on top of stack
        for value in leaf.values().iter().rev() {
            init_stack.push(*value);
        }

        // Initialize memory with peaks list
        let mut memory: HashMap<BFieldElement, BFieldElement> = HashMap::default();
        rust_shadowing_helper_functions::unsafe_list::unsafe_list_new(peaks_pointer, &mut memory);
        for peak in mmr.get_peaks() {
            rust_shadowing_helper_functions::unsafe_list::unsafe_list_push(
                peaks_pointer,
                peak.values().to_vec(),
                &mut memory,
                DIGEST_LENGTH,
            );
        }

        let leaf_index_hi = BFieldElement::new(leaf_index >> 32);
        let leaf_index_lo = BFieldElement::new(leaf_index & u32::MAX as u64);
        let mut expected_final_stack = get_init_tvm_stack();
        expected_final_stack.push(leaf_index_hi);
        expected_final_stack.push(leaf_index_lo);
        expected_final_stack.push(BFieldElement::new(expect_validation_success as u64));

        // Populate secret-in with leaf index and the provided authentication path
        let mut secret_in: Vec<BFieldElement> = vec![];
        secret_in.push(leaf_index_hi);
        secret_in.push(leaf_index_lo);
        for ap_element in auth_path.iter() {
            let mut ap_element_values = ap_element.values().to_vec();
            for _ in 0..DIGEST_LENGTH {
                secret_in.push(ap_element_values.pop().unwrap());
            }
        }

        let snippet_with_unsafe_lists = MmrVerifyLeafMembershipFromSecretIn {
            list_type: ListType::Unsafe,
        };
        test_rust_equivalence_given_input_state(
            &snippet_with_unsafe_lists,
            &init_stack,
            &[],
            &secret_in,
            &mut memory,
            MAX_MMR_HEIGHT * DIGEST_LENGTH + 1,
            Some(&expected_final_stack),
        );

        // Sanity check
        assert_eq!(
            expect_validation_success,
            MmrMembershipProof::<H>::new(leaf_index, auth_path)
                .verify(&mmr.get_peaks(), &leaf, leaf_count)
                .0
        );
    }
}
