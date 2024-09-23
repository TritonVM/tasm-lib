use std::collections::HashMap;

use itertools::Itertools;
use num::One;
use rand::prelude::*;
use triton_vm::prelude::*;
use triton_vm::twenty_first::prelude::MmrMembershipProof;
use twenty_first::math::other::random_elements;
use twenty_first::util_types::mmr::mmr_accumulator::util::mmra_with_mps;
use twenty_first::util_types::mmr::mmr_accumulator::MmrAccumulator;
use twenty_first::util_types::mmr::mmr_trait::Mmr;

use super::leaf_index_to_mt_index_and_peak_index::MmrLeafIndexToMtIndexAndPeakIndex;
use super::MAX_MMR_HEIGHT;
use crate::arithmetic::u64::div2_u64::Div2U64;
use crate::data_type::DataType;
use crate::empty_stack;
use crate::hashing::eq_digest::EqDigest;
use crate::hashing::swap_digest::SwapDigest;
use crate::library::Library;
use crate::list::get::Get;
use crate::rust_shadowing_helper_functions;
use crate::traits::deprecated_snippet::DeprecatedSnippet;
use crate::Digest;
use crate::InitVmState;

#[derive(Debug, Default, Copy, Clone, Eq, PartialEq, Hash)]
pub struct MmrVerifyFromMemory;

impl MmrVerifyFromMemory {
    fn prepare_vm_state(
        &self,
        mmr: &MmrAccumulator,
        leaf: Digest,
        leaf_index: u64,
        auth_path: Vec<Digest>,
    ) -> InitVmState {
        // BEFORE: _ *peaks leaf_count_hi leaf_count_lo leaf_index_hi leaf_index_lo [digest (leaf_digest)] *auth_path
        // AFTER:  _ validation_result
        let mut stack = empty_stack();

        let peaks_pointer = BFieldElement::one();
        stack.push(peaks_pointer);

        let leaf_count: u64 = mmr.num_leafs();
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
        let auth_path_pointer = BFieldElement::new((MAX_MMR_HEIGHT * Digest::LEN + 1) as u64);
        stack.push(auth_path_pointer);

        // Initialize memory
        let mut memory: HashMap<BFieldElement, BFieldElement> = HashMap::default();
        rust_shadowing_helper_functions::list::list_new(peaks_pointer, &mut memory);

        for peak in mmr.peaks() {
            rust_shadowing_helper_functions::list::list_push(
                peaks_pointer,
                peak.values().to_vec(),
                &mut memory,
                Digest::LEN,
            );
        }

        rust_shadowing_helper_functions::list::list_new(auth_path_pointer, &mut memory);
        for ap_element in auth_path.iter() {
            rust_shadowing_helper_functions::list::list_push(
                auth_path_pointer,
                ap_element.values().to_vec(),
                &mut memory,
                Digest::LEN,
            );
        }

        InitVmState::with_stack_and_memory(stack, memory)
    }
}

impl DeprecatedSnippet for MmrVerifyFromMemory {
    fn entrypoint_name(&self) -> String {
        "tasmlib_mmr_verify_from_memory".into()
    }

    fn input_field_names(&self) -> Vec<String> {
        vec![
            "*peaks".to_string(),
            "leaf_count_hi".to_string(),
            "leaf_count_lo".to_string(),
            "leaf_index_hi".to_string(),
            "leaf_index_lo".to_string(),
            "leaf_digest_4".to_string(),
            "leaf_digest_3".to_string(),
            "leaf_digest_2".to_string(),
            "leaf_digest_1".to_string(),
            "leaf_digest_0".to_string(),
            "*auth_path".to_string(),
        ]
    }

    fn input_types(&self) -> Vec<DataType> {
        vec![
            DataType::List(Box::new(DataType::Digest)),
            DataType::U64,
            DataType::U64,
            DataType::Digest,
            DataType::List(Box::new(DataType::Digest)),
        ]
    }

    fn output_field_names(&self) -> Vec<String> {
        vec!["validation_result".to_string()]
    }

    fn output_types(&self) -> Vec<crate::data_type::DataType> {
        vec![DataType::Bool]
    }

    fn stack_diff(&self) -> isize {
        -10
    }

    fn function_code(&self, library: &mut Library) -> String {
        let entrypoint = self.entrypoint_name();

        let leaf_index_to_mt_index = library.import(Box::new(MmrLeafIndexToMtIndexAndPeakIndex));
        let get_list_element = library.import(Box::new(Get::new(DataType::Digest)));
        let div_2 = library.import(Box::new(Div2U64));
        let swap_digests = library.import(Box::new(SwapDigest));
        let eq_digest = library.import(Box::new(EqDigest));
        let loop_label = format!("{entrypoint}_loop");

        let u32_is_even = triton_asm!(
            // _ val

            push 1
            and
            // _ is_odd

            push 0
            eq
            // _ is_even
        );

        let loop_code = triton_asm!(
                // BEFORE/AFTER: _ *auth_path[n] peak_index mt_index_hi mt_index_lo [digest (acc_hash)]
                {loop_label}:
                    dup 6 push 0 eq
                    dup 6 push 1 eq
                    mul
                    // _ *auth_path[n] peak_index mt_index_hi mt_index_lo [digest (acc_hash)] (mt_index == 1)

                    skiz return
                    // _ *auth_path[n] peak_index mt_index_hi mt_index_lo [digest (acc_hash)]

                    // declare `ap_element = auth_path[i]`
                    dup 8
                    read_mem {Digest::LEN}
                    push {Digest::LEN * 2}
                    add
                    swap 14
                    pop 1
                    // _ *auth_path[n+1] peak_index mt_index_hi mt_index_lo [digest (acc_hash)] [digest (ap_element)]

                    dup 10
                    {&u32_is_even}
                    // _ *auth_path[n+1] peak_index mt_index_hi mt_index_lo [digest (acc_hash)] [digest (ap_element)] (mt_index % 2 == 0)

                    skiz call {swap_digests}
                    // _ *auth_path[n+1] peak_index mt_index_hi mt_index_lo [digest (right_node)] [digest (left_node)]

                    hash
                    // _ *auth_path[n+1] peak_index mt_index_hi mt_index_lo [digest (acc_hash)]

                    // mt_index -> mt_index / 2
                    swap 6 swap 1 swap 5
                    call {div_2}
                    swap 5 swap 1 swap 6
                    // _ *auth_path[n+1] peak_index (mt_index / 2)_hi (mt_index / 2)_lo acc_hash_4 acc_hash_3 acc_hash_2 acc_hash_1 acc_hash_0

                    // Rename: (mt_index / 2) -> mt_index
                    // _ *auth_path[n+1] peak_index mt_index_hi mt_index_lo [digest (acc_hash)]

                    recurse
        );

        triton_asm!(
                // BEFORE: _ *peaks leaf_count_hi leaf_count_lo leaf_index_hi leaf_index_lo [digest (leaf_digest)] *auth_path
                // AFTER:  _ validation_result
                // Will crash if `leaf_index >= leaf_count`
                {entrypoint}:
                    dup 9 dup 9 dup 9 dup 9
                    call {leaf_index_to_mt_index}
                    // stack: _ *peaks leaf_count_hi leaf_count_lo leaf_index_hi leaf_index_lo [digest (leaf_digest)] *auth_path mt_index_hi mt_index_lo peak_index

                    swap 7
                    swap 3
                    swap 8
                    swap 4
                    swap 6
                    swap 2
                    swap 6
                    swap 1
                    swap 5
                    swap 1
                    // stack: _ *peaks leaf_count_hi leaf_count_lo leaf_index_hi leaf_index_lo *auth_path peak_index mt_index_hi mt_index_lo [digest (leaf_digest)]

                    // rename: leaf_digest -> acc_hash
                    // stack: _ *peaks leaf_count_hi leaf_count_lo leaf_index_hi leaf_index_lo *auth_path peak_index mt_index_hi mt_index_lo [digest (acc_hash)]

                    swap 8
                    push {Digest::LEN}
                    add
                    swap 8
                    // _ *peaks leaf_count_hi leaf_count_lo leaf_index_hi leaf_index_lo *auth_path[0] peak_index mt_index_hi mt_index_lo [digest (acc_hash)]

                    call {loop_label}
                    // _ *peaks leaf_count_hi leaf_count_lo leaf_index_hi leaf_index_lo *auth_path peak_index mt_index_hi mt_index_lo [digest (acc_hash)]

                    // Compare `acc_hash` to the `expected_peak`, where `expected_peak = peaks[peak_index]`
                    dup 13 dup 8
                    // _ *peaks leaf_count_hi leaf_count_lo leaf_index_hi leaf_index_lo *auth_path peak_index mt_index_hi mt_index_lo [digest (acc_hash)] *peaks peak_index

                    call {get_list_element}
                    // _ *peaks leaf_count_hi leaf_count_lo leaf_index_hi leaf_index_lo *auth_path peak_index mt_index_hi mt_index_lo [digest (acc_hash)] [digest (expected_peak)]

                    // Compare top two digests
                    call {eq_digest}
                    // _ *peaks leaf_count_hi leaf_count_lo leaf_index_hi leaf_index_lo *auth_path peak_index mt_index_hi mt_index_lo (expected_peak == acc_hash)

                    // Rename: expected_peak == acc_hash -> validation_result
                    // _ *peaks leaf_count_hi leaf_count_lo leaf_index_hi leaf_index_lo *auth_path peak_index mt_index_hi mt_index_lo validation_result

                    swap 9
                    pop 5
                    pop 4
                    // _ validation_result

                    return

                    {&loop_code}

        )
        .iter()
        .join("\n")
    }

    fn crash_conditions(&self) -> Vec<String> {
        vec![
            "leaf_index >= leaf_count".to_string(),
            "leaf_index values not u32s".to_string(),
        ]
    }

    fn gen_input_states(&self) -> Vec<crate::InitVmState> {
        let mut rng = thread_rng();
        let max_size = 100;
        let size = rng.gen_range(1..max_size);
        let digests: Vec<Digest> = random_elements(size);
        let leaf_index = rng.gen_range(0..size);
        let leaf = digests[leaf_index];
        let (mmra, mps) = mmra_with_mps(size as u64, vec![(leaf_index as u64, leaf)]);
        let ret0 = self.prepare_vm_state(
            &mmra,
            leaf,
            leaf_index as u64,
            mps[0].authentication_path.clone(),
        );

        vec![ret0]
    }

    fn common_case_input_state(&self) -> InitVmState {
        let log2_size = 31;
        let leaf_count_after_add = 1u64 << log2_size;
        let peaks: Vec<Digest> = random_elements(log2_size as usize);
        let mut mmra = MmrAccumulator::init(peaks, leaf_count_after_add - 1);
        let new_leaf: Digest = random();
        let mp = mmra.append(new_leaf);
        let auth_path = mp.authentication_path;

        // Sanity check of length of auth path
        assert_eq!(log2_size, auth_path.len() as u64);
        self.prepare_vm_state(&mmra, new_leaf, leaf_count_after_add - 1, auth_path)
    }

    fn worst_case_input_state(&self) -> InitVmState {
        let log2_size = 62;
        let leaf_count_after_add = 1u64 << log2_size;
        let peaks: Vec<Digest> = random_elements(log2_size as usize);
        let mut mmra = MmrAccumulator::init(peaks, leaf_count_after_add - 1);
        let new_leaf: Digest = random();
        let mp = mmra.append(new_leaf);
        let auth_path = mp.authentication_path;

        // Sanity check of length of auth path
        assert_eq!(log2_size, auth_path.len() as u64);
        self.prepare_vm_state(&mmra, new_leaf, leaf_count_after_add - 1, auth_path)
    }

    fn rust_shadowing(
        &self,
        stack: &mut Vec<BFieldElement>,
        _std_in: Vec<BFieldElement>,
        _secret_in: Vec<BFieldElement>,
        memory: &mut HashMap<BFieldElement, BFieldElement>,
    ) {
        // BEFORE: _ *peaks leaf_count_hi leaf_count_lo leaf_index_hi leaf_index_lo [digest (leaf_digest)] *auth_path
        // AFTER:  _ validation_result
        let auth_path_pointer = stack.pop().unwrap();

        let list_get = rust_shadowing_helper_functions::list::list_get;
        let auth_path_length = memory[&auth_path_pointer].value();
        let mut auth_path: Vec<Digest> = vec![];
        for i in 0..auth_path_length {
            let digest = Digest::new(
                list_get(auth_path_pointer, i as usize, memory, Digest::LEN)
                    .try_into()
                    .unwrap(),
            );
            auth_path.push(digest);
        }

        let mut new_leaf_digest_values = [BFieldElement::new(0); Digest::LEN];
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
            let digest = Digest::new(
                list_get(peaks_pointer, i as usize, memory, Digest::LEN)
                    .try_into()
                    .unwrap(),
            );
            peaks.push(digest);
        }

        let valid_mp =
            MmrMembershipProof::new(auth_path).verify(leaf_index, leaf_digest, &peaks, leaf_count);

        stack.push(BFieldElement::new(valid_mp as u64));
    }
}

#[cfg(test)]
mod tests {
    use itertools::Itertools;
    use twenty_first::prelude::AlgebraicHasher;

    use super::*;
    use crate::empty_stack;
    use crate::test_helpers::test_rust_equivalence_given_input_values_deprecated;
    use crate::test_helpers::test_rust_equivalence_multiple_deprecated;

    #[test]
    fn verify_from_memory_test() {
        test_rust_equivalence_multiple_deprecated(&MmrVerifyFromMemory, true);
    }

    // This will crash the VM because leaf?index is not strictly less than leaf_count
    #[test]
    #[should_panic]
    fn mmra_ap_verify_test_empty() {
        let digest0 = Tip5::hash(&BFieldElement::new(4545));
        let mmr = MmrAccumulator::new_from_leafs(vec![]);
        let leaf_index = 0;
        prop_verify_from_memory(&mmr, digest0, leaf_index, vec![], false);
    }

    #[test]
    fn mmra_ap_verify_test_one() {
        let digest0 = Tip5::hash(&BFieldElement::new(4545));
        let mut mmr = MmrAccumulator::new_from_leafs(vec![]);
        mmr.append(digest0);
        let leaf_index = 0;
        prop_verify_from_memory(&mmr, digest0, leaf_index, vec![], true);
    }

    #[test]
    fn mmra_ap_verify_test_two() {
        let digest0 = Tip5::hash(&BFieldElement::new(123));
        let digest1 = Tip5::hash(&BFieldElement::new(456));

        let mut mmr = MmrAccumulator::new_from_leafs(vec![]);
        mmr.append(digest0);
        mmr.append(digest1);

        let leaf_index_0 = 0;
        prop_verify_from_memory(&mmr, digest0, leaf_index_0, vec![digest1], true);

        let leaf_index_1 = 1;
        prop_verify_from_memory(&mmr, digest1, leaf_index_1, vec![digest0], true);
    }

    #[test]
    fn mmra_ap_verify_test_pbt() {
        let max_size = 19;

        for leaf_count in 0..max_size {
            let digests: Vec<Digest> = random_elements(leaf_count);

            let (mmr, mps) = mmra_with_mps(
                leaf_count as u64,
                digests
                    .iter()
                    .cloned()
                    .enumerate()
                    .map(|(i, d)| (i as u64, d))
                    .collect_vec(),
            );

            let bad_leaf: Digest = thread_rng().gen();
            for (leaf_index, leaf_digest) in digests.into_iter().enumerate() {
                let auth_path = mps[leaf_index].clone();

                // Positive test
                prop_verify_from_memory(
                    &mmr,
                    leaf_digest,
                    leaf_index as u64,
                    auth_path.authentication_path.clone(),
                    true,
                );

                // Negative tests
                let bad_index = (leaf_index + 1) % leaf_count;
                if bad_index != leaf_index {
                    prop_verify_from_memory(
                        &mmr,
                        leaf_digest,
                        bad_index as u64,
                        auth_path.authentication_path.clone(),
                        false,
                    );
                }
                prop_verify_from_memory(
                    &mmr,
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
            let mut mmr = MmrAccumulator::init(fake_peaks, init_leaf_count);

            // Insert the 1st leaf
            let second_to_last_leaf: Digest = thread_rng().gen();
            let second_to_last_leaf_index = init_leaf_count;
            let mut real_membership_proof_second_to_last = mmr.append(second_to_last_leaf);

            // Insert one more leaf and update the existing membership proof
            let last_leaf: Digest = thread_rng().gen();
            let last_leaf_index = second_to_last_leaf_index + 1;
            MmrMembershipProof::update_from_append(
                &mut real_membership_proof_second_to_last,
                second_to_last_leaf_index,
                mmr.num_leafs(),
                last_leaf,
                &mmr.peaks(),
            );
            let real_membership_proof_last = mmr.append(last_leaf);

            // Positive tests
            prop_verify_from_memory(
                &mmr,
                second_to_last_leaf,
                second_to_last_leaf_index,
                real_membership_proof_second_to_last
                    .authentication_path
                    .clone(),
                true,
            );
            prop_verify_from_memory(
                &mmr,
                last_leaf,
                last_leaf_index,
                real_membership_proof_last.authentication_path.clone(),
                true,
            );

            // Negative tests
            let bad_leaf: Digest = thread_rng().gen();
            prop_verify_from_memory(
                &mmr,
                bad_leaf,
                second_to_last_leaf_index,
                real_membership_proof_second_to_last.authentication_path,
                false,
            );
            prop_verify_from_memory(
                &mmr,
                bad_leaf,
                last_leaf_index,
                real_membership_proof_last.authentication_path,
                false,
            );
        }
    }

    fn prop_verify_from_memory(
        mmr: &MmrAccumulator,
        leaf: Digest,
        leaf_index: u64,
        auth_path: Vec<Digest>,
        expect_validation_success: bool,
    ) {
        let snippet = MmrVerifyFromMemory;
        let exec_state = snippet.prepare_vm_state(mmr, leaf, leaf_index, auth_path.clone());

        let init_stack = exec_state.stack;

        // AFTER: _ *auth_path leaf_index_hi leaf_index_lo validation_result
        let mut expected_final_stack = empty_stack();
        expected_final_stack.push(BFieldElement::new(expect_validation_success as u64));

        test_rust_equivalence_given_input_values_deprecated(
            &snippet,
            &init_stack,
            &[],
            exec_state.nondeterminism.ram,
            Some(&expected_final_stack),
        );

        // Verify that auth path expectation was correct
        assert_eq!(
            expect_validation_success,
            MmrMembershipProof::new(auth_path).verify(
                leaf_index,
                leaf,
                &mmr.peaks(),
                mmr.num_leafs(),
            )
        );
    }
}

#[cfg(test)]
mod benches {
    use super::*;
    use crate::snippet_bencher::bench_and_write;

    #[test]
    fn verify_from_memory_benchmark() {
        bench_and_write(MmrVerifyFromMemory);
    }
}
