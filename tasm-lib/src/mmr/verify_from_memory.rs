use std::collections::HashMap;

use triton_vm::prelude::*;

use super::leaf_index_to_mt_index_and_peak_index::MmrLeafIndexToMtIndexAndPeakIndex;
use crate::hashing::merkle_step_mem_u64_index::MerkleStepMemU64Index;
use crate::list::get::Get;
use crate::prelude::*;
use crate::traits::basic_snippet::Reviewer;
use crate::traits::basic_snippet::SignOffFingerprint;

/// Check whether a given leaf is a member of a pointed-to [MMR], with the
/// inclusion proof (the authentication path) residing in RAM.
///
/// ### Behavior
///
/// ```text
/// BEFORE: _ *peaks [leaf_count: u64] [leaf_index: u64] [leaf: Digest] *auth_path
/// AFTER:  _ [validation_result: bool]
/// ```
///
/// ### Preconditions
///
/// - all input arguments are properly [`BFieldCodec`] encoded
/// - input arguments `*peaks` points to a properly [`BFieldCodec`] encoded list
///   of [`Digest`]s
/// - input arguments `*auth_path` points to a properly [`BFieldCodec`] encoded
///   list of [`Digest`]s
/// - the `leaf_count` is consistent with the pointed-to MMR
/// - the `leaf_index` is smaller than the `leaf_count`
///
/// ### Postconditions
///
/// - all output is properly [`BFieldCodec`] encoded
///
/// [MMR]: twenty_first::util_types::mmr::mmr_accumulator::MmrAccumulator
#[derive(Debug, Default, Copy, Clone, Eq, PartialEq, Hash)]
pub struct MmrVerifyFromMemory;

impl BasicSnippet for MmrVerifyFromMemory {
    fn parameters(&self) -> Vec<(DataType, String)> {
        let list_type = DataType::List(Box::new(DataType::Digest));

        vec![
            (list_type.clone(), "*peaks".to_string()),
            (DataType::U64, "leaf_count".to_string()),
            (DataType::U64, "leaf_index".to_string()),
            (DataType::Digest, "leaf".to_string()),
            (list_type, "*auth_path".to_string()),
        ]
    }

    fn return_values(&self) -> Vec<(DataType, String)> {
        vec![(DataType::Bool, "validation_result".to_string())]
    }

    fn entrypoint(&self) -> String {
        "tasmlib_mmr_verify_from_memory".into()
    }

    fn code(&self, library: &mut Library) -> Vec<LabelledInstruction> {
        let leaf_index_to_mt_index = library.import(Box::new(MmrLeafIndexToMtIndexAndPeakIndex));
        let get_list_element = library.import(Box::new(Get::new(DataType::Digest)));
        let merkle_step_mem = library.import(Box::new(MerkleStepMemU64Index));

        let entrypoint = self.entrypoint();
        let loop_label = format!("{entrypoint}_loop");

        triton_asm!(
            // BEFORE: _ *peaks [leaf_count: u64] [leaf_index: u64] [leaf: Digest] *auth_path
            // AFTER:  _ validation_result
            {entrypoint}:
                pick 9 pick 9
                pick 9 pick 9
                call {leaf_index_to_mt_index}
                // _ *peaks [leaf: Digest] *auth_path [mt_index: u64] peak_index

                place 8
                // _ *peaks peak_index [leaf: Digest] *auth_path [mt_index: u64]

                place 7 place 7
                // _ *peaks peak_index [mt_index: u64] [leaf: Digest] *auth_path

                addi 1
                place 7
                // _ *peaks peak_index *auth_path[0] [mt_index: u64] [leaf: Digest]

                call {loop_label}
                // _ *peaks peak_index *auth_path[n] [1: u64] [peak: Digest]

                /* Compare computed `peak` to `peaks[peak_index]`,
                 * which is the expected peak.
                 */
                pick 9 pick 9
                // _ *auth_path[n] [1: u64] [acc: Digest] *peaks peak_index

                call {get_list_element}
                // _ *auth_path[n] [1: u64] [acc: Digest] [expected_peak: Digest]

                {&DataType::Digest.compare()}
                // _ *auth_path[n] [1: u64] (expected_peak == acc_hash)
                // _ *auth_path[n] [1: u64] validation_result

                place 3
                pop 3
                // _ validation_result

                return

            // INVARIANT: _ *auth_path[n] [mt_index: u64] [acc: Digest]
            {loop_label}:
                dup 6 push 0 eq
                dup 6 push 1 eq
                mul
                // _ *auth_path[n] [mt_index: u64] [acc: Digest] (mt_index == 1)

                skiz return
                // _ *auth_path[n] [mt_index: u64] [acc: Digest]

                call {merkle_step_mem}
                recurse
        )
    }

    fn sign_offs(&self) -> HashMap<Reviewer, SignOffFingerprint> {
        let mut sign_offs = HashMap::new();
        sign_offs.insert(Reviewer("ferdinand"), 0x79df3fecee2c597.into());
        sign_offs
    }
}

#[cfg(test)]
mod tests {
    use itertools::Itertools;
    use rand::prelude::*;
    use twenty_first::math::other::random_elements;
    use twenty_first::util_types::mmr::mmr_accumulator::util::mmra_with_mps;

    use super::*;
    use crate::empty_stack;
    use crate::mmr::MAX_MMR_HEIGHT;
    use crate::test_prelude::*;
    use crate::twenty_first::prelude::*;
    use crate::twenty_first::util_types::mmr::mmr_accumulator::MmrAccumulator;

    impl MmrVerifyFromMemory {
        fn set_up_initial_state(
            &self,
            mmr: &MmrAccumulator,
            leaf: Digest,
            leaf_index: u64,
            auth_path: Vec<Digest>,
        ) -> FunctionInitialState {
            let peaks_pointer = bfe!(1);
            let auth_path_pointer = bfe!(MAX_MMR_HEIGHT * Digest::LEN + 1);

            let mut stack = self.init_stack_for_isolated_run();
            stack.push(peaks_pointer);
            push_encodable(&mut stack, &mmr.num_leafs());
            push_encodable(&mut stack, &leaf_index);
            push_encodable(&mut stack, &leaf);
            stack.push(auth_path_pointer);

            let mut memory = HashMap::default();
            encode_to_memory(&mut memory, peaks_pointer, &mmr.peaks());
            encode_to_memory(&mut memory, auth_path_pointer, &auth_path);

            FunctionInitialState { stack, memory }
        }
    }

    impl Function for MmrVerifyFromMemory {
        fn rust_shadow(
            &self,
            stack: &mut Vec<BFieldElement>,
            memory: &mut HashMap<BFieldElement, BFieldElement>,
        ) {
            let auth_path_pointer = stack.pop().unwrap();
            let leaf = pop_encodable(stack);
            let leaf_index = pop_encodable(stack);
            let leaf_count = pop_encodable(stack);
            let peaks_pointer = stack.pop().unwrap();

            let auth_path = *Vec::<Digest>::decode_from_memory(memory, auth_path_pointer).unwrap();
            let peaks = *Vec::<Digest>::decode_from_memory(memory, peaks_pointer).unwrap();

            let proof = MmrMembershipProof::new(auth_path);
            let is_valid = proof.verify(leaf_index, leaf, &peaks, leaf_count);
            push_encodable(stack, &is_valid);
        }

        fn pseudorandom_initial_state(
            &self,
            seed: [u8; 32],
            bench_case: Option<BenchmarkCase>,
        ) -> FunctionInitialState {
            let mut rng = StdRng::from_seed(seed);
            let (leaf_index, leaf_count) = match bench_case {
                Some(BenchmarkCase::CommonCase) => ((1 << 31) - 1, 1 << 31),
                Some(BenchmarkCase::WorstCase) => ((1 << 62) - 1, 1 << 62),
                None => {
                    let leaf_count = rng.random_range(1..=1 << 62);
                    let leaf_index = rng.random_range(0..leaf_count);
                    (leaf_index, leaf_count)
                }
            };

            let leaf = rng.random();
            let (mmra, mps) = mmra_with_mps(leaf_count, vec![(leaf_index, leaf)]);
            let auth_path = mps[0].authentication_path.clone();

            self.set_up_initial_state(&mmra, leaf, leaf_index, auth_path)
        }
    }

    #[test]
    fn rust_shadow() {
        ShadowedFunction::new(MmrVerifyFromMemory).test();
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

            let bad_leaf: Digest = rand::rng().random();
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
            let second_to_last_leaf: Digest = rand::rng().random();
            let second_to_last_leaf_index = init_leaf_count;
            let mut real_membership_proof_second_to_last = mmr.append(second_to_last_leaf);

            // Insert one more leaf and update the existing membership proof
            let last_leaf: Digest = rand::rng().random();
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
            let bad_leaf: Digest = rand::rng().random();
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
        let exec_state =
            MmrVerifyFromMemory.set_up_initial_state(mmr, leaf, leaf_index, auth_path.clone());

        // AFTER: _ validation_result
        let mut expected_final_stack = empty_stack();
        expected_final_stack.push(bfe!(expect_validation_success as u64));

        test_rust_equivalence_given_complete_state(
            &ShadowedFunction::new(MmrVerifyFromMemory),
            &exec_state.stack,
            &[],
            &NonDeterminism::default().with_ram(exec_state.memory),
            &None,
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
    use crate::test_prelude::*;

    #[test]
    fn benchmark() {
        ShadowedFunction::new(MmrVerifyFromMemory).bench();
    }
}
