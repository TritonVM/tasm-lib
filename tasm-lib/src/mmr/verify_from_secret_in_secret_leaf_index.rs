use triton_vm::prelude::*;

use super::leaf_index_to_mt_index_and_peak_index::MmrLeafIndexToMtIndexAndPeakIndex;
use crate::hashing::merkle_step_u64_index::MerkleStepU64Index;
use crate::list::get::Get;
use crate::prelude::*;

/// Verify that a digest is a leaf in the MMR accumulator. Takes both authentication path and
/// leaf index from secret-in. Crashes the VM if the authentication fails.
#[derive(Debug, Default, Copy, Clone, Eq, PartialEq, Hash)]
pub struct MmrVerifyFromSecretInSecretLeafIndex;

impl BasicSnippet for MmrVerifyFromSecretInSecretLeafIndex {
    fn parameters(&self) -> Vec<(DataType, String)> {
        vec![(
            DataType::Tuple(vec![
                DataType::List(Box::new(DataType::Digest)), // *peaks
                DataType::U64,                              // leaf_count
                DataType::Digest,                           // leaf
            ]),
            "*peaks_leaf_count_and_leaf".to_owned(),
        )]
    }

    fn return_values(&self) -> Vec<(DataType, String)> {
        vec![]
    }

    fn entrypoint(&self) -> String {
        "tasmlib_mmr_verify_from_secret_in_secret_leaf_index".into()
    }

    // Already on stack (can be secret of public input): _ *peaks leaf_count_hi leaf_count_lo [digest (leaf)]
    // Secret input: _ (authentication_path: Vec<Digest>), (leaf_digest: Digest), (leaf_index: u64)
    fn code(&self, library: &mut Library) -> Vec<LabelledInstruction> {
        let entrypoint = self.entrypoint();
        let while_loop_label = format!("{entrypoint}_while");

        let leaf_index_to_mt_index = library.import(Box::new(MmrLeafIndexToMtIndexAndPeakIndex));
        let merkle_step_u64_index = library.import(Box::new(MerkleStepU64Index));
        let list_get = library.import(Box::new(Get::new(DataType::Digest)));

        // BEFORE: _ *peaks leaf_count_hi leaf_count_lo [digest (leaf_digest)]
        // AFTER:  _ leaf_index_hi leaf_index_lo validation_result
        triton_asm!(
            {entrypoint}:
                // Read leaf index from secret in
                divine 2
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

                call {while_loop_label}
                // _ leaf_index_hi leaf_index_lo *peaks peak_index mt_index_hi mt_index_lo [digest (acc_hash)]

                // Compare `acc_hash` with `peaks[peak_index]`
                dup 8 dup 8 call {list_get}
                // _ leaf_index_hi leaf_index_lo *peaks peak_index mt_index_hi mt_index_lo [digest (acc_hash)] [digest (peaks[peak_index])]

                assert_vector error_id 20
                // _ leaf_index_hi leaf_index_lo *peaks peak_index mt_index_hi mt_index_lo [digest (acc_hash)]

                pop 5
                pop 5
                pop 1
                // _

                return

            // start/end: _ mt_index_hi mt_index_lo [digest (acc_hash)]
            {while_loop_label}:
                dup 6 dup 6 push 0 push 1 {&DataType::U64.compare()}
                // __ mt_index_hi mt_index_lo [digest (acc_hash)] (mt_index == 1)

                skiz return
                // __ mt_index_hi mt_index_lo [digest (acc_hash)]

                // move up one layer in the Merkle tree
                call {merkle_step_u64_index}

                // _ mt_index_hi (mt_index_lo / 2) [digest (acc_hash)]

                recurse
        )
    }
}

#[cfg(test)]
mod tests {
    use num::One;
    use tasm_lib::test_helpers::test_assertion_failure;
    use twenty_first::math::other::random_elements;
    use twenty_first::util_types::mmr::mmr_accumulator::MmrAccumulator;
    use twenty_first::util_types::mmr::mmr_accumulator::util::mmra_with_mps;
    use twenty_first::util_types::mmr::mmr_membership_proof::MmrMembershipProof;
    use twenty_first::util_types::mmr::mmr_trait::Mmr;
    use twenty_first::util_types::mmr::shared_basic::leaf_index_to_mt_index_and_peak_index;

    use super::*;
    use crate::rust_shadowing_helper_functions;
    use crate::test_helpers::test_rust_equivalence_given_complete_state;
    use crate::test_prelude::*;

    #[test]
    fn prop() {
        for _ in 0..10 {
            ShadowedProcedure::new(MmrVerifyFromSecretInSecretLeafIndex).test();
        }
    }

    #[test]
    fn mmra_ap_verify_test_one() {
        let digest0 = Tip5::hash(&BFieldElement::new(4545));
        let (mmra, _mps) = mmra_with_mps(1u64, vec![(0, digest0)]);
        MmrVerifyFromSecretInSecretLeafIndex.prop_verify_from_secret_in_positive_test(
            &mmra,
            digest0,
            0u64,
            vec![],
        );
    }

    #[test]
    fn mmra_ap_verify_test_two() {
        let digest0 = Tip5::hash(&BFieldElement::new(123));
        let digest1 = Tip5::hash(&BFieldElement::new(456));

        let leaf_count = 2u64;
        let (mmr, _mps) = mmra_with_mps(leaf_count, vec![(0u64, digest0), (1u64, digest1)]);

        let leaf_index_0 = 0;
        MmrVerifyFromSecretInSecretLeafIndex.prop_verify_from_secret_in_positive_test(
            &mmr,
            digest0,
            leaf_index_0,
            vec![digest1],
        );

        let leaf_index_1 = 1;
        MmrVerifyFromSecretInSecretLeafIndex.prop_verify_from_secret_in_positive_test(
            &mmr,
            digest1,
            leaf_index_1,
            vec![digest0],
        );
    }

    #[test]
    fn mmra_ap_verify_test_pbt() {
        let max_size = 19;
        let snippet = MmrVerifyFromSecretInSecretLeafIndex;

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
                snippet.prop_verify_from_secret_in_positive_test(
                    &mmr,
                    leaf_digest,
                    leaf_index as u64,
                    auth_path.authentication_path.clone(),
                );

                // Negative test
                snippet.prop_verify_from_secret_in_negative_test(
                    &mmr,
                    bad_leaf,
                    leaf_index as u64,
                    auth_path.authentication_path,
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
        ] {
            let init_peak_count = init_leaf_count.count_ones();

            // We can't construct this large archival MMRs, so we have to handle it with an MMRA
            // and handle the membership proofs ourselves
            let fake_peaks: Vec<Digest> = random_elements(init_peak_count as usize);
            let mut mmr: MmrAccumulator = MmrAccumulator::init(fake_peaks, init_leaf_count);

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
            MmrVerifyFromSecretInSecretLeafIndex.prop_verify_from_secret_in_positive_test(
                &mmr,
                second_to_last_leaf,
                second_to_last_leaf_index,
                real_membership_proof_second_to_last
                    .authentication_path
                    .clone(),
            );
            MmrVerifyFromSecretInSecretLeafIndex.prop_verify_from_secret_in_positive_test(
                &mmr,
                last_leaf,
                last_leaf_index,
                real_membership_proof_last.authentication_path.clone(),
            );

            // Negative tests
            let bad_leaf: Digest = rand::rng().random();
            MmrVerifyFromSecretInSecretLeafIndex.prop_verify_from_secret_in_negative_test(
                &mmr,
                bad_leaf,
                second_to_last_leaf_index,
                real_membership_proof_second_to_last.authentication_path,
            );
            MmrVerifyFromSecretInSecretLeafIndex.prop_verify_from_secret_in_negative_test(
                &mmr,
                bad_leaf,
                last_leaf_index,
                real_membership_proof_last.authentication_path,
            );
        }
    }

    impl Procedure for MmrVerifyFromSecretInSecretLeafIndex {
        fn rust_shadow(
            &self,
            stack: &mut Vec<BFieldElement>,
            memory: &mut HashMap<BFieldElement, BFieldElement>,
            nondeterminism: &NonDeterminism,
            _public_input: &[BFieldElement],
            _sponge: &mut Option<Tip5>,
        ) -> Vec<BFieldElement> {
            let mut leaf_digest = [BFieldElement::new(0); Digest::LEN];
            for elem in leaf_digest.iter_mut() {
                *elem = stack.pop().unwrap();
            }

            let leaf_digest = Digest::new(leaf_digest);
            let leaf_count_lo: u32 = stack.pop().unwrap().try_into().unwrap();
            let leaf_count_hi: u32 = stack.pop().unwrap().try_into().unwrap();
            let leaf_count: u64 = ((leaf_count_hi as u64) << 32) + leaf_count_lo as u64;
            let peaks_pointer = stack.pop().unwrap();
            let peaks_count: u64 = memory[&peaks_pointer].value();
            let mut peaks: Vec<Digest> = vec![];
            for i in 0..peaks_count {
                let digest_innards = rust_shadowing_helper_functions::list::list_get(
                    peaks_pointer,
                    i as usize,
                    memory,
                    Digest::LEN,
                );
                peaks.push(Digest::new(digest_innards.try_into().unwrap()));
            }
            let leaf_index_hi: u32 = nondeterminism.individual_tokens[0]
                .value()
                .try_into()
                .unwrap();
            let leaf_index_lo: u32 = nondeterminism.individual_tokens[1]
                .value()
                .try_into()
                .unwrap();
            let leaf_index: u64 = ((leaf_index_hi as u64) << 32) + leaf_index_lo as u64;
            let (mut mt_index, _peak_index) =
                leaf_index_to_mt_index_and_peak_index(leaf_index, leaf_count);

            let mut auth_path: Vec<Digest> = vec![];
            let mut i = 0;
            while mt_index != 1 {
                auth_path.push(nondeterminism.digests[i]);
                mt_index /= 2;
                i += 1;
            }

            let valid_mp = MmrMembershipProof::new(auth_path).verify(
                leaf_index,
                leaf_digest,
                &peaks,
                leaf_count,
            );

            assert!(valid_mp, "MMR leaf not authenticated");

            vec![]
        }

        fn pseudorandom_initial_state(
            &self,
            seed: [u8; 32],
            bench_case: Option<BenchmarkCase>,
        ) -> ProcedureInitialState {
            let mut rng = StdRng::from_seed(seed);
            let leaf_count = rng.random_range(1..10000);
            let leaf_index = rng.random_range(0..leaf_count);

            match bench_case {
                Some(BenchmarkCase::CommonCase) => {
                    self.prepare_state_for_benchmark(32, (1 << 32) - 1)
                }
                Some(BenchmarkCase::WorstCase) => {
                    self.prepare_state_for_benchmark(62, (1 << 62) - 1)
                }
                None => self.prepare_state_for_tests(leaf_count, leaf_index as u64, true),
            }
        }
    }

    impl MmrVerifyFromSecretInSecretLeafIndex {
        fn prepare_state(
            &self,
            mmr: &MmrAccumulator,
            claimed_leaf: Digest,
            leaf_index: u64,
            auth_path: Vec<Digest>,
        ) -> ProcedureInitialState {
            let ProcedureInitialState {
                mut stack,
                mut nondeterminism,
                public_input: _,
                sponge: _,
            } = self.mmr_to_init_vm_state(mmr);

            // push digests such that element 0 of digest is on top of stack
            for value in claimed_leaf.values().iter().rev() {
                stack.push(*value);
            }

            // Populate secret-in with leaf index and the provided authentication path
            let leaf_index_hi = BFieldElement::new(leaf_index >> 32);
            let leaf_index_lo = BFieldElement::new(leaf_index & u32::MAX as u64);
            nondeterminism.individual_tokens = vec![leaf_index_hi, leaf_index_lo];
            nondeterminism.digests = auth_path;

            ProcedureInitialState {
                stack,
                nondeterminism,
                ..Default::default()
            }
        }

        fn prop_verify_from_secret_in_negative_test(
            &self,
            mmr: &MmrAccumulator,
            claimed_leaf: Digest,
            leaf_index: u64,
            auth_path: Vec<Digest>,
        ) {
            let init_state = self.prepare_state(mmr, claimed_leaf, leaf_index, auth_path.clone());

            test_assertion_failure(
                &ShadowedProcedure::new(MmrVerifyFromSecretInSecretLeafIndex),
                init_state.into(),
                &[20],
            );

            // Sanity check
            assert!(!MmrMembershipProof::new(auth_path).verify(
                leaf_index,
                claimed_leaf,
                &mmr.peaks(),
                mmr.num_leafs()
            ));
        }

        // BEFORE: _ *peaks leaf_count_hi leaf_count_lo [digest (leaf_digest)]
        // AFTER:  _
        fn prop_verify_from_secret_in_positive_test(
            &self,
            mmr: &MmrAccumulator,
            claimed_leaf: Digest,
            leaf_index: u64,
            auth_path: Vec<Digest>,
        ) {
            let init_state = self.prepare_state(mmr, claimed_leaf, leaf_index, auth_path.clone());
            let expected_final_stack = self.init_stack_for_isolated_run();
            test_rust_equivalence_given_complete_state(
                &ShadowedProcedure::new(MmrVerifyFromSecretInSecretLeafIndex),
                &init_state.stack,
                &[],
                &init_state.nondeterminism,
                &None,
                Some(&expected_final_stack),
            );

            // Sanity check
            assert!(MmrMembershipProof::new(auth_path).verify(
                leaf_index,
                claimed_leaf,
                &mmr.peaks(),
                mmr.num_leafs()
            ));
        }

        // BEFORE: _ *peaks [digest (leaf_digest)] leaf_count_hi leaf_count_lo
        // AFTER:  _
        fn prepare_state_for_tests(
            &self,
            size: usize,
            leaf_index: u64,
            generate_valid_proof: bool,
        ) -> ProcedureInitialState {
            let valid_leaf: Digest = rand::random();
            let (mmr, mps) = mmra_with_mps(size as u64, vec![(leaf_index, valid_leaf)]);
            let claimed_leaf = if generate_valid_proof {
                valid_leaf
            } else {
                rand::random()
            };

            self.prepare_state(
                &mmr,
                claimed_leaf,
                leaf_index,
                mps[0].authentication_path.clone(),
            )
        }

        fn prepare_state_for_benchmark(
            &self,
            log_2_leaf_count: u8,
            leaf_index: u64,
        ) -> ProcedureInitialState {
            let leaf_count = 2u64.pow(log_2_leaf_count as u32);
            let peaks: Vec<Digest> = random_elements(log_2_leaf_count as usize);
            let mut mmra = MmrAccumulator::init(peaks, leaf_count - 1);
            let new_leaf: Digest = rand::random();
            let authentication_path = mmra.append(new_leaf).authentication_path;

            let mut vm_init_state = self.mmr_to_init_vm_state(&mmra);

            // Populate secret-in with the leaf index value, which is a u64
            vm_init_state
                .nondeterminism
                .individual_tokens
                .push(BFieldElement::new(leaf_index >> 32));
            vm_init_state
                .nondeterminism
                .individual_tokens
                .push(BFieldElement::new(leaf_index & u32::MAX as u64));

            // Populate secret-in with the an authentication path
            vm_init_state.nondeterminism.digests = authentication_path;

            for value in new_leaf.values().iter().rev() {
                vm_init_state.stack.push(*value);
            }

            vm_init_state
        }

        /// Prepare the part of the state that can be derived from the MMR without
        /// knowing e.g. the leaf index of the leaf digest that you want to authenticate
        /// so this function does not populate e.g. `secret_in`. The caller has to do that.
        fn mmr_to_init_vm_state(&self, mmra: &MmrAccumulator) -> ProcedureInitialState {
            let mut stack: Vec<BFieldElement> = self.init_stack_for_isolated_run();
            let peaks_pointer = BFieldElement::one();
            stack.push(peaks_pointer);

            let leaf_count = mmra.num_leafs();
            let leaf_count_hi = BFieldElement::new(leaf_count >> 32);
            let leaf_count_lo = BFieldElement::new(leaf_count & u32::MAX as u64);
            stack.push(leaf_count_hi);
            stack.push(leaf_count_lo);

            // Write peaks to memory
            let mut memory: HashMap<BFieldElement, BFieldElement> = HashMap::default();
            rust_shadowing_helper_functions::list::list_new(peaks_pointer, &mut memory);

            for peak in mmra.peaks() {
                rust_shadowing_helper_functions::list::list_push(
                    peaks_pointer,
                    peak.values().to_vec(),
                    &mut memory,
                );
            }

            let nondeterminism = NonDeterminism::default().with_ram(memory);
            ProcedureInitialState {
                stack,
                nondeterminism,
                ..Default::default()
            }
        }
    }
}

#[cfg(test)]
mod benches {
    use super::*;
    use crate::test_prelude::*;

    #[test]
    fn benchmark() {
        ShadowedProcedure::new(MmrVerifyFromSecretInSecretLeafIndex).bench();
    }
}
