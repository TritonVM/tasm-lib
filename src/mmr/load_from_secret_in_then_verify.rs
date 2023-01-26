use std::collections::HashMap;

use num::Zero;
use rand::{thread_rng, Rng};
use twenty_first::shared_math::b_field_element::BFieldElement;
use twenty_first::shared_math::other::random_elements;
use twenty_first::shared_math::rescue_prime_digest::{Digest, DIGEST_LENGTH};
use twenty_first::shared_math::rescue_prime_regular::RescuePrimeRegular;
use twenty_first::test_shared::mmr::get_archival_mmr_from_digests;
use twenty_first::util_types::mmr::archival_mmr::ArchivalMmr;
use twenty_first::util_types::mmr::mmr_accumulator::MmrAccumulator;
use twenty_first::util_types::mmr::mmr_membership_proof::MmrMembershipProof;
use twenty_first::util_types::mmr::mmr_trait::Mmr;

use crate::hashing::load_auth_path_from_secret_in::LoadAuthPathFromSecretIn;
use crate::library::Library;
use crate::snippet::{NewSnippet, Snippet};
use crate::{get_init_tvm_stack, rust_shadowing_helper_functions, ExecutionState};

use super::verify_from_memory::MmrVerifyFromMemory;
use super::MAX_MMR_HEIGHT;

/// First load from secret-in, then verify from memory
pub struct MmrLoadFromSecretInThenVerify();

impl NewSnippet for MmrLoadFromSecretInThenVerify {
    fn inputs() -> Vec<&'static str> {
        vec![
            "peaks_pointer",
            "leaf_count_hi",
            "leaf_count_lo",
            "leaf_index_hi",
            "leaf_index_lo",
            "leaf_digest_4",
            "leaf_digest_3",
            "leaf_digest_2",
            "leaf_digest_1",
            "leaf_digest_0",
        ]
    }

    fn outputs() -> Vec<&'static str> {
        vec![
            "auth_path_pointer",
            "leaf_index_hi",
            "leaf_index_lo",
            "validation_result",
        ]
    }

    fn crash_conditions() -> Vec<&'static str> {
        vec!["Leaf index exceeds leaf count"]
    }

    fn gen_input_states() -> Vec<ExecutionState> {
        type H = RescuePrimeRegular;

        /// Prepare the part of the state that can be derived from the MMR without
        /// knowing e.g. the leaf index of the leaf digest that you want to authenticate
        /// so this function does not populate e.g. `secret_in`. The caller has to do that.
        fn mmr_to_init_vm_state(mmra: &mut MmrAccumulator<H>) -> ExecutionState {
            let mut stack: Vec<BFieldElement> = get_init_tvm_stack();
            let peaks_pointer = BFieldElement::zero();
            stack.push(peaks_pointer);

            let leaf_count = mmra.count_leaves() as u64;
            let leaf_count_hi = BFieldElement::new(leaf_count >> 32);
            let leaf_count_lo = BFieldElement::new(leaf_count & u32::MAX as u64);
            stack.push(leaf_count_hi);
            stack.push(leaf_count_lo);

            // Write peaks to memory
            let mut memory: HashMap<BFieldElement, BFieldElement> = HashMap::default();
            rust_shadowing_helper_functions::list_new(peaks_pointer, &mut memory);
            for peak in mmra.get_peaks() {
                rust_shadowing_helper_functions::list_push(
                    peaks_pointer,
                    peak.values(),
                    &mut memory,
                );
            }

            ExecutionState {
                stack,
                std_in: vec![],
                secret_in: vec![],
                memory,
                words_allocated: DIGEST_LENGTH * MAX_MMR_HEIGHT + 1,
            }
        }

        let mut rng = thread_rng();
        let mut init_vm_states = vec![];

        for size in 1..50 {
            // BEFORE: _ *peaks leaf_count_hi leaf_count_lo leaf_index_hi leaf_index_lo [digest (leaf_digest)]
            // AFTER: _ *auth_path leaf_index_hi leaf_index_lo validation_result
            let digests: Vec<Digest> = random_elements(size);
            let mut ammr: ArchivalMmr<H> = get_archival_mmr_from_digests(digests.clone());
            let mut vm_init_state = mmr_to_init_vm_state(&mut ammr.to_accumulator());

            // Populate secret-in with the leaf index value, which is a u64
            let leaf_index: u64 = rng.gen_range(0..size as u64);

            // Populate secret-in with the correct authentication path
            let mmr_mp = ammr.prove_membership(leaf_index as u128).0;
            let authentication_path = mmr_mp.authentication_path;
            vm_init_state
                .secret_in
                .push(BFieldElement::new(authentication_path.len() as u64));
            for ap_element in authentication_path.iter() {
                let mut ap_element_values = ap_element.values().to_vec();
                for _ in 0..DIGEST_LENGTH {
                    vm_init_state
                        .secret_in
                        .push(ap_element_values.pop().unwrap());
                }
            }

            // Push leaf index to stack
            vm_init_state
                .stack
                .push(BFieldElement::new(leaf_index >> 32));
            vm_init_state
                .stack
                .push(BFieldElement::new(leaf_index & u32::MAX as u64));

            // Push the correct leaf on the stack
            let mut positive_test_vm_init_state = vm_init_state.clone();
            let good_leaf = digests[leaf_index as usize];
            for value in good_leaf.values().iter().rev() {
                positive_test_vm_init_state.stack.push(*value);
            }

            init_vm_states.push(positive_test_vm_init_state);

            // Negative test
            // Push the false leaf on the stack
            let mut negative_test_vm_init_state = vm_init_state.clone();
            let bad_leaf = digests[(leaf_index as usize + 1) % size];
            for value in bad_leaf.values().iter().rev() {
                negative_test_vm_init_state.stack.push(*value);
            }

            init_vm_states.push(negative_test_vm_init_state);
        }

        init_vm_states
    }
}

impl Snippet for MmrLoadFromSecretInThenVerify {
    fn stack_diff() -> isize {
        -6
    }

    fn entrypoint() -> &'static str {
        "verify_load_from_secret_in"
    }

    fn function_body(library: &mut Library) -> String {
        let entrypoint = Self::entrypoint();
        let load_auth_path_from_secret_in = library.import::<LoadAuthPathFromSecretIn>();
        let verify_from_memory = library.import::<MmrVerifyFromMemory>();

        format!(
            "
                // BEFORE: _ *peaks leaf_count_hi leaf_count_lo leaf_index_hi leaf_index_lo [digest (leaf_digest)]
                // AFTER: _ *auth_path leaf_index_hi leaf_index_lo validation_result
                {entrypoint}:
                    call {load_auth_path_from_secret_in}
                    // _ *peaks leaf_count_hi leaf_count_lo leaf_index_hi leaf_index_lo [digest (leaf_digest)] *auth_path

                    call {verify_from_memory}
                    // _ *auth_path leaf_index_hi leaf_index_lo validation_result

                    return
                "
        )
    }

    fn rust_shadowing(
        stack: &mut Vec<BFieldElement>,
        _std_in: Vec<BFieldElement>,
        secret_in: Vec<BFieldElement>,
        memory: &mut HashMap<BFieldElement, BFieldElement>,
    ) {
        type H = RescuePrimeRegular;

        // BEFORE: _ *peaks leaf_count_hi leaf_count_lo leaf_index_hi leaf_index_lo [digest (leaf_digest)]
        // AFTER: _ *auth_path leaf_index_hi leaf_index_lo validation_result
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
            let digest = Digest::new(rust_shadowing_helper_functions::list_read(
                peaks_pointer,
                i as usize,
                memory,
            ));
            peaks.push(digest);
        }

        // Read authentication path from secret in, then write it to memory
        let mut secret_in_cursor = 0;
        let total_auth_path_length: u32 = secret_in[secret_in_cursor].value().try_into().unwrap();
        secret_in_cursor += 1;

        let auth_path_pointer = BFieldElement::new((DIGEST_LENGTH * MAX_MMR_HEIGHT + 1) as u64);
        rust_shadowing_helper_functions::list_new(auth_path_pointer, memory);

        let mut i = 0;
        while i != total_auth_path_length {
            let ap_element = rust_shadowing_helper_functions::read_digest_from_secret_in(
                &secret_in,
                &mut secret_in_cursor,
            );
            rust_shadowing_helper_functions::list_push(
                auth_path_pointer,
                ap_element.values(),
                memory,
            );

            i += 1;
        }

        // Read auth path from memory
        let auth_path_length = memory[&auth_path_pointer].value();
        let mut auth_path: Vec<Digest> = vec![];
        for i in 0..auth_path_length {
            let digest = Digest::new(rust_shadowing_helper_functions::list_read(
                auth_path_pointer,
                i as usize,
                memory,
            ));
            auth_path.push(digest);
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_helpers::rust_tasm_equivalence_prop_new;

    #[test]
    fn new_snippet_test() {
        rust_tasm_equivalence_prop_new::<MmrLoadFromSecretInThenVerify>();
    }
}
