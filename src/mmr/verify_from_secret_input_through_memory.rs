use std::collections::HashMap;

use num::Zero;
use twenty_first::{
    shared_math::{
        b_field_element::BFieldElement,
        other::random_elements,
        rescue_prime_digest::Digest,
        rescue_prime_regular::{RescuePrimeRegular, DIGEST_LENGTH},
    },
    test_shared::mmr::get_archival_mmr_from_digests,
    util_types::mmr::{
        archival_mmr::ArchivalMmr, mmr_accumulator::MmrAccumulator,
        mmr_membership_proof::MmrMembershipProof, mmr_trait::Mmr,
    },
};

use crate::{
    get_init_tvm_stack,
    library::Library,
    list::u32::{push::Push, set_length::SetLength},
    rust_shadowing_helper_functions,
    snippet::{NewSnippet, Snippet},
    ExecutionState,
};

use super::{verify_from_memory::MmrVerifyFromMemory, MAX_MMR_HEIGHT};

pub struct MmrVerifyFromSecretInputThroughMemory();

impl NewSnippet for MmrVerifyFromSecretInputThroughMemory {
    fn inputs() -> Vec<&'static str> {
        vec![
            "peaks_pointer",
            "leaf_digest4",
            "leaf_digest3",
            "leaf_digest2",
            "leaf_digest1",
            "leaf_digest0",
            "leaf_count_hi",
            "leaf_count_lo",
        ]
    }

    fn outputs() -> Vec<&'static str> {
        vec!["auth_path_pointer", "validation_result"]
    }

    fn crash_conditions() -> Vec<&'static str> {
        vec!["auth path is not valid"]
    }

    fn gen_input_states() -> Vec<crate::ExecutionState> {
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

            return ExecutionState {
                stack,
                std_in: vec![],
                secret_in: vec![],
                memory,
                words_allocated: 0,
            };
        }

        // BEFORE: _ *peaks [digest (leaf_digest)] leaf_count_hi leaf_count_lo
        // AFTER: _ *auth_path leaf_index_hi leaf_index_lo
        let digests: Vec<Digest> = random_elements(2);
        let mut ammr: ArchivalMmr<H> = get_archival_mmr_from_digests(digests);
        let mut vm_init_state = mmr_to_init_vm_state(&mut ammr.to_accumulator());

        // Populate secret-in with first the authentication path and then the leaf index
        let leaf_index: u64 = 0;
        let mmr_mp = ammr.prove_membership(0).0;
        let authentication_path = mmr_mp.authentication_path;
        let auth_path_length = authentication_path.len();
        let auth_path_length_indicator = BFieldElement::new(auth_path_length as u64);
        vm_init_state.secret_in.push(auth_path_length_indicator);
        for i in 0..auth_path_length {
            let mut digest_elements: Vec<BFieldElement> = authentication_path[i].values().to_vec();
            for _ in 0..DIGEST_LENGTH {
                vm_init_state.secret_in.push(digest_elements.pop().unwrap());
            }
        }

        // Populate secret-in with the leaf index value, which is a u64
        vm_init_state
            .secret_in
            .push(BFieldElement::new(leaf_index >> 32));
        vm_init_state
            .secret_in
            .push(BFieldElement::new(leaf_index & u32::MAX as u64));

        vec![vm_init_state]
    }
}

impl Snippet for MmrVerifyFromSecretInputThroughMemory {
    fn stack_diff() -> isize {
        -5
    }

    fn entrypoint() -> &'static str {
        "verify_from_secret_input_through_memory"
    }

    fn function_body(library: &mut Library) -> String {
        // Allocate memory for the returned auth path for the newly inserted element
        // Warning: This auth path is only allocated *once* even though the code is called multiple times.
        // So if this function is called multiple times, the auth_paths will be overwritten.
        let static_auth_path_pointer = library.kmalloc(DIGEST_LENGTH * MAX_MMR_HEIGHT);
        let verify_auth_path_from_memory = library.import::<MmrVerifyFromMemory>();
        let set_length = library.import::<SetLength>();
        let list_push = library.import::<Push<DIGEST_LENGTH>>();
        let entrypoint = Self::entrypoint();

        let read_digest_from_secret_in = "divine\n".repeat(DIGEST_LENGTH);

        // Public input: (peaks: Vec<Digest>), (leaf_count : u64)
        // Secret input: (authentication_path: Vec<Digest>), (leaf_digest: Digest), (leaf_index: u64)
        format!(
            "
            // BEFORE: _ *peaks leaf_count_hi leaf_count_lo
            // AFTER: _ *auth_path [digest (leaf_digest)] leaf_index_hi leaf_index_lo
                {entrypoint}:
                    // allocate space in memory for the `auth_path`
                    push {static_auth_path_pointer}
                    push 0
                    call {set_length}
                    // _ *peaks [digest (leaf_digest)] leaf_count_hi leaf_count_lo *auth_path

                    // Get the length of the authentication path from secret in
                    divine
                    // _ *peaks [digest (leaf_digest)] leaf_count_hi leaf_count_lo *auth_path auth_path_length

                    push 0
                    // _ *peaks [digest (leaf_digest)] leaf_count_hi leaf_count_lo *auth_path auth_path_length auth_path_counter

                    // Load from secret_in into the *auth_path list
                    call {entrypoint}_while
                    // _ *peaks [digest (leaf_digest)] leaf_count_hi leaf_count_lo *auth_path auth_path_length auth_path_counter

                    pop
                    pop
                    // _ *peaks [digest (leaf_digest)] leaf_count_hi leaf_count_lo *auth_path

                    swap8
                    // _ *auth_path [digest (leaf_digest)] leaf_count_hi leaf_count_lo *peaks

                    // Read the leaf index from secret input
                    divine
                    divine
                    // _ *auth_path [digest (leaf_digest)] leaf_count_hi leaf_count_lo *peaks leaf_index_hi leaf_index_lo

                    swap8 swap5 swap2 swap7 swap4 swap1 swap9 swap6 swap3
                    // _ *auth_path leaf_index_hi leaf_index_lo *peaks [digest (leaf_digest)] leaf_count_hi leaf_count_lo

                    call {verify_auth_path_from_memory}
                    // _ *auth_path leaf_index_hi leaf_index_lo validation_result

                    assert
                    // _ *auth_path leaf_index_hi leaf_index_lo

                    return

                // start/end _ *peaks [digest (leaf_digest)] leaf_count_hi leaf_count_lo *auth_path auth_path_length auth_path_counter
                {entrypoint}_while:
                    // Check end condition: auth_path_length == auth_path_counter
                    dup1 dup1 eq skiz return

                    swap2
                    // _ *peaks [digest (leaf_digest)] leaf_count_hi leaf_count_lo auth_path_counter auth_path_length *auth_path

                    {read_digest_from_secret_in}
                    // _ *peaks [digest (leaf_digest)] leaf_count_hi leaf_count_lo auth_path_counter auth_path_length *auth_path ap_elem_4 ap_elem_3 ap_elem_2 ap_elem_1 ap_elem_0

                    call {list_push}
                    // _ *peaks [digest (leaf_digest)] leaf_count_hi leaf_count_lo auth_path_counter auth_path_length *auth_path

                    swap2
                    // _ *peaks [digest (leaf_digest)] leaf_count_hi leaf_count_lo *auth_path auth_path_length auth_path_counter

                    // Increment counter
                    push 1
                    add
                    // _ *peaks [digest (leaf_digest)] leaf_count_hi leaf_count_lo *auth_path auth_path_length (auth_path_counter + 1)

                    recurse

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

        // BEFORE: _ *peaks leaf_count_hi leaf_count_lo [digest (leaf_digest)]
        // AFTER: _ *auth_path leaf_index_hi leaf_index_lo
        let mut leaf_digest_values = [BFieldElement::new(0); DIGEST_LENGTH];
        for elem in leaf_digest_values.iter_mut() {
            *elem = stack.pop().unwrap();
        }

        let leaf_digest = Digest::new(leaf_digest_values);

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

        // From secret in: get auth path
        let mut secret_in_cursor = 0;
        let auth_path_length: u32 = secret_in[secret_in_cursor].value().try_into().unwrap();
        secret_in_cursor += 1;

        let mut auth_path: Vec<Digest> = vec![];
        for _ in 0..(auth_path_length as usize) {
            let mut elements_in_digest = [BFieldElement::zero(); DIGEST_LENGTH];
            for j in 0..DIGEST_LENGTH {
                let ap_elem = secret_in[secret_in_cursor];
                secret_in_cursor += 1;

                elements_in_digest[DIGEST_LENGTH - 1 - j] = ap_elem;
            }

            let digest = Digest::new(elements_in_digest);
            auth_path.push(digest);
        }

        // Write auth_path to memory, this assumes that this address is free and can fit enough words
        let auth_path_pointer = BFieldElement::new((DIGEST_LENGTH * MAX_MMR_HEIGHT + 1) as u64);
        for ap_element in auth_path.iter() {
            rust_shadowing_helper_functions::list_push(
                auth_path_pointer,
                ap_element.values(),
                memory,
            );
        }

        // From secret in: get leaf_index
        let leaf_index_lo: u32 = secret_in[secret_in_cursor].try_into().unwrap();
        secret_in_cursor += 1;
        let leaf_index_hi: u32 = secret_in[secret_in_cursor].try_into().unwrap();
        // secret_in_cursor += 1; // Commented out to prevent a compiler warning

        let leaf_index: u64 = ((leaf_index_hi as u64) << 32) + leaf_index_lo as u64;

        let mut implied_mmr: MmrAccumulator<H> = MmrAccumulator::init(peaks, leaf_count as u128);
        let supplied_mp: MmrMembershipProof<H> =
            MmrMembershipProof::new(leaf_index as u128, auth_path);
        let verdict = supplied_mp
            .verify(&implied_mmr.get_peaks(), &leaf_digest, leaf_count as u128)
            .0;

        assert!(verdict, "Supplied authentication path must be valid");

        // Push leaf_index and auth_path_pointer onto the stack
        stack.push(BFieldElement::new(leaf_index_lo as u64));
        stack.push(BFieldElement::new(leaf_index_hi as u64));
        stack.push(auth_path_pointer);
    }
}

#[cfg(test)]
mod tests {

    use crate::test_helpers::rust_tasm_equivalence_prop_new;

    use super::*;

    #[test]
    fn new_snippet_test() {
        rust_tasm_equivalence_prop_new::<MmrVerifyFromSecretInputThroughMemory>();
    }
}
