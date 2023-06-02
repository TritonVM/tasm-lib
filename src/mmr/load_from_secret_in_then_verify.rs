use std::collections::HashMap;
use std::marker::PhantomData;

use num::One;
use rand::{thread_rng, Rng};
use twenty_first::shared_math::b_field_element::BFieldElement;
use twenty_first::shared_math::other::random_elements;
use twenty_first::test_shared::mmr::get_rustyleveldb_ammr_from_digests;
use twenty_first::util_types::algebraic_hasher::AlgebraicHasher;
use twenty_first::util_types::mmr::archival_mmr::ArchivalMmr;
use twenty_first::util_types::mmr::mmr_accumulator::MmrAccumulator;
use twenty_first::util_types::mmr::mmr_membership_proof::MmrMembershipProof;
use twenty_first::util_types::mmr::mmr_trait::Mmr;

use crate::hashing::load_auth_path_from_secret_in_unsafe_list::LoadAuthPathFromSecretInUnsafeList;
use crate::snippet::{DataType, Snippet};
use crate::snippet_state::SnippetState;
use crate::{
    get_init_tvm_stack, rust_shadowing_helper_functions, Digest, ExecutionState, DIGEST_LENGTH,
};

use super::verify_from_memory::MmrVerifyFromMemory;
use super::MAX_MMR_HEIGHT;

/// First load from secret-in, then verify from memory
#[derive(Clone, Debug)]
pub struct MmrLoadFromSecretInThenVerify<H: AlgebraicHasher>(pub PhantomData<H>);

// TODO: Compiler complains without this explicit lifetime on `H`. But is it OK?
impl<H: AlgebraicHasher + 'static> Snippet for MmrLoadFromSecretInThenVerify<H> {
    fn inputs(&self) -> Vec<String> {
        vec![
            "peaks_pointer".to_string(),
            "leaf_count_hi".to_string(),
            "leaf_count_lo".to_string(),
            "leaf_index_hi".to_string(),
            "leaf_index_lo".to_string(),
            "leaf_digest_4".to_string(),
            "leaf_digest_3".to_string(),
            "leaf_digest_2".to_string(),
            "leaf_digest_1".to_string(),
            "leaf_digest_0".to_string(),
        ]
    }

    fn outputs(&self) -> Vec<String> {
        vec![
            "auth_path_pointer".to_string(),
            "leaf_index_hi".to_string(),
            "leaf_index_lo".to_string(),
            "validation_result".to_string(),
        ]
    }

    fn input_types(&self) -> Vec<DataType> {
        vec![
            DataType::List(Box::new(DataType::Digest)),
            DataType::U64,
            DataType::U64,
            DataType::Digest,
        ]
    }

    fn output_types(&self) -> Vec<DataType> {
        vec![
            DataType::List(Box::new(DataType::Digest)),
            DataType::U64,
            DataType::Bool,
        ]
    }

    fn crash_conditions() -> Vec<String> {
        vec!["Leaf index exceeds leaf count".to_string()]
    }

    fn gen_input_states(&self) -> Vec<ExecutionState> {
        let mut rng = thread_rng();
        let mut init_vm_states = vec![];

        for size in 1..50 {
            let leaf_index = rng.gen_range(0..size);
            init_vm_states.push(prepare_state::<H>(size, leaf_index as u64, true));
            init_vm_states.push(prepare_state::<H>(size, leaf_index as u64, false));
        }

        init_vm_states
    }

    fn stack_diff(&self) -> isize {
        -6
    }

    fn entrypoint(&self) -> String {
        "tasm_mmr_verify_load_from_secret_in".to_string()
    }

    fn function_body(&self, library: &mut SnippetState) -> String {
        let entrypoint = self.entrypoint();
        let load_auth_path_from_secret_in =
            library.import(Box::new(LoadAuthPathFromSecretInUnsafeList));

        let verify_from_memory = library.import(Box::new(MmrVerifyFromMemory(PhantomData::<H>)));

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
        &self,
        stack: &mut Vec<BFieldElement>,
        _std_in: Vec<BFieldElement>,
        secret_in: Vec<BFieldElement>,
        memory: &mut HashMap<BFieldElement, BFieldElement>,
    ) {
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
            let digest = Digest::new(
                rust_shadowing_helper_functions::unsafe_list::unsafe_list_read(
                    peaks_pointer,
                    i as usize,
                    memory,
                    DIGEST_LENGTH,
                )
                .try_into()
                .unwrap(),
            );
            peaks.push(digest);
        }

        // Read authentication path from secret in, then write it to memory
        let mut secret_in_cursor = 0;
        let total_auth_path_length: u32 = secret_in[secret_in_cursor].value().try_into().unwrap();
        secret_in_cursor += 1;

        let auth_path_pointer = BFieldElement::new((DIGEST_LENGTH * MAX_MMR_HEIGHT + 2) as u64);
        rust_shadowing_helper_functions::unsafe_list::unsafe_list_new(auth_path_pointer, memory);

        let mut i = 0;
        while i != total_auth_path_length {
            let ap_element = rust_shadowing_helper_functions::input::read_digest_from_secret_in(
                &secret_in,
                &mut secret_in_cursor,
            );
            rust_shadowing_helper_functions::unsafe_list::unsafe_list_push(
                auth_path_pointer,
                ap_element.values().to_vec(),
                memory,
                DIGEST_LENGTH,
            );

            i += 1;
        }

        // Read auth path from memory
        let auth_path_length = memory[&auth_path_pointer].value();
        let mut auth_path: Vec<Digest> = vec![];
        for i in 0..auth_path_length {
            let digest = Digest::new(
                rust_shadowing_helper_functions::unsafe_list::unsafe_list_read(
                    auth_path_pointer,
                    i as usize,
                    memory,
                    DIGEST_LENGTH,
                )
                .try_into()
                .unwrap(),
            );
            auth_path.push(digest);
        }

        let valid_mp = MmrMembershipProof::<H>::new(leaf_index, auth_path)
            .verify(&peaks, &leaf_digest, leaf_count)
            .0;

        stack.push(auth_path_pointer);
        stack.push(BFieldElement::new(leaf_index_hi as u64));
        stack.push(BFieldElement::new(leaf_index_lo as u64));
        stack.push(BFieldElement::new(valid_mp as u64));
    }

    fn common_case_input_state(&self) -> ExecutionState
    where
        Self: Sized,
    {
        // This should be a bigger state, but that's not possible since
        // the current implementation uses an archival MMR, which requires
        // all MMR nodes to be stored in memory.
        prepare_state::<H>(100, 50, true)
    }

    fn worst_case_input_state(&self) -> ExecutionState
    where
        Self: Sized,
    {
        // This should be a bigger state, but that's not possible since
        // the current implementation uses an archival MMR, which requires
        // all MMR nodes to be stored in memory.
        prepare_state::<H>(2000, 1000, true)
    }
}

fn prepare_state<H: AlgebraicHasher>(
    size: usize,
    leaf_index: u64,
    produce_valid_input: bool,
) -> ExecutionState {
    // BEFORE: _ *peaks leaf_count_hi leaf_count_lo leaf_index_hi leaf_index_lo [digest (leaf_digest)]
    // AFTER: _ *auth_path leaf_index_hi leaf_index_lo validation_result

    let digests: Vec<Digest> = random_elements(size);
    let ammr: ArchivalMmr<H, _> = get_rustyleveldb_ammr_from_digests(digests.clone());
    let mut vm_init_state = mmr_to_init_vm_state(&mut ammr.to_accumulator());

    // Populate secret-in with the correct authentication path
    let mmr_mp = ammr.prove_membership(leaf_index).0;
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

    // Push the correct leaf on the stack, or a different one if we want to produce invalid input
    let mut vm_init_state = vm_init_state.clone();
    if produce_valid_input {
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

/// Prepare the part of the state that can be derived from the MMR without
/// knowing e.g. the leaf index of the leaf digest that you want to authenticate
/// so this function does not populate e.g. `secret_in`. The caller has to do that.
fn mmr_to_init_vm_state<H: AlgebraicHasher>(mmra: &mut MmrAccumulator<H>) -> ExecutionState {
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
    rust_shadowing_helper_functions::unsafe_list::unsafe_list_new(peaks_pointer, &mut memory);
    for peak in mmra.get_peaks() {
        rust_shadowing_helper_functions::unsafe_list::unsafe_list_push(
            peaks_pointer,
            peak.values().to_vec(),
            &mut memory,
            DIGEST_LENGTH,
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::snippet_bencher::bench_and_write;
    use crate::test_helpers::rust_tasm_equivalence_prop_new;
    use crate::VmHasher;

    #[test]
    fn load_from_secret_in_then_verify_test() {
        rust_tasm_equivalence_prop_new(MmrLoadFromSecretInThenVerify(PhantomData::<VmHasher>));
    }

    #[test]
    fn load_from_secret_in_then_verify_benchmark() {
        bench_and_write(MmrLoadFromSecretInThenVerify(PhantomData::<VmHasher>));
    }
}
