use num::One;
use rand::{thread_rng, Rng};
use std::collections::HashMap;
use triton_vm::NonDeterminism;
use twenty_first::shared_math::b_field_element::BFieldElement;
use twenty_first::shared_math::other::random_elements;
use twenty_first::test_shared::mmr::get_rustyleveldb_ammr_from_digests;
use twenty_first::util_types::mmr::archival_mmr::ArchivalMmr;
use twenty_first::util_types::mmr::mmr_accumulator::MmrAccumulator;
use twenty_first::util_types::mmr::mmr_membership_proof::MmrMembershipProof;
use twenty_first::util_types::mmr::mmr_trait::Mmr;

use crate::hashing::load_auth_path_from_secret_in_safe_list::LoadAuthPathFromSecretInSafeList;
use crate::hashing::load_auth_path_from_secret_in_unsafe_list::LoadAuthPathFromSecretInUnsafeList;
use crate::library::Library;
use crate::list::ListType;
use crate::snippet::{DataType, DepracatedSnippet};
use crate::{
    get_init_tvm_stack, rust_shadowing_helper_functions, Digest, ExecutionState, VmHasher,
    DIGEST_LENGTH,
};

use super::verify_from_memory::MmrVerifyFromMemory;
use super::MAX_MMR_HEIGHT;

/// First load from secret-in, then verify from memory
#[derive(Clone, Debug)]
pub struct MmrLoadFromSecretInThenVerify {
    pub list_type: ListType,
}

impl MmrLoadFromSecretInThenVerify {
    fn prepare_state(
        &self,
        size: usize,
        leaf_index: u64,
        produce_valid_input: bool,
    ) -> ExecutionState {
        // BEFORE: _ *peaks leaf_count_hi leaf_count_lo leaf_index_hi leaf_index_lo [digest (leaf_digest)]
        // AFTER: _ *auth_path leaf_index_hi leaf_index_lo validation_result

        let digests: Vec<Digest> = random_elements(size);
        let ammr: ArchivalMmr<VmHasher, _> = get_rustyleveldb_ammr_from_digests(digests.clone());
        let mut vm_init_state = self.mmr_to_init_vm_state(&mut ammr.to_accumulator());

        // Populate secret-in with the correct authentication path
        let mmr_mp = ammr.prove_membership(leaf_index).0;
        let authentication_path = mmr_mp.authentication_path;
        vm_init_state
            .nondeterminism
            .individual_tokens
            .push(BFieldElement::new(authentication_path.len() as u64));
        for ap_element in authentication_path.iter() {
            let mut ap_element_values = ap_element.values().to_vec();
            for _ in 0..DIGEST_LENGTH {
                vm_init_state
                    .nondeterminism
                    .individual_tokens
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
    fn mmr_to_init_vm_state(&self, mmra: &mut MmrAccumulator<VmHasher>) -> ExecutionState {
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

        let list_meta_data_size = match self.list_type {
            ListType::Safe => 2,
            ListType::Unsafe => 1,
        };
        ExecutionState {
            stack,
            std_in: vec![],
            nondeterminism: NonDeterminism::new(vec![]),
            memory,
            words_allocated: DIGEST_LENGTH * MAX_MMR_HEIGHT + 1 + list_meta_data_size,
        }
    }
}

// TODO: Compiler complains without this explicit lifetime on `H`. But is it OK?
impl DepracatedSnippet for MmrLoadFromSecretInThenVerify {
    fn input_field_names(&self) -> Vec<String> {
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

    fn output_field_names(&self) -> Vec<String> {
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

    fn crash_conditions(&self) -> Vec<String> {
        vec!["Leaf index exceeds leaf count".to_string()]
    }

    fn gen_input_states(&self) -> Vec<ExecutionState> {
        let mut rng = thread_rng();
        let mut init_vm_states = vec![];

        for size in 1..50 {
            let leaf_index = rng.gen_range(0..size);
            init_vm_states.push(self.prepare_state(size, leaf_index as u64, true));
            init_vm_states.push(self.prepare_state(size, leaf_index as u64, false));
        }

        init_vm_states
    }

    fn stack_diff(&self) -> isize {
        -6
    }

    fn entrypoint_name(&self) -> String {
        format!("tasm_mmr_verify_load_from_secret_in_{}", self.list_type)
    }

    fn function_code(&self, library: &mut Library) -> String {
        let entrypoint = self.entrypoint_name();
        let load_auth_path_from_secret_in = match self.list_type {
            ListType::Safe => library.import(Box::new(LoadAuthPathFromSecretInSafeList)),
            ListType::Unsafe => library.import(Box::new(LoadAuthPathFromSecretInUnsafeList)),
        };

        let verify_from_memory = library.import(Box::new(MmrVerifyFromMemory {
            list_type: self.list_type.clone(),
        }));

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
        let list_get = match self.list_type {
            ListType::Safe => rust_shadowing_helper_functions::safe_list::safe_list_get,
            ListType::Unsafe => rust_shadowing_helper_functions::unsafe_list::unsafe_list_get,
        };
        for i in 0..peaks_count {
            let digest = Digest::new(
                list_get(peaks_pointer, i as usize, memory, DIGEST_LENGTH)
                    .try_into()
                    .unwrap(),
            );
            peaks.push(digest);
        }

        // Read authentication path from secret in, then write it to memory
        let mut secret_in_cursor = 0;
        let total_auth_path_length: u32 = secret_in[secret_in_cursor].value().try_into().unwrap();
        secret_in_cursor += 1;

        let auth_path_pointer = match self.list_type {
            ListType::Safe => BFieldElement::new((DIGEST_LENGTH * MAX_MMR_HEIGHT + 1 + 2) as u64),
            ListType::Unsafe => BFieldElement::new((DIGEST_LENGTH * MAX_MMR_HEIGHT + 1 + 1) as u64),
        };
        match self.list_type {
            ListType::Safe => rust_shadowing_helper_functions::safe_list::safe_list_new(
                auth_path_pointer,
                MAX_MMR_HEIGHT as u32,
                memory,
            ),
            ListType::Unsafe => rust_shadowing_helper_functions::unsafe_list::unsafe_list_new(
                auth_path_pointer,
                memory,
            ),
        };

        let list_push = match self.list_type {
            ListType::Safe => rust_shadowing_helper_functions::safe_list::safe_list_push,
            ListType::Unsafe => rust_shadowing_helper_functions::unsafe_list::unsafe_list_push,
        };
        let mut i = 0;
        while i != total_auth_path_length {
            let ap_element = rust_shadowing_helper_functions::input::read_digest_from_secret_in(
                &secret_in,
                &mut secret_in_cursor,
            );
            list_push(
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
                list_get(auth_path_pointer, i as usize, memory, DIGEST_LENGTH)
                    .try_into()
                    .unwrap(),
            );
            auth_path.push(digest);
        }

        let valid_mp = MmrMembershipProof::<VmHasher>::new(leaf_index, auth_path)
            .verify(&peaks, &leaf_digest, leaf_count)
            .0;

        stack.push(auth_path_pointer);
        stack.push(BFieldElement::new(leaf_index_hi as u64));
        stack.push(BFieldElement::new(leaf_index_lo as u64));
        stack.push(BFieldElement::new(valid_mp as u64));
    }

    fn common_case_input_state(&self) -> ExecutionState {
        // This should be a bigger state, but that's not possible since
        // the current implementation uses an archival MMR, which requires
        // all MMR nodes to be stored in memory.
        self.prepare_state(100, 50, true)
    }

    fn worst_case_input_state(&self) -> ExecutionState {
        // This should be a bigger state, but that's not possible since
        // the current implementation uses an archival MMR, which requires
        // all MMR nodes to be stored in memory.
        self.prepare_state(2000, 1000, true)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use crate::test_helpers::test_rust_equivalence_multiple;

    #[test]
    fn load_from_secret_in_then_verify_test_safe_lists() {
        test_rust_equivalence_multiple(
            &MmrLoadFromSecretInThenVerify {
                list_type: ListType::Safe,
            },
            true,
        );
    }

    #[test]
    fn load_from_secret_in_then_verify_test_unsafe_lists() {
        test_rust_equivalence_multiple(
            &MmrLoadFromSecretInThenVerify {
                list_type: ListType::Unsafe,
            },
            true,
        );
    }
}

#[cfg(test)]
mod benches {
    use super::*;
    use crate::snippet_bencher::bench_and_write;

    #[test]
    fn load_from_secret_in_then_verify_benchmark_unsafe_lists() {
        bench_and_write(MmrLoadFromSecretInThenVerify {
            list_type: ListType::Unsafe,
        });
    }

    #[test]
    fn load_from_secret_in_then_verify_benchmark_safe_lists() {
        bench_and_write(MmrLoadFromSecretInThenVerify {
            list_type: ListType::Safe,
        });
    }
}
