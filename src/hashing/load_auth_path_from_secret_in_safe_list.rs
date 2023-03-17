use std::collections::HashMap;

use num::One;
use num::Zero;

use twenty_first::shared_math::b_field_element::BFieldElement;
use twenty_first::shared_math::other::random_elements;
use twenty_first::shared_math::rescue_prime_digest::Digest;
use twenty_first::shared_math::rescue_prime_digest::DIGEST_LENGTH;

use crate::library::Library;
use crate::list::safe_u32::new::SafeNew;
use crate::list::safe_u32::push::SafePush;
use crate::mmr::MAX_MMR_HEIGHT;
use crate::snippet::DataType;
use crate::snippet::Snippet;
use crate::{get_init_tvm_stack, rust_shadowing_helper_functions, ExecutionState};

#[derive(Clone)]
pub struct LoadAuthPathFromSecretInSafeList;

impl Snippet for LoadAuthPathFromSecretInSafeList {
    fn inputs(&self) -> Vec<String> {
        vec![]
    }

    fn outputs(&self) -> Vec<String> {
        vec!["auth_path_pointer".to_string()]
    }

    fn input_types(&self) -> Vec<crate::snippet::DataType> {
        vec![]
    }

    fn output_types(&self) -> Vec<crate::snippet::DataType> {
        vec![DataType::List(Box::new(DataType::Digest))]
    }

    fn crash_conditions() -> Vec<String> {
        vec!["Not enough elements in secret input".to_string()]
    }

    fn gen_input_states(&self) -> Vec<ExecutionState> {
        let mut init_vm_states: Vec<ExecutionState> = vec![];
        for ap_length in 0..MAX_MMR_HEIGHT {
            let ap_elements: Vec<Digest> = random_elements(ap_length);
            let mut secret_in = vec![BFieldElement::new(ap_length as u64)];
            for ap_element in ap_elements.iter() {
                rust_shadowing_helper_functions::input::write_digest_to_secret_in(
                    &mut secret_in,
                    *ap_element,
                );
            }
            let init_vm_state = ExecutionState {
                stack: get_init_tvm_stack(),
                std_in: vec![],
                secret_in,
                memory: HashMap::default(),
                words_allocated: 0,
            };
            init_vm_states.push(init_vm_state);
        }

        init_vm_states
    }

    fn stack_diff(&self) -> isize {
        1
    }

    fn entrypoint(&self) -> String {
        "tasm_hashing_load_auth_path_from_secret_in_safe_list".to_string()
    }

    fn function_body(&self, library: &mut Library) -> String {
        let entrypoint = self.entrypoint();
        let read_digest_from_secret_in = "divine\n".repeat(DIGEST_LENGTH);

        let push = library.import(Box::new(SafePush(DataType::Digest)));
        let new_list = library.import(Box::new(SafeNew(DataType::Digest)));

        // Derive value that the authentication path length must be less than
        let max_mmr_height_plus_one: usize = MAX_MMR_HEIGHT + 1;
        format!(
            "
                // BEFORE: _
                // AFTER: _ *auth_path
                {entrypoint}:
                    // Read length of authentication path
                    divine
                    // _ auth_path_length

                    // Assert that the authentication path does not exceed the max allowed MMR height
                    push {max_mmr_height_plus_one}
                    dup 1
                    lt
                    // _ auth_path_length max_length > auth_path_length

                    assert
                    // _ auth_path_length

                    // Initialize a counter counting how many elements was read
                    push 0
                    // _ auth_path_length i

                    // Initialize list where the authentication path is stored
                    push {MAX_MMR_HEIGHT}
                    call {new_list}
                    // _ auth_path_length i *auth_path

                    call {entrypoint}_while
                    // _ auth_path_length i *auth_path

                    swap 2 pop pop
                    // _ *auth_path

                    return

                // Start/end stack: _ auth_path_length i *auth_path
                {entrypoint}_while:
                    // Loop condition: end if (auth_path_length == i)
                    dup 2 dup 2 eq skiz return
                    // _ auth_path_length i *auth_path

                    dup 0
                    // _ auth_path_length i *auth_path *auth_path

                    {read_digest_from_secret_in}
                    // _ auth_path_length i *auth_path *auth_path [digests (ap_element)]

                    call {push}
                    // _ auth_path_length i *auth_path

                    // i -> i + 1
                    swap 1 push 1 add swap 1
                    // _ auth_path_length (i + 1) *auth_path

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
        let mut secret_in_cursor = 0;
        let total_auth_path_length: u32 = secret_in[secret_in_cursor].value().try_into().unwrap();
        secret_in_cursor += 1;

        // Simulate call to dynamic malloc to get a new memory-safe list
        memory.insert(
            BFieldElement::zero(),
            BFieldElement::new((MAX_MMR_HEIGHT * DIGEST_LENGTH + 3) as u64),
        );
        let auth_path_pointer = BFieldElement::one();

        rust_shadowing_helper_functions::safe_list::safe_list_new(
            auth_path_pointer,
            MAX_MMR_HEIGHT as u32,
            memory,
        );

        let mut i = 0;
        while i != total_auth_path_length {
            let ap_element = rust_shadowing_helper_functions::input::read_digest_from_secret_in(
                &secret_in,
                &mut secret_in_cursor,
            );
            rust_shadowing_helper_functions::safe_list::safe_list_push(
                auth_path_pointer,
                ap_element.values().to_vec(),
                memory,
                DIGEST_LENGTH,
            );

            i += 1;
        }

        stack.push(auth_path_pointer);
    }

    fn common_case_input_state(&self) -> ExecutionState
    where
        Self: Sized,
    {
        // Common case is defined to be half of the maximum MMR height
        let mut secret_in: Vec<BFieldElement> = vec![];
        rust_shadowing_helper_functions::input::write_dummy_ap_path(
            &mut secret_in,
            MAX_MMR_HEIGHT / 2,
        );

        ExecutionState {
            stack: get_init_tvm_stack(),
            std_in: vec![],
            secret_in,
            memory: HashMap::default(),
            words_allocated: 0,
        }
    }

    fn worst_case_input_state(&self) -> ExecutionState
    where
        Self: Sized,
    {
        // Worst case is the maximum MMR height
        let mut secret_in: Vec<BFieldElement> = vec![];
        rust_shadowing_helper_functions::input::write_dummy_ap_path(&mut secret_in, MAX_MMR_HEIGHT);

        ExecutionState {
            stack: get_init_tvm_stack(),
            std_in: vec![],
            secret_in,
            memory: HashMap::default(),
            words_allocated: 0,
        }
    }
}

#[cfg(test)]
mod load_auth_path_from_secret_in_tests {
    use rand::random;

    use super::*;
    use crate::snippet_bencher::bench_and_write;
    use crate::test_helpers::rust_tasm_equivalence_prop_new;

    #[should_panic]
    #[test]
    fn disallow_too_long_mmr_auth_paths() {
        let mut secret_in = vec![];
        let too_long_mmr_ap_length = MAX_MMR_HEIGHT + 1;
        rust_shadowing_helper_functions::input::write_value_to_secret_in(
            &mut secret_in,
            BFieldElement::new(too_long_mmr_ap_length as u64),
        );

        for _ in 0..too_long_mmr_ap_length {
            rust_shadowing_helper_functions::input::write_digest_to_secret_in(
                &mut secret_in,
                random(),
            );
        }

        LoadAuthPathFromSecretInSafeList.run_tasm_old(
            &mut get_init_tvm_stack(),
            vec![],
            secret_in.to_vec(),
            &mut HashMap::default(),
            0,
        );
    }

    #[test]
    fn allow_max_mmr_auth_path_length() {
        let mut secret_in = vec![];
        let max_length_mmr_ap = MAX_MMR_HEIGHT;
        rust_shadowing_helper_functions::input::write_value_to_secret_in(
            &mut secret_in,
            BFieldElement::new(max_length_mmr_ap as u64),
        );

        for _ in 0..max_length_mmr_ap {
            rust_shadowing_helper_functions::input::write_digest_to_secret_in(
                &mut secret_in,
                random(),
            );
        }
        LoadAuthPathFromSecretInSafeList.run_tasm_old(
            &mut get_init_tvm_stack(),
            vec![],
            secret_in.to_vec(),
            &mut HashMap::default(),
            0,
        );
    }

    #[test]
    fn load_auth_path_from_secret_in_test() {
        rust_tasm_equivalence_prop_new(LoadAuthPathFromSecretInSafeList);
    }

    #[test]
    fn load_auth_path_from_secret_in_benchmark() {
        bench_and_write(LoadAuthPathFromSecretInSafeList);
    }
}
