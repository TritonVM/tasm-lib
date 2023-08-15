use std::collections::HashMap;

use num::One;
use num::Zero;

use triton_vm::NonDeterminism;
use twenty_first::shared_math::b_field_element::BFieldElement;
use twenty_first::shared_math::other::random_elements;

use crate::library::Library;
use crate::list::safe_u32::new::SafeNew;
use crate::list::safe_u32::push::SafePush;
use crate::mmr::MAX_MMR_HEIGHT;
use crate::rust_shadowing_helper_functions;
use crate::snippet::DataType;
use crate::snippet::DeprecatedSnippet;
use crate::Digest;
use crate::DIGEST_LENGTH;
use crate::{get_init_tvm_stack, ExecutionState};

#[derive(Clone, Debug)]
pub struct LoadAuthPathFromStdInSafeList;

impl DeprecatedSnippet for LoadAuthPathFromStdInSafeList {
    fn input_field_names(&self) -> Vec<String> {
        vec![]
    }

    fn output_field_names(&self) -> Vec<String> {
        vec!["auth_path_pointer".to_string()]
    }

    fn input_types(&self) -> Vec<crate::snippet::DataType> {
        vec![]
    }

    fn output_types(&self) -> Vec<crate::snippet::DataType> {
        vec![DataType::List(Box::new(DataType::Digest))]
    }

    fn crash_conditions(&self) -> Vec<String> {
        vec!["Not enough elements in std input".to_string()]
    }

    fn gen_input_states(&self) -> Vec<ExecutionState> {
        let mut init_vm_states: Vec<ExecutionState> = vec![];
        for ap_length in 0..MAX_MMR_HEIGHT {
            let ap_elements: Vec<Digest> = random_elements(ap_length);
            let mut std_in = vec![BFieldElement::new(ap_length as u64)];
            for ap_element in ap_elements.iter() {
                rust_shadowing_helper_functions::input::write_digest_to_std_in(
                    &mut std_in,
                    *ap_element,
                );
            }
            let init_vm_state = ExecutionState {
                stack: get_init_tvm_stack(),
                std_in,
                nondeterminism: NonDeterminism::new(vec![]),
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

    fn entrypoint_name(&self) -> String {
        "tasm_hashing_load_auth_path_from_std_in_safe_list".to_string()
    }

    fn function_code(&self, library: &mut Library) -> String {
        let entrypoint = self.entrypoint_name();

        let read_digest_from_std_in = "read_io\n".repeat(DIGEST_LENGTH);

        let push = library.import(Box::new(SafePush(DataType::Digest)));
        let new_list = library.import(Box::new(SafeNew(DataType::Digest)));

        format!(
            "
                // BEFORE: _
                // AFTER: *auth_path
                {entrypoint}:
                    // Read length of authentication path
                    read_io
                    // _ total_auth_path_length

                    // It shouldn't be necessary to validate that this is a u32 since
                    // the below loop will never finish if it isn't.

                    // Initialize a counter counting how many elements was read
                    push 0
                    // _ total_auth_path_length i

                    // Initialize list where the authentication path is stored
                    push {MAX_MMR_HEIGHT}
                    call {new_list}
                    // _ auth_path_length i *auth_path

                    call {entrypoint}_while
                    // _ total_auth_path_length i *auth_path

                    swap 2 pop pop
                    // _ *auth_path

                    return

                // Start/end stack: _ total_auth_path_length i *auth_path
                {entrypoint}_while:
                    // Loop condition: end if (total_auth_path_length == i)
                    dup 2 dup 2 eq skiz return
                    // _ total_auth_path_length i *auth_path

                    dup 0
                    // _ total_auth_path_length i *auth_path *auth_path

                    {read_digest_from_std_in}
                    // _ total_auth_path_length i *auth_path *auth_path [digests (ap_element)]

                    call {push}
                    // _ total_auth_path_length i *auth_path

                    // i -> i + 1
                    swap 1 push 1 add swap 1
                    // _ total_auth_path_length (i + 1) *auth_path

                    recurse
                "
        )
    }

    fn rust_shadowing(
        &self,
        stack: &mut Vec<BFieldElement>,
        std_in: Vec<BFieldElement>,
        _secret_in: Vec<BFieldElement>,
        memory: &mut HashMap<BFieldElement, BFieldElement>,
    ) {
        let mut std_in_cursor = 0;
        let total_auth_path_length: u32 = std_in[std_in_cursor].value().try_into().unwrap();
        std_in_cursor += 1;

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
            let ap_element = rust_shadowing_helper_functions::input::read_digest_from_std_in(
                &std_in,
                &mut std_in_cursor,
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

    fn common_case_input_state(&self) -> ExecutionState {
        // Common case is defined to be half of the maximum MMR height
        let mut std_in: Vec<BFieldElement> = vec![];
        rust_shadowing_helper_functions::input::write_dummy_ap_path(
            &mut std_in,
            MAX_MMR_HEIGHT / 2,
        );

        ExecutionState {
            stack: get_init_tvm_stack(),
            std_in,
            nondeterminism: NonDeterminism::new(vec![]),
            memory: HashMap::default(),
            words_allocated: 0,
        }
    }

    fn worst_case_input_state(&self) -> ExecutionState {
        // Common case is defined to be half of the maximum MMR height
        let mut std_in: Vec<BFieldElement> = vec![];
        rust_shadowing_helper_functions::input::write_dummy_ap_path(&mut std_in, MAX_MMR_HEIGHT);

        ExecutionState {
            stack: get_init_tvm_stack(),
            std_in,
            nondeterminism: NonDeterminism::new(vec![]),
            memory: HashMap::default(),
            words_allocated: 0,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_helpers::test_rust_equivalence_multiple_deprecated;

    #[test]
    fn load_auth_path_from_std_in_test() {
        test_rust_equivalence_multiple_deprecated(&LoadAuthPathFromStdInSafeList, true);
    }
}

#[cfg(test)]
mod benches {
    use super::*;
    use crate::snippet_bencher::bench_and_write;

    #[test]
    fn load_auth_path_from_std_in_benchmark() {
        bench_and_write(LoadAuthPathFromStdInSafeList);
    }
}
