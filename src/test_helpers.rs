use std::collections::HashMap;

use itertools::Itertools;
use twenty_first::shared_math::b_field_element::BFieldElement;

use crate::dyn_malloc::DYN_MALLOC_ADDRESS;
use crate::snippet::Snippet;
use crate::snippet_state::SnippetState;
use crate::{
    exported_snippets, rust_shadowing_helper_functions, ExecutionState, VmOutputState,
    DIGEST_LENGTH,
};

#[allow(dead_code)]
pub fn test_rust_equivalence_multiple<T: Snippet>(
    snippet_struct: &T,
    export_snippet: bool,
) -> Vec<VmOutputState> {
    // Verify that snippet can be found in `all_snippets`, so it's visible to the outside
    // This call will panic if snippet is not found in that function call
    // The data type value is a dummy value for all snippets except those that handle lists.
    if export_snippet {
        let looked_up_snippet = exported_snippets::name_to_snippet(&snippet_struct.entrypoint());
        assert_eq!(
            snippet_struct.entrypoint(),
            looked_up_snippet.entrypoint(),
            "Looked up snippet must match self"
        );
    }

    let mut execution_states = snippet_struct.gen_input_states();

    let mut vm_output_states = vec![];
    for execution_state in execution_states.iter_mut() {
        let vm_output_state = test_rust_equivalence_given_input_values::<T>(
            snippet_struct,
            &execution_state.stack,
            &execution_state.std_in,
            &execution_state.secret_in,
            &mut execution_state.memory,
            execution_state.words_allocated,
            None,
        );
        vm_output_states.push(vm_output_state);
    }

    vm_output_states
}

#[allow(dead_code)]
pub fn test_rust_equivalence_given_execution_state<T: Snippet>(
    snippet_struct: &T,
    mut execution_state: ExecutionState,
) -> VmOutputState {
    test_rust_equivalence_given_input_values::<T>(
        snippet_struct,
        &execution_state.stack,
        &execution_state.std_in,
        &execution_state.secret_in,
        &mut execution_state.memory,
        execution_state.words_allocated,
        None,
    )
}

#[allow(dead_code)]
pub fn test_rust_equivalence_given_input_values<T: Snippet>(
    snippet_struct: &T,
    stack: &[BFieldElement],
    stdin: &[BFieldElement],
    secret_in: &[BFieldElement],
    memory: &mut HashMap<BFieldElement, BFieldElement>,
    words_statically_allocated: usize,
    expected_final_stack: Option<&[BFieldElement]>,
) -> VmOutputState {
    let init_memory = memory.clone();
    let mut tasm_stack = stack.to_vec();
    let mut tasm_memory = init_memory.clone();

    let mut rust_memory = init_memory;
    let mut rust_stack = stack.to_vec();

    test_rust_equivalence_given_input_values_and_initial_stacks_and_memories(
        snippet_struct,
        stack,
        stdin,
        secret_in,
        memory,
        words_statically_allocated,
        expected_final_stack,
        &mut tasm_stack,
        &mut rust_stack,
        &mut tasm_memory,
        &mut rust_memory,
    )
}

#[allow(dead_code)]
#[allow(clippy::ptr_arg)]
#[allow(clippy::too_many_arguments)]
pub fn test_rust_equivalence_given_input_values_and_initial_stacks_and_memories<T: Snippet>(
    snippet_struct: &T,
    stack: &[BFieldElement],
    stdin: &[BFieldElement],
    secret_in: &[BFieldElement],
    memory: &mut HashMap<BFieldElement, BFieldElement>,
    words_statically_allocated: usize,
    expected_final_stack: Option<&[BFieldElement]>,
    tasm_stack: &mut Vec<BFieldElement>,
    rust_stack: &mut Vec<BFieldElement>,
    tasm_memory: &mut HashMap<BFieldElement, BFieldElement>,
    rust_memory: &mut HashMap<BFieldElement, BFieldElement>,
) -> VmOutputState {
    let init_stack = stack.to_vec();

    if words_statically_allocated > 0 {
        rust_shadowing_helper_functions::dyn_malloc::rust_dyn_malloc_initialize(
            rust_memory,
            words_statically_allocated,
        );
    }

    // run rust shadow
    snippet_struct.rust_shadowing(rust_stack, stdin.to_vec(), secret_in.to_vec(), rust_memory);

    // run tvm
    let vm_output_state = snippet_struct.link_and_run_tasm_for_test(
        tasm_stack,
        stdin.to_vec(),
        secret_in.to_vec(),
        tasm_memory,
        words_statically_allocated,
    );

    // assert stacks are equal, up to program hash
    let tasm_stack_skip_program_hash = tasm_stack.iter().cloned().skip(DIGEST_LENGTH).collect_vec();
    let rust_stack_skip_program_hash = rust_stack.iter().cloned().skip(DIGEST_LENGTH).collect_vec();
    assert_eq!(
        tasm_stack_skip_program_hash,
        rust_stack_skip_program_hash,
        "Rust code must match TVM for `{}`\n\nTVM: {}\n\nRust: {}. Code was: {}",
        snippet_struct.entrypoint(),
        tasm_stack_skip_program_hash
            .iter()
            .map(|x| x.to_string())
            .collect_vec()
            .join(","),
        rust_stack_skip_program_hash
            .iter()
            .map(|x| x.to_string())
            .collect_vec()
            .join(","),
        snippet_struct.function_code(&mut SnippetState::default())
    );

    // if expected final stack is given, test against it
    if let Some(expected) = expected_final_stack {
        let expected_final_stack_skip_program_hash =
            expected.iter().skip(DIGEST_LENGTH).cloned().collect_vec();
        assert_eq!(
            tasm_stack_skip_program_hash,
            expected_final_stack_skip_program_hash,
            "TVM must produce expected stack `{}`. \n\nTVM:\n{}\nExpected:\n{}",
            snippet_struct.entrypoint(),
            tasm_stack_skip_program_hash
                .iter()
                .map(|x| x.to_string())
                .collect_vec()
                .join(","),
            expected_final_stack_skip_program_hash
                .iter()
                .map(|x| x.to_string())
                .collect_vec()
                .join(","),
        );
    }

    // Verify that memory behaves as expected, except for the dyn malloc initialization address which
    // is too cumbersome to monitor this way. Its behavior should be tested elsewhere.
    // Alternatively the rust shadowing trait function must take a `Library` argument as input
    // and statically allocate memory from there.
    // TODO: Check if we could perform this check on dyn malloc too
    rust_memory.remove(&BFieldElement::new(DYN_MALLOC_ADDRESS as u64));
    tasm_memory.remove(&BFieldElement::new(DYN_MALLOC_ADDRESS as u64));
    let memory_difference = rust_memory
        .iter()
        .filter(|(k, v)| match tasm_memory.get(*k) {
            Some(b) => *b != **v,
            None => true,
        })
        .chain(
            tasm_memory
                .iter()
                .filter(|(k, v)| match rust_memory.get(*k) {
                    Some(b) => *b != **v,
                    None => true,
                }),
        )
        .collect_vec();
    if rust_memory != tasm_memory {
        let mut tasm_memory = tasm_memory.iter().collect_vec();
        tasm_memory.sort_unstable_by(|&a, &b| a.0.value().partial_cmp(&b.0.value()).unwrap());
        let tasm_mem_str = tasm_memory
            .iter()
            .map(|x| format!("({} => {})", x.0, x.1))
            .collect_vec()
            .join(",");

        let mut rust_memory = rust_memory.iter().collect_vec();
        rust_memory.sort_unstable_by(|&a, &b| a.0.value().partial_cmp(&b.0.value()).unwrap());
        let rust_mem_str = rust_memory
            .iter()
            .map(|x| format!("({} => {})", x.0, x.1))
            .collect_vec()
            .join(",");
        let diff_str = memory_difference
            .iter()
            .map(|x| format!("({} => {})", x.0, x.1))
            .collect_vec()
            .join(",");
        panic!(
            "Memory for both implementations must match after execution.\n\nTVM: {tasm_mem_str}\n\nRust: {rust_mem_str}\n\nDifference: {diff_str}\n\nCode was:\n\n {}",
            snippet_struct.function_code(&mut SnippetState::default())
        );
    }

    // Write back memory to be able to probe it in individual tests
    *memory = tasm_memory.clone();

    // Verify that stack grows with expected number of elements
    let stack_final = tasm_stack.clone();
    let observed_stack_growth: isize = stack_final.len() as isize - init_stack.len() as isize;
    let expected_stack_growth: isize =
        snippet_struct.outputs().len() as isize - snippet_struct.inputs().len() as isize;
    assert_eq!(
        expected_stack_growth,
        observed_stack_growth,
        "Stack must pop and push expected number of elements. Got input: {}\nGot output: {}",
        init_stack.iter().map(|x| x.to_string()).join(","),
        stack_final.iter().map(|x| x.to_string()).join(",")
    );

    vm_output_state
}

#[cfg(test)]
mod test {
    use std::collections::HashMap;

    use rand::random;
    use triton_vm::BFieldElement;
    use twenty_first::shared_math::tip5::DIGEST_LENGTH;

    use crate::{get_init_tvm_stack, hashing::sample_indices::SampleIndices, list::ListType};

    use super::test_rust_equivalence_given_input_values_and_initial_stacks_and_memories;

    /// TIP6 sets the bottom of the stack to the program hash. While testing Snippets,
    /// which are not standalone programs and therefore do not come with a well defined
    /// program hash, we want to verify that the tasm and rust stacks are identical up
    /// to these first five elements. This unit test tests this.
    #[test]
    fn test_program_hash_ignored() {
        let snippet_struct = SampleIndices {
            list_type: ListType::Safe,
        };
        let mut stack = get_init_tvm_stack();
        stack.push(BFieldElement::new(45u64));
        stack.push(BFieldElement::new(1u64 << 12));

        let mut init_memory = HashMap::new();
        let mut tasm_stack = stack.to_vec();
        for item in tasm_stack.iter_mut().take(DIGEST_LENGTH) {
            *item = random();
        }

        let mut tasm_memory = init_memory.clone();

        let mut rust_memory = init_memory.clone();
        let mut rust_stack = stack.to_vec();

        test_rust_equivalence_given_input_values_and_initial_stacks_and_memories(
            &snippet_struct,
            &stack,
            &[],
            &[],
            &mut init_memory,
            1,
            None,
            &mut tasm_stack,
            &mut rust_stack,
            &mut tasm_memory,
            &mut rust_memory,
        );
    }
}
