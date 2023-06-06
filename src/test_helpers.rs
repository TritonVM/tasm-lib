use std::collections::HashMap;

use itertools::Itertools;
use twenty_first::shared_math::b_field_element::BFieldElement;

use crate::dyn_malloc::DYN_MALLOC_ADDRESS;
use crate::snippet::Snippet;
use crate::snippet_state::SnippetState;
use crate::{all_snippets, rust_shadowing_helper_functions, ExecutionResult};

#[allow(dead_code)]
pub fn rust_tasm_equivalence_prop_new<T: Snippet>(
    snippet_struct: &T,
    export_snippet: bool,
) -> Vec<ExecutionResult> {
    // Verify that snippet can be found in `all_snippets`, so it's visible to the outside
    // This call will panic if snippet is not found in that function call
    // The data type value is a dummy value for all snippets except those that handle lists.
    if export_snippet {
        let looked_up_snippet = all_snippets::name_to_snippet(&snippet_struct.entrypoint());
        assert_eq!(
            snippet_struct.entrypoint(),
            looked_up_snippet.entrypoint(),
            "Looked up snippet must match self"
        );
    }

    let mut execution_states = snippet_struct.gen_input_states();

    let mut final_execution_results = vec![];
    for execution_state in execution_states.iter_mut() {
        let stack_init = execution_state.stack.clone();
        let execution_result = rust_tasm_equivalence_prop::<T>(
            snippet_struct,
            &execution_state.stack,
            &execution_state.std_in,
            &execution_state.secret_in,
            &mut execution_state.memory,
            execution_state.words_allocated,
            None,
        );

        // Verify that stack grows with expected number of elements
        let stack_final = execution_result.final_stack.clone();
        let observed_stack_growth: isize = stack_final.len() as isize - stack_init.len() as isize;
        let expected_stack_growth: isize =
            snippet_struct.outputs().len() as isize - snippet_struct.inputs().len() as isize;
        assert_eq!(
            expected_stack_growth,
            observed_stack_growth,
            "Stack must pop and push expected number of elements. Got input: {}\nGot output: {}",
            stack_init.iter().map(|x| x.to_string()).join(","),
            stack_final.iter().map(|x| x.to_string()).join(",")
        );

        final_execution_results.push(execution_result);
    }

    final_execution_results
}

#[allow(dead_code)]
pub fn rust_tasm_equivalence_prop<T: Snippet>(
    snippet_struct: &T,
    stack: &[BFieldElement],
    stdin: &[BFieldElement],
    secret_in: &[BFieldElement],
    memory: &mut HashMap<BFieldElement, BFieldElement>,
    words_statically_allocated: usize,
    expected: Option<&[BFieldElement]>,
) -> ExecutionResult {
    let init_memory = memory.clone();
    let mut tasm_stack = stack.to_vec();
    let mut tasm_memory = init_memory.clone();
    let execution_result = snippet_struct.run_tasm_old(
        &mut tasm_stack,
        stdin.to_vec(),
        secret_in.to_vec(),
        &mut tasm_memory,
        words_statically_allocated,
    );
    println!(
        "Cycle count for `{}`: {}",
        snippet_struct.entrypoint(),
        execution_result.cycle_count
    );
    println!(
        "Hash table height for `{}`: {}",
        snippet_struct.entrypoint(),
        execution_result.hash_table_height
    );

    let mut rust_memory = init_memory;
    let mut rust_stack = stack.to_vec();

    if words_statically_allocated > 0 {
        rust_shadowing_helper_functions::dyn_malloc::rust_dyn_malloc_initialize(
            &mut rust_memory,
            words_statically_allocated + 1,
        );
    }
    snippet_struct.rust_shadowing(
        &mut rust_stack,
        stdin.to_vec(),
        secret_in.to_vec(),
        &mut rust_memory,
    );

    assert_eq!(
        tasm_stack,
        rust_stack,
        "Rust code must match TVM for `{}`\n\nTVM: {}\n\nRust: {}. Code was: {}",
        snippet_struct.entrypoint(),
        tasm_stack
            .iter()
            .map(|x| x.to_string())
            .collect_vec()
            .join(","),
        rust_stack
            .iter()
            .map(|x| x.to_string())
            .collect_vec()
            .join(","),
        snippet_struct.function_body(&mut SnippetState::default())
    );
    if let Some(expected) = expected {
        assert_eq!(
            tasm_stack,
            expected,
            "TVM must produce expected stack `{}`. \n\nTVM:\n{}\nExpected:\n{}",
            snippet_struct.entrypoint(),
            tasm_stack
                .iter()
                .map(|x| x.to_string())
                .collect_vec()
                .join(","),
            expected
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
    rust_memory.remove(&BFieldElement::new(DYN_MALLOC_ADDRESS as u64));
    tasm_memory.remove(&BFieldElement::new(DYN_MALLOC_ADDRESS as u64));
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
        panic!(
            "Memory for both implementations must match after execution.\n\nTVM: {tasm_mem_str}\n\nRust: {rust_mem_str}. Code was:\n\n {}",
            snippet_struct.function_body(&mut SnippetState::default())
        );
    }

    // Write back memory to be able to probe it in individual tests
    *memory = tasm_memory;

    execution_result
}
