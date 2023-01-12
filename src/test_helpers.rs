use std::collections::HashMap;

use itertools::Itertools;
use twenty_first::shared_math::b_field_element::BFieldElement;

use crate::snippet::NewSnippet;
use crate::snippet::Snippet;
use crate::ExecutionResult;

#[allow(dead_code)]
pub fn rust_tasm_equivalence_prop_new<T: NewSnippet>() {
    let mut execution_states = T::gen_input_states();
    for execution_state in execution_states.iter_mut() {
        let _execution_result = rust_tasm_equivalence_prop::<T>(
            &execution_state.stack,
            &execution_state.std_in,
            &execution_state.secret_in,
            &mut execution_state.memory,
            execution_state.words_allocated,
            None,
        );
    }
}

#[allow(dead_code)]
pub fn rust_tasm_equivalence_prop<T: Snippet>(
    stack: &[BFieldElement],
    stdin: &[BFieldElement],
    secret_in: &[BFieldElement],
    memory: &mut HashMap<BFieldElement, BFieldElement>,
    words_allocated: usize,
    expected: Option<&[BFieldElement]>,
) -> ExecutionResult {
    let init_memory = memory.clone();
    let mut tasm_stack = stack.to_vec();
    let mut tasm_memory = init_memory.clone();
    let execution_result = T::run_tasm(
        &mut tasm_stack,
        stdin.to_vec(),
        secret_in.to_vec(),
        &mut tasm_memory,
        words_allocated,
    );
    println!(
        "Cycle count for `{}`: {}",
        T::entrypoint(),
        execution_result.cycle_count
    );
    println!(
        "Hash table height for `{}`: {}",
        T::entrypoint(),
        execution_result.hash_table_height
    );

    let mut rust_memory = init_memory;
    let mut rust_stack = stack.to_vec();
    T::rust_shadowing(
        &mut rust_stack,
        stdin.to_vec(),
        secret_in.to_vec(),
        &mut rust_memory,
    );

    assert_eq!(
        tasm_stack,
        rust_stack,
        "Rust code must match TVM for `{}`\n\nTVM: {}\n\nRust: {}",
        T::entrypoint(),
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
    );
    if let Some(expected) = expected {
        assert_eq!(
            tasm_stack,
            expected,
            "TVM must produce expected stack `{}`. \n\nTVM:\n{}\nExpected:\n{}",
            T::entrypoint(),
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

    // Verify that memory behaves as expected
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
        panic!("Memory for both implementations must match after execution.\n\nTVM: {tasm_mem_str}\n\nRust: {rust_mem_str}",)
    }

    // Write back memory to be able to probe it in individual tests
    *memory = tasm_memory;

    execution_result
}
