use std::collections::HashMap;
use std::fmt::Display;

use itertools::Itertools;
use triton_vm::prelude::*;

use crate::dyn_malloc::DYN_MALLOC_ADDRESS;
use crate::execute_test;
use crate::exported_snippets;
use crate::library::Library;
use crate::traits::basic_snippet::BasicSnippet;
use crate::traits::deprecated_snippet::DeprecatedSnippet;
use crate::traits::rust_shadow::RustShadow;
use crate::ExecutionState;
use crate::VmHasher;
use crate::VmOutputState;
use crate::DIGEST_LENGTH;

#[allow(dead_code)]
pub fn test_rust_equivalence_multiple_deprecated<T: DeprecatedSnippet>(
    snippet_struct: &T,
    export_snippet: bool,
) -> Vec<VmOutputState> {
    // Verify that snippet can be found in `all_snippets`, so that
    // it iss visible to the outside.
    // This call will panic if snippet is not found in that
    // function call. The data type value is a dummy value for all
    // snippets except those that handle lists.
    if export_snippet {
        let looked_up_snippet = exported_snippets::name_to_snippet(&snippet_struct.entrypoint());
        assert_eq!(
            snippet_struct.entrypoint(),
            looked_up_snippet.entrypoint(),
            "Looked up snippet must match self"
        );
    }

    let mut vm_output_states = vec![];
    for execution_state in snippet_struct.gen_input_states() {
        let vm_output_state = test_rust_equivalence_given_execution_state_deprecated::<T>(
            snippet_struct,
            execution_state,
        );
        vm_output_states.push(vm_output_state);
    }

    vm_output_states
}

#[allow(dead_code)]
pub fn test_rust_equivalence_given_execution_state_deprecated<T: DeprecatedSnippet>(
    snippet_struct: &T,
    execution_state: ExecutionState,
) -> VmOutputState {
    test_rust_equivalence_given_complete_state_deprecated::<T>(
        snippet_struct,
        &execution_state.stack,
        &execution_state.std_in,
        &execution_state.nondeterminism,
        None,
    )
}

pub fn test_rust_equivalence_given_input_values_deprecated<T: DeprecatedSnippet>(
    snippet_struct: &T,
    stack: &[BFieldElement],
    stdin: &[BFieldElement],
    memory: HashMap<BFieldElement, BFieldElement>,
    expected_final_stack: Option<&[BFieldElement]>,
) -> VmOutputState {
    let nondeterminism = NonDeterminism::<BFieldElement>::new(vec![]).with_ram(memory.clone());

    test_rust_equivalence_given_complete_state_deprecated(
        snippet_struct,
        stack,
        stdin,
        &nondeterminism,
        expected_final_stack,
    )
}

fn link_for_isolated_run_deprecated<T: DeprecatedSnippet>(
    snippet_struct: &T,
) -> Vec<LabelledInstruction> {
    let mut snippet_state = Library::new();
    let entrypoint = snippet_struct.entrypoint();
    let mut function_body = snippet_struct.function_code(&mut snippet_state);
    function_body.push('\n');
    let library_code = snippet_state.all_imports();

    // The TASM code is always run through a function call, so the 1st instruction
    // is a call to the function in question.
    let code = triton_asm!(
        call {entrypoint}
        halt

        {function_body}
        {&library_code}
    );

    code
}

pub fn link_and_run_tasm_for_test_deprecated<T: DeprecatedSnippet>(
    snippet_struct: &T,
    stack: &mut Vec<BFieldElement>,
    std_in: Vec<BFieldElement>,
    secret_in: Vec<BFieldElement>,
    memory: HashMap<BFieldElement, BFieldElement>,
) -> VmOutputState {
    let expected_length_prior: usize = snippet_struct
        .inputs()
        .iter()
        .map(|(x, _n)| x.stack_size())
        .sum();
    let expected_length_after: usize = snippet_struct
        .outputs()
        .iter()
        .map(|(x, _n)| x.stack_size())
        .sum();
    assert_eq!(
        snippet_struct.stack_diff(),
        expected_length_after as isize - expected_length_prior as isize,
        "Declared stack diff must match type indicators"
    );

    let code = link_for_isolated_run_deprecated(snippet_struct);
    let nondeterminism = NonDeterminism::<BFieldElement>::new(secret_in).with_ram(memory.clone());

    execute_test(
        &code,
        stack,
        snippet_struct.stack_diff(),
        std_in,
        nondeterminism,
        None,
    )
}

pub(crate) fn test_rust_equivalence_given_complete_state_deprecated<T: DeprecatedSnippet>(
    snippet_struct: &T,
    stack: &[BFieldElement],
    stdin: &[BFieldElement],
    nondeterminism: &NonDeterminism<BFieldElement>,
    expected_final_stack: Option<&[BFieldElement]>,
) -> VmOutputState {
    let init_stack = stack.to_vec();

    let mut rust_memory = nondeterminism.ram.clone();
    let tasm_memory = nondeterminism.ram.clone();
    let mut rust_stack = stack.to_vec();
    let mut tasm_stack = stack.to_vec();

    // run rust shadow
    snippet_struct.rust_shadowing(
        &mut rust_stack,
        stdin.to_vec(),
        nondeterminism.individual_tokens.clone(),
        &mut rust_memory,
    );

    // run tvm
    let vm_output_state = link_and_run_tasm_for_test_deprecated(
        snippet_struct,
        &mut tasm_stack,
        stdin.to_vec(),
        nondeterminism.individual_tokens.clone(),
        tasm_memory,
    );
    let mut tasm_memory = vm_output_state.final_ram.clone();

    // assert stacks are equal, up to program hash
    let tasm_stack_skip_program_hash = tasm_stack.iter().cloned().skip(DIGEST_LENGTH).collect_vec();
    let rust_stack_skip_program_hash = rust_stack.iter().cloned().skip(DIGEST_LENGTH).collect_vec();
    assert_eq!(
        tasm_stack_skip_program_hash,
        rust_stack_skip_program_hash,
        "Rust code must match TVM for `{}`\n\nTVM: {}\n\nRust: {}. Code was: {}",
        snippet_struct.entrypoint(),
        tasm_stack_skip_program_hash.iter().join(","),
        rust_stack_skip_program_hash.iter().join(","),
        snippet_struct
            .annotated_code(&mut Library::new())
            .iter()
            .join("\n")
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
            tasm_stack_skip_program_hash.iter().join(","),
            expected_final_stack_skip_program_hash.iter().join(","),
        );
    }

    // Verify that memory behaves as expected, except for the dyn malloc initialization address which
    // is too cumbersome to monitor this way. Its behavior should be tested elsewhere.
    // Alternatively the rust shadowing trait function must take a `Library` argument as input
    // and statically allocate memory from there.
    // TODO: Check if we could perform this check on dyn malloc too
    rust_memory.remove(&DYN_MALLOC_ADDRESS);
    tasm_memory.remove(&DYN_MALLOC_ADDRESS);

    if rust_memory != tasm_memory {
        fn format_hash_map_iterator<K, V>(map: impl Iterator<Item = (K, V)>) -> String
        where
            u64: From<K>,
            K: Copy + Display,
            V: Display,
        {
            map.sorted_by_key(|(k, _)| u64::from(*k))
                .map(|(k, v)| format!("({k} => {v})"))
                .join(",")
        }

        let in_rust_memory_and_different_in_tasm_memory = rust_memory
            .iter()
            .filter(|(k, &v)| tasm_memory.get(k).map(|&b| b != v).unwrap_or(true));
        let in_tasm_memory_and_different_in_rust_memory = tasm_memory
            .iter()
            .filter(|(k, &v)| rust_memory.get(k).map(|&b| b != v).unwrap_or(true));

        let in_rust_memory_and_different_in_tasm_memory =
            format_hash_map_iterator(in_rust_memory_and_different_in_tasm_memory);
        let in_tasm_memory_and_different_in_rust_memory =
            format_hash_map_iterator(in_tasm_memory_and_different_in_rust_memory);

        let tasm_mem = format_hash_map_iterator(tasm_memory.into_iter());
        let rust_mem = format_hash_map_iterator(rust_memory.into_iter());

        panic!(
            "Memory for both implementations must match after execution.\n\n\
            TVM:  {tasm_mem}\n\n\
            Rust: {rust_mem}\n\n\
            In TVM, different in rust: {in_tasm_memory_and_different_in_rust_memory}\n\n\
            In rust, different in TVM: {in_rust_memory_and_different_in_tasm_memory}\n\n\
            Code was:\n\n\
            {}",
            snippet_struct
                .annotated_code(&mut Library::new())
                .iter()
                .join("\n")
        );
    }

    // Verify that stack grows with expected number of elements
    let stack_final = tasm_stack.clone();
    let observed_stack_growth: isize = stack_final.len() as isize - init_stack.len() as isize;
    let expected_stack_growth: isize = snippet_struct.output_field_names().len() as isize
        - snippet_struct.input_field_names().len() as isize;
    assert_eq!(
        expected_stack_growth,
        observed_stack_growth,
        "Stack must pop and push expected number of elements. Got input: {}\nGot output: {}",
        init_stack.iter().map(|x| x.to_string()).join(","),
        stack_final.iter().map(|x| x.to_string()).join(",")
    );

    vm_output_state
}

pub fn rust_final_state<T: RustShadow>(
    shadowed_snippet: &T,
    stack: &[BFieldElement],
    stdin: &[BFieldElement],
    nondeterminism: &NonDeterminism<BFieldElement>,
    sponge: &Option<VmHasher>,
) -> VmOutputState {
    let mut rust_memory = nondeterminism.ram.clone();
    let mut rust_stack = stack.to_vec();
    let mut rust_sponge = sponge.clone();

    // run rust shadow
    let output = shadowed_snippet.rust_shadow_wrapper(
        stdin,
        nondeterminism,
        &mut rust_stack,
        &mut rust_memory,
        &mut rust_sponge,
    );

    VmOutputState {
        output,
        final_stack: rust_stack,
        final_ram: rust_memory,
        final_sponge: rust_sponge,
    }
}

pub fn tasm_final_state<T: RustShadow>(
    shadowed_snippet: &T,
    stack: &[BFieldElement],
    stdin: &[BFieldElement],
    nondeterminism: NonDeterminism<BFieldElement>,
    sponge: &Option<VmHasher>,
) -> VmOutputState {
    // run tvm
    link_and_run_tasm_for_test(
        shadowed_snippet,
        &mut stack.to_vec(),
        stdin.to_vec(),
        nondeterminism,
        sponge.to_owned(),
    )
}

/// assert stacks are equal, up to program hash
pub fn verify_stack_equivalence(stack_a: &[BFieldElement], stack_b: &[BFieldElement]) {
    let stack_a = &stack_a[DIGEST_LENGTH..];
    let stack_b = &stack_b[DIGEST_LENGTH..];
    let display = |stack: &[BFieldElement]| stack.iter().map(|&x| x.to_string()).join(",");
    assert_eq!(
        stack_a,
        stack_b,
        "A stack must match B stack\n\nA: {}\n\nB: {}",
        display(stack_a),
        display(stack_b),
    );
}

/// Verify equivalence of memory up to the value of dynamic allocator.
pub(crate) fn verify_memory_equivalence(
    a_memory: &HashMap<BFieldElement, BFieldElement>,
    b_memory: &HashMap<BFieldElement, BFieldElement>,
) {
    let memory_without_dyn_malloc = |mem: HashMap<_, _>| -> HashMap<_, _> {
        mem.into_iter()
            .filter(|&(k, _)| k != DYN_MALLOC_ADDRESS)
            .collect()
    };
    let a_memory = memory_without_dyn_malloc(a_memory.clone());
    let b_memory = memory_without_dyn_malloc(b_memory.clone());
    if a_memory == b_memory {
        return;
    }

    fn format_hash_map_iterator<K, V>(map: impl Iterator<Item = (K, V)>) -> String
    where
        u64: From<K>,
        K: Copy + Display,
        V: Display,
    {
        map.sorted_by_key(|(k, _)| u64::from(*k))
            .map(|(k, v)| format!("({k} => {v})"))
            .join(", ")
    }

    let in_a_and_different_in_b = a_memory
        .iter()
        .filter(|(k, &v)| b_memory.get(k).map(|&b| b != v).unwrap_or(true));
    let in_b_and_different_in_a = b_memory
        .iter()
        .filter(|(k, &v)| a_memory.get(k).map(|&b| b != v).unwrap_or(true));

    let in_a_and_different_in_b = format_hash_map_iterator(in_a_and_different_in_b);
    let in_b_and_different_in_a = format_hash_map_iterator(in_b_and_different_in_a);

    panic!(
        "Memory for both implementations must match after execution.\n\n\
        In B, different in A: {in_b_and_different_in_a}\n\n\
        In A, different in B: {in_a_and_different_in_b}"
    );
}

pub fn verify_stack_growth<T: RustShadow>(
    shadowed_snippet: &T,
    initial_stack: &[BFieldElement],
    final_stack: &[BFieldElement],
) {
    let observed_stack_growth: isize = final_stack.len() as isize - initial_stack.len() as isize;
    let expected_stack_growth: isize = shadowed_snippet.inner().borrow().stack_diff();
    assert_eq!(
        expected_stack_growth,
        observed_stack_growth,
        "Stack must pop and push expected number of elements. Got input: {}\nGot output: {}",
        initial_stack.iter().map(|x| x.to_string()).join(","),
        final_stack.iter().map(|x| x.to_string()).join(",")
    );
}

pub fn verify_sponge_equivalence(a: &Option<VmHasher>, b: &Option<VmHasher>) {
    match (a, b) {
        (Some(state_a), Some(state_b)) => assert_eq!(state_a.state, state_b.state),
        (None, None) => (),
        _ => panic!("{a:?} != {b:?}"),
    };
}

pub fn test_rust_equivalence_given_complete_state<T: RustShadow>(
    shadowed_snippet: &T,
    stack: &[BFieldElement],
    stdin: &[BFieldElement],
    nondeterminism: &NonDeterminism<BFieldElement>,
    sponge: &Option<VmHasher>,
    expected_final_stack: Option<&[BFieldElement]>,
) -> VmOutputState {
    let init_stack = stack.to_vec();

    let rust = rust_final_state(shadowed_snippet, stack, stdin, nondeterminism, sponge);

    // run tvm
    let tasm = tasm_final_state(
        shadowed_snippet,
        stack,
        stdin,
        nondeterminism.clone(),
        sponge,
    );

    assert_eq!(
        rust.output, tasm.output,
        "Rust shadowing and VM std out must agree"
    );

    verify_stack_equivalence(&rust.final_stack, &tasm.final_stack);
    if let Some(expected) = expected_final_stack {
        verify_stack_equivalence(expected, &rust.final_stack);
    }
    verify_memory_equivalence(&rust.final_ram, &tasm.final_ram);
    verify_stack_growth(shadowed_snippet, &init_stack, &tasm.final_stack);

    tasm
}

pub fn link_and_run_tasm_for_test<T: RustShadow>(
    snippet_struct: &T,
    stack: &mut Vec<BFieldElement>,
    std_in: Vec<BFieldElement>,
    nondeterminism: NonDeterminism<BFieldElement>,
    maybe_sponge: Option<VmHasher>,
) -> VmOutputState {
    let code = link_for_isolated_run(snippet_struct);

    execute_test(
        &code,
        stack,
        snippet_struct.inner().borrow().stack_diff(),
        std_in,
        nondeterminism,
        maybe_sponge,
    )
}

fn link_for_isolated_run<T: RustShadow>(snippet_struct: &T) -> Vec<LabelledInstruction> {
    let mut library = Library::new();
    let entrypoint = snippet_struct.inner().borrow().entrypoint();
    let function_body = snippet_struct.inner().borrow().annotated_code(&mut library);
    let library_code = library.all_imports();

    // The TASM code is always run through a function call, so the 1st instruction is a call to the
    // function in question.
    let code = triton_asm!(
        call {entrypoint}
        halt

        {&function_body}
        {&library_code}
    );

    code
}

#[allow(dead_code)]
pub fn test_rust_equivalence_given_execution_state<T: BasicSnippet + RustShadow>(
    snippet_struct: &T,
    execution_state: ExecutionState,
) -> VmOutputState {
    let nondeterminism = execution_state.nondeterminism;
    test_rust_equivalence_given_complete_state::<T>(
        snippet_struct,
        &execution_state.stack,
        &execution_state.std_in,
        &nondeterminism,
        &None,
        None,
    )
}

#[cfg(test)]
mod test {
    use rand::random;

    use crate::empty_stack;
    use crate::hashing::algebraic_hasher::hash_varlen::HashVarlen;

    use super::*;

    /// TIP6 sets the bottom of the stack to the program hash. While testing Snippets,
    /// which are not standalone programs and therefore do not come with a well defined
    /// program hash, we want to verify that the tasm and rust stacks are identical up
    /// to these first five elements. This unit test tests this.
    #[test]
    fn test_program_hash_ignored() {
        // arbitrary snippet that does something related to hashing
        let snippet_struct = HashVarlen;
        let mut stack = empty_stack();
        stack.push(BFieldElement::new(45u64));
        stack.push(BFieldElement::new(1u64 << 12));

        let mut tasm_stack = stack.to_vec();
        for item in tasm_stack.iter_mut().take(DIGEST_LENGTH) {
            *item = random();
        }

        test_rust_equivalence_given_complete_state_deprecated(
            &snippet_struct,
            &stack,
            &[],
            &NonDeterminism::default(),
            None,
        );
    }
}
