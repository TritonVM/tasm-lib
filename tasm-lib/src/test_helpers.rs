use std::collections::HashMap;
use std::fmt::Display;

use itertools::Itertools;
use triton_vm::isa::op_stack::NUM_OP_STACK_REGISTERS;
use triton_vm::prelude::*;

use crate::InitVmState;
use crate::RustShadowOutputState;
use crate::dyn_malloc::DYN_MALLOC_ADDRESS;
use crate::execute_test;
use crate::execute_with_terminal_state;
use crate::prelude::Tip5;
use crate::traits::basic_snippet::SignedOffSnippet;
use crate::traits::rust_shadow::RustShadow;

pub fn rust_final_state<T: RustShadow>(
    shadowed_snippet: &T,
    stack: &[BFieldElement],
    stdin: &[BFieldElement],
    nondeterminism: &NonDeterminism,
    sponge: &Option<Tip5>,
) -> RustShadowOutputState {
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

    RustShadowOutputState {
        public_output: output,
        stack: rust_stack,
        ram: rust_memory,
        sponge: rust_sponge,
    }
}

pub fn tasm_final_state<T: RustShadow>(
    shadowed_snippet: &T,
    stack: &[BFieldElement],
    stdin: &[BFieldElement],
    nondeterminism: NonDeterminism,
    sponge: &Option<Tip5>,
) -> VMState {
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
pub fn verify_stack_equivalence(
    stack_a_name: &str,
    stack_a: &[BFieldElement],
    stack_b_name: &str,
    stack_b: &[BFieldElement],
) {
    let stack_a_name = format!("{stack_a_name}:");
    let stack_b_name = format!("{stack_b_name}:");
    let max_stack_name_len = stack_a_name.len().max(stack_b_name.len());

    let stack_a = &stack_a[Digest::LEN..];
    let stack_b = &stack_b[Digest::LEN..];
    let display = |stack: &[BFieldElement]| stack.iter().join(",");
    assert_eq!(
        stack_a,
        stack_b,
        "{stack_a_name} stack must match {stack_b_name} stack\n\n\
         {stack_a_name:<max_stack_name_len$} {}\n\n\
         {stack_b_name:<max_stack_name_len$} {}",
        display(stack_a),
        display(stack_b),
    );
}

/// Verify equivalence of memory up to the value of dynamic allocator.
pub(crate) fn verify_memory_equivalence(
    a_name: &str,
    a_memory: &HashMap<BFieldElement, BFieldElement>,
    b_name: &str,
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
        .filter(|&(k, v)| b_memory.get(k).map(|b| b != v).unwrap_or(true));
    let in_b_and_different_in_a = b_memory
        .iter()
        .filter(|&(k, v)| a_memory.get(k).map(|b| b != v).unwrap_or(true));

    let in_a_and_different_in_b = format_hash_map_iterator(in_a_and_different_in_b);
    let in_b_and_different_in_a = format_hash_map_iterator(in_b_and_different_in_a);

    panic!(
        "Memory for both implementations must match after execution.\n\n\
        In {b_name}, different in {a_name}: {in_b_and_different_in_a}\n\n\
        In {a_name}, different in {b_name}: {in_a_and_different_in_b}"
    );
}

pub fn verify_stack_growth<T: RustShadow>(
    shadowed_snippet: &T,
    initial_stack: &[BFieldElement],
    final_stack: &[BFieldElement],
) {
    let observed_stack_growth: isize = final_stack.len() as isize - initial_stack.len() as isize;
    let expected_stack_growth: isize = shadowed_snippet.inner().stack_diff();
    assert_eq!(
        expected_stack_growth,
        observed_stack_growth,
        "Stack must pop and push expected number of elements. Got input: {}\nGot output: {}",
        initial_stack.iter().map(|x| x.to_string()).join(","),
        final_stack.iter().map(|x| x.to_string()).join(",")
    );
}

pub fn verify_sponge_equivalence(a: &Option<Tip5>, b: &Option<Tip5>) {
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
    nondeterminism: &NonDeterminism,
    sponge: &Option<Tip5>,
    expected_final_stack: Option<&[BFieldElement]>,
) -> VMState {
    shadowed_snippet
        .inner()
        .assert_all_sign_offs_are_up_to_date();

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
        rust.public_output, tasm.public_output,
        "Rust shadowing and VM std out must agree"
    );

    verify_stack_equivalence(
        "rust-shadow final stack",
        &rust.stack,
        "TASM final stack",
        &tasm.op_stack.stack,
    );
    if let Some(expected) = expected_final_stack {
        verify_stack_equivalence("expected", expected, "actual", &rust.stack);
    }
    verify_memory_equivalence("Rust-shadow", &rust.ram, "TVM", &tasm.ram);
    verify_stack_growth(shadowed_snippet, &init_stack, &tasm.op_stack.stack);

    tasm
}

pub fn link_and_run_tasm_for_test<T: RustShadow>(
    snippet_struct: &T,
    stack: &mut Vec<BFieldElement>,
    std_in: Vec<BFieldElement>,
    nondeterminism: NonDeterminism,
    maybe_sponge: Option<Tip5>,
) -> VMState {
    let code = snippet_struct.inner().link_for_isolated_run();

    execute_test(
        &code,
        stack,
        snippet_struct.inner().stack_diff(),
        std_in,
        nondeterminism,
        maybe_sponge,
    )
}

pub fn test_rust_equivalence_given_execution_state<S: RustShadow>(
    snippet_struct: &S,
    execution_state: InitVmState,
) -> VMState {
    test_rust_equivalence_given_complete_state::<S>(
        snippet_struct,
        &execution_state.stack,
        &execution_state.public_input,
        &execution_state.nondeterminism,
        &execution_state.sponge,
        None,
    )
}

pub fn negative_test<T: RustShadow>(
    snippet: &T,
    initial_state: InitVmState,
    allowed_errors: &[InstructionError],
) {
    let err = instruction_error_from_failing_code(snippet, initial_state);
    assert!(
        allowed_errors.contains(&err),
        "Triton VM execution must fail with one of the expected errors:\n- {}\n\n Got:\n{err}",
        allowed_errors.iter().join("\n- ")
    );
}

pub fn test_assertion_failure<S: RustShadow>(
    snippet_struct: &S,
    initial_state: InitVmState,
    expected_error_ids: &[i128],
) {
    let err = instruction_error_from_failing_code(snippet_struct, initial_state);
    let maybe_error_id = match err {
        InstructionError::AssertionFailed(err)
        | InstructionError::VectorAssertionFailed(_, err) => err.id,
        _ => panic!("Triton VM execution failed, but not due to an assertion. Instead, got: {err}"),
    };
    let error_id = maybe_error_id.expect(
        "Triton VM execution failed due to unfulfilled assertion, but that assertion has no \
        error ID. See `tasm-lib/src/assertion_error_ids.md` to grab a unique ID.",
    );
    let expected_error_ids_str = expected_error_ids.iter().join(", ");
    assert!(
        expected_error_ids.contains(&error_id),
        "error ID {error_id} ∉ {{{expected_error_ids_str}}}\nTriton VM execution failed due to \
         unfulfilled assertion with error ID {error_id}, but expected one of the following IDs: \
         {{{expected_error_ids_str}}}",
    );
}

fn instruction_error_from_failing_code<S: RustShadow>(
    snippet: &S,
    init_state: InitVmState,
) -> InstructionError {
    // `AssertUnwindSafe` is fine because the caught panic is discarded immediately
    let rust_result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
        let mut rust_stack = init_state.stack.clone();
        let mut rust_memory = init_state.nondeterminism.ram.clone();
        let mut rust_sponge = init_state.sponge.clone();
        snippet.rust_shadow_wrapper(
            &init_state.public_input,
            &init_state.nondeterminism,
            &mut rust_stack,
            &mut rust_memory,
            &mut rust_sponge,
        )
    }));
    assert!(
        rust_result.is_err(),
        "Failed to fail: Rust-shadowing must panic in negative test case"
    );

    let code = snippet.inner().link_for_isolated_run();
    let tvm_result = execute_with_terminal_state(
        Program::new(&code),
        &init_state.public_input,
        &init_state.stack,
        &init_state.nondeterminism,
        init_state.sponge,
    );

    tvm_result.expect_err("Failed to fail: Triton VM execution must crash in negative test case")
}

pub fn prepend_program_with_stack_setup(
    init_stack: &[BFieldElement],
    program: &Program,
) -> Program {
    let stack_initialization_code = init_stack
        .iter()
        .skip(NUM_OP_STACK_REGISTERS)
        .map(|&word| triton_instr!(push word))
        .collect_vec();

    Program::new(&[stack_initialization_code, program.labelled_instructions()].concat())
}

pub fn prepend_program_with_sponge_init(program: &Program) -> Program {
    Program::new(&[triton_asm!(sponge_init), program.labelled_instructions()].concat())
}

/// Store the output from Triton VM's `proof` function as files, such that a deterministic
/// proof can be used for debugging purposes.
pub fn maybe_write_tvm_output_to_disk(
    stark: &Stark,
    claim: &triton_vm::proof::Claim,
    proof: &Proof,
) {
    use std::io::Write;
    let Ok(_) = std::env::var("TASMLIB_STORE") else {
        return;
    };

    let mut stark_file = std::fs::File::create("stark.json").unwrap();
    let state = serde_json::to_string(stark).unwrap();
    write!(stark_file, "{state}").unwrap();
    let mut claim_file = std::fs::File::create("claim.json").unwrap();
    let claim = serde_json::to_string(claim).unwrap();
    write!(claim_file, "{claim}").unwrap();
    let mut proof_file = std::fs::File::create("proof.json").unwrap();
    let proof = serde_json::to_string(proof).unwrap();
    write!(proof_file, "{proof}").unwrap();
}
