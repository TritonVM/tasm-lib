use std::cell::RefCell;
use std::rc::Rc;

use triton_vm::prelude::*;

use crate::library::Library;
use crate::prove_and_verify;
use crate::traits::basic_snippet::BasicSnippet;
use crate::ExecutionResult;
use crate::VmHasher;

pub fn link_for_isolated_run<T: BasicSnippet>(
    snippet: Rc<RefCell<T>>,
    words_statically_allocated: usize,
) -> Vec<LabelledInstruction> {
    let mut snippet_state = Library::with_preallocated_memory(words_statically_allocated as u32);
    let entrypoint = snippet.borrow().entrypoint();
    let function_body = snippet.borrow().annotated_code(&mut snippet_state);
    let library_code = snippet_state.all_imports();

    // The TASM code is always run through a function call, so the 1st instruction
    // is a call to the function in question.
    let code = triton_asm!(
        call {entrypoint}
        halt

        {&function_body}
        {&library_code}
    );

    code
}

/// Execute a Triton-VM program and return its output and execution trace length
pub fn execute_bench(
    code: &[LabelledInstruction],
    stack: &[BFieldElement],
    std_in: Vec<BFieldElement>,
    nondeterminism: NonDeterminism<BFieldElement>,
    sponge: Option<VmHasher>,
) -> ExecutionResult {
    let program = Program::new(code);
    let public_input = PublicInput::new(std_in.clone());

    let mut vm_state = VMState::new(&program, public_input.clone(), nondeterminism.clone());
    vm_state.op_stack.stack = stack.to_vec();
    vm_state.sponge = sponge;
    let (simulation_trace, end_state) = program.trace_execution_of_state(vm_state).unwrap();

    // If this environment variable is set, all programs, including the code to prepare the state,
    // will be proven and then verified.
    // Notice that this is only done after the successful execution of the program above, so all
    // produced proofs here should be valid.
    // If you run this, make sure `opt-level` is set to 3.
    if std::env::var("DYING_TO_PROVE").is_ok() {
        prove_and_verify(&program, &std_in, &nondeterminism, &end_state.public_output);
    }

    ExecutionResult {
        output: end_state.public_output,
        final_stack: end_state.op_stack.stack,
        final_ram: end_state.ram,
        cycle_count: end_state.cycle_count as usize,
        hash_table_height: simulation_trace.hash_table_length(),
        u32_table_height: simulation_trace.u32_table_length(),
    }
}
