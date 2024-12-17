use triton_vm::prelude::*;

use crate::prelude::*;
use crate::prove_and_verify;
use crate::snippet_bencher::BenchmarkResult;

pub fn link_for_isolated_run<T: BasicSnippet>(snippet: &T) -> Vec<LabelledInstruction> {
    let mut library = Library::new();
    let entrypoint = snippet.entrypoint();
    let function_body = snippet.annotated_code(&mut library);
    let library_code = library.all_imports();

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
    nondeterminism: NonDeterminism,
    sponge: Option<Tip5>,
) -> BenchmarkResult {
    let program = Program::new(code);
    let public_input = PublicInput::new(std_in.clone());

    let mut vm_state = VMState::new(
        program.clone(),
        public_input.clone(),
        nondeterminism.clone(),
    );
    vm_state.op_stack.stack = stack.to_vec();
    vm_state.sponge = sponge;
    let (simulation_trace, _) = VM::trace_execution_of_state(vm_state).unwrap();

    // If this environment variable is set, all programs, including the code to prepare the state,
    // will be proven and then verified.
    // Notice that this is only done after the successful execution of the program above, so all
    // produced proofs here should be valid.
    // If you run this, make sure `opt-level` is set to 3.
    if std::env::var("DYING_TO_PROVE").is_ok() {
        prove_and_verify(program, &std_in, &nondeterminism, Some(stack.to_vec()));
    }

    BenchmarkResult::new(&simulation_trace)
}
