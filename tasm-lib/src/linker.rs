use triton_vm::prelude::*;

use crate::prelude::*;
use crate::prove_and_verify;
use crate::snippet_bencher::BenchmarkResult;

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
