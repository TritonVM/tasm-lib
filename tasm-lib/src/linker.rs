use std::{cell::RefCell, collections::HashMap, rc::Rc};

use triton_vm::{
    instruction::LabelledInstruction, triton_asm, triton_instr, BFieldElement, NonDeterminism,
    Program, PublicInput,
};

use crate::{
    library::Library, prove_and_verify, snippet::BasicSnippet, state_preparation_code,
    ExecutionResult,
};

pub fn link_for_isolated_run<T: BasicSnippet>(
    snippet: Rc<RefCell<T>>,
    words_statically_allocated: usize,
) -> Vec<LabelledInstruction> {
    let mut snippet_state = Library::with_preallocated_memory(words_statically_allocated);
    let entrypoint = snippet.borrow().entrypoint();
    let function_body = snippet.borrow().code(&mut snippet_state);
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
    memory: &HashMap<BFieldElement, BFieldElement>,
    initilialize_dynamic_allocator_to: Option<usize>,
) -> ExecutionResult {
    // Prepend to program the initial stack values and initial memory values
    // such that stack is in the expected state when program logic is executed
    let prep: Vec<LabelledInstruction> =
        state_preparation_code(stack, memory, initilialize_dynamic_allocator_to);

    // Add the program after the stack initialization has been performed
    // Find the length of code used for setup. This length does not count towards
    // execution length of snippet so it must be subtracted at the end.
    let initialization_program = Program::new(&[prep.clone(), vec![triton_instr!(halt)]].concat());
    let terminal_state = initialization_program.debug_terminal_state(
        PublicInput::new(vec![]),
        NonDeterminism::new(vec![]),
        None,
        None,
    );
    let initialization_clock_cycle_count = terminal_state.unwrap().cycle_count as usize;

    // Construct the whole program (inclusive setup) to be run
    let mut executed_code = prep;
    executed_code.extend_from_slice(code);
    let extended_program = Program::new(&executed_code);

    // // Run the program, including the stack preparation and memory preparation logic
    // let (execution_trace, err) = extended_program.debug(
    //     PublicInput::new(std_in.clone()),
    //     nondeterminism.clone(),
    //     None,
    //     None,
    // );
    // if let Some(e) = err {
    //     bail!(
    //         "`debug` failed with error: {e}\nLast state before crash:\n{}",
    //         execution_trace.last().unwrap()
    //     )
    // }

    // Simulate the program, since this gives us hash table output
    let (simulation_trace, _) = extended_program
        .trace_execution(PublicInput::new(std_in.clone()), nondeterminism.clone())
        .unwrap();

    let end_state = extended_program
        .debug_terminal_state(
            PublicInput::new(std_in.clone()),
            nondeterminism.clone(),
            None,
            None,
        )
        .unwrap();

    // If this environment variable is set, all programs, including the code to prepare the state,
    // will be proven and then verified.
    // Notice that this is only done after the successful execution of the program above, so all
    // produced proofs here should be valid.
    // If you run this, make sure `opt-level` is set to 3.
    if std::env::var("DYING_TO_PROVE").is_ok() {
        prove_and_verify(
            &extended_program,
            &std_in,
            &nondeterminism,
            &end_state.public_output,
        );
    }

    ExecutionResult {
        output: end_state.public_output,

        final_stack: end_state.op_stack.stack,

        final_ram: end_state.ram,

        // Cycle count is cycles it took to run program excluding the cycles that were
        // spent on preparing the stack
        cycle_count: simulation_trace.processor_trace.nrows()
            - initialization_clock_cycle_count
            - 1,

        // Number of rows generated in the hash table after simulating program
        hash_table_height: simulation_trace.hash_trace.nrows(),

        // Number of rows generated in the u32 table after simulating program
        u32_table_height: simulation_trace.u32_table_length(),
    }
}
