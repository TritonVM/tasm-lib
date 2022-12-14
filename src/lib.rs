use num_traits::Zero;
use std::collections::HashMap;

use triton_vm::op_stack::OP_STACK_REG_COUNT;
use triton_vm::state::VMState;
use triton_vm::vm::Program;
use twenty_first::shared_math::b_field_element::BFieldElement;

mod arithmetic;
mod library;
mod mmr;
mod other_snippets;
mod recufier;
mod snippet_trait;

pub struct ExecutionResult {
    pub output: Vec<BFieldElement>,
    pub final_stack: Vec<BFieldElement>,
    pub final_ram: HashMap<BFieldElement, BFieldElement>,
    pub cycle_count: usize,
    pub hash_table_height: usize,
}

pub fn get_init_tvm_stack() -> Vec<BFieldElement> {
    vec![BFieldElement::zero(); OP_STACK_REG_COUNT]
}

/// Execute a Triton-VM program and return its output and execution trace length
pub fn execute(
    code: &str,
    stack: &mut Vec<BFieldElement>,
    expected_stack_diff: isize,
    std_in: Vec<BFieldElement>,
    secret_in: Vec<BFieldElement>,
) -> ExecutionResult {
    let init_stack_height = stack.len();

    // Prepend to program the initial stack values such that stack is in the expected
    // state when program logic is executed
    let mut executed_code: String = String::default();
    for element in stack.iter().skip(OP_STACK_REG_COUNT) {
        executed_code.push_str(&format!("push {}\n", element.value()));
    }

    // Add the program after the stack initialization has been performed
    executed_code.push_str(code);

    // Run the program, including the stack preparation logic
    let program = Program::from_code(&executed_code).expect("Could not load source code: {}");
    let (execution_trace, output, err) = program.run(std_in.clone(), secret_in.clone());
    if let Some(e) = err {
        panic!(
            "Running the program failed: {}\n\n\n Program:\n {}",
            e, code
        )
    }

    // Simulate the program, since this gives us hash table output
    let (simulation_trace, _simulation_output, err) = program.simulate(std_in, secret_in);
    if let Some(e) = err {
        panic!(
            "Simulating the program failed: {}\n\n\n Program: {}",
            e, code
        )
    }

    let start_state: VMState = execution_trace
        .first()
        .expect("VM state list must have initial element")
        .to_owned();

    let end_state: VMState = execution_trace
        .last()
        .expect("VM state list cannot be empty")
        .to_owned();
    assert!(!end_state.op_stack.is_too_shallow(), "Stack underflow");

    let jump_stack_start = start_state.jump_stack;
    let jump_stack_end = end_state.jump_stack;
    assert_eq!(
        jump_stack_start, jump_stack_end,
        "Jump stack must be unchanged after code execution"
    );

    *stack = end_state.op_stack.stack;

    let final_stack_height = stack.len() as isize;
    assert_eq!(
        expected_stack_diff,
        final_stack_height - init_stack_height as isize,
        "Code must grow/shrink stack with expected number of elements.\ninit height: {init_stack_height}\nend height: {final_stack_height}\nexpected difference: {expected_stack_diff}\n\nfinal stack: {stack:?}"
    );

    ExecutionResult {
        output,

        final_stack: stack.clone(),

        final_ram: end_state.ram,

        // Cycle count is cycles it took to run program excluding the cycles that were
        // spent on preparing the stack
        cycle_count: execution_trace.len() - (init_stack_height - OP_STACK_REG_COUNT) - 1,

        // Number of rows generated in the hash table after simulating program
        hash_table_height: simulation_trace.hash_matrix.len(),
    }
}
