use num_traits::Zero;
use triton_vm::{op_stack::OP_STACK_REG_COUNT, state::VMState, vm::Program};
use twenty_first::shared_math::b_field_element::BFieldElement;

mod other_snippets;
mod recufier;

pub struct ExecutionResult {
    pub output: Vec<BFieldElement>,
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
    expected_final_stack_height: usize,
    std_in: Vec<BFieldElement>,
    secret_in: Vec<BFieldElement>,
) -> ExecutionResult {
    let init_stack_length = stack.len();

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
        panic!("Running the program failed: {}\n\n\n Program: {}", e, code)
    }

    // Simulate the program, since this gives us hash table output
    let (simulation_trace, _simulation_output, err) = program.simulate(std_in, secret_in);
    if let Some(e) = err {
        panic!(
            "Simulating the program failed: {}\n\n\n Program: {}",
            e, code
        )
    }

    let end_state: VMState = execution_trace
        .last()
        .expect("VM state list cannot be empty")
        .to_owned();
    assert!(!end_state.op_stack.is_too_shallow(), "Stack underflow");
    *stack = end_state.op_stack.stack;

    assert_eq!(
        expected_final_stack_height,
        stack.len(),
        "Stack must match expected height"
    );

    ExecutionResult {
        output,

        // Cycle count is cycles it took to run program excluding the cycles that were
        // spent on preparing the stack
        cycle_count: execution_trace.len() - (init_stack_length - OP_STACK_REG_COUNT) - 1,

        // Number of rows generated in the hash table after simulating program
        hash_table_height: simulation_trace.hash_matrix.len(),
    }
}
