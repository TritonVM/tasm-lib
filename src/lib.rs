use triton_vm::{op_stack::OP_STACK_REG_COUNT, state::VMState, stdio::VecStream, vm::Program};
use twenty_first::shared_math::b_field_element::BFieldElement;

mod other_snippets;

pub struct ExecutionResult {
    pub output: Vec<BFieldElement>,
    pub cycle_count: usize,
}

/// Execute a Triton-VM program and return its output and execution trace length
pub fn execute(
    code: &str,
    stack: &mut Vec<BFieldElement>,
    final_stack_height: usize,
    std_in: Vec<BFieldElement>,
    secret_in: Vec<BFieldElement>,
) -> ExecutionResult {
    let init_stack_length = stack.len();

    // Prepend to program the initial stack values such that stack is in the expected
    // state when program logic is executed
    let mut executed_code: String = String::default();
    for element in stack.iter() {
        executed_code.push_str(&format!("push {}\n", element.value()));
    }

    // Add the program after the stack initialization has been performed
    executed_code.push_str(code);

    // Run the program, including the stack preparation logic
    let program = Program::from_code(&executed_code).expect("Could not load source code: {}");
    let mut std_input = VecStream::new(&std_in);
    let mut secret_in = VecStream::new(&secret_in);
    let mut output = VecStream::new(&[]);
    let (execution_trace, err) = program.run(&mut std_input, &mut secret_in, &mut output);
    if let Some(e) = err {
        panic!("Running the program failed: {}\n\n\n Program: {}", e, code)
    }

    let end_state: VMState = execution_trace
        .last()
        .expect("VM state list cannot be empty")
        .to_owned();
    assert!(!end_state.op_stack.is_too_shallow(), "Stack underflow");
    *stack = end_state.op_stack.stack;

    for _i in 0..OP_STACK_REG_COUNT {
        stack.remove(0);
    }

    assert_eq!(
        final_stack_height,
        stack.len(),
        "Stack must match expected height"
    );

    ExecutionResult {
        output: output.to_bword_vec(),

        // Cycle count is cycles it took to run program excluding the cycles that were
        // spent on preparing the stack
        cycle_count: execution_trace.len() - init_stack_length - 1,
    }
}
