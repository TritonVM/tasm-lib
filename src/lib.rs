use num_traits::Zero;
use std::collections::HashMap;
use triton_opcodes::program::Program;
use triton_vm::vm;

use triton_vm::op_stack::OP_STACK_REG_COUNT;
use triton_vm::state::VMState;
use twenty_first::shared_math::b_field_element::BFieldElement;
use twenty_first::util_types::algebraic_hasher::Hashable;

mod arithmetic;
mod hashing;
mod library;
mod list;
mod mmr;
mod other_snippets;
mod recufier;
mod rust_shadowing_helper_functions;
mod snippet;
mod snippet_bencher;
mod test_helpers;

#[derive(Clone, Debug)]
pub struct ExecutionState {
    pub stack: Vec<BFieldElement>,
    pub std_in: Vec<BFieldElement>,
    pub secret_in: Vec<BFieldElement>,
    pub memory: HashMap<BFieldElement, BFieldElement>,
    pub words_allocated: usize,
}

impl ExecutionState {
    pub fn with_stack(stack: Vec<BFieldElement>) -> Self {
        ExecutionState {
            stack,
            std_in: vec![],
            secret_in: vec![],
            memory: HashMap::default(),
            words_allocated: 0,
        }
    }
}

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

pub fn push_hashable<T: Hashable>(stack: &mut Vec<BFieldElement>, value: &T) {
    stack.append(&mut value.to_sequence().into_iter().rev().collect());
}

/// Execute a Triton-VM program and return its output and execution trace length
pub fn execute(
    code: &str,
    stack: &mut Vec<BFieldElement>,
    expected_stack_diff: isize,
    std_in: Vec<BFieldElement>,
    secret_in: Vec<BFieldElement>,
    memory: &mut HashMap<BFieldElement, BFieldElement>,
) -> ExecutionResult {
    let init_stack_height = stack.len();

    // Prepend to program the initial stack values such that stack is in the expected
    // state when program logic is executed
    let mut executed_code: String = String::default();
    for element in stack.iter().skip(OP_STACK_REG_COUNT) {
        executed_code.push_str(&format!("push {}\n", element.value()));
    }

    // Add all the initial memory to the VM
    for (address, value) in memory.iter() {
        // Prepare stack for writing
        executed_code.push_str(&format!("push {}\n", address));
        executed_code.push_str(&format!("push {}\n", value));

        // Write value to memory
        executed_code.push_str("write_mem\n");

        // Clean stack after writing to memory
        executed_code.push_str("pop\n");
        executed_code.push_str("pop\n");
    }

    // Add the program after the stack initialization has been performed
    // Find the length of code used for setup. This length does not count towards execution length of snippet
    // so it must be subtracted at the end.
    let init_code_length = vm::run(
        &Program::from_code(&executed_code).expect("Could not load source code: {}"),
        vec![],
        vec![],
    )
    .0
    .len()
        - 1;

    // Construct the whole program (inclusive setup) to be run
    executed_code.push_str(code);

    // Run the program, including the stack preparation and memory preparation logic
    let program = Program::from_code(&executed_code).expect("Could not load source code: {}");
    let (execution_trace, output, err) = vm::run(&program, std_in.clone(), secret_in.clone());
    if let Some(e) = err {
        panic!(
            "Running the program failed: {}\n\n\n Program:\n {}",
            e, code
        )
    }

    // Simulate the program, since this gives us hash table output
    let (simulation_trace, _simulation_output, err) = vm::simulate(&program, std_in, secret_in);
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

    *memory = end_state.ram.clone();

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
        cycle_count: simulation_trace.processor_trace.nrows() - init_code_length - 1,

        // Number of rows generated in the hash table after simulating program
        hash_table_height: simulation_trace.hash_trace.nrows(),
    }
}
