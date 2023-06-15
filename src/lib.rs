use anyhow::bail;
use itertools::Itertools;
use memory::dyn_malloc;
use num_traits::Zero;
use snippet::Snippet;
use snippet_state::SnippetState;
use std::collections::HashMap;
use std::time::SystemTime;
use triton_opcodes::program::Program;
use triton_vm::table::master_table::MasterBaseTable;
use triton_vm::{vm, Claim, StarkParameters};
use twenty_first::shared_math::bfield_codec::BFieldCodec;
use twenty_first::shared_math::tip5::{self, Tip5};
use twenty_first::util_types::algebraic_hasher::AlgebraicHasher;

use triton_vm::op_stack::OP_STACK_REG_COUNT;
use triton_vm::vm::VMState;
use twenty_first::shared_math::b_field_element::BFieldElement;

pub mod arithmetic;
pub mod exported_snippets;
pub mod hashing;
pub mod io;
pub mod list;
pub mod memory;
pub mod mmr;
pub mod neptune;
pub mod other_snippets;
pub mod pseudo;
pub mod recufier;
pub mod rust_shadowing_helper_functions;
pub mod snippet;
pub mod snippet_bencher;
pub mod snippet_state;
mod test_helpers;

// The hasher type must match whatever algebraic hasher the VM is using
pub type VmHasher = Tip5;
pub type Digest = tip5::Digest;
pub const DIGEST_LENGTH: usize = tip5::DIGEST_LENGTH;

#[derive(Clone, Debug)]
pub struct ExecutionState {
    pub stack: Vec<BFieldElement>,
    pub std_in: Vec<BFieldElement>,
    pub secret_in: Vec<BFieldElement>,
    pub memory: HashMap<BFieldElement, BFieldElement>,

    // Ensures that you're not overwriting statically alocated memory
    // when using the dynamic allocator.
    // When you're writing a propgram you need to know how many words
    // are statically allocated and then you need to feed that value
    // to the dynamic allocator otherwise you are *** [redacted].
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

    pub fn with_stack_and_memory(
        stack: Vec<BFieldElement>,
        memory: HashMap<BFieldElement, BFieldElement>,
        words_allocated: usize,
    ) -> Self {
        ExecutionState {
            stack,
            std_in: vec![],
            secret_in: vec![],
            memory,
            words_allocated,
        }
    }
}

#[derive(Clone, Debug)]
pub struct ExecutionResult {
    pub output: Vec<BFieldElement>,
    pub final_stack: Vec<BFieldElement>,
    pub final_ram: HashMap<BFieldElement, BFieldElement>,
    pub cycle_count: usize,
    pub hash_table_height: usize,
    pub u32_table_height: usize,
}

pub fn get_init_tvm_stack() -> Vec<BFieldElement> {
    vec![BFieldElement::zero(); OP_STACK_REG_COUNT]
}

pub fn push_encodable<T: BFieldCodec>(stack: &mut Vec<BFieldElement>, value: &T) {
    stack.append(&mut value.encode().into_iter().rev().collect());
}

pub fn execute_with_execution_state(
    mut init_state: ExecutionState,
    snippet: Box<dyn Snippet>,
    expected_stack_diff: isize,
) -> anyhow::Result<ExecutionResult> {
    let mut library = SnippetState::default();
    let entrypoint = snippet.entrypoint();
    let mut code = format!("call {entrypoint}\n");
    code.push_str("halt\n");
    code.push_str(&snippet.function_code(&mut library));
    code.push_str(&library.all_imports());
    execute(
        &code,
        &mut init_state.stack,
        expected_stack_diff,
        init_state.std_in,
        init_state.secret_in,
        &mut init_state.memory,
        None,
    )
}

/// Execute a Triton-VM program and return its output and execution trace length
pub fn execute(
    code: &str,
    stack: &mut Vec<BFieldElement>,
    expected_stack_diff: isize,
    std_in: Vec<BFieldElement>,
    secret_in: Vec<BFieldElement>,
    memory: &mut HashMap<BFieldElement, BFieldElement>,
    initilialize_dynamic_allocator_to: Option<usize>,
) -> anyhow::Result<ExecutionResult> {
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
        executed_code.push_str(&format!("push {address}\n"));
        executed_code.push_str(&format!("push {value}\n"));

        // Write value to memory
        executed_code.push_str("write_mem\n");

        // Clean stack after writing to memory
        executed_code.push_str("pop\n");
    }

    // Ensure that the dynamic allocator is initialized such that it does not overwrite
    // any statically allocated memory, if the caller requests this.
    if let Some(dyn_malloc_initial_value) = initilialize_dynamic_allocator_to {
        executed_code.push_str(&dyn_malloc::DynMalloc::get_initialization_code(
            dyn_malloc_initial_value.try_into().unwrap(),
        ));
    }

    // Add the program after the stack initialization has been performed
    // Find the length of code used for setup. This length does not count towards execution length of snippet
    // so it must be subtracted at the end.
    let initialization_clock_cycle_count = vm::debug(
        &Program::from_code(&executed_code).expect("Could not load source code: {}"),
        vec![],
        vec![],
        None,
        None,
    )
    .0
    .len()
        - 1;

    // Construct the whole program (inclusive setup) to be run
    executed_code.push_str(code);

    // Run the program, including the stack preparation and memory preparation logic
    let program = Program::from_code(&executed_code).expect("Could not load source code: {}");
    let (execution_trace, output, err) =
        vm::debug(&program, std_in.clone(), secret_in.clone(), None, None);
    if let Some(e) = err {
        bail!(
            "`debug` failed with error: {e}\nLast state before crash:\n{}",
            execution_trace.last().unwrap()
        )
    }

    // Simulate the program, since this gives us hash table output
    let (simulation_trace, _simulation_output) =
        match vm::simulate(&program, std_in.clone(), secret_in.clone()) {
            Ok(res) => res,
            Err(e) => bail!("`simulate` failed with error: {e}"),
        };

    let start_state: VMState = execution_trace
        .first()
        .expect("VM state list must have initial element")
        .to_owned();

    let end_state: VMState = execution_trace
        .last()
        .expect("VM state list cannot be empty")
        .to_owned();
    if end_state.op_stack.is_too_shallow() {
        bail!("Stack underflow")
    }

    *memory = end_state.ram.clone();

    let jump_stack_start = start_state.jump_stack;
    let jump_stack_end = end_state.jump_stack;
    if jump_stack_start != jump_stack_end {
        bail!("Jump stack must be unchanged after code execution")
    }

    *stack = end_state.op_stack.stack;

    let final_stack_height = stack.len() as isize;
    if expected_stack_diff != final_stack_height - init_stack_height as isize {
        bail!(
            "Code must grow/shrink stack with expected number of elements.\n
        init height: {init_stack_height}\nend height: {final_stack_height}\n
        expected difference: {expected_stack_diff}\n\n
        final stack: {}",
            stack.iter().map(|x| x.to_string()).join(",")
        )
    }

    // If this environment variable is set, all programs, including the code to prepare the state,
    // will be proven and then verified.
    // Notice that this is only done after the successful execution of the program above, so all
    // produced proofs here should be valid.
    // If you run this, make sure to set this in your Cargo.toml:
    // ```
    // [profile.test]
    // opt-level = 3
    // ```
    if std::env::var("MY_COMPUTER_HAS_NOTHING_BETTER_TO_DO").is_ok() {
        let claim = Claim {
            program_digest: VmHasher::hash_varlen(&program.encode()),
            input: std_in,
            output: output.clone(),
        };

        let code_header = &code[0..std::cmp::min(code.len(), 100)];
        println!("Execution suceeded. Now proving {code_header}");
        let tick = SystemTime::now();
        let proof =
            triton_vm::prove(&StarkParameters::default(), &claim, &program, &secret_in).unwrap();
        println!(
            "Done proving. Elapsed time: {:?}",
            tick.elapsed().expect("Don't mess with time")
        );

        assert!(
            triton_vm::verify(&StarkParameters::default(), &proof),
            "Generated proof must verify for program:\n {}\n\n Whole program was:\n\n{}",
            code_header,
            executed_code
        );
    }

    Ok(ExecutionResult {
        output,

        final_stack: stack.clone(),

        final_ram: end_state.ram,

        // Cycle count is cycles it took to run program excluding the cycles that were
        // spent on preparing the stack
        cycle_count: simulation_trace.processor_trace.nrows()
            - initialization_clock_cycle_count
            - 1,

        // Number of rows generated in the hash table after simulating program
        hash_table_height: simulation_trace.hash_trace.nrows(),

        // Number of rows generated in the u32 table after simulating program
        u32_table_height: MasterBaseTable::u32_table_length(&simulation_trace),
    })
}

#[cfg(test)]
mod lib_tests {
    use super::*;
    use crate::dyn_malloc::DYN_MALLOC_ADDRESS;

    #[test]
    fn initialize_dyn_malloc() {
        let mut memory = HashMap::default();
        let initial_dyn_malloc_value = 14;
        execute(
            "halt",
            &mut get_init_tvm_stack(),
            0,
            vec![],
            vec![],
            &mut memory,
            Some(initial_dyn_malloc_value),
        )
        .unwrap();
        assert_eq!(
            initial_dyn_malloc_value,
            memory[&BFieldElement::new(DYN_MALLOC_ADDRESS as u64)].value() as usize
        );
    }

    #[test]
    fn do_not_initialize_dyn_malloc() {
        // Ensure that dyn malloc is not initialized if no such initialization is requested
        let mut memory = HashMap::default();
        execute(
            "halt",
            &mut get_init_tvm_stack(),
            0,
            vec![],
            vec![],
            &mut memory,
            None,
        )
        .unwrap();
        assert!(memory
            .get(&BFieldElement::new(DYN_MALLOC_ADDRESS as u64))
            .unwrap_or(&BFieldElement::zero())
            .is_zero());
    }
}
