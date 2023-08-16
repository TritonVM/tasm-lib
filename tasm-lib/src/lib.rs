// Recursion limit for macro_rules expansions, used for
// triton_asm!
#![recursion_limit = "1024"]

use anyhow::anyhow;
use anyhow::bail;
use itertools::Itertools;
use library::Library;
use memory::dyn_malloc;
use num_traits::Zero;
use snippet::BasicSnippet;
use snippet::DeprecatedSnippet;
use std::collections::HashMap;
use std::time::SystemTime;
use triton_vm::instruction::LabelledInstruction;
use triton_vm::program::Program;
use triton_vm::triton_asm;
use triton_vm::triton_instr;
use triton_vm::NonDeterminism;
use triton_vm::PublicInput;
use triton_vm::{Claim, StarkParameters};
use twenty_first::shared_math::bfield_codec::BFieldCodec;
use twenty_first::shared_math::tip5::{self, Tip5};

use triton_vm::op_stack::NUM_OP_STACK_REGISTERS;
use triton_vm::vm::VMState;
use twenty_first::shared_math::b_field_element::BFieldElement;

pub mod algorithm;
pub mod arithmetic;
pub mod closure;
pub mod compiled_program;
pub mod exported_snippets;
pub mod function;
pub mod hashing;
pub mod io;
pub mod library;
pub mod linker;
pub mod list;
pub mod memory;
pub mod mmr;
pub mod neptune;
pub mod other_snippets;
pub mod procedure;
pub mod pseudo;
pub mod recufier;
pub mod rust_shadowing_helper_functions;
pub mod snippet;
pub mod snippet_bencher;
pub mod structure;
pub mod test_helpers;

// The hasher type must match whatever algebraic hasher the VM is using
pub type VmHasher = Tip5;
pub type Digest = tip5::Digest;
pub const DIGEST_LENGTH: usize = tip5::DIGEST_LENGTH;

// This is needed for `#[derive(TasmObject)]` macro to work consistently across crates.
// Specifically:
// From inside the `tasm-lib` crate, we need to refer to `tasm-lib` by `crate`.
// However, from outside the `tasm-lib` crate, we need to refer to it by `tasm_lib`.
// The re-export below allows using identifier `tasm_lib` even from inside `tasm-lib`.
//
// See also:
// https://github.com/bkchr/proc-macro-crate/issues/2#issuecomment-572914520
extern crate self as tasm_lib;

#[derive(Clone, Debug)]
pub struct ExecutionState {
    pub stack: Vec<BFieldElement>,
    pub std_in: Vec<BFieldElement>,
    pub nondeterminism: NonDeterminism<BFieldElement>,
    pub memory: HashMap<BFieldElement, BFieldElement>,

    // Ensures that you're not overwriting statically allocated memory
    // when using the dynamic allocator.
    // When you're writing a program you need to know how many words
    // are statically allocated and then you need to feed that value
    // to the dynamic allocator otherwise you are *** [redacted].
    pub words_allocated: usize,
}

impl ExecutionState {
    pub fn with_stack(stack: Vec<BFieldElement>) -> Self {
        ExecutionState {
            stack,
            std_in: vec![],
            nondeterminism: NonDeterminism::new(vec![]),
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
            nondeterminism: NonDeterminism::new(vec![]),
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

#[derive(Clone, Debug)]
pub struct VmOutputState {
    pub output: Vec<BFieldElement>,
    pub final_stack: Vec<BFieldElement>,
    pub final_ram: HashMap<BFieldElement, BFieldElement>,
}

pub fn get_init_tvm_stack() -> Vec<BFieldElement> {
    vec![BFieldElement::zero(); NUM_OP_STACK_REGISTERS]
}

pub fn push_encodable<T: BFieldCodec>(stack: &mut Vec<BFieldElement>, value: &T) {
    stack.append(&mut value.encode().into_iter().rev().collect());
}

#[allow(dead_code)] // used in tests
pub(crate) fn execute_with_execution_state_deprecated<T: DeprecatedSnippet>(
    snippet: T,
    mut init_state: ExecutionState,
    expected_stack_diff: isize,
) -> anyhow::Result<ExecutionResult> {
    let mut library = Library::new();
    let entrypoint = snippet.entrypoint();
    let insert_me = snippet.code(&mut library);
    let insert_library = library.all_imports();
    let code = triton_asm!(
        call {entrypoint}
        halt
        {&insert_me}
        {&insert_library}
    );
    execute_bench_deprecated(
        &code,
        &mut init_state.stack,
        expected_stack_diff,
        init_state.std_in,
        init_state.nondeterminism,
        &mut init_state.memory,
        None,
    )
}

/// Execute a Triton-VM program and return its output and execution trace length
pub fn execute_bench_deprecated(
    code: &[LabelledInstruction],
    stack: &mut Vec<BFieldElement>,
    expected_stack_diff: isize,
    std_in: Vec<BFieldElement>,
    nondeterminism: NonDeterminism<BFieldElement>,
    memory: &mut HashMap<BFieldElement, BFieldElement>,
    initilialize_dynamic_allocator_to: Option<usize>,
) -> anyhow::Result<ExecutionResult> {
    let init_stack_height = stack.len();

    // Prepend to program the initial stack values such that stack is in the expected
    // state when program logic is executed
    let prep: Vec<LabelledInstruction> =
        state_preparation_code(stack, memory, initilialize_dynamic_allocator_to);

    // Add the program after the stack initialization has been performed
    // Find the length of code used for setup. This length does not count towards execution length
    // of snippet so it must be subtracted at the end.
    let program = Program::new(&prep);
    let (all_states, _) = program.debug(
        PublicInput::new(vec![]),
        NonDeterminism::new(vec![]),
        None,
        None,
    );
    let initialization_clock_cycle_count = all_states.len() - 1;

    // Construct the whole program (inclusive setup) to be run
    let mut executed_code = prep;
    executed_code.extend_from_slice(code);
    let program = Program::new(&executed_code);

    // Run the program, including the stack preparation and memory preparation logic
    let (execution_trace, err) = program.debug(
        PublicInput::new(std_in.clone()),
        nondeterminism.clone(),
        None,
        None,
    );
    if let Some(e) = err {
        bail!(
            "`debug` failed with error: {e}\nLast state before crash:\n{}",
            execution_trace.last().unwrap()
        )
    }

    // Simulate the program, since this gives us hash table output
    let (simulation_trace, output) = program
        .trace_execution(PublicInput::new(std_in.clone()), nondeterminism.clone())
        .map_err(|error| anyhow!("`simulate` failed with error: {error}"))?;

    let start_state: VMState = execution_trace
        .first()
        .expect("VM state list must have initial element")
        .to_owned();

    let end_state: VMState = execution_trace
        .last()
        .expect("VM state list cannot be empty")
        .to_owned();

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
    // If you run this, make sure `opt-level` is set to 3.
    if std::env::var("DYING_TO_PROVE").is_ok() {
        prove_and_verify(&program, &std_in, &nondeterminism, &output);
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
        u32_table_height: simulation_trace.u32_table_length(),
    })
}

/// Execute a Triton-VM program for test; modify stack and memory
pub fn execute_test(
    code: &[LabelledInstruction],
    stack: &mut Vec<BFieldElement>,
    expected_stack_diff: isize,
    std_in: Vec<BFieldElement>,
    nondeterminism: &NonDeterminism<BFieldElement>,
    memory: &mut HashMap<BFieldElement, BFieldElement>,
    initilialize_dynamic_allocator_to: Option<usize>,
) -> anyhow::Result<VmOutputState> {
    let init_stack_height = stack.len();

    // Prepend to program the initial stack values such that stack is in the expected
    // state when program logic is executed
    let prep: Vec<LabelledInstruction> =
        state_preparation_code(stack, memory, initilialize_dynamic_allocator_to);
    let mut executed_code = prep;

    // Construct the whole program (inclusive setup) to be run
    executed_code.append(&mut code.to_vec());

    // Run the program, including the stack preparation and memory preparation logic
    let program = Program::new(&executed_code);
    let final_state = program
        .debug_terminal_state(
            PublicInput::new(std_in.clone()),
            nondeterminism.clone(),
            None,
            None,
        )
        .map_err(|(err, fs)| {
            anyhow!("VM execution failed with error: {err}.\nLast state before crash:\n{fs}")
        })?;

    *memory = final_state.ram.clone();

    if !final_state.jump_stack.is_empty() {
        bail!("Jump stack must be unchanged after code execution")
    }

    *stack = final_state.op_stack.stack;

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
    // If you run this, make sure `opt-level` is set to 3.
    if std::env::var("DYING_TO_PROVE").is_ok() {
        prove_and_verify(
            &program,
            &std_in,
            nondeterminism,
            &final_state.public_output,
        );
    }

    Ok(VmOutputState {
        output: final_state.public_output,
        final_stack: stack.to_owned(),
        final_ram: final_state.ram,
    })
}

/// Produce the code to set the stack and memory into a certain state
fn state_preparation_code(
    stack: &[BFieldElement],
    memory: &HashMap<BFieldElement, BFieldElement>,
    initilialize_dynamic_allocator_to: Option<usize>,
) -> Vec<LabelledInstruction> {
    let mut state_preparation_code = Vec::default();
    for element in stack.iter().skip(NUM_OP_STACK_REGISTERS) {
        state_preparation_code.push(triton_instr!(push element.value()));
    }

    // Add all the initial memory to the VM
    for (address, value) in memory.iter() {
        // Prepare stack for writing
        state_preparation_code.push(triton_instr!(push address.value()));
        state_preparation_code.push(triton_instr!(push value.value()));

        // Write value to memory
        state_preparation_code.push(triton_instr!(write_mem));

        // Clean stack after writing to memory
        state_preparation_code.push(triton_instr!(pop));
    }

    // Ensure that the dynamic allocator is initialized such that it does not overwrite
    // any statically allocated memory, if the caller requests this.
    if let Some(dyn_malloc_initial_value) = initilialize_dynamic_allocator_to {
        state_preparation_code.append(&mut dyn_malloc::DynMalloc::get_initialization_code(
            dyn_malloc_initial_value.try_into().unwrap(),
        ));
    }

    state_preparation_code
}

// If you run this, make sure `opt-level` is set to 3.
fn prove_and_verify(
    program: &Program,
    std_in: &[BFieldElement],
    nondeterminism: &NonDeterminism<BFieldElement>,
    output: &[BFieldElement],
) {
    let claim = Claim {
        program_digest: program.hash::<VmHasher>(),
        input: std_in.to_owned(),
        output: output.to_owned(),
    };

    let (simulation_trace, _) = program
        .trace_execution(PublicInput::new(std_in.to_owned()), nondeterminism.clone())
        .unwrap();

    // let code_header = &code[0..std::cmp::min(code.len(), 100)];
    // println!("Execution suceeded. Now proving {code_header}");
    let tick = SystemTime::now();
    let proof = triton_vm::prove(
        &StarkParameters::default(),
        &claim,
        program,
        nondeterminism.clone(),
    )
    .unwrap();
    println!(
        "Done proving. Elapsed time: {:?}",
        tick.elapsed().expect("Don't mess with time")
    );
    println!(
        "\nProof was generated from:\ntable heights:\nprocessor table: {}\nhash table: {}\nu32 table: {}",
        simulation_trace.processor_trace.rows().into_iter().count(),
        simulation_trace.hash_trace.rows().into_iter().count(),
        simulation_trace.u32_entries.len(),
    );

    assert!(
        triton_vm::verify(&StarkParameters::default(), &claim, &proof),
        "Generated proof must verify for program:\n {}",
        program,
    );
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::dyn_malloc::DYN_MALLOC_ADDRESS;

    #[test]
    fn initialize_dyn_malloc() {
        let mut memory = HashMap::default();
        let initial_dyn_malloc_value = 14;
        execute_bench_deprecated(
            &triton_asm!(halt),
            &mut get_init_tvm_stack(),
            0,
            vec![],
            NonDeterminism::new(vec![]),
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
        execute_bench_deprecated(
            &triton_asm!(halt),
            &mut get_init_tvm_stack(),
            0,
            vec![],
            NonDeterminism::new(vec![]),
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
