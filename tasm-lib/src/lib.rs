// Recursion limit for macro_rules expansions, used for
// triton_asm!
#![recursion_limit = "4096"]

// This is needed for `#[derive(TasmObject)]` macro to work consistently across crates.
// Specifically:
// From inside the `tasm-lib` crate, we need to refer to `tasm-lib` by `crate`.
// However, from outside the `tasm-lib` crate, we need to refer to it by `tasm_lib`.
// The re-export below allows using identifier `tasm_lib` even from inside `tasm-lib`.
//
// See also:
// https://github.com/bkchr/proc-macro-crate/issues/2#issuecomment-572914520
extern crate self as tasm_lib;

use std::collections::HashMap;
use std::time::SystemTime;

use anyhow::bail;
use itertools::Itertools;
use num_traits::Zero;
use triton_vm::instruction::LabelledInstruction;
use triton_vm::op_stack::NUM_OP_STACK_REGISTERS;
use triton_vm::program::Program;
use triton_vm::triton_asm;
use triton_vm::vm::VMState;
use triton_vm::NonDeterminism;
use triton_vm::PublicInput;
use triton_vm::{Claim, StarkParameters};
use twenty_first::shared_math::b_field_element::BFieldElement;
use twenty_first::shared_math::bfield_codec::BFieldCodec;
use twenty_first::shared_math::tip5::Tip5State;
use twenty_first::shared_math::tip5::{self, Tip5};

use library::Library;
use memory::dyn_malloc;
use snippet::BasicSnippet;
use snippet::DeprecatedSnippet;

pub mod algorithm;
pub mod arithmetic;
pub mod closure;
pub mod compiled_program;
mod data_type;
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
pub mod ram_builder;
pub mod recufier;
pub mod rust_shadowing_helper_functions;
pub mod snippet;
pub mod snippet_bencher;
pub mod structure;
pub mod test_helpers;

// The hasher type must match whatever algebraic hasher the VM is using
pub type VmHasher = Tip5;
pub type VmHasherState = Tip5State;
pub type Digest = tip5::Digest;
pub const DIGEST_LENGTH: usize = tip5::DIGEST_LENGTH;

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
    pub words_allocated: u32,
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
        words_statically_allocated: u32,
    ) -> Self {
        ExecutionState {
            stack,
            std_in: vec![],
            nondeterminism: NonDeterminism::new(vec![]),
            memory,
            words_allocated: words_statically_allocated,
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
    pub final_sponge_state: Option<VmHasherState>,
}

pub fn empty_stack() -> Vec<BFieldElement> {
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
) -> anyhow::Result<ExecutionResult> {
    let initial_stack_height = stack.len() as isize;
    let public_input = PublicInput::new(std_in.clone());
    let program = Program::new(code);

    let mut vm_state = VMState::new(&program, public_input.clone(), nondeterminism.clone());
    vm_state.op_stack.stack = stack.to_owned();
    vm_state.run()?;
    let terminal_state = vm_state;

    let jump_stack = terminal_state.jump_stack;
    if !jump_stack.is_empty() {
        bail!("Jump stack must be unchanged after code execution but was {jump_stack:?}")
    }

    let (simulation_trace, output) =
        program.trace_execution(public_input, nondeterminism.clone())?;

    *memory = terminal_state.ram.clone();
    *stack = terminal_state.op_stack.stack;

    let final_stack_height = stack.len() as isize;
    if expected_stack_diff != final_stack_height - initial_stack_height {
        bail!(
            "Code must grow/shrink stack with expected number of elements.\n
            init height: {initial_stack_height}\n
            end height:  {final_stack_height}\n
            expected difference: {expected_stack_diff}\n\n
            final stack: {}",
            stack.iter().join(",")
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
        final_ram: terminal_state.ram,
        cycle_count: terminal_state.cycle_count as usize,
        hash_table_height: simulation_trace.hash_table_length(),
        u32_table_height: simulation_trace.u32_table_length(),
    })
}

/// Execute a Triton-VM program and test correct behavior indicators.
/// Modify stack and memory. Panic if anything goes wrong.
#[allow(clippy::too_many_arguments)]
pub fn execute_test(
    code: &[LabelledInstruction],
    stack: &mut Vec<BFieldElement>,
    expected_stack_diff: isize,
    std_in: Vec<BFieldElement>,
    nondeterminism: NonDeterminism<BFieldElement>,
    maybe_sponge_state: Option<VmHasherState>,
) -> VmOutputState {
    let initial_stack_height = stack.len();
    let public_input = PublicInput::new(std_in.clone());
    let program = Program::new(code);

    let mut vm_state = VMState::new(&program, public_input.clone(), nondeterminism.clone());
    vm_state.op_stack.stack = stack.to_owned();
    vm_state.sponge_state = maybe_sponge_state.map(|state| state.state);
    vm_state.run().unwrap();
    let terminal_state = vm_state;

    if !terminal_state.jump_stack.is_empty() {
        panic!("Jump stack must be unchanged after code execution");
    }

    let final_stack_height = terminal_state.op_stack.stack.len() as isize;
    assert_eq!(
        expected_stack_diff,
        final_stack_height - initial_stack_height as isize,
        "Code must grow/shrink stack with expected number of elements.\n
        init height: {initial_stack_height}\n
        end height:  {final_stack_height}\n
        expected difference: {expected_stack_diff}\n\n
        initial stack: {}\n
        final stack:   {}",
        stack.iter().skip(16).join(","),
        terminal_state.op_stack.stack.iter().skip(16).join(","),
    );
    *stack = terminal_state.op_stack.stack;

    // If this environment variable is set, all programs, including the code to prepare the state,
    // will be proven and then verified.
    // Notice that this is only done after the successful execution of the program above, so all
    // produced proofs here should be valid.
    // If you run this, make sure `opt-level` is set to 3.
    if std::env::var("DYING_TO_PROVE").is_ok() {
        prove_and_verify(
            &program,
            &std_in,
            &nondeterminism,
            &terminal_state.public_output,
        );
    }

    VmOutputState {
        output: terminal_state.public_output,
        final_stack: stack.to_owned(),
        final_ram: terminal_state.ram,
        final_sponge_state: terminal_state
            .sponge_state
            .map(|state| VmHasherState { state }),
    }
}

/// Prepare state and run Triton VM
pub fn execute_with_terminal_state(
    program: &Program,
    std_in: &[BFieldElement],
    stack: &[BFieldElement],
    nondeterminism: &NonDeterminism<BFieldElement>,
    maybe_sponge_state: Option<VmHasherState>,
) -> anyhow::Result<VMState> {
    let public_input = PublicInput::new(std_in.into());
    let mut vm_state = VMState::new(program, public_input, nondeterminism.to_owned());
    vm_state.op_stack.stack = stack.to_owned();
    vm_state.sponge_state = maybe_sponge_state.map(|state| state.state);

    match vm_state.run() {
        Ok(()) => {
            println!("Triton VM execution successful.");
            anyhow::Ok(vm_state)
        }
        Err(err) => {
            if let Some(sponge_state) = vm_state.sponge_state {
                println!("tasm final sponge state:");
                println!("{}", sponge_state.iter().join(", "));
            }
            println!("Triton VM execution failed. Final state:\n{vm_state}");
            bail!("VM execution failed with error: {err}")
        }
    }
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
    // println!("Execution succeeded. Now proving {code_header}");
    let tick = SystemTime::now();
    let proof = triton_vm::prove(
        StarkParameters::default(),
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
        triton_vm::verify(StarkParameters::default(), &claim, &proof),
        "Generated proof must verify for program:\n {}",
        program,
    );
}
