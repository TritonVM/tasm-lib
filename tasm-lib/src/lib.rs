// Recursion limit for macro_rules expansions, used for
// triton_asm!
#![recursion_limit = "2048"]

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
use std::io::Write;
use std::time::SystemTime;

use anyhow::bail;
use itertools::Itertools;
use num_traits::Zero;
use snippet_bencher::BenchmarkResult;
use triton_vm::op_stack::NUM_OP_STACK_REGISTERS;
use triton_vm::prelude::*;

use library::Library;
use memory::dyn_malloc;

pub mod arithmetic;
pub mod array;
pub mod data_type;
pub mod exported_snippets;
pub mod hashing;
pub mod io;
pub mod library;
pub mod linker;
pub mod list;
pub mod memory;
pub mod mmr;
pub mod neptune;
pub mod other_snippets;
pub mod prelude;
pub mod recufier;
pub mod rust_shadowing_helper_functions;
pub mod snippet_bencher;
pub mod structure;
pub mod test_helpers;
pub mod traits;

// re-exports for types exposed in our public API.
pub use triton_vm;
use triton_vm::profiler::TritonProfiler;
use triton_vm::table::master_table::TableId;
pub use triton_vm::twenty_first;

// The hasher type must match whatever algebraic hasher the VM is using
pub type VmHasher = Tip5;
pub type Digest = tip5::Digest;
pub const DIGEST_LENGTH: usize = tip5::DIGEST_LENGTH;

#[derive(Clone, Debug)]
pub struct ExecutionState {
    pub stack: Vec<BFieldElement>,
    pub std_in: Vec<BFieldElement>,
    pub nondeterminism: NonDeterminism,
}

impl ExecutionState {
    pub fn with_stack(stack: Vec<BFieldElement>) -> Self {
        ExecutionState {
            stack,
            std_in: vec![],
            nondeterminism: NonDeterminism::default(),
        }
    }

    pub fn with_stack_and_memory(
        stack: Vec<BFieldElement>,
        memory: HashMap<BFieldElement, BFieldElement>,
    ) -> Self {
        ExecutionState {
            stack,
            std_in: vec![],
            nondeterminism: NonDeterminism::default().with_ram(memory),
        }
    }
}

#[derive(Clone, Debug)]
pub struct RustShadowOutputState {
    pub public_output: Vec<BFieldElement>,
    pub stack: Vec<BFieldElement>,
    pub ram: HashMap<BFieldElement, BFieldElement>,
    pub sponge: Option<VmHasher>,
}

pub fn empty_stack() -> Vec<BFieldElement> {
    vec![BFieldElement::zero(); NUM_OP_STACK_REGISTERS]
}

pub fn push_encodable<T: BFieldCodec>(stack: &mut Vec<BFieldElement>, value: &T) {
    stack.append(&mut value.encode().into_iter().rev().collect());
}

/// Execute a Triton-VM program and return its output and execution trace length
pub fn execute_bench_deprecated(
    code: &[LabelledInstruction],
    stack: &mut Vec<BFieldElement>,
    expected_stack_diff: isize,
    std_in: Vec<BFieldElement>,
    nondeterminism: NonDeterminism,
) -> anyhow::Result<BenchmarkResult> {
    let init_stack = stack.to_owned();
    let initial_stack_height = init_stack.len() as isize;
    let public_input = PublicInput::new(std_in.clone());
    let program = Program::new(code);

    let mut vm_state = VMState::new(&program, public_input, nondeterminism.clone());
    vm_state.op_stack.stack = init_stack.clone();

    let (simulation_trace, terminal_state) = program.trace_execution_of_state(vm_state)?;

    let jump_stack = &terminal_state.jump_stack;
    if !jump_stack.is_empty() {
        bail!("Jump stack must be unchanged after code execution but was {jump_stack:?}")
    }

    let final_stack_height = terminal_state.op_stack.stack.len() as isize;
    if expected_stack_diff != final_stack_height - initial_stack_height {
        bail!(
            "Code must grow/shrink stack with expected number of elements.\n
            init height: {initial_stack_height}\n
            end height:  {final_stack_height}\n
            expected difference: {expected_stack_diff}\n\n
            final stack: {}",
            terminal_state.op_stack.stack.iter().join(",")
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
            &nondeterminism,
            &terminal_state.public_output,
            Some(init_stack),
        );
    }

    *stack = terminal_state.op_stack.stack.clone();
    Ok(BenchmarkResult {
        clock_cycle_count: simulation_trace.height_of_table(TableId::Processor),
        hash_table_height: simulation_trace.height_of_table(TableId::Hash),
        u32_table_height: simulation_trace.height_of_table(TableId::U32),
        op_stack_table_height: simulation_trace.height_of_table(TableId::OpStack),
        ram_table_height: simulation_trace.height_of_table(TableId::Ram),
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
    nondeterminism: NonDeterminism,
    maybe_sponge: Option<VmHasher>,
) -> VMState {
    let init_stack = stack.to_owned();
    let public_input = PublicInput::new(std_in.clone());
    let program = Program::new(code);

    let mut vm_state = VMState::new(&program, public_input.clone(), nondeterminism.clone());
    vm_state.op_stack.stack = init_stack.clone();
    vm_state.sponge = maybe_sponge;

    maybe_write_debuggable_program_to_disk(&program, &vm_state);

    if let Err(err) = vm_state.run() {
        panic!("{err}\n\nFinal state was: {vm_state}")
    }
    let terminal_state = vm_state;

    if !terminal_state.jump_stack.is_empty() {
        panic!("Jump stack must be unchanged after code execution");
    }

    let final_stack_height = terminal_state.op_stack.stack.len() as isize;
    let initial_stack_height = init_stack.len();
    assert_eq!(
        expected_stack_diff,
        final_stack_height - initial_stack_height as isize,
        "Code must grow/shrink stack with expected number of elements.\n
        init height: {initial_stack_height}\n
        end height:  {final_stack_height}\n
        expected difference: {expected_stack_diff}\n\n
        initial stack: {}\n
        final stack:   {}",
        init_stack.iter().skip(NUM_OP_STACK_REGISTERS).join(","),
        terminal_state
            .op_stack
            .stack
            .iter()
            .skip(NUM_OP_STACK_REGISTERS)
            .join(","),
    );

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
            Some(init_stack),
        );
    }

    *stack = terminal_state.op_stack.stack.clone();
    terminal_state
}

/// If the environment variable “TRITON_TUI” is set, write
/// 1. the program to file `program.tasm`, and
/// 2. the VM state to file `vm_state.json`.
///
/// These files can be used to debug the program using the [Triton TUI]:
/// ```sh
/// triton-tui program.tasm --initial-state vm_state.json
/// ```
///
/// [Triton TUI]: https://crates.io/crates/triton-tui
pub fn maybe_write_debuggable_program_to_disk(program: &Program, vm_state: &VMState) {
    let Ok(_) = std::env::var("TRITON_TUI") else {
        return;
    };

    let mut state_file = std::fs::File::create("vm_state.json").unwrap();
    let state = serde_json::to_string(&vm_state).unwrap();
    write!(state_file, "{state}").unwrap();

    let mut program_file = std::fs::File::create("program.tasm").unwrap();
    write!(program_file, "{program}").unwrap();
}

/// Prepare state and run Triton VM
pub fn execute_with_terminal_state(
    program: &Program,
    std_in: &[BFieldElement],
    stack: &[BFieldElement],
    nondeterminism: &NonDeterminism,
    maybe_sponge: Option<VmHasher>,
) -> Result<VMState, InstructionError> {
    let public_input = PublicInput::new(std_in.into());
    let mut vm_state = VMState::new(program, public_input, nondeterminism.to_owned());
    vm_state.op_stack.stack = stack.to_owned();
    vm_state.sponge = maybe_sponge;

    match vm_state.run() {
        Ok(()) => {
            println!("Triton VM execution successful.");
            Ok(vm_state)
        }
        Err(err) => {
            if let Some(ref sponge) = vm_state.sponge {
                println!("tasm final sponge:");
                println!("{}", sponge.state.iter().join(", "));
            }
            println!("Triton VM execution failed. Final state:\n{vm_state}");
            Err(err)
        }
    }
}

/// Run the prover on the program. If `init_stack` is provided, the prover is run on a program
/// with the code to setup the stack prepended, since the prover will always fail if the stack
/// is not initialized to the minimal height. The first `NUM_OP_STACK_REGISTERS` of `init_stack`
/// are ignored.
/// If you run this, make sure `opt-level` is set to 3.
pub fn prove_and_verify(
    program: &Program,
    std_in: &[BFieldElement],
    nondeterminism: &NonDeterminism,
    output: &[BFieldElement],
    init_stack: Option<Vec<BFieldElement>>,
) {
    use triton_vm::instruction::AnInstruction::*;
    use triton_vm::prelude::LabelledInstruction::*;
    let timing_report_label = match program.labelled_instructions().first().unwrap() {
        Instruction(Call(func)) => func.to_owned(),
        Label(label) => label.to_owned(),
        _ => "Some program".to_owned(),
    };

    // Construct the program that initializes the stack to the expected start value.
    // If this is not done, a stack underflow will occur for most programs
    let stack_initialization_code = match init_stack {
        Some(init_stack) => init_stack
            .into_iter()
            .skip(NUM_OP_STACK_REGISTERS)
            .map(|word| triton_instr!(push word))
            .collect(),
        None => triton_asm!(),
    };

    let program =
        Program::new(&[stack_initialization_code, program.labelled_instructions()].concat());

    let (aet, _) = program
        .trace_execution(PublicInput::new(std_in.to_owned()), nondeterminism.clone())
        .unwrap();

    let stark = Stark::default();
    let claim = Claim::about_program(&program)
        .with_output(output.to_owned())
        .with_input(std_in.to_owned());

    let tick = SystemTime::now();
    let mut profiler = Some(TritonProfiler::new(&timing_report_label));
    let proof = stark.prove(&claim, &aet, &mut profiler).unwrap();
    let mut profiler = profiler.unwrap();
    profiler.finish();
    let measured_time = tick.elapsed().expect("Don't mess with time");

    let padded_height = proof.padded_height().unwrap();
    let fri = stark.derive_fri(padded_height).unwrap();
    let report = profiler
        .report()
        .with_cycle_count(aet.processor_trace.nrows())
        .with_padded_height(padded_height)
        .with_fri_domain_len(fri.domain.length);
    println!("{report}");

    println!("Done proving. Elapsed time: {:?}", measured_time);
    println!(
        "Proof was generated from:
        table lengths:
          processor table: {}
          hash table: {}
          u32 table: {}
          op-stack table: {}
          RAM table: {}
          Program table: {}
          Cascade table: {}
          Lookup table: {}",
        aet.height_of_table(TableId::Processor),
        aet.height_of_table(TableId::Hash),
        aet.height_of_table(TableId::U32),
        aet.height_of_table(TableId::OpStack),
        aet.height_of_table(TableId::Ram),
        aet.height_of_table(TableId::Program),
        aet.height_of_table(TableId::Cascade),
        aet.height_of_table(TableId::Lookup),
    );

    assert!(
        triton_vm::verify(stark, &claim, &proof),
        "Generated proof must verify for program:\n {}",
        program,
    );
}

pub fn generate_full_profile(
    name: &str,
    program: Program,
    public_input: &PublicInput,
    nondeterminism: &NonDeterminism,
    only_print_aggregate: bool,
) -> String {
    #[derive(Debug, Clone, Eq, PartialEq, Hash)]
    struct AggregateProfileLine {
        label: String,
        call_depth: usize,
        cycle_count: u32,
    }

    let (_output, profile) = program
        .profile(public_input.clone(), nondeterminism.clone())
        .unwrap();
    let mut printed_profile = format!("{name}:\n");
    if !only_print_aggregate {
        printed_profile = format!("{printed_profile}\n# call graph\n");

        for line in profile.profile.iter() {
            let indentation = vec!["  "; line.call_depth].join("");
            let label = &line.label;
            let cycle_count = line.table_heights_start.processor;
            printed_profile = format!("{printed_profile}{indentation} {label}: {cycle_count}\n");
        }
    }

    printed_profile = format!("{printed_profile}\n# aggregated unsorted\n");
    let mut aggregated: Vec<AggregateProfileLine> = vec![];
    for line in profile.profile.iter() {
        if let Some(agg) = aggregated
            .iter_mut()
            .find(|a| a.label == line.label && a.call_depth == line.call_depth)
        {
            agg.cycle_count += line.table_heights_start.processor;
        } else {
            aggregated.push(AggregateProfileLine {
                label: line.label.to_owned(),
                call_depth: line.call_depth,
                cycle_count: line.table_heights_start.processor,
            });
        }
    }

    let total_cycle_count = aggregated[0].cycle_count as f32;
    for aggregate_line in aggregated.iter() {
        let indentation = vec!["  "; aggregate_line.call_depth].join("");
        let label = &aggregate_line.label;
        let cycle_count = aggregate_line.cycle_count;
        printed_profile = format!(
            "{printed_profile}{indentation} {label}:, {cycle_count}, {};\n",
            (aggregate_line.cycle_count as f32) / total_cycle_count
        );
    }

    printed_profile = format!("{printed_profile}\n# aggregated + sorted\n");
    aggregated.sort_by(|a, b| a.cycle_count.cmp(&b.cycle_count));
    for aggregate_line in aggregated {
        let indentation = vec!["  "; aggregate_line.call_depth].join("");
        let label = aggregate_line.label;
        let cycle_count = aggregate_line.cycle_count;
        printed_profile = format!(
            "{printed_profile}{indentation} {label}:, {cycle_count}, {};\n",
            (aggregate_line.cycle_count as f32) / total_cycle_count
        );
    }
    printed_profile
}
