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

use itertools::Itertools;
use memory::dyn_malloc;
use num_traits::Zero;
use triton_vm::isa::op_stack::NUM_OP_STACK_REGISTERS;
use triton_vm::prelude::*;
use web_time::SystemTime;

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
pub mod prelude;
pub mod rust_shadowing_helper_functions;
pub mod snippet_bencher;
pub mod structure;
pub mod test_helpers;
pub mod traits;
pub mod verifier;

// re-exports for types exposed in our public API.
pub use triton_vm;
use triton_vm::isa::instruction::AnInstruction;
use triton_vm::prelude::TableId;
pub use triton_vm::twenty_first;

use crate::test_helpers::prepend_program_with_stack_setup;
use crate::traits::rust_shadow::RustShadowError;

pub(crate) const U32_TO_USIZE_ERR: &str =
    "internal error: type `usize` should have at least 32 bits";
pub(crate) const USIZE_TO_U64_ERR: &str =
    "internal error: type `usize` should have at most 64 bits";

#[derive(Clone, Debug, Default)]
pub struct InitVmState {
    pub stack: Vec<BFieldElement>,
    pub public_input: Vec<BFieldElement>,
    pub nondeterminism: NonDeterminism,
    pub sponge: Option<Tip5>,
}

impl InitVmState {
    pub fn with_stack(stack: Vec<BFieldElement>) -> Self {
        InitVmState {
            stack,
            public_input: vec![],
            nondeterminism: NonDeterminism::default(),
            sponge: None,
        }
    }

    pub fn with_stack_and_memory(
        stack: Vec<BFieldElement>,
        memory: HashMap<BFieldElement, BFieldElement>,
    ) -> Self {
        InitVmState {
            stack,
            public_input: vec![],
            nondeterminism: NonDeterminism::default().with_ram(memory),
            sponge: None,
        }
    }
}

#[derive(Clone, Debug)]
pub struct RustShadowOutputState {
    pub public_output: Vec<BFieldElement>,
    pub stack: Vec<BFieldElement>,
    pub ram: HashMap<BFieldElement, BFieldElement>,
    pub sponge: Option<Tip5>,
}

pub fn empty_stack() -> Vec<BFieldElement> {
    vec![BFieldElement::zero(); NUM_OP_STACK_REGISTERS]
}

pub fn push_encodable<T: BFieldCodec>(stack: &mut Vec<BFieldElement>, value: &T) {
    stack.extend(value.encode().into_iter().rev());
}

/// Pops an element of the specified, generic type from the stack.
///
/// ### Errors
///
/// Errors if
/// - the generic type has [dynamic length](BFieldCodec::static_length)
/// - the stack does not contain enough elements
/// - the top of the stack does not correspond to a [`BFieldCodec`] encoded
///   element of type `T`
pub fn pop_encodable<T: BFieldCodec>(stack: &mut Vec<BFieldElement>) -> Result<T, RustShadowError> {
    let Some(len) = T::static_length() else {
        return Err(RustShadowError::Other);
    };
    let limbs: Vec<_> = (0..len)
        .map(|_| stack.pop().ok_or(RustShadowError::StackUnderflow))
        .try_collect()?;

    T::decode(&limbs)
        .map(|t| *t)
        .map_err(|_| RustShadowError::DecodingError)
}

/// Execute a Triton-VM program and test correct behavior indicators.
/// Modify stack and memory. Panic if anything goes wrong.
pub(crate) fn execute_test(
    code: &[LabelledInstruction],
    stack: &mut Vec<BFieldElement>,
    expected_stack_diff: isize,
    std_in: Vec<BFieldElement>,
    nondeterminism: NonDeterminism,
    maybe_sponge: Option<Tip5>,
) -> Result<VMState, RustShadowError> {
    let init_stack = stack.to_owned();
    let public_input = PublicInput::new(std_in.clone());
    let program = Program::new(code);

    let mut vm_state = VMState::new(
        program.clone(),
        public_input.clone(),
        nondeterminism.clone(),
    );
    vm_state.op_stack.stack.clone_from(&init_stack);
    vm_state.sponge = maybe_sponge;

    maybe_write_debuggable_vm_state_to_disk(&vm_state);

    if let Err(err) = vm_state.run() {
        eprintln!("{err}\n\nFinal state was: {vm_state}");
        return Err(RustShadowError::VmError);
    }
    let terminal_state = vm_state;

    if !terminal_state.jump_stack.is_empty() {
        eprintln!("Jump stack must be unchanged after code execution");
        return Err(RustShadowError::VmError);
    }

    let final_stack_height = terminal_state.op_stack.stack.len() as isize;
    let initial_stack_height = init_stack.len();
    if expected_stack_diff != final_stack_height - initial_stack_height as isize {
        eprintln!(
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
        return Err(RustShadowError::VmError);
    }

    // If this environment variable is set, all programs, including the code to prepare the state,
    // will be proven and then verified.
    // Notice that this is only done after the successful execution of the program above, so all
    // produced proofs here should be valid.
    // If you run this, make sure `opt-level` is set to 3.
    if std::env::var("DYING_TO_PROVE").is_ok() {
        prove_and_verify(program, &std_in, &nondeterminism, Some(init_stack));
    }

    stack.clone_from(&terminal_state.op_stack.stack);
    Ok(terminal_state)
}

/// If the environment variable TASMLIB_TRITON_TUI is set, write the initial VM state
/// to file `vm_state.json`.
///
/// This file can be used to debug the program using the [Triton TUI]:
/// ```sh
/// triton-tui --initial-state vm_state.json
/// ```
///
/// [Triton TUI]: https://crates.io/crates/triton-tui
pub fn maybe_write_debuggable_vm_state_to_disk(vm_state: &VMState) {
    let Ok(_) = std::env::var("TASMLIB_TRITON_TUI") else {
        return;
    };

    let mut state_file = std::fs::File::create("vm_state.json").unwrap();
    let state = serde_json::to_string(&vm_state).unwrap();
    write!(state_file, "{state}").unwrap();
}

/// Prepare state and run Triton VM
pub(crate) fn execute_with_terminal_state(
    program: Program,
    std_in: &[BFieldElement],
    stack: &[BFieldElement],
    nondeterminism: &NonDeterminism,
    maybe_sponge: Option<Tip5>,
) -> Result<VMState, InstructionError> {
    let public_input = PublicInput::new(std_in.into());
    let mut vm_state = VMState::new(program, public_input, nondeterminism.to_owned());
    stack.clone_into(&mut vm_state.op_stack.stack);
    vm_state.sponge = maybe_sponge;

    maybe_write_debuggable_vm_state_to_disk(&vm_state);
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

/// Run prover on the program, with stack-initialization converted to code.
///
/// Run the prover on the program. If `init_stack` is provided, the prover is run on a program
/// with the code to setup the stack prepended, since the prover will always fail if the stack
/// is not initialized to the minimal height. The first `NUM_OP_STACK_REGISTERS` of `init_stack`
/// are ignored.
/// If you run this, make sure `opt-level` is set to 3.
pub fn prove_and_verify(
    program: Program,
    std_in: &[BFieldElement],
    nondeterminism: &NonDeterminism,
    init_stack: Option<Vec<BFieldElement>>,
) {
    let labelled_instructions = program.labelled_instructions();
    let timing_report_label = match labelled_instructions.first().unwrap() {
        LabelledInstruction::Instruction(AnInstruction::Call(func)) => func,
        LabelledInstruction::Label(label) => label,
        _ => "Some program",
    };

    // Construct the program that initializes the stack to the expected start value.
    // If this is not done, a stack underflow will occur for most programs
    let program = match &init_stack {
        Some(init_stack) => prepend_program_with_stack_setup(init_stack, &program),
        None => program,
    };

    let claim = Claim::about_program(&program).with_input(std_in.to_owned());
    let (aet, public_output) = VM::trace_execution(
        program.clone(),
        PublicInput::new(std_in.to_owned()),
        nondeterminism.clone(),
    )
    .unwrap();
    let claim = claim.with_output(public_output);

    let stark = Stark::default();
    let tick = SystemTime::now();
    triton_vm::profiler::start(timing_report_label);
    let proof = stark.prove(&claim, &aet).unwrap();
    let profile = triton_vm::profiler::finish();
    let measured_time = tick.elapsed().expect("Don't mess with time");

    let padded_height = proof.padded_height().unwrap();
    let fri = stark.fri(padded_height).unwrap();
    let report = profile
        .with_cycle_count(aet.processor_trace.nrows())
        .with_padded_height(padded_height)
        .with_fri_domain_len(fri.domain.len());
    println!("{report}");

    println!("Done proving. Elapsed time: {measured_time:?}");
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
        "Generated proof must verify for program:\n{program}",
    );
}

/// A thin wrapper around [`VM::profile`].
pub fn generate_full_profile(
    name: &str,
    program: Program,
    public_input: &PublicInput,
    nondeterminism: &NonDeterminism,
) -> String {
    let (_output, profile) =
        VM::profile(program, public_input.clone(), nondeterminism.clone()).unwrap();
    format!("{name}:\n{profile}")
}

/// Glob-import this module to reduce the number of imports in a test module.
///
/// Feel free to add anything you frequently `use` in a test module.
#[cfg(test)]
pub mod test_prelude {
    /// A crate-specific replacement of the `#[test]` attribute for tests that
    /// should also be executed on `wasm` targets (which is almost all tests).
    ///
    /// If you specifically want to exclude a test from `wasm` targets, use the
    /// usual `#[test]` attribute instead.
    ///
    /// # Usage
    ///
    /// ```
    /// #[macro_rules_attr::apply(test)]
    /// fn foo() {
    ///     assert_eq!(4, 2 + 2);
    /// }
    /// ```
    macro_rules! test {
        ($item:item) => {
            #[test]
            #[cfg_attr(target_arch = "wasm32", wasm_bindgen_test::wasm_bindgen_test)]
            $item
        };
    }
    pub(crate) use test;

    /// A crate-specific replacement of the `#[test_strategy::proptest]`
    /// attribute for tests that should also be executed on `wasm` targets
    /// (which is almost all tests).
    ///
    /// If you specifically want to exclude a test from `wasm` targets, use the
    /// usual `#[test_strategy::proptest]` attribute instead.
    ///
    /// # Usage
    ///
    /// ```
    /// # use proptest::prop_assert_eq;
    /// #[macro_rules_attr::apply(proptest)]
    /// fn foo(#[strategy(0..=42)] x: i32) {
    ///     prop_assert_eq!(2 * x, x + x);
    /// }
    /// ```
    ///
    /// If you want to configure the test, use the usual syntax defined by
    /// [`test_strategy`]:
    /// ```
    /// # use proptest::prop_assert_eq;
    /// #[macro_rules_attr::apply(proptest(cases = 10, max_local_rejects = 5))]
    /// fn foo(#[strategy(0..=42)] x: i32) {
    ///     prop_assert_eq!(2 * x, x + x);
    /// }
    /// ```
    macro_rules! proptest {
        ($item:item $(($($config:tt)*))?) => {
            #[test_strategy::proptest $(($($config)*))?]
            #[cfg_attr(target_arch = "wasm32", wasm_bindgen_test::wasm_bindgen_test)]
            $item
        };
    }
    pub(crate) use proptest;

    pub use std::collections::HashMap;

    pub use itertools::Itertools;
    pub use proptest::prelude::Just;
    pub use proptest::prelude::Strategy;
    pub use proptest::prelude::TestCaseError;
    pub use proptest::prelude::any;
    pub use proptest::prelude::prop_assert;
    pub use proptest::prelude::prop_assert_eq;
    pub use proptest::prelude::prop_assume;
    pub use proptest::test_runner::TestCaseResult;
    pub use proptest_arbitrary_adapter::arb;
    pub use rand::Rng;
    pub use rand::RngCore;
    pub use rand::SeedableRng;
    pub use rand::prelude::IndexedMutRandom;
    pub use rand::prelude::IndexedRandom;
    pub use rand::prelude::IteratorRandom;
    pub use rand::rngs::StdRng;
    pub use test_strategy::Arbitrary;

    pub use crate::InitVmState;
    pub use crate::memory::encode_to_memory;
    pub(crate) use crate::pop_encodable;
    pub use crate::push_encodable;
    pub use crate::snippet_bencher::BenchmarkCase;
    pub use crate::test_helpers::test_assertion_failure;
    pub use crate::test_helpers::test_rust_equivalence_given_complete_state;
    pub use crate::traits::accessor::Accessor;
    pub use crate::traits::accessor::AccessorInitialState;
    pub use crate::traits::accessor::ShadowedAccessor;
    pub use crate::traits::algorithm::Algorithm;
    pub use crate::traits::algorithm::AlgorithmInitialState;
    pub use crate::traits::algorithm::ShadowedAlgorithm;
    pub use crate::traits::closure::Closure;
    pub use crate::traits::closure::ShadowedClosure;
    pub use crate::traits::function::Function;
    pub use crate::traits::function::FunctionInitialState;
    pub use crate::traits::function::ShadowedFunction;
    pub use crate::traits::mem_preserver::MemPreserver;
    pub use crate::traits::mem_preserver::MemPreserverInitialState;
    pub use crate::traits::mem_preserver::ShadowedMemPreserver;
    pub use crate::traits::procedure::Procedure;
    pub use crate::traits::procedure::ProcedureInitialState;
    pub use crate::traits::procedure::ShadowedProcedure;
    pub use crate::traits::read_only_algorithm::ReadOnlyAlgorithm;
    pub use crate::traits::read_only_algorithm::ReadOnlyAlgorithmInitialState;
    pub use crate::traits::read_only_algorithm::ShadowedReadOnlyAlgorithm;
    pub use crate::traits::rust_shadow::RustShadow;
    pub use crate::traits::rust_shadow::RustShadowError;
}
