use std::{cell::RefCell, collections::HashMap, rc::Rc};

use itertools::Itertools;
use rand::{rngs::StdRng, thread_rng, Rng, SeedableRng};
use triton_vm::{BFieldElement, NonDeterminism};
use twenty_first::shared_math::bfield_codec::BFieldCodec;

use crate::{
    linker::{execute_bench, link_for_isolated_run},
    snippet_bencher::{write_benchmarks, BenchmarkCase, BenchmarkResult},
    test_helpers::{
        rust_final_state, tasm_final_state, verify_memory_equivalence, verify_sponge_equivalence,
        verify_stack_equivalence, verify_stack_growth,
    },
    traits::{basic_snippet::BasicSnippet, rust_shadow::RustShadow},
    VmHasherState,
};

/// A Procedure is a piece of tasm code that can do almost anything: modify stack, read
/// from and write to memory, take in nondeterminism, and read and write from standard
/// input/output. What it cannot do is stand alone. In other words, it is still wrapped
/// in a function (lower case f, as in 'labelled scope'); and cannot be proved as
/// a standalone program.
///
/// See also: [closure], [function], [algorithm]
pub trait Procedure: BasicSnippet {
    /// Returns standard output
    fn rust_shadow(
        &self,
        stack: &mut Vec<BFieldElement>,
        memory: &mut HashMap<BFieldElement, BFieldElement>,
        nondeterminism: &NonDeterminism<BFieldElement>,
        public_input: &[BFieldElement],
        sponge_state: &mut Option<VmHasherState>,
    ) -> Vec<BFieldElement>;

    fn preprocess<T: BFieldCodec>(
        _meta_input: T,
        _nondeterminism: &mut NonDeterminism<BFieldElement>,
    ) {
    }

    fn pseudorandom_initial_state(
        &self,
        seed: [u8; 32],
        bench_case: Option<BenchmarkCase>,
    ) -> ProcedureInitialState;

    fn corner_case_initial_states(&self) -> Vec<ProcedureInitialState> {
        vec![]
    }
}

#[derive(Debug, Clone, Default)]
pub struct ProcedureInitialState {
    pub stack: Vec<BFieldElement>,
    pub nondeterminism: NonDeterminism<BFieldElement>,
    pub public_input: Vec<BFieldElement>,
    pub sponge_state: Option<VmHasherState>,
}

pub struct ShadowedProcedure<P: Procedure + 'static> {
    procedure: Rc<RefCell<P>>,
}

impl<P: Procedure + 'static> ShadowedProcedure<P> {
    pub fn new(procedure: P) -> Self {
        Self {
            procedure: Rc::new(RefCell::new(procedure)),
        }
    }
}

impl<P: Procedure + 'static> RustShadow for ShadowedProcedure<P> {
    fn inner(&self) -> Rc<RefCell<dyn BasicSnippet>> {
        self.procedure.clone()
    }

    fn rust_shadow_wrapper(
        &self,
        stdin: &[BFieldElement],
        nondeterminism: &NonDeterminism<BFieldElement>,
        stack: &mut Vec<BFieldElement>,
        memory: &mut HashMap<BFieldElement, BFieldElement>,
        sponge_state: &mut Option<VmHasherState>,
    ) -> Vec<BFieldElement> {
        self.procedure
            .borrow()
            .rust_shadow(stack, memory, nondeterminism, stdin, sponge_state)
    }

    fn test(&self) {
        let num_states = 5;
        let seed: [u8; 32] = thread_rng().gen();
        // = [
        // 0xf9, 0x2c, 0x53, 0x92, 0x4b, 0x46, 0x69, 0xee, 0x2a, 0x42, 0x28, 0x3a, 0x1f, 0x9b,
        // 0x1f, 0x26, 0xf4, 0x63, 0xc1, 0xe4, 0x71, 0xb6, 0x17, 0x7b, 0x8a, 0xf7, 0x7e, 0x4c,
        // 0xb6, 0xb5, 0x64, 0xd1,
        // ];
        let mut rng: StdRng = SeedableRng::from_seed(seed);
        let procedure = &self.procedure;
        let entrypoint = procedure.borrow().entrypoint();

        for _ in 0..num_states {
            let seed: [u8; 32] = rng.gen();
            println!(
                "testing {} common case with seed: [{}]",
                entrypoint,
                seed.iter().map(|h| format!("{:#04x}", h)).join(", ")
            );
            let ProcedureInitialState {
                stack,
                nondeterminism,
                public_input,
                sponge_state,
            } = procedure.borrow().pseudorandom_initial_state(seed, None);

            let init_stack = stack.to_vec();

            let rust =
                rust_final_state(self, &stack, &public_input, &nondeterminism, &sponge_state);

            // run tvm
            let words_statically_allocated = 0;
            let tasm = tasm_final_state(
                self,
                &stack,
                &public_input,
                nondeterminism,
                &sponge_state,
                words_statically_allocated,
            );

            // assert_eq!(tasm.final_sponge_state.state, rust.final_sponge_state.state);
            // can't do this without changing the VM interface, unfortunately ...

            assert_eq!(
                rust.output, tasm.output,
                "Rust shadowing and VM std out must agree"
            );

            verify_stack_equivalence(&rust.final_stack, &tasm.final_stack);
            verify_memory_equivalence(&rust.final_ram, &tasm.final_ram);
            verify_stack_growth(self, &init_stack, &tasm.final_stack);
            verify_sponge_equivalence(&rust.final_sponge_state, &tasm.final_sponge_state);
        }
    }

    fn bench(&self) {
        let mut rng: StdRng = SeedableRng::from_seed(
            hex::decode("73a24b6b8b32e4d7d563a4d9a85f476573a24b6b8b32e4d7d563a4d9a85f4765")
                .unwrap()
                .try_into()
                .unwrap(),
        );
        let mut benchmarks = Vec::with_capacity(2);

        for bench_case in [BenchmarkCase::CommonCase, BenchmarkCase::WorstCase] {
            let ProcedureInitialState {
                stack,
                nondeterminism,
                public_input,
                sponge_state,
            } = self
                .procedure
                .borrow()
                .pseudorandom_initial_state(rng.gen(), Some(bench_case));
            let words_statically_allocated = 10; // okay buffer
            let program = link_for_isolated_run(self.procedure.clone(), words_statically_allocated);
            let execution_result =
                execute_bench(&program, &stack, public_input, nondeterminism, sponge_state);
            let benchmark = BenchmarkResult {
                name: self.procedure.borrow().entrypoint(),
                clock_cycle_count: execution_result.cycle_count,
                hash_table_height: execution_result.hash_table_height,
                u32_table_height: execution_result.u32_table_height,
                case: bench_case,
            };
            benchmarks.push(benchmark);
        }

        write_benchmarks(benchmarks);
    }
}
