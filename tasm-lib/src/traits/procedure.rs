use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use rand::prelude::*;
use triton_vm::prelude::*;

use crate::linker::execute_bench;
use crate::linker::link_for_isolated_run;
use crate::snippet_bencher::write_benchmarks;
use crate::snippet_bencher::BenchmarkCase;
use crate::snippet_bencher::NamedBenchmarkResult;
use crate::test_helpers::rust_final_state;
use crate::test_helpers::tasm_final_state;
use crate::test_helpers::verify_memory_equivalence;
use crate::test_helpers::verify_sponge_equivalence;
use crate::test_helpers::verify_stack_equivalence;
use crate::test_helpers::verify_stack_growth;
use crate::traits::basic_snippet::BasicSnippet;
use crate::traits::rust_shadow::RustShadow;
use crate::InitVmState;
use crate::VmHasher;

/// A trait that can modify all parts of the VM state.
///
/// A Procedure is a piece of tasm code that can do almost anything: modify stack, read
/// from and write to memory, take in nondeterminism, and read and write from standard
/// input/output. What it cannot do is stand alone. In other words, it is still wrapped
/// in a function (lower case f, as in 'labelled scope'); and cannot be proved as
/// a standalone program.
///
/// See also: [closure], [function], [algorithm], [read_only_algorithm],
///           [accessor], [mem_preserver]
///
/// [closure]: crate::traits::closure::Closure
/// [function]: crate::traits::function::Function
/// [algorithm]: crate::traits::algorithm::Algorithm
/// [read_only_algorithm]: crate::traits::read_only_algorithm::ReadOnlyAlgorithm
/// [accessor]: crate::traits::accessor::Accessor
/// [mem_preserver]: crate::traits::mem_preserver::MemPreserver
pub trait Procedure: BasicSnippet {
    /// Returns standard output
    fn rust_shadow(
        &self,
        stack: &mut Vec<BFieldElement>,
        memory: &mut HashMap<BFieldElement, BFieldElement>,
        nondeterminism: &NonDeterminism,
        public_input: &[BFieldElement],
        sponge: &mut Option<VmHasher>,
    ) -> Vec<BFieldElement>;

    fn preprocess<T: BFieldCodec>(_meta_input: T, _nondeterminism: &mut NonDeterminism) {}

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
    pub nondeterminism: NonDeterminism,
    pub public_input: Vec<BFieldElement>,
    pub sponge: Option<VmHasher>,
}

impl From<ProcedureInitialState> for InitVmState {
    fn from(value: ProcedureInitialState) -> Self {
        Self {
            stack: value.stack,
            public_input: value.public_input,
            nondeterminism: value.nondeterminism,
            sponge: value.sponge,
        }
    }
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
        nondeterminism: &NonDeterminism,
        stack: &mut Vec<BFieldElement>,
        memory: &mut HashMap<BFieldElement, BFieldElement>,
        sponge: &mut Option<VmHasher>,
    ) -> Vec<BFieldElement> {
        self.procedure
            .borrow()
            .rust_shadow(stack, memory, nondeterminism, stdin, sponge)
    }

    fn test(&self) {
        let num_states = 5;
        let seed: [u8; 32] = thread_rng().gen();
        let mut rng: StdRng = SeedableRng::from_seed(seed);
        let procedure = &self.procedure.borrow();

        for corner_case in procedure.corner_case_initial_states().into_iter() {
            self.test_initial_state(corner_case);
        }

        for _ in 0..num_states {
            let seed: [u8; 32] = rng.gen();
            let state = procedure.pseudorandom_initial_state(seed, None);

            self.test_initial_state(state);
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
                sponge,
            } = self
                .procedure
                .borrow()
                .pseudorandom_initial_state(rng.gen(), Some(bench_case));
            let program = link_for_isolated_run(self.procedure.clone());
            let benchmark = execute_bench(&program, &stack, public_input, nondeterminism, sponge);
            let benchmark = NamedBenchmarkResult {
                name: self.procedure.borrow().entrypoint(),
                benchmark_result: benchmark,
                case: bench_case,
            };
            benchmarks.push(benchmark);
        }

        write_benchmarks(benchmarks);
    }
}

impl<P: Procedure + 'static> ShadowedProcedure<P> {
    fn test_initial_state(&self, state: ProcedureInitialState) {
        let ProcedureInitialState {
            stack,
            nondeterminism,
            public_input,
            sponge,
        } = state;

        let rust = rust_final_state(self, &stack, &public_input, &nondeterminism, &sponge);

        // run tvm
        let tasm = tasm_final_state(self, &stack, &public_input, nondeterminism, &sponge);

        assert_eq!(
            rust.public_output, tasm.public_output,
            "Rust shadowing and VM std out must agree"
        );

        verify_stack_growth(self, &stack, &tasm.op_stack.stack);

        verify_stack_equivalence(
            "Rust-shadow",
            &rust.stack,
            "TVM execution",
            &tasm.op_stack.stack,
        );
        verify_memory_equivalence("Rust-shadow", &rust.ram, "TVM execution", &tasm.ram);
        verify_sponge_equivalence(&rust.sponge, &tasm.sponge);
    }
}
