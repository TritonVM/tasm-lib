use std::{cell::RefCell, collections::HashMap, rc::Rc};

use rand::{rngs::StdRng, thread_rng, Rng, SeedableRng};
use triton_vm::{BFieldElement, NonDeterminism};
use twenty_first::shared_math::bfield_codec::BFieldCodec;

use crate::{
    linker::{execute_bench, link_for_isolated_run},
    snippet::{BasicSnippet, RustShadow},
    snippet_bencher::{write_benchmarks, BenchmarkCase, BenchmarkResult},
    test_helpers::test_rust_equivalence_given_complete_state,
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
    ) -> Vec<BFieldElement>;

    fn preprocess<T: BFieldCodec>(
        _meta_input: T,
        _nondeterminism: &mut NonDeterminism<BFieldElement>,
    ) {
    }

    /// Returns (stack, memory, nondeterminism, public_input)
    fn pseudorandom_initial_state(
        &self,
        seed: [u8; 32],
        bench_case: Option<BenchmarkCase>,
    ) -> (
        Vec<BFieldElement>,
        HashMap<BFieldElement, BFieldElement>,
        NonDeterminism<BFieldElement>,
        Vec<BFieldElement>,
    );
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
    ) -> Vec<BFieldElement> {
        self.procedure
            .borrow()
            .rust_shadow(stack, memory, nondeterminism, stdin)
    }

    fn test(&self) {
        let num_states = 5;
        let mut rng = thread_rng();
        let procedure = &self.procedure;
        let entrypoint = procedure.borrow().entrypoint();

        for _ in 0..num_states {
            let seed: [u8; 32] = rng.gen();
            println!("testing {} common case with seed: {:x?}", entrypoint, seed);
            let (stack, memory, nondeterminism, public_input) =
                procedure.borrow().pseudorandom_initial_state(seed, None);

            test_rust_equivalence_given_complete_state(
                self,
                &stack,
                &public_input,
                &nondeterminism,
                &memory,
                1,
                None,
            );
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
            let (stack, memory, nondeterminism, public_input) = self
                .procedure
                .borrow()
                .pseudorandom_initial_state(rng.gen(), Some(bench_case));
            let program = link_for_isolated_run(self.procedure.clone(), 1);
            let execution_result = execute_bench(
                &program,
                &stack,
                public_input,
                nondeterminism,
                &memory,
                Some(1),
            );
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