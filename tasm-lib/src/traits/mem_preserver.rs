use std::collections::HashMap;
use std::collections::VecDeque;

use rand::prelude::*;
use triton_vm::prelude::*;

use super::basic_snippet::BasicSnippet;
use super::rust_shadow::RustShadow;
use crate::InitVmState;
use crate::linker::execute_bench;
use crate::snippet_bencher::BenchmarkCase;
use crate::snippet_bencher::NamedBenchmarkResult;
use crate::snippet_bencher::write_benchmarks;
use crate::test_helpers::test_rust_equivalence_given_complete_state;

/// A MemPreserver cannot modify memory
///
/// An MemPreserver is a piece of tasm code that can do pretty much everything
/// except modify memory, including static memory. It can read from any input
/// and write to standard out. It can also modify the sponge state.
/// See also: [closure], [function], [procedure], [algorithm],
///           [read_only_algorithm], [accessor]
///
/// [closure]: crate::traits::closure::Closure
/// [function]: crate::traits::function::Function
/// [procedure]: crate::traits::procedure::Procedure
/// [algorithm]: crate::traits::algorithm::Algorithm
/// [read_only_algorithm]: crate::traits::read_only_algorithm::ReadOnlyAlgorithm
/// [accessor]: crate::traits::accessor::Accessor
pub trait MemPreserver: BasicSnippet {
    fn rust_shadow(
        &self,
        stack: &mut Vec<BFieldElement>,
        memory: &HashMap<BFieldElement, BFieldElement>,
        nd_tokens: VecDeque<BFieldElement>,
        nd_digests: VecDeque<Digest>,
        stdin: VecDeque<BFieldElement>,
        sponge: &mut Option<Tip5>,
    ) -> Vec<BFieldElement>;

    fn pseudorandom_initial_state(
        &self,
        seed: [u8; 32],
        bench_case: Option<BenchmarkCase>,
    ) -> MemPreserverInitialState;

    fn corner_case_initial_states(&self) -> Vec<MemPreserverInitialState> {
        vec![]
    }
}

#[derive(Debug, Clone, Default)]
pub struct MemPreserverInitialState {
    pub stack: Vec<BFieldElement>,
    pub nondeterminism: NonDeterminism,
    pub public_input: VecDeque<BFieldElement>,
    pub sponge_state: Option<Tip5>,
}

impl From<MemPreserverInitialState> for InitVmState {
    fn from(value: MemPreserverInitialState) -> Self {
        Self {
            stack: value.stack,
            nondeterminism: value.nondeterminism,
            public_input: value.public_input.into(),
            sponge: value.sponge_state,
        }
    }
}

pub struct ShadowedMemPreserver<T: MemPreserver> {
    mem_preserver: T,
}

impl<T: MemPreserver> ShadowedMemPreserver<T> {
    pub fn new(mem_preserver: T) -> Self {
        Self { mem_preserver }
    }
}

impl<T> RustShadow for ShadowedMemPreserver<T>
where
    T: MemPreserver,
{
    fn inner(&self) -> &dyn BasicSnippet {
        &self.mem_preserver
    }

    fn rust_shadow_wrapper(
        &self,
        stdin: &[BFieldElement],
        nondeterminism: &NonDeterminism,
        stack: &mut Vec<BFieldElement>,
        memory: &mut HashMap<BFieldElement, BFieldElement>,
        sponge: &mut Option<Tip5>,
    ) -> Vec<BFieldElement> {
        self.mem_preserver.rust_shadow(
            stack,
            memory,
            nondeterminism.individual_tokens.to_owned().into(),
            nondeterminism.digests.to_owned().into(),
            stdin.to_vec().into(),
            sponge,
        )
    }

    fn test(&self) {
        for corner_case in self.mem_preserver.corner_case_initial_states() {
            let stdin: Vec<_> = corner_case.public_input.into();

            test_rust_equivalence_given_complete_state(
                self,
                &corner_case.stack,
                &stdin,
                &corner_case.nondeterminism,
                &corner_case.sponge_state,
                None,
            );
        }

        let num_states = 10;
        let mut rng = StdRng::from_seed(rand::random());
        for _ in 0..num_states {
            let MemPreserverInitialState {
                stack,
                public_input,
                sponge_state,
                nondeterminism: non_determinism,
            } = self
                .mem_preserver
                .pseudorandom_initial_state(rng.random(), None);

            let stdin: Vec<_> = public_input.into();
            test_rust_equivalence_given_complete_state(
                self,
                &stack,
                &stdin,
                &non_determinism,
                &sponge_state,
                None,
            );
        }
    }

    fn bench(&self) {
        let mut rng = StdRng::from_seed(
            hex::decode("73a24b6b8b32e4d7d563a4d9a85f476573a24b6b8b32e4d7d563a4d9a85f4765")
                .unwrap()
                .try_into()
                .unwrap(),
        );
        let mut benchmarks = Vec::with_capacity(2);

        for bench_case in [BenchmarkCase::CommonCase, BenchmarkCase::WorstCase] {
            let MemPreserverInitialState {
                stack,
                public_input,
                sponge_state,
                nondeterminism: non_determinism,
            } = self
                .mem_preserver
                .pseudorandom_initial_state(rng.random(), Some(bench_case));
            let program = self.mem_preserver.link_for_isolated_run();
            let benchmark = execute_bench(
                &program,
                &stack,
                public_input.into(),
                non_determinism,
                sponge_state,
            );
            let benchmark = NamedBenchmarkResult {
                name: self.mem_preserver.entrypoint(),
                benchmark_result: benchmark,
                case: bench_case,
            };
            benchmarks.push(benchmark);
        }

        write_benchmarks(benchmarks);
    }
}
