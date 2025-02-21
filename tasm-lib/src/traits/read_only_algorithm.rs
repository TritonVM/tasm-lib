use std::collections::HashMap;
use std::collections::VecDeque;

use rand::prelude::*;
use triton_vm::prelude::*;

use super::basic_snippet::BasicSnippet;
use super::rust_shadow::RustShadow;
use crate::InitVmState;
use crate::linker::execute_bench;
use crate::prelude::Tip5;
use crate::snippet_bencher::BenchmarkCase;
use crate::snippet_bencher::NamedBenchmarkResult;
use crate::snippet_bencher::write_benchmarks;
use crate::test_helpers::test_rust_equivalence_given_complete_state;

/// An ReadOnlyAlgorithm can read memory and take ND_input.
///
/// An ReadOnlyAlgorithm is a piece of tasm code that can read memory and read
/// from non-determinism.
///
/// See also: [closure], [function], [procedure], [accessor], [mem_preserver],
///           [algorithm]
///
/// [closure]: crate::traits::closure::Closure
/// [function]: crate::traits::function::Function
/// [procedure]: crate::traits::procedure::Procedure
/// [accessor]: crate::traits::accessor::Accessor
/// [mem_preserver]: crate::traits::mem_preserver::MemPreserver
/// [algorithm]: crate::traits::algorithm::Algorithm
pub trait ReadOnlyAlgorithm: BasicSnippet {
    fn rust_shadow(
        &self,
        stack: &mut Vec<BFieldElement>,
        memory: &HashMap<BFieldElement, BFieldElement>,
        nd_tokens: VecDeque<BFieldElement>,
        nd_digests: VecDeque<Digest>,
    );

    fn pseudorandom_initial_state(
        &self,
        seed: [u8; 32],
        bench_case: Option<BenchmarkCase>,
    ) -> ReadOnlyAlgorithmInitialState;

    fn corner_case_initial_states(&self) -> Vec<ReadOnlyAlgorithmInitialState> {
        vec![]
    }
}

#[derive(Debug, Clone, Default)]
pub struct ReadOnlyAlgorithmInitialState {
    pub stack: Vec<BFieldElement>,
    pub nondeterminism: NonDeterminism,
}

impl From<ReadOnlyAlgorithmInitialState> for InitVmState {
    fn from(value: ReadOnlyAlgorithmInitialState) -> Self {
        Self {
            stack: value.stack,
            nondeterminism: value.nondeterminism,
            ..Default::default()
        }
    }
}

pub struct ShadowedReadOnlyAlgorithm<T: ReadOnlyAlgorithm> {
    algorithm: T,
}

impl<T: ReadOnlyAlgorithm> ShadowedReadOnlyAlgorithm<T> {
    pub fn new(algorithm: T) -> Self {
        Self { algorithm }
    }
}

impl<T> RustShadow for ShadowedReadOnlyAlgorithm<T>
where
    T: ReadOnlyAlgorithm,
{
    fn inner(&self) -> &dyn BasicSnippet {
        &self.algorithm
    }

    fn rust_shadow_wrapper(
        &self,
        _stdin: &[BFieldElement],
        nondeterminism: &NonDeterminism,
        stack: &mut Vec<BFieldElement>,
        memory: &mut HashMap<BFieldElement, BFieldElement>,
        _sponge: &mut Option<Tip5>,
    ) -> Vec<BFieldElement> {
        self.algorithm.rust_shadow(
            stack,
            memory,
            nondeterminism.individual_tokens.to_owned().into(),
            nondeterminism.digests.to_owned().into(),
        );
        vec![]
    }

    fn test(&self) {
        for corner_case in self.algorithm.corner_case_initial_states() {
            let stdin = vec![];
            test_rust_equivalence_given_complete_state(
                self,
                &corner_case.stack,
                &stdin,
                &corner_case.nondeterminism,
                &None,
                None,
            );
        }

        let num_states = 10;
        let mut rng = StdRng::from_seed(rand::random());
        for _ in 0..num_states {
            let ReadOnlyAlgorithmInitialState {
                stack,
                nondeterminism,
            } = self
                .algorithm
                .pseudorandom_initial_state(rng.random(), None);

            let stdin = vec![];
            test_rust_equivalence_given_complete_state(
                self,
                &stack,
                &stdin,
                &nondeterminism,
                &None,
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
            let ReadOnlyAlgorithmInitialState {
                stack,
                nondeterminism,
            } = self
                .algorithm
                .pseudorandom_initial_state(rng.random(), Some(bench_case));
            let program = self.algorithm.link_for_isolated_run();
            let benchmark = execute_bench(&program, &stack, vec![], nondeterminism, None);
            let benchmark = NamedBenchmarkResult {
                name: self.algorithm.entrypoint(),
                benchmark_result: benchmark,
                case: bench_case,
            };
            benchmarks.push(benchmark);
        }

        write_benchmarks(benchmarks);
    }
}
