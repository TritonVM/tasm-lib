use std::cell::RefCell;
use std::collections::HashMap;
use std::collections::VecDeque;
use std::rc::Rc;

use rand::prelude::*;
use triton_vm::prelude::*;

use crate::linker::execute_bench;
use crate::linker::link_for_isolated_run;
use crate::snippet_bencher::write_benchmarks;
use crate::snippet_bencher::BenchmarkCase;
use crate::snippet_bencher::NamedBenchmarkResult;
use crate::test_helpers::test_rust_equivalence_given_complete_state;
use crate::InitVmState;
use crate::VmHasher;

use super::basic_snippet::BasicSnippet;
use super::rust_shadow::RustShadow;

/// An ReadOnlyAlgorithm can read memory and take ND_input.
///
/// An ReadOnlyAlgorithm is a piece of tasm code that can read memory and read
/// from non-determinism.
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

pub struct ShadowedReadOnlyAlgorithm<T: ReadOnlyAlgorithm + 'static> {
    read_only_algorithm: Rc<RefCell<T>>,
}

impl<T: ReadOnlyAlgorithm + 'static> ShadowedReadOnlyAlgorithm<T> {
    pub fn new(algorithm: T) -> Self {
        Self {
            read_only_algorithm: Rc::new(RefCell::new(algorithm)),
        }
    }
}

impl<T> RustShadow for ShadowedReadOnlyAlgorithm<T>
where
    T: ReadOnlyAlgorithm + 'static,
{
    fn inner(&self) -> Rc<RefCell<dyn BasicSnippet>> {
        self.read_only_algorithm.clone()
    }

    fn rust_shadow_wrapper(
        &self,
        _stdin: &[BFieldElement],
        nondeterminism: &NonDeterminism,
        stack: &mut Vec<BFieldElement>,
        memory: &mut HashMap<BFieldElement, BFieldElement>,
        _sponge: &mut Option<VmHasher>,
    ) -> Vec<BFieldElement> {
        self.read_only_algorithm.borrow().rust_shadow(
            stack,
            memory,
            nondeterminism.individual_tokens.to_owned().into(),
            nondeterminism.digests.to_owned().into(),
        );
        vec![]
    }

    fn test(&self) {
        for (i, corner_case) in self
            .read_only_algorithm
            .borrow()
            .corner_case_initial_states()
            .into_iter()
            .enumerate()
        {
            println!(
                "testing {} corner case number {i}",
                self.read_only_algorithm.borrow().entrypoint(),
            );

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
        let seed: [u8; 32] = thread_rng().gen();
        let mut rng: StdRng = SeedableRng::from_seed(seed);
        for _ in 0..num_states {
            let seed: [u8; 32] = rng.gen();
            println!(
                "testing {} common case with seed: {:#4x?}",
                self.read_only_algorithm.borrow().entrypoint(),
                seed
            );
            let ReadOnlyAlgorithmInitialState {
                stack,
                nondeterminism,
            } = self
                .read_only_algorithm
                .borrow()
                .pseudorandom_initial_state(seed, None);

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
        let mut rng: StdRng = SeedableRng::from_seed(
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
                .read_only_algorithm
                .borrow()
                .pseudorandom_initial_state(rng.gen(), Some(bench_case));
            let program = link_for_isolated_run(self.read_only_algorithm.clone());
            let benchmark = execute_bench(&program, &stack, vec![], nondeterminism, None);
            let benchmark = NamedBenchmarkResult {
                name: self.read_only_algorithm.borrow().entrypoint(),
                benchmark_result: benchmark,
                case: bench_case,
            };
            benchmarks.push(benchmark);
        }

        write_benchmarks(benchmarks);
    }
}
