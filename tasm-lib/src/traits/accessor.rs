use std::collections::HashMap;

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

/// An Accessor can modify that stack but only read from memory
///
/// An Accessor is a piece of tasm code that can read from and write to the
/// stack but can only read from memory. Note that it cannot write to static
/// memory. It cannot read from any inputs or write to standard out. It cannot
/// modify the sponge state.
/// See also: [closure], [function], [procedure], [algorithm],
///           [read_only_algorithm], [mem_preserver]
///
/// [closure]: crate::traits::closure::Closure
/// [function]: crate::traits::function::Function
/// [procedure]: crate::traits::procedure::Procedure
/// [algorithm]: crate::traits::algorithm::Algorithm
/// [read_only_algorithm]: crate::traits::read_only_algorithm::ReadOnlyAlgorithm
/// [mem_preserver]: crate::traits::mem_preserver::MemPreserver
pub trait Accessor: BasicSnippet {
    fn rust_shadow(
        &self,
        stack: &mut Vec<BFieldElement>,
        memory: &HashMap<BFieldElement, BFieldElement>,
    );

    fn pseudorandom_initial_state(
        &self,
        seed: [u8; 32],
        bench_case: Option<BenchmarkCase>,
    ) -> AccessorInitialState;

    fn corner_case_initial_states(&self) -> Vec<AccessorInitialState> {
        vec![]
    }
}

#[derive(Debug, Clone, Default)]
pub struct AccessorInitialState {
    pub stack: Vec<BFieldElement>,
    pub memory: HashMap<BFieldElement, BFieldElement>,
}

impl From<AccessorInitialState> for InitVmState {
    fn from(value: AccessorInitialState) -> Self {
        let nd = NonDeterminism::default().with_ram(value.memory);
        Self {
            stack: value.stack,
            nondeterminism: nd,
            ..Default::default()
        }
    }
}

pub struct ShadowedAccessor<T: Accessor> {
    accessor: T,
}

impl<T: Accessor> ShadowedAccessor<T> {
    pub fn new(accessor: T) -> Self {
        Self { accessor }
    }
}

impl<T> RustShadow for ShadowedAccessor<T>
where
    T: Accessor,
{
    fn inner(&self) -> &dyn BasicSnippet {
        &self.accessor
    }

    fn rust_shadow_wrapper(
        &self,
        _stdin: &[BFieldElement],
        _nondeterminism: &NonDeterminism,
        stack: &mut Vec<BFieldElement>,
        memory: &mut HashMap<BFieldElement, BFieldElement>,
        _sponge: &mut Option<Tip5>,
    ) -> Vec<BFieldElement> {
        self.accessor.rust_shadow(stack, memory);
        vec![]
    }

    fn test(&self) {
        for corner_case in self.accessor.corner_case_initial_states() {
            let stdin = vec![];
            let nd = NonDeterminism::default().with_ram(corner_case.memory);
            test_rust_equivalence_given_complete_state(
                self,
                &corner_case.stack,
                &stdin,
                &nd,
                &None,
                None,
            );
        }

        let num_states = 10;
        let mut rng = StdRng::from_seed(rand::random());
        for _ in 0..num_states {
            let AccessorInitialState { stack, memory } =
                self.accessor.pseudorandom_initial_state(rng.random(), None);

            let stdin = vec![];
            let nd = NonDeterminism::default().with_ram(memory);
            test_rust_equivalence_given_complete_state(self, &stack, &stdin, &nd, &None, None);
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
            let AccessorInitialState { stack, memory } = self
                .accessor
                .pseudorandom_initial_state(rng.random(), Some(bench_case));
            let program = self.accessor.link_for_isolated_run();
            let nd = NonDeterminism::default().with_ram(memory);
            let benchmark = execute_bench(&program, &stack, vec![], nd, None);
            let benchmark = NamedBenchmarkResult {
                name: self.accessor.entrypoint(),
                benchmark_result: benchmark,
                case: bench_case,
            };
            benchmarks.push(benchmark);
        }

        write_benchmarks(benchmarks);
    }
}
