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

/// A function can modify stack and extend memory.
///
/// A Function is a piece of tasm code that can modify the top of the stack, and can read
/// and even extend memory. Specifically: any memory writes have to happen to addresses
/// larger than the dynamic memory allocator and the dynamic memory allocator value has to
/// be updated accordingly.
///
/// See also: [closure], [algorithm], [read_only_algorithm], [procedure],
///           [accessor], [mem_preserver],
///
///
/// [closure]: crate::traits::closure::Closure
/// [algorithm]: crate::traits::algorithm::Algorithm
/// [read_only_algorithm]: crate::traits::read_only_algorithm::ReadOnlyAlgorithm
/// [procedure]: crate::traits::procedure::Procedure
/// [accessor]: crate::traits::accessor::Accessor
/// [mem_preserver]: crate::traits::mem_preserver::MemPreserver
pub trait Function: BasicSnippet {
    fn rust_shadow(
        &self,
        stack: &mut Vec<BFieldElement>,
        memory: &mut HashMap<BFieldElement, BFieldElement>,
    );

    /// Return (init_stack, init_memory)
    fn pseudorandom_initial_state(
        &self,
        seed: [u8; 32],
        bench_case: Option<BenchmarkCase>,
    ) -> FunctionInitialState;

    fn corner_case_initial_states(&self) -> Vec<FunctionInitialState> {
        vec![]
    }
}

#[derive(Debug, Clone, Default)]
pub struct FunctionInitialState {
    pub stack: Vec<BFieldElement>,
    pub memory: HashMap<BFieldElement, BFieldElement>,
}

impl From<FunctionInitialState> for InitVmState {
    fn from(value: FunctionInitialState) -> Self {
        let nd = NonDeterminism::default().with_ram(value.memory);
        Self {
            stack: value.stack,
            nondeterminism: nd,
            ..Default::default()
        }
    }
}

pub struct ShadowedFunction<F: Function> {
    function: F,
}

impl<F: Function> ShadowedFunction<F> {
    pub fn new(function: F) -> Self {
        Self { function }
    }
}

impl<P: Function> ShadowedFunction<P> {
    fn test_initial_state(&self, state: FunctionInitialState) {
        let FunctionInitialState { stack, memory } = state;

        let stdin = vec![];
        let non_determinism = NonDeterminism {
            individual_tokens: vec![],
            digests: vec![],
            ram: memory,
        };
        test_rust_equivalence_given_complete_state(
            self,
            &stack,
            &stdin,
            &non_determinism,
            &None,
            None,
        );
    }
}

impl<F> RustShadow for ShadowedFunction<F>
where
    F: Function,
{
    fn inner(&self) -> &dyn BasicSnippet {
        &self.function
    }

    fn rust_shadow_wrapper(
        &self,
        _stdin: &[BFieldElement],
        _nondeterminism: &NonDeterminism,
        stack: &mut Vec<BFieldElement>,
        memory: &mut HashMap<BFieldElement, BFieldElement>,
        _sponge: &mut Option<Tip5>,
    ) -> Vec<BFieldElement> {
        self.function.rust_shadow(stack, memory);
        vec![]
    }

    /// Test rust-tasm equivalence.
    fn test(&self) {
        for cornercase_state in self.function.corner_case_initial_states() {
            self.test_initial_state(cornercase_state);
        }

        let num_rng_states = 5;
        let mut rng = rand::rng();

        for _ in 0..num_rng_states {
            let initial_state = self.function.pseudorandom_initial_state(rng.random(), None);
            self.test_initial_state(initial_state)
        }
    }

    /// Count number of cycles and other performance indicators and save them in directory
    /// benchmarks/.
    fn bench(&self) {
        let seed = hex::decode("73a24b6b8b32e4d7d563a4d9a85f476573a24b6b8b32e4d7d563a4d9a85f4765")
            .unwrap()
            .try_into()
            .unwrap();
        let mut rng = StdRng::from_seed(seed);
        let mut benchmarks = Vec::with_capacity(2);

        for bench_case in [BenchmarkCase::CommonCase, BenchmarkCase::WorstCase] {
            let FunctionInitialState { stack, memory } = self
                .function
                .pseudorandom_initial_state(rng.random(), Some(bench_case));
            let program = self.function.link_for_isolated_run();
            let non_determinism = NonDeterminism::default().with_ram(memory);
            let benchmark = execute_bench(&program, &stack, vec![], non_determinism, None);
            let benchmark = NamedBenchmarkResult {
                name: self.function.entrypoint(),
                benchmark_result: benchmark,
                case: bench_case,
            };
            benchmarks.push(benchmark);
        }

        write_benchmarks(benchmarks);
    }
}
