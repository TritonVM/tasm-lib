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
use crate::test_helpers::test_rust_equivalence_given_complete_state;
use crate::InitVmState;
use crate::VmHasher;

use super::basic_snippet::BasicSnippet;
use super::rust_shadow::RustShadow;

/// An Accessor can modify that stack but only read from memory
///
/// An Accessor is a piece of tasm code that can read from and write to the
/// stack but can only read from memory. Note that it cannot write to static
/// memory. It cannot read from any inputs or write to standard out. It cannot
/// modify the sponge state.
/// See also: [closure], [function], [procedure], [algorithm]
///
/// [closure]: crate::traits::closure::Closure
/// [function]: crate::traits::function::Function
/// [procedure]: crate::traits::procedure::Procedure
/// [algorithm]: crate::traits::algorithm::Algorithm
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

pub struct ShadowedAccessor<T: Accessor + 'static> {
    accessor: Rc<RefCell<T>>,
}

impl<T: Accessor + 'static> ShadowedAccessor<T> {
    pub fn new(algorithm: T) -> Self {
        Self {
            accessor: Rc::new(RefCell::new(algorithm)),
        }
    }
}

impl<T> RustShadow for ShadowedAccessor<T>
where
    T: Accessor + 'static,
{
    fn inner(&self) -> Rc<RefCell<dyn BasicSnippet>> {
        self.accessor.clone()
    }

    fn rust_shadow_wrapper(
        &self,
        _stdin: &[BFieldElement],
        _nondeterminism: &NonDeterminism,
        stack: &mut Vec<BFieldElement>,
        memory: &mut HashMap<BFieldElement, BFieldElement>,
        _sponge: &mut Option<VmHasher>,
    ) -> Vec<BFieldElement> {
        self.accessor.borrow().rust_shadow(stack, memory);
        vec![]
    }

    fn test(&self) {
        for (i, corner_case) in self
            .accessor
            .borrow()
            .corner_case_initial_states()
            .into_iter()
            .enumerate()
        {
            println!(
                "testing {} corner case number {i}",
                self.accessor.borrow().entrypoint(),
            );

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
        let seed: [u8; 32] = thread_rng().gen();
        let mut rng: StdRng = SeedableRng::from_seed(seed);
        for _ in 0..num_states {
            let seed: [u8; 32] = rng.gen();
            println!(
                "testing {} common case with seed: {:#4x?}",
                self.accessor.borrow().entrypoint(),
                seed
            );
            let AccessorInitialState { stack, memory } = self
                .accessor
                .borrow()
                .pseudorandom_initial_state(seed, None);

            let stdin = vec![];
            let nd = NonDeterminism::default().with_ram(memory);
            test_rust_equivalence_given_complete_state(self, &stack, &stdin, &nd, &None, None);
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
            let AccessorInitialState { stack, memory } = self
                .accessor
                .borrow()
                .pseudorandom_initial_state(rng.gen(), Some(bench_case));
            let program = link_for_isolated_run(self.accessor.clone());
            let nd = NonDeterminism::default().with_ram(memory);
            let benchmark = execute_bench(&program, &stack, vec![], nd, None);
            let benchmark = NamedBenchmarkResult {
                name: self.accessor.borrow().entrypoint(),
                benchmark_result: benchmark,
                case: bench_case,
            };
            benchmarks.push(benchmark);
        }

        write_benchmarks(benchmarks);
    }
}