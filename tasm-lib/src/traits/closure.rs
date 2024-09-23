use std::cell::RefCell;
use std::rc::Rc;

use rand::prelude::*;
use triton_vm::prelude::*;

use super::basic_snippet::BasicSnippet;
use super::rust_shadow::RustShadow;
use crate::linker::execute_bench;
use crate::linker::link_for_isolated_run;
use crate::snippet_bencher::write_benchmarks;
use crate::snippet_bencher::BenchmarkCase;
use crate::snippet_bencher::NamedBenchmarkResult;
use crate::test_helpers::test_rust_equivalence_given_complete_state;
use crate::VmHasher;

/// A Closure is a piece of tasm code that modifies the top of the stack without access to
/// memory or nondeterminism or standard input/output.
///
/// See also: [function], [algorithm], [read_only_algorithm], [procedure],
///           [accessor], [mem_preserver]
///
/// [function]: crate::traits::function::Function
/// [algorithm]: crate::traits::algorithm::Algorithm
/// [procedure]: crate::traits::procedure::Procedure
/// [accessor]: crate::traits::accessor::Accessor
/// [mem_preserver]: crate::traits::mem_preserver::MemPreserver
/// [read_only_algorithm]: crate::traits::read_only_algorithm::ReadOnlyAlgorithm
pub trait Closure: BasicSnippet {
    fn rust_shadow(&self, stack: &mut Vec<BFieldElement>);

    /// Generate a pseudorandom initial state (in this case, just the opstack) from the
    /// given seed.
    fn pseudorandom_initial_state(
        &self,
        seed: [u8; 32],
        bench_case: Option<BenchmarkCase>,
    ) -> Vec<BFieldElement>;

    fn corner_case_initial_states(&self) -> Vec<Vec<BFieldElement>> {
        vec![]
    }
}

pub struct ShadowedClosure<C: Closure + 'static> {
    pub closure: Rc<RefCell<C>>,
}

impl<C: Closure + 'static> ShadowedClosure<C> {
    pub fn new(closure: C) -> Self {
        Self {
            closure: Rc::new(RefCell::new(closure)),
        }
    }
}

impl<C: Closure + 'static> RustShadow for ShadowedClosure<C> {
    fn inner(&self) -> Rc<RefCell<dyn BasicSnippet>> {
        self.closure.clone()
    }

    fn rust_shadow_wrapper(
        &self,
        _stdin: &[BFieldElement],
        _nondeterminism: &NonDeterminism,
        stack: &mut Vec<BFieldElement>,
        _memory: &mut std::collections::HashMap<BFieldElement, BFieldElement>,
        _sponge: &mut Option<VmHasher>,
    ) -> Vec<BFieldElement> {
        self.closure.borrow().rust_shadow(stack);
        vec![]
    }

    fn test(&self) {
        let num_states = 5;
        let mut rng = thread_rng();

        // First test corner-cases as they're easier to debug on failure
        for init_stack_corner_case in self.closure.borrow().corner_case_initial_states() {
            let stdin = vec![];
            test_rust_equivalence_given_complete_state(
                self,
                &init_stack_corner_case,
                &stdin,
                &NonDeterminism::default(),
                &None,
                None,
            );
        }

        for _ in 0..num_states {
            let seed: [u8; 32] = rng.gen();
            println!(
                "testing {} common case with seed: {:x?}",
                self.closure.borrow().entrypoint(),
                seed
            );
            let stack = self.closure.borrow().pseudorandom_initial_state(seed, None);

            let stdin = vec![];
            test_rust_equivalence_given_complete_state(
                self,
                &stack,
                &stdin,
                &NonDeterminism::default(),
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
            let stack = self
                .closure
                .borrow()
                .pseudorandom_initial_state(rng.gen(), Some(bench_case));
            let program = link_for_isolated_run(self.closure.clone());
            let benchmark =
                execute_bench(&program, &stack, vec![], NonDeterminism::new(vec![]), None);
            let benchmark = NamedBenchmarkResult {
                name: self.closure.borrow().entrypoint(),
                benchmark_result: benchmark,
                case: bench_case,
            };
            benchmarks.push(benchmark);
        }

        write_benchmarks(benchmarks);
    }
}
