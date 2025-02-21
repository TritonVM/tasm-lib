use rand::prelude::*;
use triton_vm::prelude::*;

use super::basic_snippet::BasicSnippet;
use super::rust_shadow::RustShadow;
use crate::linker::execute_bench;
use crate::prelude::Tip5;
use crate::push_encodable;
use crate::snippet_bencher::BenchmarkCase;
use crate::snippet_bencher::NamedBenchmarkResult;
use crate::snippet_bencher::write_benchmarks;
use crate::test_helpers::test_rust_equivalence_given_complete_state;

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
    /// The arguments of this closure.
    ///
    /// Because rust does not support variadic types (yet?), it is recommended to
    /// use a tuple if the closure takes more than one argument. Because of the way
    /// [`BFieldCodec`] works for tuples, specify the element the closure expects on
    /// top of the stack as the last element of the tuple.
    type Args: BFieldCodec;

    fn rust_shadow(&self, stack: &mut Vec<BFieldElement>);

    /// Given a [seed](StdRng::Seed), generate pseudorandom [arguments](Self::Args),
    /// from which an [initial stack](Self::set_up_test_stack) can be constructed.
    fn pseudorandom_args(&self, seed: [u8; 32], bench_case: Option<BenchmarkCase>) -> Self::Args;

    fn corner_case_args(&self) -> Vec<Self::Args> {
        vec![]
    }

    fn set_up_test_stack(&self, args: Self::Args) -> Vec<BFieldElement> {
        let mut stack = self.init_stack_for_isolated_run();
        push_encodable(&mut stack, &args);
        stack
    }
}

pub struct ShadowedClosure<C: Closure> {
    closure: C,
}

impl<C: Closure> ShadowedClosure<C> {
    pub fn new(closure: C) -> Self {
        Self { closure }
    }
}

impl<C: Closure> RustShadow for ShadowedClosure<C> {
    fn inner(&self) -> &dyn BasicSnippet {
        &self.closure
    }

    fn rust_shadow_wrapper(
        &self,
        _stdin: &[BFieldElement],
        _nondeterminism: &NonDeterminism,
        stack: &mut Vec<BFieldElement>,
        _memory: &mut std::collections::HashMap<BFieldElement, BFieldElement>,
        _sponge: &mut Option<Tip5>,
    ) -> Vec<BFieldElement> {
        self.closure.rust_shadow(stack);
        vec![]
    }

    fn test(&self) {
        let num_states = 5;
        let mut rng = rand::rng();

        // First test corner-cases as they're easier to debug on failure
        for args in self.closure.corner_case_args() {
            test_rust_equivalence_given_complete_state(
                self,
                &self.closure.set_up_test_stack(args),
                &[],
                &NonDeterminism::default(),
                &None,
                None,
            );
        }

        for _ in 0..num_states {
            let seed: [u8; 32] = rng.random();
            let args = self.closure.pseudorandom_args(seed, None);
            let stack = self.closure.set_up_test_stack(args);

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
        let mut rng = StdRng::from_seed(
            hex::decode("73a24b6b8b32e4d7d563a4d9a85f476573a24b6b8b32e4d7d563a4d9a85f4765")
                .unwrap()
                .try_into()
                .unwrap(),
        );
        let mut benchmarks = Vec::with_capacity(2);

        for bench_case in [BenchmarkCase::CommonCase, BenchmarkCase::WorstCase] {
            let args = self
                .closure
                .pseudorandom_args(rng.random(), Some(bench_case));
            let stack = self.closure.set_up_test_stack(args);
            let program = self.closure.link_for_isolated_run();
            let benchmark =
                execute_bench(&program, &stack, vec![], NonDeterminism::new(vec![]), None);
            let benchmark = NamedBenchmarkResult {
                name: self.closure.entrypoint(),
                benchmark_result: benchmark,
                case: bench_case,
            };
            benchmarks.push(benchmark);
        }

        write_benchmarks(benchmarks);
    }
}
