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

/// An Algorithm can modify memory and take ND_input.
///
/// An Algorithm is a piece of tasm code that can modify memory even at addresses below
/// the dynamic memory allocator, and can take nondeterministic input. It cannot read from
/// standard in or write to standard out.
///
/// See also: [closure], [function], [procedure], [accessor], [mem_preserver],
///           [read_only_algorithm]
///
/// [closure]: crate::traits::closure::Closure
/// [function]: crate::traits::function::Function
/// [procedure]: crate::traits::procedure::Procedure
/// [accessor]: crate::traits::accessor::Accessor
/// [mem_preserver]: crate::traits::mem_preserver::MemPreserver
/// [read_only_algorithm]: crate::traits::read_only_algorithm::ReadOnlyAlgorithm
pub trait Algorithm: BasicSnippet {
    fn rust_shadow(
        &self,
        stack: &mut Vec<BFieldElement>,
        memory: &mut HashMap<BFieldElement, BFieldElement>,
        nondeterminism: &NonDeterminism,
    );

    /// Take a object about which something is being proven in order to extract out the
    /// right nondeterminism. Update the mutably referenced non-determism argument.
    ///
    /// For example:
    ///  - When proving the correct verification of a proof, you might want to pull all
    ///    digests out of the authentication structures and put them in the `digests`
    ///    field of the non-determinism. This way the VM can avoid the processing
    ///    required by authentication structures and just divine in the right digests
    ///    as it walks up the Merkle trees.
    ///  - When proving the correct sorting of a list, the VM ought to avoid running a
    ///    sorting algorithm; instead it should divine the sorted list and then prove
    ///    that the two lists are equal. The preprocessing step in this case would take
    ///    the unsorted list, sort it, and use the sorted list to populate the non-
    ///    determinism.
    ///  - When verifying a Falcon signature, at some point the NTT of a vector is needed.
    ///    The VM should not compute the NTT because expensive; instead it should divine
    ///    the NTT-transformed vector and verify that it satisfies the right relation. In
    ///    this case the preprocessor calculates the NTT and populates the non-determinism
    ///    with the transformed vector.
    fn preprocess<T: BFieldCodec>(_meta_input: T, _nondeterminism: &mut NonDeterminism) {}

    fn pseudorandom_initial_state(
        &self,
        seed: [u8; 32],
        bench_case: Option<BenchmarkCase>,
    ) -> AlgorithmInitialState;

    fn corner_case_initial_states(&self) -> Vec<AlgorithmInitialState> {
        vec![]
    }
}

#[derive(Debug, Clone, Default)]
pub struct AlgorithmInitialState {
    pub stack: Vec<BFieldElement>,
    pub nondeterminism: NonDeterminism,
}

impl From<AlgorithmInitialState> for InitVmState {
    fn from(value: AlgorithmInitialState) -> Self {
        Self {
            stack: value.stack,
            nondeterminism: value.nondeterminism,
            ..Default::default()
        }
    }
}

pub struct ShadowedAlgorithm<T: Algorithm> {
    algorithm: T,
}

impl<T: Algorithm> ShadowedAlgorithm<T> {
    pub fn new(algorithm: T) -> Self {
        Self { algorithm }
    }
}

impl<T> RustShadow for ShadowedAlgorithm<T>
where
    T: Algorithm,
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
        self.algorithm.rust_shadow(stack, memory, nondeterminism);
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
        let seed: [u8; 32] = rand::rng().random();
        let mut rng = StdRng::from_seed(seed);
        for _ in 0..num_states {
            let seed: [u8; 32] = rng.random();
            let AlgorithmInitialState {
                stack,
                nondeterminism,
            } = self.algorithm.pseudorandom_initial_state(seed, None);

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
            let AlgorithmInitialState {
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
