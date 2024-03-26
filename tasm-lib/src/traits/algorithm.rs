use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use rand::rngs::StdRng;
use rand::Rng;
use rand::SeedableRng;
use triton_vm::prelude::*;

use crate::linker::execute_bench;
use crate::linker::link_for_isolated_run;
use crate::snippet_bencher::write_benchmarks;
use crate::snippet_bencher::BenchmarkCase;
use crate::snippet_bencher::NamedBenchmarkResult;
use crate::test_helpers::test_rust_equivalence_given_complete_state;
use crate::VmHasher;

use super::basic_snippet::BasicSnippet;
use super::rust_shadow::RustShadow;

/// An Algorithm is a piece of tasm code that can modify memory even at addresses below
/// the dynamic memory allocator, and can take nondeterministic input. It cannot read from
/// standard in or write to standard out.
///
/// See also: [closure], [function], [procedure]
///
/// [closure]: crate::traits::closure::Closure
/// [function]: crate::traits::function::Function
/// [procedure]: crate::traits::procedure::Procedure
pub trait Algorithm: BasicSnippet {
    fn rust_shadow(
        &self,
        stack: &mut Vec<BFieldElement>,
        memory: &mut HashMap<BFieldElement, BFieldElement>,
        nondeterminism: &NonDeterminism<BFieldElement>,
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
    fn preprocess<T: BFieldCodec>(
        _meta_input: T,
        _nondeterminism: &mut NonDeterminism<BFieldElement>,
    ) {
    }

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
    pub nondeterminism: NonDeterminism<BFieldElement>,
}

pub struct ShadowedAlgorithm<T: Algorithm + 'static> {
    algorithm: Rc<RefCell<T>>,
}

impl<T: Algorithm + 'static> ShadowedAlgorithm<T> {
    pub fn new(algorithm: T) -> Self {
        Self {
            algorithm: Rc::new(RefCell::new(algorithm)),
        }
    }
}

impl<T> RustShadow for ShadowedAlgorithm<T>
where
    T: Algorithm + 'static,
{
    fn inner(&self) -> Rc<RefCell<dyn BasicSnippet>> {
        self.algorithm.clone()
    }

    fn rust_shadow_wrapper(
        &self,
        _stdin: &[BFieldElement],
        nondeterminism: &NonDeterminism<BFieldElement>,
        stack: &mut Vec<BFieldElement>,
        memory: &mut HashMap<BFieldElement, BFieldElement>,
        _sponge: &mut Option<VmHasher>,
    ) -> Vec<BFieldElement> {
        self.algorithm
            .borrow()
            .rust_shadow(stack, memory, nondeterminism);
        vec![]
    }

    fn test(&self) {
        for (i, corner_case) in self
            .algorithm
            .borrow()
            .corner_case_initial_states()
            .into_iter()
            .enumerate()
        {
            println!(
                "testing {} corner case number {i}",
                self.algorithm.borrow().entrypoint(),
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
        let seed = [
            0x0b, 0x6f, 0x89, 0x60, 0xe3, 0x41, 0xa4, 0x36, 0x6c, 0xba, 0x34, 0x53, 0x36, 0x2e,
            0x07, 0xff, 0x18, 0x34, 0x4a, 0xbf, 0x54, 0x10, 0x40, 0x0e, 0x28, 0xff, 0x66, 0xea,
            0xc3, 0x33, 0x37, 0x9b,
        ];
        let mut rng: StdRng = SeedableRng::from_seed(seed);
        for _ in 0..num_states {
            let seed: [u8; 32] = rng.gen();
            println!(
                "testing {} common case with seed: {:#4x?}",
                self.algorithm.borrow().entrypoint(),
                seed
            );
            let AlgorithmInitialState {
                stack,
                nondeterminism,
            } = self
                .algorithm
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
            let AlgorithmInitialState {
                stack,
                nondeterminism,
            } = self
                .algorithm
                .borrow()
                .pseudorandom_initial_state(rng.gen(), Some(bench_case));
            let program = link_for_isolated_run(self.algorithm.clone());
            let benchmark = execute_bench(&program, &stack, vec![], nondeterminism, None);
            let benchmark = NamedBenchmarkResult {
                name: self.algorithm.borrow().entrypoint(),
                benchmark_result: benchmark,
                case: bench_case,
            };
            benchmarks.push(benchmark);
        }

        write_benchmarks(benchmarks);
    }
}
