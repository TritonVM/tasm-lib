use rand::{rngs::StdRng, Rng, SeedableRng};
use std::{cell::RefCell, collections::HashMap, rc::Rc};
use triton_vm::{BFieldElement, NonDeterminism};
use twenty_first::shared_math::bfield_codec::BFieldCodec;

use super::{basic_snippet::BasicSnippet, rust_shadow::RustShadow};
use crate::{
    linker::{execute_bench, link_for_isolated_run},
    snippet_bencher::{write_benchmarks, BenchmarkCase, BenchmarkResult},
    test_helpers::test_rust_equivalence_given_complete_state,
    VmHasherState,
};

/// An Algorithm is a piece of tasm code that can modify memory even at addresses below
/// the dynamic memory allocator, and can take nondeterministic input. It cannot read from
/// standard in or write to standard out.
///
/// See also: [closure], [function], [procedure]
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
    ) -> (Vec<BFieldElement>, NonDeterminism<BFieldElement>);
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
    fn rust_shadow_wrapper(
        &self,
        _stdin: &[BFieldElement],
        nondeterminism: &NonDeterminism<BFieldElement>,
        stack: &mut Vec<BFieldElement>,
        memory: &mut HashMap<BFieldElement, BFieldElement>,
        _sponge_state: &mut Option<VmHasherState>,
    ) -> Vec<BFieldElement> {
        self.algorithm
            .borrow()
            .rust_shadow(stack, memory, nondeterminism);
        vec![]
    }

    fn test(&self) {
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
            let (stack, nondeterminism) = self
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
                0,
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
            let (stack, nondeterminism) = self
                .algorithm
                .borrow()
                .pseudorandom_initial_state(rng.gen(), Some(bench_case));
            let program = link_for_isolated_run(self.algorithm.clone(), 1);
            let execution_result = execute_bench(&program, &stack, vec![], nondeterminism, None);
            let benchmark = BenchmarkResult {
                name: self.algorithm.borrow().entrypoint(),
                clock_cycle_count: execution_result.cycle_count,
                hash_table_height: execution_result.hash_table_height,
                u32_table_height: execution_result.u32_table_height,
                case: bench_case,
            };
            benchmarks.push(benchmark);
        }

        write_benchmarks(benchmarks);
    }

    fn inner(&self) -> Rc<RefCell<dyn BasicSnippet>> {
        self.algorithm.clone()
    }
}
