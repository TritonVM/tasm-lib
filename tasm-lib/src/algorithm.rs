use std::collections::HashMap;

use triton_vm::{BFieldElement, NonDeterminism};

use crate::{snippet::BasicSnippet, snippet_bencher::BenchmarkCase};

/// An Algorithm can modify memory even at addresses below the
/// dynamic memory allocator, and can take nondeterministic input.
pub trait Algorithm: BasicSnippet {
    fn rust_shadow(
        stack: &mut Vec<BFieldElement>,
        memory: &mut HashMap<BFieldElement, BFieldElement>,
        nondeterminism: NonDeterminism<BFieldElement>,
    );

    fn pseudorandom_initial_state(
        seed: [u8; 32],
        bench_case: BenchmarkCase,
    ) -> (
        Vec<BFieldElement>,
        HashMap<BFieldElement, BFieldElement>,
        NonDeterminism<BFieldElement>,
    );
}
