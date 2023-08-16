use std::collections::HashMap;

use triton_vm::BFieldElement;

use crate::{
    snippet::{BasicSnippet, RustShadow},
    snippet_bencher::BenchmarkCase,
};

/// A Function can modify the top of the stack, and can read and
/// extend memory. Specifically: any memory writes have to happen
/// to addresses larger than the dynamic memory allocator and the
/// dynamic memory allocator value has to be updated accordingly.
pub trait Function: BasicSnippet {
    fn rust_shadow(
        stack: &mut Vec<BFieldElement>,
        memory: &mut HashMap<BFieldElement, BFieldElement>,
    );

    fn pseudorandom_initial_state(
        &self,
        seed: [u8; 32],
        bench_case: Option<BenchmarkCase>,
    ) -> (Vec<BFieldElement>, HashMap<BFieldElement, BFieldElement>);
}

pub struct ShadowedFunction<F: Function + Clone + 'static> {
    pub function: F,
}

impl<F: Function + Clone + 'static> ShadowedFunction<F> {
    pub fn new(function: F) -> Self {
        Self { function }
    }
}

impl<T> RustShadow for ShadowedFunction<T>
where
    T: Function + Clone + 'static,
{
    fn rust_shadow_wrapper(
        &self,
        stdin: &[BFieldElement],
        nondeterminism: &triton_vm::NonDeterminism<BFieldElement>,
        stack: &mut Vec<BFieldElement>,
        memory: &mut HashMap<BFieldElement, BFieldElement>,
    ) -> Vec<BFieldElement> {
        todo!()
    }

    fn test(&self) {
        todo!()
    }

    fn bench(&self) {
        todo!()
    }

    fn inner(&self) -> Box<dyn BasicSnippet> {
        Box::new(self.function.clone())
    }
}
