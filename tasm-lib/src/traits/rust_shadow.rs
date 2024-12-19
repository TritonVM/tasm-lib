use std::collections::HashMap;

use triton_vm::prelude::*;

use crate::prelude::*;

pub trait RustShadow {
    fn inner(&self) -> &dyn BasicSnippet;

    fn rust_shadow_wrapper(
        &self,
        stdin: &[BFieldElement],
        nondeterminism: &NonDeterminism,
        stack: &mut Vec<BFieldElement>,
        memory: &mut HashMap<BFieldElement, BFieldElement>,
        sponge: &mut Option<Tip5>,
    ) -> Vec<BFieldElement>;

    fn test(&self);

    fn bench(&self);
}
