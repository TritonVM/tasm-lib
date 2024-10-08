use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use triton_vm::prelude::*;

use super::basic_snippet::BasicSnippet;
use crate::VmHasher;

pub trait RustShadow {
    fn inner(&self) -> Rc<RefCell<dyn BasicSnippet>>;

    fn rust_shadow_wrapper(
        &self,
        stdin: &[BFieldElement],
        nondeterminism: &NonDeterminism,
        stack: &mut Vec<BFieldElement>,
        memory: &mut HashMap<BFieldElement, BFieldElement>,
        sponge: &mut Option<VmHasher>,
    ) -> Vec<BFieldElement>;

    fn test(&self);

    fn bench(&self);
}
