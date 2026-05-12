use std::collections::HashMap;
use strum::Display;
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
    ) -> Result<Vec<BFieldElement>, RustShadowError>;

    fn test(&self);

    fn bench(&self);
}

/// Errors that can occur during the execution of the [Rust shadow](RustShadow)
/// implementation of a snippet.
#[derive(Debug, Display, Copy, Clone, Eq, PartialEq)]
pub enum RustShadowError {
    ArithmeticOverflow,
    DecodingError,
    InvalidProof,
    SpongeUninitialized,
    StackUnderflow,
    U64ToU32Error,
    U64ToUsizeError,
    UsizeToU32Error,
    VmError,

    /// Mimics a Triton VM [AssertionError](isa::instruction::AssertionError).
    ///
    /// The payload can be used to check error ID equivalence.
    AssertionError(i128),

    /// An unspecified issue.
    Other,
}

impl std::error::Error for RustShadowError {}
