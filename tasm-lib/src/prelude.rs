//! Re-exports the most commonly-needed APIs of TASM Lib.
//!
//! This module is intended to be wildcard-imported, _i.e._, `use tasm_lib::prelude::*;`.
//! You might also want to consider wildcard-importing the prelude of Triton VM, _i.e._,
//! `use tasm_lib::triton_vm::prelude::*;`.

pub use triton_vm;
pub use triton_vm::prelude::twenty_first;

pub use crate::library::Library;
pub use crate::memory::dyn_malloc::DynMalloc;
pub use crate::memory::memcpy::MemCpy;
pub use crate::structure::tasm_object::TasmObject;
pub use crate::traits::basic_snippet::BasicSnippet;
pub use crate::Digest;
