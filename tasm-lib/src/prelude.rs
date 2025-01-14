//! Re-exports the most commonly-needed APIs of TASM Lib.
//!
//! This module is intended to be wildcard-imported, _i.e._, `use tasm_lib::prelude::*;`.
//! You might also want to consider wildcard-importing the prelude of Triton VM, _i.e._,
//! `use tasm_lib::triton_vm::prelude::*;`.

pub use triton_vm;
pub use triton_vm::twenty_first;

pub use crate::data_type::DataType;
pub use crate::library::Library;
pub use crate::memory::dyn_malloc::DynMalloc;
pub use crate::memory::memcpy::MemCpy;
pub use crate::structure::tasm_object::TasmObject;
pub use crate::structure::tasm_object::TasmStruct;
pub use crate::traits::basic_snippet::BasicSnippet;
pub use crate::triton_vm::twenty_first::prelude::Digest;
pub use crate::triton_vm::twenty_first::prelude::Tip5;
