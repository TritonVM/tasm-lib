//! List operations, mimicking [`Vec`] in some ways (but not in others).
//!
//! The snippets in this module are only designed for lists that contain
//! elements whose type has [static size](crate::BFieldCodec::static_length).
//! The exception from this rule is [`Map`][self::higher_order::map::Map] (and
//! the more general [`ChainMap`][self::higher_order::map::ChainMap]).
//!
//! Like [`Vec`], a list can be [created][self::new::New],
//! [pushed to][self::push::Push], [popped from][self::pop::Pop],
//! [written to][self::set::Set], [read from][self::get::Get],
//! and asked for its [length][self::length::Length], among other operations.
//!
//! Unlike [`Vec`], lists do not track their capacity. Instead, list created at
//! Triton VM's runtime get access to an entire [memory page][crate::memory].
//! Lists spawned in Triton VM's memory through
//! [non-determinism][crate::triton_vm::prelude::NonDeterminism] might have
//! access to smaller memory regions. As with all non-determinism, handling them
//! requires additional care.

/// The number of VM words required to store the metadata / bookkeeping data of a list.
pub const LIST_METADATA_SIZE: usize = 1;

pub mod contains;
pub mod get;
pub mod higher_order;
pub mod horner_evaluation_dynamic_length;
pub mod length;
pub mod multiset_equality_digests;
pub mod multiset_equality_u64s;
pub mod new;
pub mod pop;
pub mod push;
pub mod range;
pub mod set;
pub mod set_length;
pub mod split_off;
pub mod sum_bfes;
pub mod sum_xfes;
pub mod swap_unchecked;
