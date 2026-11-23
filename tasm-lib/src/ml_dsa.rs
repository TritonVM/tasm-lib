//! ML-DSA-44 (FIPS 204) signature verification.
//!
//! [`verify::MlDsa44Verify`] checks a signature in Triton VM. The Rust
//! functions in [`helper`] prepare what the snippet takes from a prover: the
//! [expanded public key](helper::ExpandedPublicKey), which a program commits
//! to by its digest, and the [signature witness](helper::SignatureWitness).
//!
//! [`MlDsa44Verify`](verify::MlDsa44Verify) is two snippets in turn:
//! [`MlDsa44Mu`](verify::MlDsa44Mu) hashes the key's `tr` and the message to
//! μ, and [`MlDsa44VerifyExternalMu`](verify::MlDsa44VerifyExternalMu) checks
//! the signature on μ. A program that verifies a signature on a message of at
//! most 69 bytes calls `MlDsa44Verify`, and stays below 2^20 rows in every
//! table. For a longer message, one program calls `MlDsa44Mu` and another calls
//! `MlDsa44VerifyExternalMu` on the resulting μ.
//!
//! ### Example
//!
//! A program that verifies a signature `sig` on a message `msg` under a public
//! key `pk` consists of a call to [`MlDsa44Verify`](verify::MlDsa44Verify)
//! with five arguments: pointers to the expanded key, to the witness and to the
//! message, the key digest, and the message length in bytes. The key digest is
//! a constant of the program here, and is pushed so that its element 0 is on
//! top of the stack.
//!
//! The prover puts into non-deterministic memory the
//! [Keccak tables](crate::hashing::keccak::tables::insert_tables), the
//! [high-bits table](verify::insert_high_bits_table), the encoded expanded key,
//! the encoded witness, and the message, of which word `w` holds bytes `4w` to
//! `4w + 3` in little-endian order, zero past the message's end. The pointers
//! here are arbitrary addresses below the two tables.
//!
//! ```
//! use std::collections::HashMap;
//!
//! use fips204::ml_dsa_44;
//! use fips204::traits::KeyGen;
//! use fips204::traits::SerDes;
//! use fips204::traits::Signer;
//! use tasm_lib::hashing::keccak::tables::insert_tables;
//! use tasm_lib::ml_dsa::helper::ExpandedPublicKey;
//! use tasm_lib::ml_dsa::helper::SignatureWitness;
//! use tasm_lib::ml_dsa::verify::MlDsa44Verify;
//! use tasm_lib::ml_dsa::verify::insert_high_bits_table;
//! use tasm_lib::prelude::*;
//! use tasm_lib::triton_vm::prelude::*;
//!
//! let (pk, sk) = ml_dsa_44::KG::keygen_from_seed(&[1; 32]);
//! let msg = b"hello";
//! let sig = sk.try_sign_with_seed(&[2; 32], msg, &[]).unwrap();
//!
//! let key = ExpandedPublicKey::new(&pk.into_bytes());
//! let key_digest = key.digest();
//! let witness = SignatureWitness::new(&key, &sig).expect("sig decodes");
//!
//! let (key_ptr, witness_ptr, msg_ptr) = (1u64 << 20, 1u64 << 21, 1u64 << 22);
//! let [d0, d1, d2, d3, d4] = key_digest.values();
//! let mut library = Library::new();
//! let verify = library.import(Box::new(MlDsa44Verify));
//! let code = triton_asm!(
//!     push {key_ptr}
//!     push {witness_ptr}
//!     push {d4} push {d3} push {d2} push {d1} push {d0}
//!     push {msg_ptr}
//!     push {msg.len()}
//!     call {verify}
//!     halt
//!     {&library.all_imports()}
//! );
//!
//! let mut ram = HashMap::new();
//! insert_tables(&mut ram);
//! insert_high_bits_table(&mut ram);
//! for (i, word) in key.encode().into_iter().enumerate() {
//!     ram.insert(bfe!(key_ptr + i as u64), word);
//! }
//! for (i, word) in witness.encode().into_iter().enumerate() {
//!     ram.insert(bfe!(witness_ptr + i as u64), word);
//! }
//! for (i, chunk) in msg.chunks(4).enumerate() {
//!     let mut bytes = [0; 4];
//!     bytes[..chunk.len()].copy_from_slice(chunk);
//!     ram.insert(bfe!(msg_ptr + i as u64), bfe!(u32::from_le_bytes(bytes)));
//! }
//!
//! let nondeterminism = NonDeterminism::default().with_ram(ram);
//! let mut vm_state = VMState::new(Program::new(&code), PublicInput::default(), nondeterminism);
//! vm_state.run().unwrap();
//! ```

pub mod helper;
pub mod verify;

/// The modulus `q`.
pub const Q: u32 = 8_380_417;

/// The number of coefficients of a polynomial.
pub const N: usize = 256;

/// The number of bits dropped from `t`.
pub const D: u32 = 13;

/// The number of nonzero coefficients of the challenge `c`.
pub const TAU: usize = 39;

/// The length of the commitment hash `c̃` in bytes, `λ/4`.
pub const C_TILDE_LEN: usize = 32;

/// The coefficient range `γ1` of the mask.
pub const GAMMA1: u32 = 1 << 17;

/// The low-order rounding range `γ2`.
pub const GAMMA2: u32 = (Q - 1) / 88;

/// The number of rows `k` of `A`.
pub const K: usize = 4;

/// The number of columns `ℓ` of `A`.
pub const L: usize = 4;

/// `β = τ·η`.
pub const BETA: u32 = 78;

/// The maximal number of ones in the hint `h`.
pub const OMEGA: usize = 80;

/// The length of an encoded public key in bytes.
pub const PUBLIC_KEY_LEN: usize = 1312;

/// The length of an encoded signature in bytes.
pub const SIGNATURE_LEN: usize = 2420;

/// The number of values `w1` takes, `(q-1)/(2·γ2)`.
pub const W1_RANGE: u32 = (Q - 1) / (2 * GAMMA2);
