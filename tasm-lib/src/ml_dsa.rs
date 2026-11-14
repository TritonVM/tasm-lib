//! ML-DSA-44 (FIPS 204) signature verification.
//!
//! [`verify::MlDsa44Verify`] checks a signature in Triton VM. The Rust
//! functions in [`reference`](mod@reference) implement the standard, against which the
//! snippet is tested, and prepare what the snippet takes from a prover: the
//! [expanded public key](reference::ExpandedPublicKey), which a program
//! commits to by its digest, and the
//! [signature witness](reference::SignatureWitness).

pub mod reference;
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
