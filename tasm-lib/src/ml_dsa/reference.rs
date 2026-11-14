//! ML-DSA-44 in Rust: the verification algorithm of FIPS 204, and the
//! preparation of what [`MlDsa44Verify`](super::verify::MlDsa44Verify) takes
//! from a prover.

use triton_vm::prelude::*;
use twenty_first::prelude::Sponge;

use super::*;
use crate::hashing::keccak::Shake;
use crate::hashing::keccak::interleave;
use crate::hashing::keccak::shake256;

/// A polynomial with coefficients in `[0, q)`.
pub type Poly = [u32; N];

/// `ζ = 1753`, a primitive 512-th root of unity modulo `q`.
const ZETA: u64 = 1753;

/// `256⁻¹ mod q`.
const N_INVERSE: u64 = 8_347_681;

fn reduce(x: i64) -> u32 {
    x.rem_euclid(i64::from(Q)) as u32
}

fn pow_mod(base: u64, mut exp: u64) -> u64 {
    let (mut result, mut base) = (1u64, base % u64::from(Q));
    while exp > 0 {
        if exp & 1 == 1 {
            result = result * base % u64::from(Q);
        }
        base = base * base % u64::from(Q);
        exp >>= 1;
    }
    result
}

fn zetas() -> [u64; N] {
    std::array::from_fn(|m| pow_mod(ZETA, u64::from((m as u8).reverse_bits())))
}

/// The number-theoretic transform, FIPS 204 Algorithm 41.
pub fn ntt(w: &Poly) -> Poly {
    let zetas = zetas();
    let mut w = w.map(u64::from);
    let (mut m, mut len) = (0, 128);
    while len >= 1 {
        let mut start = 0;
        while start < N {
            m += 1;
            let z = zetas[m];
            for j in start..start + len {
                let t = z * w[j + len] % u64::from(Q);
                w[j + len] = (w[j] + u64::from(Q) - t) % u64::from(Q);
                w[j] = (w[j] + t) % u64::from(Q);
            }
            start += 2 * len;
        }
        len /= 2;
    }
    w.map(|c| c as u32)
}

/// The inverse transform, FIPS 204 Algorithm 42.
pub fn intt(w: &Poly) -> Poly {
    let zetas = zetas();
    let mut w = w.map(u64::from);
    let (mut m, mut len) = (N, 1);
    while len < N {
        let mut start = 0;
        while start < N {
            m -= 1;
            let z = u64::from(Q) - zetas[m];
            for j in start..start + len {
                let t = w[j];
                w[j] = (t + w[j + len]) % u64::from(Q);
                w[j + len] = (t + u64::from(Q) - w[j + len]) % u64::from(Q);
                w[j + len] = z * w[j + len] % u64::from(Q);
            }
            start += 2 * len;
        }
        len *= 2;
    }
    w.map(|c| (c * N_INVERSE % u64::from(Q)) as u32)
}

fn bit(bytes: &[u8], i: usize) -> u32 {
    u32::from((bytes[i / 8] >> (i % 8)) & 1)
}

fn unpack(bytes: &[u8], bits: usize) -> [u32; N] {
    std::array::from_fn(|i| (0..bits).map(|b| bit(bytes, bits * i + b) << b).sum())
}

/// `(ρ, t1)`, FIPS 204 Algorithm 23.
pub fn decode_public_key(pk: &[u8; PUBLIC_KEY_LEN]) -> ([u8; 32], [Poly; K]) {
    let rho = pk[..32].try_into().unwrap();
    let t1 = std::array::from_fn(|i| unpack(&pk[32 + 320 * i..32 + 320 * (i + 1)], 10));
    (rho, t1)
}

/// The decoded parts of a signature.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DecodedSignature {
    pub c_tilde: [u8; C_TILDE_LEN],
    /// Coefficients in `(-γ1, γ1]`.
    pub z: [[i32; N]; L],
    /// The positions `256·i + j` of the ones of `h`, in increasing order.
    pub hints: Vec<u16>,
}

/// `(c̃, z, h)`, or `None` if the hint is malformed; FIPS 204 Algorithm 27.
pub fn decode_signature(sig: &[u8; SIGNATURE_LEN]) -> Option<DecodedSignature> {
    let c_tilde = sig[..C_TILDE_LEN].try_into().unwrap();
    let z = std::array::from_fn(|i| {
        let start = C_TILDE_LEN + 576 * i;
        unpack(&sig[start..start + 576], 18).map(|v| GAMMA1 as i32 - v as i32)
    });
    let y = &sig[C_TILDE_LEN + 576 * L..];
    let mut hints = vec![];
    let mut index = 0;
    for i in 0..K {
        let end = usize::from(y[OMEGA + i]);
        if end < index || end > OMEGA {
            return None;
        }
        let first = index;
        while index < end {
            if index > first && y[index - 1] >= y[index] {
                return None;
            }
            hints.push((256 * i + usize::from(y[index])) as u16);
            index += 1;
        }
    }
    if y[index..OMEGA].iter().any(|&b| b != 0) {
        return None;
    }
    Some(DecodedSignature { c_tilde, z, hints })
}

/// `Â`, in the NTT domain; FIPS 204 Algorithm 32.
pub fn expand_a(rho: &[u8; 32]) -> [[Poly; L]; K] {
    std::array::from_fn(|r| {
        std::array::from_fn(|s| {
            let mut shake = Shake::shake128();
            shake.absorb(rho);
            shake.absorb(&[s as u8, r as u8]);
            let mut poly = [0; N];
            let mut j = 0;
            while j < N {
                let b = shake.squeeze(3);
                let coefficient =
                    (u32::from(b[2] & 0x7f) << 16) | (u32::from(b[1]) << 8) | u32::from(b[0]);
                if coefficient < Q {
                    poly[j] = coefficient;
                    j += 1;
                }
            }
            poly
        })
    })
}

/// The challenge `c`, with coefficients in `{-1, 0, 1}`; FIPS 204 Algorithm 29.
pub fn sample_in_ball(c_tilde: &[u8; C_TILDE_LEN]) -> [i32; N] {
    let mut shake = Shake::shake256();
    shake.absorb(c_tilde);
    let signs = shake.squeeze(8);
    let mut c = [0; N];
    for i in N - TAU..N {
        let mut j = usize::from(shake.squeeze(1)[0]);
        while j > i {
            j = usize::from(shake.squeeze(1)[0]);
        }
        c[i] = c[j];
        c[j] = 1 - 2 * bit(&signs, i + TAU - N) as i32;
    }
    c
}

/// `(r1, r0)` with `r0` in `(-γ2, γ2]`; FIPS 204 Algorithm 36.
pub fn decompose(r: u32) -> (u32, i32) {
    let mut r0 = (r % (2 * GAMMA2)) as i32;
    if r0 > GAMMA2 as i32 {
        r0 -= 2 * GAMMA2 as i32;
    }
    if r as i32 - r0 == Q as i32 - 1 {
        (0, r0 - 1)
    } else {
        ((r as i32 - r0) as u32 / (2 * GAMMA2), r0)
    }
}

/// FIPS 204 Algorithm 40.
pub fn use_hint(hint: bool, r: u32) -> u32 {
    let (r1, r0) = decompose(r);
    match (hint, r0 > 0) {
        (false, _) => r1,
        (true, true) => (r1 + 1) % W1_RANGE,
        (true, false) => (r1 + W1_RANGE - 1) % W1_RANGE,
    }
}

/// FIPS 204 Algorithm 28: six bits per coefficient.
pub fn w1_encode(w1: &[Poly; K]) -> Vec<u8> {
    let mut bytes = vec![0u8; K * N * 6 / 8];
    for (i, c) in w1.iter().flatten().enumerate() {
        for b in 0..6 {
            let pos = 6 * i + b;
            bytes[pos / 8] |= (((c >> b) & 1) as u8) << (pos % 8);
        }
    }
    bytes
}

/// The 40 bytes of a digest, each element little-endian.
pub fn digest_bytes(digest: Digest) -> [u8; 40] {
    let mut bytes = [0u8; 40];
    for (i, element) in digest.values().iter().enumerate() {
        bytes[8 * i..8 * i + 8].copy_from_slice(&element.value().to_le_bytes());
    }
    bytes
}

/// ML-DSA.Verify, FIPS 204 Algorithm 3 with Algorithm 8.
pub fn verify(
    pk: &[u8; PUBLIC_KEY_LEN],
    message: &[u8],
    sig: &[u8; SIGNATURE_LEN],
    ctx: &[u8],
) -> bool {
    if ctx.len() > 255 {
        return false;
    }
    let Some(signature) = decode_signature(sig) else {
        return false;
    };
    let (rho, t1) = decode_public_key(pk);
    let a_hat = expand_a(&rho);
    let tr = shake256(pk, 64);
    let mut shake = Shake::shake256();
    shake.absorb(&tr);
    shake.absorb(&[0, ctx.len() as u8]);
    shake.absorb(ctx);
    shake.absorb(message);
    let mu = shake.squeeze(64);

    let c_hat = ntt(&sample_in_ball(&signature.c_tilde).map(|c| reduce(c.into())));
    let z_hat = signature.z.map(|z| ntt(&z.map(|c| reduce(c.into()))));
    let mut w1 = [[0; N]; K];
    for i in 0..K {
        let t1_hat = ntt(&t1[i].map(|c| c << D));
        let mut w_hat = [0u32; N];
        for (k, w) in w_hat.iter_mut().enumerate() {
            let az: u64 = (0..L)
                .map(|j| u64::from(a_hat[i][j][k]) * u64::from(z_hat[j][k]) % u64::from(Q))
                .sum();
            let ct = u64::from(c_hat[k]) * u64::from(t1_hat[k]) % u64::from(Q);
            *w = reduce(az as i64 - ct as i64);
        }
        let w_approx = intt(&w_hat);
        for k in 0..N {
            let hint = signature.hints.contains(&((256 * i + k) as u16));
            w1[i][k] = use_hint(hint, w_approx[k]);
        }
    }
    let mut shake = Shake::shake256();
    shake.absorb(&mu);
    shake.absorb(&w1_encode(&w1));
    let c_tilde = shake.squeeze(C_TILDE_LEN);

    let z_in_range = signature
        .z
        .iter()
        .flatten()
        .all(|z| z.unsigned_abs() < GAMMA1 - BETA);
    z_in_range && c_tilde == signature.c_tilde
}

/// Verifies a signature on a digest, encoded as by [`digest_bytes`], with an
/// empty context string, as [`MlDsa44Verify`](super::verify::MlDsa44Verify)
/// does.
pub fn verify_digest(
    pk: &[u8; PUBLIC_KEY_LEN],
    message: Digest,
    sig: &[u8; SIGNATURE_LEN],
) -> bool {
    verify(pk, &digest_bytes(message), sig, &[])
}

fn sponge_digest(words: &[BFieldElement]) -> Digest {
    assert_eq!(0, words.len() % tip5::RATE);
    let mut sponge = Tip5::init();
    for chunk in words.chunks(tip5::RATE) {
        sponge.absorb(chunk.try_into().unwrap());
    }
    Digest::new(sponge.squeeze()[..Digest::LEN].try_into().unwrap())
}

/// What a program commits to instead of the public key: everything that
/// verification derives from the public key alone.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ExpandedPublicKey {
    /// `tr = H(pk, 64)`, as eight lanes of eight slices each.
    pub tr: [[u8; 8]; 8],
    /// `A = NTT⁻¹(Â)`, coefficients in `[0, q)`.
    pub a: [[Poly; L]; K],
    /// `t1·2^d`.
    pub t1_shifted: [Poly; K],
}

impl ExpandedPublicKey {
    pub const TR_OFFSET: usize = 0;
    pub const A_OFFSET: usize = 64;
    pub const T1_OFFSET: usize = Self::A_OFFSET + K * L * N;
    /// The number of words, a multiple of the sponge's rate.
    pub const NUM_WORDS: usize = (Self::T1_OFFSET + K * N).div_ceil(tip5::RATE) * tip5::RATE;

    pub fn new(pk: &[u8; PUBLIC_KEY_LEN]) -> Self {
        let (rho, t1) = decode_public_key(pk);
        let tr_bytes = shake256(pk, 64);
        let tr = std::array::from_fn(|lane| {
            interleave(u64::from_le_bytes(
                tr_bytes[8 * lane..8 * lane + 8].try_into().unwrap(),
            ))
        });
        let a = expand_a(&rho).map(|row| row.map(|a_hat| intt(&a_hat)));
        let t1_shifted = t1.map(|t| t.map(|c| c << D));
        Self { tr, a, t1_shifted }
    }

    /// The words that a prover puts into memory.
    pub fn encode(&self) -> Vec<BFieldElement> {
        let mut words = vec![bfe!(0); Self::NUM_WORDS];
        for (lane, slices) in self.tr.iter().enumerate() {
            for (j, slice) in slices.iter().enumerate() {
                words[Self::TR_OFFSET + 8 * lane + j] = bfe!(*slice);
            }
        }
        for (k, c) in self.a.iter().flatten().flatten().enumerate() {
            words[Self::A_OFFSET + k] = bfe!(*c);
        }
        for (k, c) in self.t1_shifted.iter().flatten().enumerate() {
            words[Self::T1_OFFSET + k] = bfe!(*c);
        }
        words
    }

    /// The digest by which a program commits to the public key.
    pub fn digest(&self) -> Digest {
        sponge_digest(&self.encode())
    }
}

/// A signature, and the values with which
/// [`MlDsa44Verify`](super::verify::MlDsa44Verify) checks it without
/// computing NTTs.
///
/// For each row `i`, the integer polynomial `P_i = Σ_j A_ij·z_j − c·t1_i·2^d`
/// equals `(X^256 + 1)·S_i + q·Q_i + w_i`, where `w_i` is `w'_Approx` of the
/// standard, with coefficients in `[0, q)`. Every coefficient `r` of `w_i`
/// equals `2·γ2·e + r0` with `r0` in `(-γ2, γ2]` and `e` in `[0, 44]`.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SignatureWitness {
    /// `c̃`, as four lanes of eight slices each.
    pub c_tilde: [[u8; 8]; 4],
    pub z: [[i32; N]; L],
    pub w: [Poly; K],
    pub s: [[i64; N - 1]; K],
    pub q: [[i64; N]; K],
    pub e: [[u32; N]; K],
    pub hints: Vec<u16>,
}

impl SignatureWitness {
    /// The witness holds `z + Z_SHIFT`, which lies in `[0, 2·Z_SHIFT]` iff
    /// `|z| < γ1 − β`.
    pub const Z_SHIFT: u64 = (GAMMA1 - BETA - 1) as u64;
    /// The witness holds `Q + Q_SHIFT`, which the verifier requires to lie in
    /// `[0, 2·Q_SHIFT)`.
    pub const Q_SHIFT: u64 = 1 << 28;

    pub const C_TILDE_OFFSET: usize = 0;
    pub const Z_OFFSET: usize = 32;
    pub const W_OFFSET: usize = Self::Z_OFFSET + L * N;
    pub const S_OFFSET: usize = Self::W_OFFSET + K * N;
    pub const Q_OFFSET: usize = Self::S_OFFSET + K * (N - 1);
    /// The words that determine the challenge point, a multiple of the
    /// sponge's rate.
    pub const NUM_CHALLENGE_WORDS: usize =
        (Self::Q_OFFSET + K * N).div_ceil(tip5::RATE) * tip5::RATE;
    pub const E_OFFSET: usize = Self::NUM_CHALLENGE_WORDS;
    pub const HINT_COUNT_OFFSET: usize = Self::E_OFFSET + K * N;
    pub const HINTS_OFFSET: usize = Self::HINT_COUNT_OFFSET + 1;

    /// The witness of a signature, or `None` if the signature does not decode.
    pub fn new(key: &ExpandedPublicKey, sig: &[u8; SIGNATURE_LEN]) -> Option<Self> {
        let signature = decode_signature(sig)?;
        let c = sample_in_ball(&signature.c_tilde);
        let mut w = [[0; N]; K];
        let mut s = [[0; N - 1]; K];
        let mut q = [[0; N]; K];
        let mut e = [[0; N]; K];
        for i in 0..K {
            let mut p = [0i64; 2 * N - 1];
            for j in 0..L {
                for (a, &ac) in key.a[i][j].iter().enumerate() {
                    for (b, &zc) in signature.z[j].iter().enumerate() {
                        p[a + b] += i64::from(ac) * i64::from(zc);
                    }
                }
            }
            for (a, &cc) in c.iter().enumerate() {
                if cc != 0 {
                    for (b, &tc) in key.t1_shifted[i].iter().enumerate() {
                        p[a + b] -= i64::from(cc) * i64::from(tc);
                    }
                }
            }
            for k in 0..N {
                let high = if k < N - 1 { p[k + N] } else { 0 };
                if k < N - 1 {
                    s[i][k] = high;
                }
                let r = p[k] - high;
                w[i][k] = reduce(r);
                q[i][k] = (r - i64::from(w[i][k])) / i64::from(Q);
                let (_, r0) = decompose(w[i][k]);
                e[i][k] = ((i64::from(w[i][k]) - i64::from(r0)) / i64::from(2 * GAMMA2)) as u32;
            }
        }
        let c_tilde = std::array::from_fn(|lane| {
            let bytes = signature.c_tilde[8 * lane..8 * lane + 8]
                .try_into()
                .unwrap();
            interleave(u64::from_le_bytes(bytes))
        });
        Some(Self {
            c_tilde,
            z: signature.z,
            w,
            s,
            q,
            e,
            hints: signature.hints,
        })
    }

    /// The words that a prover puts into memory.
    pub fn encode(&self) -> Vec<BFieldElement> {
        let mut words = vec![bfe!(0); Self::HINTS_OFFSET];
        for (k, slice) in self.c_tilde.iter().flatten().enumerate() {
            words[Self::C_TILDE_OFFSET + k] = bfe!(*slice);
        }
        for (k, z) in self.z.iter().flatten().enumerate() {
            words[Self::Z_OFFSET + k] = bfe!(i64::from(*z)) + bfe!(Self::Z_SHIFT);
        }
        for (k, w) in self.w.iter().flatten().enumerate() {
            words[Self::W_OFFSET + k] = bfe!(*w);
        }
        for (k, s) in self.s.iter().flatten().enumerate() {
            words[Self::S_OFFSET + k] = bfe!(*s);
        }
        for (k, q) in self.q.iter().flatten().enumerate() {
            words[Self::Q_OFFSET + k] = bfe!(*q) + bfe!(Self::Q_SHIFT);
        }
        for (k, e) in self.e.iter().flatten().enumerate() {
            words[Self::E_OFFSET + k] = bfe!(*e);
        }
        words[Self::HINT_COUNT_OFFSET] = bfe!(self.hints.len() as u64);
        words.extend(self.hints.iter().map(|&h| bfe!(h)));
        words
    }
}

#[cfg(test)]
mod tests {
    use fips204::ml_dsa_44;
    use fips204::traits::KeyGen;
    use fips204::traits::SerDes;
    use fips204::traits::Signer;

    use super::*;
    use crate::test_prelude::*;

    pub(crate) fn sign(seed: u8, message: &[u8]) -> ([u8; PUBLIC_KEY_LEN], [u8; SIGNATURE_LEN]) {
        let (pk, sk) = ml_dsa_44::KG::keygen_from_seed(&[seed; 32]);
        let sig = sk
            .try_sign_with_seed(&[seed ^ 0x5a; 32], message, &[])
            .unwrap();
        (pk.into_bytes(), sig)
    }

    #[macro_rules_attr::apply(test)]
    fn ntt_and_intt_are_inverse() {
        let poly: Poly = std::array::from_fn(|i| (i as u32 * 7919 + 13) % Q);
        assert_eq!(poly, intt(&ntt(&poly)));
    }

    #[macro_rules_attr::apply(test)]
    fn reference_verifier_agrees_with_fips204() {
        for seed in 0..4 {
            let message = [seed; 40];
            let (pk, sig) = sign(seed, &message);
            assert!(verify(&pk, &message, &sig, &[]));
            assert!(!verify(&pk, &[seed ^ 1; 40], &sig, &[]));
            let mut bad = sig;
            bad[0] ^= 1;
            assert!(!verify(&pk, &message, &bad, &[]));
        }
    }

    #[macro_rules_attr::apply(test)]
    fn witness_satisfies_its_identities() {
        let (pk, sig) = sign(7, &[1; 40]);
        let key = ExpandedPublicKey::new(&pk);
        let witness = SignatureWitness::new(&key, &sig).unwrap();
        let signature = decode_signature(&sig).unwrap();

        // w agrees with w'_Approx of the standard, computed with NTTs.
        let (_, t1) = decode_public_key(&pk);
        let c_hat = ntt(&sample_in_ball(&signature.c_tilde).map(|c| reduce(c.into())));
        let z_hat = signature.z.map(|z| ntt(&z.map(|c| reduce(c.into()))));
        let a_hat = expand_a(&decode_public_key(&pk).0);
        for i in 0..K {
            let t1_hat = ntt(&t1[i].map(|c| c << D));
            let w_hat: Poly = std::array::from_fn(|k| {
                let az: u64 = (0..L)
                    .map(|j| u64::from(a_hat[i][j][k]) * u64::from(z_hat[j][k]) % u64::from(Q))
                    .sum();
                let ct = u64::from(c_hat[k]) * u64::from(t1_hat[k]) % u64::from(Q);
                reduce(az as i64 - ct as i64)
            });
            assert_eq!(intt(&w_hat), witness.w[i]);
            for k in 0..N {
                let r0 = i64::from(witness.w[i][k]) - i64::from(2 * GAMMA2 * witness.e[i][k]);
                assert!(-i64::from(GAMMA2) < r0 && r0 <= i64::from(GAMMA2));
                assert!(witness.e[i][k] <= W1_RANGE);
                assert!(witness.q[i][k].abs() < 1 << 28);
            }
        }
    }
}
