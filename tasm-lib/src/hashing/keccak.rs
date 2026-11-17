//! Keccak-f\[1600\] and the functions of FIPS 202 built on it, SHA3-256,
//! SHA3-512, SHAKE128 and SHAKE256, with each 64-bit lane held as eight
//! *slices*.
//!
//! Slice `j` of a lane is the byte whose bit `m` is bit `j + 8·m` of the lane.
//! The steps θ, χ and ι act on every bit position alike, so they act on slices
//! as they act on lanes. A rotation of a lane by `r = 8·q + s` maps slice `j`
//! to slice `(j + s) mod 8` and rotates that slice by `q` bits, or by `q + 1`
//! bits if `j + s ≥ 8`. No bit crosses from one slice to another, so every
//! rotation of a slice is a function of one byte, which a table lookup
//! computes; see [`tables`].
//!
//! The Rust functions in this module are the reference against which the
//! Triton assembly is tested, and the helpers with which a prover prepares
//! its input.

pub mod hash;
pub mod keccak_f1600;
pub(crate) mod sponge;
pub mod tables;

/// The round constants of ι.
pub const ROUND_CONSTANTS: [u64; 24] = [
    0x0000_0000_0000_0001,
    0x0000_0000_0000_8082,
    0x8000_0000_0000_808A,
    0x8000_0000_8000_8000,
    0x0000_0000_0000_808B,
    0x0000_0000_8000_0001,
    0x8000_0000_8000_8081,
    0x8000_0000_0000_8009,
    0x0000_0000_0000_008A,
    0x0000_0000_0000_0088,
    0x0000_0000_8000_8009,
    0x0000_0000_8000_000A,
    0x0000_0000_8000_808B,
    0x8000_0000_0000_008B,
    0x8000_0000_0000_8089,
    0x8000_0000_0000_8003,
    0x8000_0000_0000_8002,
    0x8000_0000_0000_0080,
    0x0000_0000_0000_800A,
    0x8000_0000_8000_000A,
    0x8000_0000_8000_8081,
    0x8000_0000_0000_8080,
    0x0000_0000_8000_0001,
    0x8000_0000_8000_8008,
];

/// The rotation offsets of ρ, indexed `[x][y]`.
pub const ROTATION_OFFSETS: [[u32; 5]; 5] = [
    [0, 36, 3, 41, 18],
    [1, 44, 10, 45, 2],
    [62, 6, 43, 15, 61],
    [28, 55, 25, 21, 56],
    [27, 20, 39, 8, 14],
];

/// The number of rounds.
pub const NUM_ROUNDS: usize = 24;

/// The rate of SHAKE128, in bytes.
pub const SHAKE128_RATE: usize = 168;

/// The rate of SHAKE256, in bytes.
pub const SHAKE256_RATE: usize = 136;

/// A function of FIPS 202: a sponge on Keccak-f\[1600\] with a rate and a
/// first padding byte.
///
/// The first padding byte holds the domain-separation bits, `01` for SHA3 and
/// `1111` for SHAKE, followed by the first bit of `pad10*1`. Its last bit is
/// `0x80` in the rate's last byte.
#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub enum KeccakFunction {
    Sha3_256,
    Sha3_512,
    Shake128,
    Shake256,
}

impl KeccakFunction {
    pub const ALL: [Self; 4] = [
        Self::Sha3_256,
        Self::Sha3_512,
        Self::Shake128,
        Self::Shake256,
    ];

    /// The rate, in bytes.
    pub const fn rate(self) -> usize {
        match self {
            Self::Sha3_256 => 136,
            Self::Sha3_512 => 72,
            Self::Shake128 => SHAKE128_RATE,
            Self::Shake256 => SHAKE256_RATE,
        }
    }

    /// The first padding byte.
    pub const fn padding_byte(self) -> u8 {
        match self {
            Self::Sha3_256 | Self::Sha3_512 => 0x06,
            Self::Shake128 | Self::Shake256 => 0x1f,
        }
    }

    /// The length of the digest in bytes, for a hash function; `None` for an
    /// extendable-output function, whose caller chooses the length.
    pub const fn digest_len(self) -> Option<usize> {
        match self {
            Self::Sha3_256 => Some(32),
            Self::Sha3_512 => Some(64),
            Self::Shake128 | Self::Shake256 => None,
        }
    }

    pub const fn name(self) -> &'static str {
        match self {
            Self::Sha3_256 => "sha3_256",
            Self::Sha3_512 => "sha3_512",
            Self::Shake128 => "shake128",
            Self::Shake256 => "shake256",
        }
    }

    /// The function's `output_len` bytes of output on `input`.
    pub fn hash(self, input: &[u8], output_len: usize) -> Vec<u8> {
        let mut sponge = Shake::with_padding(self.rate(), self.padding_byte());
        sponge.absorb(input);
        sponge.squeeze(output_len)
    }
}

/// Keccak-f\[1600\] on lanes indexed `x + 5·y`.
pub fn keccak_f1600(state: &mut [u64; 25]) {
    for round_constant in ROUND_CONSTANTS {
        let mut c = [0u64; 5];
        for (x, cx) in c.iter_mut().enumerate() {
            *cx = (0..5).fold(0, |acc, y| acc ^ state[x + 5 * y]);
        }
        for x in 0..5 {
            let d = c[(x + 4) % 5] ^ c[(x + 1) % 5].rotate_left(1);
            for y in 0..5 {
                state[x + 5 * y] ^= d;
            }
        }
        let mut b = [0u64; 25];
        for x in 0..5 {
            for y in 0..5 {
                b[y + 5 * ((2 * x + 3 * y) % 5)] =
                    state[x + 5 * y].rotate_left(ROTATION_OFFSETS[x][y]);
            }
        }
        for x in 0..5 {
            for y in 0..5 {
                state[x + 5 * y] =
                    b[x + 5 * y] ^ (!b[(x + 1) % 5 + 5 * y] & b[(x + 2) % 5 + 5 * y]);
            }
        }
        state[0] ^= round_constant;
    }
}

/// The eight slices of a lane.
pub fn interleave(lane: u64) -> [u8; 8] {
    let mut slices = [0u8; 8];
    for (j, slice) in slices.iter_mut().enumerate() {
        for m in 0..8 {
            *slice |= (((lane >> (j + 8 * m)) & 1) as u8) << m;
        }
    }
    slices
}

/// The lane of eight slices; the inverse of [`interleave`].
pub fn deinterleave(slices: [u8; 8]) -> u64 {
    let mut lane = 0;
    for (j, slice) in slices.iter().enumerate() {
        for m in 0..8 {
            lane |= u64::from((slice >> m) & 1) << (j + 8 * m);
        }
    }
    lane
}

/// The sponge on Keccak-f\[1600\] with the given rate in bytes and first
/// padding byte (see [`KeccakFunction`]), SHAKE unless the padding byte says
/// otherwise.
#[derive(Debug, Clone)]
pub struct Shake {
    state: [u64; 25],
    rate: usize,
    padding_byte: u8,
    position: usize,
    squeezing: bool,
}

impl Shake {
    /// SHAKE with the given rate in bytes.
    pub fn new(rate: usize) -> Self {
        Self::with_padding(rate, 0x1f)
    }

    pub fn with_padding(rate: usize, padding_byte: u8) -> Self {
        Self {
            state: [0; 25],
            rate,
            padding_byte,
            position: 0,
            squeezing: false,
        }
    }

    pub fn shake128() -> Self {
        Self::new(SHAKE128_RATE)
    }

    pub fn shake256() -> Self {
        Self::new(SHAKE256_RATE)
    }

    fn xor_byte(&mut self, index: usize, byte: u8) {
        self.state[index / 8] ^= u64::from(byte) << (8 * (index % 8));
    }

    pub fn absorb(&mut self, input: &[u8]) {
        assert!(!self.squeezing, "cannot absorb after squeezing");
        for &byte in input {
            self.xor_byte(self.position, byte);
            self.position += 1;
            if self.position == self.rate {
                keccak_f1600(&mut self.state);
                self.position = 0;
            }
        }
    }

    pub fn squeeze(&mut self, len: usize) -> Vec<u8> {
        if !self.squeezing {
            self.xor_byte(self.position, self.padding_byte);
            self.xor_byte(self.rate - 1, 0x80);
            keccak_f1600(&mut self.state);
            self.position = 0;
            self.squeezing = true;
        }
        let mut output = Vec::with_capacity(len);
        for _ in 0..len {
            if self.position == self.rate {
                keccak_f1600(&mut self.state);
                self.position = 0;
            }
            output.push((self.state[self.position / 8] >> (8 * (self.position % 8))) as u8);
            self.position += 1;
        }
        output
    }
}

/// SHAKE256 of `input`, `len` bytes long.
pub fn shake256(input: &[u8], len: usize) -> Vec<u8> {
    let mut shake = Shake::shake256();
    shake.absorb(input);
    shake.squeeze(len)
}

#[cfg(test)]
mod tests {
    use proptest::collection::vec;
    use sha3::digest::ExtendableOutput;

    use super::*;
    use crate::test_prelude::*;

    /// The function's `output_len` bytes of output on `input`, computed by the
    /// `sha3` crate, against which the code here is tested for compliance with
    /// FIPS 202.
    ///
    /// # Panics
    ///
    /// Panics if `function` is SHA3 and `output_len` is not its digest length.
    pub(super) fn fips_202(function: KeccakFunction, input: &[u8], output_len: usize) -> Vec<u8> {
        let mut output = vec![0; output_len];
        match function {
            KeccakFunction::Sha3_256 => {
                output.copy_from_slice(&<sha3::Sha3_256 as sha3::Digest>::digest(input))
            }
            KeccakFunction::Sha3_512 => {
                output.copy_from_slice(&<sha3::Sha3_512 as sha3::Digest>::digest(input))
            }
            KeccakFunction::Shake128 => sha3::Shake128::digest_xof(input, &mut output),
            KeccakFunction::Shake256 => sha3::Shake256::digest_xof(input, &mut output),
        }
        output
    }

    fn output_len(function: KeccakFunction, shake_len: usize) -> usize {
        function.digest_len().unwrap_or(shake_len)
    }

    #[macro_rules_attr::apply(proptest)]
    fn permutation_complies_with_fips_202(state: [u64; 25]) {
        let mut expected = state;
        keccak::f1600(&mut expected);
        let mut actual = state;
        keccak_f1600(&mut actual);
        prop_assert_eq!(expected, actual);
    }

    #[macro_rules_attr::apply(proptest)]
    fn functions_comply_with_fips_202(
        #[strategy(0usize..4)] function: usize,
        #[strategy(vec(arb(), 0..1000))] input: Vec<u8>,
        #[strategy(0usize..1000)] shake_len: usize,
    ) {
        let function = KeccakFunction::ALL[function];
        let output_len = output_len(function, shake_len);
        prop_assert_eq!(
            fips_202(function, &input, output_len),
            function.hash(&input, output_len)
        );
    }

    /// Every input length up to three blocks and one byte, and so every
    /// position of the padding in a block.
    #[macro_rules_attr::apply(test)]
    fn functions_comply_with_fips_202_on_every_length() {
        for function in KeccakFunction::ALL {
            let rate = function.rate();
            for n in 0..=3 * rate + 1 {
                let input = (0..n).map(|i| (i * 37 + 11) as u8).collect::<Vec<_>>();
                let output_len = output_len(function, 2 * rate + 1);
                assert_eq!(
                    fips_202(function, &input, output_len),
                    function.hash(&input, output_len),
                    "{function:?} {n}"
                );
            }
        }
    }

    #[macro_rules_attr::apply(proptest)]
    fn shake256_complies_with_fips_202(
        #[strategy(vec(arb(), 0..1000))] input: Vec<u8>,
        #[strategy(0usize..1000)] len: usize,
    ) {
        prop_assert_eq!(
            fips_202(KeccakFunction::Shake256, &input, len),
            shake256(&input, len)
        );
    }

    /// [`Shake`] absorbs and squeezes in pieces of lengths `absorb_lens` and
    /// `squeeze_lens`, the former repeated until the input runs out.
    #[macro_rules_attr::apply(proptest)]
    fn shake_in_pieces_complies_with_fips_202(
        #[strategy(0usize..3)] constructor: usize,
        #[strategy(vec(arb(), 0..1000))] input: Vec<u8>,
        #[strategy(vec(1usize..400, 1..5))] absorb_lens: Vec<usize>,
        #[strategy(vec(0usize..400, 1..5))] squeeze_lens: Vec<usize>,
    ) {
        let (function, mut shake) = [
            (KeccakFunction::Shake128, Shake::shake128()),
            (KeccakFunction::Shake256, Shake::shake256()),
            (KeccakFunction::Shake128, Shake::new(SHAKE128_RATE)),
        ][constructor]
            .clone();
        let mut rest = &input[..];
        for &len in absorb_lens.iter().cycle() {
            if rest.is_empty() {
                break;
            }
            let (piece, tail) = rest.split_at(len.min(rest.len()));
            shake.absorb(piece);
            rest = tail;
        }
        let output = squeeze_lens
            .iter()
            .flat_map(|&len| shake.squeeze(len))
            .collect_vec();
        prop_assert_eq!(fips_202(function, &input, output.len()), output);
    }

    /// The permutation of the zero state, from the Keccak team's test vectors.
    #[macro_rules_attr::apply(test)]
    fn permutation_of_zero_state_matches_known_answer() {
        let mut state = [0u64; 25];
        keccak_f1600(&mut state);
        assert_eq!(0xf125_8f79_40e1_dde7, state[0]);
        assert_eq!(0x84d5_ccf9_33c0_478a, state[1]);
    }

    /// SHAKE256 of the empty string, from FIPS 202's examples.
    #[macro_rules_attr::apply(test)]
    fn shake256_of_empty_string_matches_known_answer() {
        let output = shake256(&[], 8);
        assert_eq!([0x46, 0xb9, 0xdd, 0x2b, 0x0b, 0xa8, 0x8d, 0x13], output[..]);
    }

    /// The functions on the empty string, from NIST's examples.
    #[macro_rules_attr::apply(test)]
    fn functions_of_empty_string_match_known_answers() {
        let hex = |bytes: Vec<u8>| bytes.iter().map(|b| format!("{b:02x}")).collect::<String>();
        assert_eq!(
            "a7ffc6f8bf1ed76651c14756a061d662f580ff4de43b49fa82d80a4b80f8434a",
            hex(KeccakFunction::Sha3_256.hash(&[], 32))
        );
        assert_eq!(
            "a69f73cca23a9ac5c8b567dc185a756e97c982164fe25859e0d1dcc1475c80a6\
             15b2123af1f5f94c11e3e9402c3ac558f500199d95b6d3e301758586281dcd26",
            hex(KeccakFunction::Sha3_512.hash(&[], 64))
        );
        assert_eq!(
            "7f9c2ba4e88f827d616045507605853ed73b8093f6efbc88eb1a6eacfa66ef26",
            hex(KeccakFunction::Shake128.hash(&[], 32))
        );
    }

    #[macro_rules_attr::apply(proptest)]
    fn deinterleave_inverts_interleave(lane: u64) {
        prop_assert_eq!(lane, deinterleave(interleave(lane)));
    }

    #[macro_rules_attr::apply(proptest)]
    fn rotation_maps_slices_as_documented(lane: u64, #[strategy(0u32..64)] r: u32) {
        let slices = interleave(lane);
        let rotated = interleave(lane.rotate_left(r));
        let (q, s) = (r / 8, (r % 8) as usize);
        for j in 0..8 {
            let t = (q + u32::from(j + s >= 8)) % 8;
            prop_assert_eq!(rotated[(j + s) % 8], slices[j].rotate_left(t));
        }
    }
}
