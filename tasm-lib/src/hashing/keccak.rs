//! Keccak-f\[1600\] and SHAKE, with each 64-bit lane held as eight *slices*.
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

pub mod keccak_f1600;
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

/// The extendable-output function SHAKE with the given rate in bytes.
#[derive(Debug, Clone)]
pub struct Shake {
    state: [u64; 25],
    rate: usize,
    position: usize,
    squeezing: bool,
}

impl Shake {
    pub fn new(rate: usize) -> Self {
        Self {
            state: [0; 25],
            rate,
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
            self.xor_byte(self.position, 0x1f);
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
    use super::*;
    use crate::test_prelude::*;

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
