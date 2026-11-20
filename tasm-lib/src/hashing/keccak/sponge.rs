//! The code that moves bytes between memory and the slices of the state, which
//! every sponge on [`KeccakF1600`] shares: the snippets in [`hash`](super::hash)
//! and the ML-DSA verifier.

use itertools::Itertools;
use triton_vm::prelude::*;

use super::keccak_f1600::KeccakF1600;
use super::tables::transposition_table_address;
use crate::memory::load_words_from_memory_pop_pointer;
use crate::memory::load_words_from_scattered_addresses;
use crate::memory::write_words_to_scattered_addresses;

/// The order in which [`words_to_bytes`] leaves the bytes: the byte at depth
/// `d` is byte `BYTE_ORDER_OF_WORDS_TO_BYTES[d]` of the two words.
pub(crate) const BYTE_ORDER_OF_WORDS_TO_BYTES: [usize; 8] = [3, 2, 1, 0, 7, 6, 5, 4];

/// `_ x → _ x_0 x_1 x_2 x_3` for `x < 2^32`, with byte `x_0`, the least
/// significant, deepest. Crashes if `x` is not a u32.
pub(crate) fn bytes_of_word() -> Vec<LabelledInstruction> {
    vec![triton_asm!(push 256 pick 1 div_mod pick 1); 3].concat()
}

/// `_ lo hi → _ hi_0 hi_1 hi_2 hi_3 lo_0 lo_1 lo_2 lo_3`: the eight bytes of
/// two u32 words, of which byte `k < 4` is byte `k` of `lo` and byte `k ≥ 4`
/// is byte `k - 4` of `hi`, in the order [`BYTE_ORDER_OF_WORDS_TO_BYTES`].
pub(crate) fn words_to_bytes() -> Vec<LabelledInstruction> {
    triton_asm!(
        {&bytes_of_word()}
        pick 4
        {&bytes_of_word()}
    )
}

/// `_ [v; 8] → _ lo hi`: the 8×8 transposition of bits (see
/// [`TRANSPOSITION_OFFSET`](super::tables::TRANSPOSITION_OFFSET)), of the
/// byte `v` at depth `d` taken as byte `positions[d]` of the input. Byte `k`
/// of the output, which collects bit `k` of every input byte, is byte `k` of
/// `lo` for `k < 4` and byte `k - 4` of `hi` otherwise.
///
/// The transposition turns the bytes of a lane into its slices and back.
pub(crate) fn transpose_to_words(positions: [usize; 8]) -> Vec<LabelledInstruction> {
    // One accumulator for the bytes at depths 0 to 3 and one for those at 4 to
    // 7, neither of which wraps modulo p. Each splits into its low word, the
    // bits of outputs 0 to 3, and its high word, the bits of outputs 4 to 7.
    let accumulate = |depths: std::ops::Range<usize>, pick: usize| {
        depths
            .flat_map(|d| {
                let table = transposition_table_address(positions[d]);
                triton_asm!(pick {pick} addi {table} read_mem 1 pop 1 add)
            })
            .collect_vec()
    };
    triton_asm!(
        push 0
        {&accumulate(0..4, 1)}
        push 0
        {&accumulate(4..8, 2)}
        // _ first second
        split pick 2 split pick 2 add swap 2 add
        // _ lo hi
    )
}

/// The identity of [`transpose_to_words`]: the byte at depth `d` is byte `d`.
pub(crate) const POSITIONS_BY_DEPTH: [usize; 8] = [0, 1, 2, 3, 4, 5, 6, 7];

impl KeccakF1600 {
    /// `_ [slice; 8] → _`, the slices in the order
    /// [`BYTE_ORDER_OF_WORDS_TO_BYTES`], into lane `lane` of the state: by
    /// storing if `xor` is false and by xoring into the state otherwise.
    fn write_slices(&self, lane: usize, xor: bool) -> Vec<LabelledInstruction> {
        BYTE_ORDER_OF_WORDS_TO_BYTES
            .into_iter()
            .flat_map(|j| {
                let address = self.slice_address(lane, j);
                let mut instructions = vec![];
                if xor {
                    instructions.extend(triton_asm!(push {address} read_mem 1 pop 1 xor));
                }
                instructions.extend(triton_asm!(push {address} write_mem 1 pop 1));
                instructions
            })
            .collect()
    }

    /// `_ → _ lo hi`, the two little-endian u32 words of lane `lane` of the
    /// state.
    pub(crate) fn read_lane_as_words(&self, lane: usize) -> Vec<LabelledInstruction> {
        let slices = (0..8)
            .rev()
            .map(|j| self.slice_address(lane, j))
            .collect_vec();
        triton_asm!(
            {&load_words_from_scattered_addresses(&slices)}
            {&transpose_to_words(POSITIONS_BY_DEPTH)}
        )
    }

    /// Writes lane `lane` of the state from the eight bytes at `bytes`, one
    /// byte per word, by storing if `xor` is false and by xoring into the
    /// state otherwise.
    pub(crate) fn absorb_lane_from_bytes(
        &self,
        bytes: u64,
        lane: usize,
        xor: bool,
    ) -> Vec<LabelledInstruction> {
        triton_asm!(
            push {bytes + 7}
            {&load_words_from_memory_pop_pointer(8)}
            {&transpose_to_words(POSITIONS_BY_DEPTH)}
            {&words_to_bytes()}
            {&self.write_slices(lane, xor)}
        )
    }

    /// Writes the eight bytes of lane `lane` of the state to `bytes`, one byte
    /// per word.
    pub(crate) fn squeeze_lane_to_bytes(
        &self,
        bytes: u64,
        lane: usize,
    ) -> Vec<LabelledInstruction> {
        let destinations = BYTE_ORDER_OF_WORDS_TO_BYTES.map(|k| bytes + k as u64);
        triton_asm!(
            {&self.read_lane_as_words(lane)}
            {&words_to_bytes()}
            {&write_words_to_scattered_addresses(&destinations)}
        )
    }

    /// `_ hi lo → _`: xors the lane of which `lo` and `hi` are the
    /// little-endian u32 words into lane `lane` of the state. Crashes unless
    /// both are u32s.
    pub(crate) fn absorb_words(&self, lane: usize) -> Vec<LabelledInstruction> {
        // After the split, the byte at depth d is byte 7 - d of the lane.
        triton_asm!(
            {&bytes_of_word()}
            pick 4
            {&bytes_of_word()}
            {&transpose_to_words([7, 6, 5, 4, 3, 2, 1, 0])}
            {&words_to_bytes()}
            {&self.write_slices(lane, true)}
        )
    }

    /// `_ *words → _ (*words + 2)`: xors the lane of which the two words at
    /// `*words` are the little-endian u32 words into lane `lane` of the state.
    pub(crate) fn absorb_lane_from_words(&self, lane: usize) -> Vec<LabelledInstruction> {
        triton_asm!(
            dup 0 addi 1 read_mem 2 pop 1
            // _ *words hi lo
            {&self.absorb_words(lane)}
            addi 2
        )
    }

    /// `_ *words → _ (*words + 2)`: writes lane `lane` of the state as two
    /// little-endian u32 words to `*words`.
    pub(crate) fn squeeze_lane_to_words(&self, lane: usize) -> Vec<LabelledInstruction> {
        triton_asm!(
            {&self.read_lane_as_words(lane)}
            // _ *words lo hi
            swap 1 pick 2 write_mem 2
        )
    }

    /// `_ *words → _ (*words + rate/4)`: xors `rate` bytes, as little-endian
    /// u32 words, into the first `rate/8` lanes of the state.
    pub(crate) fn absorb_block(&self, rate: usize) -> Vec<LabelledInstruction> {
        (0..rate / 8)
            .flat_map(|lane| self.absorb_lane_from_words(lane))
            .collect()
    }

    /// `_ *words → _ (*words + rate/4)`: writes the first `rate` bytes of the
    /// state as little-endian u32 words to `*words`.
    pub(crate) fn squeeze_block(&self, rate: usize) -> Vec<LabelledInstruction> {
        (0..rate / 8)
            .flat_map(|lane| self.squeeze_lane_to_words(lane))
            .collect()
    }

    /// `slice ^= value` for slice `j` of lane `lane` of the state.
    pub(crate) fn xor_constant(
        &self,
        lane: usize,
        j: usize,
        value: u64,
    ) -> Vec<LabelledInstruction> {
        let address = self.slice_address(lane, j);
        triton_asm!(
            push {address} read_mem 1 pop 1
            push {value} xor
            push {address} write_mem 1 pop 1
        )
    }

    /// Xors `byte` into byte `position` of the state, where byte `8·lane + b`
    /// is byte `b` of lane `lane`. Bit `j` of the byte is bit `b` of slice
    /// `j`.
    pub(crate) fn xor_byte(&self, position: usize, byte: u8) -> Vec<LabelledInstruction> {
        let (lane, b) = (position / 8, position % 8);
        (0..8)
            .filter(|j| (byte >> j) & 1 == 1)
            .flat_map(|j| self.xor_constant(lane, j, 1 << b))
            .collect()
    }

    /// The padding of a message that ends at byte `position` of a block of
    /// `rate` bytes: `padding_byte` at `position` and `0x80` in the block's
    /// last byte.
    pub(crate) fn pad(
        &self,
        rate: usize,
        position: usize,
        padding_byte: u8,
    ) -> Vec<LabelledInstruction> {
        [
            self.xor_byte(position, padding_byte),
            self.xor_byte(rate - 1, 0x80),
        ]
        .concat()
    }
}
