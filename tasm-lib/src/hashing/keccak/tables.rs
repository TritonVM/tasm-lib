//! The tables that the Keccak snippets look up, and the snippet that checks
//! them.
//!
//! The tables live in non-deterministically initialized memory at
//! [`KECCAK_TABLES_ADDRESS`]. The prover supplies them, and
//! [`CheckKeccakTables`] hashes them and compares the digest with the one
//! hard-coded in the program. A program that uses any Keccak snippet calls
//! [`CheckKeccakTables`] once, before the first lookup; after that, a lookup
//! is a single `read_mem`, and its result is sound as long as its index lies
//! in the table, which every caller guarantees by looking up bytes only.

use triton_vm::prelude::*;
use twenty_first::prelude::Sponge;

use crate::prelude::*;

/// The address of the first table word. It lies near the end of the memory
/// page reserved for non-determinism, so that callers can keep their own
/// non-deterministic input at low addresses.
pub const KECCAK_TABLES_ADDRESS: u64 = (1 << 32) - 8192;

/// The offset of the rotation tables: entry `256·(t-1) + v` is the byte `v`
/// rotated left by `t` bits, for `t` from 1 to 7.
pub(crate) const ROTATION_OFFSET: u64 = 0;

/// The offset of the transposition tables. For a byte `v` in position `m` of
/// eight bytes, word `256·m + v` is `lo_m(v) + 2^32·hi_m(v)`, where
///
/// ```text
/// lo_m(v) = Σ_{k<4} bit_k(v) · 2^(8·k + m)
/// hi_m(v) = Σ_{k<4} bit_{k+4}(v) · 2^(8·k + m)
/// ```
///
/// The sum of `lo_m` over the eight bytes is a word of four bytes of which
/// byte `k` collects bit `k` of every input byte, and likewise for `hi_m` and
/// bits 4 to 7. That transposes an 8×8 matrix of bits, which turns the eight
/// bytes of a lane into its eight slices, and the slices back into the bytes.
/// The words of positions 0 to 3 have their bits in distinct places, and so do
/// those of positions 4 to 7. So each of the two sums is their bitwise or, of
/// which the high word is at most `0xf0f0f0f0`, and neither wraps modulo p.
pub(crate) const TRANSPOSITION_OFFSET: u64 = 7 * 256;

/// The number of table words, a multiple of the sponge's rate.
pub const NUM_TABLE_WORDS: usize = TRANSPOSITION_OFFSET as usize + 8 * 256;

/// The address of the rotation table for a rotation by `t` bits.
pub(crate) fn rotation_table_address(t: u32) -> u64 {
    assert!((1..8).contains(&t));
    KECCAK_TABLES_ADDRESS + ROTATION_OFFSET + 256 * u64::from(t - 1)
}

/// The address of the transposition table for byte position `m`.
pub(crate) fn transposition_table_address(m: usize) -> u64 {
    KECCAK_TABLES_ADDRESS + TRANSPOSITION_OFFSET + 256 * m as u64
}

/// The table words, in order.
pub(crate) fn table_words() -> Vec<BFieldElement> {
    let mut words = vec![bfe!(0); NUM_TABLE_WORDS];
    for t in 1..8u32 {
        for v in 0..256u32 {
            let rotated = (v as u8).rotate_left(t);
            words[(256 * (t - 1) + v) as usize] = bfe!(rotated);
        }
    }
    for m in 0..8 {
        for v in 0..256u64 {
            let lo = (0..4).map(|k| ((v >> k) & 1) << (8 * k + m)).sum::<u64>();
            let hi = (0..4)
                .map(|k| ((v >> (k + 4)) & 1) << (8 * k + m))
                .sum::<u64>();
            words[TRANSPOSITION_OFFSET as usize + 256 * m as usize + v as usize] =
                bfe!(lo + (hi << 32));
        }
    }
    words
}

/// Puts the tables into memory, as a prover does.
pub fn insert_tables(memory: &mut std::collections::HashMap<BFieldElement, BFieldElement>) {
    for (i, word) in table_words().into_iter().enumerate() {
        memory.insert(bfe!(KECCAK_TABLES_ADDRESS + i as u64), word);
    }
}

/// The first five elements that Tip5's sponge squeezes after absorbing the
/// table words.
pub(crate) fn table_digest() -> [BFieldElement; Digest::LEN] {
    let mut sponge = Tip5::init();
    for chunk in table_words().chunks(tip5::RATE) {
        sponge.absorb(chunk.try_into().unwrap());
    }
    sponge.squeeze()[..Digest::LEN].try_into().unwrap()
}

/// Checks the tables at [`KECCAK_TABLES_ADDRESS`] against their digest, and
/// crashes the VM if they differ. Overwrites the sponge state.
///
/// ### Pre-conditions
///
/// None.
///
/// ### Post-conditions
///
/// - every word of the tables equals the word that [`insert_tables`] writes
#[derive(Debug, Default, Copy, Clone, Eq, PartialEq, Hash)]
pub struct CheckKeccakTables;

impl CheckKeccakTables {
    pub const TABLES_DIFFER_ERROR_ID: i128 = 640;
}

impl BasicSnippet for CheckKeccakTables {
    fn parameters(&self) -> Vec<(DataType, String)> {
        vec![]
    }

    fn return_values(&self) -> Vec<(DataType, String)> {
        vec![]
    }

    fn entrypoint(&self) -> String {
        "tasmlib_hashing_keccak_check_tables".to_string()
    }

    fn code(&self, _: &mut Library) -> Vec<LabelledInstruction> {
        let absorb_all =
            vec![triton_asm!(sponge_absorb_mem); NUM_TABLE_WORDS / tip5::RATE].concat();
        let digest = table_digest();
        let push_digest = digest
            .iter()
            .rev()
            .flat_map(|d| triton_asm!(push { d.value() }))
            .collect::<Vec<_>>();

        triton_asm!(
            {self.entrypoint()}:
                sponge_init
                push 0 push 0 push 0 push 0
                push {KECCAK_TABLES_ADDRESS}
                // _ 0 0 0 0 *tables
                {&absorb_all}
                pop 5
                sponge_squeeze
                // _ [squeezed; 10]
                {&push_digest}
                assert_vector error_id {Self::TABLES_DIFFER_ERROR_ID}
                pop 5
                pop 5
                return
        )
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use super::*;
    use crate::test_prelude::*;

    fn run(memory: HashMap<BFieldElement, BFieldElement>) -> Result<(), VMError> {
        let code = CheckKeccakTables.link_for_isolated_run();
        let nondeterminism = NonDeterminism::default().with_ram(memory);
        VM::run(Program::new(&code), PublicInput::default(), nondeterminism).map(|_| ())
    }

    #[macro_rules_attr::apply(test)]
    fn accepts_the_tables() {
        let mut memory = HashMap::new();
        insert_tables(&mut memory);
        run(memory).unwrap();
    }

    #[macro_rules_attr::apply(test)]
    fn refuses_tables_with_one_word_changed() {
        for offset in [
            0,
            1000,
            TRANSPOSITION_OFFSET + 5,
            TRANSPOSITION_OFFSET + 2000,
        ] {
            let mut memory = HashMap::new();
            insert_tables(&mut memory);
            let address = bfe!(KECCAK_TABLES_ADDRESS + offset);
            let word = memory[&address];
            memory.insert(address, word + bfe!(1));
            let err = run(memory).unwrap_err();
            let InstructionError::VectorAssertionFailed(_, err) = err.source else {
                panic!("expected a failed vector assertion, got {}", err.source);
            };
            assert_eq!(Some(CheckKeccakTables::TABLES_DIFFER_ERROR_ID), err.id);
        }
    }

    #[macro_rules_attr::apply(test)]
    fn transposition_tables_turn_bytes_into_slices() {
        let lane = u64::MAX;
        let bytes = lane.to_le_bytes();
        let words = table_words();
        let table = |m: usize, v: u8| words[TRANSPOSITION_OFFSET as usize + 256 * m + v as usize];
        let integer_sum = |range: std::ops::Range<usize>| -> u128 {
            range.map(|m| u128::from(table(m, bytes[m]).value())).sum()
        };
        let first: BFieldElement = (0..4).map(|m| table(m, bytes[m])).sum();
        let second: BFieldElement = (4..8).map(|m| table(m, bytes[m])).sum();
        assert_eq!(integer_sum(0..4), u128::from(first.value()));
        assert_eq!(integer_sum(4..8), u128::from(second.value()));
        let lo = (first.value() as u32) + (second.value() as u32);
        let hi = ((first.value() >> 32) as u32) + ((second.value() >> 32) as u32);
        let mut slices = [0u8; 8];
        slices[..4].copy_from_slice(&lo.to_le_bytes());
        slices[4..].copy_from_slice(&hi.to_le_bytes());
        assert_eq!(super::super::interleave(lane), slices);
    }
}
