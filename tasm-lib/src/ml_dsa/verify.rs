//! The ML-DSA-44 verifier in Triton assembly.

use itertools::Itertools;
use triton_vm::prelude::*;

use super::helper::ExpandedPublicKey;
use super::helper::SignatureWitness;
use super::*;
use crate::hashing::keccak::KeccakFunction;
use crate::hashing::keccak::SHAKE256_RATE;
use crate::hashing::keccak::hash::Shake256;
use crate::hashing::keccak::keccak_f1600::KeccakF1600;
use crate::hashing::keccak::tables::CheckKeccakTables;
use crate::hashing::keccak::tables::KECCAK_TABLES_ADDRESS;
use crate::memory::load_words_from_memory_pop_pointer;
use crate::memory::write_words_to_memory_pop_pointer;
use crate::memory::write_words_to_scattered_addresses;
use crate::memory::write_zeros_to_memory;
use crate::prelude::*;

const P: u64 = BFieldElement::P;

const SHAKE256_PADDING_BYTE: u8 = KeccakFunction::Shake256.padding_byte();

/// The address of the high-bits table, which lies in non-deterministically
/// initialized memory below the
/// [Keccak tables](crate::hashing::keccak::tables::KECCAK_TABLES_ADDRESS).
/// For high bits `e` from 0 to 44, position `j` in a group from 0 to 3, and
/// `b` from 0 to 1, with `w1 = e mod 44`, it holds
/// - `w1·64^j` at `45·j + e`,
/// - `γ2 + 1 − 2·γ2·e − [e = 44]` at `180 + e`, so that `r` plus it is
///   `r0 + γ2 + 1`, and
/// - `((w1 − 1 + 2·b) mod 44 − w1)·64^j` at `225 + 90·j + 2·e + b`, the change
///   a hint makes to a group, where `b = [r0 > 0]`.
///
/// The snippet checks it against its digest.
pub const HIGH_BITS_TABLE_ADDRESS: u64 = KECCAK_TABLES_ADDRESS - 1024;

const NUM_E: usize = W1_RANGE as usize + 1;
const SCALED_OFFSET: usize = 0;
const THRESHOLD_OFFSET: usize = SCALED_OFFSET + 4 * NUM_E;
const DIFF_OFFSET: usize = THRESHOLD_OFFSET + NUM_E;
const NUM_HIGH_BITS_TABLE_WORDS: usize = (DIFF_OFFSET + 8 * NUM_E).next_multiple_of(tip5::RATE);

/// The words of the high-bits table.
pub fn high_bits_table_words() -> Vec<BFieldElement> {
    let mut words = vec![bfe!(0); NUM_HIGH_BITS_TABLE_WORDS];
    let (range, gamma2) = (i64::from(W1_RANGE), i64::from(GAMMA2));
    for e in 0..NUM_E {
        let w1 = e as i64 % range;
        for j in 0..4 {
            let scale = 64_i64.pow(j as u32);
            words[SCALED_OFFSET + NUM_E * j + e] = bfe!(w1 * scale);
            for b in 0..2 {
                let new = (w1 - 1 + 2 * b as i64).rem_euclid(range);
                words[DIFF_OFFSET + 2 * NUM_E * j + 2 * e + b] = bfe!((new - w1) * scale);
            }
        }
        let special = i64::from(e == NUM_E - 1);
        words[THRESHOLD_OFFSET + e] = bfe!(gamma2 + 1 - 2 * gamma2 * e as i64 - special);
    }
    words
}

/// Puts the high-bits table into memory, as a prover does.
pub fn insert_high_bits_table(
    memory: &mut std::collections::HashMap<BFieldElement, BFieldElement>,
) {
    for (i, word) in high_bits_table_words().into_iter().enumerate() {
        memory.insert(bfe!(HIGH_BITS_TABLE_ADDRESS + i as u64), word);
    }
}

fn high_bits_table_digest() -> [BFieldElement; Digest::LEN] {
    use twenty_first::prelude::Sponge;
    let mut sponge = Tip5::init();
    for chunk in high_bits_table_words().chunks(tip5::RATE) {
        sponge.absorb(chunk.try_into().unwrap());
    }
    sponge.squeeze()[..Digest::LEN].try_into().unwrap()
}

fn neg(x: u64) -> u64 {
    if x == 0 { 0 } else { P - x }
}

/// Verifies an ML-DSA-44 signature on a message, with an empty context string,
/// and crashes the VM if the signature is invalid.
///
/// ```text
/// BEFORE: _ *expanded_key *signature_witness [key_digest] *message [message_len: u32]
/// AFTER:  _
/// ```
///
/// The snippet computes μ as [`MlDsa44Mu`] does, and checks the signature on
/// μ as [`MlDsa44VerifyExternalMu`] does. It runs the code of both on one
/// Keccak state, and checks the Keccak tables and the key digest once. Its
/// pre-conditions are those of both snippets. It checks in their order, so it
/// fails exactly when they do, with the same error ID: [`MlDsa44Mu`]'s for the
/// key digest and the message, and [`MlDsa44VerifyExternalMu`]'s otherwise.
///
/// Use it when one program verifies the signature on the message. For a
/// message of at most 69 bytes, `tr ‖ 0 ‖ 0 ‖ message` and its padding fit in
/// one block of SHAKE256, and every table stays below 2^20 rows. Every further
/// 136 bytes of message cost one more Keccak-f, about 100,000 cycles, which
/// crosses 2^20. For such a message, compute μ with [`MlDsa44Mu`] in one
/// program and verify with [`MlDsa44VerifyExternalMu`] in another.
///
/// ### Post-conditions
///
/// - ML-DSA.Verify(pk, message, σ, "") holds for the public key `pk` whose
///   expansion has digest `key_digest`, and the signature σ whose witness is
///   given
/// - the sponge state is overwritten
/// - two pages of memory are allocated with [`DynMalloc`]
#[derive(Debug, Default, Copy, Clone, Eq, PartialEq, Hash)]
pub struct MlDsa44Verify;

/// μ = SHAKE256(tr ‖ 0 ‖ 0 ‖ message, 64), the message representative of
/// ML-DSA-44 with an empty context string (FIPS 204, Algorithm 2 and
/// Algorithm 8, line 7), where `tr` is the hash of the public key.
///
/// ```text
/// BEFORE: _ *expanded_key [key_digest] *message [message_len: u32]
/// AFTER:  _ *mu
/// ```
///
/// The message is the byte string of `message_len` bytes at `*message`, held
/// as field elements as for the [Keccak snippets](crate::hashing::keccak::hash),
/// and the snippet crashes if a byte of its last element from `message_len` on
/// is not zero, or if `message_len` exceeds 2^32 − 70. The snippet takes `tr`
/// from the [expansion](ExpandedPublicKey) of the key at `*expanded_key`, which
/// it checks against `key_digest`. So μ binds the message to the key whose
/// digest is `key_digest`, as it does in FIPS 204. μ is 64 bytes, held as 16
/// field elements in the same way.
///
/// Use it together with [`MlDsa44VerifyExternalMu`] when the signature and μ
/// are checked in separate programs, for instance because the message is too
/// long for [`MlDsa44Verify`] to stay below 2^20 rows: μ costs one Keccak-f,
/// about 100,000 cycles, per 136 bytes of `tr ‖ 0 ‖ 0 ‖ message`. The snippet
/// checks the key digest itself, so that its μ is correct in a program that
/// does not verify the signature.
///
/// ### Pre-conditions
///
/// - the [Keccak tables](crate::hashing::keccak::tables) are in memory, as
///   [`insert_tables`](crate::hashing::keccak::tables::insert_tables) puts
///   them
/// - the expanded key, the [`ExpandedPublicKey::NUM_WORDS`] words from
///   `*expanded_key`, and the message, the `⌈message_len/4⌉` words from
///   `*message`, each lie in page 0 of the [memory convention](crate::memory),
///   or in a page that [`DynMalloc`] has already allocated, and overlap
///   neither each other nor the Keccak tables
///
/// ### Post-conditions
///
/// - `*mu` points to μ for the public key whose expansion has digest
///   `key_digest`, in a page allocated with [`DynMalloc`]
/// - the sponge state is overwritten
/// - two pages of memory are allocated with [`DynMalloc`]
#[derive(Debug, Default, Copy, Clone, Eq, PartialEq, Hash)]
pub struct MlDsa44Mu;

impl MlDsa44Mu {
    pub const KEY_DIGEST_MISMATCH_ERROR_ID: i128 = 670;
    pub const MESSAGE_NONZERO_PADDING_ERROR_ID: i128 = 671;
    pub const MESSAGE_TOO_LONG_ERROR_ID: i128 = 672;
}

/// Verifies an ML-DSA-44 signature on the message representative μ, and
/// crashes the VM if the signature is invalid. This is ML-DSA.Verify_internal
/// of FIPS 204 (Algorithm 8) from line 8 on, with μ given instead of computed.
///
/// ```text
/// BEFORE: _ *expanded_key *signature_witness [key_digest] *mu
/// AFTER:  _
/// ```
///
/// μ is the 64 bytes at `*mu`, held as 16 field elements as for the
/// [Keccak snippets](crate::hashing::keccak::hash). The public key enters
/// through its [expansion](ExpandedPublicKey), which lies in memory at
/// `*expanded_key` and is checked against `key_digest`. The signature enters
/// through its [witness](SignatureWitness), at `*signature_witness`.
///
/// The snippet does not check that μ belongs to the key. A signature on a
/// message is valid only for μ = H(tr ‖ 0 ‖ 0 ‖ message, 64) with the `tr` of
/// the same key, so the caller takes μ from [`MlDsa44Mu`] with the same
/// `key_digest`, or from another source that it trusts to compute μ so.
///
/// Use it when μ is computed in another program, for instance because the
/// message is long. Its cost does not depend on the message: every table stays
/// below 2^20 rows. For a message that one program both hashes and verifies,
/// [`MlDsa44Verify`] calls [`MlDsa44Mu`] and this snippet in turn.
///
/// The snippet checks the expansion against `key_digest` only: it does not
/// check that the expansion is that of any public key, for instance that `A`
/// is `ExpandA(ρ)` for some `ρ`. A caller takes `key_digest` from a source it
/// trusts, such as the public input or a constant of the program, never from
/// the prover, and whoever fixes `key_digest` computes it as
/// [`ExpandedPublicKey::digest`] of the key.
///
/// Instead of computing NTTs, the snippet checks, for each row `i`, the
/// identity of integer polynomials
///
/// ```text
/// Σ_j A_ij·z_j − c·t1_i·2^d = (X^256 + 1)·S_i + q·Q_i + w_i
/// ```
///
/// at a point `r` of the extension field that it derives from the witness
/// after the witness is fixed. Every coefficient of either side is below
/// 2^54 in absolute value, given that z, Q and w are in range, and that the
/// left side's coefficients are below 2^51. So the identity over F_p implies
/// the identity over the integers, which implies that `w_i` is `w'_Approx` of
/// the standard modulo q, whatever S_i is. The prover's `w_i` need not be
/// reduced: a representative off by q yields the same `w1`, because the high
/// bits are checked through `e`, whose range admits only the representatives
/// that `Decompose` maps alike.
///
/// ### Pre-conditions
///
/// - the [Keccak tables](crate::hashing::keccak::tables) are in memory, as
///   [`insert_tables`](crate::hashing::keccak::tables::insert_tables) puts
///   them
/// - the [high-bits table](HIGH_BITS_TABLE_ADDRESS) is in memory, as
///   [`insert_high_bits_table`] puts it
/// - the expanded key, the [`ExpandedPublicKey::NUM_WORDS`] words from
///   `*expanded_key`; the witness, the [`SignatureWitness::HINTS_OFFSET`]
///   words from `*signature_witness` and one word per hint after them; and μ,
///   the 16 words from `*mu`, each lie in page 0 of the
///   [memory convention](crate::memory), or in a page that [`DynMalloc`] has
///   already allocated, and overlap neither each other nor either table
///
/// The snippet writes only to its static memory. So none of the three regions
/// changes while the snippet runs, and each of its words reads the same every
/// time.
///
/// ### Post-conditions
///
/// - ML-DSA.Verify_internal(pk, μ, σ) holds, from line 8 on, for the public
///   key `pk` whose expansion has digest `key_digest` and the signature σ
///   whose witness is given
/// - the sponge state is overwritten
#[derive(Debug, Default, Copy, Clone, Eq, PartialEq, Hash)]
pub struct MlDsa44VerifyExternalMu;

impl MlDsa44VerifyExternalMu {
    pub const KEY_DIGEST_MISMATCH_ERROR_ID: i128 = 650;
    pub const C_TILDE_OUT_OF_RANGE_ERROR_ID: i128 = 651;
    pub const Z_OUT_OF_RANGE_ERROR_ID: i128 = 652;
    pub const Q_OUT_OF_RANGE_ERROR_ID: i128 = 653;
    pub const IDENTITY_FAILS_ERROR_ID: i128 = 654;
    pub const LOW_BITS_OUT_OF_RANGE_ERROR_ID: i128 = 655;
    pub const HIGH_BITS_OUT_OF_RANGE_ERROR_ID: i128 = 656;
    pub const TOO_MANY_HINTS_ERROR_ID: i128 = 657;
    pub const HINTS_NOT_INCREASING_ERROR_ID: i128 = 658;
    pub const C_TILDE_MISMATCH_ERROR_ID: i128 = 659;
    pub const HIGH_BITS_TABLE_DIFFERS_ERROR_ID: i128 = 660;
}

/// Addresses of the snippet's static memory.
#[derive(Debug, Clone, Copy)]
struct Layout {
    key_ptr: u64,
    witness_ptr: u64,
    /// The key digest followed by five zeros.
    key_block: u64,
    message_ptr: u64,
    message_len: u64,
    mu_ptr: u64,
    r: u64,
    r256_plus_1: u64,
    power_sum: u64,
    z_evals: u64,
    c_eval: u64,
    powers: u64,
    c: u64,
    w1_groups: u64,
    w1_bytes: u64,
    squeezed: u64,
    signs: u64,
    position: u64,
}

impl Layout {
    /// The words up to `message_len`, all that [`MlDsa44Mu`] uses.
    const MU_NUM_WORDS: u32 = 1 + 1 + 10 + 1 + 1;

    const NUM_WORDS: u32 =
        1 + 1 + 10 + 1 + 1 + 1 + 3 + 3 + 3 + 12 + 3 + 768 + 256 + 256 + 768 + 136 + 5 + 1;

    fn new(base: u64) -> Self {
        let mut next = base;
        let mut take = |n: u64| {
            let address = next;
            next += n;
            address
        };
        Self {
            key_ptr: take(1),
            witness_ptr: take(1),
            key_block: take(10),
            message_ptr: take(1),
            message_len: take(1),
            mu_ptr: take(1),
            r: take(3),
            r256_plus_1: take(3),
            power_sum: take(3),
            z_evals: take(12),
            c_eval: take(3),
            powers: take(768),
            c: take(256),
            w1_groups: take(256),
            w1_bytes: take(768),
            squeezed: take(136),
            signs: take(5),
            position: take(1),
        }
    }
}

struct Generator {
    entrypoint: String,
    layout: Layout,
    keccak: KeccakF1600,
    permute: String,
    check_tables: String,
    failures: Vec<i128>,
}

impl Generator {
    /// A generator with its own state and scratch space, and the first
    /// `layout_words` words of a [`Layout`] in static memory.
    fn new(entrypoint: String, library: &mut Library, layout_words: u32) -> Self {
        let keccak = KeccakF1600::allocate(library);
        let layout = Layout::new(library.kmalloc(layout_words).write_address().value());
        Self {
            entrypoint,
            layout,
            keccak,
            permute: library.import(Box::new(keccak)),
            check_tables: library.import(Box::new(CheckKeccakTables)),
            failures: vec![],
        }
    }

    /// The code of the routines, then of the failures that they call.
    fn finish(&self, routines: Vec<Vec<LabelledInstruction>>) -> Vec<LabelledInstruction> {
        let mut instructions = routines.concat();
        for error_id in &self.failures {
            let label = self.label(&format!("fail_{error_id}"));
            instructions.extend(triton_asm!({label}: push 0 assert error_id {error_id}));
        }
        instructions
    }

    fn label(&self, name: &str) -> String {
        format!("{}_{name}", self.entrypoint)
    }

    /// `_ → _ (*witness + offset)`
    fn witness_address(&self, offset: usize) -> Vec<LabelledInstruction> {
        triton_asm!(push {self.layout.witness_ptr} read_mem 1 pop 1 addi {offset})
    }

    /// `_ → _ (*key + offset)`
    fn key_address(&self, offset: usize) -> Vec<LabelledInstruction> {
        triton_asm!(push {self.layout.key_ptr} read_mem 1 pop 1 addi {offset})
    }

    /// Absorbs `n` words from the address on top of the stack, which it pops.
    fn absorb_words(n: usize) -> Vec<LabelledInstruction> {
        assert_eq!(0, n % tip5::RATE);
        let absorbs = triton_asm![sponge_absorb_mem; n / tip5::RATE];
        triton_asm!(
            push 0 push 0 push 0 push 0 pick 4
            {&absorbs}
            pop 5
        )
    }

    /// The label of a routine that fails the assertion with the given id.
    fn fail(&mut self, error_id: i128) -> String {
        if !self.failures.contains(&error_id) {
            self.failures.push(error_id);
        }
        self.label(&format!("fail_{error_id}"))
    }

    /// `_ v → _`, crashing unless `v ≤ max`. A value that is not a u32 crashes
    /// `lt`.
    fn check_at_most(&mut self, max: u64, error_id: i128) -> Vec<LabelledInstruction> {
        let fail = self.fail(error_id);
        triton_asm!(push {max} lt skiz call {fail})
    }

    /// `_ ptr_to_last → _`: checks `n` words, read downward from the pointer,
    /// for `0 ≤ v ≤ max`.
    fn range_check(&mut self, n: usize, max: u64, error_id: i128) -> Vec<LabelledInstruction> {
        let mut instructions = vec![];
        let mut left = n;
        while left > 0 {
            let k = left.min(5);
            instructions.extend(triton_asm!(read_mem {k} place {k}));
            for _ in 0..k {
                instructions.extend(self.check_at_most(max, error_id));
            }
            left -= k;
        }
        instructions.extend(triton_asm!(pop 1));
        instructions
    }

    /// `_ → _ [Σ_k coef_k·r^k]`, the coefficients `n` words from the address
    /// that `push_pointer` pushes.
    fn evaluate(
        &self,
        push_pointer: Vec<LabelledInstruction>,
        n: usize,
    ) -> Vec<LabelledInstruction> {
        let dot_steps = triton_asm![xb_dot_step; n];
        triton_asm!(
            push 0 push 0 push 0 push {self.layout.powers}
            {&push_pointer}
            {&dot_steps}
            pop 2
        )
    }

    fn load_xfe(address: u64) -> Vec<LabelledInstruction> {
        triton_asm!(push {address + 2} read_mem 3 pop 1)
    }

    fn store_xfe(address: u64) -> Vec<LabelledInstruction> {
        triton_asm!(push {address} write_mem 3 pop 1)
    }

    /// Writes the 32 bytes of c̃ into lanes 0 to 3 of the state.
    fn store_c_tilde_lane(&self, lane: usize) -> Vec<LabelledInstruction> {
        let slices = (0..8)
            .map(|j| self.keccak.slice_address(lane, j))
            .collect_vec();
        triton_asm!(
            {&self.witness_address(SignatureWitness::C_TILDE_OFFSET + 8 * lane + 7)}
            {&load_words_from_memory_pop_pointer(8)}
            {&write_words_to_scattered_addresses(&slices)}
        )
    }

    /// The entry of [`MlDsa44Verify`] if `with_message`, and of
    /// [`MlDsa44VerifyExternalMu`] otherwise. The former checks in the order
    /// of [`MlDsa44Mu`] and then the latter: the key and μ come before the
    /// high-bits table.
    fn entry(&self, with_message: bool) -> Vec<LabelledInstruction> {
        let l = self.layout;
        let names = [
            "challenge",
            "range_checks",
            "powers",
            "sample_in_ball",
            "identity",
            "use_hint",
        ];
        let push_digest = high_bits_table_digest()
            .iter()
            .rev()
            .flat_map(|d| triton_asm!(push { d.value() }))
            .collect_vec();
        let calls = names
            .iter()
            .flat_map(|name| triton_asm!(call { self.label(name) }))
            .collect_vec();
        let (store_message_or_mu, key_and_mu, key) = if with_message {
            (
                triton_asm!(
                    // _ *key *witness [key_digest] *message message_len
                    push {l.message_len} write_mem 1 pop 1
                    push {l.message_ptr} write_mem 1 pop 1
                ),
                triton_asm!(
                    call {self.label("check_key")}
                    call {self.label("mu")}
                    push {l.mu_ptr} write_mem 1 pop 1
                ),
                vec![],
            )
        } else {
            (
                triton_asm!(
                    // _ *key *witness [key_digest] *mu
                    push {l.mu_ptr} write_mem 1 pop 1
                ),
                vec![],
                triton_asm!(call {self.label("check_key")}),
            )
        };
        triton_asm!(
            {self.entrypoint}:
                {&store_message_or_mu}
                push {l.key_block} write_mem 5 pop 1
                {&write_zeros_to_memory(l.key_block + 5, 5)}
                push {l.witness_ptr} write_mem 1 pop 1
                push {l.key_ptr} write_mem 1 pop 1
                call {self.check_tables}
                {&key_and_mu}
                sponge_init
                push {HIGH_BITS_TABLE_ADDRESS}
                {&Self::absorb_words(NUM_HIGH_BITS_TABLE_WORDS)}
                sponge_squeeze
                {&push_digest}
                assert_vector error_id {MlDsa44VerifyExternalMu::HIGH_BITS_TABLE_DIFFERS_ERROR_ID}
                pop 5 pop 5
                {&key}
                {&calls}
                call {self.label("load_mu")}
                call {self.label("commitment")}
                return
        )
    }

    fn check_key(&self, error_id: i128) -> Vec<LabelledInstruction> {
        triton_asm!(
            {self.label("check_key")}:
                sponge_init
                {&self.key_address(0)}
                {&Self::absorb_words(ExpandedPublicKey::NUM_WORDS)}
                sponge_squeeze
                push {self.layout.key_block + 4} read_mem 5 pop 1
                assert_vector error_id {error_id}
                pop 5 pop 5
                return
        )
    }

    fn challenge(&self) -> Vec<LabelledInstruction> {
        let l = self.layout;
        triton_asm!(
            {self.label("challenge")}:
                sponge_init
                push {l.key_block}
                {&Self::absorb_words(tip5::RATE)}
                {&self.witness_address(0)}
                {&Self::absorb_words(SignatureWitness::NUM_CHALLENGE_WORDS)}
                sponge_squeeze
                {&Self::store_xfe(l.r)}
                pop 5 pop 2
                return
        )
    }

    fn range_checks(&mut self) -> Vec<LabelledInstruction> {
        let c_tilde = self.range_check(
            32,
            255,
            MlDsa44VerifyExternalMu::C_TILDE_OUT_OF_RANGE_ERROR_ID,
        );
        let z = self.range_check(
            L * N,
            2 * SignatureWitness::Z_SHIFT,
            MlDsa44VerifyExternalMu::Z_OUT_OF_RANGE_ERROR_ID,
        );
        let q = self.range_check(
            K * N,
            2 * SignatureWitness::Q_SHIFT - 1,
            MlDsa44VerifyExternalMu::Q_OUT_OF_RANGE_ERROR_ID,
        );
        triton_asm!(
            {self.label("range_checks")}:
                {&self.witness_address(SignatureWitness::C_TILDE_OFFSET + 31)}
                {&c_tilde}
                {&self.witness_address(SignatureWitness::Z_OFFSET + L * N - 1)}
                {&z}
                {&self.witness_address(SignatureWitness::Q_OFFSET + K * N - 1)}
                {&q}
                return
        )
    }

    fn powers(&self) -> Vec<LabelledInstruction> {
        let l = self.layout;
        let store_powers = (0..N)
            .flat_map(|k| {
                triton_asm!(
                    dup 2 dup 2 dup 2
                    push {l.powers + 3 * k as u64} write_mem 3 pop 1
                    dup 5 dup 5 dup 5 xx_mul
                )
            })
            .collect_vec();
        triton_asm!(
            {self.label("powers")}:
                {&Self::load_xfe(l.r)}
                push 0 push 0 push 1
                // _ [r] [r^k]
                {&store_powers}
                // _ [r] [r^256]
                dup 2 dup 2 dup 2 addi 1
                push {l.r256_plus_1} write_mem 3 pop 1
                // The sum of the powers, (r^256 - 1)/(r - 1), by which the
                // shifts of z and Q are taken out of their evaluations.
                addi {neg(1)} pick 5 pick 5 pick 5 addi {neg(1)} x_invert xx_mul
                {&Self::store_xfe(l.power_sum)}
                return
        )
    }

    fn sample_in_ball(&self) -> Vec<LabelledInstruction> {
        let l = self.layout;
        let c_tilde = (0..4)
            .flat_map(|lane| self.store_c_tilde_lane(lane))
            .collect_vec();
        // Padding after the 32 bytes of c̃.
        let padding = self.keccak.pad(SHAKE256_RATE, 32, SHAKE256_PADDING_BYTE);
        let draws = (N - TAU..N)
            .flat_map(|i| {
                let k = i + TAU - N;
                let (byte, b) = (k / 8, k % 8);
                let factor = (-bfe!(2) * bfe!(1u64 << b).inverse()).value();
                triton_asm!(
                    push {i}
                    call {self.label("draw")}
                    // _ i j
                    dup 0 addi {l.c} read_mem 1 pop 1
                    push {l.c + i as u64} write_mem 1 pop 1
                    push {l.signs + byte as u64} read_mem 1 pop 1
                    push {1u64 << b} and
                    push {factor} mul
                    addi 1
                    // _ i j sign
                    pick 1 addi {l.c} write_mem 1 pop 2
                )
            })
            .collect_vec();
        let lower_lanes = (0..8)
            .flat_map(|lane| {
                self.keccak
                    .squeeze_lane_to_bytes(l.squeezed + 8 * lane as u64, lane)
            })
            .collect_vec();
        let upper_lanes = (8..SHAKE256_RATE / 8)
            .flat_map(|lane| {
                self.keccak
                    .squeeze_lane_to_bytes(l.squeezed + 8 * lane as u64, lane)
            })
            .collect_vec();
        triton_asm!(
            {self.label("sample_in_ball")}:
                {&write_zeros_to_memory(self.keccak.state_address(), 200)}
                {&write_zeros_to_memory(l.c, N)}
                {&c_tilde}
                {&padding}
                call {self.permute}
                call {self.label("lower_lanes")}
                push {l.squeezed + 4}
                {&load_words_from_memory_pop_pointer(5)}
                push {l.signs}
                {&write_words_to_memory_pop_pointer(5)}
                push 8 push {l.position} write_mem 1 pop 1
                {&draws}
                return

            // _ i → _ i j, the next squeezed byte j ≤ i
            {self.label("draw")}:
                call {self.label("next_byte")}
                dup 1 addi 1 dup 1 lt
                skiz return
                pop 1
                recurse

            // _ → _ byte
            {self.label("next_byte")}:
                push {l.position} read_mem 1 pop 1
                dup 0 push {SHAKE256_RATE} eq
                skiz call {self.label("refill")}
                dup 0 push 64 eq
                skiz call {self.label("upper_lanes")}
                dup 0 addi 1 push {l.position} write_mem 1 pop 1
                addi {l.squeezed} read_mem 1 pop 1
                return

            {self.label("refill")}:
                pop 1
                call {self.permute}
                call {self.label("lower_lanes")}
                push 0
                return

            {self.label("lower_lanes")}:
                {&lower_lanes}
                return

            {self.label("upper_lanes")}:
                {&upper_lanes}
                return
        )
    }

    fn identity(&self) -> Vec<LabelledInstruction> {
        let l = self.layout;
        let z_evals = (0..L)
            .flat_map(|j| {
                let offset = SignatureWitness::Z_OFFSET + N * j;
                triton_asm!(
                    {&self.evaluate(self.witness_address(offset), N)}
                    {&Self::load_xfe(l.power_sum)}
                    push {neg(SignatureWitness::Z_SHIFT)} xb_mul xx_add
                    {&Self::store_xfe(l.z_evals + 3 * j as u64)}
                )
            })
            .collect_vec();
        let id = MlDsa44VerifyExternalMu::IDENTITY_FAILS_ERROR_ID;
        let rows = (0..K)
            .flat_map(|i| {
                let a_times_z = (0..L)
                    .flat_map(|j| {
                        let offset = ExpandedPublicKey::A_OFFSET + N * (L * i + j);
                        let mut instructions = triton_asm!(
                            {&self.evaluate(self.key_address(offset), N)}
                            {&Self::load_xfe(l.z_evals + 3 * j as u64)}
                            xx_mul
                        );
                        if j > 0 {
                            instructions.extend(triton_asm!(xx_add));
                        }
                        instructions
                    })
                    .collect_vec();
                let t1 = ExpandedPublicKey::T1_OFFSET + N * i;
                let w = SignatureWitness::W_OFFSET + N * i;
                let s = SignatureWitness::S_OFFSET + (N - 1) * i;
                let q = SignatureWitness::Q_OFFSET + N * i;
                let assert_zero = vec![triton_asm!(push 0 eq assert error_id {id}); 3].concat();
                triton_asm!(
                    {&a_times_z}
                    {&self.evaluate(self.key_address(t1), N)}
                    {&Self::load_xfe(l.c_eval)}
                    xx_mul push {neg(1)} xb_mul xx_add
                    {&self.evaluate(self.witness_address(w), N)}
                    push {neg(1)} xb_mul xx_add
                    {&self.evaluate(self.witness_address(s), N - 1)}
                    {&Self::load_xfe(l.r256_plus_1)}
                    xx_mul push {neg(1)} xb_mul xx_add
                    {&self.evaluate(self.witness_address(q), N)}
                    {&Self::load_xfe(l.power_sum)}
                    push {neg(SignatureWitness::Q_SHIFT)} xb_mul xx_add
                    push {neg(u64::from(Q))} xb_mul xx_add
                    {&assert_zero}
                )
            })
            .collect_vec();
        triton_asm!(
            {self.label("identity")}:
                {&z_evals}
                {&self.evaluate(triton_asm!(push {l.c}), N)}
                {&Self::store_xfe(l.c_eval)}
                {&rows}
                return
        )
    }

    fn use_hint(&mut self) -> Vec<LabelledInstruction> {
        let l = self.layout;
        let two_gamma2 = 2 * u64::from(GAMMA2);
        let low_id = MlDsa44VerifyExternalMu::LOW_BITS_OUT_OF_RANGE_ERROR_ID;
        let high_id = MlDsa44VerifyExternalMu::HIGH_BITS_OUT_OF_RANGE_ERROR_ID;
        let order_id = MlDsa44VerifyExternalMu::HINTS_NOT_INCREASING_ERROR_ID;
        let w_to_e = SignatureWitness::E_OFFSET - SignatureWitness::W_OFFSET;

        // For every coefficient r with high bits e: r - 2·γ2·e lies in
        // (-γ2, γ2], e ≤ 44, and w1 = e mod 44. Four coefficients make a
        // 24-bit group of w1Encode, which the scaled table sums.
        let t = HIGH_BITS_TABLE_ADDRESS;
        let mut groups = vec![];
        for g in 0..K * N / 4 {
            // _ *r_{4g+3}
            groups.extend(triton_asm!(
                dup 0 read_mem 4 pop 1
                dup 4 addi {w_to_e} read_mem 4 pop 1
            ));
            for m in 0..4 {
                // _ *r r_3..r_m e_3..e_m, top last, and below e_m for m > 0
                // the group so far
                if m > 0 {
                    groups.extend(triton_asm!(pick 1));
                }
                let check_low = self.check_at_most(two_gamma2 - 1, low_id);
                let r_depth = if m == 0 { 5 } else { 6 - m };
                groups.extend(triton_asm!(
                    dup 0 push {neg(two_gamma2)} mul
                    pick {r_depth} add
                    addi {GAMMA2 - 1}
                    {&check_low}
                    push {NUM_E} dup 1 lt assert error_id {high_id}
                    addi {t + (SCALED_OFFSET + NUM_E * m) as u64} read_mem 1 pop 1
                ));
                if m > 0 {
                    groups.extend(triton_asm!(add));
                }
            }
            // _ *r group
            groups.extend(triton_asm!(
                push {l.w1_groups + g as u64} write_mem 1 pop 1
                addi 4
            ));
        }

        // The bytes of w1Encode, three per group, written five groups at a
        // time.
        let mut w1_bytes = vec![];
        for chunk in (0..K * N / 4).collect_vec().chunks(5) {
            for &g in chunk.iter().rev() {
                w1_bytes.extend(triton_asm!(
                    push {l.w1_groups + g as u64} read_mem 1 pop 1
                    push 256 pick 1 div_mod pick 1
                    push 256 pick 1 div_mod pick 1
                    swap 2
                ));
            }
            w1_bytes.extend(triton_asm!(push {l.w1_bytes + 3 * chunk[0] as u64}));
            let mut left = 3 * chunk.len();
            while left > 0 {
                let n = left.min(5);
                w1_bytes.extend(triton_asm!(write_mem { n }));
                left -= n;
            }
            w1_bytes.extend(triton_asm!(pop 1));
        }

        let check_gap = self.check_at_most(K as u64 * N as u64 - 1, order_id);
        let check_first = self.check_at_most(K as u64 * N as u64, order_id);
        triton_asm!(
            {self.label("use_hint")}:
                {&self.witness_address(SignatureWitness::W_OFFSET + 3)}
                {&groups}
                pop 1
                // The hints: a count of at most ω, then strictly increasing
                // positions, which the loop reads from the last.
                {&self.witness_address(SignatureWitness::HINT_COUNT_OFFSET)}
                read_mem 1 pop 1
                push {OMEGA + 1} dup 1 lt
                assert error_id {MlDsa44VerifyExternalMu::TOO_MANY_HINTS_ERROR_ID}
                {&self.witness_address(SignatureWitness::HINTS_OFFSET - 1)}
                dup 0 pick 2 add
                push 0 push 0 push 0 push 0 push {K * N}
                // _ *before_first *last 0 0 0 0 next, and next is the
                // position after the hint at *last
                dup 6 dup 6 eq push 0 eq
                skiz call {self.label("hint_loop")}
                // Every gap was at most 1024 and the first position is not
                // negative, so every position lies in [0, 1024).
                {&check_first}
                pop 5 pop 1
                {&w1_bytes}
                return

            // _ *before_first *hint 0 0 0 0 next
            //   → _ *before_first *before_first 0 0 0 0 first
            // Each hint n must have 0 ≤ next − n − 1 < 1024, and w1[n] becomes
            // (w1[n] ± 1) mod 44, by the sign of r0, in its group.
            {self.label("hint_loop")}:
                pick 5 read_mem 1 place 6
                // _ *before_first *hint' 0 0 0 0 next n
                dup 0 push {neg(1)} mul dup 2 add addi {neg(1)}
                {&check_gap}
                pick 1 pop 1
                // _ *before_first *hint' 0 0 0 0 n
                dup 0 dup 0
                {&self.witness_address(SignatureWitness::W_OFFSET)}
                add read_mem 1 addi {w_to_e + 1} read_mem 1 pop 1
                // _ … n n r e
                dup 0 addi {t + THRESHOLD_OFFSET as u64} read_mem 1 pop 1
                pick 2 add
                // _ … n n e (r0 + γ2 + 1), and r0 > 0 iff γ2 + 1 < r0 + γ2 + 1
                push {GAMMA2 + 1} lt
                pick 1 push 2 mul add
                pick 1 push 4 swap 1 div_mod
                // _ … n (2·e + [r0 > 0]) (n div 4) (n mod 4)
                push {2 * NUM_E} mul pick 2 add
                addi {t + DIFF_OFFSET as u64} read_mem 1 pop 1
                // _ … n g diff
                dup 1 addi {l.w1_groups} read_mem 1 pop 1 add
                pick 1 addi {l.w1_groups} write_mem 1 pop 1
                recurse_or_return
        )
    }

    /// The entry of [`MlDsa44Mu`].
    fn mu_entry(&self) -> Vec<LabelledInstruction> {
        let l = self.layout;
        triton_asm!(
            {self.entrypoint}:
                // _ *key [key_digest] *message message_len
                push {l.message_len} write_mem 1 pop 1
                push {l.message_ptr} write_mem 1 pop 1
                push {l.key_block} write_mem 5 pop 1
                {&write_zeros_to_memory(l.key_block + 5, 5)}
                push {l.key_ptr} write_mem 1 pop 1
                call {self.check_tables}
                call {self.label("check_key")}
                call {self.label("mu")}
                return
        )
    }

    /// `_ → _`: puts μ, the 64 bytes at `*mu`, into lanes 0 to 7 of a state
    /// that is otherwise zero. Crashes unless its 16 words are u32s.
    fn load_mu(&self) -> Vec<LabelledInstruction> {
        let mu_lanes = (0..8)
            .flat_map(|lane| self.keccak.absorb_lane_from_words(lane))
            .collect_vec();
        triton_asm!(
            {self.label("load_mu")}:
                {&write_zeros_to_memory(self.keccak.state_address(), 200)}
                push {self.layout.mu_ptr} read_mem 1 pop 1
                {&mu_lanes}
                pop 1
                return
        )
    }

    /// `_ → _ *mu`
    fn mu(&mut self, library: &mut Library) -> Vec<LabelledInstruction> {
        // SHAKE256 takes the length of tr ‖ 0 ‖ 0 ‖ message, which must stay
        // below 2^32 − 3.
        let check_length = self.check_at_most(
            u64::from(u32::MAX) - 69,
            MlDsa44Mu::MESSAGE_TOO_LONG_ERROR_ID,
        );
        let l = self.layout;
        let shake256 = library.import(Box::new(Shake256::new(self.keccak)));
        let dyn_malloc = library.import(Box::new(DynMalloc));
        let tr = (0..8)
            .flat_map(|lane| {
                let slices = (0..8)
                    .map(|j| self.keccak.slice_address(lane, j))
                    .collect_vec();
                triton_asm!(
                    {&self.key_address(ExpandedPublicKey::TR_OFFSET + 8 * lane + 7)}
                    {&load_words_from_memory_pop_pointer(8)}
                    {&write_words_to_scattered_addresses(&slices)}
                )
            })
            .collect_vec();
        let tr_words = (0..8)
            .flat_map(|lane| self.keccak.squeeze_lane_to_words(lane))
            .collect_vec();
        let padding_id = MlDsa44Mu::MESSAGE_NONZERO_PADDING_ERROR_ID;
        triton_asm!(
            {self.label("mu")}:
                // μ = SHAKE256(tr ‖ 0 ‖ 0 ‖ message, 64), of which the input
                // is written to new memory: tr by way of the state, and the
                // message shifted by the two zero bytes.
                push {l.message_len} read_mem 1 pop 1
                {&check_length}
                {&write_zeros_to_memory(self.keccak.state_address(), 200)}
                {&tr}
                call {dyn_malloc}
                dup 0
                {&tr_words}
                // _ *input *dst
                push {l.message_len} read_mem 1 pop 1
                skiz call {self.label("check_message_padding")}
                push {l.message_ptr} read_mem 1 pop 1
                push {l.message_len} read_mem 1 pop 1
                addi 3 push 4 swap 1 div_mod pop 1
                push 0
                // _ *input *dst *message ⌈message_len/4⌉ carry
                call {self.label("shift_message")}
                place 2 pop 2
                pick 1 write_mem 1 pop 1
                // _ *input
                push {l.message_len} read_mem 1 pop 1 addi 66
                push 64
                call {shake256}
                pop 1
                // _ *μ
                return

            // _ *dst *message k carry → _ (*dst + k) (*message + k) 0 carry',
            // where carry is the value of the two bytes that the previous
            // message word passes to the next destination word. Crashes
            // unless the message words are u32s.
            {self.label("shift_message")}:
                dup 1 push 0 eq
                skiz return
                dup 2 read_mem 1 pop 1
                push 65536 swap 1 div_mod
                // _ *dst *message k carry hi lo
                push 65536 mul pick 2 add
                // _ *dst *message k hi word
                pick 4 write_mem 1
                place 3
                pick 2 addi 1 place 2
                pick 1 addi {neg(1)} place 1
                recurse

            // _ → _, for a message of length n ≥ 1: crashes unless the last
            // message word, a u32, times 256^(3 − (n − 1) mod 4) is below 2^32.
            {self.label("check_message_padding")}:
                push {l.message_ptr} read_mem 1 pop 1
                push {l.message_len} read_mem 1 pop 1
                addi {neg(1)} push 4 swap 1 div_mod
                // _ *message ⌊(n − 1)/4⌋ ((n − 1) mod 4)
                push {neg(1)} mul addi 3 push 256 pow
                pick 2 pick 2 add read_mem 1 pop 1
                mul split pop 1
                push 0 eq assert error_id {padding_id}
                return
        )
    }

    fn commitment(&self) -> Vec<LabelledInstruction> {
        let l = self.layout;
        let mut instructions = triton_asm!({self.label("commitment")}:);
        // The state holds μ in lanes 0 to 7. The first block is μ followed by
        // the first 72 bytes of w1Encode(w1).
        for lane in 17..25 {
            for j in 0..8 {
                let address = self.keccak.slice_address(lane, j);
                instructions.extend(triton_asm!(push 0 push {address} write_mem 1 pop 1));
            }
        }
        let rate_lanes = SHAKE256_RATE / 8;
        for lane in 8..rate_lanes {
            instructions.extend(self.keccak.absorb_lane_from_bytes(
                l.w1_bytes + 8 * (lane as u64 - 8),
                lane,
                false,
            ));
        }
        instructions.extend(triton_asm!(call {self.permute}));
        let num_bytes = K * N * 6 / 8;
        let mut offset = 8 * (rate_lanes - 8);
        // The padding goes into a block that w1Encode leaves partly empty.
        assert_ne!(0, (num_bytes - offset) % SHAKE256_RATE);
        while offset < num_bytes {
            let lanes = ((num_bytes - offset) / 8).min(rate_lanes);
            for lane in 0..lanes {
                instructions.extend(self.keccak.absorb_lane_from_bytes(
                    l.w1_bytes + (offset + 8 * lane) as u64,
                    lane,
                    true,
                ));
            }
            offset += 8 * lanes;
            if lanes < rate_lanes {
                instructions.extend(self.keccak.pad(
                    SHAKE256_RATE,
                    8 * lanes,
                    SHAKE256_PADDING_BYTE,
                ));
            }
            instructions.extend(triton_asm!(call {self.permute}));
        }
        // c̃' is the first 32 bytes, lanes 0 to 3, compared slice by slice.
        let id = MlDsa44VerifyExternalMu::C_TILDE_MISMATCH_ERROR_ID;
        for lane in 0..4 {
            instructions.extend(triton_asm!(
                {&self.witness_address(SignatureWitness::C_TILDE_OFFSET + 8 * lane + 7)}
                {&load_words_from_memory_pop_pointer(8)}
            ));
            for j in 0..8 {
                let address = self.keccak.slice_address(lane, j);
                instructions.extend(triton_asm!(
                    push {address} read_mem 1 pop 1 eq assert error_id {id}
                ));
            }
        }
        instructions.extend(triton_asm!(return));
        instructions
    }
}

impl BasicSnippet for MlDsa44Verify {
    fn parameters(&self) -> Vec<(DataType, String)> {
        vec![
            (DataType::VoidPointer, "*expanded_key".to_string()),
            (DataType::VoidPointer, "*signature_witness".to_string()),
            (DataType::Digest, "key_digest".to_string()),
            (DataType::VoidPointer, "*message".to_string()),
            (DataType::U32, "message_len".to_string()),
        ]
    }

    fn return_values(&self) -> Vec<(DataType, String)> {
        vec![]
    }

    fn entrypoint(&self) -> String {
        "tasmlib_ml_dsa_verify_ml_dsa_44".to_string()
    }

    fn code(&self, library: &mut Library) -> Vec<LabelledInstruction> {
        // The routines of both snippets, on one state and one layout, with
        // the tables and the key checked once.
        let mut generator = Generator::new(self.entrypoint(), library, Layout::NUM_WORDS);
        let routines = vec![
            generator.entry(true),
            generator.check_key(MlDsa44Mu::KEY_DIGEST_MISMATCH_ERROR_ID),
            generator.challenge(),
            generator.range_checks(),
            generator.powers(),
            generator.sample_in_ball(),
            generator.identity(),
            generator.use_hint(),
            generator.mu(library),
            generator.load_mu(),
            generator.commitment(),
        ];
        generator.finish(routines)
    }
}

impl BasicSnippet for MlDsa44Mu {
    fn parameters(&self) -> Vec<(DataType, String)> {
        vec![
            (DataType::VoidPointer, "*expanded_key".to_string()),
            (DataType::Digest, "key_digest".to_string()),
            (DataType::VoidPointer, "*message".to_string()),
            (DataType::U32, "message_len".to_string()),
        ]
    }

    fn return_values(&self) -> Vec<(DataType, String)> {
        vec![(DataType::VoidPointer, "*mu".to_string())]
    }

    fn entrypoint(&self) -> String {
        "tasmlib_ml_dsa_mu_ml_dsa_44".to_string()
    }

    fn code(&self, library: &mut Library) -> Vec<LabelledInstruction> {
        let mut generator = Generator::new(self.entrypoint(), library, Layout::MU_NUM_WORDS);
        let routines = vec![
            generator.mu_entry(),
            generator.check_key(Self::KEY_DIGEST_MISMATCH_ERROR_ID),
            generator.mu(library),
        ];
        generator.finish(routines)
    }
}

impl BasicSnippet for MlDsa44VerifyExternalMu {
    fn parameters(&self) -> Vec<(DataType, String)> {
        vec![
            (DataType::VoidPointer, "*expanded_key".to_string()),
            (DataType::VoidPointer, "*signature_witness".to_string()),
            (DataType::Digest, "key_digest".to_string()),
            (DataType::VoidPointer, "*mu".to_string()),
        ]
    }

    fn return_values(&self) -> Vec<(DataType, String)> {
        vec![]
    }

    fn entrypoint(&self) -> String {
        "tasmlib_ml_dsa_verify_external_mu_ml_dsa_44".to_string()
    }

    fn code(&self, library: &mut Library) -> Vec<LabelledInstruction> {
        let mut generator = Generator::new(self.entrypoint(), library, Layout::NUM_WORDS);
        let routines = vec![
            generator.entry(false),
            generator.check_key(Self::KEY_DIGEST_MISMATCH_ERROR_ID),
            generator.challenge(),
            generator.range_checks(),
            generator.powers(),
            generator.sample_in_ball(),
            generator.identity(),
            generator.use_hint(),
            generator.load_mu(),
            generator.commitment(),
        ];
        generator.finish(routines)
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use fips204::ml_dsa_44;
    use fips204::traits::KeyGen;
    use fips204::traits::SerDes;
    use fips204::traits::Signer;
    use fips204::traits::Verifier;

    use proptest::collection::vec;

    use super::*;
    use crate::hashing::keccak::deinterleave;
    use crate::hashing::keccak::interleave;
    use crate::hashing::keccak::shake256;
    use crate::hashing::keccak::tables::insert_tables;
    use crate::linker::execute_bench;
    use crate::memory::dyn_malloc::DYN_MALLOC_ADDRESS;
    use crate::memory::dyn_malloc::DYN_MALLOC_FIRST_PAGE;
    use crate::ml_dsa::helper::Poly;
    use crate::ml_dsa::helper::decompose;
    use crate::snippet_bencher::BenchmarkCase;
    use crate::snippet_bencher::NamedBenchmarkResult;
    use crate::snippet_bencher::write_benchmarks;
    use crate::test_prelude::*;

    const KEY_POINTER: u64 = 1 << 20;
    const WITNESS_POINTER: u64 = 1 << 21;
    const MESSAGE_POINTER: u64 = 1 << 22;
    const MU_POINTER: u64 = 1 << 23;

    /// The field elements that hold a byte string.
    fn to_words(bytes: &[u8]) -> Vec<BFieldElement> {
        bytes
            .chunks(4)
            .map(|chunk| {
                let mut word = [0; 4];
                word[..chunk.len()].copy_from_slice(chunk);
                bfe!(u32::from_le_bytes(word))
            })
            .collect()
    }

    struct Instance {
        key: ExpandedPublicKey,
        witness: SignatureWitness,
        key_digest: Digest,
        message: Vec<u8>,
    }

    impl Instance {
        /// A signature by the key of `seed` on a message of `40 + seed` bytes.
        fn new(seed: u8) -> Self {
            let message = (0..40 + usize::from(seed))
                .map(|i| (i as u8) ^ seed)
                .collect_vec();
            let (pk, sk) = ml_dsa_44::KG::keygen_from_seed(&[seed; 32]);
            let sig = sk
                .try_sign_with_seed(&[seed ^ 0x5a; 32], &message, &[])
                .unwrap();
            assert!(pk.verify(&message, &sig, &[]));
            Self::from_signature(&pk.into_bytes(), message, &sig).unwrap()
        }

        /// The instance of a signature, or `None` if it has no witness.
        fn from_signature(
            pk: &[u8; PUBLIC_KEY_LEN],
            message: Vec<u8>,
            sig: &[u8; SIGNATURE_LEN],
        ) -> Option<Self> {
            let key = ExpandedPublicKey::new(pk);
            let witness = SignatureWitness::new(&key, sig)?;
            let key_digest = key.digest();
            Some(Self {
                key,
                witness,
                key_digest,
                message,
            })
        }

        fn stack(&self) -> Vec<BFieldElement> {
            let mut stack = MlDsa44Verify.init_stack_for_isolated_run();
            stack.push(bfe!(KEY_POINTER));
            stack.push(bfe!(WITNESS_POINTER));
            stack.extend(self.key_digest.values().iter().rev());
            stack.push(bfe!(MESSAGE_POINTER));
            stack.push(bfe!(self.message.len() as u64));
            stack
        }

        fn nondeterminism(&self) -> NonDeterminism {
            let mut memory = HashMap::new();
            insert_tables(&mut memory);
            insert_high_bits_table(&mut memory);
            for (i, word) in self.key.encode().into_iter().enumerate() {
                memory.insert(bfe!(KEY_POINTER + i as u64), word);
            }
            for (i, word) in self.witness.encode().into_iter().enumerate() {
                memory.insert(bfe!(WITNESS_POINTER + i as u64), word);
            }
            for (i, word) in to_words(&self.message).into_iter().enumerate() {
                memory.insert(bfe!(MESSAGE_POINTER + i as u64), word);
            }
            NonDeterminism::default().with_ram(memory)
        }

        fn run(&self) -> Result<VMState, InstructionError> {
            self.run_with(self.nondeterminism())
        }

        fn run_with(&self, nondeterminism: NonDeterminism) -> Result<VMState, InstructionError> {
            let code = MlDsa44Verify.link_for_isolated_run();
            let mut state =
                VMState::new(Program::new(&code), PublicInput::default(), nondeterminism);
            state.op_stack.stack = self.stack();
            state.run().map(|()| state)
        }

        /// Runs the snippet on the encoded witness as `patch` changes it,
        /// which reaches words that [`SignatureWitness`] cannot hold, such as
        /// a negative `w`.
        fn run_patched(
            &self,
            patch: impl FnOnce(&mut [BFieldElement]),
        ) -> Result<VMState, InstructionError> {
            let mut words = self.witness.encode();
            patch(&mut words);
            let mut nondeterminism = self.nondeterminism();
            for (i, word) in words.into_iter().enumerate() {
                nondeterminism
                    .ram
                    .insert(bfe!(WITNESS_POINTER + i as u64), word);
            }
            self.run_with(nondeterminism)
        }

        /// Runs the snippet with every `w` that has a second representative
        /// in `[−γ2 + 1, q − 1 + γ2]` moved to it: `w < γ2` becomes `w + q`
        /// with `e = 44`, and `w > q − γ2` becomes `w − q` with `e = 0`, each
        /// with `Q` adjusted by one.
        fn run_with_other_representatives(&self) -> Result<VMState, InstructionError> {
            let upper = self.positions(|w| w < GAMMA2);
            let lower = self.positions(|w| w > Q - GAMMA2);
            let index = |offset: usize, (i, k): (usize, usize)| offset + N * i + k;
            self.run_patched(|words| {
                let (w, q, e) = (
                    SignatureWitness::W_OFFSET,
                    SignatureWitness::Q_OFFSET,
                    SignatureWitness::E_OFFSET,
                );
                for &p in &upper {
                    words[index(w, p)] += bfe!(Q);
                    words[index(q, p)] -= bfe!(1);
                    words[index(e, p)] = bfe!(W1_RANGE);
                }
                for &p in &lower {
                    words[index(w, p)] -= bfe!(Q);
                    words[index(q, p)] += bfe!(1);
                    words[index(e, p)] = bfe!(0);
                }
            })
        }

        /// An instance whose key is not the expansion of any public key: `A`
        /// is the identity matrix and `t1` is zero, so `w_i = z_i mod q`, and
        /// `z` puts any value of `[−Z_SHIFT, Z_SHIFT]` into `w`. Its `c̃` is
        /// computed with FIPS 204's `UseHint` and `w1Encode`.
        fn synthetic(z: [[i32; N]; L], hints: Vec<u16>, message: Vec<u8>) -> Self {
            let to_lanes = |bytes: &[u8]| {
                bytes
                    .chunks(8)
                    .map(|lane| interleave(u64::from_le_bytes(lane.try_into().unwrap())))
                    .collect_vec()
            };
            let tr_bytes = [7; 64];
            let one: Poly = std::array::from_fn(|k| u32::from(k == 0));
            let key = ExpandedPublicKey {
                tr: to_lanes(&tr_bytes).try_into().unwrap(),
                a: std::array::from_fn(|i| {
                    std::array::from_fn(|j| if i == j { one } else { [0; N] })
                }),
                t1_shifted: [[0; N]; K],
            };

            let w: [Poly; K] = z.map(|zi| zi.map(|c| c.rem_euclid(Q as i32) as u32));
            let q = std::array::from_fn(|i| {
                std::array::from_fn(|k| (i64::from(z[i][k]) - i64::from(w[i][k])) / i64::from(Q))
            });
            let e = w.map(|wi| {
                wi.map(|r| {
                    let (_, r0) = decompose(r);
                    ((i64::from(r) - i64::from(r0)) / i64::from(2 * GAMMA2)) as u32
                })
            });

            // FIPS 204: UseHint, w1Encode, μ = H(tr ‖ 0 ‖ 0 ‖ M), c̃ = H(μ ‖ w1Encode(w1)).
            let w1 = (0..K * N)
                .map(|n| {
                    let (r1, r0) = decompose(w[n / N][n % N]);
                    match (hints.contains(&(n as u16)), r0 > 0) {
                        (false, _) => r1,
                        (true, true) => (r1 + 1) % W1_RANGE,
                        (true, false) => (r1 + W1_RANGE - 1) % W1_RANGE,
                    }
                })
                .collect_vec();
            let w1_encoded = (0..K * N * 6 / 8)
                .map(|byte| {
                    (0..8)
                        .map(|t| ((w1[(8 * byte + t) / 6] >> ((8 * byte + t) % 6)) & 1) << t)
                        .sum::<u32>() as u8
                })
                .collect_vec();
            let mu = shake256(&[&tr_bytes[..], &[0, 0], &message].concat(), 64);
            let c_tilde = shake256(&[mu, w1_encoded].concat(), C_TILDE_LEN);

            let witness = SignatureWitness {
                c_tilde: to_lanes(&c_tilde).try_into().unwrap(),
                z,
                w,
                s: [[0; N - 1]; K],
                q,
                e,
                hints,
            };
            let key_digest = key.digest();
            Self {
                key,
                witness,
                key_digest,
                message,
            }
        }

        /// The positions `(i, k)` whose `w` satisfies `predicate`.
        fn positions(&self, predicate: impl Fn(u32) -> bool) -> Vec<(usize, usize)> {
            (0..K)
                .flat_map(|i| (0..N).map(move |k| (i, k)))
                .filter(|&(i, k)| predicate(self.witness.w[i][k]))
                .collect()
        }

        fn is_hinted(&self, (i, k): (usize, usize)) -> bool {
            self.witness.hints.contains(&((N * i + k) as u16))
        }

        fn assert_fails_with(&self, error_id: i128) {
            assert_fails_with(self.run(), error_id);
        }

        fn assert_fails_with_or_non_u32(&self, error_id: i128) {
            assert_fails_with_or_non_u32(self.run(), error_id);
        }

        /// μ = H(tr ‖ 0 ‖ 0 ‖ message, 64), by FIPS 202.
        fn mu(&self) -> Vec<u8> {
            let tr = self
                .key
                .tr
                .iter()
                .flat_map(|&lane| deinterleave(lane).to_le_bytes())
                .collect_vec();
            shake256(&[&tr[..], &[0, 0], &self.message].concat(), 64)
        }

        fn key_digest_words(&self) -> Vec<BFieldElement> {
            self.key_digest.values().iter().rev().copied().collect()
        }

        fn run_mu(&self) -> Result<VMState, InstructionError> {
            let arguments = [
                vec![bfe!(KEY_POINTER)],
                self.key_digest_words(),
                vec![bfe!(MESSAGE_POINTER), bfe!(self.message.len() as u64)],
            ]
            .concat();
            run_snippet(MlDsa44Mu, &arguments, self.nondeterminism())
        }

        /// Runs [`MlDsa44VerifyExternalMu`] with `mu` at [`MU_POINTER`].
        fn run_external_mu(&self, mu: &[u8]) -> Result<VMState, InstructionError> {
            let mut nondeterminism = self.nondeterminism();
            for (i, word) in to_words(mu).into_iter().enumerate() {
                nondeterminism.ram.insert(bfe!(MU_POINTER + i as u64), word);
            }
            let arguments = [
                vec![bfe!(KEY_POINTER), bfe!(WITNESS_POINTER)],
                self.key_digest_words(),
                vec![bfe!(MU_POINTER)],
            ]
            .concat();
            run_snippet(MlDsa44VerifyExternalMu, &arguments, nondeterminism)
        }

        fn assert_fails(&self) {
            assert!(self.run().is_err());
        }
    }

    fn run_snippet(
        snippet: impl BasicSnippet,
        arguments: &[BFieldElement],
        nondeterminism: NonDeterminism,
    ) -> Result<VMState, InstructionError> {
        let code = snippet.link_for_isolated_run();
        let mut state = VMState::new(Program::new(&code), PublicInput::default(), nondeterminism);
        state.op_stack.stack = [snippet.init_stack_for_isolated_run(), arguments.to_vec()].concat();
        state.run().map(|()| state)
    }

    /// Asserts that the run fails the assertion with `error_id`.
    fn assert_fails_with(result: Result<VMState, InstructionError>, error_id: i128) {
        match result {
            Ok(_) => panic!("expected failure with error id {error_id}"),
            Err(InstructionError::AssertionFailed(err))
            | Err(InstructionError::VectorAssertionFailed(_, err)) => {
                assert_eq!(Some(error_id), err.id, "{err}")
            }
            Err(err) => panic!("expected an assertion with id {error_id}, got {err}"),
        }
    }

    /// Asserts that the run fails the assertion with `error_id`, or fails on
    /// a value that is not a u32. The latter is how a range check refuses a
    /// value that is not even a u32: `lt` crashes before the assertion.
    fn assert_fails_with_or_non_u32(result: Result<VMState, InstructionError>, error_id: i128) {
        if !matches!(
            result,
            Err(InstructionError::OpStackError(
                OpStackError::FailedU32Conversion(_)
            ))
        ) {
            assert_fails_with(result, error_id);
        }
    }

    /// A program that calls [`MlDsa44Mu`] and then [`MlDsa44VerifyExternalMu`]
    /// on the arguments of [`MlDsa44Verify`].
    fn mu_then_external_mu() -> Program {
        let mut library = Library::new();
        let mu = library.import(Box::new(MlDsa44Mu));
        let verify = library.import(Box::new(MlDsa44VerifyExternalMu));
        let code = triton_asm!(
            // _ *key *witness [key_digest] *message message_len
            dup 8 dup 7 dup 7 dup 7 dup 7 dup 7 dup 7 dup 7
            call {mu}
            place 2 pop 2
            // _ *key *witness [key_digest] *mu
            call {verify}
            halt
            {&library.all_imports()}
        );
        Program::new(&code)
    }

    /// The words of the static allocation of `n` words that
    /// [`Library::kmalloc`] makes after `before` words, `None` where unwritten.
    fn allocation(state: &VMState, before: u32, n: u32) -> Vec<Option<BFieldElement>> {
        let first = crate::library::STATIC_MEMORY_FIRST_ADDRESS - bfe!(before) - bfe!(n - 1);
        (0..n)
            .map(|i| state.ram.get(&(first + bfe!(i))).copied())
            .collect()
    }

    fn is_static(address: &BFieldElement) -> bool {
        let region = crate::library::STATIC_MEMORY_LAST_ADDRESS.value()
            ..=crate::library::STATIC_MEMORY_FIRST_ADDRESS.value();
        region.contains(&address.value())
    }

    /// The memory that [`MlDsa44Verify`] (`w`) and [`mu_then_external_mu`]
    /// (`s`) leave is the same, up to where their static allocations lie.
    /// They allocate, in order,
    ///
    /// - `w`: a Keccak state, a layout, and SHAKE256's buffer;
    /// - `s`: μ's Keccak state, μ's prefix of a layout, SHAKE256's buffer, and
    ///   the external-μ snippet's Keccak state and layout.
    ///
    /// The Keccak state of `w` matches that of the external-μ snippet, which
    /// ran the same last permutation. Its layout matches the external-μ
    /// snippet's, except the message's pointer and length, which only μ's
    /// prefix holds. μ's Keccak state has no match: it holds SHAKE256's state
    /// after μ, which `w` overwrote, since it uses one state for both.
    fn assert_same_memory(w: &VMState, s: &VMState) {
        let dynamic = |state: &VMState| {
            state
                .ram
                .iter()
                .filter(|(address, _)| !is_static(address))
                .map(|(&address, &value)| (address, value))
                .collect::<HashMap<_, _>>()
        };
        assert_eq!(dynamic(w), dynamic(s));

        let k = KeccakF1600::NUM_SCRATCH_WORDS;
        let (l, m) = (Layout::NUM_WORDS, Layout::MU_NUM_WORDS);
        let b = (SHAKE256_RATE / 4 + 1) as u32;
        let w_keccak = allocation(w, 0, k);
        let w_layout = allocation(w, k, l);
        let w_buffer = allocation(w, k + l, b);
        let mu_keccak = allocation(s, 0, k);
        let mu_layout = allocation(s, k, m);
        let s_buffer = allocation(s, k + m, b);
        let external_keccak = allocation(s, k + m + b, k);
        let external_layout = allocation(s, 2 * k + m + b, l);

        assert_eq!(w_keccak, external_keccak);
        assert_eq!(w_buffer, s_buffer);
        for (i, &word) in w_layout.iter().enumerate() {
            let from_mu = mu_layout.get(i).copied().flatten();
            if let (Some(a), Some(b)) = (from_mu, external_layout[i]) {
                assert_eq!(a, b, "layout word {i}");
            }
            assert_eq!(word, from_mu.or(external_layout[i]), "layout word {i}");
        }
        assert!(mu_keccak.iter().any(Option::is_some));

        // Nothing else lies in static memory.
        let written = |words: &[&[Option<BFieldElement>]]| {
            words
                .iter()
                .map(|w| w.iter().flatten().count())
                .sum::<usize>()
        };
        let num_static = |state: &VMState| state.ram.keys().filter(|a| is_static(a)).count();
        assert_eq!(num_static(w), written(&[&w_keccak, &w_layout, &w_buffer]));
        assert_eq!(
            num_static(s),
            written(&[
                &mu_keccak,
                &mu_layout,
                &s_buffer,
                &external_keccak,
                &external_layout
            ])
        );
    }

    /// The static words that a run reads before it writes them. The prover
    /// chooses their initial values, so a snippet must have none.
    fn static_reads_before_writes(program: &Program, instance: &Instance) -> Vec<BFieldElement> {
        use triton_vm::vm::CoProcessorCall;
        let mut state = VMState::new(
            program.clone(),
            PublicInput::default(),
            instance.nondeterminism(),
        );
        state.op_stack.stack = instance.stack();
        let mut written = std::collections::HashSet::new();
        let mut unwritten_reads = vec![];
        while !state.halting {
            for call in state.step().unwrap() {
                let CoProcessorCall::Ram(call) = call else {
                    continue;
                };
                if call.is_write {
                    written.insert(call.ram_pointer);
                } else if is_static(&call.ram_pointer) && !written.contains(&call.ram_pointer) {
                    unwritten_reads.push(call.ram_pointer);
                }
            }
        }
        unwritten_reads
    }

    #[macro_rules_attr::apply(test)]
    fn reads_no_static_word_before_writing_it() {
        let wrapper = Program::new(&MlDsa44Verify.link_for_isolated_run());
        let split = mu_then_external_mu();
        for seed in [0, 1, 30] {
            let instance = Instance::new(seed);
            assert_eq!(
                Vec::<BFieldElement>::new(),
                static_reads_before_writes(&wrapper, &instance)
            );
            assert_eq!(
                Vec::<BFieldElement>::new(),
                static_reads_before_writes(&split, &instance)
            );
        }
    }

    /// [`MlDsa44Verify`] against [`MlDsa44Mu`] and then
    /// [`MlDsa44VerifyExternalMu`] in one program. On a valid signature they
    /// leave the same memory, as [`assert_same_memory`] makes precise. On an
    /// invalid input they fail with the same error, also with two faults,
    /// because they check in the same order.
    #[macro_rules_attr::apply(test)]
    fn verify_agrees_with_mu_then_external_mu() {
        let run = |program: &Program, instance: &Instance, nondeterminism: NonDeterminism| {
            let mut state = VMState::new(program.clone(), PublicInput::default(), nondeterminism);
            state.op_stack.stack = instance.stack();
            state.run().map(|()| state)
        };
        let wrapper = Program::new(&MlDsa44Verify.link_for_isolated_run());
        let split = mu_then_external_mu();

        // A message of 41 bytes ends with one byte in word 10.
        let nonzero_padding = |nondeterminism: &mut NonDeterminism| {
            let address = bfe!(MESSAGE_POINTER + 10);
            let word = nondeterminism.ram[&address];
            nondeterminism.ram.insert(address, word + bfe!(1 << 24));
        };
        let other_table = |nondeterminism: &mut NonDeterminism| {
            let address = bfe!(HIGH_BITS_TABLE_ADDRESS + 3);
            let word = nondeterminism.ram[&address];
            nondeterminism.ram.insert(address, word + bfe!(1));
        };
        let wrong_e = |instance: &mut Instance| {
            let (i, k) = instance.positions(|w| w > 2 * GAMMA2)[0];
            instance.witness.e[i][k] -= 1;
        };

        let mut cases = vec![];
        for seed in [0, 1, 2, 29, 30, 31] {
            let instance = Instance::new(seed);
            let nondeterminism = instance.nondeterminism();
            cases.push((instance, nondeterminism, None));
        }
        let mut instance = Instance::new(1);
        instance.key_digest = Instance::new(2).key_digest;
        let nondeterminism = instance.nondeterminism();
        cases.push((
            instance,
            nondeterminism,
            Some(MlDsa44Mu::KEY_DIGEST_MISMATCH_ERROR_ID),
        ));
        let instance = Instance::new(1);
        let mut nondeterminism = instance.nondeterminism();
        nonzero_padding(&mut nondeterminism);
        cases.push((
            instance,
            nondeterminism,
            Some(MlDsa44Mu::MESSAGE_NONZERO_PADDING_ERROR_ID),
        ));
        let instance = Instance::new(1);
        let mut nondeterminism = instance.nondeterminism();
        other_table(&mut nondeterminism);
        let id = MlDsa44VerifyExternalMu::HIGH_BITS_TABLE_DIFFERS_ERROR_ID;
        cases.push((instance, nondeterminism, Some(id)));
        let mut instance = Instance::new(1);
        wrong_e(&mut instance);
        let nondeterminism = instance.nondeterminism();
        let id = MlDsa44VerifyExternalMu::LOW_BITS_OUT_OF_RANGE_ERROR_ID;
        cases.push((instance, nondeterminism, Some(id)));
        // Two faults: the message's comes first in both.
        let mut instance = Instance::new(1);
        wrong_e(&mut instance);
        let mut nondeterminism = instance.nondeterminism();
        nonzero_padding(&mut nondeterminism);
        other_table(&mut nondeterminism);
        cases.push((
            instance,
            nondeterminism,
            Some(MlDsa44Mu::MESSAGE_NONZERO_PADDING_ERROR_ID),
        ));

        for (n, (instance, nondeterminism, error_id)) in cases.into_iter().enumerate() {
            let w = run(&wrapper, &instance, nondeterminism.clone());
            let s = run(&split, &instance, nondeterminism);
            match (w, s, error_id) {
                (Ok(w), Ok(s), None) => assert_same_memory(&w, &s),
                (Err(w), Err(s), Some(id)) => {
                    assert_eq!(w.to_string(), s.to_string(), "case {n}");
                    assert_fails_with(Err(w), id);
                }
                (w, s, _) => panic!("case {n}: {:?} and {:?}", w.err(), s.err()),
            }
        }
    }

    /// Messages of 40, 41, 69, 70, 71 and 140 bytes: one block of SHAKE256
    /// holds `tr ‖ 0 ‖ 0 ‖ message` and its padding up to 69 bytes.
    #[macro_rules_attr::apply(test)]
    fn mu_complies_with_fips_204() {
        for seed in [0, 1, 29, 30, 31, 100] {
            let instance = Instance::new(seed);
            let state = instance.run_mu().unwrap();
            let mu = *state.op_stack.stack.last().unwrap();
            let words = (0..16)
                .map(|i| state.ram[&(mu + bfe!(i as u64))])
                .collect_vec();
            assert_eq!(to_words(&instance.mu()), words, "seed {seed}");
        }
    }

    /// The longest message for which SHAKE256's input length is a u32 that it
    /// takes, plus one.
    #[macro_rules_attr::apply(test)]
    fn mu_refuses_a_message_too_long() {
        let mut instance = Instance::new(1);
        instance.message = vec![];
        let mut arguments = [
            vec![bfe!(KEY_POINTER)],
            instance.key_digest_words(),
            vec![bfe!(MESSAGE_POINTER), bfe!(u64::from(u32::MAX) - 68)],
        ]
        .concat();
        assert_fails_with(
            run_snippet(MlDsa44Mu, &arguments, instance.nondeterminism()),
            MlDsa44Mu::MESSAGE_TOO_LONG_ERROR_ID,
        );
        arguments.insert(1, bfe!(WITNESS_POINTER));
        assert_fails_with(
            run_snippet(MlDsa44Verify, &arguments, instance.nondeterminism()),
            MlDsa44Mu::MESSAGE_TOO_LONG_ERROR_ID,
        );
    }

    #[macro_rules_attr::apply(test)]
    fn mu_refuses_another_key_digest() {
        let mut instance = Instance::new(1);
        instance.key_digest = Instance::new(2).key_digest;
        assert_fails_with(instance.run_mu(), MlDsa44Mu::KEY_DIGEST_MISMATCH_ERROR_ID);
    }

    /// A 140-byte message, for which [`MlDsa44Verify`] would cross 2^20
    /// rows, verifies with μ from outside.
    #[macro_rules_attr::apply(test)]
    fn external_mu_accepts_the_mu_of_key_and_message_only() {
        let instance = Instance::new(100);
        let mu = instance.mu();
        assert!(instance.run_external_mu(&mu).is_ok());
        let mut other = mu.clone();
        other[63] ^= 1;
        assert_fails_with(
            instance.run_external_mu(&other),
            MlDsa44VerifyExternalMu::C_TILDE_MISMATCH_ERROR_ID,
        );
        let mut other_message = Instance::new(100);
        other_message.message[0] ^= 1;
        assert_fails_with(
            instance.run_external_mu(&other_message.mu()),
            MlDsa44VerifyExternalMu::C_TILDE_MISMATCH_ERROR_ID,
        );
    }

    #[macro_rules_attr::apply(test)]
    fn external_mu_refuses_another_key_digest() {
        let mut instance = Instance::new(1);
        let mu = instance.mu();
        instance.key_digest = Instance::new(2).key_digest;
        assert_fails_with(
            instance.run_external_mu(&mu),
            MlDsa44VerifyExternalMu::KEY_DIGEST_MISMATCH_ERROR_ID,
        );
    }

    /// Whether the snippet accepts `sig` on `message` under `pk`. A signature
    /// for which [`SignatureWitness::new`] finds no witness counts as refused.
    fn snippet_accepts(
        pk: &[u8; PUBLIC_KEY_LEN],
        message: &[u8],
        sig: &[u8; SIGNATURE_LEN],
    ) -> bool {
        Instance::from_signature(pk, message.to_vec(), sig).is_some_and(|i| i.run().is_ok())
    }

    /// Real ML-DSA-44 signatures, from random keys, signing randomness and
    /// messages of random lengths: the snippet accepts a signature, one with a
    /// bit flipped, and one on another message exactly when `fips204` does.
    ///
    /// A signature whose hints do not decode has no witness, and counts as
    /// refused without a run of the snippet. About 2% of single bit flips do
    /// that. [`agrees_with_fips204_on_a_bit_flip_in_each_part_of_a_signature`]
    /// covers both cases on every run.
    #[macro_rules_attr::apply(proptest(cases = 8))]
    fn agrees_with_fips204_on_real_signatures(
        key_seed: [u8; 32],
        signing_seed: [u8; 32],
        #[strategy(vec(arb(), 0..600))] message: Vec<u8>,
        #[strategy(vec(arb(), 0..600))] other_message: Vec<u8>,
        #[strategy(0usize..8 * SIGNATURE_LEN)] flipped_bit: usize,
    ) {
        let (pk, sk) = ml_dsa_44::KG::keygen_from_seed(&key_seed);
        let sign = |message: &[u8]| sk.try_sign_with_seed(&signing_seed, message, &[]).unwrap();
        let sig = sign(&message);
        let mut tampered = sig;
        tampered[flipped_bit / 8] ^= 1 << (flipped_bit % 8);

        let pk_bytes = pk.clone().into_bytes();
        for sig in [sig, tampered, sign(&other_message)] {
            let fips204_accepts = pk.verify(&message, &sig, &[]);
            prop_assert_eq!(fips204_accepts, snippet_accepts(&pk_bytes, &message, &sig));
        }
        prop_assert!(snippet_accepts(&pk_bytes, &message, &sig));
    }

    /// One bit flipped in each part of a real signature: c̃, z, a hint
    /// position, the number of hints of a polynomial, and the zeros after the
    /// hints. `fips204` refuses each, and so does the snippet. A flip in c̃ or
    /// z leaves the hints decodable, so the snippet runs and refuses. A flip in
    /// the zeros after the hints leaves them undecodable, so there is no
    /// witness to run the snippet on.
    #[macro_rules_attr::apply(test)]
    fn agrees_with_fips204_on_a_bit_flip_in_each_part_of_a_signature() {
        let (pk, sk) = ml_dsa_44::KG::keygen_from_seed(&[5; 32]);
        let message = b"one bit flipped".to_vec();
        let sig = sk.try_sign_with_seed(&[6; 32], &message, &[]).unwrap();
        let pk_bytes = pk.clone().into_bytes();

        // The hints: OMEGA positions, then the number of hints up to and
        // including each of the K polynomials.
        let hints = C_TILDE_LEN + 576 * L;
        let num_hints = usize::from(sig[hints + OMEGA + K - 1]);
        assert!(0 < num_hints && num_hints < OMEGA);

        // (byte, whether the hints still decode)
        let flips = [
            (0, Some(true)),
            (C_TILDE_LEN - 1, Some(true)),
            (C_TILDE_LEN, Some(true)),
            (hints - 1, Some(true)),
            (hints, None),
            (hints + OMEGA, None),
            (hints + num_hints, Some(false)),
        ];
        for (byte, decodes) in flips {
            let mut tampered = sig;
            tampered[byte] ^= 1;
            assert!(!pk.verify(&message, &tampered, &[]), "byte {byte}");
            let instance = Instance::from_signature(&pk_bytes, message.clone(), &tampered);
            if let Some(decodes) = decodes {
                assert_eq!(decodes, instance.is_some(), "byte {byte}");
            }
            if let Some(instance) = instance {
                assert!(instance.run().is_err(), "byte {byte}");
            }
        }
    }

    /// Every message length modulo 4, and the lengths at which the input to
    /// μ, of 66 bytes more, fills a block of SHAKE256 or crosses into the next.
    #[macro_rules_attr::apply(test)]
    fn accepts_real_signatures_on_messages_of_boundary_lengths() {
        let (pk, sk) = ml_dsa_44::KG::keygen_from_seed(&[3; 32]);
        let pk_bytes = pk.clone().into_bytes();
        let block = SHAKE256_RATE - 66;
        let lengths = [
            0,
            1,
            2,
            3,
            4,
            5,
            block - 1,
            block,
            block + 1,
            block + SHAKE256_RATE,
        ];
        for n in lengths {
            let message = (0..n).map(|i| (i * 37 + 11) as u8).collect_vec();
            let sig = sk.try_sign_with_seed(&[4; 32], &message, &[]).unwrap();
            assert!(snippet_accepts(&pk_bytes, &message, &sig), "{n}");
        }
    }

    #[macro_rules_attr::apply(test)]
    fn refuses_nonzero_bytes_past_the_message() {
        for n in [41, 42, 43] {
            let mut instance = Instance::new(1);
            instance.message.truncate(n);
            let mut nondeterminism = instance.nondeterminism();
            let last = bfe!(MESSAGE_POINTER + (n / 4) as u64);
            let word = nondeterminism.ram[&last];
            nondeterminism.ram.insert(last, word + bfe!(1u64 << 24));
            let Err(InstructionError::AssertionFailed(err)) = instance.run_with(nondeterminism)
            else {
                panic!("expected a failed assertion");
            };
            assert_eq!(Some(MlDsa44Mu::MESSAGE_NONZERO_PADDING_ERROR_ID), err.id);
        }
    }

    #[macro_rules_attr::apply(test)]
    fn accepts_valid_signatures() {
        for seed in 0..3 {
            let instance = Instance::new(seed);
            let state = instance.run().unwrap();
            assert_eq!(
                MlDsa44Verify.init_stack_for_isolated_run(),
                state.op_stack.stack
            );
            // Two pages: one for μ's input, and one for the output of SHAKE256.
            let first_free_page = DYN_MALLOC_FIRST_PAGE + 2;
            assert_eq!(bfe!(first_free_page), state.ram[&DYN_MALLOC_ADDRESS]);
        }
    }

    #[macro_rules_attr::apply(test)]
    fn refuses_another_message() {
        let mut instance = Instance::new(1);
        instance.message = b"another message".to_vec();
        instance.assert_fails_with(MlDsa44VerifyExternalMu::C_TILDE_MISMATCH_ERROR_ID);
    }

    #[macro_rules_attr::apply(test)]
    fn refuses_another_key() {
        let mut instance = Instance::new(1);
        instance.key_digest = Instance::new(2).key_digest;
        instance.assert_fails_with(MlDsa44Mu::KEY_DIGEST_MISMATCH_ERROR_ID);
    }

    #[macro_rules_attr::apply(test)]
    fn refuses_a_changed_commitment_hash() {
        let mut instance = Instance::new(1);
        instance.witness.c_tilde[0][0] ^= 1;
        instance.assert_fails();
    }

    #[macro_rules_attr::apply(test)]
    fn refuses_a_slice_that_is_not_a_byte() {
        let mut instance = Instance::new(1);
        instance.witness.c_tilde[1][3] = 0;
        let mut words = instance.witness.encode();
        words[SignatureWitness::C_TILDE_OFFSET + 8 + 3] = bfe!(256);
        let mut nondeterminism = instance.nondeterminism();
        for (i, word) in words.into_iter().enumerate() {
            nondeterminism
                .ram
                .insert(bfe!(WITNESS_POINTER + i as u64), word);
        }
        let code = MlDsa44Verify.link_for_isolated_run();
        let mut state = VMState::new(Program::new(&code), PublicInput::default(), nondeterminism);
        state.op_stack.stack = instance.stack();
        let Err(InstructionError::AssertionFailed(err)) = state.run() else {
            panic!("expected a failed assertion");
        };
        assert_eq!(
            Some(MlDsa44VerifyExternalMu::C_TILDE_OUT_OF_RANGE_ERROR_ID),
            err.id
        );
    }

    #[macro_rules_attr::apply(test)]
    fn refuses_z_out_of_range() {
        let mut instance = Instance::new(1);
        instance.witness.z[2][17] = (GAMMA1 - BETA) as i32;
        instance.assert_fails_with(MlDsa44VerifyExternalMu::Z_OUT_OF_RANGE_ERROR_ID);
        // Shifted, this is −1, which is not a u32.
        instance.witness.z[2][17] = -((GAMMA1 - BETA) as i32);
        instance.assert_fails_with_or_non_u32(MlDsa44VerifyExternalMu::Z_OUT_OF_RANGE_ERROR_ID);
    }

    #[macro_rules_attr::apply(test)]
    fn refuses_a_wrong_w() {
        let mut instance = Instance::new(1);
        instance.witness.w[1][5] += 1;
        instance.assert_fails_with(MlDsa44VerifyExternalMu::IDENTITY_FAILS_ERROR_ID);
    }

    #[macro_rules_attr::apply(test)]
    fn refuses_w_off_by_a_multiple_of_q_beyond_the_range_of_q_coefficients() {
        let mut instance = Instance::new(1);
        instance.witness.q[0][9] += 1 << 29;
        instance.assert_fails_with(MlDsa44VerifyExternalMu::Q_OUT_OF_RANGE_ERROR_ID);
    }

    #[macro_rules_attr::apply(test)]
    fn refuses_w_off_by_q_when_the_high_bits_cannot_match() {
        // Find a coefficient whose high bits leave no room for a shifted
        // representative.
        let mut instance = Instance::new(1);
        let (i, k) = (0..K)
            .flat_map(|i| (0..N).map(move |k| (i, k)))
            .find(|&(i, k)| {
                let w = instance.witness.w[i][k];
                w > GAMMA2 && w < Q - 1 - GAMMA2
            })
            .unwrap();
        instance.witness.w[i][k] += Q;
        instance.witness.q[i][k] -= 1;
        instance.assert_fails_with(MlDsa44VerifyExternalMu::LOW_BITS_OUT_OF_RANGE_ERROR_ID);
    }

    /// `w + q` with `e = 45` passes the check of the low bits for `w` in
    /// `[γ2, 3·γ2)`, so only the range of `e` refuses it.
    #[macro_rules_attr::apply(test)]
    fn refuses_high_bits_past_44() {
        let mut instance = Instance::new(1);
        let (i, k) = instance.positions(|w| (GAMMA2..3 * GAMMA2).contains(&w))[0];
        instance.witness.w[i][k] += Q;
        instance.witness.q[i][k] -= 1;
        instance.witness.e[i][k] = W1_RANGE + 1;
        instance.assert_fails_with(MlDsa44VerifyExternalMu::HIGH_BITS_OUT_OF_RANGE_ERROR_ID);
    }

    #[macro_rules_attr::apply(test)]
    fn refuses_another_high_bits_table() {
        let instance = Instance::new(1);
        let mut nondeterminism = instance.nondeterminism();
        let address = bfe!(HIGH_BITS_TABLE_ADDRESS + 3);
        let word = nondeterminism.ram[&address];
        nondeterminism.ram.insert(address, word + bfe!(1));
        assert_fails_with(
            instance.run_with(nondeterminism),
            MlDsa44VerifyExternalMu::HIGH_BITS_TABLE_DIFFERS_ERROR_ID,
        );
    }

    /// Every coefficient with a second representative in `[−γ2 + 1, q − 1 +
    /// γ2]` takes it at once: `w < γ2` becomes `w + q` with `e = 44`, and
    /// `w > q − γ2` becomes `w − q` with `e = 0`, each with `Q` adjusted by
    /// one. The snippet accepts, and over the seeds tried some of the moved
    /// coefficients at each edge carry a hint, so the sign of `r0` that
    /// `UseHint` reads agrees too.
    #[macro_rules_attr::apply(test)]
    fn accepts_w_off_by_q_at_both_edges() {
        let (mut hinted_upper, mut hinted_lower) = (false, false);
        for seed in 0..8 {
            let instance = Instance::new(seed);
            let upper = instance.positions(|w| w < GAMMA2);
            let lower = instance.positions(|w| w > Q - GAMMA2);
            assert!(!upper.is_empty() && !lower.is_empty());
            hinted_upper |= upper.iter().any(|&p| instance.is_hinted(p));
            hinted_lower |= lower.iter().any(|&p| instance.is_hinted(p));
            let result = instance.run_with_other_representatives();
            assert!(result.is_ok(), "seed {seed}: {result:?}");
        }
        assert!(hinted_upper && hinted_lower);
    }

    /// The values of `w` at the boundaries of `Decompose`, which real
    /// signatures almost never reach, each on a hinted and an unhinted
    /// coefficient, in either representative. Since `c̃` comes from FIPS 204's
    /// `UseHint`, the snippet accepts only if it derives the same `w1`.
    #[macro_rules_attr::apply(test)]
    fn accepts_the_boundary_values_of_decompose() {
        let gamma2 = GAMMA2 as i32;
        let values = [
            0,
            1,
            -1,
            gamma2 - 1,
            gamma2,
            gamma2 + 1,
            -gamma2 + 1,
            -gamma2,
            -gamma2 - 1,
            -gamma2 - 2,
            SignatureWitness::Z_SHIFT as i32,
            -(SignatureWitness::Z_SHIFT as i32),
        ];
        let mut z = [[0; N]; L];
        z[0][..values.len()].copy_from_slice(&values);
        z[1][..values.len()].copy_from_slice(&values);
        let hints = (0..values.len() as u16).collect_vec();
        let instance = Instance::synthetic(z, hints, vec![1, 2, 3]);
        assert!(instance.run().is_ok());
        let result = instance.run_with_other_representatives();
        assert!(result.is_ok(), "{result:?}");
    }

    /// The special case of `Decompose`, `r − r0 = q − 1`, where `e = 44` and
    /// `r0` is one less, on a coefficient that carries a hint.
    #[macro_rules_attr::apply(test)]
    fn accepts_a_hint_on_a_coefficient_with_high_bits_44() {
        let instance = (0..=255)
            .map(Instance::new)
            .find(|instance| {
                instance
                    .positions(|w| w >= Q - 1 - GAMMA2)
                    .into_iter()
                    .any(|p| instance.is_hinted(p))
            })
            .unwrap();
        assert!(instance.run().is_ok());
    }

    /// `S` enters at `X^k` and `X^(k+256)`, and only the first has a `w` to
    /// make up for a change.
    #[macro_rules_attr::apply(test)]
    fn refuses_a_changed_s_even_with_w_compensated() {
        let mut instance = Instance::new(1);
        instance.witness.s[2][17] += 1;
        instance.assert_fails_with(MlDsa44VerifyExternalMu::IDENTITY_FAILS_ERROR_ID);
        let mut instance = Instance::new(1);
        let (i, k) = instance.positions(|w| w > GAMMA2 && w < Q - GAMMA2)[0];
        instance.witness.s[i][k] += 1;
        instance.witness.w[i][k] -= 1;
        instance.assert_fails_with(MlDsa44VerifyExternalMu::IDENTITY_FAILS_ERROR_ID);
    }

    #[macro_rules_attr::apply(test)]
    fn refuses_a_wrong_e() {
        // One less makes r − 2·γ2·e + γ2 − 1 too large.
        let mut instance = Instance::new(1);
        let (i, k) = instance.positions(|w| w > 2 * GAMMA2)[0];
        instance.witness.e[i][k] -= 1;
        instance.assert_fails_with(MlDsa44VerifyExternalMu::LOW_BITS_OUT_OF_RANGE_ERROR_ID);
        // One more makes it negative, which is not a u32.
        let mut instance = Instance::new(1);
        instance.witness.e[i][k] += 1;
        instance
            .assert_fails_with_or_non_u32(MlDsa44VerifyExternalMu::LOW_BITS_OUT_OF_RANGE_ERROR_ID);
    }

    #[macro_rules_attr::apply(test)]
    /// Each makes a gap `next − n − 1` negative, which is not a u32.
    #[macro_rules_attr::apply(test)]
    fn refuses_hints_out_of_order_or_repeated() {
        let id = MlDsa44VerifyExternalMu::HINTS_NOT_INCREASING_ERROR_ID;
        let mut instance = Instance::new(1);
        assert!(instance.witness.hints.len() >= 2);
        instance.witness.hints.swap(0, 1);
        instance.assert_fails_with_or_non_u32(id);
        let mut instance = Instance::new(1);
        let first = instance.witness.hints[0];
        instance.witness.hints.insert(0, first);
        instance.assert_fails_with_or_non_u32(id);
    }

    /// A last hint at 1024 makes the gap −1, which is not a u32. One at −1
    /// makes it 1024, which the check refuses.
    #[macro_rules_attr::apply(test)]
    fn refuses_a_hint_outside_the_coefficients() {
        let id = MlDsa44VerifyExternalMu::HINTS_NOT_INCREASING_ERROR_ID;
        let mut instance = Instance::new(1);
        instance.witness.hints.push((K * N) as u16);
        instance.assert_fails_with_or_non_u32(id);
        let last = SignatureWitness::HINTS_OFFSET + instance.witness.hints.len() - 1;
        let result = instance.run_patched(|words| words[last] = bfe!(-1));
        assert_fails_with(result, id);
    }

    #[macro_rules_attr::apply(test)]
    fn refuses_too_many_hints() {
        let mut instance = Instance::new(1);
        instance.witness.hints = (0..=OMEGA as u16).collect();
        instance.assert_fails_with(MlDsa44VerifyExternalMu::TOO_MANY_HINTS_ERROR_ID);
    }

    #[macro_rules_attr::apply(test)]
    fn refuses_a_dropped_hint() {
        let mut instance = Instance::new(1);
        instance.witness.hints.pop();
        instance.assert_fails_with(MlDsa44VerifyExternalMu::C_TILDE_MISMATCH_ERROR_ID);
    }

    /// The worst case: `OMEGA` hints, spread over all of `w`, and the longest
    /// message for which μ takes one block. Every table stays at least 8192
    /// rows below `2^20`.
    #[macro_rules_attr::apply(test)]
    fn worst_case_fits_below_2_pow_20() {
        let z_shift = SignatureWitness::Z_SHIFT as i64;
        let z = std::array::from_fn(|i| {
            std::array::from_fn(|k| {
                ((7919 * (N * i + k) as i64) % (2 * z_shift + 1) - z_shift) as i32
            })
        });
        let hints = (0..OMEGA as u16).map(|h| 12 * h).collect_vec();
        let instance = Instance::synthetic(z, hints, vec![0xa5; 69]);
        let code = MlDsa44Verify.link_for_isolated_run();
        let program = crate::test_helpers::prepend_program_with_stack_setup(
            &instance.stack(),
            &Program::new(&code),
        );
        let (aet, _) =
            VM::trace_execution(program, PublicInput::default(), instance.nondeterminism())
                .unwrap();
        let height = aet.height();
        assert!(
            height.height <= (1 << 20) - 8192,
            "{} table: {} rows",
            height.table,
            height.height
        );
    }

    #[macro_rules_attr::apply(test)]
    fn benchmark() {
        let instance = Instance::new(0);
        let code = MlDsa44Verify.link_for_isolated_run();
        let benchmark = execute_bench(
            &code,
            &instance.stack(),
            vec![],
            instance.nondeterminism(),
            None,
        );
        write_benchmarks(vec![NamedBenchmarkResult {
            name: MlDsa44Verify.entrypoint(),
            benchmark_result: benchmark,
            case: BenchmarkCase::CommonCase,
        }]);

        let state = instance.run_mu().unwrap();
        let mu_pointer = *state.op_stack.stack.last().unwrap();
        let mu_stack = [
            MlDsa44Mu.init_stack_for_isolated_run(),
            vec![bfe!(KEY_POINTER)],
            instance.key_digest_words(),
            vec![bfe!(MESSAGE_POINTER), bfe!(instance.message.len() as u64)],
        ]
        .concat();
        let benchmark = execute_bench(
            &MlDsa44Mu.link_for_isolated_run(),
            &mu_stack,
            vec![],
            instance.nondeterminism(),
            None,
        );
        write_benchmarks(vec![NamedBenchmarkResult {
            name: MlDsa44Mu.entrypoint(),
            benchmark_result: benchmark,
            case: BenchmarkCase::CommonCase,
        }]);

        let mut nondeterminism = instance.nondeterminism();
        for i in 0..16 {
            let word = state.ram[&(mu_pointer + bfe!(i as u64))];
            nondeterminism.ram.insert(bfe!(MU_POINTER + i as u64), word);
        }
        let verify_stack = [
            MlDsa44VerifyExternalMu.init_stack_for_isolated_run(),
            vec![bfe!(KEY_POINTER), bfe!(WITNESS_POINTER)],
            instance.key_digest_words(),
            vec![bfe!(MU_POINTER)],
        ]
        .concat();
        let benchmark = execute_bench(
            &MlDsa44VerifyExternalMu.link_for_isolated_run(),
            &verify_stack,
            vec![],
            nondeterminism,
            None,
        );
        write_benchmarks(vec![NamedBenchmarkResult {
            name: MlDsa44VerifyExternalMu.entrypoint(),
            benchmark_result: benchmark,
            case: BenchmarkCase::CommonCase,
        }]);
    }

    #[ignore = "prints the cycle counts of many verifications"]
    #[macro_rules_attr::apply(test)]
    fn cycle_counts_across_signatures() {
        let code = MlDsa44Verify.link_for_isolated_run();
        let mut max = 0;
        for seed in 0..64 {
            let instance = Instance::new(seed);
            let benchmark = execute_bench(
                &code,
                &instance.stack(),
                vec![],
                instance.nondeterminism(),
                None,
            );
            max = max.max(benchmark.clock_cycle_count);
            println!(
                "seed {seed:2}: {} cycles, {} op stack rows, {} hints",
                benchmark.clock_cycle_count,
                benchmark.op_stack_table_height,
                instance.witness.hints.len()
            );
        }
        println!("maximum: {max} cycles");
    }

    #[ignore = "prints the profile of a verification"]
    #[macro_rules_attr::apply(test)]
    fn profile() {
        let instance = Instance::new(0);
        let code = MlDsa44Verify.link_for_isolated_run();
        let program = crate::test_helpers::prepend_program_with_stack_setup(
            &instance.stack(),
            &Program::new(&code),
        );
        let profile = crate::generate_full_profile(
            &MlDsa44Verify.entrypoint(),
            program,
            &PublicInput::default(),
            &instance.nondeterminism(),
        );
        println!("{profile}");
    }
}
