//! The ML-DSA-44 verifier in Triton assembly.

use itertools::Itertools;
use triton_vm::prelude::*;

use super::reference::ExpandedPublicKey;
use super::reference::SignatureWitness;
use super::*;
use crate::hashing::keccak::KeccakFunction;
use crate::hashing::keccak::SHAKE256_RATE;
use crate::hashing::keccak::keccak_f1600::KeccakF1600;
use crate::hashing::keccak::sponge::bytes_of_word;
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
/// initialized memory just below the
/// [Keccak tables](crate::hashing::keccak::tables::KECCAK_TABLES_ADDRESS).
/// Word `e` is `e mod 44` for `e` from 0 to 44, and word
/// [`POWERS_OF_64_OFFSET`]` + i` is `64^i` for `i` from 0 to 3. The snippet
/// checks it against its digest.
pub const HIGH_BITS_TABLE_ADDRESS: u64 = KECCAK_TABLES_ADDRESS - 64;

/// The offset of the powers of 64 in the high-bits table.
pub const POWERS_OF_64_OFFSET: u64 = W1_RANGE as u64 + 1;

const NUM_HIGH_BITS_TABLE_WORDS: usize = 50;

/// The words of the high-bits table.
pub fn high_bits_table_words() -> Vec<BFieldElement> {
    let mut words = vec![bfe!(0); NUM_HIGH_BITS_TABLE_WORDS];
    for e in 0..=W1_RANGE {
        words[e as usize] = bfe!(e % W1_RANGE);
    }
    for i in 0..4 {
        words[POWERS_OF_64_OFFSET as usize + i] = bfe!(64u64.pow(i as u32));
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

/// Verifies an ML-DSA-44 signature on a digest, with an empty context string,
/// and crashes the VM if the signature is invalid.
///
/// The message is the 40 bytes of the digest, each element little-endian
/// ([`digest_bytes`](super::reference::digest_bytes)). The public key enters
/// through its [expansion](ExpandedPublicKey), which lies in memory at
/// `*expanded_key` and is checked against `key_digest`. The signature enters
/// through its [witness](SignatureWitness), at `*signature_witness`.
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
/// - the [Keccak tables](crate::hashing::keccak::tables) are in memory
/// - the expanded key and the witness lie in memory outside the tables
///
/// ### Post-conditions
///
/// - ML-DSA.Verify(pk, message, σ, "") holds for the public key `pk` whose
///   expansion has digest `key_digest`, and the signature σ whose witness is
///   given
/// - the sponge state is overwritten
#[derive(Debug, Default, Copy, Clone, Eq, PartialEq, Hash)]
pub struct MlDsa44Verify;

impl MlDsa44Verify {
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
    message: u64,
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
    message_bytes: u64,
}

impl Layout {
    const NUM_WORDS: u32 =
        1 + 1 + 10 + 5 + 3 + 3 + 3 + 12 + 3 + 768 + 256 + 256 + 768 + 136 + 5 + 1 + 48;

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
            message: take(5),
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
            message_bytes: take(48),
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

    fn entry(&self) -> Vec<LabelledInstruction> {
        let l = self.layout;
        let names = [
            "check_key",
            "challenge",
            "range_checks",
            "powers",
            "sample_in_ball",
            "identity",
            "use_hint",
            "mu",
            "commitment",
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
        triton_asm!(
            {self.entrypoint}:
                // _ *key *witness [key_digest] [message]
                push {l.message} write_mem 5 pop 1
                push {l.key_block} write_mem 5 pop 1
                {&write_zeros_to_memory(l.key_block + 5, 5)}
                push {l.witness_ptr} write_mem 1 pop 1
                push {l.key_ptr} write_mem 1 pop 1
                call {self.check_tables}
                sponge_init
                push {HIGH_BITS_TABLE_ADDRESS}
                {&Self::absorb_words(NUM_HIGH_BITS_TABLE_WORDS)}
                sponge_squeeze
                {&push_digest}
                assert_vector error_id {MlDsa44Verify::HIGH_BITS_TABLE_DIFFERS_ERROR_ID}
                pop 5 pop 5
                {&calls}
                return
        )
    }

    fn check_key(&self) -> Vec<LabelledInstruction> {
        triton_asm!(
            {self.label("check_key")}:
                sponge_init
                {&self.key_address(0)}
                {&Self::absorb_words(ExpandedPublicKey::NUM_WORDS)}
                sponge_squeeze
                push {self.layout.key_block + 4} read_mem 5 pop 1
                assert_vector error_id {MlDsa44Verify::KEY_DIGEST_MISMATCH_ERROR_ID}
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
        let c_tilde = self.range_check(32, 255, MlDsa44Verify::C_TILDE_OUT_OF_RANGE_ERROR_ID);
        let z = self.range_check(
            L * N,
            2 * SignatureWitness::Z_SHIFT,
            MlDsa44Verify::Z_OUT_OF_RANGE_ERROR_ID,
        );
        let q = self.range_check(
            K * N,
            2 * SignatureWitness::Q_SHIFT - 1,
            MlDsa44Verify::Q_OUT_OF_RANGE_ERROR_ID,
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
        let id = MlDsa44Verify::IDENTITY_FAILS_ERROR_ID;
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
        let low_id = MlDsa44Verify::LOW_BITS_OUT_OF_RANGE_ERROR_ID;
        let high_id = MlDsa44Verify::HIGH_BITS_OUT_OF_RANGE_ERROR_ID;
        let order_id = MlDsa44Verify::HINTS_NOT_INCREASING_ERROR_ID;
        let w_to_e = SignatureWitness::E_OFFSET - SignatureWitness::W_OFFSET;

        // For every coefficient r with high bits e: r - 2·γ2·e lies in
        // (-γ2, γ2], e ≤ 44, and w1 = e mod 44. Four coefficients make a
        // 24-bit group of w1Encode.
        let mut groups = vec![];
        for g in 0..K * N / 4 {
            // _ *r_{4g+3}
            groups.extend(triton_asm!(
                dup 0 read_mem 4 pop 1
                dup 4 addi {w_to_e} read_mem 4 pop 1
            ));
            for m in 0..4 {
                // _ w1_0..w1_{m-1} r_3..r_m e_3..e_m, top last; each w1 went
                // below the remaining r and e
                let check_low = self.check_at_most(two_gamma2 - 1, low_id);
                groups.extend(triton_asm!(
                    dup 0 push {neg(two_gamma2)} mul
                    pick {5 - m} add
                    addi {GAMMA2 - 1}
                    {&check_low}
                    push {W1_RANGE + 1} dup 1 lt assert error_id {high_id}
                    addi {HIGH_BITS_TABLE_ADDRESS} read_mem 1 pop 1
                ));
                if m < 3 {
                    groups.extend(triton_asm!(place { 6 - 2 * m }));
                }
            }
            // _ *r w1_0 w1_1 w1_2 w1_3
            groups.extend(triton_asm!(
                push 64 mul add push 64 mul add push 64 mul add
                push {l.w1_groups + g as u64} write_mem 1 pop 1
                addi 4
            ));
        }

        // The bytes of w1Encode, three per group.
        let w1_bytes = (0..K * N / 4)
            .flat_map(|g| {
                triton_asm!(
                    push {l.w1_groups + g as u64} read_mem 1 pop 1
                    push 256 pick 1 div_mod pick 1
                    push 256 pick 1 div_mod pick 1
                    swap 2
                    push {l.w1_bytes + 3 * g as u64} write_mem 3 pop 1
                )
            })
            .collect_vec();

        triton_asm!(
            {self.label("use_hint")}:
                {&self.witness_address(SignatureWitness::W_OFFSET + 3)}
                {&groups}
                pop 1
                // The hints: a count of at most ω, then strictly increasing
                // positions.
                {&self.witness_address(SignatureWitness::HINT_COUNT_OFFSET)}
                read_mem 1 pop 1
                push {OMEGA + 1} dup 1 lt
                assert error_id {MlDsa44Verify::TOO_MANY_HINTS_ERROR_ID}
                {&self.witness_address(SignatureWitness::HINTS_OFFSET)}
                push 0
                // _ count *hint lower
                call {self.label("hint_loop")}
                pop 3
                {&w1_bytes}
                return

            {self.label("hint_loop")}:
                dup 2 push 0 eq
                skiz return
                dup 1 read_mem 1 pop 1
                // _ count *hint lower n
                push {K * N} dup 1 lt assert error_id {order_id}
                dup 0 dup 2 push {neg(1)} mul add
                push {K * N} swap 1 lt assert error_id {order_id}
                dup 0
                call {self.label("apply_hint")}
                addi 1 swap 1 pop 1 swap 1 addi 1 swap 1
                pick 2 addi {neg(1)} place 2
                recurse

            // _ n → _, and w1[n] becomes (w1[n] ± 1) mod 44, by the sign of
            // r0, in its group.
            {self.label("apply_hint")}:
                dup 0
                {&self.witness_address(SignatureWitness::W_OFFSET)}
                add read_mem 1 pop 1
                dup 1
                {&self.witness_address(SignatureWitness::E_OFFSET)}
                add read_mem 1 pop 1
                // _ n r e
                dup 0 push {W1_RANGE} eq
                dup 1 push {neg(two_gamma2)} mul pick 3 add
                pick 1 push {neg(1)} mul add
                // _ n e r0, and r0 > 0 iff γ2 + 1 < r0 + γ2 + 1
                addi {GAMMA2 + 1} push {GAMMA2 + 1} lt
                push 2 mul addi {neg(1)}
                // _ n e delta
                pick 1 addi {HIGH_BITS_TABLE_ADDRESS} read_mem 1 pop 1
                dup 0 pick 2 add addi {W1_RANGE}
                push {W1_RANGE} swap 1 div_mod swap 1 pop 1
                // _ n old new
                pick 1 push {neg(1)} mul add
                pick 1 push 4 swap 1 div_mod
                // _ diff g (n mod 4)
                addi {HIGH_BITS_TABLE_ADDRESS + POWERS_OF_64_OFFSET} read_mem 1 pop 1
                pick 2 mul
                dup 1 addi {l.w1_groups} read_mem 1 pop 1 add
                pick 1 addi {l.w1_groups} write_mem 1 pop 1
                return
        )
    }

    fn mu(&self) -> Vec<LabelledInstruction> {
        let l = self.layout;
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
        let message_bytes = (0..Digest::LEN)
            .flat_map(|i| {
                let first = l.message_bytes + 2 + 8 * i as u64;
                triton_asm!(
                    push {l.message + i as u64} read_mem 1 pop 1
                    split
                    // _ hi lo
                    {&bytes_of_word()}
                    {&write_words_to_scattered_addresses(&[first + 3, first + 2, first + 1, first])}
                    {&bytes_of_word()}
                    {&write_words_to_scattered_addresses(&[first + 7, first + 6, first + 5, first + 4])}
                )
            })
            .collect_vec();
        let absorb = (8..14)
            .flat_map(|lane| {
                self.keccak.absorb_lane_from_bytes(
                    l.message_bytes + 8 * (lane as u64 - 8),
                    lane,
                    false,
                )
            })
            .collect_vec();
        triton_asm!(
            {self.label("mu")}:
                {&write_zeros_to_memory(self.keccak.state_address(), 200)}
                {&tr}
                // M' = 0 ‖ 0 ‖ message bytes, then 0x1f, then zeros, as bytes
                // 64 to 111.
                {&write_zeros_to_memory(l.message_bytes, 48)}
                push 31 push {l.message_bytes + 42} write_mem 1 pop 1
                {&message_bytes}
                {&absorb}
                {&self.keccak.xor_byte(SHAKE256_RATE - 1, 0x80)}
                call {self.permute}
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
        let id = MlDsa44Verify::C_TILDE_MISMATCH_ERROR_ID;
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
            (DataType::Digest, "message".to_string()),
        ]
    }

    fn return_values(&self) -> Vec<(DataType, String)> {
        vec![]
    }

    fn entrypoint(&self) -> String {
        "tasmlib_ml_dsa_verify_ml_dsa_44".to_string()
    }

    fn code(&self, library: &mut Library) -> Vec<LabelledInstruction> {
        let keccak = KeccakF1600::allocate(library);
        let layout = Layout::new(library.kmalloc(Layout::NUM_WORDS).write_address().value());
        let permute = library.import(Box::new(keccak));
        let check_tables = library.import(Box::new(CheckKeccakTables));
        let mut generator = Generator {
            entrypoint: self.entrypoint(),
            layout,
            keccak,
            permute,
            check_tables,
            failures: vec![],
        };
        let mut instructions = [
            generator.entry(),
            generator.check_key(),
            generator.challenge(),
            generator.range_checks(),
            generator.powers(),
            generator.sample_in_ball(),
            generator.identity(),
            generator.use_hint(),
            generator.mu(),
            generator.commitment(),
        ]
        .concat();
        for error_id in generator.failures.clone() {
            let label = generator.label(&format!("fail_{error_id}"));
            instructions.extend(triton_asm!({label}: push 0 assert error_id {error_id}));
        }
        instructions
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use fips204::ml_dsa_44;
    use fips204::traits::KeyGen;
    use fips204::traits::SerDes;
    use fips204::traits::Signer;

    use super::super::reference::digest_bytes;
    use super::super::reference::verify_digest;
    use super::*;
    use crate::hashing::keccak::tables::insert_tables;
    use crate::linker::execute_bench;
    use crate::snippet_bencher::BenchmarkCase;
    use crate::snippet_bencher::NamedBenchmarkResult;
    use crate::snippet_bencher::write_benchmarks;
    use crate::test_prelude::*;

    const KEY_POINTER: u64 = 1 << 20;
    const WITNESS_POINTER: u64 = 1 << 21;

    struct Instance {
        key: ExpandedPublicKey,
        witness: SignatureWitness,
        key_digest: Digest,
        message: Digest,
    }

    impl Instance {
        fn new(seed: u8) -> Self {
            let message = Digest::new(bfe_array![seed as u64, 2, 3, u64::MAX - u32::MAX as u64, 5]);
            let (pk, sk) = ml_dsa_44::KG::keygen_from_seed(&[seed; 32]);
            let sig = sk
                .try_sign_with_seed(&[seed ^ 0x5a; 32], &digest_bytes(message), &[])
                .unwrap();
            let pk = pk.into_bytes();
            assert!(verify_digest(&pk, message, &sig));
            let key = ExpandedPublicKey::new(&pk);
            let witness = SignatureWitness::new(&key, &sig).unwrap();
            let key_digest = key.digest();
            Self {
                key,
                witness,
                key_digest,
                message,
            }
        }

        fn stack(&self) -> Vec<BFieldElement> {
            let mut stack = MlDsa44Verify.init_stack_for_isolated_run();
            stack.push(bfe!(KEY_POINTER));
            stack.push(bfe!(WITNESS_POINTER));
            stack.extend(self.key_digest.values().iter().rev());
            stack.extend(self.message.values().iter().rev());
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
            NonDeterminism::default().with_ram(memory)
        }

        fn run(&self) -> Result<VMState, InstructionError> {
            let code = MlDsa44Verify.link_for_isolated_run();
            let mut state = VMState::new(
                Program::new(&code),
                PublicInput::default(),
                self.nondeterminism(),
            );
            state.op_stack.stack = self.stack();
            state.run().map(|()| state)
        }

        fn assert_fails_with(&self, error_id: i128) {
            match self.run() {
                Ok(_) => panic!("expected failure with error id {error_id}"),
                Err(InstructionError::AssertionFailed(err))
                | Err(InstructionError::VectorAssertionFailed(_, err)) => {
                    assert_eq!(Some(error_id), err.id, "{err}")
                }
                // A range check refuses a value that is not even a u32 before
                // it reaches its assertion.
                Err(InstructionError::OpStackError(OpStackError::FailedU32Conversion(_))) => (),
                Err(err) => panic!("expected an assertion with id {error_id}, got {err}"),
            }
        }

        fn assert_fails(&self) {
            assert!(self.run().is_err());
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
        }
    }

    #[macro_rules_attr::apply(test)]
    fn refuses_another_message() {
        let mut instance = Instance::new(1);
        instance.message = Digest::new(bfe_array![9, 9, 9, 9, 9]);
        instance.assert_fails_with(MlDsa44Verify::C_TILDE_MISMATCH_ERROR_ID);
    }

    #[macro_rules_attr::apply(test)]
    fn refuses_another_key() {
        let mut instance = Instance::new(1);
        instance.key_digest = Instance::new(2).key_digest;
        instance.assert_fails_with(MlDsa44Verify::KEY_DIGEST_MISMATCH_ERROR_ID);
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
        assert_eq!(Some(MlDsa44Verify::C_TILDE_OUT_OF_RANGE_ERROR_ID), err.id);
    }

    #[macro_rules_attr::apply(test)]
    fn refuses_z_out_of_range() {
        let mut instance = Instance::new(1);
        instance.witness.z[2][17] = (GAMMA1 - BETA) as i32;
        instance.assert_fails_with(MlDsa44Verify::Z_OUT_OF_RANGE_ERROR_ID);
        instance.witness.z[2][17] = -((GAMMA1 - BETA) as i32);
        instance.assert_fails_with(MlDsa44Verify::Z_OUT_OF_RANGE_ERROR_ID);
    }

    #[macro_rules_attr::apply(test)]
    fn refuses_a_wrong_w() {
        let mut instance = Instance::new(1);
        instance.witness.w[1][5] += 1;
        instance.assert_fails_with(MlDsa44Verify::IDENTITY_FAILS_ERROR_ID);
    }

    #[macro_rules_attr::apply(test)]
    fn refuses_w_off_by_a_multiple_of_q_beyond_the_range_of_q_coefficients() {
        let mut instance = Instance::new(1);
        instance.witness.q[0][9] += 1 << 29;
        instance.assert_fails_with(MlDsa44Verify::Q_OUT_OF_RANGE_ERROR_ID);
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
        instance.assert_fails_with(MlDsa44Verify::LOW_BITS_OUT_OF_RANGE_ERROR_ID);
    }

    #[macro_rules_attr::apply(test)]
    fn refuses_a_wrong_e() {
        let mut instance = Instance::new(1);
        instance.witness.e[3][100] = (instance.witness.e[3][100] + 1) % 44;
        instance.assert_fails_with(MlDsa44Verify::LOW_BITS_OUT_OF_RANGE_ERROR_ID);
    }

    #[macro_rules_attr::apply(test)]
    fn refuses_hints_out_of_order_or_repeated() {
        let mut instance = Instance::new(1);
        assert!(instance.witness.hints.len() >= 2);
        instance.witness.hints.swap(0, 1);
        instance.assert_fails_with(MlDsa44Verify::HINTS_NOT_INCREASING_ERROR_ID);
        let mut instance = Instance::new(1);
        let first = instance.witness.hints[0];
        instance.witness.hints.insert(0, first);
        instance.assert_fails_with(MlDsa44Verify::HINTS_NOT_INCREASING_ERROR_ID);
    }

    #[macro_rules_attr::apply(test)]
    fn refuses_too_many_hints() {
        let mut instance = Instance::new(1);
        instance.witness.hints = (0..=OMEGA as u16).collect();
        instance.assert_fails_with(MlDsa44Verify::TOO_MANY_HINTS_ERROR_ID);
    }

    #[macro_rules_attr::apply(test)]
    fn refuses_a_dropped_hint() {
        let mut instance = Instance::new(1);
        instance.witness.hints.pop();
        instance.assert_fails_with(MlDsa44Verify::C_TILDE_MISMATCH_ERROR_ID);
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
