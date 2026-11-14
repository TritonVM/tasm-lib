//! The ML-DSA-44 verifier in Triton assembly.

use triton_vm::prelude::*;

use super::reference::ExpandedPublicKey;
use super::reference::SignatureWitness;
use super::*;
use crate::hashing::keccak::SHAKE256_RATE;
use crate::hashing::keccak::keccak_f1600::Asm;
use crate::hashing::keccak::keccak_f1600::KeccakF1600;
use crate::hashing::keccak::tables::CheckKeccakTables;
use crate::hashing::keccak::tables::KECCAK_TABLES_ADDRESS;
use crate::hashing::keccak::tables::transposition_table_address;
use crate::prelude::*;

const P: u64 = BFieldElement::P;

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
    asm: Asm,
}

impl Generator {
    fn e(&mut self, line: impl AsRef<str>) {
        self.asm.e(line);
    }

    fn label(&self, name: &str) -> String {
        format!("{}_{name}", self.entrypoint)
    }

    /// `_ → _ (*witness + offset)`
    fn witness_address(&mut self, offset: usize) {
        let ptr = self.layout.witness_ptr;
        self.e(format!("push {ptr} read_mem 1 pop 1 addi {offset}"));
    }

    /// `_ → _ (*key + offset)`
    fn key_address(&mut self, offset: usize) {
        let ptr = self.layout.key_ptr;
        self.e(format!("push {ptr} read_mem 1 pop 1 addi {offset}"));
    }

    /// Absorbs `n` words from the address on top of the stack, which it pops.
    fn absorb_words(&mut self, n: usize) {
        assert_eq!(0, n % tip5::RATE);
        self.e("push 0 push 0 push 0 push 0 pick 4");
        for _ in 0..n / tip5::RATE {
            self.e("sponge_absorb_mem");
        }
        self.e("pop 5");
    }

    fn zero(&mut self, first: u64, n: usize) {
        self.e(format!("push {first}"));
        let mut left = n;
        while left > 0 {
            let k = left.min(5);
            let zeros = vec!["push 0"; k].join(" ");
            self.e(format!("{zeros} pick {k} write_mem {k}"));
            left -= k;
        }
        self.e("pop 1");
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
    fn check_at_most(&mut self, max: u64, error_id: i128) {
        let fail = self.fail(error_id);
        self.e(format!("push {max} lt skiz call {fail}"));
    }

    /// `_ ptr_to_last → _`: checks `n` words, read downward from the pointer,
    /// for `0 ≤ v ≤ max`.
    fn range_check(&mut self, n: usize, max: u64, error_id: i128) {
        let mut left = n;
        while left > 0 {
            let k = left.min(5);
            self.e(format!("read_mem {k} place {k}"));
            for _ in 0..k {
                self.check_at_most(max, error_id);
            }
            left -= k;
        }
        self.e("pop 1");
    }

    /// `_ → _ [Σ_k coef_k·r^k]`, the coefficients `n` words from the address
    /// that `push_pointer` pushes.
    fn evaluate(&mut self, push_pointer: impl FnOnce(&mut Self), n: usize) {
        let powers = self.layout.powers;
        self.e(format!("push 0 push 0 push 0 push {powers}"));
        push_pointer(self);
        for _ in 0..n {
            self.e("xb_dot_step");
        }
        self.e("pop 2");
    }

    fn load_xfe(&mut self, address: u64) {
        self.e(format!("push {} read_mem 3 pop 1", address + 2));
    }

    fn store_xfe(&mut self, address: u64) {
        self.e(format!("push {address} write_mem 3 pop 1"));
    }

    /// `_ v_7 … v_1 v_0 → _ o_3 o_2 o_1 o_0 o_7 o_6 o_5 o_4`, with `o_3` on
    /// top: the 8×8 transposition of bits (see
    /// [`TRANSPOSITION_OFFSET`](crate::hashing::keccak::tables::TRANSPOSITION_OFFSET)),
    /// which turns the bytes of a lane into its slices and back.
    fn transpose(&mut self) {
        // One accumulator for positions 0 to 3 and one for 4 to 7, neither of
        // which wraps modulo p. Each splits into its low word, the bits of
        // outputs 0 to 3, and its high word, the bits of outputs 4 to 7.
        self.e("push 0");
        for m in 0..4 {
            let table = transposition_table_address(m);
            self.e(format!("pick 1 addi {table} read_mem 1 pop 1 add"));
        }
        self.e("push 0");
        for m in 4..8 {
            let table = transposition_table_address(m);
            self.e(format!("pick 2 addi {table} read_mem 1 pop 1 add"));
        }
        // _ first second
        self.e("split pick 2 split pick 2 add swap 2 add");
        // _ lo hi
        self.bytes_of_word();
        self.e("pick 4");
        self.bytes_of_word();
    }

    /// `_ x → _ b_3 b_2 b_1 b_0` for `x < 2^32`.
    fn bytes_of_word(&mut self) {
        for _ in 0..3 {
            self.e("push 256 pick 1 div_mod pick 1");
        }
    }

    fn slice_order_after_transpose() -> [usize; 8] {
        [3, 2, 1, 0, 7, 6, 5, 4]
    }

    /// Writes lane `lane` of the state from eight bytes at `bytes`, by
    /// storing if `xor` is false and by xoring into the state otherwise.
    fn absorb_lane(&mut self, bytes: u64, lane: usize, xor: bool) {
        self.asm.load_block(bytes + 7, 8);
        self.transpose();
        for j in Self::slice_order_after_transpose() {
            let address = self.keccak.slice_address(lane, j);
            if xor {
                self.asm.load(address);
                self.e("xor");
            }
            self.asm.store(address);
        }
    }

    /// Writes the eight bytes of lane `lane` of the state to `bytes`.
    fn squeeze_lane(&mut self, bytes: u64, lane: usize) {
        let slices: Vec<u64> = (0..8)
            .rev()
            .map(|j| self.keccak.slice_address(lane, j))
            .collect();
        self.asm.load_seq(&slices);
        self.transpose();
        let destinations = Self::slice_order_after_transpose().map(|m| bytes + m as u64);
        self.asm.store_seq(&destinations);
    }

    /// `slice ^= value` for a slice of the state.
    fn xor_constant(&mut self, lane: usize, j: usize, value: u64) {
        let address = self.keccak.slice_address(lane, j);
        self.asm.load(address);
        self.e(format!("push {value} xor"));
        self.asm.store(address);
    }

    fn entry(&mut self) {
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
        self.e(format!("{}:", self.entrypoint));
        // _ *key *witness [key_digest] [message]
        self.e(format!("push {} write_mem 5 pop 1", l.message));
        self.e(format!("push {} write_mem 5 pop 1", l.key_block));
        self.zero(l.key_block + 5, 5);
        self.e(format!("push {} write_mem 1 pop 1", l.witness_ptr));
        self.e(format!("push {} write_mem 1 pop 1", l.key_ptr));
        self.e(format!("call {}", self.check_tables));
        self.e(format!("sponge_init push {HIGH_BITS_TABLE_ADDRESS}"));
        self.absorb_words(NUM_HIGH_BITS_TABLE_WORDS);
        self.e("sponge_squeeze");
        for d in high_bits_table_digest().iter().rev() {
            self.e(format!("push {}", d.value()));
        }
        self.e(format!(
            "assert_vector error_id {} pop 5 pop 5",
            MlDsa44Verify::HIGH_BITS_TABLE_DIFFERS_ERROR_ID
        ));
        for name in names {
            self.e(format!("call {}", self.label(name)));
        }
        self.e("return");
    }

    fn check_key(&mut self) {
        self.e(format!("{}:", self.label("check_key")));
        self.e("sponge_init");
        self.key_address(0);
        self.absorb_words(ExpandedPublicKey::NUM_WORDS);
        self.e("sponge_squeeze");
        self.e(format!(
            "push {} read_mem 5 pop 1",
            self.layout.key_block + 4
        ));
        self.e(format!(
            "assert_vector error_id {} pop 5 pop 5 return",
            MlDsa44Verify::KEY_DIGEST_MISMATCH_ERROR_ID
        ));
    }

    fn challenge(&mut self) {
        let l = self.layout;
        self.e(format!("{}:", self.label("challenge")));
        self.e("sponge_init");
        self.e(format!("push {}", l.key_block));
        self.absorb_words(tip5::RATE);
        self.witness_address(0);
        self.absorb_words(SignatureWitness::NUM_CHALLENGE_WORDS);
        self.e("sponge_squeeze");
        self.store_xfe(l.r);
        self.e("pop 5 pop 2 return");
    }

    fn range_checks(&mut self) {
        self.e(format!("{}:", self.label("range_checks")));
        self.witness_address(SignatureWitness::C_TILDE_OFFSET + 31);
        self.range_check(32, 255, MlDsa44Verify::C_TILDE_OUT_OF_RANGE_ERROR_ID);
        self.witness_address(SignatureWitness::Z_OFFSET + L * N - 1);
        self.range_check(
            L * N,
            2 * SignatureWitness::Z_SHIFT,
            MlDsa44Verify::Z_OUT_OF_RANGE_ERROR_ID,
        );
        self.witness_address(SignatureWitness::Q_OFFSET + K * N - 1);
        self.range_check(
            K * N,
            2 * SignatureWitness::Q_SHIFT - 1,
            MlDsa44Verify::Q_OUT_OF_RANGE_ERROR_ID,
        );
        self.e("return");
    }

    fn powers(&mut self) {
        let l = self.layout;
        self.e(format!("{}:", self.label("powers")));
        self.load_xfe(l.r);
        self.e("push 0 push 0 push 1");
        // _ [r] [r^k]
        for k in 0..N {
            self.e(format!(
                "dup 2 dup 2 dup 2 push {} write_mem 3 pop 1",
                l.powers + 3 * k as u64
            ));
            self.e("dup 5 dup 5 dup 5 xx_mul");
        }
        // _ [r] [r^256]
        self.e(format!(
            "dup 2 dup 2 dup 2 addi 1 push {} write_mem 3 pop 1",
            l.r256_plus_1
        ));
        // The sum of the powers, (r^256 - 1)/(r - 1), by which the shifts of
        // z and Q are taken out of their evaluations.
        self.e(format!(
            "addi {} pick 5 pick 5 pick 5 addi {} x_invert xx_mul",
            neg(1),
            neg(1)
        ));
        self.store_xfe(l.power_sum);
        self.e("return");
    }

    fn sample_in_ball(&mut self) {
        let l = self.layout;
        let state = self.keccak.state_address();
        self.e(format!("{}:", self.label("sample_in_ball")));
        self.zero(state, 200);
        self.zero(l.c, N);
        for lane in 0..4 {
            self.witness_address(SignatureWitness::C_TILDE_OFFSET + 8 * lane + 7);
            self.asm.read_mem(8);
            self.e("pop 1");
            let slices: Vec<u64> = (0..8).map(|j| self.keccak.slice_address(lane, j)).collect();
            self.asm.store_seq(&slices);
        }
        // Padding: 0x1f after the 32 bytes of c̃, 0x80 in the rate's last byte.
        for j in 0..5 {
            self.xor_constant(4, j, 1);
        }
        self.xor_constant(SHAKE256_RATE / 8 - 1, 7, 0x80);
        self.e(format!("call {}", self.permute));
        self.e(format!("call {}", self.label("lower_lanes")));
        self.asm.load_block(l.squeezed + 4, 5);
        self.asm.store_block(l.signs, 5);
        self.e(format!("push 8 push {} write_mem 1 pop 1", l.position));
        for i in N - TAU..N {
            let k = i + TAU - N;
            let (byte, b) = (k / 8, k % 8);
            let factor = (-bfe!(2) * bfe!(1u64 << b).inverse()).value();
            self.e(format!("push {i} call {}", self.label("draw")));
            // _ i j
            self.e(format!("dup 0 addi {} read_mem 1 pop 1", l.c));
            self.e(format!("push {} write_mem 1 pop 1", l.c + i as u64));
            self.e(format!(
                "push {} read_mem 1 pop 1 push {} and push {factor} mul addi 1",
                l.signs + byte as u64,
                1u64 << b
            ));
            // _ i j sign
            self.e(format!("pick 1 addi {} write_mem 1 pop 2", l.c));
        }
        self.e("return");

        // _ i → _ i j, the next squeezed byte j ≤ i
        self.e(format!("{}:", self.label("draw")));
        self.e(format!("call {}", self.label("next_byte")));
        self.e("dup 1 addi 1 dup 1 lt skiz return pop 1 recurse");

        // _ → _ byte
        self.e(format!("{}:", self.label("next_byte")));
        self.e(format!("push {} read_mem 1 pop 1", l.position));
        self.e(format!(
            "dup 0 push {SHAKE256_RATE} eq skiz call {}",
            self.label("refill")
        ));
        self.e(format!(
            "dup 0 push 64 eq skiz call {}",
            self.label("upper_lanes")
        ));
        self.e(format!(
            "dup 0 addi 1 push {} write_mem 1 pop 1",
            l.position
        ));
        self.e(format!("addi {} read_mem 1 pop 1 return", l.squeezed));

        self.e(format!("{}:", self.label("refill")));
        self.e(format!(
            "pop 1 call {} call {} push 0 return",
            self.permute,
            self.label("lower_lanes")
        ));

        self.e(format!("{}:", self.label("lower_lanes")));
        for lane in 0..8 {
            self.squeeze_lane(l.squeezed + 8 * lane as u64, lane);
        }
        self.e("return");

        self.e(format!("{}:", self.label("upper_lanes")));
        for lane in 8..SHAKE256_RATE / 8 {
            self.squeeze_lane(l.squeezed + 8 * lane as u64, lane);
        }
        self.e("return");
    }

    fn identity(&mut self) {
        let l = self.layout;
        self.e(format!("{}:", self.label("identity")));
        for j in 0..L {
            let offset = SignatureWitness::Z_OFFSET + N * j;
            self.evaluate(|g| g.witness_address(offset), N);
            self.load_xfe(l.power_sum);
            self.e(format!(
                "push {} xb_mul xx_add",
                neg(SignatureWitness::Z_SHIFT)
            ));
            self.store_xfe(l.z_evals + 3 * j as u64);
        }
        let c = l.c;
        self.evaluate(|g| g.e(format!("push {c}")), N);
        self.store_xfe(l.c_eval);
        for i in 0..K {
            for j in 0..L {
                let offset = ExpandedPublicKey::A_OFFSET + N * (L * i + j);
                self.evaluate(|g| g.key_address(offset), N);
                self.load_xfe(l.z_evals + 3 * j as u64);
                self.e("xx_mul");
                if j > 0 {
                    self.e("xx_add");
                }
            }
            let offset = ExpandedPublicKey::T1_OFFSET + N * i;
            self.evaluate(|g| g.key_address(offset), N);
            self.load_xfe(l.c_eval);
            self.e(format!("xx_mul push {} xb_mul xx_add", neg(1)));
            let offset = SignatureWitness::W_OFFSET + N * i;
            self.evaluate(|g| g.witness_address(offset), N);
            self.e(format!("push {} xb_mul xx_add", neg(1)));
            let offset = SignatureWitness::S_OFFSET + (N - 1) * i;
            self.evaluate(|g| g.witness_address(offset), N - 1);
            self.load_xfe(l.r256_plus_1);
            self.e(format!("xx_mul push {} xb_mul xx_add", neg(1)));
            let offset = SignatureWitness::Q_OFFSET + N * i;
            self.evaluate(|g| g.witness_address(offset), N);
            self.load_xfe(l.power_sum);
            self.e(format!(
                "push {} xb_mul xx_add",
                neg(SignatureWitness::Q_SHIFT)
            ));
            self.e(format!("push {} xb_mul xx_add", neg(u64::from(Q))));
            let id = MlDsa44Verify::IDENTITY_FAILS_ERROR_ID;
            for _ in 0..3 {
                self.e(format!("push 0 eq assert error_id {id}"));
            }
        }
        self.e("return");
    }

    fn use_hint(&mut self) {
        let l = self.layout;
        let two_gamma2 = 2 * u64::from(GAMMA2);
        let low_id = MlDsa44Verify::LOW_BITS_OUT_OF_RANGE_ERROR_ID;
        let high_id = MlDsa44Verify::HIGH_BITS_OUT_OF_RANGE_ERROR_ID;
        let w_to_e = SignatureWitness::E_OFFSET - SignatureWitness::W_OFFSET;
        self.e(format!("{}:", self.label("use_hint")));
        // For every coefficient r with high bits e: r - 2·γ2·e lies in
        // (-γ2, γ2], e ≤ 44, and w1 = e mod 44. Four coefficients make a
        // 24-bit group of w1Encode.
        self.witness_address(SignatureWitness::W_OFFSET + 3);
        for g in 0..K * N / 4 {
            // _ *r_{4g+3}
            self.e(format!(
                "dup 0 read_mem 4 pop 1 dup 4 addi {w_to_e} read_mem 4 pop 1"
            ));
            for m in 0..4 {
                // _ w1_0..w1_{m-1} r_3..r_m e_3..e_m, top last; each w1 went
                // below the remaining r and e
                self.e(format!(
                    "dup 0 push {} mul pick {} add addi {}",
                    neg(two_gamma2),
                    5 - m,
                    GAMMA2 - 1
                ));
                self.check_at_most(two_gamma2 - 1, low_id);
                self.e(format!(
                    "push {} dup 1 lt assert error_id {high_id}",
                    W1_RANGE + 1
                ));
                self.e(format!("addi {HIGH_BITS_TABLE_ADDRESS} read_mem 1 pop 1"));
                if m < 3 {
                    self.e(format!("place {}", 6 - 2 * m));
                }
            }
            // _ *r w1_0 w1_1 w1_2 w1_3
            self.e("push 64 mul add push 64 mul add push 64 mul add");
            self.e(format!(
                "push {} write_mem 1 pop 1 addi 4",
                l.w1_groups + g as u64
            ));
        }
        self.e("pop 1");
        // The hints: a count of at most ω, then strictly increasing positions.
        self.witness_address(SignatureWitness::HINT_COUNT_OFFSET);
        self.e("read_mem 1 pop 1");
        self.e(format!(
            "push {} dup 1 lt assert error_id {}",
            OMEGA + 1,
            MlDsa44Verify::TOO_MANY_HINTS_ERROR_ID
        ));
        self.witness_address(SignatureWitness::HINTS_OFFSET);
        self.e("push 0");
        // _ count *hint lower
        self.e(format!("call {}", self.label("hint_loop")));
        self.e("pop 3");
        // The bytes of w1Encode, three per group.
        for g in 0..K * N / 4 {
            self.e(format!("push {} read_mem 1 pop 1", l.w1_groups + g as u64));
            self.e("push 256 pick 1 div_mod pick 1 push 256 pick 1 div_mod pick 1");
            self.e(format!(
                "swap 2 push {} write_mem 3 pop 1",
                l.w1_bytes + 3 * g as u64
            ));
        }
        self.e("return");

        let order_id = MlDsa44Verify::HINTS_NOT_INCREASING_ERROR_ID;
        self.e(format!("{}:", self.label("hint_loop")));
        self.e("dup 2 push 0 eq skiz return");
        self.e("dup 1 read_mem 1 pop 1");
        // _ count *hint lower n
        self.e(format!(
            "push {} dup 1 lt assert error_id {order_id}",
            K * N
        ));
        self.e(format!("dup 0 dup 2 push {} mul add", neg(1)));
        self.e(format!(
            "push {} swap 1 lt assert error_id {order_id}",
            K * N
        ));
        self.e(format!("dup 0 call {}", self.label("apply_hint")));
        self.e("addi 1 swap 1 pop 1 swap 1 addi 1 swap 1");
        self.e(format!("pick 2 addi {} place 2 recurse", neg(1)));

        // _ n → _, and w1[n] becomes (w1[n] ± 1) mod 44, by the sign of r0,
        // in its group.
        self.e(format!("{}:", self.label("apply_hint")));
        self.e("dup 0");
        self.witness_address(SignatureWitness::W_OFFSET);
        self.e("add read_mem 1 pop 1 dup 1");
        self.witness_address(SignatureWitness::E_OFFSET);
        self.e("add read_mem 1 pop 1");
        // _ n r e
        self.e(format!("dup 0 push {W1_RANGE} eq"));
        self.e(format!("dup 1 push {} mul pick 3 add", neg(two_gamma2)));
        self.e(format!("pick 1 push {} mul add", neg(1)));
        // _ n e r0, and r0 > 0 iff γ2 + 1 < r0 + γ2 + 1
        self.e(format!("addi {} push {} lt", GAMMA2 + 1, GAMMA2 + 1));
        self.e(format!("push 2 mul addi {}", neg(1)));
        // _ n e delta
        self.e(format!(
            "pick 1 addi {HIGH_BITS_TABLE_ADDRESS} read_mem 1 pop 1"
        ));
        self.e(format!("dup 0 pick 2 add addi {W1_RANGE}"));
        self.e(format!("push {W1_RANGE} swap 1 div_mod swap 1 pop 1"));
        // _ n old new
        self.e(format!("pick 1 push {} mul add", neg(1)));
        self.e("pick 1 push 4 swap 1 div_mod");
        // _ diff g (n mod 4)
        self.e(format!(
            "addi {} read_mem 1 pop 1 pick 2 mul",
            HIGH_BITS_TABLE_ADDRESS + POWERS_OF_64_OFFSET
        ));
        self.e(format!("dup 1 addi {} read_mem 1 pop 1 add", l.w1_groups));
        self.e(format!(
            "pick 1 addi {} write_mem 1 pop 1 return",
            l.w1_groups
        ));
    }

    fn mu(&mut self) {
        let l = self.layout;
        let state = self.keccak.state_address();
        self.e(format!("{}:", self.label("mu")));
        self.zero(state, 200);
        for lane in 0..8 {
            self.key_address(ExpandedPublicKey::TR_OFFSET + 8 * lane + 7);
            self.asm.read_mem(8);
            self.e("pop 1");
            let slices: Vec<u64> = (0..8).map(|j| self.keccak.slice_address(lane, j)).collect();
            self.asm.store_seq(&slices);
        }
        // M' = 0 ‖ 0 ‖ message bytes, then 0x1f, then zeros, as bytes 64 to 111.
        self.zero(l.message_bytes, 48);
        self.e(format!(
            "push 31 push {} write_mem 1 pop 1",
            l.message_bytes + 42
        ));
        for i in 0..Digest::LEN {
            let first = l.message_bytes + 2 + 8 * i as u64;
            self.e(format!(
                "push {} read_mem 1 pop 1 split",
                l.message + i as u64
            ));
            // _ hi lo
            self.bytes_of_word();
            self.asm
                .store_seq(&[first + 3, first + 2, first + 1, first]);
            self.bytes_of_word();
            self.asm
                .store_seq(&[first + 7, first + 6, first + 5, first + 4]);
        }
        for lane in 8..14 {
            self.absorb_lane(l.message_bytes + 8 * (lane as u64 - 8), lane, false);
        }
        self.xor_constant(SHAKE256_RATE / 8 - 1, 7, 0x80);
        self.e(format!("call {} return", self.permute));
    }

    fn commitment(&mut self) {
        let l = self.layout;
        self.e(format!("{}:", self.label("commitment")));
        // The state holds μ in lanes 0 to 7. The first block is μ followed by
        // the first 72 bytes of w1Encode(w1).
        for lane in 17..25 {
            for j in 0..8 {
                let address = self.keccak.slice_address(lane, j);
                self.e(format!("push 0 push {address} write_mem 1 pop 1"));
            }
        }
        let rate_lanes = SHAKE256_RATE / 8;
        for lane in 8..rate_lanes {
            self.absorb_lane(l.w1_bytes + 8 * (lane as u64 - 8), lane, false);
        }
        self.e(format!("call {}", self.permute));
        let num_bytes = K * N * 6 / 8;
        let mut offset = 8 * (rate_lanes - 8);
        // The padding goes into a block that w1Encode leaves partly empty.
        assert_ne!(0, (num_bytes - offset) % SHAKE256_RATE);
        while offset < num_bytes {
            let lanes = ((num_bytes - offset) / 8).min(rate_lanes);
            for lane in 0..lanes {
                self.absorb_lane(l.w1_bytes + (offset + 8 * lane) as u64, lane, true);
            }
            offset += 8 * lanes;
            if lanes == rate_lanes {
                self.e(format!("call {}", self.permute));
            } else {
                // Padding: 0x1f after the last byte, 0x80 in the rate's last byte.
                for j in 0..5 {
                    self.xor_constant(lanes, j, 1);
                }
                self.xor_constant(rate_lanes - 1, 7, 0x80);
                self.e(format!("call {}", self.permute));
            }
        }
        // c̃' is the first 32 bytes, lanes 0 to 3, compared slice by slice.
        let id = MlDsa44Verify::C_TILDE_MISMATCH_ERROR_ID;
        for lane in 0..4 {
            self.witness_address(SignatureWitness::C_TILDE_OFFSET + 8 * lane + 7);
            self.asm.read_mem(8);
            self.e("pop 1");
            for j in 0..8 {
                let address = self.keccak.slice_address(lane, j);
                self.e(format!(
                    "push {address} read_mem 1 pop 1 eq assert error_id {id}"
                ));
            }
        }
        self.e("return");
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
            asm: Asm::default(),
        };
        generator.entry();
        generator.check_key();
        generator.challenge();
        generator.range_checks();
        generator.powers();
        generator.sample_in_ball();
        generator.identity();
        generator.use_hint();
        generator.mu();
        generator.commitment();
        for error_id in generator.failures.clone() {
            let label = generator.label(&format!("fail_{error_id}"));
            generator.e(format!("{label}: push 0 assert error_id {error_id}"));
        }
        generator.asm.into_instructions()
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
