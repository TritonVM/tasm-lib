//! SHA3-256, SHA3-512, SHAKE128 and SHAKE256 on byte strings in memory, each
//! with a dedicated snippet for an input of any length and one for an input of
//! a length fixed by the snippet.
//!
//! A byte string `b_0 … b_{n-1}` is held as `⌈n/4⌉` field elements, of which
//! element `w` is the integer `Σ_{k<4} b_{4w+k}·256^k`, with `b_i = 0` for
//! `i ≥ n`. Each element is thus below `2^32`, and the snippets crash on an
//! element that is not, or on one with a nonzero `b_i` for `i ≥ n`, with the
//! error ID `NONZERO_PADDING_ERROR_ID` of the snippet.
//!
//! Every snippet here hashes the `input_len` bytes at `*input` to the
//! `output_len` bytes at `*output`. Lengths count bytes.
//!
//! ### Pre-conditions
//!
//! - [`CheckKeccakTables`](super::tables::CheckKeccakTables) ran before
//! - `*input` points to `⌈input_len/4⌉` field elements
//!
//! ### Post-conditions
//!
//! - `*output` points to `⌈output_len/4⌉` field elements, which hold the
//!   function's `output_len` bytes of output on the `input_len` bytes at
//!   `*input`, and which lie in memory that the snippet allocates with
//!   [`DynMalloc`]
//!
//! The snippets use the state of their [`KeccakF1600`]. Those for an input of
//! any length also use a static buffer of `rate/4 + 1` words.
//!
//! ### Example
//!
//! A program that computes SHA3-256 of the 3-byte string `abc` consists of:
//!
//! 1. a [`KeccakF1600`], which allocates the permutation's state in the
//!    program's static memory, and which every Keccak snippet of the program
//!    can share;
//! 2. a call to [`CheckKeccakTables`](super::tables::CheckKeccakTables), once,
//!    before the first call to a hash snippet;
//! 3. a call to [`Sha3_256`] on `*input` and `input_len`.
//!
//! The prover puts the tables into non-deterministic memory with
//! [`insert_tables`](super::tables::insert_tables). The caller's own
//! non-deterministic memory must avoid the `NUM_TABLE_WORDS` words from
//! [`KECCAK_TABLES_ADDRESS`](super::tables::KECCAK_TABLES_ADDRESS) on.
//!
//! The input `abc` is the field element `0x61 + 0x62·256 + 0x63·256²`, here
//! at address 0. The digest is 32 bytes, so 8 field elements from `*digest`
//! on, of which element `w` holds bytes `4w` to `4w + 3`.
//!
//! ```
//! use std::collections::HashMap;
//!
//! use tasm_lib::hashing::keccak::hash::Sha3_256;
//! use tasm_lib::hashing::keccak::keccak_f1600::KeccakF1600;
//! use tasm_lib::hashing::keccak::tables::CheckKeccakTables;
//! use tasm_lib::hashing::keccak::tables::insert_tables;
//! use tasm_lib::prelude::*;
//! use tasm_lib::triton_vm::prelude::*;
//!
//! let mut library = Library::new();
//! let keccak = KeccakF1600::allocate(&mut library);
//! let check_tables = library.import(Box::new(CheckKeccakTables));
//! let sha3_256 = library.import(Box::new(Sha3_256::new(keccak)));
//! let code = triton_asm!(
//!     call {check_tables}
//!     push 0              // _ *input
//!     push 3              // _ *input input_len
//!     call {sha3_256}     // _ *digest 32
//!     halt
//!     {&library.all_imports()}
//! );
//!
//! let mut ram = HashMap::new();
//! insert_tables(&mut ram);
//! ram.insert(bfe!(0), bfe!(u32::from_le_bytes(*b"abc\0")));
//!
//! let nondeterminism = NonDeterminism::default().with_ram(ram);
//! let mut vm_state = VMState::new(Program::new(&code), PublicInput::default(), nondeterminism);
//! vm_state.run().unwrap();
//!
//! let [.., digest_pointer, digest_len] = vm_state.op_stack.stack[..] else {
//!     unreachable!()
//! };
//! assert_eq!(bfe!(32), digest_len);
//! let digest: Vec<u8> = (0..8)
//!     .flat_map(|w| {
//!         let element = vm_state.ram[&(digest_pointer + bfe!(w))];
//!         u32::try_from(element.value()).unwrap().to_le_bytes()
//!     })
//!     .collect();
//! assert_eq!(
//!     "3a985da74fe225b2045c172d6bd390bd855f086e3e9d525b46bfe24511431532",
//!     hex::encode(digest),
//! );
//! ```

use triton_vm::prelude::*;

use super::KeccakFunction;
use super::keccak_f1600::KeccakF1600;
use crate::memory::write_zeros_to_memory;
use crate::prelude::*;

/// `_ r → _ ⌈r/4⌉` for a u32 `r < 2^32 - 3`.
fn words_of_bytes() -> Vec<LabelledInstruction> {
    triton_asm!(addi 3 push 4 swap 1 div_mod pop 1)
}

/// A [function](KeccakFunction) on a byte string of any length, of which the
/// output is a byte string of a length that the caller chooses for SHAKE and
/// that is the digest length for SHA3.
///
/// ### Behavior
///
/// For SHA3-256 and SHA3-512:
///
/// ```text
/// BEFORE: _ *input [input_len: u32]
/// AFTER:  _ *output [output_len: u32]
/// ```
///
/// For SHAKE128 and SHAKE256:
///
/// ```text
/// BEFORE: _ *input [input_len: u32] [output_len: u32]
/// AFTER:  _ *output [output_len: u32]
/// ```
///
/// The lengths count bytes, and the strings are field elements (see
/// [the module](self)). The output lies in memory that the snippet allocates
/// with [`DynMalloc`].
///
/// The snippet uses the state of its [`KeccakF1600`] and a static buffer of
/// `rate/4 + 1` words.
///
/// ### Pre-conditions
///
/// - [`CheckKeccakTables`](super::tables::CheckKeccakTables) ran before
/// - `*input` points to `⌈input_len/4⌉` words
///
/// ### Post-conditions
///
/// - `*output` points to `⌈output_len/4⌉` words, which hold the function's
///   `output_len` bytes of output on the `input_len` bytes at `*input`
///
/// The snippet crashes if a word of the input is not a u32 or if a byte of
/// the last word from `input_len` on is not zero.
#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub(crate) struct KeccakHash {
    function: KeccakFunction,
    keccak: KeccakF1600,
}

impl KeccakHash {
    pub(crate) const NONZERO_PADDING_ERROR_ID: i128 = 641;

    pub(crate) fn new(function: KeccakFunction, keccak: KeccakF1600) -> Self {
        Self { function, keccak }
    }
}

impl BasicSnippet for KeccakHash {
    fn parameters(&self) -> Vec<(DataType, String)> {
        let mut parameters = vec![
            (DataType::VoidPointer, "*input".to_string()),
            (DataType::U32, "input_len".to_string()),
        ];
        if self.function.digest_len().is_none() {
            parameters.push((DataType::U32, "output_len".to_string()));
        }
        parameters
    }

    fn return_values(&self) -> Vec<(DataType, String)> {
        vec![
            (DataType::VoidPointer, "*output".to_string()),
            (DataType::U32, "output_len".to_string()),
        ]
    }

    fn entrypoint(&self) -> String {
        format!(
            "tasmlib_hashing_keccak_{}_{:x}",
            self.function.name(),
            self.keccak.state_address()
        )
    }

    fn code(&self, library: &mut Library) -> Vec<LabelledInstruction> {
        let entrypoint = self.entrypoint();
        let absorb_loop = format!("{entrypoint}_absorb_loop");
        let absorb_block = format!("{entrypoint}_absorb_block");
        let squeeze_loop = format!("{entrypoint}_squeeze_loop");
        let squeeze_block = format!("{entrypoint}_squeeze_block");

        let rate = self.function.rate();
        let rate_words = rate / 4;
        // ponytail: one word past the block, so that the output's last word
        // is masked in the buffer even when it is the block's last word.
        let buffer = library
            .kmalloc((rate_words + 1) as u32)
            .write_address()
            .value();
        let permute = library.import(Box::new(self.keccak));
        let memcpy = library.import(Box::new(MemCpy));
        let dyn_malloc = library.import(Box::new(DynMalloc));
        let push_output_len = match self.function.digest_len() {
            Some(len) => triton_asm!(push { len }),
            None => vec![],
        };
        let last_buffer_word = buffer + rate_words as u64 - 1;

        triton_asm!(
            // BEFORE: _ *input input_len [output_len]
            // AFTER:  _ *output output_len
            {entrypoint}:
                {&push_output_len}
                place 2
                // _ output_len *input input_len
                {&write_zeros_to_memory(self.keccak.state_address(), 200)}
                call {absorb_loop}
                // _ output_len *input r, with r < rate

                // The last block, from the buffer.
                {&write_zeros_to_memory(buffer, rate_words + 1)}
                dup 1 push {buffer} dup 2 {&words_of_bytes()}
                call {memcpy}
                swap 1 pop 1
                // _ output_len r
                push 4 dup 1 div_mod
                push 256 pow
                // _ output_len r ⌊r/4⌋ 256^(r mod 4)
                dup 1 addi {buffer} read_mem 1 pop 1
                dup 1 dup 1 lt
                assert error_id {Self::NONZERO_PADDING_ERROR_ID}
                // _ output_len r ⌊r/4⌋ 256^(r mod 4) last_word
                pick 1 push {self.function.padding_byte()} mul add
                pick 1 addi {buffer} write_mem 1 pop 2
                push {last_buffer_word} read_mem 1 pop 1
                addi {0x80u64 << 24}
                push {last_buffer_word} write_mem 1 pop 1
                push {buffer} call {absorb_block} pop 1
                call {permute}
                // _ output_len

                call {dyn_malloc}
                dup 0 dup 2
                // _ output_len *output *output output_len
                call {squeeze_loop}
                // _ output_len *output *words left, with left ≤ rate

                // The last block, through the buffer.
                push {buffer} call {squeeze_block} pop 1
                push 4 dup 1 div_mod
                push 256 pow
                // _ output_len *output *words left ⌊left/4⌋ 256^(left mod 4)
                dup 1 addi {buffer} read_mem 1 pop 1
                div_mod swap 1 pop 1
                pick 1 addi {buffer} write_mem 1 pop 1
                // _ output_len *output *words left
                {&words_of_bytes()}
                push {buffer} place 2
                call {memcpy}
                swap 1
                return

            // BEFORE: _ *input n
            // AFTER:  _ (*input + ⌊n/rate⌋·rate/4) (n mod rate)
            {absorb_loop}:
                push {rate} dup 1 lt
                skiz return
                pick 1 call {absorb_block} call {permute} place 1
                addi {-(rate as i64)}
                recurse

            // BEFORE: _ *words left
            // AFTER:  _ (*words + k·rate/4) (left - k·rate), for the least k
            //         with left - k·rate ≤ rate
            {squeeze_loop}:
                push {rate + 1} dup 1 lt
                skiz return
                pick 1 call {squeeze_block} call {permute} place 1
                addi {-(rate as i64)}
                recurse

            {absorb_block}:
                {&self.keccak.absorb_block(rate)}
                return

            {squeeze_block}:
                {&self.keccak.squeeze_block(rate)}
                return
        )
    }
}

/// A [function](KeccakFunction) on a byte string of a length fixed by the
/// snippet, with an output of a length fixed by the snippet.
///
/// ### Behavior
///
/// ```text
/// BEFORE: _ *input
/// AFTER:  _ *output
/// ```
///
/// The strings are field elements (see [the module](self)). The output lies in
/// memory that the snippet allocates with [`DynMalloc`].
///
/// The snippet uses the state of its [`KeccakF1600`].
///
/// ### Pre-conditions
///
/// - [`CheckKeccakTables`](super::tables::CheckKeccakTables) ran before
/// - `*input` points to `⌈input_len/4⌉` words
///
/// ### Post-conditions
///
/// - `*output` points to `⌈output_len/4⌉` words, which hold the function's
///   `output_len` bytes of output on the `input_len` bytes at `*input`
///
/// The snippet crashes if a word of the input is not a u32 or if a byte of
/// the last word from `input_len` on is not zero.
#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub(crate) struct KeccakHashStaticSize {
    function: KeccakFunction,
    keccak: KeccakF1600,
    input_len: usize,
    output_len: usize,
}

impl KeccakHashStaticSize {
    pub(crate) const NONZERO_PADDING_ERROR_ID: i128 = 642;

    /// # Panics
    ///
    /// Panics if `function` is SHA3 and `output_len` is not its digest length.
    pub(crate) fn new(
        function: KeccakFunction,
        keccak: KeccakF1600,
        input_len: usize,
        output_len: usize,
    ) -> Self {
        assert!(
            function.digest_len().is_none_or(|len| len == output_len),
            "the output of {function:?} has a fixed length",
        );
        Self {
            function,
            keccak,
            input_len,
            output_len,
        }
    }

    /// `_ *words → _ *words`: xors the last `k < 8` bytes of the input, from
    /// `*words`, into lane `lane`, and checks that the bytes of their last
    /// word from `k` on are zero.
    fn absorb_partial_lane(&self, lane: usize, k: usize) -> Vec<LabelledInstruction> {
        let id = Self::NONZERO_PADDING_ERROR_ID;
        let read_and_check = if k <= 4 {
            let check = if k < 4 {
                triton_asm!(push {1u64 << (8 * k)} dup 1 lt assert error_id {id})
            } else {
                vec![]
            };
            triton_asm!(
                dup 0 read_mem 1 pop 1
                // _ *words lo
                {&check}
                push 0 swap 1
            )
        } else {
            triton_asm!(
                dup 0 addi 1 read_mem 2 pop 1
                // _ *words hi lo
                push {1u64 << (8 * (k - 4))} dup 2 lt assert error_id {id}
            )
        };
        triton_asm!(
            {&read_and_check}
            // _ *words hi lo
            {&self.keccak.absorb_words(lane)}
        )
    }

    /// `_ *words → _ (*words + ⌈k/4⌉)`: writes the first `k < 8` bytes of
    /// lane `lane` to `*words`.
    fn squeeze_partial_lane(&self, lane: usize, k: usize) -> Vec<LabelledInstruction> {
        let read = self.keccak.read_lane_as_words(lane);
        if k <= 4 {
            let mask = if k < 4 {
                triton_asm!(push {1u64 << (8 * k)} swap 1 div_mod swap 1 pop 1)
            } else {
                vec![]
            };
            triton_asm!(
                {&read}
                // _ *words lo hi
                pop 1
                {&mask}
                pick 1 write_mem 1
            )
        } else {
            triton_asm!(
                {&read}
                // _ *words lo hi
                push {1u64 << (8 * (k - 4))} swap 1 div_mod swap 1 pop 1
                swap 1 pick 2 write_mem 2
            )
        }
    }
}

impl BasicSnippet for KeccakHashStaticSize {
    fn parameters(&self) -> Vec<(DataType, String)> {
        vec![(DataType::VoidPointer, "*input".to_string())]
    }

    fn return_values(&self) -> Vec<(DataType, String)> {
        vec![(DataType::VoidPointer, "*output".to_string())]
    }

    fn entrypoint(&self) -> String {
        format!(
            "tasmlib_hashing_keccak_{}_static_size_{}_{}_{:x}",
            self.function.name(),
            self.input_len,
            self.output_len,
            self.keccak.state_address()
        )
    }

    fn code(&self, library: &mut Library) -> Vec<LabelledInstruction> {
        let entrypoint = self.entrypoint();
        let absorb_block = format!("{entrypoint}_absorb_block");
        let squeeze_block = format!("{entrypoint}_squeeze_block");

        let rate = self.function.rate();
        let permute = library.import(Box::new(self.keccak));
        let dyn_malloc = library.import(Box::new(DynMalloc));

        let full_blocks = self.input_len / rate;
        let r = self.input_len % rate;
        let absorb_full_blocks =
            vec![triton_asm!(call {absorb_block} call {permute}); full_blocks].concat();
        let mut absorb_last_block = (0..r / 8)
            .flat_map(|lane| self.keccak.absorb_lane_from_words(lane))
            .collect::<Vec<_>>();
        if !r.is_multiple_of(8) {
            absorb_last_block.extend(self.absorb_partial_lane(r / 8, r % 8));
        }

        // Every block but the last is squeezed in full, and is followed by a
        // permutation.
        let squeezed_blocks = self.output_len.saturating_sub(1) / rate;
        let left = self.output_len - squeezed_blocks * rate;
        let squeeze_full_blocks =
            vec![triton_asm!(call {squeeze_block} call {permute}); squeezed_blocks].concat();
        let mut squeeze_last_block = (0..left / 8)
            .flat_map(|lane| self.keccak.squeeze_lane_to_words(lane))
            .collect::<Vec<_>>();
        if !left.is_multiple_of(8) {
            squeeze_last_block.extend(self.squeeze_partial_lane(left / 8, left % 8));
        }

        let mut subroutines = vec![];
        if full_blocks > 0 {
            subroutines.extend(triton_asm!(
                {absorb_block}:
                    {&self.keccak.absorb_block(rate)}
                    return
            ));
        }
        if squeezed_blocks > 0 {
            subroutines.extend(triton_asm!(
                {squeeze_block}:
                    {&self.keccak.squeeze_block(rate)}
                    return
            ));
        }

        triton_asm!(
            // BEFORE: _ *input
            // AFTER:  _ *output
            {entrypoint}:
                {&write_zeros_to_memory(self.keccak.state_address(), 200)}
                {&absorb_full_blocks}
                {&absorb_last_block}
                pop 1
                {&self.keccak.pad(rate, r, self.function.padding_byte())}
                call {permute}
                call {dyn_malloc}
                dup 0
                // _ *output *output
                {&squeeze_full_blocks}
                {&squeeze_last_block}
                pop 1
                return

            {&subroutines}
        )
    }
}

/// A snippet that is `KeccakHash` or `KeccakHashStaticSize` for one
/// [function](KeccakFunction), with the same entrypoint and code.
macro_rules! dedicated_snippet {
    (
        $(#[$doc:meta])*
        $name:ident($inner:ident) = |$($arg:ident: $ty:ty),*| $construct:expr
    ) => {
        $(#[$doc])*
        #[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
        pub struct $name($inner);

        impl $name {
            pub const NONZERO_PADDING_ERROR_ID: i128 = $inner::NONZERO_PADDING_ERROR_ID;

            pub fn new($($arg: $ty),*) -> Self {
                Self($construct)
            }
        }

        impl BasicSnippet for $name {
            fn parameters(&self) -> Vec<(DataType, String)> {
                self.0.parameters()
            }

            fn return_values(&self) -> Vec<(DataType, String)> {
                self.0.return_values()
            }

            fn entrypoint(&self) -> String {
                self.0.entrypoint()
            }

            fn code(&self, library: &mut Library) -> Vec<LabelledInstruction> {
                self.0.code(library)
            }
        }
    };
}

dedicated_snippet! {
    /// SHA3-256 of a byte string of any length.
    ///
    /// ```text
    /// BEFORE: _ *input [input_len: u32]
    /// AFTER:  _ *digest [32: u32]
    /// ```
    Sha3_256(KeccakHash) = |keccak: KeccakF1600|
        KeccakHash::new(KeccakFunction::Sha3_256, keccak)
}

dedicated_snippet! {
    /// SHA3-256 of a byte string of length `input_len`, fixed by the snippet.
    ///
    /// ```text
    /// BEFORE: _ *input
    /// AFTER:  _ *digest
    /// ```
    ///
    /// The digest is 32 bytes long.
    Sha3_256StaticSize(KeccakHashStaticSize) = |keccak: KeccakF1600, input_len: usize|
        KeccakHashStaticSize::new(KeccakFunction::Sha3_256, keccak, input_len, 32)
}

dedicated_snippet! {
    /// SHA3-512 of a byte string of any length.
    ///
    /// ```text
    /// BEFORE: _ *input [input_len: u32]
    /// AFTER:  _ *digest [64: u32]
    /// ```
    Sha3_512(KeccakHash) = |keccak: KeccakF1600|
        KeccakHash::new(KeccakFunction::Sha3_512, keccak)
}

dedicated_snippet! {
    /// SHA3-512 of a byte string of length `input_len`, fixed by the snippet.
    ///
    /// ```text
    /// BEFORE: _ *input
    /// AFTER:  _ *digest
    /// ```
    ///
    /// The digest is 64 bytes long.
    Sha3_512StaticSize(KeccakHashStaticSize) = |keccak: KeccakF1600, input_len: usize|
        KeccakHashStaticSize::new(KeccakFunction::Sha3_512, keccak, input_len, 64)
}

dedicated_snippet! {
    /// SHAKE128 of a byte string of any length, with an output of any length.
    ///
    /// ```text
    /// BEFORE: _ *input [input_len: u32] [output_len: u32]
    /// AFTER:  _ *output [output_len: u32]
    /// ```
    Shake128(KeccakHash) = |keccak: KeccakF1600|
        KeccakHash::new(KeccakFunction::Shake128, keccak)
}

dedicated_snippet! {
    /// SHAKE128 of a byte string of length `input_len`, with an output of
    /// length `output_len`, both fixed by the snippet.
    ///
    /// ```text
    /// BEFORE: _ *input
    /// AFTER:  _ *output
    /// ```
    Shake128StaticSize(KeccakHashStaticSize) =
        |keccak: KeccakF1600, input_len: usize, output_len: usize|
        KeccakHashStaticSize::new(KeccakFunction::Shake128, keccak, input_len, output_len)
}

dedicated_snippet! {
    /// SHAKE256 of a byte string of any length, with an output of any length.
    ///
    /// ```text
    /// BEFORE: _ *input [input_len: u32] [output_len: u32]
    /// AFTER:  _ *output [output_len: u32]
    /// ```
    Shake256(KeccakHash) = |keccak: KeccakF1600|
        KeccakHash::new(KeccakFunction::Shake256, keccak)
}

dedicated_snippet! {
    /// SHAKE256 of a byte string of length `input_len`, with an output of
    /// length `output_len`, both fixed by the snippet.
    ///
    /// ```text
    /// BEFORE: _ *input
    /// AFTER:  _ *output
    /// ```
    Shake256StaticSize(KeccakHashStaticSize) =
        |keccak: KeccakF1600, input_len: usize, output_len: usize|
        KeccakHashStaticSize::new(KeccakFunction::Shake256, keccak, input_len, output_len)
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use proptest::collection::vec;

    use super::super::tables::CheckKeccakTables;
    use super::super::tables::insert_tables;
    use super::super::tests::fips_202;
    use super::*;
    use crate::linker::execute_bench;
    use crate::snippet_bencher::BenchmarkCase;
    use crate::snippet_bencher::NamedBenchmarkResult;
    use crate::snippet_bencher::write_benchmarks;
    use crate::test_prelude::*;

    const INPUT_ADDRESS: u64 = 0;

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

    /// A program that checks the tables, pushes `arguments`, and calls the
    /// snippet that `snippet` makes, if any.
    fn program(
        snippet: Option<&dyn Fn(KeccakF1600) -> Box<dyn BasicSnippet>>,
        arguments: &[u64],
    ) -> Vec<LabelledInstruction> {
        let mut library = Library::new();
        let keccak = KeccakF1600::allocate(&mut library);
        let check = library.import(Box::new(CheckKeccakTables));
        let call_hash = snippet
            .map(|snippet| {
                let hash = library.import(snippet(keccak));
                triton_asm!(call { hash })
            })
            .unwrap_or_default();
        let push_arguments = arguments
            .iter()
            .flat_map(|&a| triton_asm!(push { a }))
            .collect::<Vec<_>>();
        triton_asm!(
            call {check}
            {&push_arguments}
            {&call_hash}
            halt
            {&library.all_imports()}
        )
    }

    /// The tables, and `input` at [`INPUT_ADDRESS`].
    fn nondeterminism(input: &[BFieldElement]) -> NonDeterminism {
        let mut memory = HashMap::new();
        insert_tables(&mut memory);
        for (i, &word) in input.iter().enumerate() {
            memory.insert(bfe!(INPUT_ADDRESS + i as u64), word);
        }
        NonDeterminism::default().with_ram(memory)
    }

    /// Runs [`program`] with `input` at [`INPUT_ADDRESS`].
    fn run(
        snippet: impl Fn(KeccakF1600) -> Box<dyn BasicSnippet>,
        arguments: &[u64],
        input: &[BFieldElement],
    ) -> Result<VMState, InstructionError> {
        let code = program(Some(&snippet), arguments);
        let mut state = VMState::new(
            Program::new(&code),
            PublicInput::default(),
            nondeterminism(input),
        );
        state.run().map(|()| state)
    }

    fn output_words(state: &VMState, output: BFieldElement, len: usize) -> Vec<BFieldElement> {
        (0..len.div_ceil(4))
            .map(|i| state.ram[&(output + bfe!(i as u64))])
            .collect()
    }

    /// The dedicated snippet for `function` of an input of any length.
    fn dedicated(function: KeccakFunction, keccak: KeccakF1600) -> Box<dyn BasicSnippet> {
        match function {
            KeccakFunction::Sha3_256 => Box::new(Sha3_256::new(keccak)),
            KeccakFunction::Sha3_512 => Box::new(Sha3_512::new(keccak)),
            KeccakFunction::Shake128 => Box::new(Shake128::new(keccak)),
            KeccakFunction::Shake256 => Box::new(Shake256::new(keccak)),
        }
    }

    /// The dedicated snippet for `function` of an input of length `input_len`,
    /// with an output of length `output_len` if `function` is SHAKE.
    fn dedicated_static_size(
        function: KeccakFunction,
        keccak: KeccakF1600,
        input_len: usize,
        output_len: usize,
    ) -> Box<dyn BasicSnippet> {
        match function {
            KeccakFunction::Sha3_256 => Box::new(Sha3_256StaticSize::new(keccak, input_len)),
            KeccakFunction::Sha3_512 => Box::new(Sha3_512StaticSize::new(keccak, input_len)),
            KeccakFunction::Shake128 => {
                Box::new(Shake128StaticSize::new(keccak, input_len, output_len))
            }
            KeccakFunction::Shake256 => {
                Box::new(Shake256StaticSize::new(keccak, input_len, output_len))
            }
        }
    }

    fn hash_dynamic(
        function: KeccakFunction,
        input: &[u8],
        output_len: usize,
    ) -> Vec<BFieldElement> {
        let mut arguments = vec![INPUT_ADDRESS, input.len() as u64];
        if function.digest_len().is_none() {
            arguments.push(output_len as u64);
        }
        let state = run(
            |keccak| dedicated(function, keccak),
            &arguments,
            &to_words(input),
        )
        .unwrap();
        let stack = &state.op_stack.stack;
        let [output, len] = [stack[stack.len() - 2], stack[stack.len() - 1]];
        assert_eq!(bfe!(output_len as u64), len);
        output_words(&state, output, output_len)
    }

    fn hash_static(
        function: KeccakFunction,
        input: &[u8],
        output_len: usize,
    ) -> Vec<BFieldElement> {
        let input_len = input.len();
        let state = run(
            |keccak| dedicated_static_size(function, keccak, input_len, output_len),
            &[INPUT_ADDRESS],
            &to_words(input),
        )
        .unwrap();
        let output = *state.op_stack.stack.last().unwrap();
        output_words(&state, output, output_len)
    }

    fn assert_both_comply_with_fips_202(function: KeccakFunction, input: &[u8], output_len: usize) {
        let expected = to_words(&fips_202(function, input, output_len));
        let n = input.len();
        assert_eq!(
            expected,
            hash_dynamic(function, input, output_len),
            "{function:?} {n} {output_len}"
        );
        assert_eq!(
            expected,
            hash_static(function, input, output_len),
            "{function:?} {n} {output_len}"
        );
    }

    fn output_len(function: KeccakFunction, shake_len: usize) -> usize {
        function.digest_len().unwrap_or(shake_len)
    }

    #[macro_rules_attr::apply(proptest(cases = 8))]
    fn hash_complies_with_fips_202(
        #[strategy(0usize..4)] function: usize,
        #[strategy(vec(arb(), 0..600))] input: Vec<u8>,
        #[strategy(0usize..400)] shake_len: usize,
    ) {
        let function = KeccakFunction::ALL[function];
        assert_both_comply_with_fips_202(function, &input, output_len(function, shake_len));
    }

    /// Every position in a lane, and so in a word, at which the input or the
    /// output can end, and the lengths next to the end of one block or two.
    /// The padding goes to byte `n mod 8` of its lane, and the snippets place
    /// it according to `n mod 4` or `n mod 8`. The block loops run twice from
    /// `2·rate + 1` bytes on.
    #[macro_rules_attr::apply(test)]
    fn hash_complies_with_fips_202_at_boundaries() {
        for function in KeccakFunction::ALL {
            let rate = function.rate();
            let lengths = [
                0,
                1,
                2,
                3,
                4,
                5,
                6,
                7,
                8,
                9,
                rate - 1,
                rate,
                rate + 1,
                2 * rate,
                2 * rate + 1,
            ];
            for n in lengths {
                let input = (0..n).map(|i| (i * 37 + 11) as u8).collect::<Vec<_>>();
                assert_both_comply_with_fips_202(function, &input, output_len(function, 32));
            }
            if function.digest_len().is_none() {
                for d in lengths {
                    assert_both_comply_with_fips_202(function, b"abc", d);
                }
            }
        }
    }

    #[macro_rules_attr::apply(test)]
    fn refuse_nonzero_bytes_past_the_input() {
        let function = KeccakFunction::Sha3_256;
        for (input_len, word, value) in [(3, 0, 1 << 24), (6, 1, 1 << 16)] {
            let mut input = to_words(&vec![7; input_len]);
            input[word] += bfe!(value);
            let assert_fails_with = |result: Result<VMState, InstructionError>, id| {
                let Err(InstructionError::AssertionFailed(err)) = result else {
                    panic!("expected a failed assertion");
                };
                assert_eq!(Some(id), err.id);
            };
            let dynamic = run(
                |keccak| Box::new(KeccakHash::new(function, keccak)),
                &[INPUT_ADDRESS, input_len as u64],
                &input,
            );
            assert_fails_with(dynamic, KeccakHash::NONZERO_PADDING_ERROR_ID);
            let fixed = run(
                |keccak| Box::new(KeccakHashStaticSize::new(function, keccak, input_len, 32)),
                &[INPUT_ADDRESS],
                &input,
            );
            assert_fails_with(fixed, KeccakHashStaticSize::NONZERO_PADDING_ERROR_ID);
        }
    }

    /// The cost of the call alone, without the check of the tables.
    fn bench(
        snippet: &dyn Fn(KeccakF1600) -> Box<dyn BasicSnippet>,
        arguments: &[u64],
        input: &[u8],
    ) -> crate::snippet_bencher::BenchmarkResult {
        let nondeterminism = nondeterminism(&to_words(input));
        let stack = crate::empty_stack();
        let base = execute_bench(
            &program(None, arguments),
            &stack,
            vec![],
            nondeterminism.clone(),
            None,
        );
        let full = execute_bench(
            &program(Some(snippet), arguments),
            &stack,
            vec![],
            nondeterminism,
            None,
        );
        full - base
    }

    /// Every dedicated snippet, on a 32-byte input as the common case and a
    /// 1000-byte input as the "worst case". The cost grows with the input, and
    /// for SHAKE with the output, without bound: the second case is a larger
    /// sample, not a bound. SHAKE's output is as long as its input.
    #[macro_rules_attr::apply(test)]
    fn benchmark() {
        let cases = [
            (BenchmarkCase::CommonCase, 32),
            (BenchmarkCase::WorstCase, 1000),
        ];
        for function in KeccakFunction::ALL {
            let mut dynamic = vec![];
            let mut fixed = vec![];
            for (case, n) in cases {
                let input = (0..n).map(|i| (i * 37 + 11) as u8).collect::<Vec<_>>();
                let output_len = output_len(function, n);
                let mut arguments = vec![INPUT_ADDRESS, n as u64];
                if function.digest_len().is_none() {
                    arguments.push(output_len as u64);
                }
                dynamic.push(NamedBenchmarkResult {
                    name: format!("tasmlib_hashing_keccak_{}", function.name()),
                    benchmark_result: bench(
                        &|keccak| dedicated(function, keccak),
                        &arguments,
                        &input,
                    ),
                    case,
                });
                fixed.push(NamedBenchmarkResult {
                    name: format!("tasmlib_hashing_keccak_{}_static_size", function.name()),
                    benchmark_result: bench(
                        &|keccak| dedicated_static_size(function, keccak, n, output_len),
                        &[INPUT_ADDRESS],
                        &input,
                    ),
                    case,
                });
            }
            write_benchmarks(dynamic);
            write_benchmarks(fixed);
        }
    }
}
