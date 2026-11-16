//! SHA3-256, SHA3-512, SHAKE128 and SHAKE256 on byte strings in memory.
//!
//! A byte string of length `n` is held as `⌈n/4⌉` u32 words, little-endian:
//! byte `i` is byte `i mod 4` of word `⌊i/4⌋`. The bytes of the last word from
//! `n` on are zero.

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
/// The lengths count bytes, and the strings are u32 words (see
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
pub struct KeccakHash {
    function: KeccakFunction,
    keccak: KeccakF1600,
}

impl KeccakHash {
    pub const NONZERO_PADDING_ERROR_ID: i128 = 641;

    pub fn new(function: KeccakFunction, keccak: KeccakF1600) -> Self {
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
/// The strings are u32 words (see [the module](self)). The output lies in
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
pub struct KeccakHashStaticSize {
    function: KeccakFunction,
    keccak: KeccakF1600,
    input_len: usize,
    output_len: usize,
}

impl KeccakHashStaticSize {
    pub const NONZERO_PADDING_ERROR_ID: i128 = 642;

    /// # Panics
    ///
    /// Panics if `function` is SHA3 and `output_len` is not its digest length.
    pub fn new(
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

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use proptest::collection::vec;

    use super::super::tables::CheckKeccakTables;
    use super::super::tables::insert_tables;
    use super::*;
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

    /// Runs a program that checks the tables, pushes `arguments`, and calls
    /// the snippet that `snippet` makes, with `input` at [`INPUT_ADDRESS`].
    fn run(
        snippet: impl FnOnce(KeccakF1600) -> Box<dyn BasicSnippet>,
        arguments: &[u64],
        input: &[BFieldElement],
    ) -> Result<VMState, InstructionError> {
        let mut library = Library::new();
        let keccak = KeccakF1600::allocate(&mut library);
        let check = library.import(Box::new(CheckKeccakTables));
        let hash = library.import(snippet(keccak));
        let push_arguments = arguments
            .iter()
            .flat_map(|&a| triton_asm!(push { a }))
            .collect::<Vec<_>>();
        let code = triton_asm!(
            call {check}
            {&push_arguments}
            call {hash}
            halt
            {&library.all_imports()}
        );
        let mut memory = HashMap::new();
        insert_tables(&mut memory);
        for (i, &word) in input.iter().enumerate() {
            memory.insert(bfe!(INPUT_ADDRESS + i as u64), word);
        }
        let nondeterminism = NonDeterminism::default().with_ram(memory);
        let mut state = VMState::new(Program::new(&code), PublicInput::default(), nondeterminism);
        state.run().map(|()| state)
    }

    fn output_words(state: &VMState, output: BFieldElement, len: usize) -> Vec<BFieldElement> {
        (0..len.div_ceil(4))
            .map(|i| state.ram[&(output + bfe!(i as u64))])
            .collect()
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
            |keccak| Box::new(KeccakHash::new(function, keccak)),
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
            |keccak| {
                Box::new(KeccakHashStaticSize::new(
                    function, keccak, input_len, output_len,
                ))
            },
            &[INPUT_ADDRESS],
            &to_words(input),
        )
        .unwrap();
        let output = *state.op_stack.stack.last().unwrap();
        output_words(&state, output, output_len)
    }

    fn assert_both_hash_like_the_reference(
        function: KeccakFunction,
        input: &[u8],
        output_len: usize,
    ) {
        let expected = to_words(&function.hash(input, output_len));
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
    fn hash_like_the_reference(
        #[strategy(0usize..4)] function: usize,
        #[strategy(vec(arb(), 0..400))] input: Vec<u8>,
        #[strategy(0usize..400)] shake_len: usize,
    ) {
        let function = KeccakFunction::ALL[function];
        assert_both_hash_like_the_reference(function, &input, output_len(function, shake_len));
    }

    /// Every length next to a boundary of a word, a lane or a block.
    #[macro_rules_attr::apply(test)]
    fn hash_like_the_reference_at_boundaries() {
        for function in KeccakFunction::ALL {
            let rate = function.rate();
            let lengths = [0, 1, 3, 4, 5, 7, 8, 9, rate - 1, rate, rate + 1, 2 * rate];
            for n in lengths {
                let input = (0..n).map(|i| (i * 37 + 11) as u8).collect::<Vec<_>>();
                assert_both_hash_like_the_reference(function, &input, output_len(function, 32));
            }
            if function.digest_len().is_none() {
                for d in lengths {
                    assert_both_hash_like_the_reference(function, b"abc", d);
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
}
