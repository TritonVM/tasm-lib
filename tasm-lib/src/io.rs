use std::fmt::Display;

use triton_vm::prelude::*;

pub mod read_input;
pub mod write_to_stdout;

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub enum InputSource {
    StdIn,
    SecretIn,
}

impl Display for InputSource {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let str = match self {
            InputSource::StdIn => "stdin",
            InputSource::SecretIn => "secin",
        };

        write!(f, "{str}")
    }
}

/// Return the code to write `n` words to output
/// ```text
/// BEFORE: _ [words; n]
/// AFTER:  _
/// ```
pub fn write_words(n: usize) -> Vec<LabelledInstruction> {
    let num_full_chunk_writes = n / 5;
    let num_remaining_words = n % 5;
    let mut instructions = vec![triton_instr!(write_io 5); num_full_chunk_writes];
    if num_remaining_words > 0 {
        instructions.extend(triton_asm!(write_io {
            num_remaining_words
        }));
    }

    instructions
}

impl InputSource {
    /// The name of the instruction that reads from this input source
    pub const fn instruction_name(&self) -> &str {
        match self {
            InputSource::StdIn => "read_io",
            InputSource::SecretIn => "divine",
        }
    }

    /// Return a string identifiying the input source and usable as assembly label
    pub fn label_friendly_name(&self) -> &str {
        match self {
            InputSource::StdIn => "stdin",
            InputSource::SecretIn => "secin",
        }
    }

    /// Return the code used to read `n` words from the input source.
    /// ```text
    /// BEFORE: _
    /// AFTER:  _ [read_words; n]
    /// ```
    pub fn read_words(&self, n: usize) -> Vec<LabelledInstruction> {
        let input_instruction = self.instruction_name();

        let num_full_chunk_reads = n / 5;
        let num_remaining_words = n % 5;
        let mut instructions =
            vec![triton_asm!({input_instruction} 5); num_full_chunk_reads].concat();
        if num_remaining_words > 0 {
            instructions.extend(triton_asm!({input_instruction} {num_remaining_words}));
        }

        instructions
    }
}
