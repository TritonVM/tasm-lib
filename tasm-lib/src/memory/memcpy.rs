use std::collections::HashMap;

use itertools::Itertools;
use num_traits::Zero;
use rand::prelude::*;
use triton_vm::prelude::*;

use crate::empty_stack;
use crate::structure::tasm_object::DEFAULT_MAX_DYN_FIELD_SIZE;
use crate::traits::deprecated_snippet::DeprecatedSnippet;

#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
pub struct MemCpy; // TODO: add field `static_length : Option<usize>` to avoid loop

impl MemCpy {
    fn random_input_state(len: usize) -> crate::InitVmState {
        let mut rng = thread_rng();
        let read_source = random::<BFieldElement>();
        let write_dest = BFieldElement::new(read_source.value() + rng.next_u32() as u64);

        // set stack
        let mut stack = empty_stack();
        stack.push(read_source);
        stack.push(write_dest);
        stack.push(BFieldElement::new(len as u64));

        // set memory
        let mut memory = HashMap::new();
        for i in 0..len {
            memory.insert(
                BFieldElement::new(i as u64) + read_source,
                random::<BFieldElement>(),
            );
        }
        crate::InitVmState::with_stack_and_memory(stack, memory)
    }
}

impl DeprecatedSnippet for MemCpy {
    fn entrypoint_name(&self) -> String {
        "tasmlib_memory_memcpy".to_string()
    }

    fn input_field_names(&self) -> Vec<String> {
        vec![
            "read_source".to_string(),
            "write_dest".to_string(),
            "num_words".to_string(),
        ]
    }

    fn input_types(&self) -> Vec<crate::data_type::DataType> {
        vec![
            crate::data_type::DataType::VoidPointer,
            crate::data_type::DataType::VoidPointer,
            crate::data_type::DataType::U32,
        ]
    }

    fn output_field_names(&self) -> Vec<String> {
        vec![]
    }

    fn output_types(&self) -> Vec<crate::data_type::DataType> {
        vec![]
    }

    fn stack_diff(&self) -> isize {
        -3
    }

    fn function_code(&self, _library: &mut crate::library::Library) -> String {
        let entrypoint = self.entrypoint_name();

        let copy_5_words_loop_label = format!("{entrypoint}_loop_cpy5_words");
        let copy_4_words_label = format!("{entrypoint}_cpy4_words");
        let copy_3_words_label = format!("{entrypoint}_cpy3_words");
        let copy_2_words_label = format!("{entrypoint}_cpy2_words");
        let copy_1_words_label = format!("{entrypoint}_cpy1_words");
        triton_asm!(
        // BEFORE: _ read_source write_dest num_words
        // AFTER:  _
        {entrypoint}:

            /* Cap size of memcpy operation */
            push {DEFAULT_MAX_DYN_FIELD_SIZE}
            dup 1
            lt
            // _ read_source write_dest (num_words < MAX)

            assert
            // _ read_source write_dest


            swap 2
            push 4
            add
            swap 2
            // _ (read_source + 4) write_dest num_words

            call {copy_5_words_loop_label}
            // _ read_source write_dest remaining_words

            dup 0
            push 4
            eq
            // _ read_source write_dest remaining_words (remaining_words == 4)
            skiz
                call {copy_4_words_label}
            // _ read_source write_dest remaining_words

            dup 0
            push 3
            eq
            skiz
                call {copy_3_words_label}
            // _ read_source write_dest remaining_words

            dup 0
            push 2
            eq
            skiz
                call {copy_2_words_label}
            // _ read_source write_dest remaining_words

            dup 0
            push 1
            eq
            skiz
                call {copy_1_words_label}
            // read_source write_dest 0

            // clean up stack
            pop 3

            return

        // INVARIANT:  _ (read_source + 4) write_dest remaining_words
        {copy_5_words_loop_label}:
            // termination condition
            push 5
            dup 1
            lt
            // _ (read_source + 4) write_dest remaining_words (5 > remaining_words)

            skiz return
            // _ (read_source + 4) write_dest remaining_words

            // read
            swap 2     // _ remaining_words write_dest (read_source + 4)
            read_mem 5 // _ remaining_words write_dest [val4 val3 val2 val1 val0] (read_source - 1)
            push 10 add // _ remaining_words write_dest [val4 val3 val2 val1 val0] (read_source + 9)

            // write
            swap 7 // _ (read_source + 9) write_dest [val4 val3 val2 val1 val0] remaining_words
            swap 6 // _ (read_source + 9) remaining_words [val4 val3 val2 val1 val0] write_dest
            write_mem 5 //  _ (read_source + 9) remaining_words (write_dest + 5)

            swap 1 // _ (read_source + 9) (write_dest + 5) remaining_words

            addi -5 // _ (read_source + 9) (write_dest + 5) (remaining_words-5)

            recurse

        {copy_4_words_label}:
            // (read_source + 4) write_dest remaining_words

            pop 1
            // (read_source + 4) write_dest

            // read
            swap 1
            // write_dest (read_source + 4)

            addi -1
            // write_dest (read_source + 3)

            read_mem 4
            // write_dest [values] (read_source - 1)

            swap 5
            // (read_source - 1) [values] write_dest

            write_mem 4
            // (read_source - 1) (write_dest + 4)

            push 0
            // read_source' write_dest' remaining_words

            return

        {copy_3_words_label}:
            // (read_source + 4) write_dest remaining_words

            pop 1
            // (read_source + 4) write_dest

            // read
            swap 1
            // write_dest (read_source + 4)

            addi -2
            // write_dest (read_source + 2)

            read_mem 3
            // write_dest [values] (read_source - 1)

            swap 4
            // (read_source - 1) [values] write_dest

            write_mem 3
            // (read_source - 1) (write_dest + 3)

            push 0
            // read_source' write_dest' remaining_words

            return

        {copy_2_words_label}:
            // (read_source + 4) write_dest remaining_words

            pop 1
            // (read_source + 4) write_dest

            // read
            swap 1
            // write_dest (read_source + 4)

            addi -3
            // write_dest (read_source + 1)

            read_mem 2
            // write_dest [values] (read_source - 1)

            swap 3
            // (read_source - 1) [values] write_dest

            write_mem 2
            // (read_source - 1) (write_dest + 4)

            push 0
            // read_source' write_dest' remaining_words

            return

        {copy_1_words_label}:
            // (read_source + 4) write_dest remaining_words

            pop 1
            // (read_source + 4) write_dest

            // read
            swap 1
            // write_dest (read_source + 4)

            addi -4
            // write_dest (read_source)

            read_mem 1
            // write_dest value (read_source - 1)

            swap 2
            // (read_source - 1) value write_dest

            write_mem 1
            // (read_source - 1) (write_dest + 4)

            push 0
            // read_source' write_dest' remaining_words

            return
        )
        .iter()
        .join("\n")
    }

    fn crash_conditions(&self) -> Vec<String> {
        vec![format!(
            "Attempts to copy more than {} words",
            DEFAULT_MAX_DYN_FIELD_SIZE
        )]
    }

    fn gen_input_states(&self) -> Vec<crate::InitVmState> {
        let mut rng = thread_rng();
        vec![
            Self::random_input_state(1),
            Self::random_input_state(0),
            Self::random_input_state(4),
            Self::random_input_state(2),
            Self::random_input_state(3),
            Self::random_input_state(5),
            Self::random_input_state(6),
            Self::random_input_state(7),
            Self::random_input_state(8),
            Self::random_input_state(9),
            Self::random_input_state(10),
            Self::random_input_state(104),
            Self::random_input_state(rng.gen_range(11..200)),
            Self::random_input_state(rng.gen_range(11..200)),
        ]
    }

    fn common_case_input_state(&self) -> crate::InitVmState {
        Self::random_input_state(17)
    }

    fn worst_case_input_state(&self) -> crate::InitVmState {
        Self::random_input_state(1000)
    }

    fn rust_shadowing(
        &self,
        stack: &mut Vec<BFieldElement>,
        _std_in: Vec<BFieldElement>,
        _secret_in: Vec<BFieldElement>,
        memory: &mut HashMap<BFieldElement, BFieldElement>,
    ) {
        let len = stack.pop().unwrap().value() as usize;
        let write_dest = stack.pop().unwrap();
        let read_source = stack.pop().unwrap();

        assert!(len < DEFAULT_MAX_DYN_FIELD_SIZE as usize);

        for i in 0..len {
            let read_element = memory
                .get(&(BFieldElement::new(i as u64) + read_source))
                .unwrap_or(&BFieldElement::zero())
                .to_owned();
            memory.insert(BFieldElement::new(i as u64) + write_dest, read_element);
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        prelude::BasicSnippet,
        test_helpers::{negative_test, test_rust_equivalence_multiple_deprecated},
        traits::deprecated_snippet::tests::DeprecatedSnippetWrapper,
    };

    use super::*;

    #[test]
    fn memcpy_test() {
        test_rust_equivalence_multiple_deprecated(&MemCpy, true);
    }

    #[test]
    fn exceed_max_memcpy_size_negative_test() {
        let snippet_struct = DeprecatedSnippetWrapper::new(MemCpy);
        let stack = [
            snippet_struct
                .deprecated_snippet
                .init_stack_for_isolated_run(),
            vec![bfe!(0), bfe!(0), bfe!(1u64 << 31)],
        ]
        .concat();
        let init_state = crate::InitVmState {
            stack,
            public_input: Vec::default(),
            nondeterminism: NonDeterminism::default(),
            sponge: None,
        };
        negative_test(
            &snippet_struct,
            init_state,
            &[InstructionError::AssertionFailed],
        );
    }
}

#[cfg(test)]
mod benches {
    use crate::snippet_bencher::bench_and_write;

    use super::*;

    #[test]
    fn memcpy_benchmark() {
        bench_and_write(MemCpy);
    }
}
