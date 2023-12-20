use std::collections::HashMap;

use num_traits::Zero;
use rand::{random, thread_rng, Rng, RngCore};
use triton_vm::BFieldElement;

use crate::empty_stack;
use crate::traits::deprecated_snippet::DeprecatedSnippet;

pub struct MemCpy; // TODO: add field `static_length : Option<usize>` to avoid loop
impl MemCpy {
    fn random_input_state(len: usize) -> crate::ExecutionState {
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
        crate::ExecutionState::with_stack_and_memory(stack, memory, 0)
    }
}

impl DeprecatedSnippet for MemCpy {
    fn entrypoint_name(&self) -> String {
        "tasm_memory_memcpy".to_string()
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

    fn output_types(&self) -> Vec<crate::data_type::DataType> {
        vec![]
    }

    fn output_field_names(&self) -> Vec<String> {
        vec![]
    }

    fn stack_diff(&self) -> isize {
        -3
    }

    fn function_code(&self, _library: &mut crate::library::Library) -> String {
        let entrypoint = self.entrypoint_name();
        format!(
            "
        // BEFORE: _ read_source write_dest num_words
        // AFTER: _
        {entrypoint}:

            swap 2
            push 4
            add
            swap 2
            // _ (read_source + 4) write_dest num_words

            call {entrypoint}_loop_cpy5_words
            // _ read_source write_dest remaining_words

            dup 0
            push 4
            eq
            // _ read_source write_dest remaining_words (remaining_words == 4)
            skiz
                call {entrypoint}_cpy4_words
            // _ read_source write_dest remaining_words

            dup 0
            push 3
            eq
            skiz
                call {entrypoint}_cpy3_words
            // _ read_source write_dest remaining_words

            dup 0
            push 2
            eq
            skiz
                call {entrypoint}_cpy2_words
            // _ read_source write_dest remaining_words

            dup 0
            push 1
            eq
            skiz
                call {entrypoint}_cpy1_word
            // read_source write_dest 0

            // clean up stack
            pop 3

            return

        // INVARIANT:  _ (read_source + 4) write_dest remaining_words
        {entrypoint}_loop_cpy5_words:
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

            push -5 add // _ (read_source + 9) (write_dest + 5) (remaining_words-5)

            recurse

        {entrypoint}_cpy4_words:
            // (read_source + 4) write_dest remaining_words

            pop 1
            // (read_source + 4) write_dest

            // read
            swap 1
            // write_dest (read_source + 4)

            push -1 add
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

        {entrypoint}_cpy3_words:
            // (read_source + 4) write_dest remaining_words

            pop 1
            // (read_source + 4) write_dest

            // read
            swap 1
            // write_dest (read_source + 4)

            push -2 add
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

        {entrypoint}_cpy2_words:
            // (read_source + 4) write_dest remaining_words

            pop 1
            // (read_source + 4) write_dest

            // read
            swap 1
            // write_dest (read_source + 4)

            push -3 add
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

        {entrypoint}_cpy1_word:
            // (read_source + 4) write_dest remaining_words

            pop 1
            // (read_source + 4) write_dest

            // read
            swap 1
            // write_dest (read_source + 4)

            push -4 add
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
        "
        )
    }

    fn crash_conditions(&self) -> Vec<String> {
        vec![]
    }

    fn gen_input_states(&self) -> Vec<crate::ExecutionState> {
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

    fn common_case_input_state(&self) -> crate::ExecutionState {
        Self::random_input_state(17)
    }

    fn worst_case_input_state(&self) -> crate::ExecutionState {
        Self::random_input_state(1000)
    }

    fn rust_shadowing(
        &self,
        stack: &mut Vec<triton_vm::BFieldElement>,
        _std_in: Vec<triton_vm::BFieldElement>,
        _secret_in: Vec<triton_vm::BFieldElement>,
        memory: &mut std::collections::HashMap<triton_vm::BFieldElement, triton_vm::BFieldElement>,
    ) {
        let len = stack.pop().unwrap().value() as usize;
        let write_dest = stack.pop().unwrap();
        let read_source = stack.pop().unwrap();

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
    use crate::test_helpers::test_rust_equivalence_multiple_deprecated;

    use super::*;

    #[test]
    fn memcpy_test() {
        test_rust_equivalence_multiple_deprecated(&MemCpy, true);
    }
}

#[cfg(test)]
mod benches {
    use super::*;
    use crate::snippet_bencher::bench_and_write;

    #[test]
    fn memcpy_benchmark() {
        bench_and_write(MemCpy);
    }
}
