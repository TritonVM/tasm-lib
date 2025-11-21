use triton_vm::prelude::*;

use crate::prelude::*;
use crate::structure::tasm_object::DEFAULT_MAX_DYN_FIELD_SIZE;

#[derive(Debug, Default, Copy, Clone, Eq, PartialEq, Hash)]
pub struct MemCpy; // TODO: add field `static_length : Option<usize>` to avoid loop

impl MemCpy {
    pub const EXCEEDS_MAX_COPY_SIZE_ERROR_ID: i128 = 60;
}

impl BasicSnippet for MemCpy {
    fn parameters(&self) -> Vec<(DataType, String)> {
        vec![
            (DataType::VoidPointer, "read_source".to_string()),
            (DataType::VoidPointer, "write_dest".to_string()),
            (DataType::U32, "num_words".to_string()),
        ]
    }

    fn return_values(&self) -> Vec<(DataType, String)> {
        vec![]
    }

    fn entrypoint(&self) -> String {
        "tasmlib_memory_memcpy".to_string()
    }

    fn code(&self, _: &mut Library) -> Vec<LabelledInstruction> {
        let entrypoint = self.entrypoint();
        let copy_5_words_loop_label = format!("{entrypoint}_loop_copy_5_words");
        let copy_single_words_loop_label = format!("{entrypoint}_loop_copy_single_words");
        triton_asm!(
        // BEFORE: _ read_source write_dest num_words
        // AFTER:  _
        {entrypoint}:
            /* Cap size of memcpy operation */
            push {DEFAULT_MAX_DYN_FIELD_SIZE}
            dup 1
            lt
            // _ read_source write_dest (num_words < MAX)

            assert error_id {Self::EXCEEDS_MAX_COPY_SIZE_ERROR_ID}
            // _ read_source write_dest

            pick 2
            addi 4
            place 2
            // _ (read_source + 4) write_dest num_words

            call {copy_5_words_loop_label}
            // _ (read_source + 4) write_dest remaining_words

            pick 2
            addi -4
            place 2
            call {copy_single_words_loop_label}

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
            pick 2      // _ write_dest remaining_words (read_source + 4)
            read_mem 5  // _ write_dest remaining_words [val4 val3 val2 val1 val0] (read_source - 1)
            addi 10     // _ write_dest remaining_words [val4 val3 val2 val1 val0] (read_source + 9)
            place 7     // _ (read_source + 9) write_dest remaining_words [val4 val3 val2 val1 val0]

            // write
            pick 6      // _ (read_source + 9) remaining_words [val4 val3 val2 val1 val0] write_dest
            write_mem 5 // _ (read_source + 9) remaining_words (write_dest + 5)
            place 1     // _ (read_source + 9) (write_dest + 5) remaining_words

            addi -5     // _ (read_source + 9) (write_dest + 5) (remaining_words-5)
            recurse

        // BEFORE: _ read_source       write_dest       n
        // AFTER:  _ (read_source + n) (write_dest + n) 0
        {copy_single_words_loop_label}:
            dup 0 push 0 eq
            skiz return

            pick 2
            read_mem 1
            addi 2
            place 3

            pick 2
            write_mem 1
            place 1

            addi -1
            recurse
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_prelude::*;

    impl Function for MemCpy {
        fn rust_shadow(
            &self,
            stack: &mut Vec<BFieldElement>,
            memory: &mut HashMap<BFieldElement, BFieldElement>,
        ) {
            let len = pop_encodable(stack);
            let write_dest = stack.pop().unwrap();
            let read_source = stack.pop().unwrap();
            assert!(len < DEFAULT_MAX_DYN_FIELD_SIZE);

            for i in 0..len {
                let offset = bfe!(i);
                let maybe_element = memory.get(&(read_source + offset));
                let element = maybe_element.copied().unwrap_or_default();
                memory.insert(write_dest + offset, element);
            }
        }

        fn pseudorandom_initial_state(
            &self,
            seed: [u8; 32],
            bench_case: Option<BenchmarkCase>,
        ) -> FunctionInitialState {
            let mut rng = StdRng::from_seed(seed);
            let len = match bench_case {
                Some(BenchmarkCase::CommonCase) => 17,
                Some(BenchmarkCase::WorstCase) => 1000,
                None => rng.random_range(11..=200),
            };
            let read_source: BFieldElement = rng.random();
            let write_dest: BFieldElement = rng.random();

            let mut stack = self.init_stack_for_isolated_run();
            stack.extend(bfe_vec![read_source, write_dest, len]);

            let memory = (0..len)
                .map(|i| (read_source + bfe!(i), rng.random()))
                .collect();

            FunctionInitialState { stack, memory }
        }
    }

    #[test]
    fn rust_shadow() {
        ShadowedFunction::new(MemCpy).test();
    }

    #[proptest]
    fn exceeding_max_size_crashes_vm(#[strategy(DEFAULT_MAX_DYN_FIELD_SIZE..)] len: u32) {
        // actually filling memory with `len` random elements is a waste of time
        let mut stack = MemCpy.init_stack_for_isolated_run();
        stack.extend(bfe_vec![0, 0, len]);
        let initial_state = InitVmState::with_stack(stack);

        test_assertion_failure(
            &ShadowedFunction::new(MemCpy),
            initial_state,
            &[MemCpy::EXCEEDS_MAX_COPY_SIZE_ERROR_ID],
        );
    }
}

#[cfg(test)]
mod benches {
    use super::*;
    use crate::test_prelude::*;

    #[test]
    fn benchmark() {
        ShadowedFunction::new(MemCpy).bench();
    }
}
