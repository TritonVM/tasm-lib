use std::collections::HashMap;

use num_traits::Zero;
use rand::{random, thread_rng, Rng, RngCore};
use triton_vm::{BFieldElement, NonDeterminism};

use crate::{get_init_tvm_stack, snippet::DeprecatedSnippet};

pub struct MemCpy; // TODO: add field `static_length : Option<usize>` to avoid loop
impl MemCpy {
    fn random_input_state(len: usize) -> crate::ExecutionState {
        let mut rng = thread_rng();
        let read_source = random::<BFieldElement>();
        let write_dest = BFieldElement::new(read_source.value() + rng.next_u32() as u64);

        // set stack
        let mut stack = get_init_tvm_stack();
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
        crate::ExecutionState {
            stack,
            std_in: vec![],
            nondeterminism: NonDeterminism::new(vec![]),
            memory,
            words_allocated: 0,
        }
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

    fn input_types(&self) -> Vec<crate::snippet::DataType> {
        vec![
            crate::snippet::DataType::VoidPointer,
            crate::snippet::DataType::VoidPointer,
            crate::snippet::DataType::U32,
        ]
    }

    fn output_types(&self) -> Vec<crate::snippet::DataType> {
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

            call {entrypoint}_loop // read_source write_dest 0

            // clean up stack
            pop pop pop
            return

        // INVARIANT:  _ read_source write_dest remaining_words
        {entrypoint}_loop:
            // termination condition
            dup 0 push 0 eq // _ read_source write_dest remaining_words remaining_words==0
            skiz return

            // read
            swap 2 // _ remaining_words write_dest read_source
            read_mem // _ remaining_words write_dest read_source value

            // write
            swap 1 // _ remaining_words write_dest value read_source
            push 1 add // _ remaining_words write_dest value read_source+1
            swap 3 // _ read_source+1 write_dest value remaining_words
            swap 2 // _ read_source+1 remaining_words value write_dest
            swap 1 //  _ read_source+1 remaining_words write_dest value
            write_mem //  _ read_source+1 remaining_words write_dest

            push 1 add // _ read_source+1 remaining_words write_dest+1
            swap 1 // _ read_source+1 write_dest+1 remaining_words

            push -1 add // _ read_source+1 write_dest+1 remaining_words-1

            recurse
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
            Self::random_input_state(rng.gen_range(2..20)),
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
