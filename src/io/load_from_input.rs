use rand::Rng;
use triton_opcodes::shortcuts::{divine, read_io};
use triton_vm::BFieldElement;
use twenty_first::shared_math::other::random_elements;

use crate::{
    dyn_malloc, get_init_tvm_stack,
    rust_shadowing_helper_functions::dyn_malloc::rust_dyn_malloc_initialize,
    snippet::{DataType, InputSource, Snippet},
    ExecutionState,
};

#[derive(Clone, Debug)]
pub struct LoadFromInput(pub InputSource);

/// Load a list of words from the input source into memory.
/// The first element of the input source is the length of the list
/// that is loaded into memory. Returns a pointer to the first element
/// in memory.
impl Snippet for LoadFromInput {
    fn entrypoint(&self) -> String {
        format!("tasm_io_load_from_input_{}", self.0)
    }

    fn inputs(&self) -> Vec<String> {
        vec![]
    }

    fn input_types(&self) -> Vec<crate::snippet::DataType> {
        vec![]
    }

    fn output_types(&self) -> Vec<crate::snippet::DataType> {
        vec![DataType::BFE]
    }

    fn outputs(&self) -> Vec<String> {
        vec!["*addr".to_string()]
    }

    fn stack_diff(&self) -> isize {
        1
    }

    fn function_code(&self, library: &mut crate::snippet_state::SnippetState) -> String {
        let entrypoint = self.entrypoint();

        let dyn_alloc = library.import(Box::new(dyn_malloc::DynMalloc));

        let read_instruction = match self.0 {
            InputSource::StdIn => read_io(),
            InputSource::SecretIn => divine(),
        };

        format!(
            "
            // BEFORE: _
            // AFTER: _ *addr
            {entrypoint}:
                {read_instruction}
                // _ length

                // allocate memory for the input, including its own length indicator
                dup 0
                push 1
                add
                call {dyn_alloc}
                // _ length *addr

                // store the length indicator in the first element of dedicated memory
                dup 1
                write_mem
                // _ length *addr

                push 1
                add
                // _ length (*addr + 1)

                // set element counter i = 0
                push 0
                // _ length (*addr + 1) i
                call {entrypoint}_loop

                // _ length (*addr + 1) i
                pop
                push -1
                add
                // _ length *addr

                swap 1 pop
                // _ *addr

                return

                // TODO: You could probably calculate the end address here,
                // and use that in the loop termination condition instead of
                // keeping track of two variables, length and i.

                // START and END of loop: _ length (*addr + 1) i
                {entrypoint}_loop:
                    // check while-loop condition
                    dup 0
                    dup 3
                    eq

                    // _ length (*addr + 1) i (i == length)
                    skiz
                        return

                    // _ length (*addr + 1) i

                    dup 1
                    dup 1
                    add
                    // _ length (*addr + 1) i (*addr + 1 + i)

                    {read_instruction}
                    // _ length (*addr + 1) i (*addr + 1 + i) value_from_input

                    write_mem
                    // _ length (*addr + 1) i (*addr + 1 + i)

                    pop
                    // _ length (*addr + 1) i

                    push 1
                    add
                    // _ length (*addr + 1) (i + 1)

                    recurse
                "
        )
    }

    fn crash_conditions(&self) -> Vec<String> {
        vec![
            "size exceeds 2^32".to_owned(),
            "allocated memory exceeds 2^32".to_owned(),
            "input is shorter than indicated length".to_owned(),
            "input is empty".to_owned(),
        ]
    }

    fn gen_input_states(&self) -> Vec<crate::ExecutionState> {
        let mut ret = vec![];
        let mut thread_rng = rand::thread_rng();
        for _ in 0..10 {
            let length = thread_rng.gen_range(0..(1 << 10)) as u64;
            let input: Vec<BFieldElement> = vec![
                vec![BFieldElement::new(length)],
                random_elements(length as usize),
            ]
            .concat();
            ret.push(match self.0 {
                InputSource::StdIn => ExecutionState {
                    stack: get_init_tvm_stack(),
                    memory: std::collections::HashMap::new(),
                    std_in: input,
                    secret_in: vec![],
                    words_allocated: 0,
                },
                InputSource::SecretIn => ExecutionState {
                    stack: get_init_tvm_stack(),
                    memory: std::collections::HashMap::new(),
                    std_in: vec![],
                    secret_in: input,
                    words_allocated: 0,
                },
            });
        }

        ret
    }

    fn common_case_input_state(&self) -> crate::ExecutionState {
        let length = 1u64 << 9;
        let input: Vec<BFieldElement> = vec![
            vec![BFieldElement::new(length)],
            random_elements(length as usize),
        ]
        .concat();
        match self.0 {
            InputSource::StdIn => ExecutionState {
                stack: get_init_tvm_stack(),
                memory: std::collections::HashMap::new(),
                std_in: input,
                secret_in: vec![],
                words_allocated: 0,
            },
            InputSource::SecretIn => ExecutionState {
                stack: get_init_tvm_stack(),
                memory: std::collections::HashMap::new(),
                std_in: vec![],
                secret_in: input,
                words_allocated: 0,
            },
        }
    }

    fn worst_case_input_state(&self) -> crate::ExecutionState {
        let length = 1u64 << 13;
        let input: Vec<BFieldElement> = vec![
            vec![BFieldElement::new(length)],
            random_elements(length as usize),
        ]
        .concat();
        match self.0 {
            InputSource::StdIn => ExecutionState {
                stack: get_init_tvm_stack(),
                memory: std::collections::HashMap::new(),
                std_in: input,
                secret_in: vec![],
                words_allocated: 0,
            },
            InputSource::SecretIn => ExecutionState {
                stack: get_init_tvm_stack(),
                memory: std::collections::HashMap::new(),
                std_in: vec![],
                secret_in: input,
                words_allocated: 0,
            },
        }
    }

    fn rust_shadowing(
        &self,
        stack: &mut Vec<triton_vm::BFieldElement>,
        std_in: Vec<triton_vm::BFieldElement>,
        secret_in: Vec<triton_vm::BFieldElement>,
        memory: &mut std::collections::HashMap<triton_vm::BFieldElement, triton_vm::BFieldElement>,
    ) {
        // BEFORE: _
        // AFTER: _ *addr
        let input = match self.0 {
            InputSource::StdIn => std_in,
            InputSource::SecretIn => secret_in,
        };

        rust_dyn_malloc_initialize(memory, 1);

        let indicated_length: usize = input[0].value() as usize;
        memory.insert(
            BFieldElement::new(1),
            BFieldElement::new(indicated_length as u64),
        );

        for i in 0..indicated_length {
            let value_from_input = input[i + 1];
            let addr = BFieldElement::new(i as u64 + 2);
            memory.insert(addr, value_from_input);
        }

        stack.push(BFieldElement::new(1));
    }
}

#[cfg(test)]
mod tests {
    use crate::{execute_with_execution_state, test_helpers::test_rust_equivalence_multiple};

    use super::*;

    #[test]
    fn new_snippet_test() {
        test_rust_equivalence_multiple(&LoadFromInput(InputSource::SecretIn), true);
        test_rust_equivalence_multiple(&LoadFromInput(InputSource::StdIn), true);
    }

    #[test]
    fn verify_dyn_malloc_shows_correct_next_value() {
        for length in 0..10 {
            let state = ExecutionState {
                stack: get_init_tvm_stack(),
                memory: std::collections::HashMap::new(),
                std_in: vec![
                    vec![BFieldElement::new(length)],
                    random_elements(length as usize),
                ]
                .concat(),
                secret_in: vec![],
                words_allocated: 0,
            };
            let snippet = LoadFromInput(InputSource::StdIn);
            let stack_diff = snippet.stack_diff();
            let res = execute_with_execution_state(state, Box::new(snippet), stack_diff).unwrap();

            // Verify final state of dyn malloc. dyn malloc should be set to the next available
            // memory address.
            // Expected memory layout after running above program:
            // 0: dynamic allocator value
            // 1: indicated length
            // 2..N: elements of input, after indicated length
            // N+1: next available memory address
            let indicated_next_free_address =
                res.final_ram[&BFieldElement::new(dyn_malloc::DYN_MALLOC_ADDRESS as u64)].value();
            assert_eq!(length + 2, indicated_next_free_address);
            assert!(res
                .final_ram
                .get(&BFieldElement::new(indicated_next_free_address))
                .is_none());
        }
    }
}

#[cfg(test)]
mod benches {
    use super::*;
    use crate::snippet_bencher::bench_and_write;

    #[test]
    fn load_from_input_benchmark() {
        bench_and_write(LoadFromInput(InputSource::SecretIn));
        bench_and_write(LoadFromInput(InputSource::StdIn));
    }
}
