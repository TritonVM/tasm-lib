use std::collections::HashMap;

use rand::Rng;
use triton_vm::{BFieldElement, NonDeterminism};
use twenty_first::shared_math::{bfield_codec::BFieldCodec, other::random_elements};

use crate::{
    get_init_tvm_stack,
    io::load_from_input::LoadFromInput,
    snippet::{DataType, DeprecatedSnippet, InputSource},
    ExecutionState,
};

use super::load_from_input;

/// Load a given number of lists of words from the input source into memory.
/// The first element of each list is the length of the list
/// that is loaded into memory. Returns a pointer to the first element
/// in memory. This function is named `load_struct_from_input` because
/// structs are encoded as a sequence of length-prepended lists of words.
/// Note that the `field_count` input argument may never be set through
/// user input, rather it should be known statically.
/// This snippet assumes that the dynamic allocator will allocate all
/// fields contiguously in memory.
#[derive(Clone, Debug)]
pub struct LoadStructFromInput {
    pub input_source: InputSource,
}

#[derive(BFieldCodec)]
struct DummyStructSize1 {
    field_1: Vec<BFieldElement>,
}
#[derive(BFieldCodec)]
struct DummyStructSize2 {
    field_1: Vec<BFieldElement>,
    field_2: Vec<BFieldElement>,
}
#[derive(BFieldCodec)]
struct DummyStructSize3 {
    field_1: Vec<BFieldElement>,
    field_2: Vec<BFieldElement>,
    field_3: Vec<BFieldElement>,
}
#[derive(BFieldCodec)]
struct DummyStructSize5 {
    field_1: Vec<BFieldElement>,
    field_2: Vec<BFieldElement>,
    field_3: Vec<BFieldElement>,
    field_4: Vec<BFieldElement>,
    field_5: Vec<BFieldElement>,
}
impl DeprecatedSnippet for LoadStructFromInput {
    fn entrypoint_name(&self) -> String {
        format!("tasm_io_load_struct_from_input_{}", self.input_source)
    }

    fn input_field_names(&self) -> Vec<String> {
        vec!["field_count".to_owned()]
    }

    fn input_types(&self) -> Vec<crate::snippet::DataType> {
        vec![DataType::U32]
    }

    fn output_types(&self) -> Vec<crate::snippet::DataType> {
        vec![DataType::VoidPointer]
    }

    fn output_field_names(&self) -> Vec<String> {
        vec!["*addr".to_string()]
    }

    fn stack_diff(&self) -> isize {
        0
    }

    fn function_code(&self, library: &mut crate::library::Library) -> String {
        let entrypoint = self.entrypoint_name();

        let load_from_io = library.import(Box::new(LoadFromInput(self.input_source.clone())));

        format!(
            "
            // BEFORE: _ num_fields
            // AFTER: _ *addr
            {entrypoint}:
                // Get pointer to the 1st field of the struct (num_fields should not be 0)
                // Futhermore, `num_fields` should be set in the code (known statically),
                // not ever set through user input
                call {load_from_io} // _ num_fields *addr


                swap 1
                push -1 add                 // _ *addr num_fields-1

                call {entrypoint}_loop
                                            // *addr 0

                pop

                return

                // INVARIANT: _ *addr fields_remaining
                {entrypoint}_loop:
                    // test termination condition
                    dup 0
                    push 0 eq               // _ *addr num_fields num_fields==0
                    skiz return             // _ *addr num_fields

                    // decrement counter
                    push -1 add             // _ *addr num_fields-1

                    // read field
                    call {load_from_io}     // _ *addr num_fields-1 *field_addr
                    pop                     // _ *addr num_fields-1

                    recurse
                "
        )
    }

    fn crash_conditions(&self) -> Vec<String> {
        vec![
            "number of fields is zero".to_owned(),
            "number of fields exceeds 2^32".to_owned(),
            "size exceeds 2^32".to_owned(),
            "allocated memory exceeds 2^32".to_owned(),
            "input is shorter than indicated length".to_owned(),
            "input is empty".to_owned(),
        ]
    }

    fn gen_input_states(&self) -> Vec<crate::ExecutionState> {
        let mut ret = vec![];
        let mut thread_rng = rand::thread_rng();

        // 1
        let s1 = DummyStructSize1 {
            field_1: random_elements(thread_rng.gen_range(0..20)),
        };
        let s1_execution_state = match self.input_source {
            InputSource::StdIn => ExecutionState {
                stack: [get_init_tvm_stack(), vec![BFieldElement::new(1)]].concat(),
                std_in: s1.encode(),
                nondeterminism: NonDeterminism::new(vec![]),
                memory: HashMap::default(),
                words_allocated: 1,
            },
            InputSource::SecretIn => ExecutionState {
                stack: [get_init_tvm_stack(), vec![BFieldElement::new(1)]].concat(),
                std_in: vec![],
                nondeterminism: NonDeterminism::new(s1.encode()),
                memory: HashMap::default(),
                words_allocated: 1,
            },
        };
        ret.push(s1_execution_state);

        // 2
        let s2 = DummyStructSize2 {
            field_1: random_elements(thread_rng.gen_range(0..20)),
            field_2: random_elements(thread_rng.gen_range(0..20)),
        };
        let s2_execution_state = match self.input_source {
            InputSource::StdIn => ExecutionState {
                stack: [get_init_tvm_stack(), vec![BFieldElement::new(2)]].concat(),
                std_in: s2.encode(),
                nondeterminism: NonDeterminism::new(vec![]),
                memory: HashMap::default(),
                words_allocated: 1,
            },
            InputSource::SecretIn => ExecutionState {
                stack: [get_init_tvm_stack(), vec![BFieldElement::new(2)]].concat(),
                std_in: vec![],
                nondeterminism: NonDeterminism::new(s2.encode()),
                memory: HashMap::default(),
                words_allocated: 1,
            },
        };
        ret.push(s2_execution_state);

        // 3
        let s3 = DummyStructSize3 {
            field_1: random_elements(thread_rng.gen_range(0..20)),
            field_2: random_elements(thread_rng.gen_range(0..20)),
            field_3: random_elements(thread_rng.gen_range(0..20)),
        };
        let s3_execution_state = match self.input_source {
            InputSource::StdIn => ExecutionState {
                stack: [get_init_tvm_stack(), vec![BFieldElement::new(3)]].concat(),
                std_in: s3.encode(),
                nondeterminism: NonDeterminism::new(vec![]),
                memory: HashMap::default(),
                words_allocated: 1,
            },
            InputSource::SecretIn => ExecutionState {
                stack: [get_init_tvm_stack(), vec![BFieldElement::new(3)]].concat(),
                std_in: vec![],
                nondeterminism: NonDeterminism::new(s3.encode()),
                memory: HashMap::default(),
                words_allocated: 1,
            },
        };
        ret.push(s3_execution_state);

        // 5
        let s5 = DummyStructSize5 {
            field_1: random_elements(thread_rng.gen_range(0..20)),
            field_2: random_elements(thread_rng.gen_range(0..20)),
            field_3: random_elements(thread_rng.gen_range(0..20)),
            field_4: random_elements(thread_rng.gen_range(0..20)),
            field_5: random_elements(thread_rng.gen_range(0..20)),
        };
        let s5_execution_state = match self.input_source {
            InputSource::StdIn => ExecutionState {
                stack: [get_init_tvm_stack(), vec![BFieldElement::new(5)]].concat(),
                std_in: s5.encode(),
                nondeterminism: NonDeterminism::new(vec![]),
                memory: HashMap::default(),
                words_allocated: 1,
            },
            InputSource::SecretIn => ExecutionState {
                stack: [get_init_tvm_stack(), vec![BFieldElement::new(5)]].concat(),
                std_in: vec![],
                nondeterminism: NonDeterminism::new(s5.encode()),
                memory: HashMap::default(),
                words_allocated: 1,
            },
        };
        ret.push(s5_execution_state);

        ret
    }

    fn common_case_input_state(&self) -> crate::ExecutionState {
        let s5 = DummyStructSize5 {
            field_1: random_elements(20),
            field_2: random_elements(20),
            field_3: random_elements(20),
            field_4: random_elements(20),
            field_5: random_elements(20),
        };
        match self.input_source {
            InputSource::StdIn => ExecutionState {
                stack: [get_init_tvm_stack(), vec![BFieldElement::new(5)]].concat(),
                std_in: s5.encode(),
                nondeterminism: NonDeterminism::new(vec![]),
                memory: HashMap::default(),
                words_allocated: 1,
            },
            InputSource::SecretIn => ExecutionState {
                stack: [get_init_tvm_stack(), vec![BFieldElement::new(5)]].concat(),
                std_in: vec![],
                nondeterminism: NonDeterminism::new(s5.encode()),
                memory: HashMap::default(),
                words_allocated: 1,
            },
        }
    }

    fn worst_case_input_state(&self) -> crate::ExecutionState {
        let s5 = DummyStructSize5 {
            field_1: random_elements(200),
            field_2: random_elements(200),
            field_3: random_elements(200),
            field_4: random_elements(200),
            field_5: random_elements(200),
        };
        match self.input_source {
            InputSource::StdIn => ExecutionState {
                stack: [get_init_tvm_stack(), vec![BFieldElement::new(5)]].concat(),
                std_in: s5.encode(),
                nondeterminism: NonDeterminism::new(vec![]),
                memory: HashMap::default(),
                words_allocated: 1,
            },
            InputSource::SecretIn => ExecutionState {
                stack: [get_init_tvm_stack(), vec![BFieldElement::new(5)]].concat(),
                std_in: vec![],
                nondeterminism: NonDeterminism::new(s5.encode()),
                memory: HashMap::default(),
                words_allocated: 1,
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
        // BEFORE: _ num_fields
        // AFTER: _ *addr

        let mut cheated_input = match self.input_source {
            InputSource::StdIn => std_in,
            InputSource::SecretIn => secret_in,
        };

        let num_fields = stack.pop().unwrap().value() as usize;

        let mut next_field_length_indicator = cheated_input[0].value() as usize;
        for _ in 0..num_fields {
            load_from_input::LoadFromInput(self.input_source.clone()).rust_shadowing(
                stack,
                cheated_input.clone(),
                cheated_input.clone(),
                memory,
            );
            cheated_input = cheated_input[next_field_length_indicator + 1..].to_vec();
            next_field_length_indicator =
                cheated_input[next_field_length_indicator + 1].value() as usize;
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::test_helpers::test_rust_equivalence_multiple_deprecated;

    use super::*;

    #[test]
    fn new_snippet_test() {
        test_rust_equivalence_multiple_deprecated(&LoadFromInput(InputSource::SecretIn), true);
        test_rust_equivalence_multiple_deprecated(&LoadFromInput(InputSource::StdIn), true);
    }

    // TODO: Comment this test in and fix it if you feel like it.
    // #[test]
    // fn verify_dyn_malloc_shows_correct_next_value() {
    //     for length in 0..10 {
    //         let state = ExecutionState {
    //             stack: get_init_tvm_stack(),
    //             memory: std::collections::HashMap::new(),
    //             std_in: vec![
    //                 vec![BFieldElement::new(length)],
    //                 random_elements(length as usize),
    //             ]
    //             .concat(),
    //             nondeterminism: NonDeterminism::new(vec![]),
    //             words_allocated: 0,
    //         };
    //         let snippet = LoadFromInput(InputSource::StdIn);
    //         let stack_diff = snippet.stack_diff();
    //         let res = execute_with_execution_state(state, Box::new(snippet), stack_diff).unwrap();

    //         // Verify final state of dyn malloc. dyn malloc should be set to the next available
    //         // memory address.
    //         // Expected memory layout after running above program:
    //         // 0: dynamic allocator value
    //         // 1: indicated length
    //         // 2..N: elements of input, after indicated length
    //         // N+1: next available memory address
    //         let indicated_next_free_address =
    //             res.final_ram[&BFieldElement::new(dyn_malloc::DYN_MALLOC_ADDRESS as u64)].value();
    //         assert_eq!(length + 2, indicated_next_free_address);
    //         assert!(res
    //             .final_ram
    //             .get(&BFieldElement::new(indicated_next_free_address))
    //             .is_none());
    //     }
    // }
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
