use std::collections::HashMap;

use itertools::Itertools;
use rand::{random, rngs::StdRng, thread_rng, Rng, SeedableRng};
use triton_vm::BFieldElement;
use twenty_first::shared_math::{bfield_codec::BFieldCodec, other::random_elements};

use crate::{
    get_init_tvm_stack,
    snippet::{DataType, Snippet},
    Digest, ExecutionState,
};

/// Returns the number of elements of a contiguous list.
pub struct GetLength;

#[derive(Clone, Debug, BFieldCodec)]
struct DummyOuterDataStructure {
    a: Vec<DummyInnerDataStructure>,
    b: Vec<Digest>,
    c: Vec<BFieldElement>,
    d: BFieldElement,
}

#[derive(Clone, Debug, BFieldCodec)]
struct DummyInnerDataStructure {
    b: Vec<Digest>,
    c: Vec<BFieldElement>,
}

impl GetLength {
    fn pseudorandom_input_state(seed: [u8; 32], length: usize) -> ExecutionState {
        let mut rng: StdRng = SeedableRng::from_seed(seed);
        let mut data = vec![];

        for _ in 0..length {
            let inner_count = rng.gen_range(0..10);
            let mut inner_data = vec![];
            for _ in 0..inner_count {
                inner_data.push(DummyInnerDataStructure {
                    b: random_elements(rng.gen_range(0..5)),
                    c: random_elements(rng.gen_range(0..5)),
                });
            }
            data.push(DummyOuterDataStructure {
                a: inner_data,
                b: random_elements(rng.gen_range(0..5)),
                c: random_elements(rng.gen_range(0..5)),
                d: random(),
            });
        }

        let mut address = BFieldElement::new(rng.gen_range(1..(1 << 20)));
        let stack = vec![get_init_tvm_stack(), vec![address]].concat();
        let mut memory = HashMap::default();
        let encoded = data.encode();
        for word in encoded {
            memory.insert(address, word);
            address.increment();
        }

        ExecutionState::with_stack_and_memory(stack, memory, 1)
    }
}

impl Snippet for GetLength {
    fn entrypoint(&self) -> String {
        "tasm_list_contiguous_list_get_length".to_string()
    }

    fn inputs(&self) -> Vec<String> {
        vec!["*contiguous_list".to_string()]
    }

    fn input_types(&self) -> Vec<crate::snippet::DataType> {
        vec![DataType::VoidPointer]
    }

    fn output_types(&self) -> Vec<crate::snippet::DataType> {
        vec![DataType::U32]
    }

    fn outputs(&self) -> Vec<String> {
        vec!["length".to_string()]
    }

    fn stack_diff(&self) -> isize {
        0
    }

    fn function_code(&self, _library: &mut crate::snippet_state::SnippetState) -> String {
        let entrypoint = self.entrypoint();

        format!(
            "
        // BEFORE: _ *contiguous_list
        // AFTER: _ length
        {entrypoint}:
            dup 0 // _ *contiguous_list *contiguous_list
            read_mem // _ *cl *cl si
            swap 1 // _ *cl si *cl
            push 1 add // _ *cl si *start
            push 0 // _ *cl si *start 0
            call {entrypoint}_loop
            // _ *cl si *sth element_count

            swap 3 // _ element_count si *sth *cl
            pop pop pop // _ element_count

            return

        // INVARIANT: _ *cl si *elem counter
        {entrypoint}_loop:
            // evaluate termination criterion
            dup 3 dup 3 add // _ *cl si *elem counter *cl+si
            dup 2 // _ *cl si *elem counter *cl+si *elem
            lt // _ *cl si *elem  counter *elem<*cl+si
            push 0 eq // _ *cl si *elem counter *elem>=*cl+si
            skiz return

            push 1 add // _ *cl si *elem counter+1

            swap 1 // _ *cl si counter+1 *elem
            read_mem // _ *cl si counter+1 *elem elem_size
            add // _ *cl si counter+1 *elem+size
            push 1 add // _ *cl si counter+1 *next_elem
            swap 1 // _ *cl si *next_elem counter+1

            recurse
            "
        )
    }

    fn crash_conditions(&self) -> Vec<String> {
        vec!["memory blob lives outside of first 2^32 words".to_string()]
    }

    fn gen_input_states(&self) -> Vec<crate::ExecutionState> {
        let mut rng = thread_rng();
        (0..25)
            .map(|_| Self::pseudorandom_input_state(rng.gen(), rng.gen_range(0..4)))
            .collect_vec()
    }

    fn common_case_input_state(&self) -> crate::ExecutionState {
        let mut seed = [0u8; 32];
        seed[0] = 0x01;
        seed[1] = 0xfd;
        Self::pseudorandom_input_state(seed, 2)
    }

    fn worst_case_input_state(&self) -> crate::ExecutionState {
        let mut seed = [0u8; 32];
        seed[0] = 0x01;
        seed[1] = 0xfd;
        Self::pseudorandom_input_state(seed, 5)
    }

    fn rust_shadowing(
        &self,
        stack: &mut Vec<triton_vm::BFieldElement>,
        _std_in: Vec<triton_vm::BFieldElement>,
        _secret_in: Vec<triton_vm::BFieldElement>,
        memory: &mut std::collections::HashMap<triton_vm::BFieldElement, triton_vm::BFieldElement>,
    ) {
        let address = stack.pop().unwrap();
        let size = memory.get(&address).unwrap().value();
        assert!(
            address.value() + size < u32::MAX as u64,
            "Memory address may not exceed u32::MAX"
        );
        let mut encoding = vec![];
        for i in 0..=size {
            encoding.push(
                memory
                    .get(&(address + BFieldElement::new(i)))
                    .unwrap()
                    .to_owned(),
            );
        }

        let dummy_list: Vec<DummyOuterDataStructure> = *Vec::<_>::decode(&encoding).unwrap();

        stack.push(BFieldElement::new(dummy_list.len() as u64));
    }
}

#[cfg(test)]
mod tests {
    use crate::test_helpers::test_rust_equivalence_multiple;

    use super::GetLength;

    #[test]
    fn get_length_test() {
        test_rust_equivalence_multiple(&GetLength, true);
    }
}

#[cfg(test)]
mod benches {
    use crate::snippet_bencher::bench_and_write;

    use super::GetLength;

    #[test]
    fn get_length_benchmark() {
        bench_and_write(GetLength);
    }
}
