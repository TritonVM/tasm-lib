use std::collections::HashMap;

use itertools::Itertools;
use rand::random;
use rand::rngs::StdRng;
use rand::thread_rng;
use rand::Rng;
use rand::SeedableRng;
use triton_vm::prelude::*;
use twenty_first::shared_math::other::random_elements;

use crate::data_type::DataType;
use crate::empty_stack;
use crate::traits::deprecated_snippet::DeprecatedSnippet;
use crate::Digest;
use crate::ExecutionState;

/// Returns the number of elements of a contiguous list.
#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
pub struct GetLength;

#[derive(Clone, Debug, BFieldCodec)]
pub(super) struct DummyOuterDataStructure {
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

/// Determine the length (number of elements) of the given contiguous list. This method
/// assumes that the elements have statically unknown size, since determining the length
/// for lists of elements with statically known size is trivial.
///
/// BEFORE: _ *contiguous_list
///
/// AFTER: _ length
impl GetLength {
    pub(super) fn pseudorandom_input_state(seed: [u8; 32], length: usize) -> ExecutionState {
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
        let mut memory = HashMap::default();
        let encoded = data.encode();
        memory.insert(address, BFieldElement::new(encoded.len() as u64));
        address.increment();
        let stack = [empty_stack(), vec![address]].concat();
        for word in encoded {
            memory.insert(address, word);
            address.increment();
        }

        ExecutionState::with_stack_and_memory(stack, memory)
    }
}

impl DeprecatedSnippet for GetLength {
    fn entrypoint_name(&self) -> String {
        "tasm_list_contiguous_list_get_length".to_string()
    }

    fn input_field_names(&self) -> Vec<String> {
        vec!["*contiguous_list".to_string()]
    }

    fn input_types(&self) -> Vec<DataType> {
        vec![DataType::VoidPointer]
    }

    fn output_field_names(&self) -> Vec<String> {
        vec!["length".to_string()]
    }

    fn output_types(&self) -> Vec<DataType> {
        vec![DataType::U32]
    }

    fn stack_diff(&self) -> isize {
        0
    }

    fn function_code(&self, _library: &mut crate::library::Library) -> String {
        let entrypoint = self.entrypoint_name();

        format!(
            "
        // BEFORE: _ *contiguous_list
        // AFTER:  _ length
        {entrypoint}:
            read_mem 1
            pop 1
            return
        "
        )
    }

    fn crash_conditions(&self) -> Vec<String> {
        vec!["memory blob lives outside of first 2^32 words".to_string()]
    }

    fn gen_input_states(&self) -> Vec<ExecutionState> {
        let mut rng = thread_rng();
        (0..25)
            .map(|_| Self::pseudorandom_input_state(rng.gen(), rng.gen_range(0..4)))
            .collect_vec()
    }

    fn common_case_input_state(&self) -> ExecutionState {
        let mut seed = [0u8; 32];
        seed[0] = 0x01;
        seed[1] = 0xfd;
        Self::pseudorandom_input_state(seed, 2)
    }

    fn worst_case_input_state(&self) -> ExecutionState {
        let mut seed = [0u8; 32];
        seed[0] = 0x01;
        seed[1] = 0xfd;
        Self::pseudorandom_input_state(seed, 5)
    }

    fn rust_shadowing(
        &self,
        stack: &mut Vec<BFieldElement>,
        _std_in: Vec<BFieldElement>,
        _secret_in: Vec<BFieldElement>,
        memory: &mut HashMap<BFieldElement, BFieldElement>,
    ) {
        let address = stack.pop().unwrap();
        let size = memory
            .get(&(address - BFieldElement::new(1u64)))
            .unwrap()
            .value();
        assert!(
            address.value() + size < u32::MAX as u64,
            "Memory address may not exceed u32::MAX"
        );
        let mut encoding = vec![];
        for i in 0..size {
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
    use crate::test_helpers::test_rust_equivalence_multiple_deprecated;

    use super::GetLength;

    #[test]
    fn get_length_test() {
        test_rust_equivalence_multiple_deprecated(&GetLength, true);
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
