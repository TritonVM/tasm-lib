use std::collections::HashMap;

use rand::random;
use triton_vm::NonDeterminism;
use twenty_first::{
    shared_math::b_field_element::BFieldElement, util_types::algebraic_hasher::AlgebraicHasher,
};

use crate::data_type::DataType;
use crate::hashing::absorb::Absorb;
use crate::{empty_stack, snippet::DeprecatedSnippet, ExecutionState, VmHasher};

#[derive(Clone, Debug)]
pub struct HashVarlen;

impl HashVarlen {
    fn random_memory_state_read_k(k: u64) -> ExecutionState {
        let memory_start: BFieldElement = random();
        let memory: HashMap<BFieldElement, BFieldElement> = (0..k)
            .map(|i| (memory_start + BFieldElement::new(i), random()))
            .collect();

        ExecutionState {
            stack: [empty_stack(), vec![memory_start, BFieldElement::new(k)]].concat(),
            std_in: vec![],
            nondeterminism: NonDeterminism::new(vec![]),
            memory,
            words_allocated: 1,
        }
    }
}

impl DeprecatedSnippet for HashVarlen {
    fn entrypoint_name(&self) -> String {
        "tasm_hashing_hash_varlen".to_string()
    }

    fn input_field_names(&self) -> Vec<String> {
        vec!["*addr".to_string(), "length".to_string()]
    }

    fn input_types(&self) -> Vec<crate::data_type::DataType> {
        vec![DataType::Bfe, DataType::U32]
    }

    fn output_types(&self) -> Vec<crate::data_type::DataType> {
        vec![DataType::Digest]
    }

    fn output_field_names(&self) -> Vec<String> {
        vec![
            "elemement_4".to_string(),
            "elemement_3".to_string(),
            "elemement_2".to_string(),
            "elemement_1".to_string(),
            "elemement_0".to_string(),
        ]
    }

    fn stack_diff(&self) -> isize {
        3
    }

    fn function_code(&self, library: &mut crate::library::Library) -> String {
        let entrypoint = self.entrypoint_name();
        let absorb_subroutine = library.import(Box::new(Absorb));

        format!(
            "
            // BEFORE:      _ addr len
            // INVARIANT:   <none>
            // AFTER:       _ digest[4] digest[3] digest[2] digest[1] digest[0]
            {entrypoint}:
                sponge_init
                call {absorb_subroutine}
                sponge_squeeze  // _ d[9] d[8] d[7] d[6] d[5] d[4] d[3] d[2] d[1] d[0]
                swap 5 pop 1    // _ d[9] d[8] d[7] d[6] d[0] d[4] d[3] d[2] d[1]
                swap 5 pop 1    // _ d[9] d[8] d[7] d[1] d[0] d[4] d[3] d[2]
                swap 5 pop 1    // _ d[9] d[8] d[2] d[1] d[0] d[4] d[3]
                swap 5 pop 1    // _ d[9] d[3] d[2] d[1] d[0] d[4]
                swap 5 pop 1    // _ d[4] d[3] d[2] d[1] d[0]
                return
                "
        )
    }

    fn crash_conditions(&self) -> Vec<String> {
        vec!["Length exceeds u32::MAX".to_string()]
    }

    fn gen_input_states(&self) -> Vec<crate::ExecutionState> {
        vec![
            Self::random_memory_state_read_k(0),
            Self::random_memory_state_read_k(1),
            Self::random_memory_state_read_k(5),
            Self::random_memory_state_read_k(9),
            Self::random_memory_state_read_k(10),
            Self::random_memory_state_read_k(11),
            Self::random_memory_state_read_k(19),
            Self::random_memory_state_read_k(20),
            Self::random_memory_state_read_k(21),
        ]
    }

    fn common_case_input_state(&self) -> crate::ExecutionState {
        Self::random_memory_state_read_k(25)
    }

    fn worst_case_input_state(&self) -> crate::ExecutionState {
        Self::random_memory_state_read_k(1000)
    }

    fn rust_shadowing(
        &self,
        stack: &mut Vec<BFieldElement>,
        _std_in: Vec<BFieldElement>,
        _secret_in: Vec<BFieldElement>,
        memory: &mut HashMap<BFieldElement, BFieldElement>,
    ) {
        let length: u32 = stack.pop().unwrap().try_into().unwrap();
        let memory_pointer: BFieldElement = stack.pop().unwrap();

        memory.extend(Absorb::statically_allocated_memory(
            memory_pointer,
            length.into(),
        ));

        let mut preimage = vec![];
        for i in 0..length as u64 {
            let address = memory_pointer + BFieldElement::new(i);
            let maybe_memory_value = memory.get(&address).copied();
            let memory_value = maybe_memory_value.unwrap_or_default();
            preimage.push(memory_value);
        }

        let digest = VmHasher::hash_varlen(&preimage);
        stack.extend(digest.reversed().values().to_vec());
    }
}

#[cfg(test)]
mod tests {
    use crate::test_helpers::test_rust_equivalence_multiple_deprecated;

    use super::*;

    #[test]
    fn new_prop_test() {
        test_rust_equivalence_multiple_deprecated(&HashVarlen, true);
    }
}

#[cfg(test)]
mod benches {
    use super::*;
    use crate::snippet_bencher::bench_and_write;

    #[test]
    fn benchmark() {
        bench_and_write(HashVarlen);
    }
}
