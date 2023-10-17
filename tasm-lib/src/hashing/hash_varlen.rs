use std::collections::HashMap;

use rand::random;
use triton_vm::NonDeterminism;
use twenty_first::{
    shared_math::b_field_element::BFieldElement, util_types::algebraic_hasher::AlgebraicHasher,
};

use crate::{
    empty_stack,
    snippet::{DataType, DeprecatedSnippet},
    ExecutionState, VmHasher,
};

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

    fn input_types(&self) -> Vec<crate::snippet::DataType> {
        vec![DataType::BFE, DataType::U32]
    }

    fn output_types(&self) -> Vec<crate::snippet::DataType> {
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

    fn function_code(&self, _library: &mut crate::library::Library) -> String {
        let entrypoint = self.entrypoint_name();

        format!(
            "
            // BEFORE:      _ addr len
            // INVARIANT:   <none>
            // AFTER:       _ digest[4] digest[3] digest[2] digest[1] digest[0]
            {entrypoint}:
                sponge_init
                call {entrypoint}_absorb_all_full_chunks
                                // _ new_addr len_remaining

                // determine number of padding zeros
                dup 0           // _ addr len len
                push -9         // _ addr len len -9
                add             // _ addr len len-9
                push -1         // _ addr len len-9 -1
                mul             // _ addr len 9-len
                dup 2           // _ addr len 9-len addr
                dup 2           // _ addr len 9-len addr len

                call {entrypoint}_pad_with_nine_minus_remaining_length_zeros
                                // _ addr len (0)^(9-len) 0 addr len
                swap 2          // _ addr len (0)^(9-len) len addr 0
                push 1 add      // _ addr len (0)^(9-len) len addr 1
                swap 2          // _ addr len (0)^(9-len) 1 addr len

                call {entrypoint}_read_remaining_elements_from_memory
                                // _ addr len (0)^(9-len) 1 (element)^len addr 0
                pop pop         // _ addr len (0)^(9-len) 1 (element)^len
                sponge_absorb
                pop pop         // _ addr len * * * * * * * *
                sponge_squeeze  // _ d[9] d[8] d[7] d[6] d[5] d[4] d[3] d[2] d[1] d[0]
                swap 5 pop      // _ d[9] d[8] d[7] d[6] d[0] d[4] d[3] d[2] d[1]
                swap 5 pop      // _ d[9] d[8] d[7] d[1] d[0] d[4] d[3] d[2]
                swap 5 pop      // _ d[9] d[8] d[2] d[1] d[0] d[4] d[3]
                swap 5 pop      // _ d[9] d[3] d[2] d[1] d[0] d[4]
                swap 5 pop      // _ d[4] d[3] d[2] d[1] d[0]
                return

            // BEFORE:      _ addr len 9-len addr len
            // INVARIANT:   _ addr len (0)^i 9-len-i addr len
            // AFTER:       _ addr len (0)^(9-len) 0 addr len
            {entrypoint}_pad_with_nine_minus_remaining_length_zeros:
                // return condition: (9-len) many 0s have been collected
                dup 2           // _ addr len (0)^i 9-len-i addr len 9-len-i
                push 0 eq       // _ addr len (0)^i 9-len-i addr len 9-len-i==0
                skiz return     // _ addr len (0)^(9-len) 0 addr len

                                // _ addr len (0)^i 9-len-i addr len
                push 0          // _ addr len (0)^i 9-len-i addr len 0
                swap 3          // _ addr len (0)^(i+1) addr len 9-len-i
                push -1         // _ addr len (0)^(i+1) addr len 9-len-i -1
                add             // _ addr len (0)^(i+1) addr len 9-len-i-1
                swap 2          // _ addr len (0)^(i+1) 9-len-i-1 len addr
                swap 1          // _ addr len (0)^(i+1) 9-len-i-1 addr len
                recurse

            // BEFORE:      _ addr len (0)^(9-len) 1 addr len
            // INVARIANT:   _ addr len (0)^(9-len) 1 (element)^i addr len-i
            // AFTER:       _ addr len (0)^(9-len) 1 (element)^len addr 0
            {entrypoint}_read_remaining_elements_from_memory:
                // return condition: no elements remaining
                dup 0           // _ addr len (0)^(9-len) 1 (element)^i addr len len
                push 0 eq       // _ addr len (0)^(9-len) 1 (element)^i addr len len==0
                skiz return     // _ addr len (0)^(9-len) 1 (element)^len addr 0

                                // _ addr len (0)^(9-len) 1 (element)^i addr len-i
                dup 1           // _ addr len (0)^(9-len) 1 (element)^i addr len-i addr
                dup 1           // _ addr len (0)^(9-len) 1 (element)^i addr len-i addr len-i
                add             // _ addr len (0)^(9-len) 1 (element)^i addr len-i addr+len-i
                push -1         // _ addr len (0)^(9-len) 1 (element)^i addr len-i addr+len-i -1
                add             // _ addr len (0)^(9-len) 1 (element)^i addr len-i addr+len-i-1
                read_mem        // _ addr len (0)^(9-len) 1 (element)^i addr len-i addr+len-i-1 element
                swap 3          // _ addr len (0)^(9-len) 1 (element)^(i+1) len-i addr+len-i-1 addr
                swap 2          // _ addr len (0)^(9-len) 1 (element)^(i+1) addr addr+len-i-1 len-i
                swap 1          // _ addr len (0)^(9-len) 1 (element)^(i+1) addr len-i addr+len-i-1
                pop             // _ addr len (0)^(9-len) 1 (element)^(i+1) addr len-i
                push -1         // _ addr len (0)^(9-len) 1 (element)^(i+1) addr len-i -1
                add             // _ addr len (0)^(9-len) 1 (element)^(i+1) addr len-i-1
                recurse

            // BEFORE:      _ addr len
            // INVARIANT:   _ some_addr some_len
            // AFTER:       _ new_addr len_remaining
            {entrypoint}_absorb_all_full_chunks:
                // return condition: less than 10 elements remaining
                push 10         // _ addr len 10
                dup 1           // _ addr len 10 len
                lt              // _ addr len (len < 10)
                skiz return     // _ addr len

                // read 10 elements to stack
                swap 1          // _ len addr
                dup 0           // _ len addr addr
                push 9          // _ len addr addr 9
                add             // _ len addr addr+9
                read_mem        // _ len addr addr+9 element
                swap 1          // _ len addr element addr+9
                push -1         // _ len addr element addr+9 -1
                add             // _ len addr element addr+8
                read_mem        // _ len addr element addr+8 element
                swap 1          // _ len addr (element)^2 addr+8
                push -1         // _ len addr (element)^2 addr+8 -1
                add             // _ len addr (element)^2 addr+7
                read_mem        // _ len addr (element)^2 addr+7 element
                swap 1          // _ len addr (element)^3 addr+7
                push -1         // _ len addr (element)^3 addr+7 -1
                add             // _ len addr (element)^3 addr+6
                read_mem        // _ len addr (element)^3 addr+6 element
                swap 1          // _ len addr (element)^4 addr+6
                push -1         // _ len addr (element)^4 addr+6 -1
                add             // _ len addr (element)^4 addr+5
                read_mem        // _ len addr (element)^4 addr+5 element
                swap 1          // _ len addr (element)^5 addr+5
                push -1         // _ len addr (element)^5 addr+5 -1
                add             // _ len addr (element)^5 addr+4
                read_mem        // _ len addr (element)^5 addr+4 element
                swap 1          // _ len addr (element)^6 addr+4
                push -1         // _ len addr (element)^6 addr+4 -1
                add             // _ len addr (element)^6 addr+3
                read_mem        // _ len addr (element)^6 addr+3 element
                swap 1          // _ len addr (element)^7 addr+3
                push -1         // _ len addr (element)^7 addr+3 -1
                add             // _ len addr (element)^7 addr+2
                read_mem        // _ len addr (element)^7 addr+2 element
                swap 1          // _ len addr (element)^8 addr+2
                push -1         // _ len addr (element)^8 addr+2 -1
                add             // _ len addr (element)^8 addr+1
                read_mem        // _ len addr (element)^8 addr+1 element
                swap 1          // _ len addr (element)^9 addr+1
                push -1         // _ len addr (element)^9 addr+1 -1
                add             // _ len addr (element)^9 addr
                read_mem        // _ len addr (element)^9 addr element
                swap 1          // _ len addr (element)^10 addr
                pop             // _ len addr (element)^10

                sponge_absorb
                pop pop pop pop pop
                pop pop pop pop pop
                                // _ len addr
                push 10         // _ len addr 10
                add             // _ len (addr+10)
                swap 1          // _ (addr+10) len
                push -10        // _ (addr+10) len -10
                add             // _ (addr+10) (len-10)
                recurse
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
        stack: &mut Vec<triton_vm::BFieldElement>,
        _std_in: Vec<triton_vm::BFieldElement>,
        _secret_in: Vec<triton_vm::BFieldElement>,
        memory: &mut std::collections::HashMap<triton_vm::BFieldElement, triton_vm::BFieldElement>,
    ) {
        let length: u32 = stack.pop().unwrap().try_into().unwrap();
        let memory_pointer: BFieldElement = stack.pop().unwrap();

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
    fn leaf_index_to_mt_index_benchmark() {
        bench_and_write(HashVarlen);
    }
}
