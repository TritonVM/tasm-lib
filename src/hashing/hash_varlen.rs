use std::collections::HashMap;

use num::{One, Zero};
use twenty_first::{
    shared_math::b_field_element::BFieldElement, util_types::algebraic_hasher::AlgebraicHasher,
};

use crate::{
    get_init_tvm_stack,
    snippet::{DataType, Snippet},
    ExecutionState, VmHasher,
};

#[derive(Clone, Debug)]
pub struct HashVarlen;

impl Snippet for HashVarlen {
    fn entrypoint(&self) -> String {
        "tasm_hashing_hash_varlen".to_string()
    }

    fn inputs(&self) -> Vec<String>
    where
        Self: Sized,
    {
        vec!["*addr".to_string(), "length".to_string()]
    }

    fn input_types(&self) -> Vec<crate::snippet::DataType> {
        vec![DataType::BFE, DataType::U32]
    }

    fn output_types(&self) -> Vec<crate::snippet::DataType> {
        vec![DataType::Digest]
    }

    fn outputs(&self) -> Vec<String>
    where
        Self: Sized,
    {
        vec![
            "elemement_4".to_string(),
            "elemement_3".to_string(),
            "elemement_2".to_string(),
            "elemement_1".to_string(),
            "elemement_0".to_string(),
        ]
    }

    fn stack_diff(&self) -> isize
    where
        Self: Sized,
    {
        3
    }

    fn function_body(&self, _library: &mut crate::snippet_state::SnippetState) -> String {
        let entrypoint = self.entrypoint();

        format!(
            "
            // BEFORE: _ *addr length
            // AFTER: _ digest_element_4 digest_element_3 digest_element_2 digest_element_1 digest_element_0
            {entrypoint}:
                // absorb all chunks of 10 elements
                push 1
                call {entrypoint}_loop
                // _ *addr first_time length

                swap 1 // _ *addr length first_time

                // pad
                dup 2 // _ *addr length first_time length
                push -9 // _ *addr length first_time length -9
                mul // _ *addr length first_time length-9
                push -1 // _ *addr length first_time length-9 -1
                mul // _ *addr length first_time 9-length
                dup 3 // _ *addr length first_time 9-length *addr
                dup 3 // _ *addr length first_time 9-length *addr length
                call {entrypoint}_pad_varnum_zeros
                // _ addr length first_time 0^(9-length) 0 *addr length
                swap 2 // _ *addr length first_time 0^(9-length) length *addr 0
                push 1 add // _ *addr length first_time 0^(9-length) length *addr 1
                swap 2 // _ *addr length first_time 0^(9-length) 1 *addr length

                // read remaining elements from memory
                call {entrypoint}_read_remaining_elements
                // _ *addr length first_time 0^(9-length) 1 re^length *addr 0
                pop pop // _ *addr length first_time 0^(9-length) 1 re^length

                // absorb_init if first_time is 1; otherwise absorb
                dup 10 // _ *addr length first_time [0,1] [0,1] [0,1] [0,1] [0,1] [0,1] [0,1] [0,1] [0,1] [0,1] first_time
                push 1 // _ *addr length first_time [0,1] [0,1] [0,1] [0,1] [0,1] [0,1] [0,1] [0,1] [0,1] [0,1] first_time 1
                swap 1 // _ *addr length first_time [0,1] [0,1] [0,1] [0,1] [0,1] [0,1] [0,1] [0,1] [0,1] [0,1] 1 first_time
                skiz call {entrypoint}_if_first_time_absob_init
                skiz absorb

                // _ *addr length first_time [0,1] [0,1] [0,1] [0,1] [0,1] [0,1] [0,1] [0,1] [0,1] [0,1]

                pop pop pop // _ *addr length first_time  [0,1] [0,1] [0,1] [0,1] [0,1] [0,1] [0,1]

                // squeeze 5 elements
                squeeze // _ d9 d8 d7 d6 d5 d4 d3 d2 d1 d0
                swap 5 pop swap 5 pop swap 5 pop swap 5 pop swap 5 pop  // _ d4 d3 d2 d1 d0

                return

            // BEFORE: _ *addr length first_time 9-length *addr length
            // INVARIANT: _ *addr length first_time 0^(9-len) len *addr length
            // AFTER: _ addr length first_time 0^(9-length) 0 *addr length
            {entrypoint}_pad_varnum_zeros:
                // evaluate return condition
                dup 2 // _ *addr length first_time 0^(9-len) len *addr length len
                skiz return

                // _ *addr length first_time 0^(9-len) len *addr length
                push 0 // _ *addr length first_time 0^(9-len) len *addr length 0
                swap 3 // _ *addr length first_time 0^(9-len) 0 *addr length len
                push -1 // _ *addr length first_time 0^(9-len) 0 *addr length len -1
                add // _ *addr length first_time 0^(9-len) 0 *addr length len-1
                swap 2 // _ *addr length first_time 0^(9-len) 0 len-1 length *addr
                swap 1 // _ *addr length first_time 0^(9-len) 0 len-1 *addr length
                // _ *addr length first_time 0^(9-len+1) len-1 *addr length

                recurse

            // BEFORE: _ *addr length first_time 0^(9-length) 1 *addr length
            // AFTER: _ *addr length first_time 0^(9-length) 1 re^length *addr 0
            {entrypoint}_read_remaining_elements:
                // evaluate return condition
                dup 0 // _ *addr length first_time 0^(9-length) 1 re^* *addr length length
                skiz return

                // _ *addr length first_time 0^(9-length) 1 re^* *addr length

                dup 1 // _ *addr length first_time 0^(9-length) 1 re^* *addr length *addr
                dup 1 // _ *addr length first_time 0^(9-length) 1 re^* *addr length *addr length
                add // _ *addr length first_time 0^(9-length) 1 re^* *addr length *addr+length
                push -1 add // _ *addr length first_time 0^(9-length) 1 re^* *addr length *addr+length-1
                read_mem // _ *addr length first_time 0^(9-length) 1 re^* *addr length *addr+length-1 re
                swap 3
                swap 2
                swap 1
                // _ *addr length first_time 0^(9-length) 1 re^* re *addr length *addr+length-1
                pop // _ *addr length first_time 0^(9-length) 1 re^* re *addr length
                push -1 add // _ *addr length first_time 0^(9-length) 1 re^* re *addr length-1
                // _ *addr length first_time 0^(9-length) 1 re^* *addr length-1

                recurse

            {entrypoint}_if_first_time_absob_init:
                pop
                absorb_init
                push 0
                return

            // BEFORE: _ *addr length first_time=1
            // AFTER: _ *addr length first_time=0
            {entrypoint}_loop:
                // termination condition: 10 or more elements remaining?
                swap 1 // _ *addr first_time length
                push 10 // _ *addr first_time length 10
                dup 1 // _ *addr first_time length 10 length
                lt // _ *addr first_time length (length < 10)

                // check condition
                skiz
                    return
                // _ *addr first_time length

                // body
                // read 10 elements to stack
                swap 2   // _ length first_time *addr
                dup 0    // _ length first_time *addr *addr
                push 9   // _ length first_time *addr *addr 9
                add      // _ length first_time *addr (*addr+9)
                read_mem // _ length first_time *addr (*addr+9) element_9
                swap 1   // _ length first_time *addr element_9 (*addr+9)
                push -1  // _ length first_time *addr element_9 (*addr+9) -1
                add      // _ length first_time *addr element_9 (*addr+8)
                read_mem // _ length first_time *addr element_9 (*add+8) element_8
                swap 1   // _ length first_time *addr element_9 element_8 (*addr+8)
                push -1  // _ length first_time *addr element_9 element_8 (*addr+8) -1
                add      // _ length first_time *addr element_9 element_8 (*addr+7)
                read_mem // _ length first_time *addr element_9 element_8 (*addr+7) element_7
                swap 1   // _ length first_time *addr element_9 element_8 element_7 (*addr+7)
                push -1  // _ length first_time *addr element_9 element_8 element_7 (*addr+7) -1
                add      // _ length first_time *addr element_9 element_8 element_7 (*addr+6)
                read_mem // _ length first_time *addr element_9 element_8 element_7 (*addr+6) element_6
                swap 1   // _ length first_time *addr element_9 element_8 element_7 element_6 (*addr+6)
                push -1  // _ length first_time *addr element_9 element_8 element_7 element_6 (*addr+6) -1
                add      // _ length first_time *addr element_9 element_8 element_7 element_6 (*addr+5)
                read_mem // _ length first_time *addr element_9 element_8 element_7 element_6 (*addr+5) element_5
                swap 1   // _ length first_time *addr element_9 element_8 element_7 element_6 element_5 (*addr+5)
                push -1  // _ length first_time *addr element_9 element_8 element_7 element_6 element_5 (*addr+5) -1
                add      // _ length first_time *addr element_9 element_8 element_7 element_6 element_5 (*addr+4)
                read_mem // _ length first_time *addr element_9 element_8 element_7 element_6 element_5 (*addr+4) element_4
                swap 1   // _ length first_time *addr element_9 element_8 element_7 element_6 element_5 element_4 (*addr+4)
                push -1  // _ length first_time *addr element_9 element_8 element_7 element_6 element_5 element_4 (*addr+4) -1
                add      // _ length first_time *addr element_9 element_8 element_7 element_6 element_5 element_4 (*addr+3)
                read_mem // _ length first_time *addr element_9 element_8 element_7 element_6 element_5 element_4 (*addr+3) element_3
                swap 1   // _ length first_time *addr element_9 element_8 element_7 element_6 element_5 element_4 element_3 (*addr+3)
                push -1  // _ length first_time *addr element_9 element_8 element_7 element_6 element_5 element_4 element_3 (*addr+3) -1
                add      // _ length first_time *addr element_9 element_8 element_7 element_6 element_5 element_4 element_3 (*addr+2)
                read_mem // _ length first_time *addr element_9 element_8 element_7 element_6 element_5 element_4 element_3 (*addr+2) element_2
                swap 1   // _ length first_time *addr element_9 element_8 element_7 element_6 element_5 element_4 element_3 element_2 (*addr+2)
                push -1  // _ length first_time *addr element_9 element_8 element_7 element_6 element_5 element_4 element_3 element_2 (*addr+2) -1
                add      // _ length first_time *addr element_9 element_8 element_7 element_6 element_5 element_4 element_3 element_2 (*addr+1)
                read_mem // _ length first_time *addr element_9 element_8 element_7 element_6 element_5 element_4 element_3 element_2 (*addr+1) element_1
                swap 1   // _ length first_time *addr element_9 element_8 element_7 element_6 element_5 element_4 element_3 element_2 element_1 (*addr+1)
                push -1  // _ length first_time *addr element_9 element_8 element_7 element_6 element_5 element_4 element_3 element_2 element_1 (*addr+1) -1
                add      // _ length first_time *addr element_9 element_8 element_7 element_6 element_5 element_4 element_3 element_2 element_1 *addr
                read_mem // _ length first_time *addr element_9 element_8 element_7 element_6 element_5 element_4 element_3 element_2 element_1 *addr element_0
                swap 1   // _ length first_time *addr element_9 element_8 element_7 element_6 element_5 element_4 element_3 element_2 element_1 element_0 *addr
                pop      // _ length first_time *addr element_9 element_8 element_7 element_6 element_5 element_4 element_3 element_2 element_1 element_0

                dup 0    // _ length first_time *addr element_9 element_8 element_7 element_6 element_5 element_4 element_3 element_2 element_1 element_0 element_0
                swap 12  // _ length element_0 *addr element_9 element_8 element_7 element_6 element_5 element_4 element_3 element_2 element_1 element_0 first_time
                push 1   // _ length element_0 *addr element_9 element_8 element_7 element_6 element_5 element_4 element_3 element_2 element_1 element_0 first_time 1
                swap 1   // _ length element_0 *addr element_9 element_8 element_7 element_6 element_5 element_4 element_3 element_2 element_1 element_0 1 first_time
                skiz
                    call {entrypoint}_if_branch
                skiz
                    absorb

                // _ length element_0 *addr element_9 element_8 element_7 element_6 element_5 element_4 element_3 element_2 element_1 element_0

                pop pop pop pop pop pop pop pop pop pop
                // _ length element_0 *addr

                swap 1   // _ length *addr element_0
                pop      // _ length *addr

                push 10  // _ length *addr 10
                add      // _ length (*addr+10)
                swap 1   // _ (*addr+10) length
                push -10 // _ (*addr+10) length -10
                add      // _ (*addr+10) (length-10)
                push 0   // _ (*addr+10) (length-10) 0

                recurse

                // BEFORE: // _ length element_0 *addr element_9 element_8 element_7 element_6 element_5 element_4 element_3 element_2 element_1 element_0 1
                // AFTER:  // _ length element_0 *addr element_9 element_8 element_7 element_6 element_5 element_4 element_3 element_2 element_1 element_0 0
                {entrypoint}_if_branch:
                    pop         // _ length element_0 *addr element_9 element_8 element_7 element_6 element_5 element_4 element_3 element_2 element_1 element_0
                    absorb_init // _ length element_0 *addr element_9 element_8 element_7 element_6 element_5 element_4 element_3 element_2 element_1 element_0
                    push 0
                    return
                "
        )
    }

    fn crash_conditions() -> Vec<String>
    where
        Self: Sized,
    {
        // TODO: Add crash conditions, if any
        vec!["Length exceeds u32::MAX".to_string()]
    }

    fn gen_input_states(&self) -> Vec<crate::ExecutionState>
    where
        Self: Sized,
    {
        let empty_memory_state_read_0 = ExecutionState {
            stack: vec![
                get_init_tvm_stack(),
                vec![BFieldElement::one(), BFieldElement::new(0)],
            ]
            .concat(),
            std_in: vec![],
            secret_in: vec![],
            memory: HashMap::new(),
            words_allocated: 1,
        };
        let empty_memory_state_read_10 = ExecutionState {
            stack: vec![
                get_init_tvm_stack(),
                vec![BFieldElement::one(), BFieldElement::new(10)],
            ]
            .concat(),
            std_in: vec![],
            secret_in: vec![],
            memory: HashMap::new(),
            words_allocated: 1,
        };
        let empty_memory_state_read_20 = ExecutionState {
            stack: vec![
                get_init_tvm_stack(),
                vec![BFieldElement::one(), BFieldElement::new(20)],
            ]
            .concat(),
            std_in: vec![],
            secret_in: vec![],
            memory: HashMap::new(),
            words_allocated: 1,
        };
        let empty_memory_state_read_30 = ExecutionState {
            stack: vec![
                get_init_tvm_stack(),
                vec![BFieldElement::one(), BFieldElement::new(30)],
            ]
            .concat(),
            std_in: vec![],
            secret_in: vec![],
            memory: HashMap::new(),
            words_allocated: 1,
        };

        vec![
            empty_memory_state_read_0,
            empty_memory_state_read_10,
            empty_memory_state_read_20,
            empty_memory_state_read_30,
        ]
    }

    fn common_case_input_state(&self) -> crate::ExecutionState
    where
        Self: Sized,
    {
        todo!()
    }

    fn worst_case_input_state(&self) -> crate::ExecutionState
    where
        Self: Sized,
    {
        todo!()
    }

    fn rust_shadowing(
        &self,
        stack: &mut Vec<triton_vm::BFieldElement>,
        _std_in: Vec<triton_vm::BFieldElement>,
        _secret_in: Vec<triton_vm::BFieldElement>,
        memory: &mut std::collections::HashMap<triton_vm::BFieldElement, triton_vm::BFieldElement>,
    ) where
        Self: Sized,
    {
        let length: u32 = stack.pop().unwrap().try_into().unwrap();
        let memory_pointer: BFieldElement = stack.pop().unwrap();

        let mut preimage = vec![];
        for i in 0..length as u64 {
            preimage.push(
                memory
                    .get(&(memory_pointer + BFieldElement::new(i)))
                    .unwrap_or(&BFieldElement::zero())
                    .to_owned(),
            );
        }

        let digest = VmHasher::hash_varlen(&preimage);
        stack.append(&mut digest.reversed().values().to_vec());
    }
}

#[cfg(test)]
mod tests {
    use crate::test_helpers::rust_tasm_equivalence_prop_new;

    use super::*;

    #[test]
    fn new_prop_test() {
        rust_tasm_equivalence_prop_new(HashVarlen);
    }
}
