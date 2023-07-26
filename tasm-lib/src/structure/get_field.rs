use std::collections::HashMap;

use itertools::Itertools;
use num_traits::One;
use rand::{random, rngs::StdRng, Rng, SeedableRng};
use triton_vm::BFieldElement;

use crate::{
    get_init_tvm_stack,
    snippet::{DataType, Snippet},
    ExecutionState,
};

/// Returns a pointer to the current struct's nth field.
pub struct GetField;

impl Snippet for GetField {
    fn entrypoint(&self) -> String {
        "tasm_structure_get_field".to_string()
    }

    fn inputs(&self) -> Vec<String> {
        vec!["*struct".to_string(), "field_index".to_string()]
    }

    fn input_types(&self) -> Vec<crate::snippet::DataType> {
        vec![DataType::VoidPointer, DataType::U32]
    }

    fn output_types(&self) -> Vec<crate::snippet::DataType> {
        vec![DataType::VoidPointer]
    }

    fn outputs(&self) -> Vec<String> {
        vec!["*field".to_string()]
    }

    fn stack_diff(&self) -> isize {
        -1
    }

    fn function_code(&self, _library: &mut crate::library::Library) -> String {
        let entrypoint = self.entrypoint();

        format!(
            "
        // BEFORE: _ *struct field_index
        // AFTER: _ *field
        {entrypoint}:
            call {entrypoint}_loop
            pop
            return

        // INVARIANT: _ *field index
        {entrypoint}_loop:
            dup 0 // _ *field field_index field_index
            push 0 eq // _ *field field_index field_index==0
            skiz return // _ *field field_index

            push -1 add // _ *field field_index-1
            swap 1 // _ field_index-1 *field
            read_mem // _ field_index-1 *field field_size
            push 1 add add // _ field_index-1 *field+field_size+1
            swap 1 // _ *field+field_size+1 field_index-1
            recurse
        "
        )
    }

    fn crash_conditions(&self) -> Vec<String> {
        vec!["memory improperly formatted; does not correspond to encoding of a struct".to_string()]
    }

    fn gen_input_states(&self) -> Vec<crate::ExecutionState> {
        let num_fields = 10;
        let structure = &random_struct(num_fields);
        (0..num_fields)
            .map(|index| init_state_field_i_in_struct(random(), index, structure))
            .collect_vec()
    }

    fn common_case_input_state(&self) -> ExecutionState {
        let mut seed = [0u8; 32];
        seed[0] = 0xfa;
        seed[1] = 0xfa;
        seed[2] = 0x12;
        seed[3] = 0x01;
        init_state_field_i_in_struct(1, 5, &pseudorandom_struct(5, seed))
    }

    fn worst_case_input_state(&self) -> ExecutionState {
        let mut seed = [0u8; 32];
        seed[0] = 0xfa;
        seed[1] = 0xda;
        seed[2] = 0x12;
        seed[3] = 0xa1;
        init_state_field_i_in_struct(1, 5, &pseudorandom_struct(20, seed))
    }

    fn rust_shadowing(
        &self,
        stack: &mut Vec<triton_vm::BFieldElement>,
        _std_in: Vec<triton_vm::BFieldElement>,
        _secret_in: Vec<triton_vm::BFieldElement>,
        memory: &mut std::collections::HashMap<triton_vm::BFieldElement, triton_vm::BFieldElement>,
    ) {
        let mut field_index = stack.pop().unwrap().value() as usize;
        let mut address = stack.pop().unwrap();

        while field_index != 0 {
            address += BFieldElement::one() + *memory.get(&address).unwrap();
            field_index -= 1;
        }

        stack.push(address);
    }
}

pub fn random_struct(num_fields: usize) -> Vec<BFieldElement> {
    pseudorandom_struct(num_fields, random())
}

pub fn pseudorandom_struct(num_fields: usize, seed: [u8; 32]) -> Vec<BFieldElement> {
    let mut rng: StdRng = SeedableRng::from_seed(seed);
    let mut encoded = vec![];
    for _ in 0..num_fields {
        let length = rng.gen_range(0..10);
        let mut field: Vec<BFieldElement> = (0..length).map(|_| rng.gen()).collect_vec();
        encoded.push(BFieldElement::new(length as u64));
        encoded.append(&mut field);
    }
    encoded
}

pub fn init_state_field_i_in_struct(
    address: u32,
    index: usize,
    structure: &[BFieldElement],
) -> ExecutionState {
    let mut memory: HashMap<BFieldElement, BFieldElement> = HashMap::new();
    let address = BFieldElement::new(address as u64);
    for (i, s) in structure.iter().enumerate() {
        memory.insert(address + BFieldElement::new(i as u64), *s);
    }
    let mut stack = get_init_tvm_stack();
    stack.push(address);
    stack.push(BFieldElement::new(index as u64));

    ExecutionState {
        stack,
        std_in: vec![],
        secret_in: vec![],
        memory,
        words_allocated: 0,
    }
}

#[cfg(test)]
mod tests {
    use crate::test_helpers::test_rust_equivalence_multiple;

    use super::GetField;

    #[test]
    fn new_prop_test() {
        test_rust_equivalence_multiple(&GetField, true);
    }
}

#[cfg(test)]
mod benches {
    use super::*;
    use crate::snippet_bencher::bench_and_write;

    #[test]
    fn get_transaction_kernel_field_size_benchmark() {
        bench_and_write(GetField);
    }
}
