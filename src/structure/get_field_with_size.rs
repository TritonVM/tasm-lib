use itertools::Itertools;
use num_traits::One;
use rand::random;
use triton_vm::BFieldElement;

use crate::{
    snippet::{DataType, Snippet},
    ExecutionState,
};

use super::get_field::{init_state_field_i_in_struct, pseudorandom_struct, random_struct};

/// Returns a pointer to the current struct's nth field.
pub struct GetFieldWithSize;

impl Snippet for GetFieldWithSize {
    fn entrypoint(&self) -> String {
        "tasm_structure_get_field_with_size".to_string()
    }

    fn inputs(&self) -> Vec<String> {
        vec!["*struct".to_string(), "field_index".to_string()]
    }

    fn input_types(&self) -> Vec<crate::snippet::DataType> {
        vec![DataType::VoidPointer, DataType::U32]
    }

    fn output_types(&self) -> Vec<crate::snippet::DataType> {
        vec![DataType::VoidPointer, DataType::U32]
    }

    fn outputs(&self) -> Vec<String> {
        vec!["*field".to_string(), "field_size".to_string()]
    }

    fn stack_diff(&self) -> isize {
        0
    }

    fn function_code(&self, _library: &mut crate::snippet_state::SnippetState) -> String {
        let entrypoint = self.entrypoint();

        format!(
            "
        // BEFORE: _ *field-1 field_index
        // AFTER: _ *field field_size
        {entrypoint}:
            call {entrypoint}_loop
            pop
            read_mem
            swap 1
            push 1 add
            swap 1
            return

        // INVARIANT: _ *field-1 index
        {entrypoint}_loop:
            dup 0 // _ *field-1 field_index field_index
            push 0 eq // _ *field-1 field_index field_index==0
            skiz return // _ *field-1 field_index
            push -1 add // _ *field-1 field_index-1
            swap 1 // _ field_index-1 *field-1
            read_mem // _ field_index-1 *field-1 field_size
            push 1 add add // _ field_index-1 *field+field_size
            swap 1 // _ *field+field_size field_index-1
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

        let mut field_size = *memory.get(&address).unwrap();
        while field_index != 0 {
            address += BFieldElement::one() + field_size;
            field_index -= 1;
            field_size = *memory.get(&address).unwrap();
        }

        stack.push(address + BFieldElement::one());
        stack.push(field_size);
    }
}

#[cfg(test)]
mod tests {
    use crate::test_helpers::rust_tasm_equivalence_prop_new;

    use super::GetFieldWithSize;

    #[test]
    fn new_prop_test() {
        rust_tasm_equivalence_prop_new(&GetFieldWithSize, true);
    }
}

#[cfg(test)]
mod benches {
    use super::*;
    use crate::snippet_bencher::bench_and_write;

    #[test]
    fn get_transaction_kernel_field_size_benchmark() {
        bench_and_write(GetFieldWithSize);
    }
}
