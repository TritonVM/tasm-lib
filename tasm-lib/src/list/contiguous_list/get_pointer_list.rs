use num_traits::One;
use rand::prelude::*;
use triton_vm::prelude::*;

use crate::list::contiguous_list::get_length::DummyOuterDataStructure;
use crate::list::contiguous_list::get_length::GetLength;
use crate::list::new::New;
use crate::list::set::Set;
use crate::list::set_length::SetLength;
use crate::memory::dyn_malloc;
use crate::prelude::*;
use crate::rust_shadowing_helper_functions;
use crate::traits::deprecated_snippet::DeprecatedSnippet;

/// All of `contiguous_list` assumes that each element has its length prepended
#[derive(Debug, Default, Copy, Clone, Eq, PartialEq, Hash)]
pub struct GetPointerList;

impl DeprecatedSnippet for GetPointerList {
    fn entrypoint_name(&self) -> String {
        "tasmlib_list_contiguous_list_get_pointer_list".into()
    }

    fn input_field_names(&self) -> Vec<String> {
        vec!["*contiguous_list".to_owned()]
    }

    fn input_types(&self) -> Vec<DataType> {
        vec![DataType::VoidPointer]
    }

    fn output_field_names(&self) -> Vec<String> {
        vec!["*list_of_pointers".to_owned()]
    }

    fn output_types(&self) -> Vec<DataType> {
        vec![DataType::List(Box::new(DataType::VoidPointer))]
    }

    fn stack_diff(&self) -> isize {
        0
    }

    fn function_code(&self, library: &mut Library) -> String {
        let entrypoint = self.entrypoint_name();

        let data_type = DataType::VoidPointer;
        let get_list_length = library.import(Box::new(GetLength));
        let new_list = library.import(Box::new(New::new(data_type.clone())));
        let set_length = library.import(Box::new(SetLength::new(data_type.clone())));
        let set_element = library.import(Box::new(Set::new(data_type)));

        format!(
            "
            // BEFORE: _ *contiguous_list
            // AFTER:  _ *list_of_pointers
            {entrypoint}:
                dup 0
                // _ *cl *cl

                call {get_list_length}
                // _ *cl (list_length = capacity)

                call {new_list}
                // _ *cl list_length *list_of_pointers

                dup 1
                // _ *cl list_length *list_of_pointers list_length

                call {set_length}
                // _ *cl list_length *list_of_pointers

                swap 2
                push 1
                add
                swap 2
                // _ (*cl + 1) list_length *list_of_pointers

                push 0
                // _ *element_size list_length *list_of_pointers index

                call {entrypoint}_loop
                // _ *element_size list_length *list_of_pointers index

                pop 1
                swap 2
                pop 2
                // _ *list_of_pointers

                return

            // Invariant: _ *element_size list_length *list_of_pointers index
            {entrypoint}_loop:

                // check terminal condition
                dup 2
                dup 1
                eq
                skiz return

                // _ *element_size list_length *list_of_pointers index

                swap 3
                // _ index list_length *list_of_pointers *element_size

                dup 0 push 1 add dup 2 dup 5
                // _ index list_length *list_of_pointers *element_size *element *list_of_pointers index

                call {set_element}
                // _ index list_length *list_of_pointers *element_size

                read_mem 1
                // _ index list_length *list_of_pointers (*element_size - 1) element_size

                push 2 add

                add
                // _ index list_length *list_of_pointers *next_element_size

                swap 3
                // _ *next_element_size list_length *list_of_pointers index

                push 1 add
                // _ *next_element_size list_length *list_of_pointers (index + 1)

                recurse
        "
        )
    }

    fn crash_conditions(&self) -> Vec<String> {
        vec!["Memory outside of first 2^32 words is accessed".to_owned()]
    }

    fn gen_input_states(&self) -> Vec<crate::InitVmState> {
        let mut rng = thread_rng();
        vec![
            GetLength::pseudorandom_input_state(rng.gen(), 0),
            GetLength::pseudorandom_input_state(rng.gen(), 1),
            GetLength::pseudorandom_input_state(rng.gen(), 3),
            GetLength::pseudorandom_input_state(rng.gen(), 4),
            GetLength::pseudorandom_input_state(rng.gen(), 5),
            GetLength::pseudorandom_input_state(rng.gen(), 10),
        ]
    }

    fn common_case_input_state(&self) -> crate::InitVmState {
        let mut seed = [0u8; 32];
        seed[0] = 0x01;
        seed[1] = 0xdd;
        GetLength::pseudorandom_input_state(seed, 2)
    }

    fn worst_case_input_state(&self) -> crate::InitVmState {
        let mut seed = [0u8; 32];
        seed[0] = 0xa1;
        seed[1] = 0xde;
        GetLength::pseudorandom_input_state(seed, 5)
    }

    fn rust_shadowing(
        &self,
        stack: &mut Vec<BFieldElement>,
        std_in: Vec<BFieldElement>,
        secret_in: Vec<BFieldElement>,
        memory: &mut std::collections::HashMap<BFieldElement, BFieldElement>,
    ) {
        // read address
        let mut address = stack.last().unwrap().to_owned();
        assert!(
            address.value() < (1u64 << 32),
            "Sanity check: Address was outside of expected memory range. Got: {address}"
        );
        let size = memory
            .get(&(address - BFieldElement::new(1)))
            .unwrap()
            .value();

        // read length
        GetLength.rust_shadowing(stack, std_in, secret_in, memory);
        let length = stack.pop().unwrap().value() as usize;

        // read object
        let mut encoding = vec![];
        for i in 0..size {
            encoding.push(
                memory
                    .get(&(address + BFieldElement::new(i)))
                    .unwrap()
                    .to_owned(),
            );
        }

        // decode object
        let dummy_list: Vec<DummyOuterDataStructure> =
            *Vec::<DummyOuterDataStructure>::decode(&encoding).unwrap();
        assert_eq!(dummy_list.len(), length);

        // create list
        let output_list_pointer = dyn_malloc::DYN_MALLOC_FIRST_ADDRESS;
        rust_shadowing_helper_functions::list::list_new(output_list_pointer, memory);

        // populate list
        address.increment();
        for d in dummy_list.into_iter() {
            rust_shadowing_helper_functions::list::list_push(
                output_list_pointer,
                vec![address + BFieldElement::one()],
                memory,
                1,
            );
            let size_indicator = *memory.get(&address).unwrap();

            assert_eq!(size_indicator.value() as usize, d.encode().len());
            address += size_indicator + BFieldElement::one();
        }

        stack.push(output_list_pointer);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_helpers::test_rust_equivalence_multiple_deprecated;

    #[test]
    fn get_pointer_list_test() {
        let output_states = test_rust_equivalence_multiple_deprecated(&GetPointerList, true);

        // Verify that the pointers in the list actually point to correctly encoded objects.
        for output_state in output_states {
            let mut stack = output_state.op_stack.stack;
            let memory = output_state.ram;
            let output_list_pointer = stack.pop().unwrap();
            let num_elements = rust_shadowing_helper_functions::list::list_get_length(
                output_list_pointer,
                &memory,
            );
            if num_elements < 2 {
                continue;
            }

            let start =
                rust_shadowing_helper_functions::list::list_get(output_list_pointer, 0, &memory, 1)
                    [0];
            let stop =
                rust_shadowing_helper_functions::list::list_get(output_list_pointer, 1, &memory, 1)
                    [0]
                    - BFieldElement::one();

            let mut addr = start;
            let mut encoding = vec![];
            while addr != stop {
                encoding.push(*memory.get(&addr).unwrap());
                addr.increment();
            }

            let decoded_object = *DummyOuterDataStructure::decode(&encoding).unwrap();
            let reencoded_object = decoded_object.encode();
            assert_eq!(reencoded_object, encoding);
        }
    }
}

#[cfg(test)]
mod benches {
    use super::GetPointerList;
    use crate::snippet_bencher::bench_and_write;

    #[test]
    fn get_pointer_list_benchmark() {
        bench_and_write(GetPointerList);
    }
}
