use num_traits::One;
use rand::{thread_rng, Rng};
use triton_vm::BFieldElement;
use twenty_first::shared_math::bfield_codec::BFieldCodec;

use crate::{
    list::{
        self,
        contiguous_list::{
            self,
            get_length::{DummyOuterDataStructure, GetLength},
        },
        ListType,
    },
    rust_shadowing_helper_functions,
    snippet::{DataType, DeprecatedSnippet},
};

// All of `contiguous_list` assumes that each element has its length prepended
pub struct GetPointerList {
    pub output_list_type: ListType,
}

impl DeprecatedSnippet for GetPointerList {
    fn entrypoint_name(&self) -> String {
        format!(
            "tasm_list_contiguous_list_get_pointer_list_{}",
            self.output_list_type
        )
    }

    fn input_field_names(&self) -> Vec<String> {
        vec!["*contiguous_list".to_owned()]
    }

    fn input_types(&self) -> Vec<crate::snippet::DataType> {
        vec![DataType::VoidPointer]
    }

    fn output_types(&self) -> Vec<crate::snippet::DataType> {
        vec![DataType::List(Box::new(DataType::VoidPointer))]
    }

    fn output_field_names(&self) -> Vec<String> {
        vec!["*list_of_pointers".to_owned()]
    }

    fn stack_diff(&self) -> isize {
        0
    }

    fn function_code(&self, library: &mut crate::library::Library) -> String {
        let entrypoint = self.entrypoint_name();
        let get_list_length = library.import(Box::new(contiguous_list::get_length::GetLength));
        let new_list = match self.output_list_type {
            ListType::Safe => library.import(Box::new(list::safe_u32::new::SafeNew(
                DataType::VoidPointer,
            ))),
            ListType::Unsafe => library.import(Box::new(list::unsafe_u32::new::UnsafeNew(
                DataType::VoidPointer,
            ))),
        };

        let set_length = match self.output_list_type {
            ListType::Safe => library.import(Box::new(list::safe_u32::set_length::SafeSetLength(
                DataType::VoidPointer,
            ))),
            ListType::Unsafe => library.import(Box::new(
                list::unsafe_u32::set_length::UnsafeSetLength(DataType::VoidPointer),
            )),
        };
        let set_element = match self.output_list_type {
            ListType::Safe => library.import(Box::new(list::safe_u32::set::SafeSet(
                DataType::VoidPointer,
            ))),
            ListType::Unsafe => library.import(Box::new(list::unsafe_u32::set::UnsafeSet(
                DataType::VoidPointer,
            ))),
        };

        format!(
            "
            // BEFORE: _ *contiguous_list
            // AFTER: _ *list_of_pointers
            {entrypoint}:
                dup 0
                // _ *cl *cl

                call {get_list_length}
                // _ *cl (list_length = capacity)

                dup 0
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

                pop
                swap 2
                pop
                pop
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

                read_mem
                // _ index list_length *list_of_pointers *element_size element_size

                push 1 add

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

    fn gen_input_states(&self) -> Vec<crate::ExecutionState> {
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

    fn common_case_input_state(&self) -> crate::ExecutionState {
        let mut seed = [0u8; 32];
        seed[0] = 0x01;
        seed[1] = 0xdd;
        GetLength::pseudorandom_input_state(seed, 2)
    }

    fn worst_case_input_state(&self) -> crate::ExecutionState {
        let mut seed = [0u8; 32];
        seed[0] = 0xa1;
        seed[1] = 0xde;
        GetLength::pseudorandom_input_state(seed, 5)
    }

    fn rust_shadowing(
        &self,
        stack: &mut Vec<triton_vm::BFieldElement>,
        std_in: Vec<triton_vm::BFieldElement>,
        secret_in: Vec<triton_vm::BFieldElement>,
        memory: &mut std::collections::HashMap<triton_vm::BFieldElement, triton_vm::BFieldElement>,
    ) {
        // read address
        let mut address = stack.last().unwrap().to_owned();
        let size = memory
            .get(&(address - BFieldElement::new(1)))
            .unwrap()
            .value();

        // read length
        contiguous_list::get_length::GetLength.rust_shadowing(stack, std_in, secret_in, memory);
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
        let output_list_pointer =
            rust_shadowing_helper_functions::dyn_malloc::dynamic_allocator(length, memory);
        match self.output_list_type {
            ListType::Safe => rust_shadowing_helper_functions::safe_list::safe_list_new(
                output_list_pointer,
                length as u32,
                memory,
            ),
            ListType::Unsafe => rust_shadowing_helper_functions::unsafe_list::unsafe_list_new(
                output_list_pointer,
                memory,
            ),
        };

        // populate list
        let list_push = match self.output_list_type {
            ListType::Safe => rust_shadowing_helper_functions::safe_list::safe_list_push,
            ListType::Unsafe => rust_shadowing_helper_functions::unsafe_list::unsafe_list_push,
        };
        address.increment();
        for d in dummy_list.into_iter() {
            list_push(
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

    use crate::test_helpers::test_rust_equivalence_multiple_deprecated;

    use super::*;

    #[test]
    fn get_pointer_list_safe_test() {
        test_rust_equivalence_multiple_deprecated(
            &GetPointerList {
                output_list_type: ListType::Safe,
            },
            true,
        );
    }

    #[test]
    fn get_pointer_list_unsafe_test() {
        let output_states = test_rust_equivalence_multiple_deprecated(
            &GetPointerList {
                output_list_type: ListType::Unsafe,
            },
            true,
        );

        // Verify that the pointers in the list actually point to correctly encoded objects.
        for output_state in output_states {
            let mut stack = output_state.final_stack;
            let memory = output_state.final_ram;
            let output_list_pointer = stack.pop().unwrap();
            let num_elements = rust_shadowing_helper_functions::unsafe_list::unsafe_list_get_length(
                output_list_pointer,
                &memory,
            );
            if num_elements < 2 {
                continue;
            }

            let start = rust_shadowing_helper_functions::unsafe_list::unsafe_list_get(
                output_list_pointer,
                0,
                &memory,
                1,
            )[0];
            let stop = rust_shadowing_helper_functions::unsafe_list::unsafe_list_get(
                output_list_pointer,
                1,
                &memory,
                1,
            )[0] - BFieldElement::one();

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
    use crate::{list::ListType, snippet_bencher::bench_and_write};

    use super::GetPointerList;

    #[test]
    fn get_pointer_list_unsafe_benchmark() {
        bench_and_write(GetPointerList {
            output_list_type: ListType::Unsafe,
        });
    }

    #[test]
    fn get_pointer_list_safe_benchmark() {
        bench_and_write(GetPointerList {
            output_list_type: ListType::Safe,
        });
    }
}
