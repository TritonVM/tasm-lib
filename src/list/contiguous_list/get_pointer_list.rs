use crate::{
    list::{self, contiguous_list, ListType},
    snippet::{DataType, Snippet},
};

pub struct GetPointerList {
    output_list_type: ListType,
}

impl Snippet for GetPointerList {
    fn entrypoint(&self) -> String {
        format!(
            "tasm_list_contiguous_list_get_pointer_list_{}",
            self.output_list_type
        )
    }

    fn inputs(&self) -> Vec<String> {
        vec!["*contiguous_list".to_owned()]
    }

    fn input_types(&self) -> Vec<crate::snippet::DataType> {
        vec![DataType::VoidPointer]
    }

    fn output_types(&self) -> Vec<crate::snippet::DataType> {
        vec![DataType::List(Box::new(DataType::VoidPointer))]
    }

    fn outputs(&self) -> Vec<String> {
        vec!["*list_of_pointers".to_owned()]
    }

    fn stack_diff(&self) -> isize {
        0
    }

    fn function_code(&self, library: &mut crate::snippet_state::SnippetState) -> String {
        let entrypoint = self.entrypoint();
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
                // _ *first_element list_length *list_of_pointers index

                call {entrypoint}_loop
                // _ *element list_length *list_of_pointers index

                pop
                swap 2
                pop
                pop
                // _ *list_of_pointers

                return

            // Invariant: _ *element list_length *list_of_pointers index
            {entrypoint}_loop:

                // check terminal condition
                dup 2
                dup 1
                eq
                skiz return

                // _ *element list_length *list_of_pointers index

                swap 3
                // _ index list_length *list_of_pointers *element

                dup 0 dup 2 dup 5
                // _ index list_length *list_of_pointers *element *element *list_of_pointers index

                call {set_element}
                // _ index list_length *list_of_pointers *element

                read_mem
                // _ index list_length *list_of_pointers *element element_size

                push 1 add

                add
                // _ index list_length *list_of_pointers *next_element

                swap 3
                // _ *next_element list_length *list_of_pointers index

                push 1 add
                // _ *next_element list_length *list_of_pointers (index + 1)

                recurse
        "
        )
    }

    fn crash_conditions(&self) -> Vec<String> {
        vec!["Memory outside of first 2^32 words is accessed".to_owned()]
    }

    fn gen_input_states(&self) -> Vec<crate::ExecutionState> {
        todo!()
    }

    fn common_case_input_state(&self) -> crate::ExecutionState {
        todo!()
    }

    fn worst_case_input_state(&self) -> crate::ExecutionState {
        todo!()
    }

    fn rust_shadowing(
        &self,
        stack: &mut Vec<triton_vm::BFieldElement>,
        std_in: Vec<triton_vm::BFieldElement>,
        secret_in: Vec<triton_vm::BFieldElement>,
        memory: &mut std::collections::HashMap<triton_vm::BFieldElement, triton_vm::BFieldElement>,
    ) {
        let contiguous_list_pointer = stack.pop().unwrap();

        let cl_length =
            contiguous_list::get_length::GetLength.rust_shadowing(stack, std_in, secret_in, memory);

        todo!()
    }
}

#[cfg(test)]
mod tests {
    use crate::test_helpers::test_rust_equivalence_multiple;

    use super::*;

    #[test]
    fn get_pointer_list_test() {
        test_rust_equivalence_multiple(
            &GetPointerList {
                output_list_type: ListType::Safe,
            },
            true,
        );
        test_rust_equivalence_multiple(
            &GetPointerList {
                output_list_type: ListType::Unsafe,
            },
            true,
        );
    }
}
