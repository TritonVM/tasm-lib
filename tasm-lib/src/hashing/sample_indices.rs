use std::collections::HashMap;

use itertools::Itertools;
use triton_vm::NonDeterminism;
use twenty_first::{
    shared_math::{
        b_field_element::BFieldElement,
        other::is_power_of_two,
        tip5::{Tip5, Tip5State},
    },
    util_types::algebraic_hasher::{AlgebraicHasher, Domain, SpongeHasher},
};

use crate::{
    get_init_tvm_stack,
    list::{
        self,
        safe_u32::{new::SafeNew, set_length::SafeSetLength},
        unsafe_u32::{new::UnsafeNew, set_length::UnsafeSetLength},
        ListType,
    },
    rust_shadowing_helper_functions,
    snippet::{DataType, DepracatedSnippet},
    ExecutionState, VmHasher,
};

#[derive(Clone, Debug)]
pub struct SampleIndices {
    pub list_type: ListType,
}

/// SampleIndices is a snippet that samples n pseudorandom integers
/// between 0 and k. It does this by squeezing the sponge. It is the
/// caller's responsibility to ensure that the sponge is initialized
/// to the right state.
impl SampleIndices {
    fn test_state(number: usize, upper_bound: u32) -> ExecutionState {
        ExecutionState {
            stack: vec![
                get_init_tvm_stack(),
                vec![
                    BFieldElement::new(number as u64),
                    BFieldElement::new(upper_bound as u64),
                ],
            ]
            .concat(),
            std_in: vec![],
            nondeterminism: NonDeterminism::new(vec![]),
            memory: HashMap::new(),
            words_allocated: 1,
        }
    }
}

impl DepracatedSnippet for SampleIndices {
    fn entrypoint_name(&self) -> String {
        format!("tasm_hashing_sample_indices_to_{}_list", self.list_type)
    }

    fn input_field_names(&self) -> Vec<String>
    where
        Self: Sized,
    {
        vec!["number".to_string(), "upper_bound".to_string()]
    }

    fn input_types(&self) -> Vec<crate::snippet::DataType> {
        vec![DataType::U32, DataType::U32]
    }

    fn output_types(&self) -> Vec<crate::snippet::DataType> {
        vec![DataType::List(Box::new(DataType::U32))]
    }

    fn output_field_names(&self) -> Vec<String>
    where
        Self: Sized,
    {
        vec!["index_list".to_string()]
    }

    fn stack_diff(&self) -> isize
    where
        Self: Sized,
    {
        -1
    }

    fn function_code(&self, library: &mut crate::library::Library) -> String {
        let entrypoint = self.entrypoint_name();
        let new_list = match self.list_type {
            ListType::Safe => library.import(Box::new(SafeNew(DataType::U32))),
            ListType::Unsafe => library.import(Box::new(UnsafeNew(DataType::U32))),
        };
        let set_length = match self.list_type {
            ListType::Safe => library.import(Box::new(SafeSetLength(DataType::U32))),
            ListType::Unsafe => library.import(Box::new(UnsafeSetLength(DataType::U32))),
        };
        let safety_offset = match self.list_type {
            ListType::Safe => 2,
            ListType::Unsafe => 1,
        };
        let minus_safety_offset = match self.list_type {
            ListType::Safe => -2,
            ListType::Unsafe => -1,
        };

        let process_top = format!("
                // _ number upper_bound-1 address list_index prn^10
                dup 10 // _ number upper_bound-1 address list_index prn^10 list_index
                dup 14 // _ number upper_bound-1 address list_index prn^10 list_index number
                eq // _ number upper_bound-1 address list_index prn^10 list_index==number
                dup 1  // _ number upper_bound-1 address list_index prn^10 list_index==number prn0
                push -1 eq  // _ number upper_bound-1 address list_index prn^10 list_index==number prn0==-1
                add  // _ number upper_bound-1 address list_index prn^10 list_index==number||prn0==-1
                push 0 eq // _ number upper_bound-1 address list_index prn^10 list_index!=number&&prn0!=-1
                skiz call {entrypoint}_process_top_function_body
                // _ number upper_bound-1 address list_index prn^10
        ");

        let rotate_10 = "
            swap 9
            swap 8
            swap 7
            swap 6
            swap 5
            swap 4
            swap 3
            swap 2
            swap 1"
            .to_string();

        format!(
            "
            // BEFORE: _ number upper_bound
            // AFTER: _ list
            {entrypoint}:
                // assert power of two
                dup 0 dup 0 // _ number upper_bound upper_bound upper_bound
                push -1 add and // _ number upper_bound upper_bound&(upper_bound-1)
                push 0 eq assert // asserts that upper_bound = 2^k for some k
                push -1 add // _ number upper_bound-1

                // create list
                dup 1 // _ number upper_bound-1 number
                call {new_list} // _ number upper_bound-1 list
                dup 2 //  _ number upper_bound-1 list number
                call {set_length} // _ number upper_bound-1 list
                push {safety_offset} add // _ number upper_bound-1 address

                // prepare and call loop
                push 0 // _ number upper_bound-1 address 0
                push 0 push 0 push 0 push 0 push 0 push 0 push 0 push 0 push 0 push 0
                // _ number upper_bound-1 address list_index 0 0 0 0 0 0 0 0 0 0

                squeeze // overwrite top 10 elements with fresh randomness
                call {entrypoint}_loop // _ number upper_bound-1 address number prn9 prn8 prn7 prn6 prn5 prn4 prn3 prn2 prn1 prn0

                // clean up stack
                pop pop pop pop pop pop pop pop pop pop // _ number upper_bound-1 address number
                pop // _ number upper_bound-1 address
                swap 2  // _ address  upper_bound-1 number
                pop pop // _ address
                push {minus_safety_offset} add // _ list

                return

            // INVARIANT: _ number upper_bound-1 list list_index prn9 prn8 prn7 prn6 prn5 prn4 prn3 prn2 prn1 prn0
            {entrypoint}_loop:
                // evaluate termination 
                dup 13 // _ number upper_bound-1 address list_index prn9 prn8 prn7 prn6 prn5 prn4 prn3 prn2 prn1 prn0 number
                dup 11 // _ number upper_bound-1 address list_index prn9 prn8 prn7 prn6 prn5 prn4 prn3 prn2 prn1 prn0 number list_index
                eq // _ number upper_bound-1 address list_index prn9 prn8 prn7 prn6 prn5 prn4 prn3 prn2 prn1 prn0 number==list_index

                skiz return // continue if unequal
                // _ number upper_bound-1 address list_index  prn9 prn8 prn7 prn6 prn5 prn4 prn3 prn2 prn1 prn0

                {process_top}
                {rotate_10}

                {process_top}
                {rotate_10}

                {process_top}
                {rotate_10}

                {process_top}
                {rotate_10}

                {process_top}
                {rotate_10}

                {process_top}
                {rotate_10}

                {process_top}
                {rotate_10}

                {process_top}
                {rotate_10}

                {process_top}
                {rotate_10}

                {process_top}
                {rotate_10}

                // _ number upper_bound-1 address list_index prn9 prn8 prn7 prn6 prn5 prn4 prn3 prn2 prn1 prn0

                dup 10
                push 0 eq
                push 0 eq
                assert // crash if list_index == 0

                squeeze // overwrite top 10 elements with fresh randomness

                recurse
            
            {entrypoint}_process_top_function_body:
                dup 0  //  _ number upper_bound-1 address list_index prn^10 prn0
                split //  _ number upper_bound-1 address list_index prn^10 hi lo
                dup 13 //  _ number upper_bound-1 address list_index prn^10 hi lo address
                dup 13 //  _ number upper_bound-1 address list_index prn^10 hi lo address list_index
                add //  _ number upper_bound-1 address list_index prn^10 hi lo address+list_index

                swap 1 //  _ number upper_bound-1 address list_index prn^10 hi address+list_index lo
                dup 15 // _ number upper_bound-1 address list_index prn^10 hi address+list_index lo upper_bound-1
                and // _ number upper_bound-1 address list_index prn^10 hi address+list_index (lo&(upper_bound-1))

                write_mem  // _ number upper_bound-1 address list_index prn^10 hi address+list_index
                pop pop // _ number upper_bound-1 address list_index prn^10

                swap 10
                push 1 add
                swap 10
                // _ number upper_bound-1 address list_index+1 prn^10
                return
            "
        )
    }

    fn crash_conditions(&self) -> Vec<String>
    where
        Self: Sized,
    {
        vec![
            "Number exceeds u32::MAX".to_string(),
            "Upper bound is not a power of two".to_string(),
        ]
    }

    fn gen_input_states(&self) -> Vec<crate::ExecutionState>
    where
        Self: Sized,
    {
        vec![
            Self::test_state(0, 1 << 12),
            Self::test_state(1, 1 << 12),
            Self::test_state(10, 1 << 12),
            Self::test_state(11, 1 << 12),
            Self::test_state(45, 1 << 12),
            Self::test_state(4, 1 << 31),
        ]
    }

    fn common_case_input_state(&self) -> crate::ExecutionState
    where
        Self: Sized,
    {
        Self::test_state(45, 1 << 12)
    }

    fn worst_case_input_state(&self) -> crate::ExecutionState
    where
        Self: Sized,
    {
        Self::test_state(160, 1 << 23)
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
        let upper_bound = stack.pop().unwrap().value() as u32;
        let number = stack.pop().unwrap().value() as usize;

        assert!(
            is_power_of_two(upper_bound),
            "Upper bound {upper_bound} must be a power of two"
        );

        // helper functions
        let set_element = match self.list_type {
            ListType::Safe => rust_shadowing_helper_functions::safe_list::safe_list_set,
            ListType::Unsafe => rust_shadowing_helper_functions::unsafe_list::unsafe_list_set,
        };

        // sample indices
        let mut indices: Vec<u32> = vec![];
        let mut sponge_state = Tip5State::new(Domain::VariableLength);
        let mut squeezed = vec![];
        while indices.len() < number {
            if squeezed.is_empty() {
                squeezed = Tip5::squeeze(&mut sponge_state)
                    .into_iter()
                    .rev()
                    .collect_vec();
            }

            let element = squeezed.pop().unwrap();
            if element != BFieldElement::new(BFieldElement::MAX) {
                indices.push(element.value() as u32 % upper_bound);
            }
        }

        // double-shadow with twenty-first
        let twenty_first_indices = VmHasher::sample_indices(
            &mut Tip5State::new(Domain::VariableLength),
            upper_bound,
            number,
        );
        assert_eq!(twenty_first_indices, indices);

        // create list object
        let capacity = number;
        let list = match self.list_type {
            ListType::Safe => {
                // Push capacity to stack
                stack.push(BFieldElement::new(capacity as u64));
                list::safe_u32::new::SafeNew(DataType::U32).rust_shadowing(
                    stack,
                    _std_in.clone(),
                    _secret_in.clone(),
                    memory,
                );
                stack.pop().unwrap()
            }
            ListType::Unsafe => {
                stack.push(BFieldElement::new(capacity as u64));
                list::unsafe_u32::new::UnsafeNew(DataType::U32).rust_shadowing(
                    stack,
                    _std_in.clone(),
                    _secret_in.clone(),
                    memory,
                );
                stack.pop().unwrap()
            }
        };

        // set length
        stack.push(list);
        stack.push(BFieldElement::new(number as u64));
        match self.list_type {
            ListType::Safe => {
                list::safe_u32::set_length::SafeSetLength(DataType::U32)
                    .rust_shadowing(stack, _std_in, _secret_in, memory);
            }
            ListType::Unsafe => {
                list::unsafe_u32::set_length::UnsafeSetLength(DataType::U32)
                    .rust_shadowing(stack, _std_in, _secret_in, memory);
            }
        }
        stack.pop();

        // store list to memory
        for (i, index) in indices.into_iter().enumerate() {
            set_element(
                list,
                i,
                vec![BFieldElement::new(index as u64)],
                memory,
                DataType::U32.get_size(),
            );
        }

        stack.push(list);
    }
}

#[cfg(test)]
mod tests {

    use crate::{list::ListType, test_helpers::test_rust_equivalence_multiple};

    use super::SampleIndices;

    #[test]
    fn new_prop_test() {
        test_rust_equivalence_multiple(
            &SampleIndices {
                list_type: ListType::Safe,
            },
            true,
        );
        test_rust_equivalence_multiple(
            &SampleIndices {
                list_type: ListType::Unsafe,
            },
            true,
        );
    }
}

#[cfg(test)]
mod benches {
    use super::*;
    use crate::snippet_bencher::bench_and_write;

    #[test]
    fn sample_indices_benchmark_safe() {
        bench_and_write(SampleIndices {
            list_type: ListType::Safe,
        });
    }

    #[test]
    fn sample_indices_benchmark_unsafe() {
        bench_and_write(SampleIndices {
            list_type: ListType::Unsafe,
        });
    }
}
