use std::collections::HashMap;

use itertools::Itertools;
use rand::{rngs::StdRng, Rng, SeedableRng};
use triton_vm::{triton_asm, NonDeterminism};
use twenty_first::{
    shared_math::{
        b_field_element::BFieldElement,
        other::is_power_of_two,
        tip5::{Tip5, STATE_SIZE},
    },
    util_types::algebraic_hasher::{AlgebraicHasher, Domain, SpongeHasher},
};

use crate::{
    empty_stack,
    list::{
        self,
        safeimplu32::{new::SafeNew, push::SafePush, set_length::SafeSetLength},
        unsafeimplu32::{new::UnsafeNew, push::UnsafePush, set_length::UnsafeSetLength},
        ListType,
    },
    procedure::Procedure,
    rust_shadowing_helper_functions,
    snippet::{BasicSnippet, DataType, DeprecatedSnippet},
    ExecutionState, VmHasher, VmHasherState,
};

#[derive(Clone, Debug)]
pub struct SampleIndices {
    pub list_type: ListType,
}

/// SampleIndices samples n pseudorandom integers
/// between 0 and k. It does this by squeezing the sponge. It is the
/// caller's responsibility to ensure that the sponge is initialized
/// to the right state.
impl SampleIndices {
    fn test_state(number: usize, upper_bound: u32) -> ExecutionState {
        ExecutionState {
            stack: [
                empty_stack(),
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

// impl DeprecatedSnippet for SampleIndices {
//     fn entrypoint_name(&self) -> String {
//         format!("tasm_hashing_sample_indices_to_{}_list", self.list_type)
//     }

//     fn input_field_names(&self) -> Vec<String>
//     where
//         Self: Sized,
//     {
//         vec!["number".to_string(), "upper_bound".to_string()]
//     }

//     fn input_types(&self) -> Vec<crate::snippet::DataType> {
//         vec![DataType::U32, DataType::U32]
//     }

//     fn output_types(&self) -> Vec<crate::snippet::DataType> {
//         vec![DataType::List(Box::new(DataType::U32))]
//     }

//     fn output_field_names(&self) -> Vec<String>
//     where
//         Self: Sized,
//     {
//         vec!["index_list".to_string()]
//     }

//     fn stack_diff(&self) -> isize
//     where
//         Self: Sized,
//     {
//         -1
//     }

//     fn function_code(&self, library: &mut crate::library::Library) -> String {
//         let entrypoint = self.entrypoint_name();
//         let new_list = match self.list_type {
//             ListType::Safe => library.import(Box::new(SafeNew(DataType::U32))),
//             ListType::Unsafe => library.import(Box::new(UnsafeNew(DataType::U32))),
//         };
//         let set_length = match self.list_type {
//             ListType::Safe => library.import(Box::new(SafeSetLength(DataType::U32))),
//             ListType::Unsafe => library.import(Box::new(UnsafeSetLength(DataType::U32))),
//         };
//         let safety_offset = match self.list_type {
//             ListType::Safe => 2,
//             ListType::Unsafe => 1,
//         };
//         let minus_safety_offset = match self.list_type {
//             ListType::Safe => -2,
//             ListType::Unsafe => -1,
//         };

//         let process_top = format!("
//                 // _ number upper_bound-1 address list_index prn^10
//                 dup 10 // _ number upper_bound-1 address list_index prn^10 list_index
//                 dup 14 // _ number upper_bound-1 address list_index prn^10 list_index number
//                 eq // _ number upper_bound-1 address list_index prn^10 list_index==number
//                 dup 1  // _ number upper_bound-1 address list_index prn^10 list_index==number prn0
//                 push -1 eq  // _ number upper_bound-1 address list_index prn^10 list_index==number prn0==-1
//                 add  // _ number upper_bound-1 address list_index prn^10 list_index==number||prn0==-1
//                 push 0 eq // _ number upper_bound-1 address list_index prn^10 list_index!=number&&prn0!=-1
//                 skiz call {entrypoint}_process_top_function_body
//                 // _ number upper_bound-1 address list_index prn^10
//         ");

//         let rotate_10 = "
//             swap 9
//             swap 8
//             swap 7
//             swap 6
//             swap 5
//             swap 4
//             swap 3
//             swap 2
//             swap 1"
//             .to_string();

//         format!(
//             "
//             // BEFORE: _ number upper_bound
//             // AFTER: _ list
//             {entrypoint}:
//                 // assert power of two
//                 dup 0 dup 0 // _ number upper_bound upper_bound upper_bound
//                 push -1 add and // _ number upper_bound upper_bound&(upper_bound-1)
//                 push 0 eq assert // asserts that upper_bound = 2^k for some k
//                 push -1 add // _ number upper_bound-1

//                 // create list
//                 dup 1 // _ number upper_bound-1 number
//                 call {new_list} // _ number upper_bound-1 list
//                 dup 2 //  _ number upper_bound-1 list number
//                 call {set_length} // _ number upper_bound-1 list
//                 push {safety_offset} add // _ number upper_bound-1 address

//                 // prepare and call loop
//                 push 0 // _ number upper_bound-1 address 0
//                 push 0 push 0 push 0 push 0 push 0 push 0 push 0 push 0 push 0 push 0
//                 // _ number upper_bound-1 address list_index 0 0 0 0 0 0 0 0 0 0

//                 sponge_squeeze // overwrite top 10 elements with fresh randomness
//                 call {entrypoint}_loop // _ number upper_bound-1 address number prn9 prn8 prn7 prn6 prn5 prn4 prn3 prn2 prn1 prn0

//                 // clean up stack
//                 pop pop pop pop pop pop pop pop pop pop // _ number upper_bound-1 address number
//                 pop // _ number upper_bound-1 address
//                 swap 2  // _ address  upper_bound-1 number
//                 pop pop // _ address
//                 push {minus_safety_offset} add // _ list

//                 return

//             // INVARIANT: _ number upper_bound-1 list list_index prn9 prn8 prn7 prn6 prn5 prn4 prn3 prn2 prn1 prn0
//             {entrypoint}_loop:
//                 // evaluate termination
//                 dup 13 // _ number upper_bound-1 address list_index prn9 prn8 prn7 prn6 prn5 prn4 prn3 prn2 prn1 prn0 number
//                 dup 11 // _ number upper_bound-1 address list_index prn9 prn8 prn7 prn6 prn5 prn4 prn3 prn2 prn1 prn0 number list_index
//                 eq // _ number upper_bound-1 address list_index prn9 prn8 prn7 prn6 prn5 prn4 prn3 prn2 prn1 prn0 number==list_index

//                 skiz return // continue if unequal
//                 // _ number upper_bound-1 address list_index  prn9 prn8 prn7 prn6 prn5 prn4 prn3 prn2 prn1 prn0

//                 {process_top}
//                 {rotate_10}

//                 {process_top}
//                 {rotate_10}

//                 {process_top}
//                 {rotate_10}

//                 {process_top}
//                 {rotate_10}

//                 {process_top}
//                 {rotate_10}

//                 {process_top}
//                 {rotate_10}

//                 {process_top}
//                 {rotate_10}

//                 {process_top}
//                 {rotate_10}

//                 {process_top}
//                 {rotate_10}

//                 {process_top}
//                 {rotate_10}

//                 // _ number upper_bound-1 address list_index prn9 prn8 prn7 prn6 prn5 prn4 prn3 prn2 prn1 prn0

//                 dup 10
//                 push 0 eq
//                 push 0 eq
//                 assert // crash if list_index == 0

//                 sponge_squeeze // overwrite top 10 elements with fresh randomness

//                 recurse

//             {entrypoint}_process_top_function_body:
//                 dup 0  //  _ number upper_bound-1 address list_index prn^10 prn0
//                 split //  _ number upper_bound-1 address list_index prn^10 hi lo
//                 dup 13 //  _ number upper_bound-1 address list_index prn^10 hi lo address
//                 dup 13 //  _ number upper_bound-1 address list_index prn^10 hi lo address list_index
//                 add //  _ number upper_bound-1 address list_index prn^10 hi lo address+list_index

//                 swap 1 //  _ number upper_bound-1 address list_index prn^10 hi address+list_index lo
//                 dup 15 // _ number upper_bound-1 address list_index prn^10 hi address+list_index lo upper_bound-1
//                 and // _ number upper_bound-1 address list_index prn^10 hi address+list_index (lo&(upper_bound-1))

//                 write_mem  // _ number upper_bound-1 address list_index prn^10 hi address+list_index
//                 pop pop // _ number upper_bound-1 address list_index prn^10

//                 swap 10
//                 push 1 add
//                 swap 10
//                 // _ number upper_bound-1 address list_index+1 prn^10
//                 return
//             "
//         )
//     }

//     fn crash_conditions(&self) -> Vec<String>
//     where
//         Self: Sized,
//     {
//         vec![
//             "Number exceeds u32::MAX".to_string(),
//             "Upper bound is not a power of two".to_string(),
//         ]
//     }

//     fn gen_input_states(&self) -> Vec<crate::ExecutionState>
//     where
//         Self: Sized,
//     {
//         vec![
//             Self::test_state(0, 1 << 12),
//             Self::test_state(1, 1 << 12),
//             Self::test_state(10, 1 << 12),
//             Self::test_state(11, 1 << 12),
//             Self::test_state(45, 1 << 12),
//             Self::test_state(4, 1 << 31),
//         ]
//     }

//     fn common_case_input_state(&self) -> crate::ExecutionState
//     where
//         Self: Sized,
//     {
//         Self::test_state(45, 1 << 12)
//     }

//     fn worst_case_input_state(&self) -> crate::ExecutionState
//     where
//         Self: Sized,
//     {
//         Self::test_state(160, 1 << 23)
//     }

//     fn rust_shadowing(
//         &self,
//         stack: &mut Vec<triton_vm::BFieldElement>,
//         _std_in: Vec<triton_vm::BFieldElement>,
//         _secret_in: Vec<triton_vm::BFieldElement>,
//         memory: &mut std::collections::HashMap<triton_vm::BFieldElement, triton_vm::BFieldElement>,
//     ) where
//         Self: Sized,
//     {
//         let upper_bound = stack.pop().unwrap().value() as u32;
//         let number = stack.pop().unwrap().value() as usize;

//         assert!(
//             is_power_of_two(upper_bound),
//             "Upper bound {upper_bound} must be a power of two"
//         );

//         // helper functions
//         let set_element = match self.list_type {
//             ListType::Safe => rust_shadowing_helper_functions::safe_list::safe_list_set,
//             ListType::Unsafe => rust_shadowing_helper_functions::unsafe_list::unsafe_list_set,
//         };

//         // sample indices
//         let mut indices: Vec<u32> = vec![];
//         let mut sponge_state = VmHasherState::new(Domain::VariableLength);
//         let mut squeezed = vec![];
//         while indices.len() < number {
//             if squeezed.is_empty() {
//                 squeezed = Tip5::squeeze(&mut sponge_state)
//                     .into_iter()
//                     .rev()
//                     .collect_vec();
//             }

//             let element = squeezed.pop().unwrap();
//             if element != BFieldElement::new(BFieldElement::MAX) {
//                 indices.push(element.value() as u32 % upper_bound);
//             }
//         }

//         // double-shadow with twenty-first
//         let twenty_first_indices = VmHasher::sample_indices(
//             &mut VmHasherState::new(Domain::VariableLength),
//             upper_bound,
//             number,
//         );
//         assert_eq!(twenty_first_indices, indices);

//         // create list object
//         let capacity = number;
//         let list = match self.list_type {
//             ListType::Safe => {
//                 // Push capacity to stack
//                 stack.push(BFieldElement::new(capacity as u64));
//                 list::safeimplu32::new::SafeNew(DataType::U32).rust_shadowing(
//                     stack,
//                     _std_in.clone(),
//                     _secret_in.clone(),
//                     memory,
//                 );
//                 stack.pop().unwrap()
//             }
//             ListType::Unsafe => {
//                 stack.push(BFieldElement::new(capacity as u64));
//                 list::unsafeimplu32::new::UnsafeNew(DataType::U32).rust_shadowing(
//                     stack,
//                     _std_in.clone(),
//                     _secret_in.clone(),
//                     memory,
//                 );
//                 stack.pop().unwrap()
//             }
//         };

//         // set length
//         stack.push(list);
//         stack.push(BFieldElement::new(number as u64));
//         match self.list_type {
//             ListType::Safe => {
//                 list::safeimplu32::set_length::SafeSetLength(DataType::U32)
//                     .rust_shadowing(stack, _std_in, _secret_in, memory);
//             }
//             ListType::Unsafe => {
//                 list::unsafeimplu32::set_length::UnsafeSetLength(DataType::U32)
//                     .rust_shadowing(stack, _std_in, _secret_in, memory);
//             }
//         }
//         stack.pop();

//         // store list to memory
//         for (i, index) in indices.into_iter().enumerate() {
//             set_element(
//                 list,
//                 i,
//                 vec![BFieldElement::new(index as u64)],
//                 memory,
//                 DataType::U32.get_size(),
//             );
//         }

//         stack.push(list);
//     }
// }

// #[cfg(test)]
// mod tests {

//     use crate::{list::ListType, test_helpers::test_rust_equivalence_multiple_deprecated};

//     use super::SampleIndices;

//     #[test]
//     fn new_prop_test() {
//         test_rust_equivalence_multiple_deprecated(
//             &SampleIndices {
//                 list_type: ListType::Safe,
//             },
//             true,
//         );
//         test_rust_equivalence_multiple_deprecated(
//             &SampleIndices {
//                 list_type: ListType::Unsafe,
//             },
//             true,
//         );
//     }
// }

// #[cfg(test)]
// mod benches {
//     use super::*;
//     use crate::snippet_bencher::bench_and_write;

//     #[test]
//     fn sample_indices_benchmark_safe() {
//         bench_and_write(SampleIndices {
//             list_type: ListType::Safe,
//         });
//     }

//     #[test]
//     fn sample_indices_benchmark_unsafe() {
//         bench_and_write(SampleIndices {
//             list_type: ListType::Unsafe,
//         });
//     }
// }

impl BasicSnippet for SampleIndices {
    fn inputs(&self) -> Vec<(DataType, String)> {
        vec![
            (DataType::U32, "number".to_string()),
            (DataType::U32, "upper_bound".to_string()),
        ]
    }

    fn outputs(&self) -> Vec<(DataType, String)> {
        vec![(
            DataType::List(Box::new(DataType::U32)),
            "*indices".to_string(),
        )]
    }

    fn entrypoint(&self) -> String {
        "sample_indices".to_string()
    }

    fn code(
        &self,
        library: &mut crate::library::Library,
    ) -> Vec<triton_vm::instruction::LabelledInstruction> {
        let entrypoint = self.entrypoint();
        let main_loop = format!("{entrypoint}_main_loop");
        let then_reduce_and_save = format!("{entrypoint}_then_reduce_and_save");
        let then_reduce_and_save_special = format!("{entrypoint}_then_reduce_and_save_special");
        let else_drop_tip = format!("{entrypoint}_else_drop_tip");
        let else_drop_tip_special = format!("{entrypoint}_else_drop_tip_special");
        let new_list = match self.list_type {
            ListType::Safe => library.import(Box::new(SafeNew(DataType::U32))),
            ListType::Unsafe => library.import(Box::new(UnsafeNew(DataType::U32))),
        };
        let length = match self.list_type {
            ListType::Safe => {
                library.import(Box::new(list::safeimplu32::length::Length(DataType::U32)))
            }
            ListType::Unsafe => {
                library.import(Box::new(list::unsafeimplu32::length::Length(DataType::U32)))
            }
        };
        let push_element = match self.list_type {
            ListType::Safe => library.import(Box::new(SafePush(DataType::U32))),
            ListType::Unsafe => library.import(Box::new(UnsafePush(DataType::U32))),
        };

        let if_can_sample = triton_asm! (
            // BEFORE: _ prn number upper_bound *indices
            // AFTER: _ prn number upper_bound *indices can_use
            dup 0 call {length}         // _ prn number upper_bound *indices length
            dup 3 eq                    // _ prn number upper_bound *indices length==number
            push 0 eq                   // _ prn number upper_bound *indices length!=number
            dup 4 push -1 eq            // _ prn number upper_bound *indices length!=number prn==max
            push 0 eq                   // _ prn number upper_bound *indices length!=number prn!=max
            mul                         // _ prn number upper_bound *indices length!=number&&prn!=max
            push 1337 assert
        );

        triton_asm! (
            // BEFORE: _ number upper_bound
            // AFTER: _ *indices
            {entrypoint}:
                // allocate a large enough list
                dup 1                   // _ number upper_bound length
                call {new_list}         // _ number upper_bound *indices

                // prepare and call main while lop
                swap 1                  // _ number *indices upper_bound
                push -1 add             // _ number *indices upper_bound-1
                swap 1                  // _ number upper_bound-1 *indices
                call {main_loop}        // _ number upper_bound-1 *indices

                // clean up and return
                swap 2 pop pop
                return

            // INVARIANT: _ number upper_bound-1 *indices
            {main_loop}:
                // evaluate termination condition
                dup 0 call {length}     // _ number upper_bound-1 *indices length
                dup 3 eq                // _ number upper_bound-1 *indices length==number
                skiz return             // _ number upper_bound-1 *indices

                // we need to squeeze so squeeze
                push 0 push 0 push 0 push 0 push 0
                push 0 push 0 push 0 push 0 push 0
                sponge_squeeze          // _ number upper_bound-1 *indices [prn]

                // reject or reduce-and-store
                dup 12 dup 12 dup 12    // _ number upper_bound-1 *indices [prn] number upper_bound-1 *indices

                {&if_can_sample}
                skiz call {then_reduce_and_save}
                skiz call {else_drop_tip}
                {&if_can_sample}
                skiz call {then_reduce_and_save}
                skiz call {else_drop_tip}
                {&if_can_sample}
                skiz call {then_reduce_and_save}
                skiz call {else_drop_tip}
                {&if_can_sample}
                skiz call {then_reduce_and_save}
                skiz call {else_drop_tip}
                // push 1340 assert
                {&if_can_sample}    // _ number upper_bound-1 *indices 1
                push 1341 assert
                skiz call {then_reduce_and_save_special}
                skiz call {else_drop_tip_special}

                {&if_can_sample}
                skiz call {then_reduce_and_save}
                skiz call {else_drop_tip}
                {&if_can_sample}
                skiz call {then_reduce_and_save}
                skiz call {else_drop_tip}
                {&if_can_sample}
                skiz call {then_reduce_and_save}
                skiz call {else_drop_tip}
                {&if_can_sample}
                skiz call {then_reduce_and_save}
                skiz call {else_drop_tip}
                {&if_can_sample}
                skiz call {then_reduce_and_save}
                skiz call {else_drop_tip}         // _ number upper_bound-1 *indices number upper_bound-1 *indices


                dup 0 call {length} push 1338 assert

                // return to invariant and repeat
                pop pop pop
                recurse

            // BEFORE: _ prn number upper_bound-1 *indices
            // AFTER: _ number upper_bound-1 *indices 0
            {then_reduce_and_save}:
                swap 2 swap 3           // _ number *indices upper_bound-1 prn
                split                   // _ number *indices upper_bound-1 hi lo
                dup 2 and               // _ number *indices upper_bound-1 hi index
                swap 1 pop              // _ number *indices upper_bound-1 index

                swap 1 swap 2 swap 1    // _ number upper_bound-1 *indices index
                dup 1 swap 1            // _ number upper_bound-1 *indices *indices index
                call {push_element}

                push 0
                return

            // BEFORE: _ prn number upper_bound-1 *indices
            // AFTER: _ number upper_bound-1 *indices 0
            {then_reduce_and_save_special}:
                swap 2 swap 3           // _ number *indices upper_bound-1 prn
                split                   // _ number *indices upper_bound-1 hi lo
                dup 2 and               // _ number *indices upper_bound-1 hi index
                swap 1 pop              // _ number *indices upper_bound-1 index

                swap 1 swap 2 swap 1    // _ number upper_bound-1 *indices index
                dup 1 swap 1            // _ number upper_bound-1 *indices *indices index
                call {push_element}     // _ number upper_bound-1 *indices

                dup 0 call {length} push 1339 assert

                push 0
                return

            // BEFORE: _ prn number upper_bound-1 *indices
            // AFTER: _ number upper_bound-1 *indices
            {else_drop_tip}:
                swap 2 swap 3           // _ number *indices upper_bound-1 prn
                pop swap 1              // _ number upper_bound-1 *indices
                return

            // BEFORE: _ prn number upper_bound-1 *indices
            // AFTER: _ number upper_bound-1 *indices
            {else_drop_tip_special}:
                dup 0 call {length}
                push 1337 assert
                swap 2 swap 3           // _ number *indices upper_bound-1 prn
                pop swap 1              // _ number upper_bound-1 *indices
                return
        )
    }
}

impl Procedure for SampleIndices {
    fn rust_shadow(
        &self,
        stack: &mut Vec<BFieldElement>,
        memory: &mut HashMap<BFieldElement, BFieldElement>,
        _nondeterminism: &NonDeterminism<BFieldElement>,
        _public_input: &[BFieldElement],
        sponge_state: &mut VmHasherState,
    ) -> Vec<BFieldElement> {
        // collect upper bound and number from stack
        let upper_bound = stack.pop().unwrap().value() as u32;
        let number = stack.pop().unwrap().value() as usize;

        println!("sampling {number} indices between 0 and {upper_bound}");
        println!(
            "sponge state before: {}",
            sponge_state.state.iter().map(|b| b.value()).join(",")
        );

        // sample indices
        let indices = VmHasher::sample_indices(sponge_state, upper_bound, number);

        // allocate memory for (unsafe) list
        let list_pointer =
            rust_shadowing_helper_functions::dyn_malloc::dynamic_allocator(1 + number, memory);
        rust_shadowing_helper_functions::unsafe_list::unsafe_list_new(list_pointer, memory);

        // store all indices
        for index in indices.iter() {
            rust_shadowing_helper_functions::unsafe_list::unsafe_list_push(
                list_pointer,
                vec![BFieldElement::new(*index as u64)],
                memory,
                1,
            );
        }
        println!(
            "sponge state after: {}",
            sponge_state.state.iter().map(|b| b.value()).join(",")
        );

        // populate the stack with the list pointer
        stack.push(list_pointer);

        vec![]
    }

    fn pseudorandom_initial_state(
        &self,
        seed: [u8; 32],
        bench_case: Option<crate::snippet_bencher::BenchmarkCase>,
    ) -> (
        Vec<BFieldElement>,
        HashMap<BFieldElement, BFieldElement>,
        NonDeterminism<BFieldElement>,
        Vec<BFieldElement>,
        VmHasherState,
    ) {
        let mut rng: StdRng = SeedableRng::from_seed(seed);
        let number = if let Some(case) = bench_case {
            match case {
                crate::snippet_bencher::BenchmarkCase::CommonCase => 45,
                crate::snippet_bencher::BenchmarkCase::WorstCase => 105,
            }
        } else {
            rng.gen_range(0..20)
        };
        let upper_bound = 1 << rng.gen_range(0..20);

        let mut stack = empty_stack();
        stack.push(BFieldElement::new(number as u64));
        stack.push(BFieldElement::new(upper_bound as u64));

        let memory: HashMap<BFieldElement, BFieldElement> = HashMap::new();

        let nondeterminism = NonDeterminism::new(vec![]);
        let public_input: Vec<BFieldElement> = vec![];
        let state = VmHasherState {
            state: rng.gen::<[BFieldElement; STATE_SIZE]>(),
        };

        (stack, memory, nondeterminism, public_input, state)
    }
}

#[cfg(test)]
mod test {
    use crate::{list::ListType, procedure::ShadowedProcedure, snippet::RustShadow};

    use super::SampleIndices;

    #[test]
    fn test() {
        ShadowedProcedure::new(SampleIndices {
            list_type: ListType::Unsafe,
        })
        .test();
    }
}

#[cfg(test)]
mod bench {
    use crate::{list::ListType, procedure::ShadowedProcedure, snippet::RustShadow};

    use super::SampleIndices;

    #[test]
    fn bench() {
        ShadowedProcedure::new(SampleIndices {
            list_type: ListType::Unsafe,
        })
        .bench();
    }
}
