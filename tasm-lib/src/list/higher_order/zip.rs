use itertools::Itertools;
use num::Zero;
use rand::rngs::StdRng;
use rand::{Rng, SeedableRng};
use std::collections::HashMap;
use triton_vm::{triton_asm, NonDeterminism};
use twenty_first::shared_math::b_field_element::BFieldElement;

use crate::function::Function;
use crate::list::safe_u32::length::SafeLength;
use crate::list::safe_u32::new::SafeNew;
use crate::list::safe_u32::set_length::SafeSetLength;
use crate::list::unsafe_u32::length::UnsafeLength;
use crate::list::unsafe_u32::new::UnsafeNew;
use crate::list::unsafe_u32::set_length::UnsafeSetLength;
use crate::list::ListType;
use crate::memory::memcpy::MemCpy;
use crate::rust_shadowing_helper_functions::safe_list::safe_insert_random_list;
use crate::rust_shadowing_helper_functions::unsafe_list::untyped_unsafe_insert_random_list;
use crate::snippet::BasicSnippet;
use crate::{get_init_tvm_stack, rust_shadowing_helper_functions};
use crate::{library::Library, snippet::DataType, ExecutionState};

/// Zips two lists of equal length, returning a new list of pairs of elements.
pub struct Zip {
    pub list_type: ListType,
    pub left_type: DataType,
    pub right_type: DataType,
}

impl BasicSnippet for Zip {
    fn inputs(&self) -> Vec<(DataType, String)> {
        vec![
            (
                DataType::List(Box::new(self.left_type.clone())),
                "*left_list".to_string(),
            ),
            (
                DataType::List(Box::new(self.right_type.clone())),
                "*right_list".to_string(),
            ),
        ]
    }

    fn outputs(&self) -> Vec<(DataType, String)> {
        vec![(
            DataType::List(Box::new(DataType::Pair(
                Box::new(self.left_type.clone()),
                Box::new(self.right_type.clone()),
            ))),
            "*output_list".to_string(),
        )]
    }

    fn entrypoint(&self) -> String {
        format!(
            "tasm_list_higher_order_{}_u32_zip_{}_with_{}",
            self.list_type,
            self.left_type.label_friendly_name(),
            self.right_type.label_friendly_name()
        )
    }

    fn code(&self, library: &mut Library) -> Vec<triton_vm::instruction::LabelledInstruction> {
        let safety_offset = match self.list_type {
            ListType::Safe => 2,
            ListType::Unsafe => 1,
        };

        // helper functions for list(left)
        let get_length_left = match self.list_type {
            ListType::Safe => library.import(Box::new(SafeLength(self.left_type.clone()))),
            ListType::Unsafe => library.import(Box::new(UnsafeLength(self.left_type.clone()))),
        };
        let left_element_size = self.left_type.get_size();

        // helper functions for list(right)
        let get_length_right = match self.list_type {
            ListType::Safe => library.import(Box::new(SafeLength(self.right_type.clone()))),
            ListType::Unsafe => library.import(Box::new(UnsafeLength(self.right_type.clone()))),
        };
        let right_element_size = self.right_type.get_size();

        // helper functions for list(left,right)
        let pair_type = DataType::Pair(
            Box::new(self.left_type.clone()),
            Box::new(self.right_type.clone()),
        );
        let pair_element_size = pair_type.get_size();
        let new_list_pair = match self.list_type {
            ListType::Safe => library.import(Box::new(SafeNew(pair_type.clone()))),
            ListType::Unsafe => library.import(Box::new(UnsafeNew(pair_type.clone()))),
        };
        let set_length_pair = match self.list_type {
            ListType::Safe => library.import(Box::new(SafeSetLength(pair_type))),
            ListType::Unsafe => library.import(Box::new(UnsafeSetLength(pair_type))),
        };

        // helper function for memory
        let memcpy = library.import(Box::new(MemCpy));

        let entrypoint = self.entrypoint();
        let main_loop = format!("{entrypoint}_loop");

        triton_asm!(
            // BEFORE: _ left_list right_list
            // AFTER: _ pair_list
            {entrypoint}:
            // get lengths
            dup 1 // _ left_list right_list left_list
            call {get_length_left} // _ left_list right_list left_len
            dup 1 // _ left_list right_list left_len right_list
            call {get_length_right} // _left_list right_list left_len right_len

            // assert equal lengths
            dup 1 // _ left_list right_list left_len right_len left_len
            eq assert // crashes if left_len != right_len
            // _ left_list right_list len

            // create object for pair list and set length
            dup 0
            call {new_list_pair} // _ left_list right_list len pair_list

            dup 1  // _ left_list right_list len pair_list len
            call {set_length_pair} // _ left_list right_list len pair_list

            // prepare stack for loop
            swap 1 // _ left_list right_list pair_list len

            call {main_loop} // _ left_list right_list pair_list 0

            // clean up stack
            pop // _ left_list right_list pair_list
            swap 2 pop pop // _ pair_list

            return

            // INVARIANT: _ left_list right_list pair_list itr
            {main_loop}:
                // test return condition
                dup 0 // _ left_list right_list pair_list itr itr
                push 0 eq // _ left_list right_list pair_list itr itr==0

                skiz return

                // decrease itr
                push -1 add // _ left_list right_list pair_list index

                // main body

                // compute write dest
                dup 1 // _ left_list right_list pair_list index pair_list
                push {safety_offset} add // _ left_list right_list pair_list index pair_list_start
                dup 1 // _ left_list right_list pair_list index pair_list_start index
                push {pair_element_size} // _ left_list right_list pair_list index pair_list_start index size
                mul add // _ left_list right_list pair_list index pair_list_start+index*size
                // _ left_list right_list pair_list index write_dest

                // compute read source
                dup 4 // _ left_list right_list pair_list index write_dest left_list
                push {safety_offset} add // _ left_list right_list pair_list index write_dest left_list_start
                dup 2 // _ left_list right_list pair_list index write_dest left_list_start index
                push {left_element_size} // _ left_list right_list pair_list index write_dest left_list_start index size
                mul add // _ left_list right_list pair_list index write_dest left_list_start+index*size
                // _ left_list right_list pair_list index write_dest read_source

                dup 1 // _ left_list right_list pair_list index write_dest read_source write_dest

                // compute number of words
                push {left_element_size} // _ left_list right_list pair_list index write_dest read_source write_dest size

                // copy memory
                call {memcpy} // _ left_list right_list pair_list index write_dest

                // compute write dest
                push {left_element_size} add // _ left_list right_list pair_list index write_dest+size
                // _ left_list right_list pair_list index write_dest*

                // compute read source
                dup 3                                   // _ left_list right_list pair_list index write_dest* right_list
                push {safety_offset} add                // _ left_list right_list pair_list index write_dest* right_list_start
                dup 2 push {right_element_size} mul add // _ left_list right_list pair_list index write_dest* right_list_start+index*size
                // _ left_list right_list pair_list index write_dest* read_source

                swap 1 // _ left_list right_list pair_list index read_source write_dest*

                // compute number of words
                push {right_element_size} // _ left_list right_list pair_list index read_source write_dest* size

                // copy memory
                call {memcpy} // _ left_list right_list pair_list index

                recurse
        )
    }
}

impl Function for Zip {
    fn rust_shadow(
        &self,
        stack: &mut Vec<BFieldElement>,
        memory: &mut HashMap<BFieldElement, BFieldElement>,
    ) {
        // get lists
        let right_pointer = stack.pop().unwrap();
        let left_pointer = stack.pop().unwrap();

        // get list lengths
        let right_length = match self.list_type {
            ListType::Safe => rust_shadowing_helper_functions::safe_list::safe_list_get_length(
                right_pointer,
                memory,
            ),
            ListType::Unsafe => {
                rust_shadowing_helper_functions::unsafe_list::unsafe_list_get_length(
                    right_pointer,
                    memory,
                )
            }
        };
        let left_length = match self.list_type {
            ListType::Safe => rust_shadowing_helper_functions::safe_list::safe_list_get_length(
                left_pointer,
                memory,
            ),
            ListType::Unsafe => {
                rust_shadowing_helper_functions::unsafe_list::unsafe_list_get_length(
                    left_pointer,
                    memory,
                )
            }
        };
        assert_eq!(left_length, right_length);

        let get_element = match self.list_type {
            ListType::Safe => rust_shadowing_helper_functions::safe_list::safe_list_get,
            ListType::Unsafe => rust_shadowing_helper_functions::unsafe_list::unsafe_list_get,
        };

        let set_element = match self.list_type {
            ListType::Safe => rust_shadowing_helper_functions::safe_list::safe_list_set,
            ListType::Unsafe => rust_shadowing_helper_functions::unsafe_list::unsafe_list_set,
        };

        let len = left_length;
        let pair_list_capacity = len;
        let pair_type = DataType::Pair(
            Box::new(self.left_type.clone()),
            Box::new(self.right_type.clone()),
        );

        // Get pointer for pair list through dynamic allocator
        let pair_list_size = match self.list_type {
            ListType::Safe => 2 + len * pair_type.get_size(),
            ListType::Unsafe => 1 + len * pair_type.get_size(),
        };
        let pair_list =
            rust_shadowing_helper_functions::dyn_malloc::dynamic_allocator(pair_list_size, memory);

        match self.list_type {
            ListType::Safe => rust_shadowing_helper_functions::safe_list::safe_list_new(
                pair_list,
                pair_list_capacity as u32,
                memory,
            ),
            ListType::Unsafe => {
                rust_shadowing_helper_functions::unsafe_list::unsafe_list_new(pair_list, memory)
            }
        };

        match self.list_type {
            ListType::Safe => {
                rust_shadowing_helper_functions::safe_list::safe_list_set_length(
                    pair_list, len, memory,
                );
            }
            ListType::Unsafe => {
                rust_shadowing_helper_functions::unsafe_list::unsafe_list_set_length(
                    pair_list, len as u32, memory,
                );
            }
        }

        // forall elements, read + merge + write
        for i in 0..len {
            // read
            let left_item = get_element(left_pointer, i, memory, self.left_type.get_size());
            let right_item = get_element(right_pointer, i, memory, self.right_type.get_size());

            // merge
            let pair = left_item
                .into_iter()
                .chain(right_item.into_iter())
                .collect_vec();

            // write
            set_element(pair_list, i, pair, memory, pair_type.get_size());
        }

        stack.push(pair_list);
    }

    fn pseudorandom_initial_state(
        &self,
        seed: [u8; 32],
        _bench_case: Option<crate::snippet_bencher::BenchmarkCase>,
    ) -> (Vec<BFieldElement>, HashMap<BFieldElement, BFieldElement>) {
        let mut rng: StdRng = SeedableRng::from_seed(seed);
        let left_length = rng.gen_range(0..20);
        let execution_state = self.generate_input_state(left_length, left_length);
        (execution_state.stack, execution_state.memory)
    }
}

impl Zip {
    fn generate_input_state(&self, left_length: usize, right_length: usize) -> ExecutionState {
        let left_pointer = BFieldElement::new(1u64);
        let left_capacity = left_length;
        let right_capacity = right_length;

        let mut memory = HashMap::default();

        // allocate
        let left_size;
        let right_size;
        memory.insert(
            BFieldElement::zero(),
            match self.list_type {
                ListType::Safe => {
                    left_size = 2 + left_length * self.left_type.get_size();
                    right_size = 2 + right_length * self.right_type.get_size();
                    BFieldElement::new((1 + left_size + right_size) as u64)
                }

                ListType::Unsafe => {
                    left_size = 1 + left_length * self.left_type.get_size();
                    right_size = 1 + right_length * self.right_type.get_size();
                    BFieldElement::new((1 + left_size + right_size) as u64)
                }
            },
        );
        let right_pointer = left_pointer + BFieldElement::new(left_size as u64);

        // insert lists
        match self.list_type {
            ListType::Safe => safe_insert_random_list(
                &self.left_type,
                left_pointer,
                left_capacity as u32,
                left_length,
                &mut memory,
            ),
            ListType::Unsafe => untyped_unsafe_insert_random_list(
                left_pointer,
                left_length,
                &mut memory,
                self.left_type.get_size(),
            ),
        };
        match self.list_type {
            ListType::Safe => safe_insert_random_list(
                &self.right_type,
                right_pointer,
                right_capacity as u32,
                right_length,
                &mut memory,
            ),
            ListType::Unsafe => untyped_unsafe_insert_random_list(
                right_pointer,
                right_length,
                &mut memory,
                self.right_type.get_size(),
            ),
        };

        // populate stack
        let mut stack = get_init_tvm_stack();
        stack.push(left_pointer);
        stack.push(right_pointer);

        let mut memory_vec = memory.clone().into_iter().collect_vec();
        memory_vec.sort_unstable_by(|&a, &b| a.0.value().partial_cmp(&b.0.value()).unwrap());

        ExecutionState {
            stack,
            std_in: vec![],
            nondeterminism: NonDeterminism::new(vec![]),
            memory,
            words_allocated: 0,
        }
    }
}

// impl DeprecatedSnippet for Zip {
//     fn entrypoint_name(&self) -> String {
//         format!(
//             "tasm_list_higher_order_{}_u32_zip_{}_with_{}",
//             self.list_type,
//             self.left_type.label_friendly_name(),
//             self.right_type.label_friendly_name()
//         )
//     }

//     fn input_field_names(&self) -> Vec<String> {
//         vec!["left_list".to_string(), "right_list".to_string()]
//     }

//     fn input_types(&self) -> Vec<crate::snippet::DataType> {
//         vec![
//             DataType::List(Box::new(self.left_type.clone())),
//             DataType::List(Box::new(self.right_type.clone())),
//         ]
//     }

//     fn output_types(&self) -> Vec<crate::snippet::DataType> {
//         vec![DataType::List(Box::new(DataType::Pair(
//             Box::new(self.left_type.clone()),
//             Box::new(self.right_type.clone()),
//         )))]
//     }

//     fn output_field_names(&self) -> Vec<String> {
//         vec!["output_list".to_string()]
//     }

//     fn stack_diff(&self) -> isize {
//         -1
//     }

//     fn function_code(&self, library: &mut Library) -> String {
//         let safety_offset = match self.list_type {
//             ListType::Safe => 2,
//             ListType::Unsafe => 1,
//         };

//         // helper functions for list(left)
//         let get_length_left = match self.list_type {
//             ListType::Safe => library.import(Box::new(SafeLength(self.left_type.clone()))),
//             ListType::Unsafe => library.import(Box::new(UnsafeLength(self.left_type.clone()))),
//         };
//         let left_element_size = self.left_type.get_size();

//         // helper functions for list(right)
//         let get_length_right = match self.list_type {
//             ListType::Safe => library.import(Box::new(SafeLength(self.right_type.clone()))),
//             ListType::Unsafe => library.import(Box::new(UnsafeLength(self.right_type.clone()))),
//         };
//         let right_element_size = self.right_type.get_size();

//         // helper functions for list(left,right)
//         let pair_type = DataType::Pair(
//             Box::new(self.left_type.clone()),
//             Box::new(self.right_type.clone()),
//         );
//         let pair_element_size = pair_type.get_size();
//         let new_list_pair = match self.list_type {
//             ListType::Safe => library.import(Box::new(SafeNew(pair_type.clone()))),
//             ListType::Unsafe => library.import(Box::new(UnsafeNew(pair_type.clone()))),
//         };
//         let set_length_pair = match self.list_type {
//             ListType::Safe => library.import(Box::new(SafeSetLength(pair_type))),
//             ListType::Unsafe => library.import(Box::new(UnsafeSetLength(pair_type))),
//         };

//         // helper function for memory
//         let memcpy = library.import(Box::new(MemCpy));

//         let entrypoint = self.entrypoint_name();

//         format!(
//             "
//             // BEFORE: _ left_list right_list
//             // AFTER: _ pair_list
//             {entrypoint}:
//             // get lengths
//             dup 1 // _ left_list right_list left_list
//             call {get_length_left} // _ left_list right_list left_len
//             dup 1 // _ left_list right_list left_len right_list
//             call {get_length_right} // _left_list right_list left_len right_len

//             // assert equal lengths
//             dup 1 // _ left_list right_list left_len right_len left_len
//             eq assert // crashes if left_len != right_len
//             // _ left_list right_list len

//             // create object for pair list and set length
//             dup 0
//             call {new_list_pair} // _ left_list right_list len pair_list
//             dup 1  // _ left_list right_list len pair_list len
//             call {set_length_pair} // _ left_list right_list len pair_list

//             // prepare stack for loop
//             swap 1 // _ left_list right_list pair_list len

//             call {entrypoint}_loop // _ left_list right_list pair_list 0

//             // clean up stack
//             pop // _ left_list right_list pair_list
//             swap 2 pop pop // _ pair_list

//             return

//             // INVARIANT: _ left_list right_list pair_list itr
//             {entrypoint}_loop:
//                 // test return condition
//                 dup 0 // _ left_list right_list pair_list itr itr
//                 push 0 eq // _ left_list right_list pair_list itr itr==0

//                 skiz return

//                 // decrease itr
//                 push -1 add // _ left_list right_list pair_list index

//                 // main body

//                 // compute write dest
//                 dup 1 // _ left_list right_list pair_list index pair_list
//                 push {safety_offset} add // _ left_list right_list pair_list index pair_list_start
//                 dup 1 // _ left_list right_list pair_list index pair_list_start index
//                 push {pair_element_size} // _ left_list right_list pair_list index pair_list_start index size
//                 mul add // _ left_list right_list pair_list index pair_list_start+index*size
//                 // _ left_list right_list pair_list index write_dest

//                 // compute read source
//                 dup 4 // _ left_list right_list pair_list index write_dest left_list
//                 push {safety_offset} add // _ left_list right_list pair_list index write_dest left_list_start
//                 dup 2 // _ left_list right_list pair_list index write_dest left_list_start index
//                 push {left_element_size} // _ left_list right_list pair_list index write_dest left_list_start index size
//                 mul add // _ left_list right_list pair_list index write_dest left_list_start+index*size
//                 // _ left_list right_list pair_list index write_dest read_source

//                 dup 1 // _ left_list right_list pair_list index write_dest read_source write_dest

//                 // compute number of words
//                 push {left_element_size} // _ left_list right_list pair_list index write_dest read_source write_dest size

//                 // copy memory
//                 call {memcpy} // _ left_list right_list pair_list index write_dest

//                 // compute write dest
//                 push {left_element_size} add // _ left_list right_list pair_list index write_dest+size
//                 // _ left_list right_list pair_list index write_dest*

//                 // compute read source
//                 dup 3                                   // _ left_list right_list pair_list index write_dest* right_list
//                 push {safety_offset} add                // _ left_list right_list pair_list index write_dest* right_list_start
//                 dup 2 push {right_element_size} mul add // _ left_list right_list pair_list index write_dest* right_list_start+index*size
//                 // _ left_list right_list pair_list index write_dest* read_source

//                 swap 1 // _ left_list right_list pair_list index read_source write_dest*

//                 // compute number of words
//                 push {right_element_size} // _ left_list right_list pair_list index read_source write_dest* size

//                 // copy memory
//                 call {memcpy} // _ left_list right_list pair_list index

//                 recurse
//             "
//         )
//     }

//     fn crash_conditions(&self) -> Vec<String> {
//         vec![
//             "Lengths do not agree".to_string(),
//             "Length exceeds u32::MAX".to_string(),
//         ]
//     }

//     fn gen_input_states(&self) -> Vec<ExecutionState> {
//         // Create random list of input data type
//         let mut rng = thread_rng();
//         let list_length: usize = rng.gen_range(1..=100);

//         vec![self.generate_input_state(list_length, list_length)]
//     }

//     fn common_case_input_state(&self) -> ExecutionState {
//         // Create random list of input data type
//         let list_length: usize = 10;
//         self.generate_input_state(list_length, list_length)
//     }

//     fn worst_case_input_state(&self) -> ExecutionState {
//         // Create random list of input data type
//         let list_length: usize = 400;
//         self.generate_input_state(list_length, list_length)
//     }

//     fn rust_shadowing(
//         &self,
//         stack: &mut Vec<triton_vm::BFieldElement>,
//         _std_in: Vec<triton_vm::BFieldElement>,
//         _secret_in: Vec<triton_vm::BFieldElement>,
//         memory: &mut std::collections::HashMap<triton_vm::BFieldElement, triton_vm::BFieldElement>,
//     ) {
//         let right_pointer = stack.pop().unwrap();
//         let left_pointer = stack.pop().unwrap();

//         // get list lengths
//         let right_length = match self.list_type {
//             ListType::Safe => rust_shadowing_helper_functions::safe_list::safe_list_get_length(
//                 right_pointer,
//                 memory,
//             ),
//             ListType::Unsafe => {
//                 rust_shadowing_helper_functions::unsafe_list::unsafe_list_get_length(
//                     right_pointer,
//                     memory,
//                 )
//             }
//         };
//         let left_length = match self.list_type {
//             ListType::Safe => rust_shadowing_helper_functions::safe_list::safe_list_get_length(
//                 left_pointer,
//                 memory,
//             ),
//             ListType::Unsafe => {
//                 rust_shadowing_helper_functions::unsafe_list::unsafe_list_get_length(
//                     left_pointer,
//                     memory,
//                 )
//             }
//         };
//         assert_eq!(left_length, right_length);

//         let get_element = match self.list_type {
//             ListType::Safe => rust_shadowing_helper_functions::safe_list::safe_list_get,
//             ListType::Unsafe => rust_shadowing_helper_functions::unsafe_list::unsafe_list_get,
//         };

//         let set_element = match self.list_type {
//             ListType::Safe => rust_shadowing_helper_functions::safe_list::safe_list_set,
//             ListType::Unsafe => rust_shadowing_helper_functions::unsafe_list::unsafe_list_set,
//         };

//         let len = left_length;
//         let pair_list_capacity = len;
//         let pair_type = DataType::Pair(
//             Box::new(self.left_type.clone()),
//             Box::new(self.right_type.clone()),
//         );

//         // Get pointer for pair list through dynamic allocator
//         let pair_list_size = match self.list_type {
//             ListType::Safe => 2 + len * pair_type.get_size(),
//             ListType::Unsafe => 1 + len * pair_type.get_size(),
//         };
//         let pair_list =
//             rust_shadowing_helper_functions::dyn_malloc::dynamic_allocator(pair_list_size, memory);

//         match self.list_type {
//             ListType::Safe => rust_shadowing_helper_functions::safe_list::safe_list_new(
//                 pair_list,
//                 pair_list_capacity as u32,
//                 memory,
//             ),
//             ListType::Unsafe => {
//                 rust_shadowing_helper_functions::unsafe_list::unsafe_list_new(pair_list, memory)
//             }
//         };

//         match self.list_type {
//             ListType::Safe => {
//                 rust_shadowing_helper_functions::safe_list::safe_list_set_length(
//                     pair_list, len, memory,
//                 );
//             }
//             ListType::Unsafe => {
//                 rust_shadowing_helper_functions::unsafe_list::unsafe_list_set_length(
//                     pair_list, len as u32, memory,
//                 );
//             }
//         }

//         // forall elements, read + merge + write
//         for i in 0..len {
//             // read
//             let left_item = get_element(left_pointer, i, memory, self.left_type.get_size());
//             let right_item = get_element(right_pointer, i, memory, self.right_type.get_size());

//             // merge
//             let pair = left_item
//                 .into_iter()
//                 .chain(right_item.into_iter())
//                 .collect_vec();

//             // write
//             set_element(pair_list, i, pair, memory, pair_type.get_size());
//         }

//         stack.push(pair_list);
//     }
// }

#[cfg(test)]
mod tests {
    use crate::{function::ShadowedFunction, snippet::RustShadow};

    use super::*;

    #[test]
    fn unsafe_list_prop_test() {
        ShadowedFunction::new(Zip {
            list_type: ListType::Unsafe,
            left_type: DataType::XFE,
            right_type: DataType::Digest,
        })
        .test();
    }

    #[test]
    fn with_safe_list_prop_test() {
        ShadowedFunction::new(Zip {
            list_type: ListType::Safe,
            left_type: DataType::XFE,
            right_type: DataType::Digest,
        })
        .test()
    }
}

#[cfg(test)]
mod benches {
    use super::*;
    use crate::{function::ShadowedFunction, snippet::RustShadow};

    #[test]
    fn unsafe_list_map_benchmark() {
        ShadowedFunction::new(Zip {
            list_type: ListType::Unsafe,
            left_type: DataType::XFE,
            right_type: DataType::Digest,
        })
        .bench();
    }

    #[test]
    fn safe_list_map_benchmark() {
        ShadowedFunction::new(Zip {
            list_type: ListType::Safe,
            left_type: DataType::XFE,
            right_type: DataType::Digest,
        })
        .bench();
    }
}
