use itertools::Itertools;
use num::Zero;
use rand::rngs::StdRng;
use rand::{Rng, SeedableRng};
use std::collections::HashMap;
use triton_vm::{triton_asm, NonDeterminism};
use twenty_first::shared_math::b_field_element::BFieldElement;

use crate::data_type::DataType;
use crate::list::safeimplu32::length::Length as SafeLength;
use crate::list::safeimplu32::new::SafeNew;
use crate::list::safeimplu32::set_length::SafeSetLength;
use crate::list::unsafeimplu32::length::Length as UnsafeLength;
use crate::list::unsafeimplu32::new::UnsafeNew;
use crate::list::unsafeimplu32::set_length::UnsafeSetLength;
use crate::list::ListType;
use crate::memory::memcpy::MemCpy;
use crate::rust_shadowing_helper_functions::safe_list::safe_insert_random_list;
use crate::rust_shadowing_helper_functions::unsafe_list::untyped_unsafe_insert_random_list;
use crate::traits::basic_snippet::BasicSnippet;
use crate::traits::function::Function;
use crate::{empty_stack, rust_shadowing_helper_functions};
use crate::{library::Library, ExecutionState};

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
            DataType::List(Box::new(DataType::Tuple(vec![
                self.left_type.clone(),
                self.right_type.clone(),
            ]))),
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
            ListType::Safe => library.import(Box::new(SafeLength {
                data_type: self.left_type.clone(),
            })),
            ListType::Unsafe => library.import(Box::new(UnsafeLength {
                data_type: self.left_type.clone(),
            })),
        };
        let left_element_size = self.left_type.stack_size();

        // helper functions for list(right)
        let get_length_right = match self.list_type {
            ListType::Safe => library.import(Box::new(SafeLength {
                data_type: self.right_type.clone(),
            })),
            ListType::Unsafe => library.import(Box::new(UnsafeLength {
                data_type: self.right_type.clone(),
            })),
        };
        let right_element_size = self.right_type.stack_size();

        let output_type = DataType::Tuple(vec![self.left_type.clone(), self.right_type.clone()]);
        let output_element_size = output_type.stack_size();
        let new_output_list = match self.list_type {
            ListType::Safe => library.import(Box::new(SafeNew {
                data_type: output_type.clone(),
            })),
            ListType::Unsafe => library.import(Box::new(UnsafeNew {
                data_type: output_type.clone(),
            })),
        };
        let set_output_list_length = match self.list_type {
            ListType::Safe => library.import(Box::new(SafeSetLength {
                data_type: output_type,
            })),
            ListType::Unsafe => library.import(Box::new(UnsafeSetLength {
                data_type: output_type,
            })),
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
            call {new_output_list} // _ left_list right_list len pair_list

            dup 1  // _ left_list right_list len pair_list len
            call {set_output_list_length} // _ left_list right_list len pair_list

            // prepare stack for loop
            swap 1 // _ left_list right_list pair_list len

            call {main_loop} // _ left_list right_list pair_list 0

            // clean up stack
            pop 1 // _ left_list right_list pair_list
            swap 2 pop 2 // _ pair_list

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
                push {output_element_size} // _ left_list right_list pair_list index pair_list_start index size
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
        let output_list_capacity = len;
        let output_type = DataType::Tuple(vec![self.left_type.clone(), self.right_type.clone()]);

        // Get pointer for pair list through dynamic allocator
        let output_list_size = match self.list_type {
            ListType::Safe => 2 + len * output_type.stack_size(),
            ListType::Unsafe => 1 + len * output_type.stack_size(),
        };
        let output_list = rust_shadowing_helper_functions::dyn_malloc::dynamic_allocator(
            output_list_size,
            memory,
        );

        match self.list_type {
            ListType::Safe => rust_shadowing_helper_functions::safe_list::safe_list_new(
                output_list,
                output_list_capacity as u32,
                memory,
            ),
            ListType::Unsafe => {
                rust_shadowing_helper_functions::unsafe_list::unsafe_list_new(output_list, memory)
            }
        };

        match self.list_type {
            ListType::Safe => {
                rust_shadowing_helper_functions::safe_list::safe_list_set_length(
                    output_list,
                    len,
                    memory,
                );
            }
            ListType::Unsafe => {
                rust_shadowing_helper_functions::unsafe_list::unsafe_list_set_length(
                    output_list,
                    len as u32,
                    memory,
                );
            }
        }

        // forall elements, read + merge + write
        for i in 0..len {
            // read
            let left_item = get_element(left_pointer, i, memory, self.left_type.stack_size());
            let right_item = get_element(right_pointer, i, memory, self.right_type.stack_size());

            // merge
            let pair = left_item
                .into_iter()
                .chain(right_item.into_iter())
                .collect_vec();

            // write
            set_element(output_list, i, pair, memory);
        }

        stack.push(output_list);
    }

    fn pseudorandom_initial_state(
        &self,
        seed: [u8; 32],
        _bench_case: Option<crate::snippet_bencher::BenchmarkCase>,
    ) -> (Vec<BFieldElement>, HashMap<BFieldElement, BFieldElement>) {
        let mut rng: StdRng = SeedableRng::from_seed(seed);
        let left_length = rng.gen_range(0..20);
        let execution_state = self.generate_input_state(left_length, left_length);
        (execution_state.stack, execution_state.nondeterminism.ram)
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
                    left_size = 2 + left_length * self.left_type.stack_size();
                    right_size = 2 + right_length * self.right_type.stack_size();
                    BFieldElement::new((1 + left_size + right_size) as u64)
                }

                ListType::Unsafe => {
                    left_size = 1 + left_length * self.left_type.stack_size();
                    right_size = 1 + right_length * self.right_type.stack_size();
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
                self.left_type.stack_size(),
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
                self.right_type.stack_size(),
            ),
        };

        // populate stack
        let mut stack = empty_stack();
        stack.push(left_pointer);
        stack.push(right_pointer);

        let nondeterminism = NonDeterminism::default().with_ram(memory);
        ExecutionState {
            stack,
            std_in: vec![],
            nondeterminism,
            words_allocated: 0,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::traits::function::ShadowedFunction;
    use crate::traits::rust_shadow::RustShadow;

    #[test]
    fn unsafe_list_prop_test_xfe_digest() {
        ShadowedFunction::new(Zip {
            list_type: ListType::Unsafe,
            left_type: DataType::Xfe,
            right_type: DataType::Digest,
        })
        .test();
    }

    #[test]
    fn with_safe_list_prop_test_xfe_digest() {
        ShadowedFunction::new(Zip {
            list_type: ListType::Safe,
            left_type: DataType::Xfe,
            right_type: DataType::Digest,
        })
        .test()
    }

    #[test]
    fn list_prop_test_more_types() {
        for list_type in [ListType::Unsafe, ListType::Safe] {
            ShadowedFunction::new(Zip {
                list_type: list_type.clone(),
                left_type: DataType::Bfe,
                right_type: DataType::Bfe,
            })
            .test();
            ShadowedFunction::new(Zip {
                list_type: list_type.clone(),
                left_type: DataType::U64,
                right_type: DataType::U32,
            })
            .test();
            ShadowedFunction::new(Zip {
                list_type: list_type.clone(),
                left_type: DataType::Bool,
                right_type: DataType::Digest,
            })
            .test();
            ShadowedFunction::new(Zip {
                list_type: list_type.clone(),
                left_type: DataType::U128,
                right_type: DataType::VoidPointer,
            })
            .test();
            ShadowedFunction::new(Zip {
                list_type: list_type.clone(),
                left_type: DataType::U128,
                right_type: DataType::Digest,
            })
            .test();
            ShadowedFunction::new(Zip {
                list_type: list_type.clone(),
                left_type: DataType::U128,
                right_type: DataType::U128,
            })
            .test();
            ShadowedFunction::new(Zip {
                list_type: list_type.clone(),
                left_type: DataType::Digest,
                right_type: DataType::Digest,
            })
            .test();
        }
    }
}

#[cfg(test)]
mod benches {
    use super::*;
    use crate::traits::function::ShadowedFunction;
    use crate::traits::rust_shadow::RustShadow;

    #[test]
    fn unsafe_list_map_benchmark() {
        ShadowedFunction::new(Zip {
            list_type: ListType::Unsafe,
            left_type: DataType::Xfe,
            right_type: DataType::Digest,
        })
        .bench();
    }

    #[test]
    fn safe_list_map_benchmark() {
        ShadowedFunction::new(Zip {
            list_type: ListType::Safe,
            left_type: DataType::Xfe,
            right_type: DataType::Digest,
        })
        .bench();
    }
}
