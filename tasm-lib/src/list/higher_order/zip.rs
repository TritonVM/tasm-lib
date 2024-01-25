use std::collections::HashMap;

use itertools::Itertools;
use rand::rngs::StdRng;
use rand::*;
use triton_vm::prelude::*;

use crate::data_type::DataType;
use crate::list::ListType;
use crate::memory::memcpy::MemCpy;
use crate::rust_shadowing_helper_functions::safe_list::safe_insert_random_list;
use crate::rust_shadowing_helper_functions::unsafe_list::untyped_unsafe_insert_random_list;
use crate::traits::basic_snippet::BasicSnippet;
use crate::traits::function::*;
use crate::*;

/// Zips two lists of equal length, returning a new list of pairs of elements.
pub struct Zip {
    pub list_type: ListType,
    pub left_type: DataType,
    pub right_type: DataType,
}

impl BasicSnippet for Zip {
    fn inputs(&self) -> Vec<(DataType, String)> {
        let list = |data_type| DataType::List(Box::new(data_type));

        let left_list = (list(self.left_type.clone()), "*left_list".to_string());
        let right_list = (list(self.right_type.clone()), "*right_list".to_string());
        vec![left_list, right_list]
    }

    fn outputs(&self) -> Vec<(DataType, String)> {
        let list = |data_type| DataType::List(Box::new(data_type));

        let tuple_type = DataType::Tuple(vec![self.left_type.clone(), self.right_type.clone()]);
        let output_list = (list(tuple_type), "*output_list".to_string());
        vec![output_list]
    }

    fn entrypoint(&self) -> String {
        format!(
            "tasm_list_higher_order_{}_u32_zip_{}_with_{}",
            self.list_type,
            self.left_type.label_friendly_name(),
            self.right_type.label_friendly_name()
        )
    }

    fn code(&self, library: &mut Library) -> Vec<LabelledInstruction> {
        let safety_offset = self.list_type.metadata_size();

        let get_length_left = library.import(self.list_type.length_snippet(self.left_type.clone()));
        let left_element_size = self.left_type.stack_size();

        let get_length_right =
            library.import(self.list_type.length_snippet(self.right_type.clone()));
        let right_element_size = self.right_type.stack_size();

        let output_type = DataType::Tuple(vec![self.left_type.clone(), self.right_type.clone()]);
        let output_element_size = output_type.stack_size();

        let new_output_list = library.import(self.list_type.new_list_snippet(output_type.clone()));
        let set_output_list_length = library.import(self.list_type.set_length(output_type));

        // helper function for memory
        let memcpy = library.import(Box::new(MemCpy));

        let entrypoint = self.entrypoint();
        let main_loop = format!("{entrypoint}_loop");

        triton_asm!(
            // BEFORE: _ *left_list *right_list
            // AFTER:  _ *pair_list
            {entrypoint}:
            // get lengths
            dup 1                   // _ *left_list *right_list *left_list
            call {get_length_left}  // _ *left_list *right_list left_len
            dup 1                   // _ *left_list *right_list left_len *right_list
            call {get_length_right} // _ *left_list *right_list left_len right_len

            // assert equal lengths
            dup 1                   // _ *left_list *right_list left_len right_len left_len
            eq assert               // _ *left_list *right_list len

            // create object for pair list and set length
            dup 0
            call {new_output_list}  // _ *left_list *right_list len *pair_list

            dup 1                   // _ *left_list *right_list len *pair_list len
            call {set_output_list_length}
                                    // _ *left_list *right_list len *pair_list

            // prepare stack for loop
            swap 1                  // _ *left_list *right_list *pair_list len
            call {main_loop}        // _ *left_list *right_list *pair_list 0

            // clean up stack
            pop 1                   // _ *left_list *right_list *pair_list
            swap 2 pop 2            // _ *pair_list

            return

            // INVARIANT: _ *left_list *right_list *pair_list itr
            {main_loop}:
                // test return condition
                dup 0               // _ *left_list *right_list *pair_list itr itr
                push 0 eq           // _ *left_list *right_list *pair_list itr itr==0

                skiz return

                // decrease itr
                push -1 add         // _ *left_list *right_list *pair_list index

                // main body

                // compute write dest
                dup 1               // _ *left_list *right_list *pair_list index *pair_list
                push {safety_offset} add
                                    // _ *left_list *right_list *pair_list index *pair_list_start
                dup 1               // _ *left_list *right_list *pair_list index *pair_list_start index
                push {output_element_size}
                                    // _ *left_list *right_list *pair_list index *pair_list_start index size
                mul add             // _ *left_list *right_list *pair_list index *pair_list_start+index*size
                                    // _ *left_list *right_list *pair_list index *write_dest

                // compute read source
                dup 3               // _ *left_list *right_list *pair_list index *write_dest *right_list
                push {safety_offset} add
                                    // _ *left_list *right_list *pair_list index *write_dest *right_list_start
                dup 2               // _ *left_list *right_list *pair_list index *write_dest *right_list_start index
                push {right_element_size}
                                    // _ *left_list *right_list *pair_list index *write_dest *right_list_start index size
                mul add             // _ *left_list *right_list *pair_list index *write_dest *right_list_start+index*size
                                    // _ *left_list *right_list *pair_list index *write_dest *read_source

                dup 1               // _ *left_list *right_list *pair_list index *write_dest *read_source *write_dest

                // compute number of words
                push {right_element_size}
                                    // _ *left_list *right_list *pair_list index *write_dest *read_source *write_dest size

                // copy memory
                call {memcpy}       // _ *left_list *right_list *pair_list index *write_dest

                // compute write dest
                push {right_element_size} add
                                    // _ *left_list *right_list *pair_list index *write_dest+size
                                    // _ *left_list *right_list *pair_list index *write_dest'

                // compute read source
                dup 4               // _ *left_list *right_list *pair_list index *write_dest' *left_list
                push {safety_offset} add
                                    // _ *left_list *right_list *pair_list index *write_dest' *left_list_start
                dup 2               // _ *left_list *right_list *pair_list index *write_dest' *left_list_start index
                push {left_element_size}
                                    // _ *left_list *right_list *pair_list index *write_dest' *left_list_start index size
                mul add             // _ *left_list *right_list *pair_list index *write_dest' *left_list_start+index*size
                                    // _ *left_list *right_list *pair_list index *write_dest' *read_source

                swap 1              // _ *left_list *right_list *pair_list index *read_source *write_dest'

                // compute number of words
                push {left_element_size}
                                    // _ *left_list *right_list *pair_list index *read_source *write_dest' size

                // copy memory
                call {memcpy}       // _ *left_list *right_list *pair_list index

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
        use rust_shadowing_helper_functions::*;

        // get helper methods
        let new_list = |pointer, capacity, memory| match self.list_type {
            ListType::Safe => safe_list::safe_list_new(pointer, capacity, memory),
            ListType::Unsafe => unsafe_list::unsafe_list_new(pointer, memory),
        };

        let get_list_length = match self.list_type {
            ListType::Safe => safe_list::safe_list_get_length,
            ListType::Unsafe => unsafe_list::unsafe_list_get_length,
        };

        let set_list_len = match self.list_type {
            ListType::Safe => safe_list::safe_list_set_length,
            ListType::Unsafe => unsafe_list::unsafe_list_set_length,
        };

        let get_element = match self.list_type {
            ListType::Safe => safe_list::safe_list_get,
            ListType::Unsafe => unsafe_list::unsafe_list_get,
        };

        let set_element = match self.list_type {
            ListType::Safe => safe_list::safe_list_set,
            ListType::Unsafe => unsafe_list::unsafe_list_set,
        };

        // start shadowing
        let right_pointer = stack.pop().unwrap();
        let left_pointer = stack.pop().unwrap();

        let left_length = get_list_length(left_pointer, memory);
        let right_length = get_list_length(right_pointer, memory);
        assert_eq!(left_length, right_length);
        let len = left_length;

        // Get pointer for pair list through dynamic allocator
        let (output_type, _) = &self.outputs()[0];
        let output_list_size = self.list_type.metadata_size() + len * output_type.stack_size();
        let output_pointer = dyn_malloc::dynamic_allocator(output_list_size, memory);

        new_list(output_pointer, len as u32, memory);
        set_list_len(output_pointer, len, memory);

        for i in 0..len {
            let left_item = get_element(left_pointer, i, memory, self.left_type.stack_size());
            let right_item = get_element(right_pointer, i, memory, self.right_type.stack_size());

            let pair = right_item.into_iter().chain(left_item).collect_vec();
            set_element(output_pointer, i, pair, memory);
        }

        stack.push(output_pointer);
    }

    fn pseudorandom_initial_state(
        &self,
        seed: [u8; 32],
        _bench_case: Option<snippet_bencher::BenchmarkCase>,
    ) -> FunctionInitialState {
        let mut rng: StdRng = SeedableRng::from_seed(seed);
        let list_len = rng.gen_range(0..20);
        let execution_state = self.generate_input_state(list_len, list_len);
        FunctionInitialState {
            stack: execution_state.stack,
            memory: execution_state.nondeterminism.ram,
        }
    }
}

impl Zip {
    fn generate_input_state(&self, left_length: usize, right_length: usize) -> ExecutionState {
        let fill_with_random_elements =
            |data_type: &DataType, list_pointer, list_len, memory: &mut _| match self.list_type {
                ListType::Safe => safe_insert_random_list(
                    data_type,
                    list_pointer,
                    list_len as u32,
                    list_len,
                    memory,
                ),
                ListType::Unsafe => untyped_unsafe_insert_random_list(
                    list_pointer,
                    list_len,
                    memory,
                    data_type.stack_size(),
                ),
            };

        let left_pointer = BFieldElement::new(0);
        let left_size = self.list_type.metadata_size() + left_length * self.left_type.stack_size();
        let right_pointer = left_pointer + BFieldElement::new(left_size as u64);

        let mut memory = HashMap::default();
        fill_with_random_elements(&self.left_type, left_pointer, left_length, &mut memory);
        fill_with_random_elements(&self.right_type, right_pointer, right_length, &mut memory);

        ExecutionState {
            stack: [empty_stack(), vec![left_pointer, right_pointer]].concat(),
            std_in: vec![],
            nondeterminism: NonDeterminism::default().with_ram(memory),
            words_allocated: 0,
        }
    }
}

#[cfg(test)]
mod tests {
    use proptest::collection::vec;
    use proptest::prelude::*;
    use proptest_arbitrary_interop::arb;
    use test_strategy::proptest;

    use crate::rust_shadowing_helper_functions::unsafe_list::*;
    use crate::structure::tasm_object::MemoryIter;
    use crate::traits::function::ShadowedFunction;
    use crate::traits::rust_shadow::RustShadow;

    use super::*;

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

    #[proptest]
    fn zipping_u32s_with_x_field_elements_correspond_to_bfieldcodec(
        left_list: Vec<u32>,
        #[strategy(vec(arb(), #left_list.len()))] right_list: Vec<XFieldElement>,
    ) {
        let left_type = DataType::U32;
        let right_type = DataType::Xfe;

        let left_pointer = BFieldElement::new(0);
        let right_pointer = BFieldElement::new(1 << 60); // far enough

        let mut ram = HashMap::default();
        write_unsafe_list_to_ram(&mut ram, left_pointer, &left_type, &left_list);
        write_unsafe_list_to_ram(&mut ram, right_pointer, &right_type, &right_list);

        let mut stack = [empty_stack(), vec![left_pointer, right_pointer]].concat();

        let zip = Zip {
            list_type: ListType::Unsafe,
            left_type,
            right_type,
        };
        zip.rust_shadow(&mut stack, &mut ram);

        let zipped = left_list.into_iter().zip_eq(right_list).collect_vec();
        let encoding = zipped.encode();

        let output_list_pointer = stack.pop().unwrap();
        let memory_iter = MemoryIter::new(&ram, output_list_pointer);
        let tasm_zip_result = memory_iter.take(encoding.len()).collect_vec();

        prop_assert_eq!(encoding, tasm_zip_result);
    }

    fn write_unsafe_list_to_ram<T: BFieldCodec + Copy>(
        ram: &mut HashMap<BFieldElement, BFieldElement>,
        list_pointer: BFieldElement,
        item_type: &DataType,
        list: &[T],
    ) {
        unsafe_list_new(list_pointer, ram);
        for &item in list {
            unsafe_list_push(list_pointer, item.encode(), ram, item_type.stack_size());
        }
    }
}

#[cfg(test)]
mod benches {
    use crate::traits::function::ShadowedFunction;
    use crate::traits::rust_shadow::RustShadow;

    use super::*;

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
