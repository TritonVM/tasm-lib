use triton_vm::prelude::*;

use crate::data_type::DataType;
use crate::library::Library;
use crate::memory::dyn_malloc;
use crate::memory::memcpy;
use crate::traits::basic_snippet::BasicSnippet;

/// Mutates an existing vector by reducing its length to `at` and returns the new vector. Mirrors
/// the behavior of the Rust method `Vec::split_off`.
#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct SplitOff {
    pub element_type: DataType,
}

impl SplitOff {
    fn self_type(&self) -> DataType {
        DataType::List(Box::new(self.element_type.to_owned()))
    }
}

impl BasicSnippet for SplitOff {
    fn inputs(&self) -> Vec<(DataType, String)> {
        vec![
            (self.self_type(), "self".to_owned()),
            (DataType::U32, "at".to_owned()),
        ]
    }

    fn outputs(&self) -> Vec<(DataType, String)> {
        vec![(self.self_type(), "other".to_owned())]
    }

    fn entrypoint(&self) -> String {
        format!(
            "tasm_list_split_off_{}",
            self.element_type.label_friendly_name()
        )
    }

    fn code(&self, library: &mut Library) -> Vec<LabelledInstruction> {
        let entrypoint = self.entrypoint();
        let element_size = self.element_type.stack_size();
        let dyn_malloc = library.import(Box::new(dyn_malloc::DynMalloc));
        let mem_cpy = library.import(Box::new(memcpy::MemCpy));

        triton_asm!(
            {entrypoint}:
                // _ *list at

                // Get original length
                dup 1
                read_mem 1
                pop 1
                // _ *list at original_length

                // Assert that `at <= original_length`
                dup 1
                dup 1
                // _ *list at original_length at original_length

                lt
                // _ *list at original_length (at > original_length)

                push 0
                eq
                // _ *list at original_length (at <= original_length)

                assert

                // Write new length
                dup 1
                dup 3
                // _ *list at original_length at *list

                write_mem 1
                // _ *list at original_length *word_0

                // Jump to read source
                push {element_size}
                dup 3
                mul
                add
                // _ *list at original_length (*word_0 + (at * element_size))
                // _ *list at original_length *read_source

                call {dyn_malloc}
                // _ *list at original_length *read_source *new_vec

                // Write length of new vector
                dup 2
                dup 4
                push -1
                mul
                add
                // _ *list at original_length *read_source *new_vec (original_length - at)
                // _ *list at original_length *read_source *new_vec new_length

                dup 0
                // _ *list at original_length *read_source *new_vec new_length new_length

                swap 2
                // _ *list at original_length *read_source new_length new_length *new_vec

                dup 0
                // _ *list at original_length *read_source new_length new_length *new_vec *new_vec

                swap 7
                pop 1
                // _ *new_vec at original_length *read_source new_length new_length *new_vec

                write_mem 1
                // _ *new_vec at original_length *read_source new_length *write_dest

                swap 1
                // _ *new_vec at original_length *read_source *write_dest new_length

                push {element_size}
                mul
                // _ *new_vec at original_length *read_source *write_dest new_size

                call {mem_cpy}
                // _ *new_vec at original_length

                pop 2
                // _ *new_vec

                return
        )
    }
}

#[cfg(test)]
mod tests {
    use std::cell::RefCell;
    use std::collections::HashMap;
    use std::rc::Rc;

    use rand::rngs::StdRng;
    use rand::thread_rng;
    use rand::Rng;
    use rand::SeedableRng;

    use crate::execute_with_terminal_state;
    use crate::linker::link_for_isolated_run;
    use crate::rust_shadowing_helper_functions::dyn_malloc::dynamic_allocator;
    use crate::rust_shadowing_helper_functions::list::insert_random_list;
    use crate::rust_shadowing_helper_functions::list::list_set_length;
    use crate::rust_shadowing_helper_functions::list::load_list_unstructured;
    use crate::snippet_bencher::BenchmarkCase;
    use crate::traits::function::Function;
    use crate::traits::function::FunctionInitialState;
    use crate::traits::function::ShadowedFunction;
    use crate::traits::rust_shadow::RustShadow;

    use super::*;

    #[test]
    fn split_off_pbt() {
        for element_type in [
            DataType::U32,
            DataType::U64,
            DataType::Xfe,
            DataType::U128,
            DataType::Digest,
        ] {
            ShadowedFunction::new(SplitOff { element_type }).test()
        }
    }

    #[test]
    fn split_off_negative_test() {
        let element_type = DataType::Xfe;
        let snippet = SplitOff {
            element_type: element_type.clone(),
        };
        let mut init_stack = snippet.init_stack_for_isolated_run();
        let code = link_for_isolated_run(Rc::new(RefCell::new(snippet.clone())));

        let mut memory = HashMap::default();
        let mut rng = thread_rng();
        let list_pointer: BFieldElement = rng.gen();
        init_stack.push(list_pointer);
        let list_length = rng.gen_range(0..30);
        let at = list_length + 1;
        init_stack.push(BFieldElement::new(at as u64));
        insert_random_list(&element_type, list_pointer, list_length, &mut memory);

        let rust_result = std::panic::catch_unwind(|| {
            let mut rust_stack = init_stack.clone();
            ShadowedFunction::new(snippet).rust_shadow_wrapper(
                &[],
                &NonDeterminism::default().with_ram(memory.clone()),
                &mut rust_stack,
                &mut memory.clone(),
                &mut None,
            )
        });

        let program = Program::new(&code);
        let tvm_result = execute_with_terminal_state(
            &program,
            &[],
            &init_stack,
            &NonDeterminism::default().with_ram(memory),
            None,
        );

        assert!(
            rust_result.is_err() && tvm_result.is_err(),
            "Test case: split_off for list length={list_length} `at`={at} must fail"
        );
        let err = tvm_result.unwrap_err();
        assert_eq!(InstructionError::AssertionFailed, err);
    }

    impl Function for SplitOff {
        fn rust_shadow(
            &self,
            stack: &mut Vec<BFieldElement>,
            memory: &mut HashMap<BFieldElement, BFieldElement>,
        ) {
            let at: u32 = stack.pop().unwrap().try_into().unwrap();
            let self_vec_pointer = stack.pop().unwrap();
            let mut self_vec =
                load_list_unstructured(self.element_type.stack_size(), self_vec_pointer, memory);
            list_set_length(self_vec_pointer, at as usize, memory);

            let other_vec = self_vec.split_off(at as usize);
            let other_vec_pointer = dynamic_allocator(memory);
            stack.push(other_vec_pointer);

            list_set_length(self_vec_pointer, self_vec.len(), memory);
            list_set_length(other_vec_pointer, other_vec.len(), memory);

            let mut other_vec_word_pointer = other_vec_pointer;
            other_vec_word_pointer.increment();

            for elem in other_vec.iter() {
                for word in elem.iter() {
                    memory.insert(other_vec_word_pointer, *word);
                    other_vec_word_pointer.increment();
                }
            }
        }

        fn pseudorandom_initial_state(
            &self,
            seed: [u8; 32],
            bench_case: Option<BenchmarkCase>,
        ) -> FunctionInitialState {
            let mut rng: StdRng = StdRng::from_seed(seed);
            let mut init_memory = HashMap::default();
            let (list_length, at) = match bench_case {
                Some(BenchmarkCase::CommonCase) => (100, 50),
                Some(BenchmarkCase::WorstCase) => (1000, 0),
                None => {
                    let list_length = rng.gen_range(1..1000);
                    (list_length, rng.gen_range(0..list_length))
                }
            };
            let list_pointer = rng.gen();
            insert_random_list(
                &self.element_type,
                list_pointer,
                list_length,
                &mut init_memory,
            );
            let init_stack = [
                self.init_stack_for_isolated_run(),
                vec![list_pointer, BFieldElement::new(at as u64)],
            ]
            .concat();

            FunctionInitialState {
                stack: init_stack,
                memory: init_memory,
            }
        }
    }
}

#[cfg(test)]
mod benches {
    use crate::traits::function::ShadowedFunction;
    use crate::traits::rust_shadow::RustShadow;

    use super::*;

    #[test]
    fn split_off_bench() {
        ShadowedFunction::new(SplitOff {
            element_type: DataType::Xfe,
        })
        .bench();
    }
}
