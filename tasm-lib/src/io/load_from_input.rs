use std::collections::HashMap;

use rand::{rngs::StdRng, Rng, SeedableRng};
use triton_vm::{instruction::LabelledInstruction, triton_asm, BFieldElement, NonDeterminism};
use twenty_first::shared_math::other::random_elements;

use crate::data_type::DataType;
use crate::{
    empty_stack,
    memory::dyn_malloc,
    procedure::Procedure,
    snippet::{BasicSnippet, InputSource},
    snippet_bencher::BenchmarkCase,
};

#[derive(Clone, Debug)]
pub struct LoadFromInput(pub InputSource);

impl LoadFromInput {
    /// Return the instruction for reading n words. n can be 1..=5.
    fn read_n_words_instruction(&self, n: usize) -> Vec<LabelledInstruction> {
        match self.0 {
            InputSource::StdIn => triton_asm!(read_io { n }),
            InputSource::SecretIn => triton_asm!(divine { n }),
        }
    }

    fn read_n_words_and_store_in_memory_subroutine(
        &self,
        n: usize,
    ) -> (Vec<LabelledInstruction>, String) {
        let label = format!("{}_read_{}_words_label", self.entrypoint(), n);
        let code = triton_asm!(
            // BEFORE: _ *elem (n \in [0, 4])
            // AFTER: _ (*last_elem + 1) 0
            {label}:
                pop 1
                {&self.read_n_words_instruction(n)}
                // _ *elem [read_words]

                push 0
                swap {n + 1}
                // _ 0 [read_words] *elem

                write_mem {n}
                // _ 0 (*last_elem + 1)

                swap 1
                // _ (*last_elem + 1) 0

                return
        );

        (code, label)
    }
}

impl BasicSnippet for LoadFromInput {
    fn inputs(&self) -> Vec<(DataType, String)> {
        vec![]
    }

    fn outputs(&self) -> Vec<(DataType, String)> {
        vec![(DataType::VoidPointer, "pointer_to_loaded_data".to_owned())]
    }

    fn entrypoint(&self) -> String {
        format!("tasm_io_load_from_input_{}", self.0)
    }

    fn code(
        &self,
        library: &mut crate::library::Library,
    ) -> Vec<triton_vm::instruction::LabelledInstruction> {
        let entrypoint = self.entrypoint();
        let read_5_words_loop_label = format!("{entrypoint}_read_5_words_loop");
        let dyn_alloc = library.import(Box::new(dyn_malloc::DynMalloc));

        let (read_4_words_code, read_4_words_label) =
            self.read_n_words_and_store_in_memory_subroutine(4);
        let (read_3_words_code, read_3_words_label) =
            self.read_n_words_and_store_in_memory_subroutine(3);
        let (read_2_words_code, read_2_words_label) =
            self.read_n_words_and_store_in_memory_subroutine(2);
        let (read_1_words_code, read_1_words_label) =
            self.read_n_words_and_store_in_memory_subroutine(1);

        let read_5_words_loop_code = triton_asm!(
            // Invariant: _ *elem remaining_size
            {read_5_words_loop_label}:
                push 5
                dup 1
                lt
                // _ *elem remaining_size remaining_size < 5

                skiz return
                // _ *elem remaining_size

                push -5
                add
                // _ *elem remaining_size'

                {&self.read_n_words_instruction(5)}
                // _ *elem remaining_size' e4 e3 e2 e1 e0

                push 0
                swap 7
                // _ 0 remaining_size' e4 e3 e2 e1 e0 *elem

                write_mem 5
                // _ 0 remaining_size' *elem'

                swap 2
                pop 1
                // _ *elem' remaining_size'

                recurse
        );

        triton_asm!(
            // BEFORE: _
            // AFTER: _ *addr
            {entrypoint}:

                {&self.read_n_words_instruction(1)}
                // _ size

                // allocate memory for the input, including its own length indicator
                dup 0
                push 1
                add
                // _size (size + 1)

                call {dyn_alloc}
                // _ size *addr

                swap 1
                // _ *addr size

                dup 0
                dup 2
                // _ *addr size size *addr

                // store the size indicator in the first element of dedicated memory
                write_mem 1
                // _ *addr size (*addr + 1)
                // _ *addr size *first_element

                swap 1
                // _ *addr *first_element size

                call {read_5_words_loop_label}
                // _ *addr *elem remaining_size

                dup 0 push 4 eq
                // _ *addr *elem remaining_size (4 == remaining_size)

                skiz
                    call {read_4_words_label}

                dup 0 push 3 eq
                skiz
                    call {read_3_words_label}

                dup 0 push 2 eq
                skiz
                    call {read_2_words_label}

                dup 0 push 1 eq
                skiz
                    call {read_1_words_label}

                // _ *addr (*last_elem + 1) 0
                pop 2

                // _ *addr
                return

            {&read_5_words_loop_code}
            {&read_4_words_code}
            {&read_3_words_code}
            {&read_2_words_code}
            {&read_1_words_code}

        )
    }
}

impl Procedure for LoadFromInput {
    fn rust_shadow(
        &self,
        stack: &mut Vec<BFieldElement>,
        memory: &mut std::collections::HashMap<BFieldElement, BFieldElement>,
        nondeterminism: &NonDeterminism<BFieldElement>,
        public_input: &[BFieldElement],
        _sponge_state: &mut Option<crate::VmHasherState>,
    ) -> Vec<BFieldElement> {
        let input = match self.0 {
            InputSource::StdIn => public_input,
            InputSource::SecretIn => &nondeterminism.individual_tokens,
        };

        let indicated_length = input[0].value() as usize;
        let pointer = crate::rust_shadowing_helper_functions::dyn_malloc::dynamic_allocator(
            indicated_length + 1,
            memory,
        );

        for (i, value_from_input) in input.iter().enumerate() {
            let addr = pointer + BFieldElement::new(i as u64);
            memory.insert(addr, *value_from_input);
        }

        stack.push(pointer);

        vec![]
    }

    fn pseudorandom_initial_state(
        &self,
        seed: [u8; 32],
        bench_case: Option<crate::snippet_bencher::BenchmarkCase>,
    ) -> (
        Vec<BFieldElement>,
        std::collections::HashMap<BFieldElement, BFieldElement>,
        NonDeterminism<BFieldElement>,
        Vec<BFieldElement>,
        Option<crate::VmHasherState>,
    ) {
        let mut rng: StdRng = SeedableRng::from_seed(seed);
        let length = match bench_case {
            Some(BenchmarkCase::CommonCase) => 100,
            Some(BenchmarkCase::WorstCase) => 5000,
            None => rng.gen_range(0..(1 << 10)) as u64,
        };
        let input: Vec<BFieldElement> = [
            vec![BFieldElement::new(length)],
            random_elements(length as usize),
        ]
        .concat();

        let (stdin, secin) = match self.0 {
            InputSource::StdIn => (input, vec![]),
            InputSource::SecretIn => (vec![], input),
        };

        let stack = empty_stack();
        (
            stack,
            HashMap::default(),
            NonDeterminism {
                individual_tokens: secin,
                digests: vec![],
                ram: HashMap::default(),
            },
            stdin,
            None,
        )
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::{procedure::ShadowedProcedure, snippet::RustShadow};

    #[test]
    fn test() {
        for _ in 0..4 {
            ShadowedProcedure::new(LoadFromInput(InputSource::StdIn)).test();
            ShadowedProcedure::new(LoadFromInput(InputSource::SecretIn)).test();
        }
    }
}

#[cfg(test)]
mod benches {
    use super::*;
    use crate::{procedure::ShadowedProcedure, snippet::RustShadow};

    #[test]
    fn load_from_input_bench() {
        ShadowedProcedure::new(LoadFromInput(InputSource::StdIn)).bench();
    }
}
