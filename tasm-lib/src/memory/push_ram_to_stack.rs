use std::collections::HashMap;

use rand::random;
use triton_vm::{triton_asm, BFieldElement};

use crate::{
    data_type::DataType,
    empty_stack,
    snippet_bencher::BenchmarkCase,
    structure::tasm_object::encode_to_memory,
    traits::{
        basic_snippet::BasicSnippet,
        function::{Function, FunctionInitialState},
    },
};

#[derive(Clone, Debug)]
pub struct PushRamToStack {
    pub data_type: DataType,
}

impl BasicSnippet for PushRamToStack {
    fn inputs(&self) -> Vec<(DataType, String)> {
        vec![(DataType::VoidPointer, "*first_word".to_owned())]
    }

    fn outputs(&self) -> Vec<(DataType, String)> {
        vec![(self.data_type.to_owned(), "value".to_owned())]
    }

    fn entrypoint(&self) -> String {
        format!(
            "tasm_memory_push_ram_to_stack___{}",
            self.data_type.label_friendly_name()
        )
    }

    fn code(
        &self,
        _library: &mut crate::library::Library,
    ) -> Vec<triton_vm::instruction::LabelledInstruction> {
        let entrypoint = self.entrypoint();
        let stack_size = self.data_type.stack_size();
        let increment_pointer_to_last_word = if stack_size == 0 {
            triton_asm!()
        } else {
            triton_asm!(
            push {self.data_type.stack_size() - 1}
            add
            )
        };

        let read_all_words = self.data_type.read_value_from_memory_leave_pointer();

        triton_asm!(
            {entrypoint}:
                // _ *first_word

                {&increment_pointer_to_last_word}
                // _ *last_word

                {&read_all_words}
                // _ [value] (*first_word - 1)

                pop 1
                // _ [value]

                return
        )
    }
}

impl Function for PushRamToStack {
    fn rust_shadow(
        &self,
        stack: &mut Vec<triton_vm::BFieldElement>,
        memory: &mut std::collections::HashMap<triton_vm::BFieldElement, triton_vm::BFieldElement>,
    ) {
        let first_word = stack.pop().unwrap();
        let stack_size = self.data_type.stack_size();
        let last_word = first_word + BFieldElement::new(self.data_type.stack_size() as u64 - 1);

        let mut pointer = last_word;
        for _ in 0..stack_size {
            stack.push(memory[&pointer]);
            pointer.decrement();
        }
    }

    fn pseudorandom_initial_state(
        &self,
        _seed: [u8; 32],
        _bench_case: Option<BenchmarkCase>,
    ) -> FunctionInitialState {
        let mut memory = HashMap::<BFieldElement, BFieldElement>::new();
        let address: BFieldElement = random();
        let value = self.data_type.random_elements(1)[0].clone();
        encode_to_memory(&mut memory, address, value);
        let mut stack = empty_stack();
        stack.push(address);

        FunctionInitialState { stack, memory }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::traits::function::ShadowedFunction;
    use crate::traits::rust_shadow::RustShadow;

    #[test]
    fn push_ram_to_stack_test() {
        for data_type in DataType::big_random_generatable_type_collection() {
            ShadowedFunction::new(PushRamToStack { data_type }).test();
        }
    }
}

#[cfg(test)]
mod benches {
    use super::*;
    use crate::traits::function::ShadowedFunction;
    use crate::traits::rust_shadow::RustShadow;

    #[test]
    fn push_ram_to_stack_bench() {
        ShadowedFunction::new(PushRamToStack {
            data_type: DataType::Digest,
        })
        .bench();
    }
}
