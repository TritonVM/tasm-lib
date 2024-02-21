use std::collections::HashMap;

use num_traits::Zero;
use rand::random;
use triton_vm::prelude::*;

use crate::data_type::DataType;
use crate::empty_stack;
use crate::memory::encode_to_memory;
use crate::snippet_bencher::BenchmarkCase;
use crate::traits::basic_snippet::BasicSnippet;
use crate::traits::function::Function;
use crate::traits::function::FunctionInitialState;

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

    fn code(&self, _library: &mut crate::library::Library) -> Vec<LabelledInstruction> {
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
        stack: &mut Vec<BFieldElement>,
        memory: &mut HashMap<BFieldElement, BFieldElement>,
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
        let address: BFieldElement = random();
        let value = self.data_type.random_elements(1)[0].clone();

        Self::init_state(address, value)
    }

    fn corner_case_initial_states(&self) -> Vec<FunctionInitialState> {
        let address = BFieldElement::zero();
        let value = self.data_type.random_elements(1)[0].clone();
        let pointer_is_zero = Self::init_state(address, value);

        vec![pointer_is_zero]
    }
}

impl PushRamToStack {
    fn init_state<T: BFieldCodec>(address: BFieldElement, value: T) -> FunctionInitialState {
        let mut memory = HashMap::<BFieldElement, BFieldElement>::new();
        encode_to_memory(&mut memory, address, value);
        let mut stack = empty_stack();
        stack.push(address);

        FunctionInitialState { stack, memory }
    }
}

#[cfg(test)]
mod tests {
    use num_traits::One;

    use crate::test_helpers::tasm_final_state;
    use crate::traits::function::ShadowedFunction;
    use crate::traits::rust_shadow::RustShadow;

    use super::*;

    #[test]
    fn push_ram_to_stack_test() {
        for data_type in DataType::big_random_generatable_type_collection() {
            ShadowedFunction::new(PushRamToStack { data_type }).test();
        }
    }

    #[test]
    fn u64_test() {
        let u64_snippet = PushRamToStack {
            data_type: DataType::U64,
        };
        let value_stored_to_memory: u64 = (1u64 << 46) + 3;
        let init_state = PushRamToStack::init_state(BFieldElement::one(), value_stored_to_memory);
        let mut final_state = tasm_final_state(
            &ShadowedFunction::new(u64_snippet),
            &init_state.stack,
            &[],
            NonDeterminism::default().with_ram(init_state.memory),
            &None,
        );

        let u64_lo = final_state.final_stack.pop().unwrap().value();
        let u64_hi = final_state.final_stack.pop().unwrap().value();

        assert_eq!((1u64 << 46) + 3, (u64_hi << 32) + u64_lo);
    }
}

#[cfg(test)]
mod benches {
    use crate::traits::function::ShadowedFunction;
    use crate::traits::rust_shadow::RustShadow;

    use super::*;

    #[test]
    fn push_ram_to_stack_bench() {
        ShadowedFunction::new(PushRamToStack {
            data_type: DataType::Digest,
        })
        .bench();
    }
}
