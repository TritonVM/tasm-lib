use std::collections::HashMap;

use num::{One, Zero};
use rand::{thread_rng, Rng};
use triton_vm::BFieldElement;

use crate::{
    get_init_tvm_stack,
    snippet::{DataType, Snippet},
    ExecutionState,
};

pub struct PushRamToStack {
    pub output_type: DataType,
}

impl PushRamToStack {
    fn get_init_state(&self, mut ram_address: BFieldElement) -> ExecutionState {
        let mut stack = get_init_tvm_stack();

        stack.push(ram_address);

        let rand_val = self.output_type.random_elements(1)[0].clone();
        let mut memory = HashMap::default();

        for word in rand_val {
            memory.insert(ram_address, word);
            ram_address.increment();
        }

        ExecutionState::with_stack_and_memory(stack, memory, 1)
    }
}

impl Snippet for PushRamToStack {
    fn entrypoint(&self) -> String {
        assert!(
            !self.output_type.get_size().is_zero(),
            "Cannot move value of size 0 to stack"
        );
        format!(
            "tasm_memory_push_ram_to_stack_{}",
            self.output_type.label_friendly_name()
        )
    }

    fn inputs(&self) -> Vec<String> {
        vec!["*ram_pointer".to_owned()]
    }

    fn input_types(&self) -> Vec<crate::snippet::DataType> {
        vec![DataType::VoidPointer]
    }

    fn output_types(&self) -> Vec<crate::snippet::DataType> {
        vec![self.output_type.to_owned()]
    }

    fn outputs(&self) -> Vec<String> {
        let mut ret = vec![];

        for i in (0..self.output_type.get_size()).rev() {
            ret.push(format!("element_{i}"));
        }

        ret
    }

    fn stack_diff(&self) -> isize {
        self.output_type.get_size() as isize - 1isize
    }

    fn function_code(&self, _library: &mut crate::library::Library) -> String {
        let entrypoint = self.entrypoint();

        let mut ram_to_stack_code = String::default();
        let dataype_size = self.output_type.get_size();
        for i in 0..dataype_size {
            ram_to_stack_code.push_str("read_mem \n");
            ram_to_stack_code.push_str("swap 1 \n");
            if i != dataype_size - 1 {
                ram_to_stack_code.push_str("push -1 \n");
                ram_to_stack_code.push_str("add \n");
            }
        }

        let move_pointer_to_last_word = if dataype_size == 1 {
            String::default()
        } else {
            let size_minus_one = dataype_size - 1;
            format!("push {size_minus_one} add\n")
        };

        format!("
                // BEFORE: _ *ram_addr
                // AFTER: _ [elements]
                {entrypoint}:
                    // Memory is read from high-to-low address. So move the pointer to the highest address of the value.
                    // _ *first_word

                    {move_pointer_to_last_word}
                    // _ *last_word

                    {ram_to_stack_code}
                    // _ [elements] *ram_pointer

                    pop

                    return
                ")
    }

    fn crash_conditions(&self) -> Vec<String> {
        vec![]
    }

    fn gen_input_states(&self) -> Vec<crate::ExecutionState> {
        let mut rng = thread_rng();
        let ram_address = BFieldElement::new(rng.gen_range(0..(1 << 31)));
        vec![Self::get_init_state(self, ram_address)]
    }

    fn common_case_input_state(&self) -> crate::ExecutionState {
        Self::get_init_state(self, BFieldElement::one())
    }

    fn worst_case_input_state(&self) -> crate::ExecutionState {
        Self::get_init_state(self, BFieldElement::new((1 << 31) + 127))
    }

    fn rust_shadowing(
        &self,
        stack: &mut Vec<triton_vm::BFieldElement>,
        _std_in: Vec<triton_vm::BFieldElement>,
        _secret_in: Vec<triton_vm::BFieldElement>,
        memory: &mut std::collections::HashMap<triton_vm::BFieldElement, triton_vm::BFieldElement>,
    ) {
        let dt_size = self.output_type.get_size();
        let ram_pointer = stack.pop().unwrap();

        for i in (0..dt_size).rev() {
            stack.push(memory[&(ram_pointer + BFieldElement::new(i as u64))]);
        }
    }
}

#[cfg(test)]
mod tests {
    use triton_vm::op_stack::NUM_OP_STACK_REGISTERS;

    use crate::{test_helpers::test_rust_equivalence_multiple, DIGEST_LENGTH};

    use super::*;

    #[test]
    fn push_ram_to_stack_test() {
        let ret_for_digest = test_rust_equivalence_multiple(
            &PushRamToStack {
                output_type: DataType::Digest,
            },
            true,
        );

        // sanity check
        let mut stack = ret_for_digest[0].final_stack.clone();
        for _ in 0..DIGEST_LENGTH {
            assert!(stack.pop().is_some());
        }

        // Below the digest values it must be all zeros
        for _ in 0..(NUM_OP_STACK_REGISTERS - DIGEST_LENGTH) {
            assert!(stack.pop().unwrap().is_zero());
        }

        test_rust_equivalence_multiple(
            &PushRamToStack {
                output_type: DataType::Bool,
            },
            true,
        );
        test_rust_equivalence_multiple(
            &PushRamToStack {
                output_type: DataType::U32,
            },
            true,
        );
        test_rust_equivalence_multiple(
            &PushRamToStack {
                output_type: DataType::U64,
            },
            true,
        );
        test_rust_equivalence_multiple(
            &PushRamToStack {
                output_type: DataType::U128,
            },
            true,
        );
        test_rust_equivalence_multiple(
            &PushRamToStack {
                output_type: DataType::VoidPointer,
            },
            true,
        );
        test_rust_equivalence_multiple(
            &PushRamToStack {
                output_type: DataType::BFE,
            },
            true,
        );
        test_rust_equivalence_multiple(
            &PushRamToStack {
                output_type: DataType::XFE,
            },
            true,
        );
        test_rust_equivalence_multiple(
            &PushRamToStack {
                output_type: DataType::XFE,
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
    fn memcpy_benchmark() {
        bench_and_write(PushRamToStack {
            output_type: DataType::Digest,
        });
        bench_and_write(PushRamToStack {
            output_type: DataType::Bool,
        });
        bench_and_write(PushRamToStack {
            output_type: DataType::U32,
        });
        bench_and_write(PushRamToStack {
            output_type: DataType::U64,
        });
        bench_and_write(PushRamToStack {
            output_type: DataType::U128,
        });
        bench_and_write(PushRamToStack {
            output_type: DataType::VoidPointer,
        });
        bench_and_write(PushRamToStack {
            output_type: DataType::BFE,
        });
        bench_and_write(PushRamToStack {
            output_type: DataType::XFE,
        });
        bench_and_write(PushRamToStack {
            output_type: DataType::XFE,
        });
    }
}
