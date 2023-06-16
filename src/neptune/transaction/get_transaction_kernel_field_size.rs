use num_traits::One;
use rand::random;
use triton_vm::BFieldElement;

use crate::{
    neptune::transaction::{
        get_transaction_kernel_field::GetTransactionKernelField,
        transaction_kernel::{
            example_transaction_kernel_encoded, input_state_with_kernel_in_memory,
            random_transaction_kernel_encoding,
        },
    },
    snippet::{DataType, Snippet},
};

use super::transaction_kernel::{pseudorandom_transaction_kernel_encoding, TransactionKernelField};

/// Gets a named field from a TransactionKernel object.
#[derive(Debug, Clone)]
pub struct GetTransactionKernelFieldSize(pub TransactionKernelField);

impl GetTransactionKernelFieldSize {
    fn local_field_size(_field: TransactionKernelField) -> String {
        "
        // _ *[field_addr-1]
        read_mem // _ *[field_addr-1] field_size
        swap 1 pop // _ field_size
        "
        .to_string()
    }
}

impl Snippet for GetTransactionKernelFieldSize {
    fn entrypoint(&self) -> String {
        format!(
            "tasm_neptune_transaction_get_transaction_kernel_field_size_{}",
            self.0
        )
    }

    fn inputs(&self) -> Vec<String> {
        vec!["*addr".to_string()]
    }

    fn input_types(&self) -> Vec<crate::snippet::DataType> {
        vec![DataType::VoidPointer]
    }

    fn output_types(&self) -> Vec<crate::snippet::DataType> {
        vec![DataType::U32]
    }

    fn outputs(&self) -> Vec<String> {
        vec!["*addr".to_string()]
    }

    fn stack_diff(&self) -> isize {
        0
    }

    fn function_code(&self, _library: &mut crate::snippet_state::SnippetState) -> String {
        let entrypoint = self.entrypoint();
        let skip_fields = GetTransactionKernelField::skip_fields_upto(self.0);
        let local_size = Self::local_field_size(self.0);

        format!(
            "
        // BEFORE: _ *addr
        // AFTER: _ field_size
        {entrypoint}:
            {skip_fields}
            {local_size}
            return
            "
        )
    }

    fn crash_conditions(&self) -> Vec<String> {
        vec!["Transaction kernel is improperly formatted in memory.".to_string()]
    }

    fn gen_input_states(&self) -> Vec<crate::ExecutionState> {
        vec![
            input_state_with_kernel_in_memory(random(), &random_transaction_kernel_encoding()),
            input_state_with_kernel_in_memory(random(), &random_transaction_kernel_encoding()),
            input_state_with_kernel_in_memory(random(), &random_transaction_kernel_encoding()),
            input_state_with_kernel_in_memory(random(), &random_transaction_kernel_encoding()),
            input_state_with_kernel_in_memory(random(), &example_transaction_kernel_encoded()),
        ]
    }

    fn common_case_input_state(&self) -> crate::ExecutionState {
        let mut seed: [u8; 32] = [0u8; 32];
        seed[0] = 0xba;
        seed[1] = 0xdd;
        seed[2] = 0xbe;
        seed[3] = 0xef;
        input_state_with_kernel_in_memory(
            BFieldElement::new(1),
            &pseudorandom_transaction_kernel_encoding(seed, 360, 2, 500),
        )
    }

    fn worst_case_input_state(&self) -> crate::ExecutionState {
        let mut seed: [u8; 32] = [0u8; 32];
        seed[0] = 0xba;
        seed[1] = 0xdd;
        seed[2] = 0xbe;
        seed[3] = 0xef;
        input_state_with_kernel_in_memory(
            BFieldElement::new(1),
            &pseudorandom_transaction_kernel_encoding(seed, 1000, 5, 1500),
        )
    }

    fn rust_shadowing(
        &self,
        stack: &mut Vec<triton_vm::BFieldElement>,
        _std_in: Vec<triton_vm::BFieldElement>,
        _secret_in: Vec<triton_vm::BFieldElement>,
        memory: &mut std::collections::HashMap<triton_vm::BFieldElement, triton_vm::BFieldElement>,
    ) {
        // read address
        let mut address = stack.pop().unwrap();

        let one = BFieldElement::one();

        let field_size = {
            let mut size = memory.get(&address).unwrap().value() as usize;
            if matches!(self.0, TransactionKernelField::Inputs) {
                size
            } else {
                address += one + BFieldElement::new(size as u64);
                size = memory.get(&address).unwrap().value() as usize;
                if matches!(self.0, TransactionKernelField::Outputs) {
                    size
                } else {
                    address += one + BFieldElement::new(size as u64);
                    size = memory.get(&address).unwrap().value() as usize;
                    if matches!(self.0, TransactionKernelField::PubscriptHashesAndInputs) {
                        size
                    } else {
                        address += one + BFieldElement::new(size as u64);
                        size = memory.get(&address).unwrap().value() as usize;
                        if matches!(self.0, TransactionKernelField::Fee) {
                            size
                        } else {
                            address += one + BFieldElement::new(size as u64);
                            size = memory.get(&address).unwrap().value() as usize;
                            if matches!(self.0, TransactionKernelField::Coinbase) {
                                size
                            } else {
                                address += one + BFieldElement::new(size as u64);
                                size = memory.get(&address).unwrap().value() as usize;
                                if matches!(self.0, TransactionKernelField::Timestamp) {
                                    size
                                } else {
                                    address += one + BFieldElement::new(size as u64);
                                    size = memory.get(&address).unwrap().value() as usize;
                                    if matches!(self.0, TransactionKernelField::MutatorSetHash) {
                                        size
                                    } else {
                                        unreachable!("enum field does not match any enum variants");
                                    }
                                }
                            }
                        }
                    }
                }
            }
        };

        // write resulting address back
        stack.push(BFieldElement::new(field_size as u64));
    }
}

#[cfg(test)]
mod tests {
    use crate::{snippet_bencher::bench_and_write, test_helpers::rust_tasm_equivalence_prop_new};

    use super::{GetTransactionKernelFieldSize, TransactionKernelField};

    #[test]
    fn new_prop_test() {
        rust_tasm_equivalence_prop_new(
            &GetTransactionKernelFieldSize(TransactionKernelField::Inputs),
            true,
        );
        rust_tasm_equivalence_prop_new(
            &GetTransactionKernelFieldSize(TransactionKernelField::Outputs),
            true,
        );
        rust_tasm_equivalence_prop_new(
            &GetTransactionKernelFieldSize(TransactionKernelField::PubscriptHashesAndInputs),
            true,
        );
        rust_tasm_equivalence_prop_new(
            &GetTransactionKernelFieldSize(TransactionKernelField::Fee),
            true,
        );
        rust_tasm_equivalence_prop_new(
            &GetTransactionKernelFieldSize(TransactionKernelField::Coinbase),
            true,
        );
        rust_tasm_equivalence_prop_new(
            &GetTransactionKernelFieldSize(TransactionKernelField::Timestamp),
            true,
        );
        rust_tasm_equivalence_prop_new(
            &GetTransactionKernelFieldSize(TransactionKernelField::MutatorSetHash),
            true,
        );
    }

    #[test]
    fn get_transaction_kernel_field_size_benchmark() {
        bench_and_write(GetTransactionKernelFieldSize(
            TransactionKernelField::Inputs,
        ));
        bench_and_write(GetTransactionKernelFieldSize(
            TransactionKernelField::Outputs,
        ));
        bench_and_write(GetTransactionKernelFieldSize(
            TransactionKernelField::PubscriptHashesAndInputs,
        ));
        bench_and_write(GetTransactionKernelFieldSize(TransactionKernelField::Fee));
        bench_and_write(GetTransactionKernelFieldSize(
            TransactionKernelField::Coinbase,
        ));
        bench_and_write(GetTransactionKernelFieldSize(
            TransactionKernelField::Timestamp,
        ));
        bench_and_write(GetTransactionKernelFieldSize(
            TransactionKernelField::MutatorSetHash,
        ));
    }
}
