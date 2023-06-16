use itertools::Itertools;

use num_traits::One;
use rand::random;
use triton_vm::BFieldElement;

use crate::{
    neptune::transaction::transaction_kernel::{
        example_transaction_kernel_encoded, input_state_with_kernel_in_memory,
        random_transaction_kernel_encoding,
    },
    snippet::{DataType, Snippet},
};

use super::transaction_kernel::{pseudorandom_transaction_kernel_encoding, TransactionKernelField};

/// Gets the size, in number of BFieldElements, of a named field from
/// a TransactionKernel object.
#[derive(Debug, Clone)]
pub struct GetTransactionKernelField(pub TransactionKernelField);

impl GetTransactionKernelField {
    fn skip_field(_field: TransactionKernelField) -> String {
        "
        read_mem add
        push 1 add
        "
        .to_string()
    }

    pub fn skip_fields_upto(field: TransactionKernelField) -> String {
        let predecessor_fields = match field {
            TransactionKernelField::Inputs => vec![],
            TransactionKernelField::Outputs => vec![TransactionKernelField::Inputs],
            TransactionKernelField::PubscriptHashesAndInputs => vec![
                TransactionKernelField::Inputs,
                TransactionKernelField::Outputs,
            ],
            TransactionKernelField::Fee => vec![
                TransactionKernelField::Inputs,
                TransactionKernelField::Outputs,
                TransactionKernelField::PubscriptHashesAndInputs,
            ],
            TransactionKernelField::Coinbase => vec![
                TransactionKernelField::Inputs,
                TransactionKernelField::Outputs,
                TransactionKernelField::PubscriptHashesAndInputs,
                TransactionKernelField::Fee,
            ],
            TransactionKernelField::Timestamp => vec![
                TransactionKernelField::Inputs,
                TransactionKernelField::Outputs,
                TransactionKernelField::PubscriptHashesAndInputs,
                TransactionKernelField::Fee,
                TransactionKernelField::Coinbase,
            ],
            TransactionKernelField::MutatorSetHash => vec![
                TransactionKernelField::Inputs,
                TransactionKernelField::Outputs,
                TransactionKernelField::PubscriptHashesAndInputs,
                TransactionKernelField::Fee,
                TransactionKernelField::Coinbase,
                TransactionKernelField::Timestamp,
            ],
        };
        predecessor_fields
            .into_iter()
            .map(Self::skip_field)
            .join("\n\n")
    }
}

impl Snippet for GetTransactionKernelField {
    fn entrypoint(&self) -> String {
        format!(
            "tasm_neptune_transaction_get_transaction_kernel_field_{}",
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
        vec![DataType::VoidPointer]
    }

    fn outputs(&self) -> Vec<String> {
        vec!["*addr".to_string()]
    }

    fn stack_diff(&self) -> isize {
        0
    }

    fn function_code(&self, _library: &mut crate::snippet_state::SnippetState) -> String {
        let entrypoint = self.entrypoint();
        let skip_fields = Self::skip_fields_upto(self.0);

        format!(
            "
        // BEFORE: _ *addr
        // AFTER: _ *field_addr
        {entrypoint}:
            {skip_fields}
            push 1 add
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

        if !matches!(self.0, TransactionKernelField::Inputs) {
            // inputs
            let inputs_size = *memory.get(&address).expect("could not read inputs size");
            address += one + inputs_size;
            if !matches!(self.0, TransactionKernelField::Outputs) {
                // outputs
                let outputs_size = *memory.get(&address).expect("could not read outputs size");
                address += one + outputs_size;
                if !matches!(self.0, TransactionKernelField::PubscriptHashesAndInputs) {
                    // pubscript_hashes_and_inputs
                    let pubscript_hashes_and_inputs_size = *memory
                        .get(&address)
                        .expect("could not read pubscripts size");
                    address += one + pubscript_hashes_and_inputs_size;
                    if !matches!(self.0, TransactionKernelField::Fee) {
                        // fee
                        let fee_size = *memory.get(&address).expect("could not read fee size");
                        address += one + fee_size;
                        if !matches!(self.0, TransactionKernelField::Coinbase) {
                            // coinbase
                            let coinbase_size =
                                *memory.get(&address).expect("could not read coinbase size");
                            address += one + coinbase_size;
                            // timestamp
                            if !matches!(self.0, TransactionKernelField::Timestamp) {
                                let timestamp_size =
                                    *memory.get(&address).expect("could not read timestamp size");
                                address += one + timestamp_size;
                                if !matches!(self.0, TransactionKernelField::MutatorSetHash) {
                                    // mutator_set_hash
                                    let mutator_set_hash_size = *memory
                                        .get(&address)
                                        .expect("could not read mutator set hash size");
                                    address += one + mutator_set_hash_size;
                                    unreachable!("Enum field 0 does not match any variant.");
                                } else {
                                    address += one;
                                }
                            } else {
                                address += one;
                            }
                        } else {
                            address += one;
                        }
                    } else {
                        address += one;
                    }
                } else {
                    address += one;
                }
            } else {
                address += one;
            }
        } else {
            address += one;
        }

        // write resulting address back
        stack.push(address)
    }
}

#[cfg(test)]
mod tests {
    use crate::{snippet_bencher::bench_and_write, test_helpers::rust_tasm_equivalence_prop_new};

    use super::{GetTransactionKernelField, TransactionKernelField};

    #[test]
    fn new_prop_test() {
        rust_tasm_equivalence_prop_new(
            &GetTransactionKernelField(TransactionKernelField::Inputs),
            true,
        );
        rust_tasm_equivalence_prop_new(
            &GetTransactionKernelField(TransactionKernelField::Outputs),
            true,
        );
        rust_tasm_equivalence_prop_new(
            &GetTransactionKernelField(TransactionKernelField::PubscriptHashesAndInputs),
            true,
        );
        rust_tasm_equivalence_prop_new(
            &GetTransactionKernelField(TransactionKernelField::Fee),
            true,
        );
        rust_tasm_equivalence_prop_new(
            &GetTransactionKernelField(TransactionKernelField::Coinbase),
            true,
        );
        rust_tasm_equivalence_prop_new(
            &GetTransactionKernelField(TransactionKernelField::Timestamp),
            true,
        );
        rust_tasm_equivalence_prop_new(
            &GetTransactionKernelField(TransactionKernelField::MutatorSetHash),
            true,
        );
    }

    #[test]
    fn get_transaction_kernel_field_benchmark() {
        bench_and_write(GetTransactionKernelField(TransactionKernelField::Inputs));
        bench_and_write(GetTransactionKernelField(TransactionKernelField::Outputs));
        bench_and_write(GetTransactionKernelField(
            TransactionKernelField::PubscriptHashesAndInputs,
        ));
        bench_and_write(GetTransactionKernelField(TransactionKernelField::Fee));
        bench_and_write(GetTransactionKernelField(TransactionKernelField::Coinbase));
        bench_and_write(GetTransactionKernelField(TransactionKernelField::Timestamp));
        bench_and_write(GetTransactionKernelField(
            TransactionKernelField::MutatorSetHash,
        ));
    }
}
