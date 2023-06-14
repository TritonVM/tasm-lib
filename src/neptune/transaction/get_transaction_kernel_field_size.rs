use std::collections::HashMap;

use rand::random;
use triton_vm::BFieldElement;

use crate::{
    get_init_tvm_stack,
    neptune::transaction::{
        get_transaction_kernel_field::GetTransactionKernelField,
        transaction_kernel::random_transaction_kernel_encoding,
    },
    snippet::{DataType, Snippet},
    ExecutionState, DIGEST_LENGTH,
};

use super::transaction_kernel::{pseudorandom_transaction_kernel_encoding, TransactionKernelField};

/// Gets a named field from a TransactionKernel object.
#[derive(Debug, Clone)]
pub struct GetTransactionKernelFieldSize(pub TransactionKernelField);

impl GetTransactionKernelFieldSize {
    fn local_field_size(field: TransactionKernelField) -> String {
        match field {
            TransactionKernelField::Inputs => {
                // The field `inputs` is a `Vec` of `RemovalRecord`s, whose
                // sizes are not known at compile time because of SWBF
                // authentication paths.
                // So the entire vector is prepended with its length in
                // BFieldElements. Yay!
                "
                read_mem // _ *inputs len
                push 1 add // _ *inputs len+1
                swap 1 pop // _ len+1
                "
                .to_string()
            }
            TransactionKernelField::Outputs => {
                // The field `outputs` is a `Vec` of `AdditionRecord`s, which
                // are just wrapped `Digest`s and have lengths known at compile
                // time.
                // So the vector is prepended with the number of elements.
                format!(
                    "
                read_mem // _ *outputs num_outputs
                push {DIGEST_LENGTH} // _ *outputs num_outputs digest_length
                mul // _ *outputs len
                push 1 add // _ *outputs len+1
                swap 1 pop"
                )
            }
            TransactionKernelField::PubscriptHashesAndInputs => {
                // The field `pubscript_hashes_and_inputs` is a `Vec` of pairs
                // of `Digest` and `Vec<BFieldElement>`, the latter of which has
                // length unknown at compile time.
                // So the entire vector is prepended with its length in
                // BFieldElements. Yay!
                "
                read_mem // _ *pubscript_hashes_and_inputs len
                push 1 add // _ *pubscript_hashes_and_input len+1
                swap 1 pop
                "
                .to_string()
            }
            TransactionKernelField::Fee => {
                // The field `fee` is an `Amount`, which currently consists of 4
                // U32s. But this will change in the future to something unknown
                // at compile time!
                "
                push 4 // _ *fee 4
                swap 1 pop
                "
                .to_string()
            }
            TransactionKernelField::Coinbase => {
                // The field `coinbase` is an `Option` of `Amount`, so its length
                // is not known at compile time. So we have to read the length
                // indicator (which can be 0 or 1) and then add 4 (current size of
                // `Amount`) if it is set, in addition to the 1 we add to account
                // for the length indicator.
                "
                    read_mem // _ *coinbase opt
                    push 4 // _ *coinbase opt 4
                    mul // _ *coinbase opt*4
                    push 1 add // _ *coinbase opt*4+1
                    swap 1 pop"
                    .to_string()
            }
            TransactionKernelField::Timestamp => {
                // The field `timestamp` is just a single `BFieldElement` with
                // static size 1.
                "push 1
                swap 1 pop
                "
                .to_string()
            }
            TransactionKernelField::MutatorSetHash => {
                // The field `mutator_set_hash` is a Digest with static size
                // DIGEST_LENGTH.
                format!(
                    "push {DIGEST_LENGTH}
                swap 1 pop"
                )
            }
        }
    }
    fn input_state(
        address: BFieldElement,
        transaction_kernel_encoded: &[BFieldElement],
    ) -> ExecutionState {
        let mut memory: HashMap<BFieldElement, BFieldElement> = HashMap::new();
        for (i, t) in transaction_kernel_encoded.iter().enumerate() {
            memory.insert(address + BFieldElement::new(i as u64), *t);
        }
        let mut stack = get_init_tvm_stack();
        stack.push(address);
        ExecutionState {
            stack,
            std_in: vec![],
            secret_in: vec![],
            memory,
            words_allocated: 0,
        }
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
            Self::input_state(random(), &random_transaction_kernel_encoding()),
            Self::input_state(random(), &random_transaction_kernel_encoding()),
            Self::input_state(random(), &random_transaction_kernel_encoding()),
            Self::input_state(random(), &random_transaction_kernel_encoding()),
        ]
    }

    fn common_case_input_state(&self) -> crate::ExecutionState {
        let mut seed: [u8; 32] = [0u8; 32];
        seed[0] = 0xba;
        seed[1] = 0xdd;
        seed[2] = 0xbe;
        seed[3] = 0xef;
        Self::input_state(
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
        Self::input_state(
            BFieldElement::new(1),
            &pseudorandom_transaction_kernel_encoding(seed, 3600, 20, 5000),
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

        // traverse down all fields until we find the one we are interested in; get the size of that one
        let size;
        let inputs_distance = BFieldElement::new(1) + *memory.get(&address).unwrap();
        if matches!(self.0, TransactionKernelField::Inputs) {
            size = inputs_distance.value() as usize;
        } else {
            // vec of variable width elements => prepended with encoding length
            address += inputs_distance;

            let outputs_distance = BFieldElement::new(1)
                + *memory.get(&address).unwrap() * BFieldElement::new(DIGEST_LENGTH as u64);
            if matches!(self.0, TransactionKernelField::Outputs) {
                size = outputs_distance.value() as usize;
            } else {
                // vec of fixed length elements => prepended with number of elements
                address += outputs_distance;

                let pubscript_distance = BFieldElement::new(1) + *memory.get(&address).unwrap();
                if matches!(self.0, TransactionKernelField::PubscriptHashesAndInputs) {
                    size = pubscript_distance.value() as usize;
                } else {
                    // vec of variable width elements => prepended with encoding length
                    address += pubscript_distance;

                    if matches!(self.0, TransactionKernelField::Fee) {
                        size = 4;
                    } else {
                        // fixed length
                        address += BFieldElement::new(4);

                        let coinbase_distance = BFieldElement::new(1)
                            + BFieldElement::new(4) * *memory.get(&address).unwrap();
                        if matches!(self.0, TransactionKernelField::Coinbase) {
                            size = coinbase_distance.value() as usize;
                        } else {
                            // option of fixed length element
                            address += coinbase_distance;

                            if matches!(self.0, TransactionKernelField::Timestamp) {
                                size = 1;
                            } else {
                                // fixed length element
                                address += BFieldElement::new(1);

                                if matches!(self.0, TransactionKernelField::MutatorSetHash) {
                                    size = DIGEST_LENGTH;
                                } else {
                                    unreachable!(
                                        "Enum TransactionKernelField must match with some variant."
                                    );
                                }
                            }
                        }
                    }
                }
            }
        }

        // write resulting address back
        stack.push(BFieldElement::new(size as u64));
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
