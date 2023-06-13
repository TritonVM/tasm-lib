use std::{collections::HashMap, fmt::Display};

use itertools::Itertools;

use rand::{random, rngs::StdRng, thread_rng, Rng, RngCore, SeedableRng};
use triton_vm::BFieldElement;
use twenty_first::{
    amount::u32s::U32s,
    shared_math::{bfield_codec::BFieldCodec, other::random_elements},
};

use crate::{
    get_init_tvm_stack,
    snippet::{DataType, Snippet},
    Digest, ExecutionState, DIGEST_LENGTH,
};

#[derive(Copy, Clone, Debug)]
pub enum TransactionKernelField {
    Inputs,
    Outputs,
    PubscriptHashesAndInputs,
    Fee,
    Coinbase,
    Timestamp,
    MutatorSetHash,
}

impl Display for TransactionKernelField {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TransactionKernelField::Inputs => write!(f, "inputs"),
            TransactionKernelField::Outputs => write!(f, "outputs"),
            TransactionKernelField::PubscriptHashesAndInputs => {
                write!(f, "pubscript_hashes_and_inputs")
            }
            TransactionKernelField::Fee => write!(f, "fee"),
            TransactionKernelField::Coinbase => write!(f, "coinbase"),
            TransactionKernelField::Timestamp => write!(f, "timestamp"),
            TransactionKernelField::MutatorSetHash => write!(f, "mutator_set_hash"),
        }
    }
}

/// Gets a named field from a TransactionKernel object.
#[derive(Debug, Clone)]
pub struct GetTransactionKernelField(pub TransactionKernelField);

impl GetTransactionKernelField {
    fn skip_field(field: TransactionKernelField) -> String {
        match field {
            TransactionKernelField::Inputs => {
                // The field `inputs` is a `Vec` of `RemovalRecord`s, whose
                // sizes are not known at compile time because of SWBF
                // authentication paths.
                // So the entire vector is prepended with its length in
                // BFieldElements. Yay!
                "
                read_mem // _ *inputs len
                add // _ *inputs+len
                push 1 add // _ *inputs+len+1
                // _ *outputs"
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
                add // _ *outputs+len
                push 1 add // _ *outputs+len+1
                // _ *pubscript_hashes_and_inputs"
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
                add // _ *pubscript_hashes_and_inputs+len
                push 1 add // _ *pubscript_hashes_and_input+len+1
                // _ *fee"
                    .to_string()
            }
            TransactionKernelField::Fee => {
                // The field `fee` is an `Amount`, which currently consists of 4
                // U32s. But this will change in the future to something unknown
                // at compile time!
                "
                push 4 // _ *fee 4
                add // _ *fee+4
                // _ *coinbase"
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
                    add // _ *timestamp"
                    .to_string()
            }
            TransactionKernelField::Timestamp => {
                // The field `timestamp` is just a single `BFieldElement` with
                // static size 1.
                "push 1 add".to_string()
            }
            TransactionKernelField::MutatorSetHash => {
                // The field `mutator_set_hash` is a Digest with static size
                // DIGEST_LENGTH.
                format!("push {DIGEST_LENGTH} add")
            }
        }
    }

    fn skip_fields_upto(field: TransactionKernelField) -> String {
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

    fn random_transaction_kernel_encoding() -> Vec<BFieldElement> {
        let mut rng = thread_rng();
        let upper_bound = 2 + (rng.next_u32() % 15) as usize;
        let inputs_length = rng.gen_range(1..upper_bound);
        let mut inputs: Vec<BFieldElement> = random_elements(inputs_length);
        inputs[0] = BFieldElement::new(inputs_length as u64 - 1);
        let upper_bound = 1 + (rng.next_u32() % 15) as usize;
        let num_outputs = rng.gen_range(0..upper_bound);
        let outputs: Vec<Digest> = random_elements(num_outputs);
        let outputs_encoded = [
            vec![BFieldElement::new(num_outputs as u64)],
            outputs
                .into_iter()
                .flat_map(|d| d.values().to_vec())
                .collect_vec(),
        ]
        .concat();
        let pubscripts_length = 2 + (rng.next_u32() % 20) as usize;
        let mut pubscript_hashes_and_inputs: Vec<BFieldElement> =
            random_elements(pubscripts_length);
        pubscript_hashes_and_inputs[0] = BFieldElement::new(pubscripts_length as u64 - 1);
        let fee: U32s<4> = random();
        let coinbase: Option<U32s<4>> = random();
        let timestamp: BFieldElement = random();
        let mutator_set_hash: Digest = random();

        [
            inputs.encode(),
            outputs_encoded,
            pubscript_hashes_and_inputs.encode(),
            fee.encode(),
            coinbase.encode(),
            timestamp.encode(),
            mutator_set_hash.encode(),
        ]
        .concat()
    }

    fn pseudorandom_transaction_kernel_encoding(
        seed: [u8; 32],
        inputs_length: usize,
        num_outputs: usize,
        pubscripts_length: usize,
    ) -> Vec<BFieldElement> {
        let mut rng: StdRng = SeedableRng::from_seed(seed);
        let mut inputs = (0..inputs_length + 1)
            .map(|_| BFieldElement::new(rng.next_u64()))
            .collect_vec();
        inputs[0] = BFieldElement::new(inputs_length as u64);
        let outputs = (0..num_outputs)
            .map(|_| {
                Digest::new([
                    BFieldElement::new(rng.next_u64()),
                    BFieldElement::new(rng.next_u64()),
                    BFieldElement::new(rng.next_u64()),
                    BFieldElement::new(rng.next_u64()),
                    BFieldElement::new(rng.next_u64()),
                ])
            })
            .collect_vec();
        let outputs_encoded = [
            vec![BFieldElement::new(num_outputs as u64)],
            outputs
                .into_iter()
                .flat_map(|d| d.values().to_vec())
                .collect_vec(),
        ]
        .concat();
        let mut pubscript_hashes_and_inputs: Vec<BFieldElement> = (0..pubscripts_length + 1)
            .map(|_| BFieldElement::new(rng.next_u64()))
            .collect_vec();
        pubscript_hashes_and_inputs[0] = BFieldElement::new(pubscripts_length as u64);
        let fee: U32s<4> =
            U32s::<4>::try_from(((rng.next_u64() as u128) << 64) ^ rng.next_u64() as u128).unwrap();
        let coinbase: Option<U32s<4>> = None;
        let timestamp = BFieldElement::new(rng.next_u64());
        let mutator_set_hash = Digest::new([
            BFieldElement::new(rng.next_u64()),
            BFieldElement::new(rng.next_u64()),
            BFieldElement::new(rng.next_u64()),
            BFieldElement::new(rng.next_u64()),
            BFieldElement::new(rng.next_u64()),
        ]);

        [
            inputs.encode(),
            outputs_encoded,
            pubscript_hashes_and_inputs.encode(),
            fee.encode(),
            coinbase.encode(),
            timestamp.encode(),
            mutator_set_hash.encode(),
        ]
        .concat()
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
            return
            "
        )
    }

    fn crash_conditions(&self) -> Vec<String> {
        vec!["Transaction kernel is improperly formatted in memory.".to_string()]
    }

    fn gen_input_states(&self) -> Vec<crate::ExecutionState> {
        vec![
            Self::input_state(random(), &Self::random_transaction_kernel_encoding()),
            Self::input_state(random(), &Self::random_transaction_kernel_encoding()),
            Self::input_state(random(), &Self::random_transaction_kernel_encoding()),
            Self::input_state(random(), &Self::random_transaction_kernel_encoding()),
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
            &Self::pseudorandom_transaction_kernel_encoding(seed, 360, 2, 500),
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
            &Self::pseudorandom_transaction_kernel_encoding(seed, 3600, 20, 5000),
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

        // for each field, add offset to address
        if !matches!(self.0, TransactionKernelField::Inputs) {
            // vec of variable width elements => prepended with encoding length
            address += BFieldElement::new(1) + *memory.get(&address).unwrap();

            if !matches!(self.0, TransactionKernelField::Outputs) {
                // vec of fixed length elements => prepended with number of elements
                address += BFieldElement::new(1)
                    + *memory.get(&address).unwrap() * BFieldElement::new(DIGEST_LENGTH as u64);

                if !matches!(self.0, TransactionKernelField::PubscriptHashesAndInputs) {
                    // vec of variable width elements => prepended with encoding length
                    address += BFieldElement::new(1) + *memory.get(&address).unwrap();

                    if !matches!(self.0, TransactionKernelField::Fee) {
                        // fixed length
                        address += BFieldElement::new(4);

                        if !matches!(self.0, TransactionKernelField::Coinbase) {
                            // option of fixed length element
                            address += BFieldElement::new(1)
                                + BFieldElement::new(4) * *memory.get(&address).unwrap();

                            if !matches!(self.0, TransactionKernelField::Timestamp) {
                                // fixed length element
                                address += BFieldElement::new(1);

                                if !matches!(self.0, TransactionKernelField::MutatorSetHash) {
                                    unreachable!("field in TransactionKernel is not matched by any enum variant")
                                }
                            }
                        }
                    }
                }
            }
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
