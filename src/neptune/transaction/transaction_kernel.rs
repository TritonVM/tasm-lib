use std::{collections::HashMap, fmt::Display};

use itertools::Itertools;
use num_traits::Zero;
use rand::{random, rngs::StdRng, thread_rng, Rng, RngCore, SeedableRng};
use triton_vm::BFieldElement;
use twenty_first::{
    amount::u32s::U32s,
    shared_math::{bfield_codec::BFieldCodec, other::random_elements},
};

use crate::{get_init_tvm_stack, Digest, ExecutionState};

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

pub fn random_transaction_kernel_encoding() -> Vec<BFieldElement> {
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
    let mut pubscript_hashes_and_inputs: Vec<BFieldElement> = random_elements(pubscripts_length);
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

pub fn pseudorandom_transaction_kernel_encoding(
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

pub fn input_state_with_kernel_in_memory(
    address: BFieldElement,
    transaction_kernel_encoded: &[BFieldElement],
) -> ExecutionState {
    // populate memory
    let mut memory: HashMap<BFieldElement, BFieldElement> = HashMap::new();
    for (i, t) in transaction_kernel_encoded.iter().enumerate() {
        memory.insert(address + BFieldElement::new(i as u64), *t);
    }

    // set dynamic allocator
    memory.insert(
        BFieldElement::zero(),
        BFieldElement::new(transaction_kernel_encoded.len() as u64) + address,
    );

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
