use std::{cmp::max, collections::HashMap};

use anyhow::Result;
use rand::{rngs::StdRng, Rng, RngCore, SeedableRng};
use triton_vm::{
    instruction::LabelledInstruction,
    proof_item::{FriResponse, ProofItem},
    table::master_table::{NUM_BASE_COLUMNS, NUM_EXT_COLUMNS},
    triton_asm, BFieldElement, NonDeterminism,
};
use twenty_first::shared_math::{bfield_codec::BFieldCodec, x_field_element::XFieldElement};

use crate::{
    algorithm::Algorithm,
    field, field_with_size, get_init_tvm_stack,
    library::Library,
    snippet::{BasicSnippet, DataType},
    snippet_bencher::BenchmarkCase,
    structure::tasm_object::TasmObject,
    Digest,
};

#[derive(Debug, Clone, BFieldCodec, TasmObject)]
pub struct VmProofStream {
    word_index: u32,
    data: Vec<BFieldElement>,
}

impl VmProofStream {
    pub fn new(items: &[ProofItem]) -> Self {
        Self {
            word_index: 1,
            data: items.to_vec().encode(),
        }
    }
    pub fn dequeue(&mut self) -> Result<Box<ProofItem>> {
        let size = self.data[self.word_index as usize].value() as usize;
        let sequence =
            &self.data[(self.word_index as usize + 1)..(self.word_index as usize + 1 + size)];
        self.word_index += size as u32 + 1;
        ProofItem::decode(sequence)
    }

    pub fn pseudorandom_items_list(seed: [u8; 32]) -> Vec<ProofItem> {
        let mut rng: StdRng = SeedableRng::from_seed(seed);
        let num_iterations = rng.next_u32() % 5;
        let mut proof_items = vec![];
        for _ in 0..num_iterations {
            if rng.next_u32() % 2 == 1 {
                let authentication_structure: Vec<Digest> = (0..20).map(|_| rng.gen()).collect();
                proof_items.push(ProofItem::AuthenticationStructure(authentication_structure));
            }
            if rng.next_u32() % 2 == 1 {
                let fri_codeword: Vec<XFieldElement> = (0..20).map(|_| rng.gen()).collect();
                proof_items.push(ProofItem::FriCodeword(fri_codeword));
            }
            if rng.next_u32() % 2 == 1 {
                let auth_structure: Vec<Digest> = (0..20).map(|_| rng.gen()).collect();
                let revealed_leaves: Vec<XFieldElement> = (0..20).map(|_| rng.gen()).collect();
                let fri_response = FriResponse {
                    auth_structure,
                    revealed_leaves,
                };
                proof_items.push(ProofItem::FriResponse(fri_response));
            }
            if rng.next_u32() % 2 == 1 {
                proof_items.push(ProofItem::Log2PaddedHeight(rng.next_u32()));
            }
            if rng.next_u32() % 2 == 1 {
                let master_table_base_rows: Vec<Vec<BFieldElement>> = (0..20)
                    .map(|_| {
                        (0..20)
                            .map(|_| rng.gen::<BFieldElement>())
                            .collect::<Vec<BFieldElement>>()
                    })
                    .collect::<Vec<Vec<BFieldElement>>>();
                proof_items.push(ProofItem::MasterBaseTableRows(master_table_base_rows));
            }
            if rng.next_u32() % 2 == 1 {
                let master_table_ext_rows: Vec<Vec<XFieldElement>> = (0..20)
                    .map(|_| {
                        (0..20)
                            .map(|_| rng.gen::<XFieldElement>())
                            .collect::<Vec<XFieldElement>>()
                    })
                    .collect::<Vec<Vec<XFieldElement>>>();
                proof_items.push(ProofItem::MasterExtTableRows(master_table_ext_rows));
            }
            if rng.next_u32() % 2 == 1 {
                proof_items.push(ProofItem::MerkleRoot(rng.gen()));
            }
            if rng.next_u32() % 2 == 1 {
                let ood_base_row = (0..NUM_BASE_COLUMNS).map(|_| rng.gen()).collect();
                proof_items.push(ProofItem::OutOfDomainBaseRow(ood_base_row));
            }
            if rng.next_u32() % 2 == 1 {
                let ood_ext_row = (0..NUM_EXT_COLUMNS).map(|_| rng.gen()).collect();
                proof_items.push(ProofItem::OutOfDomainExtRow(ood_ext_row));
            }
            if rng.next_u32() % 2 == 1 {
                let ood_quotient_segments = rng.gen();
                proof_items.push(ProofItem::OutOfDomainQuotientSegments(
                    ood_quotient_segments,
                ));
            }
            if rng.next_u32() % 2 == 1 {
                let quotient_segment_elements = (0..20).map(|_| rng.gen()).collect();
                proof_items.push(ProofItem::QuotientSegmentsElements(
                    quotient_segment_elements,
                ));
            }
        }
        proof_items
    }
}

/// Dequeue reads the next object from the `ProofStream`.
#[derive(Clone)]
pub struct Dequeue {}

impl BasicSnippet for Dequeue {
    fn inputs(&self) -> Vec<(DataType, String)> {
        vec![(DataType::VoidPointer, "*proof_stream".to_string())]
    }

    fn outputs(&self) -> Vec<(DataType, String)> {
        vec![
            (DataType::VoidPointer, "*proof_stream".to_string()),
            (DataType::VoidPointer, "*object".to_string()),
        ]
    }

    fn entrypoint(&self) -> String {
        "tasm_recufier_proof_stream_dequeue".to_string()
    }

    fn code(&self, _library: &mut Library) -> Vec<LabelledInstruction> {
        let entrypoint = self.entrypoint();
        let field_word_index = field!(VmProofStream::word_index);
        let field_data_with_size = field_with_size!(VmProofStream::data);
        triton_asm!(
            // BEFORE: _ *proof_stream
            // AFTER: _ *proof_stream *object
            {entrypoint}:

                dup 0               // _ *proof_stream *proof_stream
                {&field_word_index} // _ *proof_stream *word_index
                read_mem            // _ *proof_stream *word_index word_index
                dup 0               // _ *proof_stream *word_index word_index word_index

                dup 3               // _ *proof_stream *word_index word_index word_index *proof_stream
                {&field_data_with_size}
                                    // _ *proof_stream *word_index word_index word_index *data_list_length_indicator data_field_size
                dup 2               // _ *proof_stream *word_index word_index word_index *data_list_length_indicator data_field_size word_index
                push 1 add          // _ *proof_stream *word_index word_index word_index *data_list_length_indicator data_field_size word_index+1
                lt                  // _ *proof_stream *word_index word_index word_index *data_list_length_indicator word_index+1<data_field_size
                assert              // _ *proof_stream *word_index word_index word_index *data_list_length_indicator
                push 1 add          // _ *proof_stream *word_index word_index word_index *data
                add                 // _ *proof_stream *word_index word_index *object_si

                read_mem            // _ *proof_stream *word_index word_index *object_si object_size
                push 1 add          // _ *proof_stream *word_index word_index *object_si object_size+1
                dup 2               // _ *proof_stream *word_index word_index *object_si object_size+1 word_index
                add                 // _ *proof_stream *word_index  word_index *object_si word_index'

                swap 1              // _ *proof_stream *word_index word_index word_index' *object_si
                push 1 add          // _ *proof_stream *word_index word_index word_index' *object
                swap 3              // _ *proof_stream *object word_index word_index' *word_index
                swap 1              // _ *proof_stream *object word_index *word_index word_index'
                write_mem           // _ *proof_stream *object word_index *word_index

                pop pop             // _ *proof_stream *object

                return

        )
    }
}

impl Algorithm for Dequeue {
    fn rust_shadow(
        &self,
        stack: &mut Vec<BFieldElement>,
        memory: &mut std::collections::HashMap<BFieldElement, BFieldElement>,
        _nondeterminism: &triton_vm::NonDeterminism<BFieldElement>,
    ) {
        // read proof stream pointer from stack
        let proof_stream_pointer = stack.pop().unwrap();

        // decode from memory
        let proof_stream =
            *VmProofStream::decode_from_memory(memory, proof_stream_pointer).unwrap();

        // calculate proof item address
        let proof_item_pointer = proof_stream_pointer
            + BFieldElement::new(1) // skip over word_index
            + BFieldElement::new(1) // skip over data field length indicator
            + BFieldElement::new(1) // skip over data list length indicator
            + BFieldElement::new(proof_stream.word_index as u64) // jump to proof item size indicator
            + BFieldElement::new(1); // skip over size indicator

        // check bounds
        let first_inaccessible_address =
            proof_stream_pointer + BFieldElement::new(proof_stream.encode().len() as u64);
        if proof_item_pointer.value() >= first_inaccessible_address.value() {
            panic!("Proof stream out of bounds");
        }

        // read size
        let object_size = memory
            .get(
                &(proof_stream_pointer
                    + BFieldElement::new(1) // skip over word_index
                    + BFieldElement::new(1) // skip over data field length indicator
                    + BFieldElement::new(1) // skip over data list length indicator
                    + BFieldElement::new(proof_stream.word_index as u64)),
            )
            .unwrap()
            .value() as u32;

        // jump word index forward
        let new_word_index = proof_stream.word_index + object_size + 1;
        memory.insert(
            proof_stream_pointer,
            BFieldElement::new(new_word_index as u64),
        );

        // add proof item pointer to stack
        stack.push(proof_stream_pointer);
        stack.push(proof_item_pointer);
    }

    fn pseudorandom_initial_state(
        &self,
        seed: [u8; 32],
        bench_case: Option<BenchmarkCase>,
    ) -> (
        Vec<BFieldElement>,
        HashMap<BFieldElement, BFieldElement>,
        NonDeterminism<BFieldElement>,
    ) {
        let mut rng: StdRng = SeedableRng::from_seed(seed);

        let mut proof_items = vec![];

        // populate with random proof items
        while proof_items.is_empty() {
            proof_items = VmProofStream::pseudorandom_items_list(rng.gen());
        }
        assert!(!proof_items.is_empty());

        // create proof stream object and populate it with these proof items
        let mut proof_stream = VmProofStream::new(&proof_items);

        // dequeue some number of elements
        let dequeue_count = if matches!(bench_case, Some(BenchmarkCase::WorstCase)) {
            proof_items.len() - 1
        } else {
            rng.next_u32() as usize % max(1, proof_items.len())
        };

        for _ in 0..dequeue_count {
            proof_stream.dequeue().unwrap();
        }

        // write to memory at random address
        let address = BFieldElement::new((rng.next_u32() % (1 << 20)) as u64);
        let sequence = proof_stream.encode();
        let mut memory = HashMap::new();
        for (i, b) in sequence.into_iter().enumerate() {
            memory.insert(address + BFieldElement::new(i as u64), b);
        }

        // drop address on stack
        let mut stack = get_init_tvm_stack();
        stack.push(address);

        (stack, memory, NonDeterminism::new(vec![]))
    }
}

#[cfg(test)]
mod test {

    use rand::{rngs::StdRng, Rng, SeedableRng};

    use crate::{
        algorithm::{Algorithm, ShadowedAlgorithm},
        test_helpers::test_rust_equivalence_given_complete_state,
    };

    use super::Dequeue;

    #[test]
    fn test() {
        let num_states = 10;
        let seed = [
            0x88, 0x58, 0x6b, 0xe, 0xb7, 0x36, 0xed, 0x57, 0x88, 0xcd, 0xf7, 0x57, 0xc0, 0x29,
            0x02, 0xca, 0xfb, 0x66, 0x41, 0xac, 0x2d, 0xb1, 0xe6, 0x1b, 0x4a, 0x9c, 0x68, 0x29,
            0xa5, 0x23, 0x2e, 0xa6,
        ];
        let mut rng: StdRng = SeedableRng::from_seed(seed);

        let dequeue = Dequeue {};
        let algorithm = ShadowedAlgorithm::new(dequeue.clone());

        for _ in 0..num_states {
            let (stack, memory, nondeterminism) =
                dequeue.pseudorandom_initial_state(rng.gen(), None);

            let stdin = vec![];
            let _vm_output_state = test_rust_equivalence_given_complete_state(
                &algorithm,
                &stack,
                &stdin,
                &nondeterminism,
                &memory,
                1,
                None,
            );
        }
    }

    // TODO: test that behavior is the same (crash in both cases) when the proof stream is empty
}
