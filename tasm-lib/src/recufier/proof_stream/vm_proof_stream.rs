use anyhow::bail;
use anyhow::Result;
use rand::rngs::StdRng;
use rand::Rng;
use rand::SeedableRng;
use triton_vm::instruction::LabelledInstruction;
use triton_vm::prelude::*;
use triton_vm::proof_item::FriResponse;
use triton_vm::proof_item::ProofItem;
use triton_vm::table::master_table::NUM_BASE_COLUMNS;
use triton_vm::table::master_table::NUM_EXT_COLUMNS;
use triton_vm::twenty_first::prelude::*;
use twenty_first::util_types::algebraic_hasher::Domain;

use crate::structure::tasm_object::TasmObject;
use crate::VmHasher;
use crate::VmHasherState;

#[derive(Debug, Clone, BFieldCodec, TasmObject)]
pub struct VmProofStream {
    pub word_index: u32,
    data: Vec<BFieldElement>,
    #[bfield_codec(ignore)]
    #[tasm_object(ignore)]
    pub sponge_state: VmHasherState,
}

impl VmProofStream {
    pub fn new(items: &[ProofItem]) -> Self {
        Self {
            word_index: 1,
            data: items.to_vec().encode(),
            sponge_state: VmHasherState::new(Domain::VariableLength),
        }
    }

    pub fn dequeue(&mut self) -> Result<Box<ProofItem>> {
        if self.word_index as usize >= self.data.len() {
            bail!("No more words left in stream.")
        }
        let size = self.data[self.word_index as usize].value() as usize;
        let word_data_index = self.word_index as usize + 1;
        let word = &self.data[word_data_index..word_data_index + size];
        let item = *ProofItem::decode(word)?;
        self.word_index = (word_data_index + size) as u32;

        if item.include_in_fiat_shamir_heuristic() {
            self.fiat_shamir(&item);
        }

        Ok(Box::new(item))
    }

    fn fiat_shamir<T: BFieldCodec>(&mut self, item: &T) {
        VmHasher::pad_and_absorb_all(&mut self.sponge_state, &item.encode());
    }

    pub fn sample_scalars(&mut self, number: usize) -> Vec<XFieldElement> {
        VmHasher::sample_scalars(&mut self.sponge_state, number)
    }

    pub fn sample_indices(&mut self, upper_bound: u32, number: u32) -> Vec<u32> {
        VmHasher::sample_indices(&mut self.sponge_state, upper_bound, number as usize)
    }

    pub fn pseudorandom_items_list(seed: [u8; 32]) -> Vec<ProofItem> {
        let mut rng: StdRng = SeedableRng::from_seed(seed);
        let num_iterations = rng.gen_range(0..5);
        let mut proof_items = vec![];
        for _ in 0..num_iterations {
            if rng.gen() {
                proof_items.push(ProofItem::AuthenticationStructure(
                    (0..20).map(|_| rng.gen()).collect(),
                ));
            }
            if rng.gen() {
                proof_items.push(ProofItem::FriCodeword((0..20).map(|_| rng.gen()).collect()));
            }
            if rng.gen() {
                let fri_response = FriResponse {
                    auth_structure: (0..20).map(|_| rng.gen()).collect(),
                    revealed_leaves: (0..20).map(|_| rng.gen()).collect(),
                };
                proof_items.push(ProofItem::FriResponse(fri_response));
            }
            if rng.gen() {
                proof_items.push(ProofItem::Log2PaddedHeight(rng.gen()));
            }
            if rng.gen() {
                proof_items.push(ProofItem::MasterBaseTableRows(
                    (0..20)
                        .map(|_| (0..20).map(|_| rng.gen()).collect())
                        .collect(),
                ));
            }
            if rng.gen() {
                proof_items.push(ProofItem::MasterExtTableRows(
                    (0..20)
                        .map(|_| (0..20).map(|_| rng.gen()).collect())
                        .collect(),
                ));
            }
            if rng.gen() {
                proof_items.push(ProofItem::MerkleRoot(rng.gen()));
            }
            if rng.gen() {
                proof_items.push(ProofItem::OutOfDomainBaseRow(
                    (0..NUM_BASE_COLUMNS).map(|_| rng.gen()).collect(),
                ));
            }
            if rng.gen() {
                proof_items.push(ProofItem::OutOfDomainExtRow(
                    (0..NUM_EXT_COLUMNS).map(|_| rng.gen()).collect(),
                ));
            }
            if rng.gen() {
                proof_items.push(ProofItem::OutOfDomainQuotientSegments(rng.gen()));
            }
            if rng.gen() {
                proof_items.push(ProofItem::QuotientSegmentsElements(
                    (0..20).map(|_| rng.gen()).collect(),
                ));
            }
        }
        proof_items
    }

    pub fn proof_item_as_merkle_root_code() -> Vec<LabelledInstruction> {
        let merkle_root_discriminant =
            ProofItem::MerkleRoot(Digest::default()).bfield_codec_discriminant();
        triton_asm! {
            // *proof_item
            read_mem 1
            push 2 add
            swap 1
            push {merkle_root_discriminant}
            eq assert
        }
    }

    pub fn proof_item_as_fri_codeword_code() -> Vec<LabelledInstruction> {
        let fri_codeword_discriminant = ProofItem::FriCodeword(vec![]).bfield_codec_discriminant();
        triton_asm! {
                                // _ *fri_codeword_ev
            read_mem 1          // _ fri_codeword_ev *fri_codeword_ev-1
            push 2 add          // _ fri_codeword_ev *fri_codeword_encoding
            swap 1              // _ *fri_codeword_encoding fri_codeword_ev
            push {fri_codeword_discriminant}
                                // _ *fri_codeword_encoding fri_codeword_ev 9 (<-- discriminant for enum variant FriCodeword)
            eq assert           // _ *fri_codeword_encoding
            read_mem 1          // _ encoding_length *fri_codeword_encoding-1
            push 2 add          // _ encoding_length *fri_codeword
            read_mem 1          // _ encoding_length vector_length *fri_codeword-1
            push 1 add          // _ encoding_length vector_length *fri_codeword
            swap 2 swap 1       // _ *fri_codeword encoding_length vector_length
            push 3 mul          // _ *fri_codeword encoding_length vector_length*3 (<-- 3 BFEs per XFE)
            push 1 add          // _ *fri_codeword encoding_length vector_length*3+1 (<-- +1 for length indicator)
            eq assert           // _ *fri_codeword
        }
    }

    pub fn proof_item_as_fri_response_code() -> Vec<LabelledInstruction> {
        // fri_response is encoded as:
        // enum-discriminant, size-of-assoc-data, size-of-second-field, [encoding of second field], size-of-first-field, [encoding of first field]
        // we want to land here:                   ^

        let fri_response_discriminant = ProofItem::FriResponse(FriResponse {
            revealed_leaves: vec![],
            auth_structure: vec![],
        })
        .bfield_codec_discriminant();
        triton_asm! {
                                // _ *fri_response_ev
            read_mem 1          // _ fri_response_ev *fri_response_ev-1
            push 2 add          // _ fri_response_ev *fri_response_si
            swap 1 push {fri_response_discriminant}
                                // _ *fri_response_si fri_response_ev fri_response_discriminant
            eq assert           // _ *fri_response_si
            push 1 add          // _ *fri_response
        }
    }
}
