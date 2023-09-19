use crate::VmHasherState;
use anyhow::{bail, Result};
use rand::{rngs::StdRng, Rng, RngCore, SeedableRng};
use triton_vm::{
    proof_item::{FriResponse, ProofItem},
    table::master_table::{NUM_BASE_COLUMNS, NUM_EXT_COLUMNS},
    BFieldElement,
};
use twenty_first::{
    shared_math::{
        b_field_element::{BFIELD_ONE, BFIELD_ZERO},
        bfield_codec::BFieldCodec,
        x_field_element::XFieldElement,
    },
    util_types::algebraic_hasher::{AlgebraicHasher, Domain, SpongeHasher},
};

use crate::{structure::tasm_object::TasmObject, Digest, VmHasher};

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
        let sequence =
            &self.data[(self.word_index as usize + 1)..(self.word_index as usize + 1 + size)];
        self.word_index += size as u32 + 1;
        let item = *ProofItem::decode(sequence)?;

        if item.include_in_fiat_shamir_heuristic() {
            self.fiat_shamir(&item);
        }

        Ok(Box::new(item))
    }

    fn fiat_shamir<T: BFieldCodec>(&mut self, item: &T) {
        VmHasher::absorb_repeatedly(
            &mut self.sponge_state,
            Self::encode_and_pad_item(item).iter(),
        )
    }

    fn encode_and_pad_item<T: BFieldCodec>(item: &T) -> Vec<BFieldElement> {
        let encoding = item.encode();
        let last_chunk_len = (encoding.len() + 1) % VmHasher::RATE;
        let num_padding_zeros = match last_chunk_len {
            0 => 0,
            _ => VmHasher::RATE - last_chunk_len,
        };
        [
            encoding,
            vec![BFIELD_ONE],
            vec![BFIELD_ZERO; num_padding_zeros],
        ]
        .concat()
    }

    pub fn sample_scalars(&mut self, number: usize) -> Vec<XFieldElement> {
        VmHasher::sample_scalars(&mut self.sponge_state, number)
    }

    pub fn sample_indices(&mut self, upper_bound: u32, number: u32) -> Vec<u32> {
        VmHasher::sample_indices(&mut self.sponge_state, upper_bound, number as usize)
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
