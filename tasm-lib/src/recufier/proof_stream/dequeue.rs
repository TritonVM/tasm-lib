use num_traits::Zero;
use std::{cmp::max, collections::HashMap};

use rand::Rng;
use rand::{rngs::StdRng, RngCore, SeedableRng};
use triton_vm::proof_item::ProofItem;
use triton_vm::{instruction::LabelledInstruction, triton_asm, BFieldElement, NonDeterminism};
use twenty_first::shared_math::bfield_codec::BFieldCodec;
use twenty_first::shared_math::x_field_element::XFieldElement;

use crate::data_type::DataType;
use crate::hashing::absorb::Absorb;
use crate::procedure::Procedure;
use crate::structure::tasm_object::TasmObject;
use crate::VmHasherState;
use crate::{
    empty_stack, field, field_with_size, library::Library, snippet::BasicSnippet,
    snippet_bencher::BenchmarkCase,
};

use super::vm_proof_stream::VmProofStream;

/// Dequeue reads the next object from the `ProofStream`.
#[derive(Clone)]
pub struct Dequeue;

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
        let include_in_fiat_shamir_heuristic =
            format!("{entrypoint}_include_in_fiat_shamir_heuristic");
        let fiat_shamir = format!("{entrypoint}_fiat_shamir");
        let absorb = _library.import(Box::new(Absorb {}));
        triton_asm!(
            // BEFORE: _ *object_si
            // AFTER: _ *object_si bool
            {include_in_fiat_shamir_heuristic}:
                // get enum variant indicator
                dup 0           // _ *object_si *object_si
                push 1 add      // _ *object_si *object_evi
                read_mem        // _ *object_si *object_evi object_evi
                swap 1 pop      // _ *object_si object_evi

                // is it a Merkle root?
                push {ProofItem::MerkleRoot(Default::default()).bfield_codec_discriminant()}
                dup 1 eq        // _ *object object_evi object=merkle_root
                swap 1          // _ *object object=merkle_root object_evi

                // is it an ood base row?
                push {ProofItem::OutOfDomainBaseRow(vec![]).bfield_codec_discriminant()}
                dup 1 eq        // _ *object object=merkle_root object_evi object_evi=ood_base_row
                swap 1          // _ *object object=merkle_root object_evi=ood_base_row object_evi

                // is it an ood ext row?
                push {ProofItem::OutOfDomainExtRow(vec![]).bfield_codec_discriminant()}
                dup 1 eq
                swap 1

                // is it an ood quotient segment?
                push {ProofItem::OutOfDomainQuotientSegments([XFieldElement::zero(); 4])
                    .bfield_codec_discriminant()}
                dup 1 eq
                swap 1 pop

                // compress
                add add add
                return

            // BEFORE: _ *proof_item_si
            // AFTER: _ *proof_item_si
            {fiat_shamir}:
                dup 0           // _ *proof_item_si *proof_item_si
                read_mem        // _ *proof_item_si *proof_item_si proof_item_size
                swap 1          // _ *proof_item_si proof_item_size *proof_item_si
                push 1 add      // _ *proof_item_si proof_item_size *proof_item

                swap 1          // _ *proof_item_si *proof_item proof_item_size
                call {absorb}
                return

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


                call {include_in_fiat_shamir_heuristic}
                                    // _ *proof_stream *word_index word_index *object_si bool

                skiz call {fiat_shamir}
                                    // _ *proof_stream *word_index word_index *object_si

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

impl Procedure for Dequeue {
    fn rust_shadow(
        &self,
        stack: &mut Vec<BFieldElement>,
        memory: &mut std::collections::HashMap<BFieldElement, BFieldElement>,
        _nondeterminism: &triton_vm::NonDeterminism<BFieldElement>,
        _stdin: &[BFieldElement],
        sponge_state: &mut Option<VmHasherState>,
    ) -> Vec<BFieldElement> {
        // read proof stream pointer from stack
        let proof_stream_pointer = stack.pop().unwrap();

        // decode from memory
        let mut proof_stream =
            *VmProofStream::decode_from_memory(memory, proof_stream_pointer).unwrap();
        proof_stream.sponge_state = sponge_state
            .as_ref()
            .expect("Sponge state must be initialized")
            .to_owned();

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

        // read object
        let _proof_item = proof_stream.dequeue();

        // percolate sponge changes
        *sponge_state = Some(proof_stream.sponge_state.clone());

        // write proof stream object back
        for (i, s) in proof_stream.encode().into_iter().enumerate() {
            memory.insert(proof_stream_pointer + BFieldElement::new(i as u64), s);
        }

        // add proof item pointer to stack
        stack.push(proof_stream_pointer);
        stack.push(proof_item_pointer);

        vec![]
    }

    fn pseudorandom_initial_state(
        &self,
        seed: [u8; 32],
        bench_case: Option<BenchmarkCase>,
    ) -> (
        Vec<BFieldElement>,
        HashMap<BFieldElement, BFieldElement>,
        NonDeterminism<BFieldElement>,
        Vec<BFieldElement>,
        Option<VmHasherState>,
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
        let mut stack = empty_stack();
        stack.push(address);

        // populate sponge state at random
        let sponge_state = VmHasherState { state: rng.gen() };

        (
            stack,
            memory,
            NonDeterminism::new(vec![]),
            vec![],
            Some(sponge_state),
        )
    }
}

#[cfg(test)]
mod test {

    use std::{cell::RefCell, collections::HashMap, rc::Rc};

    use rand::{rngs::StdRng, Rng, RngCore, SeedableRng};
    use triton_vm::{BFieldElement, NonDeterminism, Program};
    use twenty_first::shared_math::bfield_codec::BFieldCodec;

    use crate::{
        empty_stack, execute_with_terminal_state,
        linker::link_for_isolated_run,
        procedure::{Procedure, ShadowedProcedure},
        snippet::RustShadow,
        test_helpers::test_rust_equivalence_given_complete_state,
    };

    use super::{Dequeue, VmProofStream};

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
        let algorithm = ShadowedProcedure::new(dequeue.clone());

        for _ in 0..num_states {
            let (stack, memory, nondeterminism, _stdin, _sponge_state) =
                dequeue.pseudorandom_initial_state(rng.gen(), None);

            let stdin = vec![];
            let _vm_output_state = test_rust_equivalence_given_complete_state(
                &algorithm,
                &stack,
                &stdin,
                &nondeterminism,
                &memory,
                &None,
                1,
                None,
            );
        }
    }

    #[test]
    fn negative_test() {
        let num_states = 10;
        let seed = [
            0x88, 0x58, 0x6b, 0xe, 0xb7, 0x36, 0xed, 0x57, 0x88, 0xcd, 0xf7, 0x57, 0xc0, 0x29,
            0x02, 0xca, 0xfb, 0x66, 0x41, 0xac, 0x2d, 0xb1, 0xe6, 0x1b, 0x4a, 0x9c, 0x68, 0x29,
            0xa5, 0x23, 0x2e, 0xa6,
        ];
        let mut rng: StdRng = SeedableRng::from_seed(seed);

        let dequeue = Dequeue {};

        for _ in 0..num_states {
            let proof_items = VmProofStream::pseudorandom_items_list(rng.gen());

            // create proof stream object and populate it with these proof items
            let mut proof_stream = VmProofStream::new(&proof_items);

            // dequeue all elements
            let dequeue_count = proof_items.len();

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
            let mut stack = empty_stack();
            stack.push(address);

            // test rust/tasm equivalence
            // in this case: verify that they both fail
            // (because you can't dequeue from an empty stream)

            let stdin = vec![];
            let mut nondeterminism = NonDeterminism::new(vec![]);

            // run rust shadow
            let rust_result = std::panic::catch_unwind(|| {
                let mut rust_stack = stack.clone();
                let mut rust_memory = memory.clone();
                ShadowedProcedure::new(dequeue.clone()).rust_shadow_wrapper(
                    &stdin,
                    &nondeterminism,
                    &mut rust_stack,
                    &mut rust_memory,
                    &mut None,
                )
            });

            // run tvm
            let code = link_for_isolated_run(Rc::new(RefCell::new(dequeue.clone())), 0);
            let program = Program::new(&code);
            let tvm_result =
                execute_with_terminal_state(&program, &stdin, &stack, &nondeterminism, None);
            println!("tvm_result: {tvm_result:?}");

            assert!(rust_result.is_err() && tvm_result.is_err());
        }
    }
}
