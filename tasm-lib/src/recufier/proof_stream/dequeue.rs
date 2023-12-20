use std::{cmp::max, collections::HashMap};

use num_traits::Zero;
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

    fn code(&self, library: &mut Library) -> Vec<LabelledInstruction> {
        let entrypoint = self.entrypoint();
        let field_word_index = field!(VmProofStream::word_index);
        let field_data_with_size = field_with_size!(VmProofStream::data);
        let include_in_fiat_shamir_heuristic =
            format!("{entrypoint}_include_in_fiat_shamir_heuristic");
        let fiat_shamir = format!("{entrypoint}_fiat_shamir");
        let absorb = library.import(Box::new(Absorb {}));
        triton_asm!(
            // BEFORE: _ *object_si
            // AFTER:  _ *object_si bool
            {include_in_fiat_shamir_heuristic}:
                // get enum variant indicator
                dup 0           // _ *object_si *object_si
                push 1 add      // _ *object_si *object_evi
                read_mem 1
                pop 1           // _ *object_si object_evi

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
                swap 1 pop 1

                // compress
                add add add
                return

            // BEFORE: _ *proof_item_size
            // AFTER:  _ *proof_item_size
            {fiat_shamir}:
                dup 0           // _ *proof_item_size *proof_item_size
                read_mem 1
                push 2 add      // _ *proof_item_size proof_item_size *proof_item

                swap 1          // _ *proof_item_size *proof_item proof_item_size
                call {absorb}   // _ *proof_item_size
                return

            // BEFORE: _ *proof_stream
            // AFTER:  _ *proof_stream *object
            {entrypoint}:

                dup 0               // _ *proof_stream *proof_stream
                {&field_word_index} // _ *proof_stream *word_index
                read_mem 1          // _ *proof_stream word_index (*word_index - 1)
                push 1 add          // _ *proof_stream word_index *word_index
                swap 1              // _ *proof_stream *word_index word_index
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

                read_mem 1          // _ *proof_stream *word_index word_index object_size (*object_si - 1)
                push 2 add          // _ *proof_stream *word_index word_index object_size *object
                swap 1              // _ *proof_stream *word_index word_index *object object_size
                push 1 add          // _ *proof_stream *word_index word_index *object object_size+1
                dup 2               // _ *proof_stream *word_index word_index *object object_size+1 word_index
                add                 // _ *proof_stream *word_index word_index *object word_index'

                swap 1              // _ *proof_stream *word_index word_index word_index' *object
                swap 3              // _ *proof_stream *object word_index word_index' *word_index
                write_mem 1         // _ *proof_stream *object word_index (*word_index + 1)

                pop 2               // _ *proof_stream *object

                return

        )
    }
}

impl Procedure for Dequeue {
    fn rust_shadow(
        &self,
        stack: &mut Vec<BFieldElement>,
        memory: &mut HashMap<BFieldElement, BFieldElement>,
        _nondeterminism: &NonDeterminism<BFieldElement>,
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
            + BFieldElement::new(1) // skip over total encoding length indicator
            + BFieldElement::new(1) // skip over data's encoding length indicator
            + BFieldElement::new(1) // skip over data's number of items indicator
            + BFieldElement::from(proof_stream.word_index); // jump to proof item size indicator

        // check bounds
        let first_inaccessible_address =
            proof_stream_pointer + BFieldElement::new(proof_stream.encode().len() as u64);
        if proof_item_pointer.value() >= first_inaccessible_address.value() {
            panic!("Proof stream out of bounds");
        }

        // read object
        let proof_item = proof_stream.dequeue();
        let sponge_absorb_has_been_called = proof_item.unwrap().include_in_fiat_shamir_heuristic();

        // percolate sponge changes
        *sponge_state = Some(proof_stream.sponge_state.clone());

        // write proof stream object back
        for (i, s) in proof_stream.encode().into_iter().enumerate() {
            memory.insert(proof_stream_pointer + BFieldElement::new(i as u64), s);
        }

        // add proof item pointer to stack
        stack.push(proof_stream_pointer);
        stack.push(proof_item_pointer);

        if sponge_absorb_has_been_called {
            // this is a highly specific implementation detail of BFieldCodec
            let proof_item_length_pointer = proof_item_pointer - BFieldElement::new(1);
            let &proof_item_length = memory.get(&proof_item_length_pointer).unwrap();
            memory.extend(Absorb::statically_allocated_memory(
                proof_item_pointer,
                proof_item_length,
            ));
        }

        vec![]
    }

    fn pseudorandom_initial_state(
        &self,
        seed: [u8; 32],
        bench_case: Option<BenchmarkCase>,
    ) -> (
        Vec<BFieldElement>,
        NonDeterminism<BFieldElement>,
        Vec<BFieldElement>,
        Option<VmHasherState>,
    ) {
        // populate with random proof items
        let mut rng: StdRng = SeedableRng::from_seed(seed);
        let mut proof_items = vec![];
        while proof_items.is_empty() {
            proof_items = VmProofStream::pseudorandom_items_list(rng.gen());
        }

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
        let address: u64 = rng.gen_range(0..(1 << 20));
        let address = BFieldElement::new(address);
        let sequence = proof_stream.encode();
        let mut memory = HashMap::new();
        for (i, b) in sequence.into_iter().enumerate() {
            memory.insert(address + BFieldElement::new(i as u64), b);
        }

        let stack = [empty_stack(), vec![address]].concat();
        let non_determinism = NonDeterminism::default().with_ram(memory);
        let sponge_state = VmHasherState { state: rng.gen() };

        (stack, non_determinism, vec![], Some(sponge_state))
    }
}

#[cfg(test)]
mod test {
    use std::{cell::RefCell, collections::HashMap, rc::Rc};

    use rand::thread_rng;
    use rand::{rngs::StdRng, Rng, RngCore, SeedableRng};
    use triton_vm::proof_item::{FriResponse, ProofItem};
    use triton_vm::vm::VMState;
    use triton_vm::{triton_asm, BFieldElement, NonDeterminism, Program, PublicInput};
    use twenty_first::shared_math::bfield_codec::BFieldCodec;
    use twenty_first::shared_math::x_field_element::XFieldElement;
    use twenty_first::util_types::algebraic_hasher::Domain;

    use crate::library::Library;
    use crate::structure::tasm_object::{decode_from_memory_with_size, encode_to_memory};
    use crate::{
        empty_stack, execute_with_terminal_state,
        linker::link_for_isolated_run,
        procedure::{Procedure, ShadowedProcedure},
        snippet::RustShadow,
        test_helpers::test_rust_equivalence_given_complete_state,
        VmHasherState,
    };
    use crate::{Digest, DIGEST_LENGTH};

    use super::{Dequeue, VmProofStream};

    #[test]
    fn dequeue_from_proof_stream_with_up_to_3_items() {
        for n in 1..=3 {
            dequeue_from_proof_stream_with_given_number_of_items(n);
        }
    }

    fn dequeue_from_proof_stream_with_given_number_of_items(num_items: usize) {
        let dequeue = ShadowedProcedure::new(Dequeue {});
        let (stack, non_determinism) = very_small_initial_state(num_items);
        let sponge_state = VmHasherState::new(Domain::VariableLength);

        test_rust_equivalence_given_complete_state(
            &dequeue,
            &stack,
            &[],
            &non_determinism,
            &Some(sponge_state),
            0,
            None,
        );
    }

    fn very_small_initial_state(
        num_items: usize,
    ) -> (Vec<BFieldElement>, NonDeterminism<BFieldElement>) {
        let address = 0_u64.into();
        let proof_stream = proof_stream_with_num_items(num_items);

        let mut memory = HashMap::default();
        for (i, b) in proof_stream.encode().into_iter().enumerate() {
            memory.insert(address + BFieldElement::new(i as u64), b);
        }

        let stack = [empty_stack(), vec![address]].concat();
        let non_determinism = NonDeterminism::default().with_ram(memory);

        (stack, non_determinism)
    }

    fn proof_stream_with_num_items(num_items: usize) -> VmProofStream {
        let proof_item_0 = ProofItem::Log2PaddedHeight(0);
        let proof_item_1 = ProofItem::MerkleRoot(Default::default());
        let proof_item_2 = ProofItem::OutOfDomainExtRow(vec![XFieldElement::from(0); 3]);
        let proof_items = [proof_item_0, proof_item_1, proof_item_2];
        VmProofStream::new(&proof_items[..num_items])
    }

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
            let (stack, nondeterminism, _stdin, sponge_state) =
                dequeue.pseudorandom_initial_state(rng.gen(), None);

            let stdin = vec![];
            let _vm_output_state = test_rust_equivalence_given_complete_state(
                &algorithm,
                &stack,
                &stdin,
                &nondeterminism,
                &sponge_state,
                0,
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
            let nondeterminism = NonDeterminism::new(vec![]);

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

    #[test]
    fn test_can_dequeue_three_items_of_distinct_type() {
        let mut rng = thread_rng();

        // create proof stream with at least 3 items of various types
        let merkle_root = rng.gen::<Digest>();
        let fri_response = FriResponse {
            auth_structure: rng.gen::<[Digest; 10]>().to_vec(),
            revealed_leaves: rng.gen::<[XFieldElement; 16]>().to_vec(),
        };
        let fri_response_length = fri_response.encode().len();
        let fri_codeword = rng.gen::<[XFieldElement; 32]>().to_vec();
        let fri_codeword_length = fri_codeword.encode().len();
        let proof_items = vec![
            ProofItem::MerkleRoot(merkle_root),
            ProofItem::FriCodeword(fri_codeword.clone()),
            ProofItem::FriResponse(fri_response.clone()),
        ];
        let proof_stream = VmProofStream::new(&proof_items);

        // populate stack and memory
        let mut stack = empty_stack();
        let mut memory = HashMap::<BFieldElement, BFieldElement>::new();
        let address = BFieldElement::new(rng.next_u64() & 0xffffff);
        stack.push(address);
        encode_to_memory(&mut memory, address, proof_stream);

        // make program
        let mut library = Library::empty();
        let dequeue = library.import(Box::new(Dequeue));
        let proof_item_as_merkle_root = VmProofStream::proof_item_as_merkle_root_code();
        let proof_item_as_fri_codeword = VmProofStream::proof_item_as_fri_codeword_code();
        let proof_item_as_fri_response = VmProofStream::proof_item_as_fri_response_code();
        let entrypoint = "tasm_recufier_proof_stream_dequeue_test_simple_program".to_string();
        let simple_program = triton_asm! {
            {entrypoint}:
                // _ *proof_stream
                call {dequeue}                  // _ *proof_stream *merkle_root_ev
                {&proof_item_as_merkle_root}    // _ *proof_stream *merkle_root
                swap 1                          // _ *merkle_root *proof_stream

                call {dequeue}                  // _ *merkle_root *proof_stream *fri_codeword_ev
                {&proof_item_as_fri_codeword}   // _ *merkle_root *proof_stream *fri_codeword
                swap 1                          // _ *merkle_root *fri_codeword *proof_stream

                call {dequeue}                  // _ *merkle_root *fri_codeword *proof_stream *fri_response_ev
                {&proof_item_as_fri_response}   // _ *merkle_root *fri_codeword *proof_stream *fri_response
                swap 1 pop 1
                return

                // _ *merkle_root *fri_codeword *fri_response
        };

        // compile
        let library_code = library.all_imports();
        let code = triton_asm!(
            call {entrypoint}
            halt

            {&simple_program}
            {&library_code}
        );

        println!("Compiled program.");

        // run vm
        let program = Program::new(&code);
        let mut state = VMState::new(
            &program,
            PublicInput::default(),
            NonDeterminism::new(vec![]).with_ram(memory),
        );
        state.op_stack.stack = stack;
        state.sponge_state = Some(rng.gen());
        state.run().unwrap();

        println!("Ran VM.");

        // read addresses
        let response_address = state.op_stack.stack.pop().unwrap();
        let codeword_address = state.op_stack.stack.pop().unwrap();
        let root_address = state.op_stack.stack.pop().unwrap();

        // read objects from memory
        let read_merkle_root =
            *decode_from_memory_with_size::<Digest>(&state.ram, root_address, DIGEST_LENGTH)
                .unwrap();
        let read_fri_codeword = *decode_from_memory_with_size::<Vec<XFieldElement>>(
            &state.ram,
            codeword_address,
            fri_codeword_length,
        )
        .unwrap();
        let read_fri_response = *decode_from_memory_with_size::<FriResponse>(
            &state.ram,
            response_address,
            fri_response_length,
        )
        .unwrap();

        // assert equalities
        assert_eq!(merkle_root, read_merkle_root);
        assert_eq!(fri_codeword, read_fri_codeword);
        assert_eq!(fri_response, read_fri_response);
    }
}
