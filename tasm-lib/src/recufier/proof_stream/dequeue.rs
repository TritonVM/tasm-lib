use std::cmp::max;
use std::collections::HashMap;

use num_traits::Zero;
use rand::rngs::StdRng;
use rand::Rng;
use rand::RngCore;
use rand::SeedableRng;
use triton_vm::prelude::*;
use triton_vm::proof_item::ProofItem;
use triton_vm::proof_item::ProofItemVariant;

use crate::data_type::DataType;
use crate::empty_stack;
use crate::field;
use crate::field_with_size;
use crate::hashing::sponge_hasher::absorb::Absorb;
use crate::library::Library;
use crate::snippet_bencher::BenchmarkCase;
use crate::structure::tasm_object::TasmObject;
use crate::traits::basic_snippet::BasicSnippet;
use crate::traits::procedure::Procedure;
use crate::traits::procedure::ProcedureInitialState;
use crate::VmHasherState;

use super::vm_proof_stream::VmProofStream;

/// Reads a proof item of the supplied type from the [`ProofStream`].
/// Crashes Triton VM if the proof item is not of the expected type.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct DequeueAs {
    pub proof_item: ProofItemVariant,
}

impl DequeueAs {
    fn item_name(&self) -> String {
        self.proof_item.to_string().to_lowercase()
    }
}

impl BasicSnippet for DequeueAs {
    fn inputs(&self) -> Vec<(DataType, String)> {
        let input_name = "*current_proof_item_list_element_size".to_string();

        vec![(DataType::VoidPointer, input_name)]
    }

    fn outputs(&self) -> Vec<(DataType, String)> {
        let payload_pointer_str = format!("*{}_payload", self.item_name());
        let payload_pointer = (DataType::VoidPointer, payload_pointer_str);

        let next_element_pointer_str = "*next_proof_item_list_element_size".to_string();
        let next_proof_item_pointer = (DataType::VoidPointer, next_element_pointer_str);

        vec![payload_pointer, next_proof_item_pointer]
    }

    fn entrypoint(&self) -> String {
        let proof_item_name = self.item_name();
        format!("tasm_recufier_proof_stream_dequeue_as_{}", proof_item_name)
    }

    /// BEFORE: _ *current_proof_item_list_element_size
    /// AFTER:  _ *proof_item_payload *next_proof_item_list_element_size
    fn code(&self, _: &mut Library) -> Vec<LabelledInstruction> {
        triton_asm! {
            {self.entrypoint()}:
            push 1 add
            read_mem 1          // _ discriminant *current_proof_item_list_element_size
                hint proof_item_discriminant = stack[1]
            swap 1
            push {self.proof_item.bfield_codec_discriminant()}
                hint expected_proof_item_discriminant = stack[0]
            eq assert           // _ *current_proof_item_list_element_size

            dup 0 push 2 add    // _ *current_proof_item_list_element_size *proof_item_payload
                hint proof_item_payload_pointer = stack[0]

            swap 1
            read_mem 1 push 1
            add add             // _ *proof_item_payload *next_proof_item_list_element_size
                hint next_proof_item_list_element_size_pointer = stack[0]

            return
        }
    }
}

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
        let absorb = library.import(Box::new(Absorb));
        triton_asm!(
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
        proof_stream.dequeue().unwrap();

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
    ) -> ProcedureInitialState {
        // populate with random proof items
        let mut rng = StdRng::from_seed(seed);
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
        let address = rng.gen();
        let sequence = proof_stream.encode();
        let mut memory = HashMap::new();
        for (i, b) in sequence.into_iter().enumerate() {
            memory.insert(address + BFieldElement::new(i as u64), b);
        }

        let stack = [empty_stack(), vec![address]].concat();
        let non_determinism = NonDeterminism::default().with_ram(memory);
        let sponge_state = VmHasherState { state: rng.gen() };

        ProcedureInitialState {
            stack,
            nondeterminism: non_determinism,
            public_input: vec![],
            sponge_state: Some(sponge_state),
        }
    }
}

#[cfg(test)]
mod test {
    use std::cell::RefCell;
    use std::rc::Rc;

    use arbitrary::Arbitrary;
    use arbitrary::Unstructured;
    use num_traits::One;
    use rand::thread_rng;
    use test_strategy::proptest;
    use triton_vm::proof_item::FriResponse;
    use triton_vm::proof_stream::ProofStream;
    use twenty_first::util_types::algebraic_hasher::Domain;

    use crate::execute_with_terminal_state;
    use crate::linker::link_for_isolated_run;
    use crate::memory::encode_to_memory;
    use crate::structure::tasm_object::decode_from_memory_with_size;
    use crate::test_helpers::test_rust_equivalence_given_complete_state;
    use crate::traits::procedure::ShadowedProcedure;
    use crate::traits::rust_shadow::RustShadow;
    use crate::DIGEST_LENGTH;

    use super::*;

    impl Procedure for DequeueAs {
        fn rust_shadow(
            &self,
            stack: &mut Vec<BFieldElement>,
            memory: &mut HashMap<BFieldElement, BFieldElement>,
            _: &NonDeterminism<BFieldElement>,
            _: &[BFieldElement],
            _: &mut Option<VmHasherState>,
        ) -> Vec<BFieldElement> {
            let current_proof_item_list_element_size_pointer = stack.pop().unwrap();
            let &current_proof_item_list_element_size = memory
                .get(&current_proof_item_list_element_size_pointer)
                .unwrap();
            let next_proof_item_list_element_size_pointer =
                current_proof_item_list_element_size_pointer + current_proof_item_list_element_size;

            let discriminant_pointer =
                current_proof_item_list_element_size_pointer + BFieldElement::one();
            let discriminant = memory.get(&discriminant_pointer).unwrap().value() as usize;
            let expected_discriminant = self.proof_item.bfield_codec_discriminant();
            assert_eq!(expected_discriminant, discriminant);

            let proof_item_payload_pointer = discriminant_pointer + BFieldElement::one();

            stack.push(proof_item_payload_pointer);
            stack.push(next_proof_item_list_element_size_pointer);

            vec![]
        }

        fn pseudorandom_initial_state(
            &self,
            seed: [u8; 32],
            _: Option<BenchmarkCase>,
        ) -> ProcedureInitialState {
            let mut rng = StdRng::from_seed(seed);
            let mut proof_stream = ProofStream::<Tip5>::new();
            proof_stream.enqueue(self.pseudorandom_proof_item_for_self(rng.gen()));
            proof_stream.enqueue(self.pseudorandom_proof_item_for_self(rng.gen()));

            Self::initial_state_from_proof_stream_and_address(proof_stream, rng.gen())
        }
    }

    impl DequeueAs {
        fn initial_state_from_proof_stream_and_address(
            proof_stream: ProofStream<Tip5>,
            address: BFieldElement,
        ) -> ProcedureInitialState {
            let ram = proof_stream
                .encode()
                .into_iter()
                .enumerate()
                .map(|(idx, elt)| (address + BFieldElement::new(idx as u64), elt))
                .collect();

            // uses highly specific knowledge of `BFieldCodec`
            let address_of_first_element = address + BFieldElement::new(2);
            ProcedureInitialState {
                stack: [empty_stack(), vec![address_of_first_element]].concat(),
                nondeterminism: NonDeterminism::default().with_ram(ram),
                public_input: vec![],
                sponge_state: None,
            }
        }

        fn pseudorandom_proof_item_variant(seed: [u8; 32]) -> ProofItemVariant {
            let mut unstructured = Unstructured::new(&seed);
            let item: ProofItem = Arbitrary::arbitrary(&mut unstructured).unwrap();
            item.into()
        }

        fn pseudorandom_proof_item_for_self(&self, seed: [u8; 32]) -> ProofItem {
            let mut rng = StdRng::from_seed(seed);
            let proof_stream_seed: [u8; 100] = rng.gen();
            let mut unstructured = Unstructured::new(&proof_stream_seed);

            match &self.proof_item {
                ProofItemVariant::MerkleRoot => {
                    ProofItem::MerkleRoot(Arbitrary::arbitrary(&mut unstructured).unwrap())
                }
                ProofItemVariant::OutOfDomainBaseRow => {
                    ProofItem::OutOfDomainBaseRow(Arbitrary::arbitrary(&mut unstructured).unwrap())
                }
                ProofItemVariant::OutOfDomainExtRow => {
                    ProofItem::OutOfDomainExtRow(Arbitrary::arbitrary(&mut unstructured).unwrap())
                }
                ProofItemVariant::OutOfDomainQuotientSegments => {
                    ProofItem::OutOfDomainQuotientSegments(
                        Arbitrary::arbitrary(&mut unstructured).unwrap(),
                    )
                }
                ProofItemVariant::AuthenticationStructure => ProofItem::AuthenticationStructure(
                    Arbitrary::arbitrary(&mut unstructured).unwrap(),
                ),
                ProofItemVariant::MasterBaseTableRows => {
                    ProofItem::MasterBaseTableRows(Arbitrary::arbitrary(&mut unstructured).unwrap())
                }
                ProofItemVariant::MasterExtTableRows => {
                    ProofItem::MasterExtTableRows(Arbitrary::arbitrary(&mut unstructured).unwrap())
                }
                ProofItemVariant::Log2PaddedHeight => {
                    ProofItem::Log2PaddedHeight(Arbitrary::arbitrary(&mut unstructured).unwrap())
                }
                ProofItemVariant::QuotientSegmentsElements => ProofItem::QuotientSegmentsElements(
                    Arbitrary::arbitrary(&mut unstructured).unwrap(),
                ),
                ProofItemVariant::FriCodeword => {
                    ProofItem::FriCodeword(Arbitrary::arbitrary(&mut unstructured).unwrap())
                }
                ProofItemVariant::FriResponse => {
                    ProofItem::FriResponse(Arbitrary::arbitrary(&mut unstructured).unwrap())
                }
            }
        }

        fn test_rust_equivalence(self, initial_state: ProcedureInitialState) {
            let dequeue_as = ShadowedProcedure::new(self);
            test_rust_equivalence_given_complete_state(
                &dequeue_as,
                &initial_state.stack,
                &initial_state.public_input,
                &initial_state.nondeterminism,
                &initial_state.sponge_state,
                0,
                None,
            );
        }
    }

    #[test]
    fn dequeueing_a_merkle_root_is_equivalent_in_rust_and_tasm() {
        let proof_item = ProofItemVariant::MerkleRoot;
        let dequeue_as = DequeueAs { proof_item };
        dequeue_as.test_rust_equivalence(small_merkle_root_initial_state());
    }

    fn small_merkle_root_initial_state() -> ProcedureInitialState {
        let dummy_digest = Digest::new([42, 43, 44, 45, 46].map(BFieldElement::new));
        let proof_item = ProofItem::MerkleRoot(dummy_digest);
        let mut proof_stream = ProofStream::<Tip5>::new();
        proof_stream.enqueue(proof_item.clone());
        proof_stream.enqueue(proof_item);

        DequeueAs::initial_state_from_proof_stream_and_address(proof_stream, BFieldElement::zero())
    }

    #[proptest]
    fn dequeuing_as_suggested_element_is_equivalent_in_rust_and_tasm(seed: [u8; 32]) {
        let proof_item = DequeueAs::pseudorandom_proof_item_variant(seed);
        let dequeue_as = DequeueAs { proof_item };
        let initial_state = dequeue_as.pseudorandom_initial_state(seed, None);
        dequeue_as.test_rust_equivalence(initial_state);
    }

    #[test]
    fn dequeue_from_proof_stream_with_up_to_3_items() {
        for n in 1..=3 {
            dequeue_from_proof_stream_with_given_number_of_items(n);
        }
    }

    fn dequeue_from_proof_stream_with_given_number_of_items(num_items: usize) {
        let dequeue = ShadowedProcedure::new(Dequeue);
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

        let dequeue = Dequeue;
        let algorithm = ShadowedProcedure::new(dequeue.clone());

        for _ in 0..num_states {
            let ProcedureInitialState {
                stack,
                nondeterminism,
                sponge_state,
                ..
            } = dequeue.pseudorandom_initial_state(rng.gen(), None);

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

        let dequeue = Dequeue;

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
