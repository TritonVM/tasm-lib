use triton_vm::prelude::*;
use triton_vm::proof_item::ProofItemVariant;

use crate::data_type::DataType;
use crate::library::Library;
use crate::traits::basic_snippet::BasicSnippet;

/// Reads a proof item of the supplied type from the [`ProofStream`].
/// Crashes Triton VM if the proof item is not of the expected type.
/// Updates an internal pointer to the next proof item.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct DequeueNextAs {
    pub proof_item: ProofItemVariant,
}

impl DequeueNextAs {
    fn item_name(&self) -> String {
        self.proof_item.to_string().to_lowercase()
    }
}

impl BasicSnippet for DequeueNextAs {
    fn inputs(&self) -> Vec<(DataType, String)> {
        vec![(DataType::VoidPointer, "*proof_item_iter".to_string())]
    }

    fn outputs(&self) -> Vec<(DataType, String)> {
        let payload_pointer_str = format!("*{}_payload", self.item_name());
        vec![(DataType::VoidPointer, payload_pointer_str)]
    }

    fn entrypoint(&self) -> String {
        let proof_item_name = self.item_name();
        format!("tasm_recufier_proof_stream_dequeue_next_as_{proof_item_name}")
    }

    /// BEFORE: _ *proof_item_iter
    /// AFTER:  _ *proof_item_payload
    fn code(&self, _: &mut Library) -> Vec<LabelledInstruction> {
        triton_asm! {
            {self.entrypoint()}:
            read_mem 1
                hint proof_item_list_element_size_pointer = stack[1]
            push 1 add
            swap 1              // _ *proof_item_iter *proof_item_list_element_size

            push 1 add
                hint proof_item_discriminant_pointer = stack[0]
            read_mem 1          // _ *proof_item_iter discriminant *proof_item_list_element_size
                hint proof_item_list_element_size_pointer = stack[0]
                hint proof_item_discriminant = stack[1]
            swap 1
            push {self.proof_item.bfield_codec_discriminant()}
                hint expected_proof_item_discriminant = stack[0]
            eq assert           // _ *proof_item_iter *proof_item_list_element_size

            dup 0 read_mem 1    // _ *proof_item_iter *p_i_l_e_s p_i_l_e_s (*p_i_l_e_s - 1)
                hint proof_item_length = stack[1]
            push 2
                hint length_indicator_and_read_mem_offset = stack[0]
            add add             // _ *proof_item_iter *p_i_l_e_s *next_proof_item_list_element_size
                hint next_proof_item_list_element_size_pointer = stack[0]

            swap 1 swap 2       // _ *p_i_l_e_s *next_proof_item_list_element_size *proof_item_iter
            write_mem 1 pop 1   // _ *proof_item_list_element_size

            push 2 add          // _ *proof_item_payload
                hint proof_item_payload_pointer = stack[0]

            return
        }
    }
}

#[cfg(test)]
mod test {
    use std::collections::HashMap;

    use arbitrary::Arbitrary;
    use arbitrary::Unstructured;
    use itertools::Itertools;
    use num_traits::One;
    use num_traits::Zero;
    use proptest_arbitrary_interop::arb;
    use rand::prelude::*;
    use test_strategy::proptest;
    use triton_vm::proof_item::ProofItem;
    use triton_vm::proof_stream::ProofStream;

    use crate::empty_stack;
    use crate::memory::encode_to_memory;
    use crate::snippet_bencher::BenchmarkCase;
    use crate::test_helpers::test_rust_equivalence_given_complete_state;
    use crate::traits::procedure::Procedure;
    use crate::traits::procedure::ProcedureInitialState;
    use crate::traits::procedure::ShadowedProcedure;

    use super::*;

    impl Procedure for DequeueNextAs {
        fn rust_shadow(
            &self,
            stack: &mut Vec<BFieldElement>,
            memory: &mut HashMap<BFieldElement, BFieldElement>,
            _: &NonDeterminism<BFieldElement>,
            _: &[BFieldElement],
            _: &mut Option<tip5::Tip5State>,
        ) -> Vec<BFieldElement> {
            let proof_iter_pointer = stack.pop().unwrap();
            let &current_proof_item_list_element_size_pointer =
                memory.get(&proof_iter_pointer).unwrap();

            let discriminant_pointer =
                current_proof_item_list_element_size_pointer + BFieldElement::one();
            let discriminant = memory.get(&discriminant_pointer).unwrap().value() as usize;
            let expected_discriminant = self.proof_item.bfield_codec_discriminant();
            assert_eq!(expected_discriminant, discriminant);

            let &current_proof_item_list_element_size = memory
                .get(&current_proof_item_list_element_size_pointer)
                .unwrap();
            let next_proof_item_list_element_size_pointer =
                current_proof_item_list_element_size_pointer
                    + current_proof_item_list_element_size
                    + BFieldElement::one();
            memory.insert(
                proof_iter_pointer,
                next_proof_item_list_element_size_pointer,
            );

            let proof_item_payload_pointer = discriminant_pointer + BFieldElement::one();
            stack.push(proof_item_payload_pointer);

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

    impl DequeueNextAs {
        fn initial_state_from_proof_stream_and_address(
            proof_stream: ProofStream<Tip5>,
            address: BFieldElement,
        ) -> ProcedureInitialState {
            let mut ram = HashMap::new();
            encode_to_memory(&mut ram, address, proof_stream);

            // uses highly specific knowledge of `BFieldCodec`
            let address_of_first_element = address + BFieldElement::new(2);
            let proof_iter_address = address - BFieldElement::one();
            ram.insert(proof_iter_address, address_of_first_element);

            ProcedureInitialState {
                stack: [empty_stack(), vec![proof_iter_address]].concat(),
                nondeterminism: NonDeterminism::default().with_ram(ram),
                public_input: vec![],
                sponge_state: None,
            }
        }

        fn pseudorandom_new(seed: [u8; 32]) -> Self {
            let mut unstructured = Unstructured::new(&seed);
            let proof_item: ProofItem = Arbitrary::arbitrary(&mut unstructured).unwrap();
            let proof_item = proof_item.into();
            Self { proof_item }
        }

        /// The item that is to be `Dequeued` must match the variant stored in `self`.
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
            test_rust_equivalence_given_complete_state(
                &ShadowedProcedure::new(self),
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
        let dequeue_next_as = DequeueNextAs { proof_item };
        dequeue_next_as.test_rust_equivalence(small_merkle_root_initial_state());
    }

    fn small_merkle_root_initial_state() -> ProcedureInitialState {
        let dummy_digest = Digest::new([42, 43, 44, 45, 46].map(BFieldElement::new));
        let proof_item = ProofItem::MerkleRoot(dummy_digest);
        let mut proof_stream = ProofStream::<Tip5>::new();
        proof_stream.enqueue(proof_item.clone());
        proof_stream.enqueue(proof_item);

        let address = BFieldElement::zero();
        DequeueNextAs::initial_state_from_proof_stream_and_address(proof_stream, address)
    }

    #[proptest]
    fn dequeuing_as_suggested_element_is_equivalent_in_rust_and_tasm(seed: [u8; 32]) {
        let dequeue_next_as = DequeueNextAs::pseudorandom_new(seed);
        let initial_state = dequeue_next_as.pseudorandom_initial_state(seed, None);
        dequeue_next_as.test_rust_equivalence(initial_state);
    }

    /// Helps testing dequeuing multiple items.
    struct TestHelperDequeueMultipleAs {
        proof_items: Vec<ProofItemVariant>,
    }

    impl BasicSnippet for TestHelperDequeueMultipleAs {
        fn inputs(&self) -> Vec<(DataType, String)> {
            vec![(DataType::VoidPointer, "*proof_item_iter".to_string())]
        }

        fn outputs(&self) -> Vec<(DataType, String)> {
            vec![(DataType::VoidPointer, "*proof_item_iter".to_string())]
        }

        fn entrypoint(&self) -> String {
            "test_helper_dequeue_multiple_as".to_string()
        }

        fn code(&self, library: &mut Library) -> Vec<LabelledInstruction> {
            let dequeue_next_as_entrypoints = self
                .proof_items
                .iter()
                .map(|&proof_item| library.import(Box::new(DequeueNextAs { proof_item })))
                .collect::<Vec<_>>();

            let code_body = dequeue_next_as_entrypoints
                .into_iter()
                .flat_map(|snippet| triton_asm!(dup 0 call {snippet} pop 1))
                .collect_vec();

            triton_asm!({self.entrypoint()}: {&code_body} return)
        }
    }

    impl Procedure for TestHelperDequeueMultipleAs {
        fn rust_shadow(
            &self,
            stack: &mut Vec<BFieldElement>,
            memory: &mut HashMap<BFieldElement, BFieldElement>,
            non_determinism: &NonDeterminism<BFieldElement>,
            std_in: &[BFieldElement],
            sponge_state: &mut Option<tip5::Tip5State>,
        ) -> Vec<BFieldElement> {
            let &proof_iter_pointer = stack.last().unwrap();
            for &proof_item in &self.proof_items {
                let dequeue_next_as = DequeueNextAs { proof_item };
                stack.push(proof_iter_pointer);
                dequeue_next_as.rust_shadow(stack, memory, non_determinism, std_in, sponge_state);
                stack.pop().unwrap();
            }
            vec![]
        }

        fn pseudorandom_initial_state(
            &self,
            seed: [u8; 32],
            _: Option<BenchmarkCase>,
        ) -> ProcedureInitialState {
            let mut rng = StdRng::from_seed(seed);
            let mut proof_stream = ProofStream::<Tip5>::new();
            for &proof_item in &self.proof_items {
                let dequeue_next_as = DequeueNextAs { proof_item };
                let item = dequeue_next_as.pseudorandom_proof_item_for_self(rng.gen());
                proof_stream.enqueue(item);
            }
            DequeueNextAs::initial_state_from_proof_stream_and_address(proof_stream, rng.gen())
        }
    }

    impl TestHelperDequeueMultipleAs {
        fn test_rust_equivalence(self, initial_state: ProcedureInitialState) {
            test_rust_equivalence_given_complete_state(
                &ShadowedProcedure::new(self),
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
    fn dequeue_two() {
        let proof_items = vec![ProofItemVariant::MerkleRoot, ProofItemVariant::MerkleRoot];
        let dequeue_multiple = TestHelperDequeueMultipleAs { proof_items };
        let initial_state = dequeue_multiple.pseudorandom_initial_state([0; 32], None);
        dequeue_multiple.test_rust_equivalence(initial_state);
    }

    #[proptest]
    fn dequeue_multiple(#[strategy(arb())] proof_items: Vec<ProofItem>, seed: [u8; 32]) {
        let proof_items = proof_items.into_iter().map(|i| i.into()).collect_vec();
        let dequeue_multiple = TestHelperDequeueMultipleAs { proof_items };
        let initial_state = dequeue_multiple.pseudorandom_initial_state(seed, None);
        dequeue_multiple.test_rust_equivalence(initial_state);
    }
}
