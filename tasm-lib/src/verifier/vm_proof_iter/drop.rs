use triton_vm::prelude::*;

use crate::prelude::*;
use crate::verifier::vm_proof_iter::shared::vm_proof_iter_type;

/// Signals the end of the lifetime of a VmProofIter
///
/// This snippet crashes the VM if the VmProofIter does not end up in a sane
/// state after a verification.
#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub struct Drop;

impl BasicSnippet for Drop {
    fn parameters(&self) -> Vec<(DataType, String)> {
        vec![(
            DataType::StructRef(vm_proof_iter_type()),
            "vm_proof_iter".to_owned(),
        )]
    }

    fn return_values(&self) -> Vec<(DataType, String)> {
        vec![]
    }

    fn entrypoint(&self) -> String {
        "tasmlib_verifier_vm_proof_iter_drop".to_owned()
    }

    fn code(&self, _library: &mut Library) -> Vec<LabelledInstruction> {
        let entrypoint = self.entrypoint();

        triton_asm!(
            {entrypoint}:
                // _ *vm_proof_iter

                /* Assert that item count matches indicated number of items */
                addi 4
                read_mem 5
                pop 1

                // _ current_item_count total_item_count proof_start_pointer proof_length current_item_pointer

                place 2
                // _ current_item_count total_item_count current_item_pointer proof_start_pointer proof_length

                add
                addi 1
                eq
                // _ current_item_count total_item_count (current_item_pointer == proof_start_pointer + proof_length + 1)

                assert error_id 50
                // _ current_item_count total_item_count

                eq
                assert error_id 60
                // _

                return
        )
    }
}

#[cfg(test)]
mod tests {
    use arbitrary::Arbitrary;
    use arbitrary::Unstructured;
    use triton_vm::proof_item::ProofItemVariant;
    use triton_vm::proof_stream::ProofStream;

    use super::*;
    use crate::test_prelude::*;
    use crate::verifier::vm_proof_iter::dequeue_next_as::DequeueNextAs;
    use crate::verifier::vm_proof_iter::shared::vm_proof_iter_struct::VmProofIter;

    #[test]
    fn rust_shadow() {
        ShadowedAccessor::new(Drop).test();
    }

    #[test]
    fn negative_test_proof_len_mismatch() {
        let proof_ptr = bfe!(450);
        let proof_len = 10_000u32;
        let correct_proof_end = proof_ptr + bfe!(proof_len);
        let bad_proof_length = VmProofIter {
            current_item_count: 12,
            total_item_count: 12,
            proof_start_pointer: proof_ptr,
            proof_length: proof_len,
            current_item_pointer: correct_proof_end + bfe!(4),
        };

        test_assertion_failure(
            &ShadowedAccessor::new(Drop),
            Drop.init_state(bad_proof_length).into(),
            &[50],
        );
    }

    #[test]
    fn negative_test_proof_item_count_mismatch() {
        let proof_ptr = bfe!(450);
        let proof_len = 10_000u32;
        let correct_proof_end = proof_ptr + bfe!(proof_len);
        let bad_proof_length = VmProofIter {
            current_item_count: 16,
            total_item_count: 12,
            proof_start_pointer: proof_ptr,
            proof_length: proof_len,
            current_item_pointer: correct_proof_end,
        };

        test_assertion_failure(
            &ShadowedAccessor::new(Drop),
            Drop.init_state(bad_proof_length).into(),
            &[50],
        );
    }

    impl Drop {
        fn init_state(&self, vm_proof_iter: VmProofIter) -> AccessorInitialState {
            let mut memory = HashMap::default();
            let vm_iter_ptr = bfe!(1u64 << 32);
            encode_to_memory(&mut memory, vm_iter_ptr, &vm_proof_iter);

            let stack = [self.init_stack_for_isolated_run(), vec![vm_iter_ptr]].concat();
            AccessorInitialState { stack, memory }
        }
    }

    impl Accessor for Drop {
        fn rust_shadow(
            &self,
            stack: &mut Vec<BFieldElement>,
            memory: &HashMap<BFieldElement, BFieldElement>,
        ) {
            let vm_proof_iter_ptr = stack.pop().unwrap();
            let vm_proof_iter =
                *VmProofIter::decode_from_memory(memory, vm_proof_iter_ptr).unwrap();
            assert_eq!(
                vm_proof_iter.current_item_count,
                vm_proof_iter.total_item_count
            );

            assert_eq!(
                vm_proof_iter.proof_start_pointer + bfe!(vm_proof_iter.proof_length + 1),
                vm_proof_iter.current_item_pointer,
                "{} + {} and {} must match",
                vm_proof_iter.proof_start_pointer,
                vm_proof_iter.proof_length,
                vm_proof_iter.current_item_pointer
            );
        }

        fn pseudorandom_initial_state(
            &self,
            seed: [u8; 32],
            bench_case: Option<BenchmarkCase>,
        ) -> AccessorInitialState {
            let mut rng = StdRng::from_seed(seed);

            let fake_proof_stream = match bench_case {
                Some(BenchmarkCase::CommonCase) => {
                    let proof_item_variants = vec![ProofItemVariant::MerkleRoot; 20];
                    DequeueNextAs::pseudorandom_proof_stream(proof_item_variants, seed)
                }
                Some(BenchmarkCase::WorstCase) => {
                    let proof_item_variants = vec![ProofItemVariant::MerkleRoot; 40];
                    DequeueNextAs::pseudorandom_proof_stream(proof_item_variants, seed)
                }
                None => {
                    let bigger_seed: Vec<u8> = (0..1_000_000).map(|_| rng.random()).collect();
                    let unstructured = Unstructured::new(bigger_seed.as_ref());
                    ProofStream::arbitrary_take_rest(unstructured).unwrap()
                }
            };

            let proof_ptr = bfe!(rng.random_range(0..20u32));
            let fake_proof = fake_proof_stream.clone().into();

            // Construct `VmProofIter` as we expect to see it after verifying proof
            let mut vm_proof_iter = VmProofIter::new(proof_ptr, &fake_proof);
            vm_proof_iter.current_item_count = fake_proof_stream.items.len().try_into().unwrap();
            vm_proof_iter.current_item_pointer = proof_ptr + bfe!(fake_proof.0.len() as u64 + 2);

            self.init_state(vm_proof_iter)
        }
    }
}

#[cfg(test)]
mod benches {
    use super::*;
    use crate::test_prelude::*;

    #[test]
    fn benchmark() {
        ShadowedAccessor::new(Drop).bench();
    }
}
