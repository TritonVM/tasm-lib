use triton_vm::prelude::*;

use crate::prelude::*;
use crate::verifier::stark_verify::NUM_PROOF_ITEMS_EXCLUDING_FRI;
use crate::verifier::stark_verify::NUM_PROOF_ITEMS_PER_FRI_ROUND;
use crate::verifier::vm_proof_iter::shared::vm_proof_iter_type;

/// Create a new `VmProofIter` instance.
///
/// A `VmProofIter` points to the next proof item in memory to be read in
/// verifying a proof. It also counts how many proof items have been read and
/// records the starting point and indicated length of the proof.
#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub struct New;

impl New {
    pub const FIRST_PROOF_ITEM_OFFSET: u64 = 4;
    pub const MAX_PROOF_SIZE: usize = 1 << 26;

    const MAX_NUMBER_OF_FRI_ROUNDS: usize = 24;
    const SAFETY_MARGIN_PER_FRI_ROUND: usize = 1;
    pub const MAX_NUM_PROOF_ITEMS: usize = Self::MAX_NUMBER_OF_FRI_ROUNDS
        * (NUM_PROOF_ITEMS_PER_FRI_ROUND + Self::SAFETY_MARGIN_PER_FRI_ROUND)
        + NUM_PROOF_ITEMS_EXCLUDING_FRI;
}

impl BasicSnippet for New {
    fn parameters(&self) -> Vec<(DataType, String)> {
        vec![(DataType::VoidPointer, "*proof".to_string())]
    }

    fn return_values(&self) -> Vec<(DataType, String)> {
        vec![(
            DataType::StructRef(vm_proof_iter_type()),
            "vm_proof_iter".to_owned(),
        )]
    }

    fn entrypoint(&self) -> String {
        "tasmlib_verifier_vm_proof_iter_new".to_owned()
    }

    fn code(&self, library: &mut Library) -> Vec<LabelledInstruction> {
        let entrypoint = self.entrypoint();
        let dyn_malloc = library.import(Box::new(DynMalloc));

        triton_asm!(
            {entrypoint}:
                // _ *proof

                addi {Self::FIRST_PROOF_ITEM_OFFSET}
                dup 0
                addi -1
                read_mem {Self::FIRST_PROOF_ITEM_OFFSET}
                addi 1
                push 0
                place 5
                hint first_proof_item: u32 = stack[6]
                hint current_proof_item: u32 = stack[5]
                hint num_proof_items: u32 = stack[4]
                hint proof_len: u32 = stack[1]
                // _ *first_proof_item current_proof_item num_proof_items (proof_len - 2) (proof_len - 1) proof_len *proof


                /* Verify consistent size-indicators */
                place 3
                place 2
                // _ *first_proof_item current_proof_item num_proof_items *proof proof_len (proof_len - 2) (proof_len - 1)

                addi 1
                dup 2
                eq
                assert error_id 300
                // _ *first_proof_item current_proof_item num_proof_items *proof proof_len (proof_len - 2)

                addi 2
                dup 1
                eq
                assert error_id 301
                // _ *first_proof_item current_proof_item num_proof_items *proof proof_len


                /* Verify sane sizes */
                push {Self::MAX_PROOF_SIZE}
                dup 1
                lt
                // _ *first_proof_item current_proof_item num_proof_items *proof proof_len (proof_len < MAX_SIZE)

                assert error_id 302

                push {Self::MAX_NUM_PROOF_ITEMS}
                dup 3
                lt
                // _ *first_proof_item current_proof_item num_proof_items *proof proof_len (num_proof_items < MAX_NUM_ITEMS)

                assert error_id 303
                // _ *first_proof_item current_proof_item num_proof_items *proof proof_len


                /* Verify that entire proof lives in first memory page */
                dup 1
                pop_count
                pop 1
                // _ *first_proof_item current_proof_item num_proof_items *proof proof_len

                dup 0
                pop_count
                pop 1
                // _ *first_proof_item current_proof_item num_proof_items *proof proof_len

                dup 1
                dup 1
                add
                pop_count
                pop 1


                /* Write proof information to memory */
                pick 4
                // _ current_proof_item num_proof_items *proof proof_len *first_proof_item

                call {dyn_malloc}
                // _ current_proof_item num_proof_items *proof proof_len *first_proof_item *vm_proof_iter

                write_mem 5
                // _ (*vm_proof_iter + 5)

                addi -5
                // _ *vm_proof_iter

                return
        )
    }
}

#[cfg(test)]
mod tests {
    use arbitrary::Arbitrary;
    use arbitrary::Unstructured;
    use triton_vm::proof_item::ProofItem;
    use triton_vm::proof_stream::ProofStream;

    use super::*;
    use crate::rust_shadowing_helper_functions;
    use crate::test_prelude::*;
    use crate::verifier::vm_proof_iter::shared::vm_proof_iter_struct::VmProofIter;

    #[test]
    fn vm_proof_iter_new_pbt() {
        ShadowedFunction::new(New).test()
    }

    impl New {
        fn init_state(
            &self,
            proof_items: Vec<ProofItem>,
            proof_pointer: BFieldElement,
        ) -> FunctionInitialState {
            let mut proof_stream = ProofStream::default();
            for proof_item in proof_items {
                proof_stream.enqueue(proof_item);
            }

            let proof: Proof = proof_stream.into();
            let mut memory = HashMap::default();
            encode_to_memory(&mut memory, proof_pointer, &proof);

            FunctionInitialState {
                stack: [self.init_stack_for_isolated_run(), vec![proof_pointer]].concat(),
                memory,
            }
        }
    }

    impl Function for New {
        fn rust_shadow(
            &self,
            stack: &mut Vec<BFieldElement>,
            memory: &mut HashMap<BFieldElement, BFieldElement>,
        ) {
            let pointer_to_proof = stack.pop().unwrap();
            let proof = *Proof::decode_from_memory(memory, pointer_to_proof).unwrap();
            let pointer_to_vm_proof_iter =
                rust_shadowing_helper_functions::dyn_malloc::dynamic_allocator(memory);
            let vm_proof_iter = VmProofIter::new(pointer_to_proof, &proof);
            encode_to_memory(memory, pointer_to_vm_proof_iter, &vm_proof_iter);
            stack.push(pointer_to_vm_proof_iter);
        }

        fn pseudorandom_initial_state(
            &self,
            seed: [u8; 32],
            _bench_case: Option<BenchmarkCase>,
        ) -> FunctionInitialState {
            let mut rng = StdRng::from_seed(seed);
            let proof_pointer = bfe!(rng.random_range(0..(1 << 20)));

            // put randomness on heap because stack might be too small
            let mut randomness = vec![0; 1_000_000];
            rng.fill_bytes(&mut randomness);
            let mut unstructured = Unstructured::new(&randomness);
            let proof_items = (0..rng.random_range(10..25))
                .map(|_| ProofItem::arbitrary(&mut unstructured).unwrap())
                .collect();

            self.init_state(proof_items, proof_pointer)
        }
    }
}
