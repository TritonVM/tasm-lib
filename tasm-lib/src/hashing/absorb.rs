use std::collections::HashMap;

use itertools::Itertools;
use rand::{rngs::StdRng, Rng, RngCore, SeedableRng};
use triton_vm::{triton_asm, BFieldElement, NonDeterminism};
use twenty_first::{
    shared_math::tip5::{Tip5State, STATE_SIZE},
    util_types::algebraic_hasher::SpongeHasher,
};

use crate::{
    data_type::DataType,
    traits::{basic_snippet::BasicSnippet, procedure::Procedure},
};
use crate::{empty_stack, snippet_bencher::BenchmarkCase, VmHasher, VmHasherState};

/// Absorb a sequence of field elements stored in memory, into the sponge state.
pub struct Absorb;

impl BasicSnippet for Absorb {
    fn inputs(&self) -> Vec<(crate::data_type::DataType, String)> {
        vec![
            (DataType::VoidPointer, "*sequence".to_string()),
            (DataType::U32, "len".to_string()),
        ]
    }

    fn outputs(&self) -> Vec<(crate::data_type::DataType, String)> {
        vec![]
    }

    fn entrypoint(&self) -> String {
        "tasm_hashing_absorb".to_string()
    }

    fn code(
        &self,
        library: &mut crate::library::Library,
    ) -> Vec<triton_vm::instruction::LabelledInstruction> {
        let address_pointer = library.kmalloc(1);
        let length_pointer = library.kmalloc(1);
        let pre_first_chunk_pointer_pointer = library.kmalloc(1);
        let num_remaining_absorbs_pointer = library.kmalloc(1);

        let entrypoint = self.entrypoint();
        let pad_varnum_zeros = format!("{entrypoint}_pad_varnum_zeros");
        let read_remainder = format!("{entrypoint}_read_remainder");
        let read_remainder_loop = format!("{read_remainder}_loop");
        let read_full_chunks = format!("{entrypoint}_read_full_chunks");
        let absorb_all_chunks = format!("{entrypoint}_absorb_all_chunks");

        triton_asm! {
            // BEFORE: _ *addr length
            // AFTER:  _
            {entrypoint}:
                dup 0       // *addr length length
                push {length_pointer}
                write_mem 1
                pop 1       // _ *addr length
                swap 1      // _ length *addr

                dup 0
                push -1
                add
                push {pre_first_chunk_pointer_pointer}
                write_mem 1
                pop 1       // _ length *addr

                push {address_pointer}
                write_mem 1
                pop 1       // _ length

                push 10
                swap 1
                div_mod     // _ length/10 length%10
                swap 1      // _ length%10 length/10

                push 1
                add
                push {num_remaining_absorbs_pointer}
                write_mem 1
                pop 1       // _ length%10

                push -1
                mul
                push 9
                add         // _ 9-length%10
                call {pad_varnum_zeros}
                pop 1       // _ [0; num_pad_zeros]
                push 1      // _ [padding]

                call {read_remainder}
                // _ [padding] [remainder] *last_chunk_end

                call {read_full_chunks}
                pop 1       // _ [padding] [remainder] [[chunk]]

                call {absorb_all_chunks}
                pop 1
                return

            // BEFORE:    _ len
            // INVARIANT: _ [0; 9-len'] len'
            // AFTER:     _ [0; 9-len] 0
            {pad_varnum_zeros}:
                dup 0       // _ [0; 9-len] len len
                push 0 eq   // _ [0; 9-len] len len==0
                skiz return
                // _ [0; 9-len] len

                push 0
                swap 1
                push -1
                add
                recurse

            // BEFORE: _
            // AFTER:  _ [elements; remainder_length] *last_chunk_end
            {read_remainder}:
                push {address_pointer}
                read_mem 1
                pop 1       // _ *addr
                push {length_pointer}
                read_mem 1
                pop 1       // _ *addr length

                swap 1      // _ length *addr
                dup 1       // _ length *addr length
                add         // _ length *(addr + length)
                push -1     // _ length *(addr + length) -1
                add         // _ length *last_addr
                swap 1      // _ *last_addr length

                push 10
                swap 1
                div_mod
                swap 1
                pop 1       // _ *last_addr length%10
                swap 1      // _ length%10 *last_addr

                call {read_remainder_loop}
                // _ [elements; remainder_length] 0 *last_chunk_end
                swap 1
                pop 1       // _ [elements; remainder_length] *last_chunk_end
                return

            // BEFORE:    _ remainder_length *addr
            // INVARIANT: _ [elements; num_elements_read] num_elements_to_read *some_addr
            // AFTER:     _ [elements; remainder_length] 0 *last_chunk_end
            {read_remainder_loop}:
                dup 1
                push 0
                eq
                skiz return
                // _ [elements; num_elements_read] num_elements_to_read *addr
                read_mem 1  // _ [elements; num_elements_read] num_elements_to_read element *(addr-1)
                swap 1      // _ [elements; num_elements_read] num_elements_to_read *(addr-1) element
                swap 2      // _ [elements; num_elements_read+1] *(addr-1) num_elements_to_read
                push -1
                add
                swap 1
                recurse     // _ [elements; num_elements_read+1] (num_elements_to_read-1) *(addr-1)


            // BEFORE:    _ *last_chunk_addr
            // INVARIANT: _ [[chunk]] *some_addr
            // AFTER:     _ [[chunk]; length/10] *pre_first_chunk_pointer
            {read_full_chunks}:
                dup 0
                push {pre_first_chunk_pointer_pointer}
                read_mem 1
                pop 1       // _ *some_addr *some_addr *pre_first_chunk_pointer
                eq
                skiz return
                read_mem 5
                read_mem 5
                recurse

            // BEFORE: _ [[chunks]]
            // AFTER:  _ 0
            {absorb_all_chunks}:
                push {num_remaining_absorbs_pointer}
                read_mem 1
                pop 1       // _ [[chunks]] num_remaining_absorbs
                dup 0       // _ [[chunks]] num_remaining_absorbs num_remaining_absorbs
                push 0
                eq          // _ [[chunks]] num_remaining_absorbs (num_remaining_absorbs == 0)
                skiz return
                push -1
                add         // _ [[chunks]] (num_remaining_absorbs-1)
                push {num_remaining_absorbs_pointer}
                write_mem 1
                pop 1

                sponge_absorb
                recurse
        }
    }
}

impl Procedure for Absorb {
    fn rust_shadow(
        &self,
        stack: &mut Vec<BFieldElement>,
        memory: &mut HashMap<BFieldElement, BFieldElement>,
        _nondeterminism: &NonDeterminism<BFieldElement>,
        _public_input: &[BFieldElement],
        sponge_state: &mut Option<VmHasherState>,
    ) -> Vec<BFieldElement> {
        // read arguments
        let length = stack.pop().unwrap().value() as usize;
        let address = stack.pop().unwrap();

        // read sequence from memory
        let mut sequence = vec![];
        for i in 0..length {
            sequence.push(
                memory
                    .get(&(address + BFieldElement::new(i as u64)))
                    .copied()
                    .unwrap(),
            )
        }

        // pad sequence with 1 and then so many zeros such that 10 divides length
        sequence.push(BFieldElement::new(1));
        while sequence.len() % 10 != 0 {
            sequence.push(BFieldElement::new(0));
        }

        // absorb into sponge state
        let Some(sponge_state) = sponge_state else {
            panic!("sponge must be initialized")
        };
        VmHasher::absorb_repeatedly(sponge_state, sequence.iter());

        let length = (length as u64).into();
        memory.extend(Self::statically_allocated_memory(address, length));

        // output empty
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
        let mut rng: StdRng = SeedableRng::from_seed(seed);

        // sample address
        let address = BFieldElement::new(rng.next_u64() % (1 << 20));

        // sample sequence
        let length = if matches!(bench_case, Some(BenchmarkCase::WorstCase)) {
            97
        } else {
            rng.next_u32() % 20
        };
        let sequence = (0..length)
            .map(|_| rng.gen::<BFieldElement>())
            .collect_vec();

        // write to memory
        let mut memory = HashMap::new();
        for (i, s) in sequence.into_iter().enumerate() {
            memory.insert(address + BFieldElement::new(i as u64), s);
        }

        // leave address and length on stack
        let mut stack = empty_stack();
        stack.push(address);
        stack.push(BFieldElement::new(length as u64));

        // sample sponge state
        let sponge_state: [BFieldElement; STATE_SIZE] = rng.gen();
        let vm_hasher_state = Tip5State {
            state: sponge_state,
        };

        let nondeterminism = NonDeterminism::default().with_ram(memory);
        (stack, nondeterminism, vec![], Some(vm_hasher_state))
    }
}

impl Absorb {
    pub(crate) fn statically_allocated_memory(
        address: BFieldElement,
        length: BFieldElement,
    ) -> HashMap<BFieldElement, BFieldElement> {
        let mut memory = HashMap::default();
        memory.insert(-BFieldElement::new(1), address);
        memory.insert(-BFieldElement::new(2), length);
        memory.insert(-BFieldElement::new(3), address - BFieldElement::new(1));
        memory.insert(-BFieldElement::new(4), BFieldElement::new(0));
        memory
    }
}

#[cfg(test)]
mod test {
    use super::Absorb;
    use crate::traits::procedure::ShadowedProcedure;
    use crate::traits::rust_shadow::RustShadow;

    #[test]
    fn test() {
        ShadowedProcedure::new(Absorb).test();
    }
}
