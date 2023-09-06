use std::collections::HashMap;

use itertools::Itertools;
use rand::{rngs::StdRng, Rng, RngCore, SeedableRng};
use triton_vm::{triton_asm, BFieldElement, NonDeterminism};
use twenty_first::{
    shared_math::tip5::{Tip5State, RATE, STATE_SIZE},
    util_types::algebraic_hasher::{Domain, SpongeHasher},
};

use crate::{
    get_init_tvm_stack,
    procedure::Procedure,
    snippet::{BasicSnippet, DataType},
    snippet_bencher::BenchmarkCase,
    VmHasher, VmHasherState,
};

/// Absorb a sequence of field elements stored in memory, into the sponge state.
struct Absorb {}

impl BasicSnippet for Absorb {
    fn inputs(&self) -> Vec<(crate::snippet::DataType, String)> {
        vec![
            (DataType::VoidPointer, "*sequence".to_string()),
            (DataType::U32, "len".to_string()),
        ]
    }

    fn outputs(&self) -> Vec<(crate::snippet::DataType, String)> {
        vec![]
    }

    fn entrypoint(&self) -> String {
        "tasm_hashing_absorb".to_string()
    }

    fn code(
        &self,
        _library: &mut crate::library::Library,
    ) -> Vec<triton_vm::instruction::LabelledInstruction> {
        let entrypoint = self.entrypoint();
        let main_loop = format!("{entrypoint}_loop");
        let pad_varnum_zeros = format!("{entrypoint}_pad_varnum_zeros");
        let read_remaining_elements = format!("{entrypoint}_read_remaining_elements");
        triton_asm! {
            // BEFORE: _ *addr length
            // AFTER: _
            {entrypoint}:
                // absorb_init to get a deterministic sponge state
                push 0 push 0 push 0 push 0 push 0 push 0 push 0 push 0 push 0 push 0
                absorb_init
                pop pop pop pop pop pop pop pop pop pop

                // absorb all chunks of 10 elements
                call {main_loop}
                // _ *new_addr length_remaining

                // pad
                dup 0   // _ *addr length length
                push -9 // _ *addr length length -9
                add     // _ *addr length length-9
                push -1 // _ *addr length length-9 -1
                mul     // _ *addr length 9-length
                dup 2   // _ *addr length 9-length *addr
                dup 2   // _ *addr length 9-length *addr length

                call {pad_varnum_zeros}
                // _ addr length 0^(9-length) 0 *addr length
                swap 2      // _ *addr length 0^(9-length) length *addr 0
                push 1 add  // _ *addr length 0^(9-length) length *addr 1
                swap 2      // _ *addr length 0^(9-length) 1 *addr length

                // read remaining elements from memory
                call {read_remaining_elements}
                // _ *addr length 0^(9-length) 1 re^length *addr 0

                pop pop // _ *addr length 0^(9-length) 1 re^length

                // absorb
                absorb

                // _ *addr length 0^(9-length) 1 re^length

                pop pop pop
                pop pop pop
                pop pop pop
                pop pop pop

                return

            // BEFORE: _ *addr length 9-length *addr length
            // INVARIANT: _ *addr length 0^(9-len) len *addr length
            // AFTER: _ addr length 0^(9-length) 0 *addr length
            {pad_varnum_zeros}:

                // evaluate return condition
                dup 2       // _ *addr length 0^(9-len) len *addr length len
                push 0 eq   // _ *addr length 0^(9-len) len *addr length len==0
                skiz return

                // _ *addr length 0^(9-len) len *addr length
                push 0  // _ *addr length 0^(9-len) len *addr length 0
                swap 3  // _ *addr length 0^(9-len) 0 *addr length len
                push -1 // _ *addr length 0^(9-len) 0 *addr length len -1
                add     // _ *addr length 0^(9-len) 0 *addr length len-1
                swap 2  // _ *addr length 0^(9-len) 0 len-1 length *addr
                swap 1  // _ *addr length 0^(9-len) 0 len-1 *addr length
                // _ *addr length 0^(9-len) len-1 *addr length

                recurse

            // BEFORE: _ *addr length 0^(9-length) 1 *addr length
            // AFTER: _ *addr length 0^(9-length) 1 re^length *addr 0
            {read_remaining_elements}:
                // evaluate return condition
                dup 0       // _ *addr length 0^(9-length) 1 re^* *addr length length
                push 0 eq   // _ *addr length 0^(9-length) 1 re^* *addr length length==0

                skiz return

                // _ *addr length 0^(9-length) 1 re^* *addr length

                dup 1       // _ *addr length 0^(9-length) 1 re^* *addr length *addr
                dup 1       // _ *addr length 0^(9-length) 1 re^* *addr length *addr length
                add         // _ *addr length 0^(9-length) 1 re^* *addr length *addr+length
                push -1 add // _ *addr length 0^(9-length) 1 re^* *addr length *addr+length-1
                read_mem    // _ *addr length 0^(9-length) 1 re^* *addr length *addr+length-1 re
                swap 3
                swap 2
                swap 1
                            // _ *addr length 0^(9-length) 1 re^* re *addr length *addr+length-1
                pop         // _ *addr length 0^(9-length) 1 re^* re *addr length
                push -1 add // _ *addr length 0^(9-length) 1 re^* re *addr length-1
                // _ *addr length 0^(9-length) 1 re^* *addr length-1

                recurse

            // BEFORE: _ *addr length
            // AFTER: _ *addr length
            {main_loop}:
                // termination condition: 10 or more elements remaining?
                push 10 // _ *addr length 10
                dup 1 // _ *addr length 10 length
                lt // _ *addr length (length < 10)

                // check condition
                skiz
                    return
                // _ *addr length

                // body
                // read 10 elements to stack
                swap 1   // _ length *addr
                dup 0    // _ length *addr *addr
                push 9   // _ length *addr *addr 9
                add      // _ length *addr (*addr+9)
                read_mem // _ length *addr (*addr+9) element_9
                swap 1   // _ length *addr element_9 (*addr+9)
                push -1  // _ length *addr element_9 (*addr+9) -1
                add      // _ length *addr element_9 (*addr+8)
                read_mem // _ length *addr element_9 (*addr+8) element_8
                swap 1   // _ length *addr element_9 element_8 (*addr+8)
                push -1  // _ length *addr element_9 element_8 (*addr+8) -1
                add      // _ length *addr element_9 element_8 (*addr+7)
                read_mem // _ length *addr element_9 element_8 (*addr+7) element_7
                swap 1   // _ length *addr element_9 element_8 element_7 (*addr+7)
                push -1  // _ length *addr element_9 element_8 element_7 (*addr+7) -1
                add      // _ length *addr element_9 element_8 element_7 (*addr+6)
                read_mem // _ length *addr element_9 element_8 element_7 (*addr+6) element_6
                swap 1   // _ length *addr element_9 element_8 element_7 element_6 (*addr+6)
                push -1  // _ length *addr element_9 element_8 element_7 element_6 (*addr+6) -1
                add      // _ length *addr element_9 element_8 element_7 element_6 (*addr+5)
                read_mem // _ length *addr element_9 element_8 element_7 element_6 (*addr+5) element_5
                swap 1   // _ length *addr element_9 element_8 element_7 element_6 element_5 (*addr+5)
                push -1  // _ length *addr element_9 element_8 element_7 element_6 element_5 (*addr+5) -1
                add      // _ length *addr element_9 element_8 element_7 element_6 element_5 (*addr+4)
                read_mem // _ length *addr element_9 element_8 element_7 element_6 element_5 (*addr+4) element_4
                swap 1   // _ length *addr element_9 element_8 element_7 element_6 element_5 element_4 (*addr+4)
                push -1  // _ length *addr element_9 element_8 element_7 element_6 element_5 element_4 (*addr+4) -1
                add      // _ length *addr element_9 element_8 element_7 element_6 element_5 element_4 (*addr+3)
                read_mem // _ length *addr element_9 element_8 element_7 element_6 element_5 element_4 (*addr+3) element_3
                swap 1   // _ length *addr element_9 element_8 element_7 element_6 element_5 element_4 element_3 (*addr+3)
                push -1  // _ length *addr element_9 element_8 element_7 element_6 element_5 element_4 element_3 (*addr+3) -1
                add      // _ length *addr element_9 element_8 element_7 element_6 element_5 element_4 element_3 (*addr+2)
                read_mem // _ length *addr element_9 element_8 element_7 element_6 element_5 element_4 element_3 (*addr+2) element_2
                swap 1   // _ length *addr element_9 element_8 element_7 element_6 element_5 element_4 element_3 element_2 (*addr+2)
                push -1  // _ length *addr element_9 element_8 element_7 element_6 element_5 element_4 element_3 element_2 (*addr+2) -1
                add      // _ length *addr element_9 element_8 element_7 element_6 element_5 element_4 element_3 element_2 (*addr+1)
                read_mem // _ length *addr element_9 element_8 element_7 element_6 element_5 element_4 element_3 element_2 (*addr+1) element_1
                swap 1   // _ length *addr element_9 element_8 element_7 element_6 element_5 element_4 element_3 element_2 element_1 (*addr+1)
                push -1  // _ length *addr element_9 element_8 element_7 element_6 element_5 element_4 element_3 element_2 element_1 (*addr+1) -1
                add      // _ length *addr element_9 element_8 element_7 element_6 element_5 element_4 element_3 element_2 element_1 *addr
                read_mem // _ length *addr element_9 element_8 element_7 element_6 element_5 element_4 element_3 element_2 element_1 *addr element_0
                swap 1   // _ length *addr element_9 element_8 element_7 element_6 element_5 element_4 element_3 element_2 element_1 element_0 *addr
                pop      // _ length *addr element_9 element_8 element_7 element_6 element_5 element_4 element_3 element_2 element_1 element_0

                absorb

                // _ length *addr element_9 element_8 element_7 element_6 element_5 element_4 element_3 element_2 element_1 element_0

                pop pop pop pop pop pop pop pop pop pop
                // _ length *addr

                push 10  // _ length *addr 10
                add      // _ length (*addr+10)
                swap 1   // _ (*addr+10) length
                push -10 // _ (*addr+10) length -10
                add      // _ (*addr+10) (length-10)

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
        sponge_state: &mut VmHasherState,
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
        *sponge_state = VmHasherState::new(Domain::VariableLength);
        VmHasher::absorb(sponge_state, &[BFieldElement::new(0); RATE]);
        VmHasher::absorb_repeatedly(sponge_state, sequence.iter());

        // output empty
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
        VmHasherState,
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
        let mut stack = get_init_tvm_stack();
        stack.push(address);
        stack.push(BFieldElement::new(length as u64));

        // sample sponge state
        let sponge_state: [BFieldElement; STATE_SIZE] = rng.gen();
        let vm_hasher_state = Tip5State {
            state: sponge_state,
        };

        (
            stack,
            memory,
            NonDeterminism::new(vec![]),
            vec![],
            vm_hasher_state,
        )
    }
}

#[cfg(test)]
mod test {
    use crate::{procedure::ShadowedProcedure, snippet::RustShadow};

    use super::Absorb;

    #[test]
    fn test() {
        ShadowedProcedure::new(Absorb {}).test();
    }
}
