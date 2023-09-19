use std::collections::HashMap;

use itertools::Itertools;
use rand::{rngs::StdRng, Rng, RngCore, SeedableRng};
use triton_vm::{triton_asm, BFieldElement, NonDeterminism};
use twenty_first::{shared_math::tip5::RATE, util_types::algebraic_hasher::SpongeHasher};

use crate::{
    get_init_tvm_stack,
    procedure::Procedure,
    snippet::{BasicSnippet, DataType},
    VmHasher, VmHasherState,
};

/// Squeeze the sponge n times, storing all the produced pseudorandom `BFieldElement`s
/// contiguously in memory. It is the caller's responsibility to allocate enough memory.
struct SqueezeRepeatedly;

impl BasicSnippet for SqueezeRepeatedly {
    fn inputs(&self) -> Vec<(crate::snippet::DataType, String)> {
        vec![
            (DataType::VoidPointer, "address".to_string()),
            (DataType::U32, "num_squeezes".to_string()),
        ]
    }

    fn outputs(&self) -> Vec<(crate::snippet::DataType, String)> {
        vec![
            (DataType::VoidPointer, "address".to_string()),
            (DataType::U32, "num_squeezes".to_string()),
        ]
    }

    fn entrypoint(&self) -> String {
        "tasm_hashing_squeeze_repeatedly".to_string()
    }

    fn code(
        &self,
        _library: &mut crate::library::Library,
    ) -> Vec<triton_vm::instruction::LabelledInstruction> {
        let entrypoint = self.entrypoint();
        triton_asm! {
            // BEFORE: _ address num_squeezes
            // AFTER: _ address' 0
            {entrypoint}:

                // test termination condition
                dup 0
                push 0 eq   // _ address num_squeezes num_squeezes==0
                skiz return

                // decrement squeeze number
                push -1 add

                // add padding
                push 0 push 0 push 0 push 0 push 0
                push 0 push 0 push 0 push 0 push 0

                // squeeze
                squeeze     // _ address num_squeezes-1 r9 r8 r7 r6 r5 r4 r3 r2 r1 r0

                // store to memory
                dup 11      // _ address num_squeezes-1 r9 r8 r7 r6 r5 r4 r3 r2 r1 r0 address

                swap 1      // _ address num_squeezes-1 r9 r8 r7 r6 r5 r4 r3 r2 r1 address r0
                write_mem   // _ address num_squeezes-1 r9 r8 r7 r6 r5 r4 r3 r2 r1 address
                push 1 add  // _ address num_squeezes-1 r9 r8 r7 r6 r5 r4 r3 r2 r1 address+1

                swap 1      // _ address num_squeezes-1 r9 r8 r7 r6 r5 r4 r3 r2 address+1 r1
                write_mem   // _ address num_squeezes-1 r9 r8 r7 r6 r5 r4 r3 r2 address+1
                push 1 add  // _ address num_squeezes-1 r9 r8 r7 r6 r5 r4 r3 r2 address+2

                swap 1      // _ address num_squeezes-1 r9 r8 r7 r6 r5 r4 r3 address+2 r2
                write_mem   // _ address num_squeezes-1 r9 r8 r7 r6 r5 r4 r3 address+2
                push 1 add  // _ address num_squeezes-1 r9 r8 r7 r6 r5 r4 r3 address+3

                swap 1      // _ address num_squeezes-1 r9 r8 r7 r6 r5 r4 address+3 r3
                write_mem   // _ address num_squeezes-1 r9 r8 r7 r6 r5 r4 address+3
                push 1 add  // _ address num_squeezes-1 r9 r8 r7 r6 r5 r4 address+4

                swap 1      // _ address num_squeezes-1 r9 r8 r7 r6 r5 address+4 r4
                write_mem   // _ address num_squeezes-1 r9 r8 r7 r6 r5 address+4
                push 1 add  // _ address num_squeezes-1 r9 r8 r7 r6 r5 address+5

                swap 1      // _ address num_squeezes-1 r9 r8 r7 r6 address+5 r5
                write_mem   // _ address num_squeezes-1 r9 r8 r7 r6 address+5
                push 1 add  // _ address num_squeezes-1 r9 r8 r7 r6 address+6

                swap 1      // _ address num_squeezes-1 r9 r8 r7 address+6 r6
                write_mem   // _ address num_squeezes-1 r9 r8 r7 address+6
                push 1 add  // _ address num_squeezes-1 r9 r8 r7 address+7

                swap 1      // _ address num_squeezes-1 r9 r8 address+7 r7
                write_mem   // _ address num_squeezes-1 r9 r8 address+7
                push 1 add  // _ address num_squeezes-1 r9 r8 address+8

                swap 1      // _ address num_squeezes-1 r9 address+8 r8
                write_mem   // _ address num_squeezes-1 r9 address+8
                push 1 add  // _ address num_squeezes-1 r9 address+9

                swap 1      // _ address num_squeezes-1 address+9 r9
                write_mem   // _ address num_squeezes-1 address+9
                push 1 add  // _ address num_squeezes-1 address+10

                swap 2 pop  // _ address+10 num_squeezes-1

                recurse
        }
    }
}

impl Procedure for SqueezeRepeatedly {
    fn rust_shadow(
        &self,
        stack: &mut Vec<triton_vm::BFieldElement>,
        memory: &mut std::collections::HashMap<triton_vm::BFieldElement, triton_vm::BFieldElement>,
        _nondeterminism: &triton_vm::NonDeterminism<triton_vm::BFieldElement>,
        _public_input: &[triton_vm::BFieldElement],
        sponge_state: &mut crate::VmHasherState,
    ) -> Vec<triton_vm::BFieldElement> {
        let num_squeezes = stack.pop().unwrap().value() as usize;
        let address = stack.pop().unwrap();

        let sequence = (0..num_squeezes)
            .flat_map(|_| VmHasher::squeeze(sponge_state).to_vec())
            .collect_vec();

        for (i, s) in sequence.into_iter().enumerate() {
            memory.insert(address + BFieldElement::new(i as u64), s);
        }

        let new_address = address + BFieldElement::new(RATE as u64 * num_squeezes as u64);
        stack.push(new_address);
        stack.push(BFieldElement::new(0));

        vec![]
    }

    fn pseudorandom_initial_state(
        &self,
        seed: [u8; 32],
        _bench_case: Option<crate::snippet_bencher::BenchmarkCase>,
    ) -> (
        Vec<triton_vm::BFieldElement>,
        std::collections::HashMap<triton_vm::BFieldElement, triton_vm::BFieldElement>,
        triton_vm::NonDeterminism<triton_vm::BFieldElement>,
        Vec<triton_vm::BFieldElement>,
        crate::VmHasherState,
    ) {
        let mut rng: StdRng = SeedableRng::from_seed(seed);
        let sponge_state = VmHasherState { state: rng.gen() };
        let mut stack = get_init_tvm_stack();
        let address = BFieldElement::new(rng.next_u64() % (1 << 20));
        let num_squeezes = rng.gen_range(0..10);
        stack.push(address);
        stack.push(BFieldElement::new(num_squeezes as u64));

        (
            stack,
            HashMap::new(),
            NonDeterminism::new(vec![]),
            vec![],
            sponge_state,
        )
    }
}

#[cfg(test)]
mod test {
    use rand::{thread_rng, Rng};

    use crate::{
        procedure::{Procedure, ShadowedProcedure},
        snippet::RustShadow,
        test_helpers::{
            rust_final_state, tasm_final_state, verify_memory_equivalence,
            verify_stack_equivalence, verify_stack_growth,
        },
    };

    use super::SqueezeRepeatedly;

    #[test]
    fn test() {
        // custom test procedure because it is a procedure but we do want to test memory equivalence

        let shadow = ShadowedProcedure::new(SqueezeRepeatedly);
        let num_states = 15;
        let mut rng = thread_rng();
        let procedure = &shadow.inner();
        let entrypoint = procedure.borrow().entrypoint();

        for _ in 0..num_states {
            let seed: [u8; 32] = rng.gen();
            println!("testing {} common case with seed: {:x?}", entrypoint, seed);
            let (stack, memory, nondeterminism, stdin, sponge_state) =
                SqueezeRepeatedly.pseudorandom_initial_state(seed, None);

            let init_stack = stack.to_vec();
            let words_statically_allocated = 0;

            let rust = rust_final_state(
                &shadow,
                &stack,
                &stdin,
                &nondeterminism,
                &memory,
                &sponge_state,
                words_statically_allocated,
            );

            // run tvm
            let tasm = tasm_final_state(
                &shadow,
                &stack,
                &stdin,
                &nondeterminism,
                &memory,
                &sponge_state,
                words_statically_allocated,
            );

            // assert_eq!(tasm.final_sponge_state.state, rust.final_sponge_state.state);
            // can't do this without changing the VM interface, unfortunately ...

            assert_eq!(
                rust.output, tasm.output,
                "Rust shadowing and VM std out must agree"
            );

            verify_stack_equivalence(&rust.final_stack, &tasm.final_stack);
            verify_memory_equivalence(&rust.final_ram, &tasm.final_ram);
            verify_stack_growth(&shadow, &init_stack, &tasm.final_stack);
        }
    }
}
