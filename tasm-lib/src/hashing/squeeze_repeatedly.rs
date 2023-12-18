use std::collections::HashMap;

use itertools::Itertools;
use rand::{rngs::StdRng, Rng, RngCore, SeedableRng};
use triton_vm::{triton_asm, BFieldElement, NonDeterminism};
use twenty_first::{shared_math::tip5::RATE, util_types::algebraic_hasher::SpongeHasher};

use crate::data_type::DataType;
use crate::snippet_bencher::BenchmarkCase;
use crate::{empty_stack, procedure::Procedure, snippet::BasicSnippet, VmHasher, VmHasherState};

/// Squeeze the sponge n times, storing all the produced pseudorandom `BFieldElement`s
/// contiguously in memory. It is the caller's responsibility to allocate enough memory.
pub struct SqueezeRepeatedly;

impl BasicSnippet for SqueezeRepeatedly {
    fn inputs(&self) -> Vec<(crate::data_type::DataType, String)> {
        vec![
            (DataType::VoidPointer, "address".to_string()),
            (DataType::U32, "num_squeezes".to_string()),
        ]
    }

    fn outputs(&self) -> Vec<(crate::data_type::DataType, String)> {
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
            // AFTER:  _ address' 0
            {entrypoint}:

                // test termination condition
                dup 0
                push 0 eq       // _ address num_squeezes num_squeezes==0
                skiz return

                push -1 add

                sponge_squeeze  // _ address num_squeezes-1 r9 r8 r7 r6 r5 r4 r3 r2 r1 r0

                dup 11          // _ address num_squeezes-1 r9 r8 r7 r6 r5 r4 r3 r2 r1 r0 address
                write_mem 5
                write_mem 5
                swap 2 pop 1    // _ address+10 num_squeezes-1

                recurse
        }
    }
}

impl Procedure for SqueezeRepeatedly {
    fn rust_shadow(
        &self,
        stack: &mut Vec<BFieldElement>,
        memory: &mut HashMap<BFieldElement, BFieldElement>,
        _nondeterminism: &NonDeterminism<BFieldElement>,
        _public_input: &[BFieldElement],
        sponge_state: &mut Option<VmHasherState>,
    ) -> Vec<BFieldElement> {
        let num_squeezes = stack.pop().unwrap().value() as usize;
        let address = stack.pop().unwrap();

        let Some(sponge_state) = sponge_state else {
            panic!("sponge state must be initialized");
        };
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
        bench_case: Option<BenchmarkCase>,
    ) -> (
        Vec<BFieldElement>,
        NonDeterminism<BFieldElement>,
        Vec<BFieldElement>,
        Option<VmHasherState>,
    ) {
        let mut rng: StdRng = SeedableRng::from_seed(seed);
        let num_squeezes = match bench_case {
            Some(BenchmarkCase::CommonCase) => 10,
            Some(BenchmarkCase::WorstCase) => 200,
            None => rng.gen_range(0..10),
        };

        let sponge_state = VmHasherState { state: rng.gen() };
        let mut stack = empty_stack();
        let address = BFieldElement::new(rng.next_u64() % (1 << 20));
        stack.push(address);
        stack.push(BFieldElement::new(num_squeezes as u64));

        (stack, NonDeterminism::default(), vec![], Some(sponge_state))
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
            verify_sponge_equivalence, verify_stack_equivalence, verify_stack_growth,
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
            let (stack, nondeterminism, stdin, sponge_state) =
                SqueezeRepeatedly.pseudorandom_initial_state(seed, None);

            let init_stack = stack.to_vec();
            let words_statically_allocated = 0;

            let rust = rust_final_state(
                &shadow,
                &stack,
                &stdin,
                &nondeterminism,
                &sponge_state,
                words_statically_allocated,
            );

            // run tvm
            let tasm = tasm_final_state(
                &shadow,
                &stack,
                &stdin,
                nondeterminism,
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
            verify_sponge_equivalence(&rust.final_sponge_state, &tasm.final_sponge_state);
        }
    }
}

#[cfg(test)]
mod benches {
    use super::*;
    use crate::{procedure::ShadowedProcedure, snippet::RustShadow};

    #[test]
    fn squeeze_repeatedly_bench() {
        ShadowedProcedure::new(SqueezeRepeatedly).bench();
    }
}
