use std::collections::HashMap;

use itertools::Itertools;
use rand::rngs::StdRng;
use rand::Rng;
use rand::RngCore;
use rand::SeedableRng;
use triton_vm::prelude::*;
use triton_vm::twenty_first::prelude::Sponge;

use crate::data_type::DataType;
use crate::empty_stack;
use crate::snippet_bencher::BenchmarkCase;
use crate::traits::basic_snippet::BasicSnippet;
use crate::traits::procedure::Procedure;
use crate::traits::procedure::ProcedureInitialState;
use crate::VmHasher;

/// Squeeze the sponge n times, storing all the produced pseudorandom `BFieldElement`s
/// contiguously in memory. It is the caller's responsibility to allocate enough memory.
#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
pub struct SqueezeRepeatedly;

impl BasicSnippet for SqueezeRepeatedly {
    fn inputs(&self) -> Vec<(DataType, String)> {
        vec![
            (DataType::VoidPointer, "address".to_string()),
            (DataType::U32, "num_squeezes".to_string()),
        ]
    }

    fn outputs(&self) -> Vec<(DataType, String)> {
        vec![
            (DataType::VoidPointer, "address".to_string()),
            (DataType::U32, "num_squeezes".to_string()),
        ]
    }

    fn entrypoint(&self) -> String {
        "tasm_hashing_squeeze_repeatedly".to_string()
    }

    fn code(&self, _library: &mut crate::library::Library) -> Vec<LabelledInstruction> {
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

                sponge_squeeze  // _ address (num_squeezes-1) r9 r8 r7 r6 r5 r4 r3 r2 r1 r0

                dup 11          // _ address (num_squeezes-1) r9 r8 r7 r6 r5 r4 r3 r2 r1 r0 address
                write_mem 5
                write_mem 5
                                // _ address (num_squeezes-1) (address + 10)

                swap 2 pop 1    // _ (address + 10) (num_squeezes-1)

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
        sponge: &mut Option<VmHasher>,
    ) -> Vec<BFieldElement> {
        let num_squeezes = stack.pop().unwrap().value() as usize;
        let address = stack.pop().unwrap();

        let sponge = sponge.as_mut().expect("sponge must be initialized");
        let sequence = (0..num_squeezes)
            .flat_map(|_| sponge.squeeze().to_vec())
            .collect_vec();

        for (i, s) in sequence.into_iter().enumerate() {
            memory.insert(address + BFieldElement::new(i as u64), s);
        }

        let new_address = address + BFieldElement::new(tip5::RATE as u64 * num_squeezes as u64);
        stack.push(new_address);
        stack.push(BFieldElement::new(0));

        vec![]
    }

    fn pseudorandom_initial_state(
        &self,
        seed: [u8; 32],
        bench_case: Option<BenchmarkCase>,
    ) -> ProcedureInitialState {
        let mut rng: StdRng = SeedableRng::from_seed(seed);
        let num_squeezes = match bench_case {
            Some(BenchmarkCase::CommonCase) => 10,
            Some(BenchmarkCase::WorstCase) => 200,
            None => rng.gen_range(0..10),
        };

        let sponge = VmHasher { state: rng.gen() };
        let mut stack = empty_stack();
        let address = BFieldElement::new(rng.next_u64() % (1 << 20));
        stack.push(address);
        stack.push(BFieldElement::new(num_squeezes as u64));

        ProcedureInitialState {
            stack,
            nondeterminism: NonDeterminism::default(),
            public_input: vec![],
            sponge: Some(sponge),
        }
    }
}

#[cfg(test)]
mod test {
    use rand::thread_rng;
    use rand::Rng;

    use crate::test_helpers::rust_final_state;
    use crate::test_helpers::tasm_final_state;
    use crate::test_helpers::verify_memory_equivalence;
    use crate::test_helpers::verify_sponge_equivalence;
    use crate::test_helpers::verify_stack_equivalence;
    use crate::test_helpers::verify_stack_growth;
    use crate::traits::procedure::Procedure;
    use crate::traits::procedure::ProcedureInitialState;
    use crate::traits::procedure::ShadowedProcedure;
    use crate::traits::rust_shadow::RustShadow;

    use super::*;

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
            let ProcedureInitialState {
                stack,
                nondeterminism,
                public_input: stdin,
                sponge,
            } = SqueezeRepeatedly.pseudorandom_initial_state(seed, None);

            let init_stack = stack.to_vec();

            let rust = rust_final_state(&shadow, &stack, &stdin, &nondeterminism, &sponge);

            // run tvm
            let tasm = tasm_final_state(&shadow, &stack, &stdin, nondeterminism, &sponge);

            assert_eq!(
                rust.public_output, tasm.public_output,
                "Rust shadowing and VM std out must agree"
            );

            verify_stack_equivalence(
                "Rust-shadow",
                &rust.stack,
                "TVM execution",
                &tasm.op_stack.stack,
            );
            verify_memory_equivalence("Rust-shadow", &rust.ram, "TVM execution", &tasm.ram);
            verify_stack_growth(&shadow, &init_stack, &tasm.op_stack.stack);
            verify_sponge_equivalence(&rust.sponge, &tasm.sponge);
        }
    }
}

#[cfg(test)]
mod benches {
    use crate::traits::procedure::ShadowedProcedure;
    use crate::traits::rust_shadow::RustShadow;

    use super::*;

    #[test]
    fn squeeze_repeatedly_bench() {
        ShadowedProcedure::new(SqueezeRepeatedly).bench();
    }
}
