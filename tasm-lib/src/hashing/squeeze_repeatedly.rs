use triton_vm::prelude::*;

use crate::prelude::*;

/// Squeeze the sponge n times, storing all the produced pseudorandom `BFieldElement`s
/// contiguously in memory. It is the caller's responsibility to allocate enough memory.
#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub struct SqueezeRepeatedly;

impl BasicSnippet for SqueezeRepeatedly {
    fn parameters(&self) -> Vec<(DataType, String)> {
        vec![
            (DataType::VoidPointer, "address".to_string()),
            (DataType::U32, "num_squeezes".to_string()),
        ]
    }

    fn return_values(&self) -> Vec<(DataType, String)> {
        vec![
            (DataType::VoidPointer, "address".to_string()),
            (DataType::U32, "num_squeezes".to_string()),
        ]
    }

    fn entrypoint(&self) -> String {
        "tasmlib_hashing_squeeze_repeatedly".to_string()
    }

    fn code(&self, _: &mut Library) -> Vec<LabelledInstruction> {
        triton_asm! {
            // BEFORE: _ address num_squeezes
            // AFTER:  _ address' 0
            {self.entrypoint()}:

                // test termination condition
                dup 0
                push 0 eq       // _ address num_squeezes num_squeezes==0
                skiz return

                addi -1

                sponge_squeeze  // _ address (num_squeezes-1) r9 r8 r7 r6 r5 r4 r3 r2 r1 r0

                pick 11         // _ (num_squeezes-1) r9 r8 r7 r6 r5 r4 r3 r2 r1 r0 address
                write_mem 5
                write_mem 5
                                // _ (num_squeezes-1) (address + 10)

                place 1         // _ (address + 10) (num_squeezes-1)

                recurse
        }
    }
}

#[cfg(test)]
mod tests {
    use twenty_first::prelude::Sponge;

    use super::*;
    use crate::empty_stack;
    use crate::test_helpers::rust_final_state;
    use crate::test_helpers::tasm_final_state;
    use crate::test_helpers::verify_memory_equivalence;
    use crate::test_helpers::verify_sponge_equivalence;
    use crate::test_helpers::verify_stack_equivalence;
    use crate::test_helpers::verify_stack_growth;
    use crate::test_prelude::*;

    impl Procedure for SqueezeRepeatedly {
        fn rust_shadow(
            &self,
            stack: &mut Vec<BFieldElement>,
            memory: &mut HashMap<BFieldElement, BFieldElement>,
            _: &NonDeterminism,
            _: &[BFieldElement],
            sponge: &mut Option<Tip5>,
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
            let mut rng = StdRng::from_seed(seed);
            let num_squeezes = match bench_case {
                Some(BenchmarkCase::CommonCase) => 10,
                Some(BenchmarkCase::WorstCase) => 200,
                None => rng.random_range(0..10),
            };

            let sponge = Tip5 {
                state: rng.random(),
            };
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

    #[test]
    fn test() {
        // custom test procedure because it is a procedure but we do want to test memory equivalence

        let shadow = ShadowedProcedure::new(SqueezeRepeatedly);
        let num_states = 15;
        let mut rng = rand::rng();
        let entrypoint = shadow.inner().entrypoint();

        for _ in 0..num_states {
            let seed: [u8; 32] = rng.random();
            println!("testing {entrypoint} common case with seed: {seed:x?}");
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
    use super::*;
    use crate::test_prelude::*;

    #[test]
    fn benchmark() {
        ShadowedProcedure::new(SqueezeRepeatedly).bench();
    }
}
