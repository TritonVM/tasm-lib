use triton_vm::instruction::LabelledInstruction;
use triton_vm::prelude::*;
use triton_vm::twenty_first::math::tip5::DIGEST_LENGTH;

use crate::data_type::DataType;
use crate::hashing::absorb_multiple::AbsorbMultiple;
use crate::library::Library;
use crate::recufier::claim::shared::claim_type;
use crate::traits::basic_snippet::BasicSnippet;

pub struct InstantiateFiatShamirWithClaim;

impl BasicSnippet for InstantiateFiatShamirWithClaim {
    fn inputs(&self) -> Vec<(DataType, String)> {
        let claim_type = claim_type();
        vec![(DataType::StructRef(claim_type), "*claim".to_owned())]
    }

    fn outputs(&self) -> Vec<(DataType, String)> {
        vec![]
    }

    fn entrypoint(&self) -> String {
        "tasmlib_recufier_claim_instantiate_fiat_shamir_with_claim".to_owned()
    }

    fn code(&self, library: &mut Library) -> Vec<LabelledInstruction> {
        let entrypoint = self.entrypoint();

        // We call absorb multiple with the right length. The right length has to be read from
        // the structure. Absorb multiple handles padding.
        let calculate_claim_size = triton_asm!(
            // _ *claim
            // _ *output_size

            read_mem 1
            // _ output_size (*output - 1)

            dup 1
            push 2
            add
            add
            // _ output_size (*output + 1 + output_size)
            // _ output_size *input_size

            read_mem 1
            pop 1
            // _ output_size input_size

            push {DIGEST_LENGTH + 2}
            add
            add
            // _ size


        );

        let absorb_multiple = library.import(Box::new(AbsorbMultiple));
        triton_asm!(
            {entrypoint}:
                // _ *claim

                dup 0
                {&calculate_claim_size}
                // _ *claim claim_size

                call {absorb_multiple}
                // _

                return
        )
    }
}

#[cfg(test)]
mod tests {
    use rand::{rngs::StdRng, thread_rng, Rng, SeedableRng};
    use std::collections::HashMap;
    use triton_vm::twenty_first::math::other::random_elements;
    use triton_vm::twenty_first::util_types::algebraic_hasher::Sponge;

    use crate::memory::encode_to_memory;
    use crate::rust_shadowing_helper_functions::claim::load_claim_from_memory;
    use crate::snippet_bencher::BenchmarkCase;
    use crate::traits::procedure::{Procedure, ProcedureInitialState};

    use super::*;

    impl Procedure for InstantiateFiatShamirWithClaim {
        fn rust_shadow(
            &self,
            stack: &mut Vec<BFieldElement>,
            memory: &mut std::collections::HashMap<BFieldElement, BFieldElement>,
            _nondeterminism: &NonDeterminism,
            _public_input: &[BFieldElement],
            sponge: &mut Option<crate::VmHasher>,
        ) -> Vec<BFieldElement> {
            let claim_pointer = stack.pop().unwrap();

            let claim = load_claim_from_memory(claim_pointer, memory);

            sponge.as_mut().unwrap().pad_and_absorb_all(&claim.encode());

            vec![]
        }

        fn pseudorandom_initial_state(
            &self,
            seed: [u8; 32],
            bench_case: Option<BenchmarkCase>,
        ) -> ProcedureInitialState {
            let mut rng: StdRng = SeedableRng::from_seed(seed);

            let (input_length, output_length) = match bench_case {
                Some(BenchmarkCase::CommonCase) => (0, 0),
                Some(BenchmarkCase::WorstCase) => (100, 100),
                None => (rng.gen_range(0..1000), rng.gen_range(0..1000)),
            };

            let claim = Claim {
                program_digest: rng.gen(),
                input: random_elements(input_length),
                output: random_elements(output_length),
            };

            let mut memory = HashMap::default();

            let claim_pointer = rng.gen();
            encode_to_memory(&mut memory, claim_pointer, claim);

            let sponge: Tip5 = Tip5 { state: rng.gen() };
            ProcedureInitialState {
                stack: [self.init_stack_for_isolated_run(), vec![claim_pointer]].concat(),
                nondeterminism: NonDeterminism::default().with_ram(memory),
                public_input: vec![],
                sponge: Some(sponge),
            }
        }

        fn corner_case_initial_states(&self) -> Vec<ProcedureInitialState> {
            let empty_everything = {
                let minimal_claim = Claim {
                    program_digest: Default::default(),
                    input: vec![],
                    output: vec![],
                };

                let claim_pointer = thread_rng().gen();
                let mut memory = HashMap::default();
                encode_to_memory(&mut memory, claim_pointer, minimal_claim);
                ProcedureInitialState {
                    stack: [self.init_stack_for_isolated_run(), vec![claim_pointer]].concat(),
                    nondeterminism: NonDeterminism::default().with_ram(memory),
                    public_input: vec![],
                    sponge: Some(Tip5::init()),
                }
            };
            vec![empty_everything]
        }
    }
}

#[cfg(test)]
mod test {
    use crate::traits::procedure::ShadowedProcedure;
    use crate::traits::rust_shadow::RustShadow;

    use super::*;

    #[test]
    fn test() {
        ShadowedProcedure::new(InstantiateFiatShamirWithClaim).test();
    }
}

#[cfg(test)]
mod benches {
    use crate::traits::procedure::ShadowedProcedure;
    use crate::traits::rust_shadow::RustShadow;

    use super::*;

    #[test]
    fn benchmark() {
        ShadowedProcedure::new(InstantiateFiatShamirWithClaim).bench();
    }
}
