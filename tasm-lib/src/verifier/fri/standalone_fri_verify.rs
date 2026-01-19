use std::collections::HashMap;

use triton_vm::prelude::*;

use super::verify::FriSnippet;
use super::verify::FriVerify;
use crate::library::Library;
use crate::memory::FIRST_NON_DETERMINISTICALLY_INITIALIZED_MEMORY_ADDRESS;
use crate::memory::encode_to_memory;
use crate::traits::compiled_program::CompiledProgram;

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
struct StandaloneFriVerify {
    seed: [u8; 32],
}

impl StandaloneFriVerify {
    fn instance() -> FriVerify {
        let offset = BFieldElement::new(7);
        let computation_size = 1 << 21;
        let expansion_factor = 1 << 4;
        let num_collinearity_checks = 40;
        let domain_length = computation_size * expansion_factor;
        FriVerify::new(
            offset,
            domain_length,
            expansion_factor,
            num_collinearity_checks,
        )
    }

    fn singleton() -> StandaloneFriVerify {
        let mut seed = [0u8; 32];
        seed[0] = 0xba;
        seed[1] = 0xda;
        seed[2] = 0x55;
        seed[3] = 0xa5;
        seed[4] = 0xfc;
        Self { seed }
    }

    fn pseudorandom_intermediate_state(&self) -> IntermediateState {
        let fri_verify = StandaloneFriVerify::instance();
        let proof_stream = fri_verify.pseudorandom_fri_proof_stream(self.seed);
        let digests = fri_verify.extract_digests_required_for_proving(&proof_stream);
        let mut memory = HashMap::<BFieldElement, BFieldElement>::new();
        let proof_stream_pointer = FIRST_NON_DETERMINISTICALLY_INITIALIZED_MEMORY_ADDRESS;
        let fri_verify_pointer = encode_to_memory(&mut memory, proof_stream_pointer, &proof_stream);
        encode_to_memory(&mut memory, fri_verify_pointer, &fri_verify);
        let nondeterminism = NonDeterminism::new(vec![])
            .with_digests(digests)
            .with_ram(memory);
        let stack_excess = vec![proof_stream_pointer, fri_verify_pointer];

        IntermediateState {
            fri_verify,
            stack_excess,
            nondeterminism,
        }
    }
}

struct IntermediateState {
    fri_verify: FriVerify,
    stack_excess: Vec<BFieldElement>,
    nondeterminism: NonDeterminism,
}

impl CompiledProgram for StandaloneFriVerify {
    fn rust_shadow(
        _public_input: &PublicInput,
        _nondeterminism: &NonDeterminism,
    ) -> anyhow::Result<Vec<BFieldElement>> {
        todo!()
    }

    fn code() -> (Vec<LabelledInstruction>, Library) {
        let intermediate_state = Self::singleton().pseudorandom_intermediate_state();

        let mut library = Library::new();
        let snippet = FriSnippet {
            test_instance: intermediate_state.fri_verify,
        };
        let fri_verify_entrypoint = library.import(Box::new(snippet));

        let mut invocation_code = vec![];
        invocation_code.push(triton_instr!(sponge_init));
        for sti in intermediate_state.stack_excess {
            invocation_code.push(triton_instr!(push sti.value()));
        }
        invocation_code.extend(triton_asm!(call {
            fri_verify_entrypoint
        }));
        invocation_code.push(triton_instr!(halt));

        (invocation_code, library)
    }
}

#[cfg(test)]
mod bench {
    use super::*;
    use crate::snippet_bencher::BenchmarkCase;
    use crate::traits::compiled_program::bench_and_profile_program;

    #[ignore = "Very slow, about 340s on my powerful laptop"]
    #[test]
    fn benchmark() {
        let public_input = PublicInput::default();
        let intermediate_state = StandaloneFriVerify::singleton().pseudorandom_intermediate_state();

        bench_and_profile_program::<StandaloneFriVerify>(
            "tasmlib_verifier_standalone_fri_verify",
            BenchmarkCase::WorstCase,
            &public_input,
            &intermediate_state.nondeterminism,
        );
    }
}
