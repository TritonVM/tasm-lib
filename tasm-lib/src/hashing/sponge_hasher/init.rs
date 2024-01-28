use triton_vm::instruction::LabelledInstruction;
use triton_vm::prelude::*;

use crate::data_type::DataType;
use crate::library::Library;
use crate::traits::basic_snippet::BasicSnippet;

pub struct Init;

impl BasicSnippet for Init {
    fn inputs(&self) -> Vec<(DataType, String)> {
        vec![]
    }

    fn outputs(&self) -> Vec<(DataType, String)> {
        vec![]
    }

    fn entrypoint(&self) -> String {
        "tasm_hashing_sponge_hasher_init".to_string()
    }

    fn code(&self, _library: &mut Library) -> Vec<LabelledInstruction> {
        let entrypoint = self.entrypoint();
        triton_asm!(
            {entrypoint}:
                sponge_init
                return
        )
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::empty_stack;
    use crate::snippet_bencher::BenchmarkCase;
    use crate::traits::procedure::*;
    use crate::traits::rust_shadow::RustShadow;
    use crate::VmHasherState;
    use arbitrary::*;
    use rand::rngs::StdRng;
    use rand::*;
    use std::collections::HashMap;
    use triton_vm::twenty_first::shared_math::tip5::Tip5State;
    use triton_vm::twenty_first::util_types::algebraic_hasher::SpongeHasher;

    impl Procedure for Init {
        fn rust_shadow(
            &self,
            _stack: &mut Vec<BFieldElement>,
            _memory: &mut HashMap<BFieldElement, BFieldElement>,
            _nondeterminism: &NonDeterminism<BFieldElement>,
            _public_input: &[BFieldElement],
            sponge_state: &mut Option<VmHasherState>,
        ) -> Vec<BFieldElement> {
            *sponge_state = Some(Tip5::init());
            Vec::default()
        }

        fn pseudorandom_initial_state(
            &self,
            seed: [u8; 32],
            _bench_case: Option<BenchmarkCase>,
        ) -> ProcedureInitialState {
            let mut rng: StdRng = SeedableRng::from_seed(seed);
            let mut bytes = [0u8; 400];
            rng.fill_bytes(&mut bytes);
            let mut unstructured = Unstructured::new(&bytes);
            ProcedureInitialState {
                stack: empty_stack(),
                nondeterminism: NonDeterminism::default(),
                public_input: Vec::default(),
                sponge_state: Some(Tip5State::arbitrary(&mut unstructured).unwrap()),
            }
        }
    }

    #[test]
    fn sponge_init_test() {
        ShadowedProcedure::new(Init).test();
    }
}

#[cfg(test)]
mod benches {
    use super::*;
    use crate::traits::procedure::ShadowedProcedure;
    use crate::traits::rust_shadow::RustShadow;

    #[test]
    fn bench() {
        ShadowedProcedure::new(Init).bench();
    }
}
