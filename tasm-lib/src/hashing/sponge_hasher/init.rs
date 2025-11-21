use triton_vm::prelude::*;

use crate::prelude::*;

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub struct Init;

impl BasicSnippet for Init {
    fn parameters(&self) -> Vec<(DataType, String)> {
        vec![]
    }

    fn return_values(&self) -> Vec<(DataType, String)> {
        vec![]
    }

    fn entrypoint(&self) -> String {
        "tasmlib_hashing_sponge_hasher_init".to_string()
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
mod tests {
    use arbitrary::Arbitrary;
    use arbitrary::Unstructured;
    use twenty_first::prelude::Sponge;

    use super::*;
    use crate::empty_stack;
    use crate::test_prelude::*;

    impl Procedure for Init {
        fn rust_shadow(
            &self,
            _: &mut Vec<BFieldElement>,
            _: &mut HashMap<BFieldElement, BFieldElement>,
            _: &NonDeterminism,
            _: &[BFieldElement],
            sponge: &mut Option<Tip5>,
        ) -> Vec<BFieldElement> {
            *sponge = Some(Tip5::init());
            vec![]
        }

        fn pseudorandom_initial_state(
            &self,
            seed: [u8; 32],
            _: Option<BenchmarkCase>,
        ) -> ProcedureInitialState {
            let bytes = StdRng::from_seed(seed).random::<[u8; 400]>();
            let mut unstructured = Unstructured::new(&bytes);
            let sponge = Tip5::arbitrary(&mut unstructured).unwrap();

            ProcedureInitialState {
                stack: empty_stack(),
                nondeterminism: NonDeterminism::default(),
                public_input: vec![],
                sponge: Some(sponge),
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
    use crate::test_prelude::*;

    #[test]
    fn benchmark() {
        ShadowedProcedure::new(Init).bench();
    }
}
