use triton_vm::prelude::*;

use crate::data_type::DataType;
use crate::library::Library;
use crate::traits::basic_snippet::BasicSnippet;

pub struct ComputeTerminalFromDigestInitialIsOne;

impl BasicSnippet for ComputeTerminalFromDigestInitialIsOne {
    fn inputs(&self) -> Vec<(DataType, String)> {
        vec![
            (DataType::Xfe, "challenge".to_owned()),
            (DataType::Digest, "digest".to_owned()),
        ]
    }

    fn outputs(&self) -> Vec<(DataType, String)> {
        vec![(DataType::Xfe, "terminal".to_owned())]
    }

    fn entrypoint(&self) -> String {
        "tasmlib_verifier_eval_arg_compute_terminal_from_digest".to_owned()
    }

    fn code(&self, _library: &mut Library) -> Vec<LabelledInstruction> {
        let entrypoint = self.entrypoint();

        triton_asm!(
            {entrypoint}:
                // _ [challenge] d4 d3 d2 d1 d0

                dup 7
                dup 7
                dup 7
                // _ [challenge] d4 d3 d2 d1 d0 c2 c1 c0

                dup 3
                add
                // _ [challenge] d4 d3 d2 d1 d0 [challenge + d0]

                dup 10
                dup 10
                dup 10
                xx_mul
                // _ [challenge] d4 d3 d2 d1 d0 [(challenge + d0) * challenge]

                dup 4
                add
                // _ [challenge] d4 d3 d2 d1 d0 [(challenge + d0) * challenge + d1]

                dup 10
                dup 10
                dup 10
                xx_mul
                // _ [challenge] d4 d3 d2 d1 d0 [((challenge + d0) * challenge + d1) * challenge]

                dup 5
                add
                // _ [challenge] d4 d3 d2 d1 d0 [((challenge + d0) * challenge + d1) * challenge + d2]

                dup 10
                dup 10
                dup 10
                xx_mul
                dup 6
                add
                // _ [challenge] d4 d3 d2 d1 d0 [running_eval_3]

                dup 10
                dup 10
                dup 10
                xx_mul
                dup 7
                add
                // _ [challenge] d4 d3 d2 d1 d0 [terminal]

                swap 8
                pop 1
                swap 8
                pop 1
                swap 8
                pop 1
                // _ [terminal] d4 d3 d2 d1 d0

                pop 5
                // _ [terminal]

                return
        )
    }
}

#[cfg(test)]
mod tests {
    use rand::prelude::*;
    use triton_vm::air::cross_table_argument::CrossTableArg;
    use triton_vm::air::cross_table_argument::EvalArg;

    use super::*;
    use crate::pop_encodable;
    use crate::push_encodable;
    use crate::snippet_bencher::BenchmarkCase;
    use crate::traits::closure::Closure;
    use crate::traits::closure::ShadowedClosure;
    use crate::traits::rust_shadow::RustShadow;

    impl Closure for ComputeTerminalFromDigestInitialIsOne {
        type Args = (XFieldElement, Digest);

        fn rust_shadow(&self, stack: &mut Vec<BFieldElement>) {
            let (challenge, digest) = pop_encodable::<Self::Args>(stack);
            let terminal =
                EvalArg::compute_terminal(&digest.0, EvalArg::default_initial(), challenge);
            push_encodable(stack, &terminal);
        }

        fn pseudorandom_args(&self, seed: [u8; 32], _: Option<BenchmarkCase>) -> Self::Args {
            StdRng::from_seed(seed).gen()
        }
    }

    #[test]
    fn rust_shadow() {
        ShadowedClosure::new(ComputeTerminalFromDigestInitialIsOne).test();
    }
}

#[cfg(test)]
mod benches {
    use super::*;
    use crate::traits::closure::ShadowedClosure;
    use crate::traits::rust_shadow::RustShadow;

    #[test]
    fn benchmark() {
        ShadowedClosure::new(ComputeTerminalFromDigestInitialIsOne).bench()
    }
}
