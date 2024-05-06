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
    use itertools::Itertools;
    use num::One;
    use rand::rngs::StdRng;
    use rand::Rng;
    use rand::SeedableRng;
    use triton_vm::table::cross_table_argument::CrossTableArg;
    use triton_vm::table::cross_table_argument::EvalArg;

    use crate::snippet_bencher::BenchmarkCase;
    use crate::traits::closure::Closure;
    use crate::traits::closure::ShadowedClosure;
    use crate::traits::rust_shadow::RustShadow;

    use super::*;

    #[test]
    fn compute_terminal_from_digest_pbt() {
        ShadowedClosure::new(ComputeTerminalFromDigestInitialIsOne).test();
    }

    impl Closure for ComputeTerminalFromDigestInitialIsOne {
        fn rust_shadow(&self, stack: &mut Vec<BFieldElement>) {
            let digest = Digest::new([
                stack.pop().unwrap(),
                stack.pop().unwrap(),
                stack.pop().unwrap(),
                stack.pop().unwrap(),
                stack.pop().unwrap(),
            ]);

            let challenge = XFieldElement::new([
                stack.pop().unwrap(),
                stack.pop().unwrap(),
                stack.pop().unwrap(),
            ]);

            let result = EvalArg::compute_terminal(&digest.0, XFieldElement::one(), challenge);

            for elem in result.coefficients.into_iter().rev() {
                stack.push(elem);
            }
        }

        fn pseudorandom_initial_state(
            &self,
            seed: [u8; 32],
            _bench_case: Option<BenchmarkCase>,
        ) -> Vec<BFieldElement> {
            let mut rng: StdRng = SeedableRng::from_seed(seed);
            let challenge = rng.gen();
            let digest = rng.gen();
            self.prepare_state(challenge, digest)
        }
    }

    impl ComputeTerminalFromDigestInitialIsOne {
        fn prepare_state(&self, challenge: XFieldElement, digest: Digest) -> Vec<BFieldElement> {
            let challenge = challenge.coefficients.into_iter().rev().collect_vec();
            let digest = digest.values().into_iter().rev().collect_vec();
            [self.init_stack_for_isolated_run(), challenge, digest].concat()
        }
    }
}

#[cfg(test)]
mod benches {
    use crate::traits::closure::ShadowedClosure;
    use crate::traits::rust_shadow::RustShadow;

    use super::*;

    #[test]
    fn compute_terminal_from_digest_bench() {
        ShadowedClosure::new(ComputeTerminalFromDigestInitialIsOne).bench()
    }
}
