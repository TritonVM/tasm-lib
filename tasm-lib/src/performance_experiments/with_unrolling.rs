use crate::data_type::DataType;
use crate::library::Library;
use crate::prelude::BasicSnippet;

use crate::triton_vm::prelude::*;

struct WithUnrolling;

impl BasicSnippet for WithUnrolling {
    fn inputs(&self) -> Vec<(DataType, String)> {
        vec![]
    }

    fn outputs(&self) -> Vec<(DataType, String)> {
        vec![(DataType::Bfe, "value".to_string()); 10]
    }

    fn entrypoint(&self) -> String {
        "tasmlib_performance_experiments_with_unrolling".to_string()
    }

    fn code(&self, _library: &mut Library) -> Vec<LabelledInstruction> {
        let entrypoint = self.entrypoint();

        triton_asm!(
            {entrypoint}:
                push 10
                push 20
                push 30
                push 40
                push 50
                push 10
                push 20
                push 30
                push 40
                push 50

                return
        )
    }
}

#[cfg(test)]
mod benches {
    use crate::snippet_bencher::BenchmarkCase;
    use crate::traits::closure::Closure;
    use crate::traits::closure::ShadowedClosure;
    use crate::traits::rust_shadow::RustShadow;

    use super::*;

    #[test]
    fn bench_with_unrolling() {
        ShadowedClosure::new(WithUnrolling).bench()
    }

    impl Closure for WithUnrolling {
        fn rust_shadow(&self, _stack: &mut Vec<BFieldElement>) {
            todo!()
        }

        fn pseudorandom_initial_state(
            &self,
            _seed: [u8; 32],
            _bench_case: Option<BenchmarkCase>,
        ) -> Vec<BFieldElement> {
            self.init_stack_for_isolated_run()
        }
    }
}
