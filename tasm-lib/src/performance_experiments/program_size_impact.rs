use crate::data_type::DataType;
use crate::library::Library;
use crate::prelude::BasicSnippet;

use crate::triton_vm::prelude::*;

struct ProgramSizeImpact {
    instruction_count: usize,
}

impl BasicSnippet for ProgramSizeImpact {
    fn inputs(&self) -> Vec<(DataType, String)> {
        vec![]
    }

    fn outputs(&self) -> Vec<(DataType, String)> {
        vec![]
    }

    fn entrypoint(&self) -> String {
        format!(
            "tasmlib_performance_experiments_program_size_impact_{}",
            self.instruction_count
        )
    }

    fn code(&self, _library: &mut Library) -> Vec<LabelledInstruction> {
        let entrypoint = self.entrypoint();

        let important_code = triton_asm![nop; self.instruction_count];

        triton_asm!(
            {entrypoint}:
                {&important_code}
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
    fn bench_program_size_impact() {
        ShadowedClosure::new(ProgramSizeImpact {
            instruction_count: 1_000,
        })
        .bench();
        ShadowedClosure::new(ProgramSizeImpact {
            instruction_count: 100_000,
        })
        .bench()
    }

    impl Closure for ProgramSizeImpact {
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
