use crate::data_type::DataType;
use crate::library::Library;
use crate::prelude::BasicSnippet;

use crate::triton_vm::prelude::*;

struct LoopNoUnrolling;

impl BasicSnippet for LoopNoUnrolling {
    fn inputs(&self) -> Vec<(DataType, String)> {
        vec![]
    }

    fn outputs(&self) -> Vec<(DataType, String)> {
        vec![(DataType::Bfe, "value".to_owned())]
    }

    fn entrypoint(&self) -> String {
        "tasmlib_performance_experiments_loop_no_unrolling".to_owned()
    }

    fn code(&self, _library: &mut Library) -> Vec<LabelledInstruction> {
        let entrypoint = self.entrypoint();

        let loop_label = format!("{entrypoint}_loop_____0");
        let loop_body = triton_asm!(
                // _ i' remaining_iterations'

                push 1000
                dup 2
                // _ i remaining_iterations 1000 i

                push 1
                add
                // _ i remaining_iterations 1000 (i + 1)
                // _ i remaining_iterations 1000 i'   <-- rename

                swap 3
                pop 1
                // _ i' remaining_iterations 1000

                dup 1
                push -1
                add
                // _ i' remaining_iterations 1000 (remaining_iterations - 1)
                // _ i' remaining_iterations 1000 remaining_iterations' <-- rename

                dup 0
                swap 3
                pop 1
                // _ i' remaining_iterations' 1000 remaining_iterations'

                dup 2
                swap 2
                // _ i' remaining_iterations' remaining_iterations' remaining_iterations' 1000

                pop 1
                pop 1
                // _ i' remaining_iterations' remaining_iterations'
        );

        let loop_code = triton_asm!(
            // INVARIANT: _ i remaining_iterations
            {loop_label}:
                {&loop_body}
                // _ (i - 1) (remaining_iterations - 1) (remaining_iterations - 1)
                // _ i' remaining_iterations' remaining_iterations'   <-- rename

                /* End-condition: remaining_iterations' == 0 */
                skiz
                    recurse
                // _ i' remaining_iterations'

                pop 1
                // _ i'

                return
        );

        triton_asm!(
            {entrypoint}:
                push 0
                push 10
                // _ 0 10
                // _ i remaining_iterations <-- rename

                call {loop_label}
                // _ 10

                return
                {&loop_code}
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
    fn bench_loop_no_unrolling() {
        ShadowedClosure::new(LoopNoUnrolling).bench()
    }

    impl Closure for LoopNoUnrolling {
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
