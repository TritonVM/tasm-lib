use std::collections::HashMap;

use rand::prelude::*;
use triton_vm::prelude::*;

use crate::empty_stack;
use crate::prelude::*;
use crate::traits::procedure::Procedure;
use crate::traits::procedure::ProcedureInitialState;

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct WriteToStdout {
    pub data_type: DataType,
}

impl BasicSnippet for WriteToStdout {
    fn parameters(&self) -> Vec<(DataType, String)> {
        vec![(self.data_type.clone(), "value".to_string())]
    }

    fn return_values(&self) -> Vec<(DataType, String)> {
        vec![]
    }

    fn entrypoint(&self) -> String {
        format!(
            "tasmlib_io_write_to_stdout___{}",
            self.data_type.label_friendly_name()
        )
    }

    fn code(&self, _: &mut Library) -> Vec<LabelledInstruction> {
        triton_asm!(
            {self.entrypoint()}:
                {&self.data_type.write_value_to_stdout()}
                return
        )
    }
}

impl Procedure for WriteToStdout {
    fn rust_shadow(
        &self,
        stack: &mut Vec<BFieldElement>,
        _: &mut HashMap<BFieldElement, BFieldElement>,
        _: &NonDeterminism,
        _: &[BFieldElement],
        _: &mut Option<Tip5>,
    ) -> Vec<BFieldElement> {
        let mut ret = vec![];
        for _ in 0..self.data_type.stack_size() {
            let value = stack.pop().unwrap();
            ret.push(value);
        }
        ret
    }

    fn pseudorandom_initial_state(
        &self,
        seed: [u8; 32],
        _bench_case: Option<crate::snippet_bencher::BenchmarkCase>,
    ) -> ProcedureInitialState {
        let mut rng = StdRng::from_seed(seed);
        let mut stack = empty_stack();
        let random_value = self.data_type.seeded_random_element(&mut rng);
        stack.extend(random_value.into_iter().rev());

        ProcedureInitialState {
            stack,
            ..Default::default()
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_prelude::*;

    #[test]
    fn rust_shadow() {
        for data_type in DataType::big_random_generatable_type_collection() {
            ShadowedProcedure::new(WriteToStdout { data_type }).test();
        }
    }
}

#[cfg(test)]
mod benches {
    use super::*;
    use crate::test_prelude::*;

    #[test]
    fn benchmark_digest_writing() {
        ShadowedProcedure::new(WriteToStdout {
            data_type: DataType::Digest,
        })
        .bench();
    }
}
