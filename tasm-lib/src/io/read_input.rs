use std::collections::HashMap;

use triton_vm::prelude::*;

use super::InputSource;
use crate::empty_stack;
use crate::prelude::*;
use crate::traits::procedure::Procedure;
use crate::traits::procedure::ProcedureInitialState;

/// Move an element of type `DataType` from standard-in or secret-in's token stream to the stack
#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct ReadInput {
    pub data_type: DataType,
    pub input_source: InputSource,
}

impl BasicSnippet for ReadInput {
    fn parameters(&self) -> Vec<(DataType, String)> {
        vec![]
    }

    fn return_values(&self) -> Vec<(DataType, String)> {
        vec![(self.data_type.clone(), "read_value".to_string())]
    }

    fn entrypoint(&self) -> String {
        format!(
            "tasmlib_io_read_{}___{}",
            self.input_source.label_friendly_name(),
            self.data_type.label_friendly_name()
        )
    }

    fn code(&self, _library: &mut crate::library::Library) -> Vec<LabelledInstruction> {
        let entrypoint = self.entrypoint();
        let read_an_element = self.data_type.read_value_from_input(self.input_source);
        triton_asm!(
            {entrypoint}:
                {&read_an_element}
                return
        )
    }
}

impl Procedure for ReadInput {
    fn rust_shadow(
        &self,
        stack: &mut Vec<BFieldElement>,
        _memory: &mut HashMap<BFieldElement, BFieldElement>,
        nondeterminism: &NonDeterminism,
        public_input: &[BFieldElement],
        _sponge: &mut Option<crate::prelude::Tip5>,
    ) -> Vec<BFieldElement> {
        let input_source = match self.input_source {
            InputSource::StdIn => public_input,
            InputSource::SecretIn => &nondeterminism.individual_tokens,
        };
        for elem in input_source.iter().take(self.data_type.stack_size()) {
            stack.push(*elem);
        }

        // Output nothing
        vec![]
    }

    fn pseudorandom_initial_state(
        &self,
        _seed: [u8; 32],
        _bench_case: Option<crate::snippet_bencher::BenchmarkCase>,
    ) -> ProcedureInitialState {
        let input_stream: Vec<BFieldElement> = self.data_type.random_elements(1)[0].encode();

        let (std_in, secret_in) = match self.input_source {
            InputSource::StdIn => (input_stream, vec![]),
            InputSource::SecretIn => (vec![], input_stream),
        };

        ProcedureInitialState {
            stack: empty_stack(),
            nondeterminism: NonDeterminism::new(secret_in),
            public_input: std_in,
            sponge: None,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_prelude::*;

    #[test]
    fn test() {
        for data_type in DataType::big_random_generatable_type_collection() {
            ShadowedProcedure::new(ReadInput {
                data_type: data_type.clone(),
                input_source: InputSource::StdIn,
            })
            .test();
            ShadowedProcedure::new(ReadInput {
                data_type,
                input_source: InputSource::SecretIn,
            })
            .test();
        }
    }
}

#[cfg(test)]
mod benches {
    use super::*;
    use crate::test_prelude::*;

    #[test]
    fn benchmark() {
        ShadowedProcedure::new(ReadInput {
            data_type: DataType::Digest,
            input_source: InputSource::StdIn,
        })
        .bench();
    }
}
