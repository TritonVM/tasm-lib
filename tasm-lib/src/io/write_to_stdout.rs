use std::collections::HashMap;

use rand::{rngs::StdRng, SeedableRng};
use triton_vm::{triton_asm, NonDeterminism};
use twenty_first::{
    shared_math::b_field_element::BFieldElement, util_types::algebraic_hasher::Domain,
};

use crate::{
    get_init_tvm_stack,
    procedure::Procedure,
    snippet::{BasicSnippet, DataType},
    VmHasherState,
};

pub struct WriteToStdout(pub DataType);

impl BasicSnippet for WriteToStdout {
    fn inputs(&self) -> Vec<(DataType, String)> {
        vec![(self.0.clone(), "value".to_string())]
    }

    fn outputs(&self) -> Vec<(DataType, String)> {
        vec![]
    }

    fn entrypoint(&self) -> String {
        format!("tasm_io_write_to_stdout___{}", self.0.label_friendly_name())
    }

    fn code(
        &self,
        _library: &mut crate::library::Library,
    ) -> Vec<triton_vm::instruction::LabelledInstruction> {
        let write_whole_datatype = "write_io\n".repeat(self.0.get_size());
        triton_asm!(
            {self.entrypoint()}:
                {write_whole_datatype}
                return
        )
    }
}

impl Procedure for WriteToStdout {
    fn rust_shadow(
        &self,
        stack: &mut Vec<BFieldElement>,
        _memory: &mut HashMap<BFieldElement, BFieldElement>,
        _nondeterminism: &NonDeterminism<BFieldElement>,
        _public_input: &[BFieldElement],
        _sponge_state: &mut VmHasherState,
    ) -> Vec<BFieldElement> {
        let mut ret = vec![];
        for _ in 0..self.0.get_size() {
            let value = stack.pop().unwrap();
            ret.push(value);
        }
        ret
    }

    fn pseudorandom_initial_state(
        &self,
        seed: [u8; 32],
        _bench_case: Option<crate::snippet_bencher::BenchmarkCase>,
    ) -> (
        Vec<BFieldElement>,
        HashMap<BFieldElement, BFieldElement>,
        NonDeterminism<BFieldElement>,
        Vec<BFieldElement>,
        VmHasherState,
    ) {
        let mut rng: StdRng = SeedableRng::from_seed(seed);
        let mut stack = get_init_tvm_stack();
        let random_value = self.0.seeded_random_elements(1, &mut rng);
        for elem in random_value[0].clone().into_iter().rev() {
            stack.push(elem);
        }

        (
            stack,
            HashMap::default(),
            NonDeterminism::new(vec![]),
            vec![],
            VmHasherState::new(Domain::VariableLength),
        )
    }
}

#[cfg(test)]
mod tests {
    use crate::{procedure::ShadowedProcedure, snippet::RustShadow};

    use super::*;

    #[test]
    fn write_to_stdout_auto_test() {
        for data_type in DataType::big_random_generatable_type_collection() {
            ShadowedProcedure::new(WriteToStdout(data_type.clone())).test();
        }
    }
}
