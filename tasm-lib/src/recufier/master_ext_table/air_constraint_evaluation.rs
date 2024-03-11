use crate::data_type::{ArrayType, DataType};
use crate::library::Library;
use crate::traits::basic_snippet::BasicSnippet;
use crate::triton_vm::table::*;
use ndarray::Array1;
use triton_vm::prelude::*;
use triton_vm::table::challenges::Challenges;
use triton_vm::table::extension_table::Evaluable;
use triton_vm::table::master_table::{num_quotients, MasterExtTable};
use triton_vm::table::tasm_air_constraints::air_constraint_evaluation_tasm;

use crate::recufier::challenges::new_empty_input_and_output::NewEmptyInputAndOutput;

const NUM_TOTAL_CONSTRAINTS: usize = num_quotients();

#[derive(Debug, Clone)]
pub struct AirConstraintEvaluation {
    mem_layout: TasmConstraintEvaluationMemoryLayout,
}

impl AirConstraintEvaluation {
    pub fn with_conventional_memory_layout() -> Self {
        Self {
            mem_layout: Self::conventional_air_constraint_memory_layout(),
        }
    }

    pub fn output_type(&self) -> DataType {
        DataType::Array(Box::new(ArrayType {
            element_type: DataType::Xfe,
            length: NUM_TOTAL_CONSTRAINTS,
        }))
    }

    /// The values returned here should match those used by STARK proof
    pub fn conventional_air_constraint_memory_layout() -> TasmConstraintEvaluationMemoryLayout {
        let mem_layout = TasmConstraintEvaluationMemoryLayout {
            free_mem_page_ptr: BFieldElement::new((u32::MAX as u64 - 1) * (1u64 << 32)),
            curr_base_row_ptr: BFieldElement::new(20u64),
            curr_ext_row_ptr: BFieldElement::new(1068),
            next_base_row_ptr: BFieldElement::new(1312),
            next_ext_row_ptr: BFieldElement::new(2360),
            challenges_ptr: NewEmptyInputAndOutput::conventional_challenges_pointer(),
        };
        assert!(mem_layout.is_integral());

        mem_layout
    }

    /// Return the concatenated AIR-constraint evaluation
    pub fn host_machine_air_constraint_evaluation(
        input_values: AirConstraintSnippetInputs,
    ) -> Vec<XFieldElement> {
        let current_base_row = Array1::from(input_values.current_base_row);
        let current_ext_row = Array1::from(input_values.current_ext_row);
        let next_base_row = Array1::from(input_values.next_base_row);
        let next_ext_row = Array1::from(input_values.next_ext_row);
        let evaluated_initial_constraints = MasterExtTable::evaluate_initial_constraints(
            current_base_row.view(),
            current_ext_row.view(),
            &input_values.challenges,
        );
        let evaluated_consistency_constraints = MasterExtTable::evaluate_consistency_constraints(
            current_base_row.view(),
            current_ext_row.view(),
            &input_values.challenges,
        );
        let evaluated_transition_constraints = MasterExtTable::evaluate_transition_constraints(
            current_base_row.view(),
            current_ext_row.view(),
            next_base_row.view(),
            next_ext_row.view(),
            &input_values.challenges,
        );
        let evaluated_terminal_constraints = MasterExtTable::evaluate_terminal_constraints(
            current_base_row.view(),
            current_ext_row.view(),
            &input_values.challenges,
        );

        [
            evaluated_initial_constraints,
            evaluated_consistency_constraints,
            evaluated_transition_constraints,
            evaluated_terminal_constraints,
        ]
        .concat()
    }
}

impl BasicSnippet for AirConstraintEvaluation {
    fn inputs(&self) -> Vec<(DataType, String)> {
        vec![]
    }

    fn outputs(&self) -> Vec<(DataType, String)> {
        vec![(self.output_type(), "evaluated_constraints".to_owned())]
    }

    fn entrypoint(&self) -> String {
        assert!(
            self.mem_layout.is_integral(),
            "Memory layout for input values for constraint evaluation must be integral"
        );

        // TODO: Use `let preimage = self.mem_layout.encode()` here once [https://github.com/TritonVM/triton-vm/issues/253]
        // is closed.
        // let preimage = [
        //     self.mem_layout.free_mem_page_ptr,
        //     self.mem_layout.curr_base_row_ptr,
        //     self.mem_layout.curr_ext_row_ptr,
        //     self.mem_layout.next_base_row_ptr,
        //     self.mem_layout.next_ext_row_ptr,
        //     self.mem_layout.challenges_ptr,
        // ];
        // let digest = Tip5::hash(&preimage);
        // format!(
        //     "tasm_recufier_master_ext_table_air_constraint_evaluation_{}",
        //     digest.values()[0]
        // )
        // We probably only ever need to use *one* of these, so no need to parameterize
        // the entrypoint name, I think.
        "tasm_recufier_master_ext_table_air_constraint_evaluation".to_owned()
    }

    fn code(&self, _library: &mut Library) -> Vec<LabelledInstruction> {
        assert!(
            self.mem_layout.is_integral(),
            "Memory layout for input values for constraint evaluation must be integral"
        );

        let snippet_body = air_constraint_evaluation_tasm(self.mem_layout);

        let entrypoint = self.entrypoint();
        let mut code = triton_asm!(
            {entrypoint}:
        );
        code.extend(snippet_body);
        code.extend(triton_asm!(return));

        code
    }
}

#[cfg(test)]
fn an_integral_memory_layout() -> TasmConstraintEvaluationMemoryLayout {
    let mem_layout = TasmConstraintEvaluationMemoryLayout {
        free_mem_page_ptr: BFieldElement::new((u32::MAX as u64 - 1) * (1u64 << 32)),
        curr_base_row_ptr: BFieldElement::new(1u64),
        curr_ext_row_ptr: BFieldElement::new(1u64 << 20),
        next_base_row_ptr: BFieldElement::new(1u64 << 21),
        next_ext_row_ptr: BFieldElement::new(1u64 << 22),
        challenges_ptr: BFieldElement::new(1u64 << 23),
    };
    assert!(mem_layout.is_integral());

    mem_layout
}

#[derive(Debug, Clone)]
pub struct AirConstraintSnippetInputs {
    pub current_base_row: Vec<XFieldElement>,
    pub current_ext_row: Vec<XFieldElement>,
    pub next_base_row: Vec<XFieldElement>,
    pub next_ext_row: Vec<XFieldElement>,
    pub challenges: Challenges,
}

#[cfg(test)]
mod tests {
    use std::cell::RefCell;
    use std::collections::HashMap;
    use std::rc::Rc;

    use arbitrary::{Arbitrary, Unstructured};
    use rand::distributions::Standard;
    use rand::rngs::StdRng;
    use rand::thread_rng;
    use rand::Rng;
    use rand::RngCore;
    use rand::SeedableRng;
    use triton_vm::twenty_first::shared_math::x_field_element::EXTENSION_DEGREE;

    use crate::execute_test;
    use crate::linker::link_for_isolated_run;
    use crate::rust_shadowing_helper_functions::array::array_get;
    use crate::rust_shadowing_helper_functions::array::insert_as_array;
    use crate::traits::function::Function;
    use crate::traits::function::FunctionInitialState;
    use crate::VmOutputState;

    use super::*;

    #[test]
    fn conventional_air_constraint_memory_layout_is_integral() {
        AirConstraintEvaluation::conventional_air_constraint_memory_layout();
    }

    impl Function for AirConstraintEvaluation {
        fn rust_shadow(
            &self,
            _stack: &mut Vec<BFieldElement>,
            _memory: &mut HashMap<BFieldElement, BFieldElement>,
        ) {
            // Never called as we do a more manual test.
            // The more manual test is done bc we don't want to
            // have to simulate all the intermediate calculations
            // that are stored to memory.
            unimplemented!()
        }

        fn pseudorandom_initial_state(
            &self,
            seed: [u8; 32],
            _bench_case: Option<crate::snippet_bencher::BenchmarkCase>,
        ) -> FunctionInitialState {
            // Used for benchmarking
            let mut rng: StdRng = SeedableRng::from_seed(seed);
            let input_values = Self::random_input_values(&mut rng);
            let memory = self.prepare_tvm_memory(input_values);

            FunctionInitialState {
                stack: self.init_stack_for_isolated_run(),
                memory,
            }
        }
    }

    #[test]
    fn constraint_evaluation_test() {
        let snippet = AirConstraintEvaluation {
            mem_layout: an_integral_memory_layout(),
        };

        let mut seed: [u8; 32] = [0u8; 32];
        thread_rng().fill_bytes(&mut seed);
        snippet.prop(seed);
    }

    impl AirConstraintEvaluation {
        fn prop(&self, seed: [u8; 32]) {
            let mut rng: StdRng = SeedableRng::from_seed(seed);
            let input_values = Self::random_input_values(&mut rng);

            let tasm_result = self.tasm_result(input_values.clone());
            let host_machine_result = Self::host_machine_air_constraint_evaluation(input_values);

            assert_eq!(tasm_result.len(), host_machine_result.len());
            assert_eq!(tasm_result, host_machine_result);
        }

        fn random_input_values(rng: &mut StdRng) -> AirConstraintSnippetInputs {
            let current_base_row: Vec<XFieldElement> =
                rng.sample_iter(Standard).take(NUM_BASE_COLUMNS).collect();
            let current_ext_row: Vec<XFieldElement> =
                rng.sample_iter(Standard).take(NUM_EXT_COLUMNS).collect();
            let next_base_row: Vec<XFieldElement> =
                rng.sample_iter(Standard).take(NUM_BASE_COLUMNS).collect();
            let next_ext_row: Vec<XFieldElement> =
                rng.sample_iter(Standard).take(NUM_EXT_COLUMNS).collect();

            let mut ch_seed = [0u8; 12000];
            rng.fill_bytes(&mut ch_seed);
            let mut unstructured = Unstructured::new(&ch_seed);
            let challenges: Challenges = Challenges::arbitrary(&mut unstructured).unwrap();

            AirConstraintSnippetInputs {
                current_base_row,
                current_ext_row,
                next_base_row,
                next_ext_row,
                challenges,
            }
        }

        fn prepare_tvm_memory(
            &self,
            input_values: AirConstraintSnippetInputs,
        ) -> HashMap<BFieldElement, BFieldElement> {
            let mut memory: HashMap<BFieldElement, BFieldElement> = HashMap::default();
            insert_as_array(
                self.mem_layout.curr_base_row_ptr,
                &mut memory,
                input_values.current_base_row,
            );
            insert_as_array(
                self.mem_layout.curr_ext_row_ptr,
                &mut memory,
                input_values.current_ext_row,
            );
            insert_as_array(
                self.mem_layout.next_base_row_ptr,
                &mut memory,
                input_values.next_base_row,
            );
            insert_as_array(
                self.mem_layout.next_ext_row_ptr,
                &mut memory,
                input_values.next_ext_row,
            );
            insert_as_array(
                self.mem_layout.challenges_ptr,
                &mut memory,
                input_values.challenges.challenges.to_vec(),
            );

            memory
        }

        /// Note that the result lives as an array in TVM memory but is represented as a list here
        /// since its length is not known at `tasm-lib`'s compile time.
        fn read_result_from_memory(mut final_state: VmOutputState) -> Vec<XFieldElement> {
            let result_pointer = final_state.final_stack.pop().unwrap();
            let mut tasm_result: Vec<XFieldElement> = vec![];
            for i in 0..NUM_TOTAL_CONSTRAINTS {
                tasm_result.push(XFieldElement::new(
                    array_get(result_pointer, i, &final_state.final_ram, EXTENSION_DEGREE)
                        .try_into()
                        .unwrap(),
                ));
            }

            tasm_result
        }

        fn tasm_result(&self, input_values: AirConstraintSnippetInputs) -> Vec<XFieldElement> {
            let init_memory = self.prepare_tvm_memory(input_values);

            let stack = self.init_stack_for_isolated_run();
            let code = link_for_isolated_run(Rc::new(RefCell::new(self.to_owned())));
            let final_state = execute_test(
                &code,
                &mut stack.clone(),
                self.stack_diff(),
                vec![],
                NonDeterminism::default().with_ram(init_memory),
                None,
            );

            Self::read_result_from_memory(final_state)
        }
    }
}

#[cfg(test)]
mod bench {
    use crate::traits::function::ShadowedFunction;
    use crate::traits::rust_shadow::RustShadow;

    use super::*;

    #[test]
    fn bench_air_constraint_evaluation() {
        ShadowedFunction::new(AirConstraintEvaluation {
            mem_layout: an_integral_memory_layout(),
        })
        .bench();
    }
}
