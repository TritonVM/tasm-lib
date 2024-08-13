use triton_vm::instruction::LabelledInstruction;
use triton_vm::prelude::*;
use triton_vm::table::extension_table::Quotientable;
use triton_vm::table::master_table::MasterExtTable;
use triton_vm::twenty_first::math::x_field_element::EXTENSION_DEGREE;

use crate::data_type::DataType;
use crate::library::Library;
use crate::traits::basic_snippet::BasicSnippet;
use crate::verifier::master_ext_table::air_constraint_evaluation::AirConstraintEvaluation;
use crate::verifier::master_ext_table::zerofiers_inverse::ConstraintType;
use crate::verifier::master_ext_table::zerofiers_inverse::ZerofiersInverse;

#[derive(Debug, Clone)]
pub struct QuotientSummands {
    pub air_constraint_evaluation: AirConstraintEvaluation,
}

impl QuotientSummands {
    pub fn with_conventional_static_memory_layout() -> Self {
        Self {
            air_constraint_evaluation:
                AirConstraintEvaluation::with_conventional_static_memory_layout(),
        }
    }
    pub fn with_conventional_dynamic_memory_layout() -> Self {
        Self {
            air_constraint_evaluation:
                AirConstraintEvaluation::with_conventional_dynamic_memory_layout(),
        }
    }
}

impl BasicSnippet for QuotientSummands {
    fn inputs(&self) -> Vec<(DataType, String)> {
        let air_evaluation_arguments = self.air_constraint_evaluation.inputs();
        let zerofiers_inverse_arguments = vec![
            (DataType::Xfe, "out_of_domain_point_curr_row".to_owned()),
            (DataType::U32, "padded_height".to_owned()),
            (DataType::Bfe, "trace_domain_generator".to_owned()),
        ];
        [air_evaluation_arguments, zerofiers_inverse_arguments].concat()
    }

    fn outputs(&self) -> Vec<(DataType, String)> {
        vec![(
            AirConstraintEvaluation::output_type(),
            "quotient_summands".to_owned(),
        )]
    }

    fn entrypoint(&self) -> String {
        "tasmlib_verifier_master_ext_table_quotient_summands".to_owned()
    }

    fn code(&self, library: &mut Library) -> Vec<LabelledInstruction> {
        let entrypoint = self.entrypoint();
        let air_constraints_evaluation = library.import(Box::new(self.air_constraint_evaluation));

        let zerofiers_inverse_pointer =
            library.kmalloc(ZerofiersInverse::array_size().try_into().unwrap());
        let zerofiers_inverse_snippet = ZerofiersInverse {
            zerofiers_inverse_pointer,
        };
        let zerofiers_inverse = library.import(Box::new(zerofiers_inverse_snippet));

        let read_all_air_elements = vec![
            triton_asm!(
                // _ *air_elem
                read_mem { EXTENSION_DEGREE } // _ [air_elem] *air_elem_prev
            );
            MasterExtTable::NUM_CONSTRAINTS
        ]
        .concat();

        let mul_and_write = |constraint_type: ConstraintType, num_constraints: usize| {
            vec![
                triton_asm!(
                    // _ [[air_elem]] *air_elem

                    swap 3
                    swap 2
                    swap 1
                    // _ *air_elem [[air_elem]]

                    push {zerofiers_inverse_snippet.zerofier_inv_read_address(constraint_type)}
                    read_mem {EXTENSION_DEGREE}
                    pop 1

                    xx_mul
                    // _ [[air_elem] *air_elem [[air_elem * z_inv]]]

                    swap 1
                    swap 2
                    swap 3
                    // _ [[air_elem] [[air_elem * z_inv]]] *air_elem

                    write_mem {EXTENSION_DEGREE}
                    // _ [[air_elem] *air_elem_next

                );
                num_constraints
            ]
            .concat()
        };

        let mul_and_write = [
            mul_and_write(
                ConstraintType::Initial,
                MasterExtTable::NUM_INITIAL_CONSTRAINTS,
            ),
            mul_and_write(
                ConstraintType::Consistency,
                MasterExtTable::NUM_CONSISTENCY_CONSTRAINTS,
            ),
            mul_and_write(
                ConstraintType::Transition,
                MasterExtTable::NUM_TRANSITION_CONSTRAINTS,
            ),
            mul_and_write(
                ConstraintType::Terminal,
                MasterExtTable::NUM_TERMINAL_CONSTRAINTS,
            ),
        ]
        .concat();

        let point_to_last_air_element = triton_asm!(
            // _ *air_elem[0]

            push {MasterExtTable::NUM_CONSTRAINTS * EXTENSION_DEGREE - 1}
            add
            // _ *air_elem_last_word
        );

        let point_to_first_air_element = triton_asm!(
            // _ *air_elem[n]

            push {-((MasterExtTable::NUM_CONSTRAINTS * EXTENSION_DEGREE) as i32)}
            add
            // _ *air_elem_last_word
        );

        triton_asm!(
            {entrypoint}:
                // _ (*curr_base_row *curr_ext_row *next_bas_row *next_ext_row)? [out_of_domain_point_curr_row] padded_height trace_domain_generator

                call {zerofiers_inverse}
                // _

                call {air_constraints_evaluation}
                // _ *air_constraints

                {&point_to_last_air_element}
                // _ *air_elem_last_word

                {&read_all_air_elements}
                // _ [[air_elem]] (*air_constraints - 1)

                push 1 add
                // _ [[air_elem]] *air_constraints

                {&mul_and_write}
                // _ (*air_elem_last + 3)

                {&point_to_first_air_element}

                return
        )
    }
}

#[cfg(test)]
mod tests {
    use std::cell::RefCell;
    use std::rc::Rc;

    use itertools::Itertools;
    use ndarray::Array1;
    use rand::prelude::*;
    use triton_vm::table::extension_table::Evaluable;
    use triton_vm::twenty_first::math::traits::Inverse;
    use triton_vm::twenty_first::math::traits::ModPowU32;
    use triton_vm::twenty_first::math::traits::PrimitiveRootOfUnity;

    use crate::execute_test;
    use crate::linker::link_for_isolated_run;
    use crate::verifier::master_ext_table::air_constraint_evaluation::AirConstraintSnippetInputs;
    use crate::verifier::master_ext_table::air_constraint_evaluation::MemoryLayout;

    use super::*;

    #[test]
    fn quotient_summands_evaluation_test() {
        let static_snippet = QuotientSummands::with_conventional_static_memory_layout();
        let dynamic_snippet = QuotientSummands::with_conventional_dynamic_memory_layout();

        let mut seed: [u8; 32] = [0u8; 32];
        thread_rng().fill_bytes(&mut seed);
        static_snippet.test_equivalence_with_host_machine(seed);

        thread_rng().fill_bytes(&mut seed);
        dynamic_snippet.test_equivalence_with_host_machine(seed);
    }

    impl QuotientSummands {
        fn test_equivalence_with_host_machine(&self, seed: [u8; 32]) {
            let mut rng: StdRng = SeedableRng::from_seed(seed);
            let (air_input_values, ood_point_curr_row, padded_height, trace_domain_generator) =
                Self::random_input_values(&mut rng);

            let (tasm_result, quotient_array_address) = self.tasm_result(
                air_input_values.clone(),
                ood_point_curr_row,
                padded_height,
                trace_domain_generator,
            );
            let host_machine_result = Self::host_machine_quotient_evaluation(
                air_input_values.clone(),
                ood_point_curr_row,
                padded_height,
                trace_domain_generator,
            );

            assert_eq!(tasm_result.len(), host_machine_result.len());
            assert_eq!(tasm_result, host_machine_result);

            // Verify that quotient is stored in same address as AIR constraints, i.e. that
            // AIR constraints were overwritten.
            let (_air_tasm_result, air_array_address) =
                self.air_constraint_evaluation.tasm_result(air_input_values);
            assert_eq!(air_array_address, quotient_array_address);
        }

        pub(super) fn random_input_values(
            rng: &mut StdRng,
        ) -> (
            AirConstraintSnippetInputs,
            XFieldElement,
            u32,
            BFieldElement,
        ) {
            let air_input_values = AirConstraintEvaluation::random_input_values(rng);
            let ood_point_curr_row: XFieldElement = rng.gen();
            let padded_height = 2u32.pow(rng.gen_range(8..32));
            let trace_domain_generator =
                BFieldElement::primitive_root_of_unity(padded_height as u64).unwrap();

            (
                air_input_values,
                ood_point_curr_row,
                padded_height,
                trace_domain_generator,
            )
        }

        /// Return the evaluated array of quotient values, and its address in memory
        fn tasm_result(
            &self,
            air_input_value: AirConstraintSnippetInputs,
            out_of_domain_point_curr_row: XFieldElement,
            padded_height: u32,
            trace_domain_generator: BFieldElement,
        ) -> (Vec<XFieldElement>, BFieldElement) {
            let (init_memory, stack) = match self.air_constraint_evaluation.memory_layout {
                MemoryLayout::Dynamic(dynamic_layout) => self
                    .air_constraint_evaluation
                    .prepare_tvm_memory_and_stack_with_dynamic_layout(
                        dynamic_layout,
                        air_input_value,
                    ),
                MemoryLayout::Static(static_layout) => self
                    .air_constraint_evaluation
                    .prepare_tvm_memory_and_stack_with_static_layout(
                        static_layout,
                        air_input_value,
                    ),
            };

            let stack = [
                stack,
                out_of_domain_point_curr_row
                    .coefficients
                    .into_iter()
                    .rev()
                    .collect_vec(),
                vec![
                    BFieldElement::new(padded_height as u64),
                    trace_domain_generator,
                ],
            ]
            .concat();
            let code = link_for_isolated_run(Rc::new(RefCell::new(self.to_owned())));
            let final_state = execute_test(
                &code,
                &mut stack.clone(),
                self.stack_diff(),
                vec![],
                NonDeterminism::default().with_ram(init_memory),
                None,
            );

            AirConstraintEvaluation::read_result_from_memory(final_state)
        }

        pub fn host_machine_quotient_evaluation(
            air_evaluation_input: AirConstraintSnippetInputs,
            out_of_domain_point_curr_row: XFieldElement,
            padded_height: u32,
            trace_domain_generator: BFieldElement,
        ) -> Vec<XFieldElement> {
            let initial_zerofier_inv = (out_of_domain_point_curr_row - bfe!(1)).inverse();
            let consistency_zerofier_inv =
                (out_of_domain_point_curr_row.mod_pow_u32(padded_height) - bfe!(1)).inverse();
            let except_last_row = out_of_domain_point_curr_row - trace_domain_generator.inverse();
            let transition_zerofier_inv = except_last_row * consistency_zerofier_inv;
            let terminal_zerofier_inv = except_last_row.inverse(); // i.e., only last row
            let current_base_row = Array1::from(air_evaluation_input.current_base_row);
            let current_ext_row = Array1::from(air_evaluation_input.current_ext_row);
            let next_base_row = Array1::from(air_evaluation_input.next_base_row);
            let next_ext_row = Array1::from(air_evaluation_input.next_ext_row);
            let initial_quotients = MasterExtTable::evaluate_initial_constraints(
                current_base_row.view(),
                current_ext_row.view(),
                &air_evaluation_input.challenges,
            )
            .into_iter()
            .map(|x| x * initial_zerofier_inv)
            .collect_vec();
            let consistency_quotients = MasterExtTable::evaluate_consistency_constraints(
                current_base_row.view(),
                current_ext_row.view(),
                &air_evaluation_input.challenges,
            )
            .into_iter()
            .map(|x| x * consistency_zerofier_inv)
            .collect_vec();
            let transition_quotients = MasterExtTable::evaluate_transition_constraints(
                current_base_row.view(),
                current_ext_row.view(),
                next_base_row.view(),
                next_ext_row.view(),
                &air_evaluation_input.challenges,
            )
            .into_iter()
            .map(|x| x * transition_zerofier_inv)
            .collect_vec();
            let terminal_quotients = MasterExtTable::evaluate_terminal_constraints(
                current_base_row.view(),
                current_ext_row.view(),
                &air_evaluation_input.challenges,
            )
            .into_iter()
            .map(|x| x * terminal_zerofier_inv)
            .collect_vec();

            [
                initial_quotients,
                consistency_quotients,
                transition_quotients,
                terminal_quotients,
            ]
            .concat()
        }
    }
}

#[cfg(test)]
mod bench {
    use std::collections::HashMap;

    use itertools::Itertools;
    use rand::prelude::*;

    use crate::traits::function::Function;
    use crate::traits::function::FunctionInitialState;
    use crate::traits::function::ShadowedFunction;
    use crate::traits::rust_shadow::RustShadow;
    use crate::verifier::master_ext_table::air_constraint_evaluation::MemoryLayout;

    use super::*;

    #[test]
    fn bench_quotient_summands_evaluation() {
        ShadowedFunction::new(QuotientSummands::with_conventional_static_memory_layout()).bench();
    }

    impl Function for QuotientSummands {
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
            let (air_input_values, ood_point_curr_row, padded_height, trace_domain_generator) =
                Self::random_input_values(&mut rng);
            let (memory, stack) = match self.air_constraint_evaluation.memory_layout {
                MemoryLayout::Dynamic(dynamic_layout) => self
                    .air_constraint_evaluation
                    .prepare_tvm_memory_and_stack_with_dynamic_layout(
                        dynamic_layout,
                        air_input_values,
                    ),
                MemoryLayout::Static(static_layout) => self
                    .air_constraint_evaluation
                    .prepare_tvm_memory_and_stack_with_static_layout(
                        static_layout,
                        air_input_values,
                    ),
            };

            let stack = [
                stack,
                ood_point_curr_row
                    .coefficients
                    .into_iter()
                    .rev()
                    .collect_vec(),
                vec![
                    BFieldElement::new(padded_height as u64),
                    trace_domain_generator,
                ],
            ]
            .concat();

            FunctionInitialState { stack, memory }
        }
    }
}
