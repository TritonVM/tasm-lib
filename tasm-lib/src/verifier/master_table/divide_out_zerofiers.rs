use triton_vm::prelude::LabelledInstruction;
use triton_vm::prelude::*;
use triton_vm::table::ConstraintType;
use triton_vm::table::master_table::MasterAuxTable;
use twenty_first::math::x_field_element::EXTENSION_DEGREE;

use crate::prelude::*;
use crate::verifier::master_table::air_constraint_evaluation::AirConstraintEvaluation;
use crate::verifier::master_table::zerofiers_inverse::ZerofiersInverse;

/// Takes an AIR evaluation and divides out the zerofiers.
#[derive(Debug, Clone)]
pub struct DivideOutZerofiers;

impl BasicSnippet for DivideOutZerofiers {
    fn parameters(&self) -> Vec<(DataType, String)> {
        vec![
            (DataType::VoidPointer, "*air_evaluation_result".to_string()),
            (DataType::Xfe, "out_of_domain_point_curr_row".to_owned()),
            (DataType::U32, "padded_height".to_owned()),
            (DataType::Bfe, "trace_domain_generator".to_owned()),
        ]
    }

    fn return_values(&self) -> Vec<(DataType, String)> {
        vec![(
            AirConstraintEvaluation::output_type(),
            "*quotient_summands".to_owned(),
        )]
    }

    fn entrypoint(&self) -> String {
        "tasmlib_verifier_master_table_divide_out_zerofiers".to_owned()
    }

    fn code(&self, library: &mut Library) -> Vec<LabelledInstruction> {
        let entrypoint = self.entrypoint();

        let zerofiers_inverse_alloc =
            library.kmalloc(ZerofiersInverse::array_size().try_into().unwrap());
        let zerofiers_inverse_snippet = ZerofiersInverse {
            zerofiers_inverse_write_address: zerofiers_inverse_alloc.write_address(),
        };
        let zerofiers_inverse = library.import(Box::new(zerofiers_inverse_snippet));

        let read_all_air_elements = vec![
            triton_asm!(
                // _ *air_elem
                read_mem { EXTENSION_DEGREE } // _ [air_elem] *air_elem_prev
            );
            MasterAuxTable::NUM_CONSTRAINTS
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
                MasterAuxTable::NUM_INITIAL_CONSTRAINTS,
            ),
            mul_and_write(
                ConstraintType::Consistency,
                MasterAuxTable::NUM_CONSISTENCY_CONSTRAINTS,
            ),
            mul_and_write(
                ConstraintType::Transition,
                MasterAuxTable::NUM_TRANSITION_CONSTRAINTS,
            ),
            mul_and_write(
                ConstraintType::Terminal,
                MasterAuxTable::NUM_TERMINAL_CONSTRAINTS,
            ),
        ]
        .concat();

        let jump_to_last_air_element = triton_asm!(
            // _ *air_elem[0]

            push {MasterAuxTable::NUM_CONSTRAINTS * EXTENSION_DEGREE - 1}
            add
            // _ *air_elem_last_word
        );

        let jump_to_first_air_element = triton_asm!(
            // _ *air_elem[n]

            push {-((MasterAuxTable::NUM_CONSTRAINTS * EXTENSION_DEGREE) as i32)}
            add
            // _ *air_elem_last_word
        );

        triton_asm!(
            {entrypoint}:
                // _ *air_evaluation_result [out_of_domain_point_curr_row] padded_height trace_domain_generator

                call {zerofiers_inverse}
                // _ *air_evaluation_result

                {&jump_to_last_air_element}
                // _ *air_elem_last_word

                {&read_all_air_elements}
                // _ [[air_elem]] (*air_constraints - 1)

                push 1 add
                // _ [[air_elem]] *air_constraints

                {&mul_and_write}
                // _ (*air_elem_last + 3)

                {&jump_to_first_air_element}

                return
        )
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use itertools::Itertools;
    use rand::prelude::*;
    use twenty_first::math::traits::ModPowU32;
    use twenty_first::math::traits::PrimitiveRootOfUnity;

    use super::*;
    use crate::empty_stack;
    use crate::execute_test;

    #[test]
    fn divide_out_zerofiers_test() {
        let snippet = DivideOutZerofiers;
        let mut seed: [u8; 32] = [0u8; 32];
        rand::rng().fill_bytes(&mut seed);
        snippet.test_equivalence_with_host_machine(seed);
    }

    impl DivideOutZerofiers {
        fn test_equivalence_with_host_machine(&self, seed: [u8; 32]) {
            let mut rng = StdRng::from_seed(seed);
            let (air_evaluation_result, ood_point_curr_row, padded_height, trace_domain_generator) =
                Self::random_input_values(&mut rng);

            let rust_result = Self::rust_result(
                air_evaluation_result,
                ood_point_curr_row,
                padded_height,
                trace_domain_generator,
            );

            let tasm_result = self.tasm_result(
                air_evaluation_result,
                ood_point_curr_row,
                padded_height,
                trace_domain_generator,
            );

            assert_eq!(tasm_result.len(), rust_result.len());
            assert_eq!(
                tasm_result.iter().copied().sum::<XFieldElement>(),
                rust_result.iter().copied().sum::<XFieldElement>(),
                "\ntasm: [{},...]\nrust: [{},...]",
                tasm_result.iter().take(3).join(","),
                rust_result.iter().take(3).join(",")
            );
            assert_eq!(tasm_result, rust_result);
        }

        pub(super) fn random_input_values(
            rng: &mut StdRng,
        ) -> (
            [XFieldElement; MasterAuxTable::NUM_CONSTRAINTS],
            XFieldElement,
            u32,
            BFieldElement,
        ) {
            let air_evaluation_result =
                rng.random::<[XFieldElement; MasterAuxTable::NUM_CONSTRAINTS]>();
            let ood_point_curr_row: XFieldElement = rng.random();
            let padded_height = 2u32.pow(rng.random_range(8..32));
            let trace_domain_generator =
                BFieldElement::primitive_root_of_unity(padded_height as u64).unwrap();

            (
                air_evaluation_result,
                ood_point_curr_row,
                padded_height,
                trace_domain_generator,
            )
        }

        /// Return the evaluated array of quotient values, and its address in memory
        fn tasm_result(
            &self,
            air_evaluation_result: [XFieldElement; MasterAuxTable::NUM_CONSTRAINTS],
            out_of_domain_point_curr_row: XFieldElement,
            padded_height: u32,
            trace_domain_generator: BFieldElement,
        ) -> Vec<XFieldElement> {
            let free_page_pointer = BFieldElement::new(((1u64 << 32) - 3) * (1 << 32));
            let mut memory = HashMap::<BFieldElement, BFieldElement>::new();
            println!(
                "air evaluation result encoded: [{}, ...]",
                air_evaluation_result.encode().iter().take(4).join(",")
            );
            for (i, e) in air_evaluation_result.encode().into_iter().enumerate() {
                memory.insert(free_page_pointer + BFieldElement::new(i as u64), e);
            }

            let stack = [
                empty_stack(),
                vec![free_page_pointer],
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
            let final_state = execute_test(
                &self.link_for_isolated_run(),
                &mut stack.clone(),
                self.stack_diff(),
                vec![],
                NonDeterminism::default().with_ram(memory),
                None,
            );

            // read the array pointed to by the pointer living on top of the stack
            AirConstraintEvaluation::read_result_from_memory(final_state).0
        }

        pub fn rust_result(
            air_evaluation_result: [XFieldElement; MasterAuxTable::NUM_CONSTRAINTS],
            out_of_domain_point_curr_row: XFieldElement,
            padded_height: u32,
            trace_domain_generator: BFieldElement,
        ) -> Vec<XFieldElement> {
            println!("trace domain generator: {trace_domain_generator}");
            println!("padded height: {padded_height}");
            println!("out-of-domain point current row: {out_of_domain_point_curr_row}");
            let initial_zerofier_inv = (out_of_domain_point_curr_row - bfe!(1)).inverse();
            let consistency_zerofier_inv =
                (out_of_domain_point_curr_row.mod_pow_u32(padded_height) - bfe!(1)).inverse();
            let except_last_row = out_of_domain_point_curr_row - trace_domain_generator.inverse();
            let transition_zerofier_inv = except_last_row * consistency_zerofier_inv;
            let terminal_zerofier_inv = except_last_row.inverse(); // i.e., only last row

            println!("initial zerofier inverse: {initial_zerofier_inv}");
            println!("consistency zerofier inverse: {consistency_zerofier_inv}");
            println!("transition zerofier inverse: {transition_zerofier_inv}");
            println!("terminal zerofier inverse: {terminal_zerofier_inv}");

            let mut running_total_constraints = 0;
            let initial_quotients = air_evaluation_result[running_total_constraints
                ..(running_total_constraints + MasterAuxTable::NUM_INITIAL_CONSTRAINTS)]
                .iter()
                .map(|&x| x * initial_zerofier_inv)
                .collect_vec();
            running_total_constraints += MasterAuxTable::NUM_INITIAL_CONSTRAINTS;

            let consistency_quotients = air_evaluation_result[running_total_constraints
                ..(running_total_constraints + MasterAuxTable::NUM_CONSISTENCY_CONSTRAINTS)]
                .iter()
                .map(|&x| x * consistency_zerofier_inv)
                .collect_vec();
            running_total_constraints += MasterAuxTable::NUM_CONSISTENCY_CONSTRAINTS;

            let transition_quotients = air_evaluation_result[running_total_constraints
                ..(running_total_constraints + MasterAuxTable::NUM_TRANSITION_CONSTRAINTS)]
                .iter()
                .map(|&x| x * transition_zerofier_inv)
                .collect_vec();
            running_total_constraints += MasterAuxTable::NUM_TRANSITION_CONSTRAINTS;

            let terminal_quotients = air_evaluation_result[running_total_constraints
                ..(running_total_constraints + MasterAuxTable::NUM_TERMINAL_CONSTRAINTS)]
                .iter()
                .map(|&x| x * terminal_zerofier_inv)
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
    use twenty_first::math::traits::PrimitiveRootOfUnity;

    use super::*;
    use crate::empty_stack;
    use crate::test_prelude::*;

    #[test]
    fn bench_divide_out_zerofiers() {
        ShadowedFunction::new(DivideOutZerofiers).bench();
    }

    impl Function for DivideOutZerofiers {
        fn rust_shadow(
            &self,
            _: &mut Vec<BFieldElement>,
            _: &mut HashMap<BFieldElement, BFieldElement>,
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
            _: Option<BenchmarkCase>,
        ) -> FunctionInitialState {
            // Used for benchmarking
            let mut rng = StdRng::from_seed(seed);
            let air_evaluation_result =
                rng.random::<[XFieldElement; MasterAuxTable::NUM_CONSTRAINTS]>();
            let ood_point_current_row = rng.random::<XFieldElement>();
            let padded_height = 1 << 20;
            let trace_domain_generator =
                BFieldElement::primitive_root_of_unity(padded_height).unwrap();

            let free_page_pointer = BFieldElement::new(((1u64 << 32) - 3) * (1 << 32));
            let mut memory = HashMap::<BFieldElement, BFieldElement>::new();
            for (i, e) in air_evaluation_result
                .encode()
                .into_iter()
                .skip(1)
                .enumerate()
            {
                memory.insert(free_page_pointer + BFieldElement::new(i as u64), e);
            }

            let stack = [
                empty_stack(),
                vec![free_page_pointer],
                ood_point_current_row
                    .coefficients
                    .into_iter()
                    .rev()
                    .collect_vec(),
                vec![BFieldElement::new(padded_height), trace_domain_generator],
            ]
            .concat();

            FunctionInitialState { stack, memory }
        }
    }
}
