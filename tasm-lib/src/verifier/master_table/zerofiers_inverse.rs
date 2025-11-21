use strum::EnumCount;
use triton_vm::prelude::*;
use triton_vm::table::ConstraintType;
use twenty_first::math::x_field_element::EXTENSION_DEGREE;

use crate::arithmetic::xfe::to_the_power_of_power_of_2::ToThePowerOfPowerOf2;
use crate::prelude::*;

/// Calculate all inverses of the zerofiers. It is the caller's responsibility
/// to statically allocate memory for the array where the result is stored.
#[derive(Debug, Copy, Clone)]
pub struct ZerofiersInverse {
    pub zerofiers_inverse_write_address: BFieldElement,
}

impl ZerofiersInverse {
    pub(super) fn array_size() -> usize {
        EXTENSION_DEGREE * ConstraintType::COUNT
    }

    /// Return the address needed to write to the inverted zerofiers array
    fn zerofier_inv_write_address(&self, constraint_type: ConstraintType) -> BFieldElement {
        let constraint_type_offset =
            u64::try_from(EXTENSION_DEGREE * constraint_type as usize).unwrap();
        self.zerofiers_inverse_write_address + bfe!(constraint_type_offset)
    }

    /// Return the address needed to read from the inverted zerofiers array
    pub(super) fn zerofier_inv_read_address(
        &self,
        constraint_type: ConstraintType,
    ) -> BFieldElement {
        self.zerofier_inv_write_address(constraint_type)
            + BFieldElement::new((EXTENSION_DEGREE - 1).try_into().unwrap())
    }
}

impl BasicSnippet for ZerofiersInverse {
    fn parameters(&self) -> Vec<(DataType, String)> {
        vec![
            (
                DataType::Xfe,
                "out_of_domain_point_curr_row_point".to_owned(),
            ),
            (DataType::U32, "padded_height".to_owned()),
            (DataType::Bfe, "trace_domain_generator".to_owned()),
        ]
    }

    fn return_values(&self) -> Vec<(DataType, String)> {
        vec![]
    }

    fn entrypoint(&self) -> String {
        "tasmlib_verifier_master_table_zerofiers_inverse".to_owned()
    }

    fn code(&self, library: &mut Library) -> Vec<LabelledInstruction> {
        let xfe_mod_pow_pow_2 = library.import(Box::new(ToThePowerOfPowerOf2));

        let calculate_initial_zerofier_inverse = triton_asm!(
            // _ [out_of_domain_point_curr_row] padded_height trace_domain_generator

            dup 4
            dup 4
            dup 4
            push -1
            add
            x_invert
            // _ [out_of_domain_point_curr_row] padded_height trace_domain_generator [initial_zerofier_inv]
        );

        let calculate_consistency_zerofier_inv = triton_asm!(
           // _ [out_of_domain_point_curr_row] padded_height trace_domain_generator

           swap 1
           // _ [out_of_domain_point_curr_row] trace_domain_generator padded_height

           log_2_floor

           dup 4
           dup 4
           dup 4
           // _ [out_of_domain_point_curr_row] trace_domain_generator log2_padded_height [out_of_domain_point_curr_row]

           call {xfe_mod_pow_pow_2}
           // _ [out_of_domain_point_curr_row] trace_domain_generator [out_of_domain_point_curr_row^{padded_height}]

           push -1
           add
           x_invert
           // _ [out_of_domain_point_curr_row] trace_domain_generator [consistency_zerofier_inv]
        );

        let calculate_except_last_row = triton_asm!(
            // _ [out_of_domain_point_curr_row] trace_domain_generator

            invert
            // _ [out_of_domain_point_curr_row] (1 / trace_domain_generator)

            push -1
            mul
            add
            // _ [except_last_row]
        );

        let calculate_transition_zerofier_inv = triton_asm!(
            // _ [except_last_row]

            dup 2
            dup 2
            dup 2
            // _ [except_last_row] [except_last_row]

            push {self.zerofier_inv_read_address(ConstraintType::Consistency)}
            read_mem {EXTENSION_DEGREE}
            pop 1
            // _ [except_last_row] [except_last_row] [consistency_zerofier_inv]

            xx_mul
            // _ [except_last_row] [transition_zerofier_inv]
        );

        let calculate_terminal_zerofier_inv = triton_asm!(
            // _ [except_last_row]
            x_invert // _ [terminal_zerofier_inv]
        );

        let entrypoint = self.entrypoint();
        triton_asm!(
            {entrypoint}:
                // _ [out_of_domain_point_curr_row] padded_height trace_domain_generator

                {&calculate_initial_zerofier_inverse}
                // _ [out_of_domain_point_curr_row] padded_height trace_domain_generator [initial_zerofier_inv]

                push {self.zerofier_inv_write_address(ConstraintType::Initial)}
                write_mem {EXTENSION_DEGREE}
                pop 1
                // _ [out_of_domain_point_curr_row] padded_height trace_domain_generator

                {&calculate_consistency_zerofier_inv}
                 // _ [out_of_domain_point_curr_row] trace_domain_generator [consistency_zerofier_inv]

                push {self.zerofier_inv_write_address(ConstraintType::Consistency)}
                write_mem {EXTENSION_DEGREE}
                pop 1
                // _ [out_of_domain_point_curr_row] trace_domain_generator

                {&calculate_except_last_row}
                // _ [except_last_row]

                {&calculate_transition_zerofier_inv}
                // _ [except_last_row] [transition_zerofier_inv]

                push {self.zerofier_inv_write_address(ConstraintType::Transition)}
                write_mem {EXTENSION_DEGREE}
                pop 1
                 // _ [except_last_row]

                {&calculate_terminal_zerofier_inv}
                // _ [terminal_zerofier_inv]

                push {self.zerofier_inv_write_address(ConstraintType::Terminal)}
                write_mem {EXTENSION_DEGREE}
                pop 1
                // _


                return
        )
    }
}

#[cfg(test)]
mod tests {
    use num::One;
    use twenty_first::math::traits::ModPowU32;
    use twenty_first::math::traits::PrimitiveRootOfUnity;

    use super::*;
    use crate::rust_shadowing_helper_functions::array::insert_as_array;
    use crate::test_prelude::*;

    #[test]
    fn zerofiers_inverse_pbt() {
        let mem_address_if_first_static_malloc =
            -BFieldElement::new(ZerofiersInverse::array_size() as u64) - BFieldElement::one();
        ShadowedFunction::new(ZerofiersInverse {
            zerofiers_inverse_write_address: mem_address_if_first_static_malloc,
        })
        .test()
    }

    impl Function for ZerofiersInverse {
        fn rust_shadow(
            &self,
            stack: &mut Vec<BFieldElement>,
            memory: &mut HashMap<BFieldElement, BFieldElement>,
        ) {
            let trace_domain_generator = stack.pop().unwrap();
            let padded_height: u32 = stack.pop().unwrap().value().try_into().unwrap();
            let out_of_domain_point_curr_row = XFieldElement::new([
                stack.pop().unwrap(),
                stack.pop().unwrap(),
                stack.pop().unwrap(),
            ]);
            let initial_zerofier_inv =
                (out_of_domain_point_curr_row - BFieldElement::one()).inverse();
            let consistency_zerofier_inv =
                (out_of_domain_point_curr_row.mod_pow_u32(padded_height) - BFieldElement::one())
                    .inverse();
            let except_last_row = out_of_domain_point_curr_row - trace_domain_generator.inverse();
            let transition_zerofier_inv = except_last_row * consistency_zerofier_inv;
            let terminal_zerofier_inv = except_last_row.inverse();

            insert_as_array(
                self.zerofiers_inverse_write_address,
                memory,
                vec![
                    initial_zerofier_inv,
                    consistency_zerofier_inv,
                    transition_zerofier_inv,
                    terminal_zerofier_inv,
                ],
            );
        }

        fn pseudorandom_initial_state(
            &self,
            seed: [u8; 32],
            _bench_case: Option<BenchmarkCase>,
        ) -> FunctionInitialState {
            let mut rng = StdRng::from_seed(seed);
            let ood_current_row: XFieldElement = rng.random();
            let log_2_padded_height: u32 = rng.random_range(8..32);
            let padded_height = 2u32.pow(log_2_padded_height);
            let trace_domain_generator =
                BFieldElement::primitive_root_of_unity(padded_height as u64).unwrap();

            FunctionInitialState {
                stack: [
                    self.init_stack_for_isolated_run(),
                    ood_current_row.coefficients.into_iter().rev().collect_vec(),
                    vec![
                        BFieldElement::new(padded_height as u64),
                        trace_domain_generator,
                    ],
                ]
                .concat(),
                memory: HashMap::default(),
            }
        }
    }
}
