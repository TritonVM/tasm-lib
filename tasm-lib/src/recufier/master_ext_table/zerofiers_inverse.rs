use strum::EnumCount;
use strum::EnumIter;
use triton_vm::prelude::*;
use triton_vm::twenty_first::shared_math::b_field_element::BFieldElement;
use triton_vm::twenty_first::shared_math::x_field_element::EXTENSION_DEGREE;

use crate::arithmetic::xfe::to_the_power_of_power_of_2::ToThePowerOfPowerOf2;
use crate::data_type::ArrayType;
use crate::data_type::DataType;
use crate::library::Library;
use crate::traits::basic_snippet::BasicSnippet;

// TODO: Remove this once `ConstraintType` is made public in Triton VM:
// https://github.com/TritonVM/triton-vm/issues/263
#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash, EnumCount, EnumIter)]
pub(super) enum ConstraintType {
    Initial,
    Consistency,
    Transition,
    Terminal,
}

pub struct ZerofiersInverse;

impl ZerofiersInverse {
    fn array_size() -> i32 {
        (EXTENSION_DEGREE * ConstraintType::COUNT)
            .try_into()
            .unwrap()
    }
}

impl BasicSnippet for ZerofiersInverse {
    fn inputs(&self) -> Vec<(DataType, String)> {
        vec![
            (DataType::Xfe, "out_of_domain_point_curr_row".to_owned()),
            (DataType::U32, "padded_height".to_owned()),
            (DataType::Bfe, "trace_domain_generator".to_owned()),
        ]
    }

    fn outputs(&self) -> Vec<(DataType, String)> {
        vec![(
            DataType::Array(Box::new(ArrayType {
                element_type: DataType::Xfe,
                length: ConstraintType::COUNT,
            })),
            "*zerofiers_inv".to_owned(),
        )]
    }

    fn entrypoint(&self) -> String {
        "tasm_recufier_master_ext_table_zerofiers".to_owned()
    }

    fn code(&self, library: &mut Library) -> Vec<LabelledInstruction> {
        let xfe_mod_pow_pow_2 = library.import(Box::new(ToThePowerOfPowerOf2));

        let zerofier_inverse_pointer = library.kmalloc(Self::array_size().try_into().unwrap());

        let zerofier_inv_write_address = |x: ConstraintType| {
            zerofier_inverse_pointer
                + BFieldElement::new((EXTENSION_DEGREE * x as usize).try_into().unwrap())
        };
        let zerofier_inv_read_address = |x: ConstraintType| {
            zerofier_inv_write_address(x)
                + BFieldElement::new((EXTENSION_DEGREE - 1).try_into().unwrap())
        };

        let calculate_initial_zerofier_inverse = triton_asm!(
            // _ [out_of_domain_point_curr_row] padded_height trace_domain_generator

            dup 4
            dup 4
            dup 4
            push -1
            add
            xinvert
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
           xinvert
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

            push {zerofier_inv_read_address(ConstraintType::Consistency)}
            read_mem {EXTENSION_DEGREE}
            pop 1
            // _ [except_last_row] [except_last_row] [consistency_zerofier_inv]

            xxmul
            // _ [except_last_row] [transition_zerofier_inv]
        );

        let calculate_terminal_zerofier_inv = triton_asm!(
            // _ [except_last_row]
            xinvert // _ [terminal_zerofier_inv]
        );

        let entrypoint = self.entrypoint();
        triton_asm!(
            {entrypoint}:
                // _ [out_of_domain_point_curr_row] padded_height trace_domain_generator

                {&calculate_initial_zerofier_inverse}
                // _ [out_of_domain_point_curr_row] padded_height trace_domain_generator [initial_zerofier_inv]

                push {zerofier_inv_write_address(ConstraintType::Initial)}
                write_mem {EXTENSION_DEGREE}
                pop 1
                // _ [out_of_domain_point_curr_row] padded_height trace_domain_generator

                {&calculate_consistency_zerofier_inv}
                 // _ [out_of_domain_point_curr_row] trace_domain_generator [consistency_zerofier_inv]

                push {zerofier_inv_write_address(ConstraintType::Consistency)}
                write_mem {EXTENSION_DEGREE}
                pop 1
                // _ [out_of_domain_point_curr_row] trace_domain_generator

                {&calculate_except_last_row}
                // _ [except_last_row]

                {&calculate_transition_zerofier_inv}
                // _ [except_last_row] [transition_zerofier_inv]

                push {zerofier_inv_write_address(ConstraintType::Transition)}
                write_mem {EXTENSION_DEGREE}
                pop 1
                 // _ [except_last_row]

                {&calculate_terminal_zerofier_inv}
                // _ [terminal_zerofier_inv]

                push {zerofier_inv_write_address(ConstraintType::Terminal)}
                write_mem {EXTENSION_DEGREE}
                pop 1
                // _

                push {zerofier_inverse_pointer}

                return
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use std::collections::HashMap;

    use itertools::Itertools;
    use num::One;
    use rand::{rngs::StdRng, Rng, SeedableRng};
    use triton_vm::twenty_first::shared_math::{
        b_field_element::BFieldElement,
        traits::{Inverse, ModPowU32, PrimitiveRootOfUnity},
        x_field_element::XFieldElement,
    };

    use crate::{
        rust_shadowing_helper_functions::array::insert_as_array,
        snippet_bencher::BenchmarkCase,
        traits::{
            basic_snippet::BasicSnippet,
            function::{Function, FunctionInitialState, ShadowedFunction},
            rust_shadow::RustShadow,
        },
    };

    #[test]
    fn zerofiers_inverse_pbt() {
        ShadowedFunction::new(ZerofiersInverse).test()
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

            let zerofier_inverse_pointer = bfe!(-(Self::array_size() + 1));

            insert_as_array(
                zerofier_inverse_pointer,
                memory,
                vec![
                    initial_zerofier_inv,
                    consistency_zerofier_inv,
                    transition_zerofier_inv,
                    terminal_zerofier_inv,
                ],
            );

            stack.push(zerofier_inverse_pointer)
        }

        fn pseudorandom_initial_state(
            &self,
            seed: [u8; 32],
            _bench_case: Option<BenchmarkCase>,
        ) -> FunctionInitialState {
            let mut rng: StdRng = SeedableRng::from_seed(seed);
            let ood_current_row: XFieldElement = rng.gen();
            let log_2_padded_height: u32 = rng.gen_range(8..32);
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
