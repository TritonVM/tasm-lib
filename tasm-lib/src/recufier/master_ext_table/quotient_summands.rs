use triton_vm::instruction::LabelledInstruction;
use triton_vm::prelude::*;
use triton_vm::table::TasmConstraintEvaluationMemoryLayout;

use crate::data_type::DataType;
use crate::library::Library;
use crate::recufier::master_ext_table::air_constraint_evaluation::AirConstraintEvaluation;
use crate::recufier::master_ext_table::zerofiers_inverse::ZerofiersInverse;
use crate::traits::basic_snippet::BasicSnippet;

#[derive(Debug, Clone)]
pub struct QuotientSummands {
    pub mem_layout: TasmConstraintEvaluationMemoryLayout,
}

impl QuotientSummands {
    pub fn with_conventional_memory_layout() -> Self {
        Self {
            mem_layout: AirConstraintEvaluation::conventional_air_constraint_memory_layout(),
        }
    }
}

impl BasicSnippet for QuotientSummands {
    fn inputs(&self) -> Vec<(DataType, String)> {
        vec![
            (DataType::Xfe, "out_of_domain_point_curr_row".to_owned()),
            (DataType::U32, "padded_height".to_owned()),
            (DataType::Bfe, "trace_domain_generator".to_owned()),
        ]
    }

    fn outputs(&self) -> Vec<(DataType, String)> {
        vec![(
            AirConstraintEvaluation::output_type(),
            "quotient_summands".to_owned(),
        )]
    }

    fn entrypoint(&self) -> String {
        "tasm_recufier_master_ext_table_quotient_summands".to_owned()
    }

    fn code(&self, library: &mut Library) -> Vec<LabelledInstruction> {
        let entrypoint = self.entrypoint();
        let air_constraints_evaluation = library.import(Box::new(AirConstraintEvaluation {
            mem_layout: self.mem_layout,
        }));

        let zerofiers_inverse_pointer =
            library.kmalloc(ZerofiersInverse::array_size().try_into().unwrap());
        let zerofiers_inverse = library.import(Box::new(ZerofiersInverse {
            zerofiers_inverse_pointer,
        }));

        // let mul_with_initial_zerofier_inv = triton_asm!(
        //     // _ [out_of_domain_point_curr_row] padded_height trace_domain_generator 0 [initial_zerofier_inv] *air_elem

        //     read_mem { EXTENSION_DEGREE }
        //     // _ [out_of_domain_point_curr_row] padded_height trace_domain_generator 0 [initial_zerofier_inv] [air_elem] *air_elem_prev

        //     swap 7
        //     pop 1
        //     // _ [out_of_domain_point_curr_row] padded_height trace_domain_generator *air_elem_prev [initial_zerofier_inv] [air_elem]

        //     dup 5
        //     dup 5
        //     dup 5
        //     xxmul
        //     // _ [out_of_domain_point_curr_row] padded_height trace_domain_generator *air_elem_prev [initial_zerofier_inv] [initial_zerofier_inv * air_elem]

        //     push 0
        //     swap 7
        //     // _ [out_of_domain_point_curr_row] padded_height trace_domain_generator 0 [initial_zerofier_inv] [initial_zerofier_inv * air_elem] *air_elem_prev

        //     push {EXTENSION_DEGREE}
        //     add
        //     write_mem {EXTENSION_DEGREE}

        // );

        triton_asm!(
            {entrypoint}:
                // _ [out_of_domain_point_curr_row] padded_height trace_domain_generator

                call {zerofiers_inverse}
                // _

                call {air_constraints_evaluation}
                // _ *air_constraints

                return
        )
    }
}
