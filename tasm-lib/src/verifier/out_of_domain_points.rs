use triton_vm::prelude::triton_asm;
use triton_vm::prelude::LabelledInstruction;
use triton_vm::twenty_first::math::x_field_element::EXTENSION_DEGREE;

use crate::arithmetic::xfe::to_the_fourth::ToTheFourth;
use crate::data_type::ArrayType;
use crate::data_type::DataType;
use crate::library::Library;
use crate::traits::basic_snippet::BasicSnippet;

/// Calculate the three needed values related to out-of-domain points and store them in a statically
/// allocated array. Return the pointer to this array.
#[derive(Debug, Clone, Copy)]
pub struct OutOfDomainPoints;

pub const NUM_OF_OUT_OF_DOMAIN_POINTS: usize = 3;

#[derive(Debug, Clone, Copy)]
pub enum OodPoint {
    CurrentRow,
    NextRow,
    CurrentRowPowNumSegments,
}

impl OutOfDomainPoints {
    /// Push the requested OOD point to the stack, pop the pointer.
    pub fn read_ood_point(ood_point_type: OodPoint) -> Vec<LabelledInstruction> {
        let address_offset = (ood_point_type as usize) * EXTENSION_DEGREE + (EXTENSION_DEGREE - 1);
        triton_asm!(
            // _ *ood_points // of type same as the output value of this snippet

            push {address_offset}
            add
            // _ (*ood_points[n] + 2)

            read_mem {EXTENSION_DEGREE}
            // _ [ood_piont] (*ood_points[n] - 1)

            pop 1
        )
    }
}

impl BasicSnippet for OutOfDomainPoints {
    fn inputs(&self) -> Vec<(DataType, String)> {
        vec![
            (DataType::Bfe, "trace_domain_generator".to_owned()),
            (DataType::Xfe, "out_of_domain_curr_row".to_owned()),
        ]
    }

    fn outputs(&self) -> Vec<(DataType, String)> {
        vec![(
            DataType::Array(Box::new(ArrayType {
                element_type: DataType::Xfe,
                length: NUM_OF_OUT_OF_DOMAIN_POINTS,
            })),
            "out_of_domain_points".to_owned(),
        )]
    }

    fn entrypoint(&self) -> String {
        "tasmlib_verifier_out_of_domain_points".to_owned()
    }

    fn code(&self, library: &mut Library) -> Vec<LabelledInstruction> {
        let entrypoint = self.entrypoint();

        // Snippet for sampling *one* scalar, and holding the values:
        // - `out_of_domain_point_curr_row`
        // - `out_of_domain_point_next_row`
        // - `out_of_domain_point_curr_row_pow_num_segments`
        let num_words_for_out_of_domain_points = (NUM_OF_OUT_OF_DOMAIN_POINTS * EXTENSION_DEGREE)
            .try_into()
            .unwrap();
        let ood_points_alloc = library.kmalloc(num_words_for_out_of_domain_points);

        let pow_four = library.import(Box::new(ToTheFourth));

        triton_asm!(
            {entrypoint}:
                // _ trace_domain_generator [ood_curr_row]

                dup 2
                dup 2
                dup 2
                dup 2
                dup 2
                dup 2
                push {ood_points_alloc.write_address()}
                write_mem {EXTENSION_DEGREE}
                // _ trace_domain_generator [ood_curr_row] [ood_curr_row] *ood_points[1]

                swap 7
                // _ *ood_points[1] [ood_curr_row] [ood_curr_row] trace_domain_generator

                xb_mul
                // _ *ood_points[1] [ood_curr_row] [ood_next_row]

                dup 6
                write_mem {EXTENSION_DEGREE}
                // _ *ood_points[1] [ood_curr_row] *ood_points[2]

                swap 4
                pop 1
                // _ *ood_points[2] [ood_curr_row]

                call {pow_four}
                // _ *ood_points[2] [ood_curr_row**4]

                swap 1
                swap 2
                swap 3
                // _ [ood_curr_row**4] *ood_points[2]

                write_mem {EXTENSION_DEGREE}
                // _ *ood_points[3]

                push {-(3 * EXTENSION_DEGREE as i32)}
                add
                // _ *ood_points

                return
        )
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use rand::prelude::*;
    use triton_vm::prelude::*;
    use triton_vm::table::NUM_QUOTIENT_SEGMENTS;
    use triton_vm::twenty_first::math::traits::ModPowU32;
    use triton_vm::twenty_first::math::traits::PrimitiveRootOfUnity;

    use crate::rust_shadowing_helper_functions::array::insert_as_array;
    use crate::snippet_bencher::BenchmarkCase;
    use crate::traits::function::Function;
    use crate::traits::function::FunctionInitialState;
    use crate::traits::function::ShadowedFunction;
    use crate::traits::rust_shadow::RustShadow;

    use super::*;

    #[test]
    fn ood_points_pbt() {
        ShadowedFunction::new(OutOfDomainPoints).test();
    }

    impl Function for OutOfDomainPoints {
        fn rust_shadow(
            &self,
            stack: &mut Vec<BFieldElement>,
            memory: &mut HashMap<BFieldElement, BFieldElement>,
        ) {
            let ood_curr_row = XFieldElement::new([
                stack.pop().unwrap(),
                stack.pop().unwrap(),
                stack.pop().unwrap(),
            ]);
            let domain_generator = stack.pop().unwrap();
            let ood_next_row = ood_curr_row * domain_generator;
            let ood_curr_row_pow_num_segments =
                ood_curr_row.mod_pow_u32(NUM_QUOTIENT_SEGMENTS.try_into().unwrap());
            let static_malloc_size: i32 = (EXTENSION_DEGREE * NUM_OF_OUT_OF_DOMAIN_POINTS)
                .try_into()
                .unwrap();
            let ood_points_pointer = bfe!(-static_malloc_size - 1);
            insert_as_array(
                ood_points_pointer,
                memory,
                vec![ood_curr_row, ood_next_row, ood_curr_row_pow_num_segments],
            );

            stack.push(ood_points_pointer)
        }

        fn pseudorandom_initial_state(
            &self,
            seed: [u8; 32],
            bench_case: Option<crate::snippet_bencher::BenchmarkCase>,
        ) -> FunctionInitialState {
            let domain_length = match bench_case {
                Some(BenchmarkCase::CommonCase) => 1u64 << 20,
                Some(BenchmarkCase::WorstCase) => 1u64 << 24,
                None => {
                    let mut rng: StdRng = SeedableRng::from_seed(seed);
                    1u64 << rng.gen_range(8..=32)
                }
            };
            println!("domain_length: {domain_length}");

            let domain_generator = BFieldElement::primitive_root_of_unity(domain_length).unwrap();
            let ood_curr_row: XFieldElement = random();

            FunctionInitialState {
                stack: [
                    self.init_stack_for_isolated_run(),
                    vec![
                        domain_generator,
                        ood_curr_row.coefficients[2],
                        ood_curr_row.coefficients[1],
                        ood_curr_row.coefficients[0],
                    ],
                ]
                .concat(),
                memory: HashMap::default(),
            }
        }
    }
}

#[cfg(test)]
mod benches {
    use crate::traits::function::ShadowedFunction;
    use crate::traits::rust_shadow::RustShadow;

    use super::*;

    #[test]
    fn ood_points_pbt_bench() {
        ShadowedFunction::new(OutOfDomainPoints).bench();
    }
}
