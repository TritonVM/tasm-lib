use itertools::Itertools;
use triton_vm::triton_asm;

use crate::data_type::ArrayType;
use crate::data_type::DataType;
use crate::traits::basic_snippet::BasicSnippet;

/// Evaluate a polynomial in a point using the Horner method.
///
/// HornerEvaluation takes an array of coefficients (representing a polynomial)
/// and a scalar (representing an indeterminate) and computes the value of the
/// polynomial in that point. It can be used for univariate batching, whereby
/// the object is to compute a random linear sum of a given set of points, and
/// the weights are given by the powers of one challenge.
pub struct HornerEvaluation {
    pub num_coefficients: usize,
}

impl HornerEvaluation {
    pub fn new(num_coefficients: usize) -> Self {
        Self { num_coefficients }
    }
}

impl BasicSnippet for HornerEvaluation {
    fn inputs(&self) -> Vec<(crate::data_type::DataType, String)> {
        vec![
            (
                DataType::Array(Box::new(ArrayType {
                    element_type: DataType::Xfe,
                    length: self.num_coefficients,
                })),
                "*coefficients".to_string(),
            ),
            (DataType::Xfe, "indeterminate".to_string()),
        ]
    }

    fn outputs(&self) -> Vec<(crate::data_type::DataType, String)> {
        vec![(DataType::Xfe, "value".to_string())]
    }

    fn entrypoint(&self) -> String {
        format!(
            "tasmlib_array_horner_evaluation_with_{}_coefficients",
            self.num_coefficients
        )
    }

    fn code(
        &self,
        _library: &mut crate::library::Library,
    ) -> Vec<triton_vm::prelude::LabelledInstruction> {
        let entrypoint = self.entrypoint();

        let update_running_evaluation = triton_asm! {
            // BEFORE: _ *coefficients_end [x] [v]
            // AFTER : _ [vx+c]
            dup 5 dup 5 dup 5           // _ *coefficients_end [x] [v] [x]
            xx_mul                       // _ *coefficients_end [x] [vx]
            dup 6                       // _ *coefficients_end [x] [vx] *coefficients_end
            read_mem 3                  // _ *coefficients_end [x] [vx] [c] *coefficients_end+3
            swap 10                     // _ *coefficients_end-3 [x] [vx] [c] *coefficients_end
            pop 1                       // _ *coefficients_end-3 [x] [vx] [c]
            xx_add                       // _ *coefficients_end-3 [x] [vc+c]
        };

        let update_running_evaluation_for_each_coefficient = (0..self.num_coefficients)
            .flat_map(|_| update_running_evaluation.clone())
            .collect_vec();

        let jump_to_end = self.num_coefficients as isize * 3 - 1;

        triton_asm! {
            // BEFORE: _ *coefficients x2 x1 x0
            // AFTER: _ v2 v1 v0
            {entrypoint}:
                // point to the last element of the array
                swap 3
                push {jump_to_end} add
                swap 3

                // push initial value of running evaluation
                push 0 push 0 push 0    // _ *coefficients_end [x] [v]

                // update running evaluation {num_coefficients_end}-many times
                {&update_running_evaluation_for_each_coefficient}
                                        // _ *coefficients_end-3n [x] [v']

                // clean up stack
                                        // _ *coefficients_end-3n x2 x1 x0 v2 v1 v0
                swap 4 pop 1            // _ *coefficients_end-3n x2 v0 x0 v2 v1
                swap 4 pop 1            // _ *coefficients_end-3n v1 v0 x0 v2
                swap 4 pop 1            // _ v2 v1 v0 x0
                pop 1                   // _ v2 v1 v0
                return
        }
    }
}

#[cfg(test)]
mod test {
    use std::collections::HashMap;

    use num::Zero;
    use rand::prelude::*;
    use triton_vm::twenty_first::prelude::*;

    use crate::empty_stack;
    use crate::rust_shadowing_helper_functions::array::array_get;
    use crate::rust_shadowing_helper_functions::array::insert_as_array;
    use crate::traits::function::Function;
    use crate::traits::function::FunctionInitialState;
    use crate::traits::function::ShadowedFunction;
    use crate::traits::rust_shadow::RustShadow;

    use super::*;

    impl Function for HornerEvaluation {
        fn rust_shadow(
            &self,
            stack: &mut Vec<triton_vm::prelude::BFieldElement>,
            memory: &mut std::collections::HashMap<
                triton_vm::prelude::BFieldElement,
                triton_vm::prelude::BFieldElement,
            >,
        ) {
            // read indeterminate
            let x = XFieldElement::new([
                stack.pop().unwrap(),
                stack.pop().unwrap(),
                stack.pop().unwrap(),
            ]);

            // read location of array
            let pointer = stack.pop().unwrap();

            // read array of coefficients
            let coefficients = (0..self.num_coefficients)
                .map(|i| array_get(pointer, i, memory, 3))
                .map(|bfes| XFieldElement::new(bfes.try_into().unwrap()))
                .collect_vec();

            // evaluate polynomial using Horner's method
            let mut running_evaluation = XFieldElement::zero();
            for c in coefficients.into_iter().rev() {
                running_evaluation *= x;
                running_evaluation += c;
            }

            // push value to stack
            let mut value = running_evaluation.coefficients.to_vec();
            stack.push(value.pop().unwrap());
            stack.push(value.pop().unwrap());
            stack.push(value.pop().unwrap());
        }

        fn pseudorandom_initial_state(
            &self,
            seed: [u8; 32],
            _bench_case: Option<crate::snippet_bencher::BenchmarkCase>,
        ) -> crate::traits::function::FunctionInitialState {
            let mut rng: StdRng = SeedableRng::from_seed(seed);

            // sample coefficients
            let coefficients = (0..self.num_coefficients)
                .map(|_| rng.gen::<XFieldElement>())
                .collect_vec();

            // sample address
            let address = BFieldElement::new(rng.next_u64() % (1 << 30));
            println!("address: {}", address.value());

            // store coefficients
            let mut memory: HashMap<BFieldElement, BFieldElement> = HashMap::new();
            insert_as_array(address, &mut memory, coefficients);

            // sample indeterminate
            let x: XFieldElement = rng.gen();

            // prepare stack
            let mut stack = empty_stack();
            stack.push(address);
            stack.push(x.coefficients[2]);
            stack.push(x.coefficients[1]);
            stack.push(x.coefficients[0]);

            FunctionInitialState { stack, memory }
        }
    }

    #[test]
    fn horner_evaluation() {
        for n in [0, 1, 20, 587, 1000] {
            let horner = HornerEvaluation {
                num_coefficients: n,
            };
            ShadowedFunction::new(horner).test();
        }
    }
}

#[cfg(test)]
mod benches {
    use crate::traits::function::ShadowedFunction;
    use crate::traits::rust_shadow::RustShadow;

    use super::*;

    #[test]
    fn horner_evaluation_100() {
        ShadowedFunction::new(HornerEvaluation {
            num_coefficients: 100,
        })
        .bench();
    }

    #[test]
    fn horner_evaluation_587() {
        ShadowedFunction::new(HornerEvaluation {
            num_coefficients: 587,
        })
        .bench();
    }
}
