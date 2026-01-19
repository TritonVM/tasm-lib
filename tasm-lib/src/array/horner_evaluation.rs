use std::collections::HashMap;

use itertools::Itertools;
use triton_vm::prelude::*;

use crate::data_type::ArrayType;
use crate::prelude::*;
use crate::traits::basic_snippet::Reviewer;
use crate::traits::basic_snippet::SignOffFingerprint;

/// Evaluate a polynomial in a point using the Horner method.
///
/// `HornerEvaluation` takes an array of coefficients (representing a
/// polynomial) and a scalar (representing an indeterminate) and computes the
/// value of the polynomial in that point. It can be used for univariate
/// batching, whereby the object is to compute a random linear sum of a given
/// set of points, and the weights are given by the powers of one challenge.
///
/// ### Behavior
///
/// ```text
/// BEFORE: _ *coefficients [indeterminate: XFieldElement]
/// AFTER:  _ [evaluation: XFieldElement]
/// ```
///
/// ### Preconditions
///
/// None.
///
/// ### Postconditions
///
/// None.
#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub struct HornerEvaluation {
    pub num_coefficients: usize,
}

impl HornerEvaluation {
    pub fn new(num_coefficients: usize) -> Self {
        Self { num_coefficients }
    }
}

impl BasicSnippet for HornerEvaluation {
    fn parameters(&self) -> Vec<(DataType, String)> {
        let coefficients_ty = DataType::Array(Box::new(ArrayType {
            element_type: DataType::Xfe,
            length: self.num_coefficients,
        }));

        vec![
            (coefficients_ty, "*coefficients".to_string()),
            (DataType::Xfe, "indeterminate".to_string()),
        ]
    }

    fn return_values(&self) -> Vec<(DataType, String)> {
        vec![(DataType::Xfe, "evaluation".to_string())]
    }

    fn entrypoint(&self) -> String {
        let n = self.num_coefficients;
        format!("tasmlib_array_horner_evaluation_with_{n}_coefficients")
    }

    fn code(&self, _: &mut Library) -> Vec<LabelledInstruction> {
        let update_running_evaluation = triton_asm! {
            // BEFORE: _ *coefficients_end   [x: XFE] [v: XFE]
            // AFTER : _ *coefficients_end-3 [x: XFE] [vx+c: XFE]
            dup 5 dup 5 dup 5           // _ *coefficients_end [x] [v] [x]
            xx_mul                      // _ *coefficients_end [x] [vx]
            pick 6                      // _ [x] [vx] *coefficients_end
            read_mem 3                  // _ [x] [vx] [c] *coefficients_end-3
            place 9                     // _ *coefficients_end-3 [x] [vx] [c]
            xx_add                      // _ *coefficients_end-3 [x] [vx+c]
        };
        let update_running_evaluation_for_each_coefficient = (0..self.num_coefficients)
            .flat_map(|_| update_running_evaluation.clone())
            .collect_vec();

        triton_asm! {
            // BEFORE: _ *coefficients [x: XFE]
            // AFTER:  _ [v: XFE]
            {self.entrypoint()}:
                pick 3                  // _ [x: XFE] *coefficients
                addi {(self.num_coefficients * 3).saturating_sub(1)}
                place 3                 // _ *coefficients_end [x: XFE]
                push 0 push 0 push 0    // _ *coefficients_end [x: XFE] [v: XFE]
                {&update_running_evaluation_for_each_coefficient}
                                        // _ *coefficients_end-3n [x: XFE] [v': XFE]
                place 6 place 6 place 6 // _ [v': XFE] *coefficients_end-3n [x: XFE]
                pop 4                   // _ [v': XFE]
                return
        }
    }

    fn sign_offs(&self) -> HashMap<Reviewer, SignOffFingerprint> {
        let mut sign_offs = HashMap::new();
        if self.num_coefficients == 4 {
            sign_offs.insert(Reviewer("ferdinand"), 0xd6cdaea8d7f5385a.into());
        }

        sign_offs
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::rust_shadowing_helper_functions::array::array_get;
    use crate::rust_shadowing_helper_functions::array::insert_as_array;
    use crate::test_prelude::*;
    use crate::twenty_first::prelude::Polynomial;

    impl Accessor for HornerEvaluation {
        fn rust_shadow(
            &self,
            stack: &mut Vec<BFieldElement>,
            memory: &HashMap<BFieldElement, BFieldElement>,
        ) {
            let indeterminate = pop_encodable(stack);
            let coefficient_ptr = stack.pop().unwrap();

            let coefficients = (0..self.num_coefficients)
                .map(|i| array_get(coefficient_ptr, i, memory, 3))
                .map(|bfes| XFieldElement::new(bfes.try_into().unwrap()))
                .collect();
            let polynomial = Polynomial::new(coefficients);
            let evaluation = polynomial.evaluate_in_same_field(indeterminate);

            push_encodable(stack, &evaluation);
        }

        fn pseudorandom_initial_state(
            &self,
            seed: [u8; 32],
            _: Option<BenchmarkCase>,
        ) -> AccessorInitialState {
            let mut rng = StdRng::from_seed(seed);

            let coefficients = (0..self.num_coefficients)
                .map(|_| rng.random::<XFieldElement>())
                .collect();
            let address = rng.random();

            let mut memory = HashMap::new();
            insert_as_array(address, &mut memory, coefficients);

            let mut stack = self.init_stack_for_isolated_run();
            stack.push(address);
            push_encodable(&mut stack, &rng.random::<XFieldElement>()); // indeterminate

            AccessorInitialState { stack, memory }
        }
    }

    #[test]
    fn rust_shadow() {
        for n in [0, 1, 4, 20, 587, 1000] {
            ShadowedAccessor::new(HornerEvaluation::new(n)).test();
        }
    }
}

#[cfg(test)]
mod benches {
    use super::*;
    use crate::test_prelude::*;

    #[test]
    fn bench() {
        for n in [100, 587] {
            ShadowedAccessor::new(HornerEvaluation::new(n)).bench();
        }
    }
}
