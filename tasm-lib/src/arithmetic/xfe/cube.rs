use triton_vm::prelude::*;

use crate::data_type::DataType;
use crate::library::Library;
use crate::traits::basic_snippet::BasicSnippet;

pub struct Cube;

impl BasicSnippet for Cube {
    fn inputs(&self) -> Vec<(DataType, String)> {
        vec![(DataType::Xfe, "input".to_owned())]
    }

    fn outputs(&self) -> Vec<(DataType, String)> {
        vec![(DataType::Xfe, "result".to_owned())]
    }

    fn entrypoint(&self) -> String {
        "tasmlib_arithmetic_xfe_cube".to_owned()
    }

    fn code(&self, _library: &mut Library) -> Vec<LabelledInstruction> {
        let entrypoint = self.entrypoint();
        triton_asm!(
            {entrypoint}:
                dup 2
                dup 2
                dup 2
                dup 2
                dup 2
                dup 2
                xx_mul
                xx_mul

                return
        )
    }
}

#[cfg(test)]
mod tests {
    use itertools::Itertools;
    use num::One;
    use num::Zero;
    use rand::random;
    use rand::rngs::StdRng;
    use rand::Rng;
    use rand::SeedableRng;
    use triton_vm::twenty_first::math::traits::ModPowU32;
    use triton_vm::twenty_first::math::x_field_element::EXTENSION_DEGREE;

    use super::*;
    use crate::arithmetic::xfe::mod_pow_u32_generic::XfeModPowU32Generic;
    use crate::snippet_bencher::BenchmarkCase;
    use crate::test_helpers::test_rust_equivalence_given_complete_state;
    use crate::traits::closure::Closure;
    use crate::traits::closure::ShadowedClosure;
    use crate::traits::rust_shadow::RustShadow;

    impl Cube {
        fn setup_init_stack(&self, input_value: XFieldElement) -> Vec<BFieldElement> {
            [
                self.init_stack_for_isolated_run(),
                input_value.encode().into_iter().rev().collect_vec(),
            ]
            .concat()
        }
    }

    impl Closure for Cube {
        fn rust_shadow(&self, stack: &mut Vec<BFieldElement>) {
            let input = XFieldElement::new([
                stack.pop().unwrap(),
                stack.pop().unwrap(),
                stack.pop().unwrap(),
            ]);
            let result = input.mod_pow_u32(3);
            for word in result.encode().into_iter().rev() {
                stack.push(word)
            }
        }

        fn pseudorandom_initial_state(
            &self,
            seed: [u8; 32],
            _bench_case: Option<BenchmarkCase>,
        ) -> Vec<BFieldElement> {
            let mut rng: StdRng = SeedableRng::from_seed(seed);
            let random_input: XFieldElement = rng.gen();

            self.setup_init_stack(random_input)
        }

        fn corner_case_initial_states(&self) -> Vec<Vec<BFieldElement>> {
            let zero = self.setup_init_stack(XFieldElement::zero());
            let one = self.setup_init_stack(XFieldElement::one());

            let max_bfe = BFieldElement::new(BFieldElement::MAX);
            let max_max_max =
                self.setup_init_stack(XFieldElement::new([max_bfe; EXTENSION_DEGREE]));

            vec![zero, one, max_max_max]
        }
    }

    #[test]
    fn cube_xfe_pbt() {
        ShadowedClosure::new(Cube).test()
    }

    #[test]
    fn compare_to_generic_pow_u32() {
        // Run `cube` snippet
        let input: XFieldElement = random();
        let init_stack_to_fourth = [
            Cube.init_stack_for_isolated_run(),
            input.coefficients.into_iter().rev().collect_vec(),
        ]
        .concat();
        let final_state_from_to_fourth = test_rust_equivalence_given_complete_state(
            &ShadowedClosure::new(Cube),
            &init_stack_to_fourth,
            &[],
            &NonDeterminism::default(),
            &None,
            None,
        );

        // Run snippet for generic pow
        let init_stack_to_generic = [
            XfeModPowU32Generic.init_stack_for_isolated_run(),
            vec![BFieldElement::new(3)],
            input.coefficients.into_iter().rev().collect_vec(),
        ]
        .concat();
        let final_state_from_generic = test_rust_equivalence_given_complete_state(
            &ShadowedClosure::new(XfeModPowU32Generic),
            &init_stack_to_generic,
            &[],
            &NonDeterminism::default(),
            &None,
            None,
        );

        // Assert that height agrees, and the top-3 elements agree
        assert_eq!(
            final_state_from_generic.op_stack.stack.len(),
            final_state_from_to_fourth.op_stack.stack.len()
        );
        assert_eq!(
            final_state_from_generic.op_stack.stack[16..=18],
            final_state_from_to_fourth.op_stack.stack[16..=18],
        );
    }
}

#[cfg(test)]
mod benches {
    use crate::traits::closure::ShadowedClosure;
    use crate::traits::rust_shadow::RustShadow;

    use super::*;

    #[test]
    fn xfe_cube_benchmark() {
        ShadowedClosure::new(Cube).bench();
    }
}
