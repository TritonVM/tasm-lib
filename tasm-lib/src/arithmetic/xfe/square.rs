use triton_vm::prelude::*;

use crate::prelude::*;

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub struct Square;

impl BasicSnippet for Square {
    fn inputs(&self) -> Vec<(DataType, String)> {
        vec![(DataType::Xfe, "input".to_owned())]
    }

    fn outputs(&self) -> Vec<(DataType, String)> {
        vec![(DataType::Xfe, "result".to_owned())]
    }

    fn entrypoint(&self) -> String {
        "tasmlib_arithmetic_xfe_square".to_owned()
    }

    fn code(&self, _library: &mut Library) -> Vec<LabelledInstruction> {
        let entrypoint = self.entrypoint();
        triton_asm!(
            {entrypoint}:
                dup 2
                dup 2
                dup 2
                xx_mul

                return
        )
    }
}

#[cfg(test)]
mod tests {
    use twenty_first::math::x_field_element::EXTENSION_DEGREE;

    use super::*;
    use crate::arithmetic::xfe::mod_pow_u32_generic::XfeModPowU32Generic;
    use crate::test_prelude::*;

    impl Closure for Square {
        type Args = XFieldElement;

        fn rust_shadow(&self, stack: &mut Vec<BFieldElement>) {
            let input = pop_encodable::<Self::Args>(stack);
            push_encodable(stack, &input.square());
        }

        fn pseudorandom_args(&self, seed: [u8; 32], _: Option<BenchmarkCase>) -> Self::Args {
            StdRng::from_seed(seed).gen()
        }

        fn corner_case_args(&self) -> Vec<Self::Args> {
            let max = xfe!([-1; EXTENSION_DEGREE]);

            xfe_vec![0, 1, max]
        }
    }

    #[test]
    fn square_xfe_pbt() {
        ShadowedClosure::new(Square).test()
    }

    #[test]
    fn compare_to_generic_pow_u32() {
        // Run `cube` snippet
        let input: XFieldElement = random();
        let init_stack_to_fourth = [
            Square.init_stack_for_isolated_run(),
            input.coefficients.into_iter().rev().collect_vec(),
        ]
        .concat();
        let final_state_from_to_fourth = test_rust_equivalence_given_complete_state(
            &ShadowedClosure::new(Square),
            &init_stack_to_fourth,
            &[],
            &NonDeterminism::default(),
            &None,
            None,
        );

        // Run snippet for generic pow
        let init_stack_to_generic = [
            XfeModPowU32Generic.init_stack_for_isolated_run(),
            vec![BFieldElement::new(2)],
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
    use super::*;
    use crate::test_prelude::*;

    #[test]
    fn benchmark() {
        ShadowedClosure::new(Square).bench();
    }
}
