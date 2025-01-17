use triton_vm::prelude::*;

use crate::prelude::*;

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
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

    fn code(&self, _: &mut Library) -> Vec<LabelledInstruction> {
        triton_asm!(
            {self.entrypoint()}:
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
    use twenty_first::math::traits::ModPowU32;

    use super::*;
    use crate::arithmetic::xfe::mod_pow_u32_generic::XfeModPowU32Generic;
    use crate::test_prelude::*;

    impl Closure for Cube {
        type Args = XFieldElement;

        fn rust_shadow(&self, stack: &mut Vec<BFieldElement>) {
            let input = pop_encodable::<Self::Args>(stack);
            push_encodable(stack, &input.mod_pow_u32(3));
        }

        fn pseudorandom_args(&self, seed: [u8; 32], _: Option<BenchmarkCase>) -> Self::Args {
            StdRng::from_seed(seed).gen()
        }

        fn corner_case_args(&self) -> Vec<Self::Args> {
            let max = xfe!([-1; 3]);
            xfe_vec![0, 1, max]
        }
    }

    #[test]
    fn rust_shadow() {
        ShadowedClosure::new(Cube).test()
    }

    #[proptest]
    fn compare_to_generic_pow_u32(#[strategy(arb())] input: XFieldElement) {
        let final_state_cubed = test_rust_equivalence_given_complete_state(
            &ShadowedClosure::new(Cube),
            &Cube.set_up_test_stack(input),
            &[],
            &NonDeterminism::default(),
            &None,
            None,
        );

        let final_state_generic = test_rust_equivalence_given_complete_state(
            &ShadowedClosure::new(XfeModPowU32Generic),
            &XfeModPowU32Generic.set_up_test_stack((3, input)),
            &[],
            &NonDeterminism::default(),
            &None,
            None,
        );

        prop_assert_eq!(19, final_state_generic.op_stack.stack.len());
        prop_assert_eq!(19, final_state_cubed.op_stack.stack.len());
        prop_assert_eq!(
            &final_state_generic.op_stack.stack[16..=18],
            &final_state_cubed.op_stack.stack[16..=18]
        );
    }
}

#[cfg(test)]
mod benches {
    use super::*;
    use crate::test_prelude::*;

    #[test]
    fn benchmark() {
        ShadowedClosure::new(Cube).bench();
    }
}
