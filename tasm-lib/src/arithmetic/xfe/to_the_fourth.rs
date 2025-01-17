use std::collections::HashMap;
use triton_vm::prelude::*;

use crate::prelude::*;
use crate::traits::basic_snippet::Reviewer;
use crate::traits::basic_snippet::SignOffFingerprint;

/// Take an [extension field element](XFieldElement) to the fourth power.
///
/// ### Behavior
///
/// ```text
/// BEFORE: _ [arg; 3]
/// AFTER:  _ [result; 3]
/// ```
///
/// ### Preconditions
///
/// None.
///
/// ### Postconditions
///
/// - the result is the argument raised to the fourth power
#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub struct ToTheFourth;

impl BasicSnippet for ToTheFourth {
    fn inputs(&self) -> Vec<(DataType, String)> {
        vec![(DataType::Xfe, "input".to_owned())]
    }

    fn outputs(&self) -> Vec<(DataType, String)> {
        vec![(DataType::Xfe, "result".to_owned())]
    }

    fn entrypoint(&self) -> String {
        "tasmlib_arithmetic_xfe_to_the_fourth".to_owned()
    }

    fn code(&self, _: &mut Library) -> Vec<LabelledInstruction> {
        triton_asm!(
            {self.entrypoint()}:
                dup 2
                dup 2
                dup 2
                xx_mul

                dup 2
                dup 2
                dup 2
                xx_mul

                return
        )
    }

    fn sign_offs(&self) -> HashMap<Reviewer, SignOffFingerprint> {
        let mut sign_offs = HashMap::new();
        sign_offs.insert(Reviewer("ferdinand"), 0x7277edad6da06ec.into());
        sign_offs
    }
}

#[cfg(test)]
mod tests {
    use twenty_first::math::traits::ModPowU32;

    use super::*;
    use crate::arithmetic::xfe::mod_pow_u32_generic::XfeModPowU32Generic;
    use crate::test_prelude::*;

    impl Closure for ToTheFourth {
        type Args = XFieldElement;

        fn rust_shadow(&self, stack: &mut Vec<BFieldElement>) {
            let arg = pop_encodable::<Self::Args>(stack);
            push_encodable(stack, &arg.mod_pow_u32(4));
        }

        fn pseudorandom_args(&self, seed: [u8; 32], _: Option<BenchmarkCase>) -> Self::Args {
            StdRng::from_seed(seed).gen()
        }

        fn corner_case_args(&self) -> Vec<Self::Args> {
            let max = xfe!([-1, -1, -1]);

            xfe_vec![0, 1, max]
        }
    }

    #[test]
    fn to_the_fourth_xfe_pbt() {
        ShadowedClosure::new(ToTheFourth).test();
    }

    #[proptest]
    fn compare_to_generic_pow_u32(#[strategy(arb())] input: XFieldElement) {
        let final_state_to_fourth = test_rust_equivalence_given_complete_state(
            &ShadowedClosure::new(ToTheFourth),
            &ToTheFourth.set_up_test_stack(input),
            &[],
            &NonDeterminism::default(),
            &None,
            None,
        );

        let final_state_generic = test_rust_equivalence_given_complete_state(
            &ShadowedClosure::new(XfeModPowU32Generic),
            &XfeModPowU32Generic.set_up_test_stack((4, input)),
            &[],
            &NonDeterminism::default(),
            &None,
            None,
        );

        prop_assert_eq!(19, final_state_generic.op_stack.len());
        prop_assert_eq!(19, final_state_to_fourth.op_stack.len());
        prop_assert_eq!(
            &final_state_generic.op_stack.stack[16..=18],
            &final_state_to_fourth.op_stack.stack[16..=18]
        );
    }
}

#[cfg(test)]
mod benches {
    use super::*;
    use crate::test_prelude::*;

    #[test]
    fn benchmark() {
        ShadowedClosure::new(ToTheFourth).bench();
    }
}
