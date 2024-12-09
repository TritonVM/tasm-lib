use std::collections::HashMap;
use triton_vm::prelude::*;

use crate::data_type::DataType;
use crate::library::Library;
use crate::traits::basic_snippet::BasicSnippet;
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
    use rand::prelude::*;
    use triton_vm::twenty_first::math::traits::ModPowU32;

    use super::*;
    use crate::arithmetic::xfe::mod_pow_u32_generic::XfeModPowU32Generic;
    use crate::pop_encodable;
    use crate::push_encodable;
    use crate::snippet_bencher::BenchmarkCase;
    use crate::test_helpers::test_rust_equivalence_given_complete_state;
    use crate::traits::closure::Closure;
    use crate::traits::closure::ShadowedClosure;
    use crate::traits::rust_shadow::RustShadow;

    impl ToTheFourth {
        fn setup_init_stack(&self, arg: XFieldElement) -> Vec<BFieldElement> {
            let mut stack = self.init_stack_for_isolated_run();
            push_encodable(&mut stack, &arg);

            stack
        }
    }

    impl Closure for ToTheFourth {
        fn rust_shadow(&self, stack: &mut Vec<BFieldElement>) {
            let arg = pop_encodable::<XFieldElement>(stack);
            push_encodable(stack, &arg.mod_pow_u32(4));
        }

        fn pseudorandom_initial_state(
            &self,
            seed: [u8; 32],
            _: Option<BenchmarkCase>,
        ) -> Vec<BFieldElement> {
            self.setup_init_stack(StdRng::from_seed(seed).gen())
        }

        fn corner_case_initial_states(&self) -> Vec<Vec<BFieldElement>> {
            let bfe_max = BFieldElement::MAX;
            let xfe_max = xfe!([bfe_max, bfe_max, bfe_max]);

            xfe_array![0, 1, xfe_max]
                .map(|arg| self.setup_init_stack(arg))
                .to_vec()
        }
    }

    #[test]
    fn to_the_fourth_xfe_pbt() {
        ShadowedClosure::new(ToTheFourth).test();
    }

    #[test]
    fn compare_to_generic_pow_u32() {
        let input = random();

        let final_state_from_to_fourth = test_rust_equivalence_given_complete_state(
            &ShadowedClosure::new(ToTheFourth),
            &ToTheFourth.setup_init_stack(input),
            &[],
            &NonDeterminism::default(),
            &None,
            None,
        );

        let final_state_from_generic = test_rust_equivalence_given_complete_state(
            &ShadowedClosure::new(XfeModPowU32Generic),
            &XfeModPowU32Generic.prepare_state(input, 4),
            &[],
            &NonDeterminism::default(),
            &None,
            None,
        );

        assert_eq!(
            final_state_from_generic.op_stack.len(),
            final_state_from_to_fourth.op_stack.len(),
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
    use crate::traits::closure::ShadowedClosure;
    use crate::traits::rust_shadow::RustShadow;

    #[test]
    fn xfe_to_the_fourth_benchmark() {
        ShadowedClosure::new(ToTheFourth).bench();
    }
}
