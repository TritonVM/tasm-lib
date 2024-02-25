use triton_vm::prelude::*;

use crate::data_type::DataType;
use crate::library::Library;
use crate::traits::basic_snippet::BasicSnippet;

pub struct ToTheFourth;

impl BasicSnippet for ToTheFourth {
    fn inputs(&self) -> Vec<(DataType, String)> {
        vec![(DataType::Xfe, "input".to_owned())]
    }

    fn outputs(&self) -> Vec<(DataType, String)> {
        vec![(DataType::Xfe, "result".to_owned())]
    }

    fn entrypoint(&self) -> String {
        "tasm_arithmetic_xfe_to_the_fourth".to_owned()
    }

    fn code(&self, _library: &mut Library) -> Vec<LabelledInstruction> {
        let entrypoint = self.entrypoint();
        triton_asm!(
            {entrypoint}:
                dup 2
                dup 2
                dup 2
                xxmul

                dup 2
                dup 2
                dup 2
                xxmul

                return
        )
    }
}

#[cfg(test)]
mod tests {
    use itertools::Itertools;
    use num::One;
    use num::Zero;
    use rand::rngs::StdRng;
    use rand::Rng;
    use rand::SeedableRng;
    use triton_vm::twenty_first::shared_math::traits::ModPowU32;
    use triton_vm::twenty_first::shared_math::x_field_element::EXTENSION_DEGREE;

    use super::*;
    use crate::snippet_bencher::BenchmarkCase;
    use crate::traits::closure::Closure;
    use crate::traits::closure::ShadowedClosure;
    use crate::traits::rust_shadow::RustShadow;

    impl ToTheFourth {
        fn setup_init_stack(&self, input_value: XFieldElement) -> Vec<BFieldElement> {
            [
                self.init_stack_for_isolated_run(),
                input_value.encode().into_iter().rev().collect_vec(),
            ]
            .concat()
        }
    }

    impl Closure for ToTheFourth {
        fn rust_shadow(&self, stack: &mut Vec<BFieldElement>) {
            let input = XFieldElement::new([
                stack.pop().unwrap(),
                stack.pop().unwrap(),
                stack.pop().unwrap(),
            ]);
            let result = input.mod_pow_u32(4);
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
    fn to_the_fourth_xfe_pbt() {
        ShadowedClosure::new(ToTheFourth).test()
    }
}

#[cfg(test)]
mod benches {
    use crate::traits::closure::ShadowedClosure;
    use crate::traits::rust_shadow::RustShadow;

    use super::*;

    #[test]
    fn xfe_to_the_fourth_benchmark() {
        ShadowedClosure::new(ToTheFourth).bench();
    }
}
