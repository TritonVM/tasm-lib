use triton_vm::prelude::*;
use triton_vm::twenty_first::math::x_field_element::EXTENSION_DEGREE;

use crate::arithmetic::bfe::primitive_root_of_unity::PrimitiveRootOfUnity;
use crate::data_type::DataType;
use crate::library::Library;
use crate::traits::basic_snippet::BasicSnippet;

/// Use the barycentric Lagrange evaluation formula to extrapolate the codeword
/// to an out-of-domain location.
pub struct BarycentricEvaluation;

impl BasicSnippet for BarycentricEvaluation {
    fn inputs(&self) -> Vec<(DataType, String)> {
        vec![
            (
                DataType::List(Box::new(DataType::Xfe)),
                "codeword".to_owned(),
            ),
            (DataType::Xfe, "indeterminate".to_owned()),
        ]
    }

    fn outputs(&self) -> Vec<(DataType, String)> {
        vec![(DataType::Xfe, "evaluation_result".to_owned())]
    }

    fn entrypoint(&self) -> String {
        "tasmlib_verifier_fri_barycentric_evaluation".to_owned()
    }

    fn code(&self, library: &mut Library) -> Vec<LabelledInstruction> {
        // This snippet is implemented as:
        // fn barycentric_evaluate_local(
        //     codeword: &[XFieldElement],
        //     indeterminate: XFieldElement,
        // ) -> XFieldElement {
        //     let root_order = codeword.len().try_into().unwrap();
        //     let generator = BFieldElement::primitive_root_of_unity(root_order).unwrap();
        //     let mut numerator = xfe!(0);
        //     let mut denominator = xfe!(0);
        //     let mut domain_iter_elem = bfe!(1);
        //     for code_word_elem in codeword {
        //         let domain_shift_elem = indeterminate - domain_iter_elem;
        //         let domain_over_domain_shift_elem = domain_iter_elem.lift() / domain_shift_elem;
        //         denominator += domain_over_domain_shift_elem;
        //         numerator += domain_over_domain_shift_elem * *code_word_elem;
        //         domain_iter_elem *= generator;
        //     }

        //     numerator / denominator
        // }

        let entrypoint = self.entrypoint();
        let loop_label = format!("{entrypoint}_loop");
        let generator = library.import(Box::new(PrimitiveRootOfUnity));

        let swap_top_and_third_xfe = triton_asm!(
            // [xfe1] buf0 buf1 buf 2 [xfe0]
            swap 6
            swap 2
            swap 8
            swap 2
            swap 1
            swap 7
            swap 1
            // [xfe0] buf0 buf1 buf 2 [xfe1]
        );

        let swap_top_two_xfes = triton_asm!(
            // _ y2 y1 y0 x2 x1 x0
            swap 3
            swap 1
            swap 4
            swap 1
            swap 2
            swap 5
            swap 2

            // _ x2 x1 x0 y2 y1 y0
        );

        let loop_over_codeword_elements = triton_asm!(
            // INVARIANT:  *stop_condition [indeterminate] geni geniacc *codeword_elem [denominator] [numerator]
            {loop_label}:
                /* End-condition: `*stop_condition == *codeword_elem` */
                /* Note that `geniacc == 1` could also be used for loop stop-condition but then the
                   loop stop-condition would have to happen at the end of the loop body. */
                dup 12
                dup 7
                eq
                skiz return
                // _  *stop_condition [indeterminate] geni geniacc *codeword_elem [denominator] [numerator]

                /* Loop body:
                1. Put a lifted $ geniacc $ on top of stack
                2. calculate $ indeterminate - geniacc $
                3. Calculate $ 1 / (indeterminate - geniacc) $
                4. Multiply two top stack elements to get $ dodse = geniacc / (indeterminate - geniacc) $
                5. Add this value to $ denominator $ while preserving $ dodse $
                6. Read element from $ codewords $
                7. Multiply this element with $ dodse $ and add to $ numerator $
                8. Get next power of `geniacc`
                */

                /* 1. */
                push 0
                push 0
                dup 9
                // _ *stop_condition [indeterminate] geni geniacc *codeword_elem [denominator] [numerator] [geniacc]

                /* 2. */
                dup 14
                dup 14
                dup 14
                dup 13
                push -1
                mul
                add
                // _ *stop_condition [indeterminate] geni geniacc *codeword_elem [denominator] [numerator] [geniacc] [indeterminate - geniacc]

                /* 3. */
                xinvert
                // _ *stop_condition [indeterminate] geni geniacc *codeword_elem [denominator] [numerator] [geniacc] [1 / (indeterminate - geniacc)]

                /* 4. */
                xxmul
                // _ *stop_condition [indeterminate] geni geniacc *codeword_elem [denominator] [numerator] [geniacc / (indeterminate - geniacc)]
                // _ *stop_condition [indeterminate] geni geniacc *codeword_elem [denominator] [numerator] [dodse] <-- rename

                /* 5. */
                {&swap_top_and_third_xfe}

                // _ *stop_condition [indeterminate] geni geniacc *codeword_elem [dodse] [numerator] [denominator]

                dup 8
                dup 8
                dup 8
                xxadd
                // _ *stop_condition [indeterminate] geni geniacc *codeword_elem [dodse] [numerator] [denominator + dodse]
                // _ *stop_condition [indeterminate] geni geniacc *codeword_elem [dodse] [numerator] [denominator'] <-- rename

                {&swap_top_and_third_xfe}
                // _ *stop_condition [indeterminate] geni geniacc *codeword_elem [denominator'] [numerator] [dodse]

                /* 6. */
                dup 9
                read_mem {EXTENSION_DEGREE}
                swap 13
                pop 1
                // _ *stop_condition [indeterminate] geni geniacc *codeword_elem_prev [denominator'] [numerator] [dodse] [code_word_elem]
                // _ *stop_condition [indeterminate] geni geniacc *codeword_elem' [denominator'] [numerator] [dodse] [code_word_elem]     <-- rename

                /* 7. */
                xxmul
                xxadd
                // _ *stop_condition [indeterminate] geni geniacc *codeword_elem' [denominator'] [numerator + dodse * code_word_elem]
                // _ *stop_condition [indeterminate] geni geniacc *codeword_elem' [denominator'] [numerator']   <-- rename

                /* 8. */
                swap 7
                dup 8
                mul
                swap 7
                // _ *stop_condition [indeterminate] geni (geniacc * geni) *codeword_elem' [denominator'] [numerator']
                // _ *stop_condition [indeterminate] geni geniacc' *codeword_elem' [denominator'] [numerator']   <-- rename

                recurse
        );

        triton_asm!(
            {entrypoint}:
                // _ *codeword [indeterminate]

                /* Get generator */
                dup 3
                read_mem 1
                pop 1
                // _ *codeword [indeterminate] codeword_len

                push 0
                dup 1
                // _ *codeword [indeterminate] codeword_len 0 codeword_len

                /* Notice that this call to `PrimitiveRootOfUnity` will fail if `codeword_len` is not
                   a valid `u32` (exceeds `u32::MAX`) which is desired behavior.
                */
                call {generator}

                /* Invert generator since codeword is traversed high-to-low */
                invert
                // _ *codeword [indeterminate] codeword_len generator^{-1}
                // _ *codeword [indeterminate] len geni <-- rename

                dup 0
                swap 2
                // _ *codeword [indeterminate] geni geni len

                /* Find last word of `codeword` list */
                push {EXTENSION_DEGREE}
                mul
                // _ *codeword [indeterminate] geni geni offset

                dup 6
                add
                // _ *codeword [indeterminate] geni geni *codeword_last_word
                // _ *codeword [indeterminate] geni geniacc *codeword_last_word <-- rename

                push 0
                push 0
                push 0
                push 0
                push 0
                push 0
                // _ *codeword [indeterminate] geni geniacc *codeword_last_word [denominator] [numerator]

                call {loop_label}
                // _ *codeword [indeterminate] geni geniacc *codeword [denominator] [numerator]

                {&swap_top_two_xfes}
                // _ *codeword [indeterminate] geni geniacc *codeword [numerator] [denominator]

                xinvert
                xxmul
                // _ *codeword [indeterminate] geni geniacc *codeword [numerator / denominator]

                swap 7
                pop 1
                swap 7
                pop 1
                swap 7
                // _ [numerator / denominator] i0 geni geniacc *codeword id1

                pop 5
                // _ [numerator / denominator]

                return

                {&loop_over_codeword_elements}
        )
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use itertools::Itertools;
    use rand::rngs::StdRng;
    use rand::Rng;
    use rand::SeedableRng;
    use triton_vm::fri::barycentric_evaluate;
    use triton_vm::twenty_first::math::other::random_elements;
    use triton_vm::twenty_first::xfe_vec;

    use crate::rust_shadowing_helper_functions::list::list_insert;
    use crate::rust_shadowing_helper_functions::list::load_list_with_copy_elements;
    use crate::snippet_bencher::BenchmarkCase;
    use crate::traits::function::Function;
    use crate::traits::function::FunctionInitialState;
    use crate::traits::function::ShadowedFunction;
    use crate::traits::rust_shadow::RustShadow;

    use super::*;

    #[test]
    fn barycentric_evaluation_pbt() {
        ShadowedFunction::new(BarycentricEvaluation).test()
    }

    impl BarycentricEvaluation {
        fn prepare_state(
            &self,
            codeword: Vec<XFieldElement>,
            codeword_pointer: BFieldElement,
            indeterminate: XFieldElement,
        ) -> FunctionInitialState {
            let mut memory = HashMap::default();
            list_insert(codeword_pointer, codeword, &mut memory);

            let mut stack = self.init_stack_for_isolated_run();
            stack.push(codeword_pointer);

            for word in indeterminate.coefficients.into_iter().rev() {
                stack.push(word);
            }

            FunctionInitialState { stack, memory }
        }
    }

    impl Function for BarycentricEvaluation {
        fn rust_shadow(
            &self,
            stack: &mut Vec<BFieldElement>,
            memory: &mut std::collections::HashMap<BFieldElement, BFieldElement>,
        ) {
            let indeterminate = XFieldElement::new([
                stack.pop().unwrap(),
                stack.pop().unwrap(),
                stack.pop().unwrap(),
            ]);
            let codeword_pointer = stack.pop().unwrap();
            let codeword =
                load_list_with_copy_elements::<EXTENSION_DEGREE>(codeword_pointer, memory);
            let codeword: Vec<XFieldElement> = codeword.into_iter().map(|x| x.into()).collect_vec();
            let result = barycentric_evaluate(&codeword, indeterminate);

            for word in result.coefficients.into_iter().rev() {
                stack.push(word);
            }
        }

        fn corner_case_initial_states(&self) -> Vec<FunctionInitialState> {
            let some_indeterminate = XFieldElement::new([bfe!(1555), bfe!(1556), bfe!(1557)]);
            let some_codeword_pointer = bfe!(19191919);
            let codeword_of_length_one =
                self.prepare_state(xfe_vec![155], some_codeword_pointer, some_indeterminate);
            let codeword_of_length_two = self.prepare_state(
                xfe_vec![155, 1_919_191_919],
                some_codeword_pointer,
                some_indeterminate,
            );

            vec![codeword_of_length_one, codeword_of_length_two]
        }

        fn pseudorandom_initial_state(
            &self,
            seed: [u8; 32],
            bench_case: Option<crate::snippet_bencher::BenchmarkCase>,
        ) -> FunctionInitialState {
            let mut rng: StdRng = SeedableRng::from_seed(seed);
            let codeword_length = match bench_case {
                Some(BenchmarkCase::CommonCase) => 256,
                Some(BenchmarkCase::WorstCase) => 512,
                None => 1 << rng.gen_range(0..=14),
            };

            let codeword_pointer = rng.gen_range(0..=(1u64 << 34));
            let codeword_pointer = bfe!(codeword_pointer);
            let indeterminate: XFieldElement = rng.gen();
            let codeword = random_elements(codeword_length);

            self.prepare_state(codeword, codeword_pointer, indeterminate)
        }
    }
}

#[cfg(test)]
mod benches {
    use crate::traits::function::ShadowedFunction;
    use crate::traits::rust_shadow::RustShadow;

    use super::*;

    #[test]
    fn bench_barycentric_evaluation() {
        ShadowedFunction::new(BarycentricEvaluation).bench();
    }
}
