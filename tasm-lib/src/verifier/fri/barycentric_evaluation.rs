use triton_vm::prelude::*;
use triton_vm::twenty_first::math::x_field_element::EXTENSION_DEGREE;

use crate::arithmetic::bfe::primitive_root_of_unity::PrimitiveRootOfUnity;
use crate::data_type::DataType;
use crate::library::Library;
use crate::traits::basic_snippet::BasicSnippet;

const MAX_CODEWORD_LENGTH: u32 = 1 << 15;

/// Use the barycentric Lagrange evaluation formula to extrapolate the codeword
/// to an out-of-domain location. Codeword must have a length that is a power
/// of 2 and may not exceed `MAX_CODEWORD_LENGTH`.
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
        let entrypoint = self.entrypoint();
        let generator = library.import(Box::new(PrimitiveRootOfUnity));
        let partial_terms_pointer = library.kmalloc(MAX_CODEWORD_LENGTH * EXTENSION_DEGREE as u32);
        let partial_terms_pointer_minus_one = partial_terms_pointer - bfe!(1);

        // Partial terms are of the form $ g^i / (g^i - indeterminate) $
        let calculate_and_store_partial_terms_loop_label =
            format!("{entrypoint}_partial_terms_loop");
        let calculate_and_store_partial_terms_code = triton_asm!(
            // BEGIN:     _ *partial_terms_end_cond *partial_terms[0] generator [-indeterminate] 1
            // INVARIANT: _ *partial_terms_end_cond *partial_terms[n] generator [-indeterminate] generator_acc
            {calculate_and_store_partial_terms_loop_label}:
                dup 3
                dup 3
                dup 3
                dup 3
                add
                // _ *partial_terms_end_cond *partial_terms generator [-indeterminate] generator_acc [generator_acc-indeterminate]

                x_invert
                // _ *partial_terms_end_cond *partial_terms generator [-indeterminate] generator_acc [1/(generator_acc-indeterminate)]

                dup 3
                xb_mul
                // _ *partial_terms_end_cond *partial_terms generator [-indeterminate] generator_acc [generator_acc/(generator_acc-indeterminate)]

                dup 8
                write_mem {EXTENSION_DEGREE}
                swap 6
                pop 1
                // _ *partial_terms_end_cond *partial_terms' generator [-indeterminate] generator_acc

                dup 4
                mul
                // _ *partial_terms_end_cond *partial_terms' generator [-indeterminate] generator_acc'

                recurse_or_return
        );

        // The numerator is the sum of codeword[n] * partial_terms[n], i.e. the
        // inner product of the codeword and the partial terms.
        let numerator_from_partial_sums_loop_label =
            format!("{entrypoint}_numerator_from_partial_sums");
        let numerator_from_partial_sums_loop_code = triton_asm!(
            // BEGIN:     _ *ptec *partial_terms[0] [0]   *codeword[0] *partial_terms[0]
            // INVARIANT: _ *ptec *partial_terms[n] [acc] *codeword[n] *partial_terms[n]
            {numerator_from_partial_sums_loop_label}:
                // _ *ptec *partial_terms[n] [acc] *codeword[n] *partial_terms[n]

                xx_dot_step
                // _ *ptec *partial_terms[n] [acc'] *codeword[n'] *partial_terms[n']

                dup 0
                swap 6
                pop 1
                // _ *ptec *partial_terms[n'] [acc'] *codeword[n'] *partial_terms[n']

                recurse_or_return
        );

        // The denominator is the sum of all terms in `partial_terms`
        let denominator_from_partial_sums_loop_label =
            format!("{entrypoint}_denominator_from_partial_sums");
        let denominator_from_partial_sums_loop_code = triton_asm!(
            // START:     _ (*partial_terms-1) *partial_terms_last_word 0 0 [0]
            // INVARIANT: _ (*partial_terms-1) *partial_terms[n]        0 0 [acc]
            {denominator_from_partial_sums_loop_label}:
                // _ (*partial_terms-1) *partial_terms[n] 0 0 [acc]

                dup 5
                read_mem {EXTENSION_DEGREE}
                // _ (*partial_terms-1) *partial_terms[n] 0 0 [acc] [term] *partial_terms[n-1]

                swap 9
                pop 1
                // _ (*partial_terms-1) *partial_terms[n-1] 0 0 [acc] [term]

                xx_add
                // _ (*partial_terms-1) *partial_terms[n-1] 0 0 [acc']

                recurse_or_return
        );

        triton_asm!(
            {entrypoint}:
                // _ *codeword [indeterminate]

                push -1
                xb_mul
                hint neg_indeterminate = stack[0..3]
                // _ *codeword [-indeterminate]

                /* Prepare stack for call to partial terms' loop */
                dup 3
                read_mem 1
                pop 1
                // _ *codeword [-indeterminate] codeword_len

                /* assert `codeword_len <= MAX_CODEWORD_LENGTH`` */
                push {MAX_CODEWORD_LENGTH + 1}
                dup 1
                lt
                assert
                // _ *codeword [-indeterminate] codeword_len

                push 0
                dup 1
                // _ *codeword [-indeterminate] codeword_len [codeword_len; as u64]

                /* This call to get the generator will fail if codeword_len
                   exceeds u32::MAX, or, if it is not a power of 2. Which is
                   desired behavior. */
                call {generator}
                hint generator: BFieldElement = stack[0]
                // _ *codeword [-indeterminate] codeword_len generator

                swap 1
                // _ *codeword [-indeterminate] generator codeword_len

                push {EXTENSION_DEGREE}
                mul
                push {partial_terms_pointer}
                add
                // _ *codeword [-indeterminate]  generator (*partial_terms_last_word+1)
                // _ *codeword (-i2) (-i1) (-i0) generator (*partial_terms_last_word+1) <-- rename

                swap 4
                // _ *codeword (*partial_terms_last_word+1) (-i1) (-i0) generator (-i2)

                push {partial_terms_pointer}
                swap 4
                // _ *codeword (*partial_terms_last_word+1) *partial_terms[0] (-i0) generator (-i2) (-i1)

                swap 1
                swap 2
                swap 3
                // _ *codeword (*partial_terms_last_word+1) *partial_terms[0] generator (-i2) (-i1) (-i0)
                // _ *codeword (*partial_terms_last_word+1) *partial_terms[0] generator [-indeterminate] <-- rename

                push 1
                // _ *codeword (*partial_terms_last_word+1) *partial_terms[0] generator [-indeterminate] generator_acc

                call {calculate_and_store_partial_terms_loop_label}
                // _ *codeword (*partial_terms_last_word+1) (*partial_terms_last_word+1) generator [-indeterminate] generator_acc

                pop 5
                pop 1
                // _ *codeword (*partial_terms_last_word+1)

                swap 1
                // _ (*partial_terms_last_word+1) *codeword

                push 1
                add
                // _ (*partial_terms_last_word+1) *codeword[0]

                push 0
                push 0
                push 0
                // _ (*partial_terms_last_word+1) *codeword[0] [acc]

                push {partial_terms_pointer}
                swap 4
                // _ (*partial_terms_last_word+1) *partial_terms[0] [acc] *codeword[0]

                push {partial_terms_pointer}
                // _ (*partial_terms_last_word+1) *partial_terms[0] [acc] *codeword[0] *partial_terms[0]

                call {numerator_from_partial_sums_loop_label}
                // _ (*partial_terms_last_word+1) (*partial_terms_last_word+1) [numerator] *codeword[last + 1] (*partial_terms_last_word+1)

                pop 2
                // _ (*partial_terms_last_word+1) (*partial_terms_last_word+1) [numerator]
                hint numerator: XFieldElement = stack[0..3]

                // _ (*partial_terms_last_word+1) (*partial_terms_last_word+1) n2 n1 n0    <-- rename


                swap 2
                swap 4
                // _ n2 (*partial_terms_last_word+1) n0 n1 (*partial_terms_last_word+1)

                push -1
                add
                // _ n2 (*partial_terms_last_word+1) n0 n1 *partial_terms_last_word

                push {partial_terms_pointer_minus_one}
                // _ n2 (*partial_terms_last_word+1) n0 n1 *partial_terms_last_word (*partial_terms - 1)

                swap 2
                swap 4
                pop 1
                // _ n2 n1 n0 (*partial_terms - 1) *partial_terms_last_word
                // _ [numerator] (*partial_terms - 1) *partial_terms_last_word <-- rename

                push 0
                push 0
                push 0
                push 0
                push 0
                // _ [numerator] (*partial_terms - 1) *partial_terms_last_word 0 0 [0]

                call {denominator_from_partial_sums_loop_label}
                // _ [numerator] (*partial_terms - 1) *partial_terms_last_word 0 0 [denominator]

                swap 4
                pop 1
                swap 4
                pop 1
                swap 4
                pop 2
                // _ [numerator] [denominator]
                hint denominator: XFieldElement = stack[0..2]

                x_invert
                xx_mul
                // _ [numerator / denominator]
                // _ [result]                  <-- rename

                return

            {&calculate_and_store_partial_terms_code}
            {&numerator_from_partial_sums_loop_code}
            {&denominator_from_partial_sums_loop_code}
        )
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use itertools::Itertools;
    use rand::prelude::*;
    use triton_vm::fri::barycentric_evaluate;
    use triton_vm::twenty_first::math::other::random_elements;
    use triton_vm::twenty_first::xfe_vec;
    use twenty_first::math::traits::Inverse;
    use twenty_first::math::traits::PrimitiveRootOfUnity;

    use crate::library::STATIC_MEMORY_START_ADDRESS;
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
            let codeword_length: u32 = codeword.len().try_into().unwrap();
            assert!(codeword_length <= MAX_CODEWORD_LENGTH);

            let codeword: Vec<XFieldElement> = codeword.into_iter().map(|x| x.into()).collect_vec();
            let result = barycentric_evaluate(&codeword, indeterminate);

            // Emulate effect on memory
            let generator = BFieldElement::primitive_root_of_unity(codeword.len() as u64).unwrap();
            let mut partial_terms_pointer = STATIC_MEMORY_START_ADDRESS
                - bfe!(MAX_CODEWORD_LENGTH * EXTENSION_DEGREE as u32 - 1);
            let mut gen_acc = bfe!(1);
            for _ in 0..codeword_length {
                let n = gen_acc;
                let d = gen_acc - indeterminate;
                let term: XFieldElement = d.inverse() * n;
                memory.insert(partial_terms_pointer, term.coefficients[0]);
                partial_terms_pointer.increment();
                memory.insert(partial_terms_pointer, term.coefficients[1]);
                partial_terms_pointer.increment();
                memory.insert(partial_terms_pointer, term.coefficients[2]);
                partial_terms_pointer.increment();

                gen_acc *= generator;
            }

            for word in result.coefficients.into_iter().rev() {
                stack.push(word);
            }
        }

        fn corner_case_initial_states(&self) -> Vec<FunctionInitialState> {
            let some_indeterminate = XFieldElement::new([bfe!(1555), bfe!(1556), bfe!(1557)]);
            let some_codeword_pointer = bfe!(19191919);
            let codeword_of_length_one =
                self.prepare_state(xfe_vec![155], some_codeword_pointer, some_indeterminate);
            let const_codeword_of_length_two = self.prepare_state(
                xfe_vec![155, 155],
                some_codeword_pointer,
                some_indeterminate,
            );
            let non_const_codeword_of_length_two = self.prepare_state(
                xfe_vec![155, 1_919_191_919],
                some_codeword_pointer,
                some_indeterminate,
            );
            let const_codeword_of_length_8 =
                self.prepare_state(xfe_vec![155; 8], some_codeword_pointer, some_indeterminate);

            vec![
                codeword_of_length_one,
                const_codeword_of_length_two,
                non_const_codeword_of_length_two,
                const_codeword_of_length_8,
            ]
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
