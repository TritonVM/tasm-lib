use triton_vm::prelude::*;
use twenty_first::math::x_field_element::EXTENSION_DEGREE;

use crate::arithmetic::bfe::primitive_root_of_unity::PrimitiveRootOfUnity;
use crate::prelude::*;

const MAX_CODEWORD_LENGTH: u32 = 1 << 15;

/// Use the barycentric Lagrange evaluation formula to extrapolate the codeword
/// to an out-of-domain location. Codeword must have a length that is a power
/// of 2 and may not exceed `MAX_CODEWORD_LENGTH`.
#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub struct BarycentricEvaluation;

impl BasicSnippet for BarycentricEvaluation {
    fn parameters(&self) -> Vec<(DataType, String)> {
        vec![
            (
                DataType::List(Box::new(DataType::Xfe)),
                "codeword".to_owned(),
            ),
            (DataType::Xfe, "indeterminate".to_owned()),
        ]
    }

    fn return_values(&self) -> Vec<(DataType, String)> {
        vec![(DataType::Xfe, "evaluation_result".to_owned())]
    }

    fn entrypoint(&self) -> String {
        "tasmlib_verifier_fri_barycentric_evaluation".to_owned()
    }

    fn code(&self, library: &mut Library) -> Vec<LabelledInstruction> {
        let entrypoint = self.entrypoint();
        let generator = library.import(Box::new(PrimitiveRootOfUnity));
        let partial_terms_alloc = library.kmalloc(MAX_CODEWORD_LENGTH * EXTENSION_DEGREE as u32);

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

                pick 8
                write_mem {EXTENSION_DEGREE}
                place 5
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
            // BEGIN:     _ *ptec *partial_terms[0] 0 [0; 3]   *codeword[0]
            // INVARIANT: _ *ptec *partial_terms[n] 0 [acc; 3] *codeword[n]
            {numerator_from_partial_sums_loop_label}:
                pick 5
                xx_dot_step
                place 5
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

                pick 5
                read_mem {EXTENSION_DEGREE}
                place 8
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

                /* assert `codeword_len <= MAX_CODEWORD_LENGTH` */
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

                pick 1
                // _ *codeword [-indeterminate] generator codeword_len

                push {EXTENSION_DEGREE}
                mul
                push {partial_terms_alloc.write_address()}
                add
                // _ *codeword [-indeterminate] generator (*partial_terms_last_word+1)

                place 4
                place 3
                push {partial_terms_alloc.write_address()}
                place 4
                // _ *codeword (*partial_terms_last_word+1) *partial_terms[0] generator [-indeterminate]

                push 1
                // _ *codeword (*partial_terms_last_word+1) *partial_terms[0] generator [-indeterminate] generator_acc

                call {calculate_and_store_partial_terms_loop_label}
                // _ *codeword (*partial_terms_last_word+1) (*partial_terms_last_word+1) generator [-indeterminate] generator_acc

                pop 5
                pop 1
                // _ *codeword (*partial_terms_last_word+1)

                place 1
                push {partial_terms_alloc.write_address()}
                push 0
                push 0 push 0 push 0
                // _ (*partial_terms_last_word+1) *codeword *partial_terms[0] 0 [acc; 3]

                pick 5
                addi 1
                // _ (*partial_terms_last_word+1) *partial_terms[0] 0 [acc; 3] *codeword[0]

                call {numerator_from_partial_sums_loop_label}
                hint numerator: XFieldElement = stack[1..4]
                // _ (*partial_terms_last_word+1) (*partial_terms_last_word+1) 0 [numerator; 3] *codeword[last + 1]

                pick 4
                pick 5
                pop 3
                // _ (*partial_terms_last_word+1) [numerator; 3]

                pick 3
                addi -1
                // _ [numerator; 3] *partial_terms_last_word

                push 0
                push 0
                push 0
                push 0
                push 0
                // _ [numerator; 3] *partial_terms_last_word 0 0 [0]

                push {partial_terms_alloc.write_address() - bfe!(1)}
                hint loop_end_condition = stack[0]
                place 6
                // _ [numerator; 3] (*partial_terms - 1) *partial_terms_last_word 0 0 [0]

                call {denominator_from_partial_sums_loop_label}
                hint denominator: XFieldElement = stack[0..2]
                // _ [numerator; 3] (*partial_terms - 1) (*partial_terms - 1) 0 0 [denominator]

                place 6
                place 6
                place 6
                pop 4
                // _ [numerator; 3] [denominator]

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
    use twenty_first::math::other::random_elements;
    use twenty_first::math::polynomial::barycentric_evaluate;
    use twenty_first::math::traits::PrimitiveRootOfUnity;

    use super::*;
    use crate::library::STATIC_MEMORY_FIRST_ADDRESS;
    use crate::rust_shadowing_helper_functions::list::list_insert;
    use crate::rust_shadowing_helper_functions::list::load_list_with_copy_elements;
    use crate::test_prelude::*;

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
            memory: &mut HashMap<BFieldElement, BFieldElement>,
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
            let mut partial_terms_pointer = STATIC_MEMORY_FIRST_ADDRESS
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

        fn pseudorandom_initial_state(
            &self,
            seed: [u8; 32],
            bench_case: Option<BenchmarkCase>,
        ) -> FunctionInitialState {
            let mut rng = StdRng::from_seed(seed);
            let codeword_length = match bench_case {
                Some(BenchmarkCase::CommonCase) => 256,
                Some(BenchmarkCase::WorstCase) => 512,
                None => 1 << rng.random_range(0..=14),
            };

            let codeword_pointer = rng.random_range(0..=(1u64 << 34));
            let codeword_pointer = bfe!(codeword_pointer);
            let indeterminate: XFieldElement = rng.random();
            let codeword = random_elements(codeword_length);

            self.prepare_state(codeword, codeword_pointer, indeterminate)
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
    }
}

#[cfg(test)]
mod benches {
    use super::*;
    use crate::test_prelude::*;

    #[test]
    fn bench_barycentric_evaluation() {
        ShadowedFunction::new(BarycentricEvaluation).bench();
    }
}
