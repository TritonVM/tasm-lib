use triton_vm::table::{NUM_BASE_COLUMNS, NUM_EXT_COLUMNS};
use triton_vm::triton_asm;
use triton_vm::twenty_first::shared_math::x_field_element::EXTENSION_DEGREE;

use crate::{
    data_type::{ArrayType, DataType},
    traits::basic_snippet::BasicSnippet,
};

pub struct InnerProductOfThreeRowsWithWeights {
    base_length: usize,
    ext_length: usize,
    randomizer_length: usize,
}

impl InnerProductOfThreeRowsWithWeights {
    pub fn new(base_length: usize, ext_length: usize, randomizer_length: usize) -> Self {
        Self {
            base_length,
            ext_length,
            randomizer_length,
        }
    }

    pub fn recufier_parameters() -> Self {
        Self {
            base_length: NUM_BASE_COLUMNS,
            ext_length: NUM_EXT_COLUMNS,
            // TODO: Use NUM_RANDOMIZERS from TVM here instead, once randomizers
            // are handled correctly in TVM.
            randomizer_length: 0,
        }
    }
}

impl BasicSnippet for InnerProductOfThreeRowsWithWeights {
    fn inputs(&self) -> Vec<(DataType, String)> {
        vec![
            (
                DataType::Array(Box::new(ArrayType {
                    element_type: DataType::Xfe,
                    length: self.base_length + self.ext_length - self.randomizer_length,
                })),
                "*weights".to_string(),
            ),
            (
                DataType::Array(Box::new(ArrayType {
                    element_type: DataType::Bfe,
                    length: self.base_length,
                })),
                "*base_row".to_string(),
            ),
            (
                DataType::Array(Box::new(ArrayType {
                    element_type: DataType::Xfe,
                    length: self.ext_length,
                })),
                "*ext_row".to_string(),
            ),
        ]
    }

    fn outputs(&self) -> Vec<(DataType, String)> {
        vec![(DataType::Xfe, "inner_product".to_owned())]
    }

    fn entrypoint(&self) -> String {
        "tasm_array_inner_product_of_three_rows_with_weights".to_string()
    }

    fn code(
        &self,
        _library: &mut crate::library::Library,
    ) -> Vec<triton_vm::prelude::LabelledInstruction> {
        let entrypoint = self.entrypoint();

        let move_to_last_word = |length: usize| {
            triton_asm! {
                // _ *ptr
                push {length-1}
                add
            }
        };
        let move_weights_pointer_to_last_word = move_to_last_word(
            (self.base_length + self.ext_length - self.randomizer_length) * EXTENSION_DEGREE,
        );
        let move_ext_row_pointer_to_last_word =
            move_to_last_word((self.ext_length - self.randomizer_length) * EXTENSION_DEGREE);
        let move_base_row_pointer_to_last_word = move_to_last_word(self.base_length);

        let acc_once_from_ext = triton_asm!(
            // _ *a *b [acc]

            dup 3
            // _ *a *b [acc] *b

            read_mem 3
            // _ *a *b [acc] [b_elem] *b_prev

            swap 8
            // _ *b_prev *b [acc] [b_elem] *a

            read_mem 3
            // _ *b_prev *b [acc] [b_elem] [a_elem] *a_prev

            swap 10
            pop 1
            // _ *b_prev *a_prev [acc] [b_elem] [a_elem]

            xxmul
            xxadd
            // _ *b_prev *a_prev [acc']
        );
        let acc_all_from_ext =
            vec![acc_once_from_ext; self.ext_length - self.randomizer_length].concat();
        let clean_stack = if (self.ext_length + self.randomizer_length) % 2 == 1 {
            triton_asm!(
                // _ *w_last *e_last [acc]
                swap 3 pop 1        // _ *w_last acc0 acc2 acc1
                swap 1 swap 2       // _ *w_last acc2 acc1 acc0
                // _ *w_last [acc]
            )
        } else {
            triton_asm!(
            // _ *e_last *w_last [acc]
            swap 1              // _ *e_last *w_last acc2 acc0 acc1
            swap 2              // _ *e_last *w_last acc1 acc0 acc2
            swap 3              // _ *e_last acc2 acc1 acc0 *w_last
            swap 4              // _ *w_last acc2 acc1 acc0 *e_last
            pop 1
            // _ *w_last [acc]
            )
        };

        let acc_once_from_base_even = triton_asm!(
            // _ *b *w [acc]

            dup 3
            // _ *b *w [acc] *w

            read_mem 3
            // _ *b *w [acc] [w_elem] *w_prev

            swap 8
            // _ *w_prev *w [acc] [w_elem] *b

            read_mem 1
            // _ *w_prev *w [acc] [w_elem] b_elem *b_prev

            swap 8
            pop 1
            // _ *w_prev *b_prev [acc] [w_elem] b_elem

            xbmul
            xxadd
            // _ *w_prev *b_prev [acc']
        );

        let acc_once_from_base_odd = triton_asm!(
            // _ *w *b [acc]

            dup 4
            // _ *w *b [acc] *w

            read_mem 3
            // _ *w *b [acc] [w_elem] *w_prev

            swap 7
            // _ *w *w_prev [acc] [w_elem] *b

            read_mem 1
            // _ *w *w_prev [acc] [w_elem] b_elem *b_prev

            swap 9
            pop 1
            // _ *b_prev *w_prev [acc] [w_elem] b_elem

            xbmul
            xxadd
            // _ *b_prev *w_prev [acc']
        );

        let acc_two_from_base = [acc_once_from_base_even.clone(), acc_once_from_base_odd].concat();
        let mut acc_all_from_base = vec![acc_two_from_base; self.base_length / 2].concat();
        if self.base_length % 2 == 1 {
            acc_all_from_base.extend(acc_once_from_base_even);
        }

        triton_asm! {
            // BEFORE: _ *weights *base_row *ext_row
            // AFTER: _ [inner_product]
            {entrypoint}:
                // move pointers to last word
                {&move_ext_row_pointer_to_last_word}    // _ *w *b *e_last
                swap 1                                  // _ *w *e_last *b
                {&move_base_row_pointer_to_last_word}   // _ *w *e_last *b_last
                swap 2                                  // _ *b_last *e_last *w
                {&move_weights_pointer_to_last_word}    // _ *b_last *e_last *w_last

                // push accumulator
                push 0
                push 0
                push 0

                // accumulate ext inner product
                {&acc_all_from_ext}
                {&clean_stack}                          // _ *b_last *w_middle [acc]

                // accumulate base inner product
                {&acc_all_from_base}
                // _ garbage0 garbage1 [acc]

                swap 2 // _ g0 g1 acc0 acc1 acc2
                swap 4 // _ acc2 g1 acc0 acc1 g0
                pop 1  // _ acc2 g1 acc0 acc1
                swap 2 // _ acc2 acc1 acc0 g1
                pop 1
                // _ acc2 acc1 acc0

                return
        }
    }
}

#[cfg(test)]
mod test {
    use std::collections::HashMap;

    use itertools::Itertools;
    use rand::rngs::StdRng;
    use rand::Rng;
    use rand::SeedableRng;
    use test_strategy::proptest;
    use triton_vm::twenty_first::shared_math::b_field_element::BFieldElement;
    use triton_vm::twenty_first::shared_math::x_field_element::XFieldElement;

    use crate::rust_shadowing_helper_functions::array::array_from_memory;
    use crate::rust_shadowing_helper_functions::array::insert_random_array;
    use crate::snippet_bencher::BenchmarkCase;
    use crate::traits::function::{Function, FunctionInitialState, ShadowedFunction};
    use crate::traits::rust_shadow::RustShadow;

    use super::*;

    #[test]
    fn three_rows_tvm_parameters_test() {
        ShadowedFunction::new(InnerProductOfThreeRowsWithWeights::recufier_parameters()).test()
    }

    #[proptest(cases = 5)]
    fn three_rows_pbt(
        #[strategy(0_usize..500)] base_length: usize,
        #[strategy(0_usize..500)] ext_length: usize,
        #[filter(#randomizer_length<#ext_length)]
        #[strategy(0_usize..10)]
        randomizer_length: usize,
    ) {
        let snippet = InnerProductOfThreeRowsWithWeights {
            base_length,
            ext_length,
            randomizer_length,
        };
        ShadowedFunction::new(snippet).test()
    }

    impl Function for InnerProductOfThreeRowsWithWeights {
        fn rust_shadow(
            &self,
            stack: &mut Vec<BFieldElement>,
            memory: &mut HashMap<BFieldElement, BFieldElement>,
        ) {
            // read stack: _ *w *b *e
            let extension_row_address = stack.pop().unwrap();
            let base_row_address = stack.pop().unwrap();
            let weights_address = stack.pop().unwrap();

            // read arrays
            let weights_len = self.base_length + self.ext_length - self.randomizer_length;
            let weights: Vec<XFieldElement> =
                array_from_memory(weights_address, weights_len, memory);
            let ext_row: Vec<XFieldElement> = array_from_memory(
                extension_row_address,
                self.ext_length - self.randomizer_length,
                memory,
            );
            let base_row: Vec<BFieldElement> =
                array_from_memory(base_row_address, self.base_length, memory);

            // compute inner product
            let ip = base_row
                .iter()
                .map(|b| b.lift())
                .chain(ext_row)
                .zip_eq(weights)
                .map(|(l, r)| l * r)
                .sum::<XFieldElement>();

            // write inner product back to stack
            let mut res = ip.coefficients.to_vec();
            stack.push(res.pop().unwrap());
            stack.push(res.pop().unwrap());
            stack.push(res.pop().unwrap());
        }

        fn pseudorandom_initial_state(
            &self,
            seed: [u8; 32],
            _bench_case: Option<BenchmarkCase>,
        ) -> FunctionInitialState {
            let mut rng: StdRng = SeedableRng::from_seed(seed);
            let base_address = rng.gen();
            let ext_address = rng.gen();
            let weights_address = rng.gen();

            let mut memory: HashMap<BFieldElement, BFieldElement> = HashMap::default();

            insert_random_array(&DataType::Bfe, base_address, self.base_length, &mut memory);
            insert_random_array(&DataType::Xfe, ext_address, self.ext_length, &mut memory);
            insert_random_array(
                &DataType::Xfe,
                weights_address,
                self.base_length + self.ext_length,
                &mut memory,
            );

            let stack = [
                self.init_stack_for_isolated_run(),
                vec![weights_address, base_address, ext_address],
            ]
            .concat();

            FunctionInitialState { stack, memory }
        }
    }
}

#[cfg(test)]
mod benches {
    use triton_vm::stark::NUM_RANDOMIZER_POLYNOMIALS;
    use triton_vm::table::NUM_BASE_COLUMNS;
    use triton_vm::table::NUM_EXT_COLUMNS;

    use crate::traits::function::ShadowedFunction;
    use crate::traits::rust_shadow::RustShadow;

    use super::*;

    #[test]
    fn inner_product_of_three_rows_bench_current_tvm() {
        ShadowedFunction::new(InnerProductOfThreeRowsWithWeights {
            base_length: NUM_BASE_COLUMNS,
            ext_length: NUM_EXT_COLUMNS,
            randomizer_length: NUM_RANDOMIZER_POLYNOMIALS,
        })
        .bench();
    }
}
