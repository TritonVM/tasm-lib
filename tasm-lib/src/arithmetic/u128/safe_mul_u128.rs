use rand::RngCore;
use twenty_first::amount::u32s::U32s;
use twenty_first::shared_math::b_field_element::BFieldElement;

use crate::library::Library;
use crate::snippet::{DataType, DeprecatedSnippet};
use crate::{get_init_tvm_stack, push_encodable, ExecutionState};

#[derive(Clone, Debug)]
pub struct SafeMulU128;

impl DeprecatedSnippet for SafeMulU128 {
    fn entrypoint_name(&self) -> String {
        "tasm_arithmetic_u128_safe_mul".to_string()
    }

    fn input_field_names(&self) -> Vec<String> {
        vec![
            "rhs_3".to_string(),
            "rhs_2".to_string(),
            "rhs_1".to_string(),
            "rhs_0".to_string(),
            "lhs_3".to_string(),
            "lhs_2".to_string(),
            "lhs_1".to_string(),
            "lhs_0".to_string(),
        ]
    }

    fn input_types(&self) -> Vec<DataType> {
        vec![DataType::U128, DataType::U128]
    }

    fn output_types(&self) -> Vec<DataType> {
        vec![DataType::U128]
    }

    fn output_field_names(&self) -> Vec<String> {
        vec![
            "prod_0".to_string(),
            "prod_1".to_string(),
            "prod_2".to_string(),
            "prod_3".to_string(),
        ]
    }

    fn stack_diff(&self) -> isize {
        -4
    }

    fn function_code(&self, _library: &mut Library) -> String {
        let entrypoint = self.entrypoint_name();

        format!(

    "

                    // BEFORE: _ rhs_3 rhs_2 rhs_1 rhs_0 lhs_3 lhs_2 lhs_1 lhs_0
                    // AFTER: _ prod_3 prod_2 prod_1 prod_0

                    // The product limbs are defined as follows:
                    // a = lhs_0*rhs_0
                    // prod_0 = a_lo
                    // b = (lhs_1*rhs_0 + lhs_0*rhs_1 + a_hi)
                    // prod_1 = b_lo
                    // c = (lhs_2*rhs_0 + lhs_1*rhs_1 + lhs_0*rhs_2 + b_hi)
                    // prod_2 = c_lo
                    // d = (lhs_3*rhs_0 + lhs_2*rhs_1 + lhs_1*rhs_2 + lhs_0*rhs_3 +c_hi)
                    // prod_3 = d_lo

                    //The checks to be performed are:
                    // 1. d_hi == 0
                    // 2. lhs_1*rhs_3 == 0
                    // 3. lhs_2*rhs_2 == 0
                    // 3. lhs_2*rhs_3 == 0
                    // 4. lhs_3*rhs_1 == 0
                    // 6. lhs_3*rhs_2 == 0
                    // 7. lhs_3*rhs_3 == 0

                    {entrypoint}:
                            // a = lhs_0 * rhs_lo
                            dup 0 dup 5 mul 
                            // _ rhs_3 rhs_2 rhs_1 rhs_0 lhs_3 lhs_2 lhs_1 lhs_0 a

                            // split `a` into `a_hi` and `a_lo`
                            split
                            // _ rhs_3 rhs_2 rhs_1 rhs_0 lhs_3 lhs_2 lhs_1 lhs_0 a_hi a_lo

                            // swap a_hi and a_lo
                            swap 1
                            // _ rhs_3 rhs_2 rhs_1 rhs_0 lhs_3 lhs_2 lhs_1 lhs_0 a_lo a_hi

                            // lhs_1 * rhs_0
                            dup 3 dup 7 mul
                            // _ rhs_3 rhs_2 rhs_1 rhs_0 lhs_3 lhs_2 lhs_1 lhs_0 a_lo a_hi (lhs_1 * rhs_0)

                            //split (lhs_1*rhs_0)
                            split
                            // _ rhs_3 rhs_2 rhs_1 rhs_0 lhs_3 lhs_2 lhs_1 lhs_0 a_lo a_hi (lhs_1 * rhs_0)_hi (lhs_1 * rhs_0)_lo

                            // duplicate a_hi and add
                            dup 2 add
                            // _ rhs_3 rhs_2 rhs_1 rhs_0 lhs_3 lhs_2 lhs_1 lhs_0 a_lo a_hi (lhs_1 * rhs_0)_hi ((lhs_1 * rhs_0)_lo + a_hi)

                            //swap a_hi and pop
                            swap 2 pop
                            // _ rhs_3 rhs_2 rhs_1 rhs_0 lhs_3 lhs_2 lhs_1 lhs_0 a_lo  ((lhs_1*rhs_0)_lo+a_hi) (lhs_1 * rhs_0)_hi

                            // (lhs_0*rhs_1)
                            dup 3 dup 9 mul
                            // _ rhs_3 rhs_2 rhs_1 rhs_0 lhs_3 lhs_2 lhs_1 lhs_0 a_lo ((lhs_1*rhs_0)_lo+a_hi) (lhs_1 * rhs_0)_hi (lhs_0 * rhs_1)

                            // split (lhs_0*rhs_1)
                            split
                            // _ rhs_3 rhs_2 rhs_1 rhs_0 lhs_3 lhs_2 lhs_1 lhs_0 a_lo ((lhs_1*rhs_0)_lo+a_hi) (lhs_1 * rhs_0)_hi (lhs_0 * rhs_1)_hi (lhs_0 * rhs_1)_lo

                            // duplicate '3' and add 
                            dup 3 add 
                            // _ rhs_3 rhs_2 rhs_1 rhs_0 lhs_3 lhs_2 lhs_1 lhs_0 a_lo ((lhs_1*rhs_0)_lo+a_hi) (lhs_1 * rhs_0)_hi (lhs_0 * rhs_1)_hi ((lhs_0 * rhs_1)_lo + (lhs_1 * rhs_0)_lo + a_hi)

                            //swap pop and add
                            split swap 4 pop add add
                            // _ rhs_3 rhs_2 rhs_1 rhs_0 lhs_3 lhs_2 lhs_1 lhs_0 p_0 p_1 ((lhs_0 * rhs_1)_hi + (lhs_1*rhs_0)_hi)

                            // rename to carry_1
                            // _ rhs_3 rhs_2 rhs_1 rhs_0 lhs_3 lhs_2 lhs_1 lhs_0 p_0 p_1 carry_1

                            // (lhs_2 * rhs_0)
                            dup 5 dup 8 mul
                            // _ rhs_3 rhs_2 rhs_1 rhs_0 lhs_3 lhs_2 lhs_1 lhs_0 p_0 p_1 carry_1 (lhs_2 * rhs_0)

                            // split (lhs_2 * rhs_0)
                            split
                            // _ rhs_3 rhs_2 rhs_1 rhs_0 lhs_3 lhs_2 lhs_1 lhs_0 p_0 p_1 carry_1 (lhs_2 * rhs_0)_hi (lhs_2 * rhs_0)_lo

                            // duplicate '2' and add
                            dup 2 add
                            // _ rhs_3 rhs_2 rhs_1 rhs_0 lhs_3 lhs_2 lhs_1 lhs_0 p_0 p_1 carry_1 (lhs_2 * rhs_0)_hi ((lhs_2 * rhs_0)_lo + carry_1)

                            // swap carry_1 pop
                            swap 2 pop
                            // _ rhs_3 rhs_2 rhs_1 rhs_0 lhs_3 lhs_2 lhs_1 lhs_0 p_0 p_1 ((lhs_2 * rhs_0)_lo + carry_1) (lhs_2 * rhs_0)_hi

                            // (lhs_1 * rhs_1)
                            dup 5 dup 10 mul
                            // _ rhs_3 rhs_2 rhs_1 rhs_0 lhs_3 lhs_2 lhs_1 lhs_0 p_0 p_1 ((lhs_2 * rhs_0)_lo + carry_1) (lhs_2 * rhs_0)_hi (lhs_1 * rhs_1)

                            // split (lhs_1 * rhs_1)
                            split
                            // _ rhs_3 rhs_2 rhs_1 rhs_0 lhs_3 lhs_2 lhs_1 lhs_0 p_0 p_1 ((lhs_2 * rhs_0)_lo + carry_1) (lhs_2 * rhs_0)_hi (lhs_1 * rhs_1)_hi (lhs_1 * rhs_1)_lo

                            // duplicate '3' and add
                            dup 3 add
                            // _ rhs_3 rhs_2 rhs_1 rhs_0 lhs_3 lhs_2 lhs_1 lhs_0 p_0 p_1 ((lhs_2 * rhs_0)_lo + carry_1) (lhs_2 * rhs_0)_hi (lhs_1 * rhs_1)_hi ((lhs_1 * rhs_1)_lo + (lhs_2 * rhs_0)_lo + carry_1)
                            
                            // swap pop and add
                            swap 3 pop add
                            // _ rhs_3 rhs_2 rhs_1 rhs_0 lhs_3 lhs_2 lhs_1 lhs_0 p_0 p_1 ((lhs_1 * rhs_1)_lo + (lhs_2 * rhs_0)_lo + carry_1) ((lhs_1 * rhs_1)_hi + (lhs_2 * rhs_0)_hi)

                            // (lhs_0 * rhs_2)
                            dup 4 dup 11 mul
                            // _ rhs_3 rhs_2 rhs_1 rhs_0 lhs_3 lhs_2 lhs_1 lhs_0 p_0 p_1 ((lhs_1 * rhs_1)_lo + (lhs_2 * rhs_0)_lo + carry_1) ((lhs_1 * rhs_1)_hi + (lhs_2 * rhs_0)_hi) (lhs_0 * rhs_2)

                            // split (lhs_0 * rhs_2)
                            split
                            // _ rhs_3 rhs_2 rhs_1 rhs_0 lhs_3 lhs_2 lhs_1 lhs_0 p_0 p_1 ((lhs_1 * rhs_1)_lo + (lhs_2 * rhs_0)_lo + carry_1) ((lhs_1 * rhs_1)_hi + (lhs_2 * rhs_0)_hi) (lhs_0 * rhs_2)_hi (lhs_0 * rhs_2)_lo
                            
                            // duplicate '3' and add
                            dup 3 add
                            // _ rhs_3 rhs_2 rhs_1 rhs_0 lhs_3 lhs_2 lhs_1 lhs_0 p_0 p_1 ((lhs_1 * rhs_1)_lo + (lhs_2 * rhs_0)_lo + carry_1) ((lhs_1 * rhs_1)_hi + (lhs_2 * rhs_0)_hi) (lhs_0 * rhs_2)_hi ((lhs_0 * rhs_2)_lo + (lhs_1 * rhs_1)_lo + (lhs_2 * rhs_0)_lo + carry_1)

                            // swap pop and add
                            split swap 4 pop add add
                            // _ rhs_3 rhs_2 rhs_1 rhs_0 lhs_3 lhs_2 lhs_1 lhs_0 p_0 p_1 p_2 ((lhs_0 * rhs_2)_hi + (lhs_1 * rhs_1)_hi + (lhs_2 * rhs_0)_hi)

                            // rename to carry_2
                            // _ rhs_3 rhs_2 rhs_1 rhs_0 lhs_3 lhs_2 lhs_1 lhs_0 p_0 p_1 p_2 carry_2

                            // (lhs_3 * rhs_0)
                            push 0 swap 9
                            // _ rhs_3 rhs_2 rhs_1 0 lhs_3 lhs_2 lhs_1 lhs_0 p_0 p_1 p_2 carry_2 rhs_0

                            dup 8 mul
                            // _ rhs_3 rhs_2 rhs_1 0 lhs_3 lhs_2 lhs_1 lhs_0 p_0 p_1 p_2 carry_2 (lhs_3 * rhs_0)
                            
                            // split (lhs_3 * rhs_0)
                            split
                            // _ rhs_3 rhs_2 rhs_1 0 lhs_3 lhs_2 lhs_1 lhs_0 p_0 p_1 p_2 carry_2 (lhs_3 * rhs_0)_hi (lhs_3 * rhs_0)_lo

                            // duplicate '2' and add
                            dup 2 add
                            // _ rhs_3 rhs_2 rhs_1 0 lhs_3 lhs_2 lhs_1 lhs_0 p_0 p_1 p_2 carry_2 (lhs_3 * rhs_0)_hi ((lhs_3 * rhs_0)_lo + carry_2)

                            // swap carry_2 pop
                            swap 2 pop
                            // _ rhs_3 rhs_2 rhs_1 0 lhs_3 lhs_2 lhs_1 lhs_0 p_0 p_1 p_2 ((lhs_3 * rhs_0)_lo + carry_2) (lhs_3 * rhs_0)_hi

                            // (lhs_2 * rhs_1)
                            dup 7 dup 11 mul
                            // _ rhs_3 rhs_2 rhs_1 0 lhs_3 lhs_2 lhs_1 lhs_0 p_0 p_1 p_2 ((lhs_3 * rhs_0)_lo + carry_2) (lhs_3 * rhs_0)_hi (lhs_2 * rhs_1)
                            
                            // split (lhs_2 * rhs_1)
                            split
                            // _ rhs_3 rhs_2 rhs_1 0 lhs_3 lhs_2 lhs_1 lhs_0 p_0 p_1 p_2 ((lhs_3 * rhs_0)_lo + carry_2) (lhs_3 * rhs_0)_hi (lhs_2 * rhs_1)_hi (lhs_2 * rhs_1)_lo

                            // duplicate '3' and add
                            dup 3 add
                            // _ rhs_3 rhs_2 rhs_1 0 lhs_3 lhs_2 lhs_1 lhs_0 p_0 p_1 p_2 ((lhs_3 * rhs_0)_lo + carry_2) (lhs_3 * rhs_0)_hi (lhs_2 * rhs_1)_hi ((lhs_2 * rhs_1)_lo + (lhs_3 * rhs_0)_lo + carry_2)

                            // swap pop and add
                            swap 3 pop add
                            // _ rhs_3 rhs_2 rhs_1 0 lhs_3 lhs_2 lhs_1 lhs_0 p_0 p_1 p_2 ((lhs_2 * rhs_1)_lo + (lhs_3 * rhs_0)_lo + carry_2) ((lhs_2 * rhs_1)_hi + (lhs_3 * rhs_0)_hi)
                            
                            // (lhs_1 * rhs_2)
                            dup 6 dup 12 mul
                            // _ rhs_3 rhs_2 rhs_1 0 lhs_3 lhs_2 lhs_1 lhs_0 p_0 p_1 p_2 ((lhs_2 * rhs_1)_lo + (lhs_3 * rhs_0)_lo + carry_2) ((lhs_2 * rhs_1)_hi + (lhs_3 * rhs_0)_hi) (lhs_1 * rhs_2)
                            
                            // split (lhs_1 * rhs_2)
                            split
                            // _ rhs_3 rhs_2 rhs_1 0 lhs_3 lhs_2 lhs_1 lhs_0 p_0 p_1 p_2 ((lhs_2 * rhs_1)_lo + (lhs_3 * rhs_0)_lo + carry_2) ((lhs_2 * rhs_1)_hi + (lhs_3 * rhs_0)_hi) (lhs_1 * rhs_2)_hi (lhs_1 * rhs_2)_lo

                            // duplicate '3' and add
                            dup 3 add
                            // _ rhs_3 rhs_2 rhs_1 0 lhs_3 lhs_2 lhs_1 lhs_0 p_0 p_1 p_2 ((lhs_2 * rhs_1)_lo + (lhs_3 * rhs_0)_lo + carry_2) ((lhs_2 * rhs_1)_hi + (lhs_3 * rhs_0)_hi) (lhs_1 * rhs_2)_hi ((lhs_1 * rhs_2)_lo + (lhs_2 * rhs_1)_lo + (lhs_3 * rhs_0)_lo + carry_2)

                            // swap pop and add
                            swap 3 pop add
                            // _ rhs_3 rhs_2 rhs_1 0 lhs_3 lhs_2 lhs_1 lhs_0 p_0 p_1 p_2 ((lhs_1 * rhs_2)_lo + (lhs_2 * rhs_1)_lo + (lhs_3 * rhs_0)_lo + carry_2) ((lhs_1 * rhs_2)_hi + (lhs_2 * rhs_1)_hi + (lhs_3 * rhs_0)_hi)
                          
                            // (lhs_0 * rhs_3) -- have to consume lhs_0
                            push 0 swap 6 dup 13 mul
                            // _ rhs_3 rhs_2 rhs_1 0 lhs_3 lhs_2 lhs_1 0 p_0 p_1 p_2 ((lhs_1 * rhs_2)_lo + (lhs_2 * rhs_1)_lo + (lhs_3 * rhs_0)_lo + carry_2) ((lhs_1 * rhs_2)_hi + (lhs_2 * rhs_1)_hi + (lhs_3 * rhs_0)_hi) (lhs_0 * rhs_3)
                            
                            // split (lhs_0 * rhs_3)
                            split
                            // _ rhs_3 rhs_2 rhs_1 0 lhs_3 lhs_2 lhs_1 0 p_0 p_1 p_2 ((lhs_1 * rhs_2)_lo + (lhs_2 * rhs_1)_lo + (lhs_3 * rhs_0)_lo + carry_2) ((lhs_1 * rhs_2)_hi + (lhs_2 * rhs_1)_hi + (lhs_3 * rhs_0)_hi) (lhs_0 * rhs_3)_hi (lhs_0 * rhs_3)_lo
                            
                            // duplicate '3' and add
                            dup 3 add
                            // _ rhs_3 rhs_2 rhs_1 0 lhs_3 lhs_2 lhs_1 0 p_0 p_1 p_2 ((lhs_1 * rhs_2)_lo + (lhs_2 * rhs_1)_lo + (lhs_3 * rhs_0)_lo + carry_2) ((lhs_1 * rhs_2)_hi + (lhs_2 * rhs_1)_hi + (lhs_3 * rhs_0)_hi) (lhs_0 * rhs_3)_hi ((lhs_0 * rhs_3)_lo + (lhs_1 * rhs_2)_lo + (lhs_2 * rhs_1)_lo + (lhs_3 * rhs_0)_lo + carry_2)

                            // swap pop and add
                            swap 3 pop add
                            // _ rhs_3 rhs_2 rhs_1 0 lhs_3 lhs_2 lhs_1 0 p_0 p_1 p_2 p_3 ((lhs_0 * rhs_3)_hi + (lhs_1 * rhs_2)_hi + (lhs_2 * rhs_1)_hi + (lhs_3 * rhs_0)_hi)

                            // rename to carry_3
                            // _ rhs_3 rhs_2 rhs_1 0 lhs_3 lhs_2 lhs_1 0 p_0 p_1 p_2 p_3 carry_3

                            // check whether carry_3 == 0, crash if carry_3 != 0, crash
                            push 0
                            eq
                            assert
                            // _ rhs_3 rhs_2 rhs_1 0 lhs_3 lhs_2 lhs_1 0 p_0 p_1 p_2 p_3

                            // check whether lhs_3 * rhs_1 == 0, crash if lhs_3 * rhs_1 != 0, crash
                            push 0 swap 10 dup 8 mul
                            // _ rhs_3 rhs_2 0 0 lhs_3 lhs_2 lhs_1 0 p_0 p_1 p_2 p_3 (lhs_3 * rhs_1)
                            
                            push 0
                            eq
                            assert
                            // _ rhs_3 rhs_2 0 0 lhs_3 lhs_2 lhs_1 0 p_0 p_1 p_2 p_3

                            // check whether lhs_2 * rhs_2 == 0, crash if lhs_2 * rhs_2 != 0, crash
                            dup 6 dup 11 mul
                            // _ rhs_3 rhs_2 0 0 lhs_3 lhs_2 lhs_1 0 p_0 p_1 p_2 p_3 (lhs_2 * rhs_2)

                            push 0
                            eq
                            assert
                            // _ rhs_3 rhs_2 0 0 lhs_3 lhs_2 lhs_1 0 p_0 p_1 p_2 p_3

                            // check whether lhs_1 * rhs_3 == 0, crash if lhs_1 * rhs_3 != 0, crash
                            push 0 swap 6 dup 12 mul
                            // _ rhs_3 rhs_2 0 0 lhs_3 lhs_2 0 0 p_0 p_1 p_2 p_3 (lhs_1 * rhs_3)

                            push 0
                            eq
                            assert
                            // _ rhs_3 rhs_2 0 0 lhs_3 lhs_2 0 0 p_0 p_1 p_2 p_3

                            // check whether lhs_3 * rhs_2 == 0, crash if lhs_3 * rhs_2 != 0, crash
                            push 0 swap 11 dup 8 mul
                            // _ rhs_3 0 0 0 lhs_3 lhs_2 0 0 p_0 p_1 p_2 p_3 (lhs_3 * rhs_2)

                            // check whether lhs_3 * rhs_2 == 0, crash if lhs_3 * rhs_2 != 0, crash
                            push 0
                            eq
                            assert
                            // _ rhs_3 0 0 0 lhs_3 lhs_2 0 0 p_0 p_1 p_2 p_3

                            // check whether lhs_2 * rhs_3 == 0, crash if lhs_2 * rhs_3 != 0, crash
                            push 0 swap 7 dup 8 mul
                            // _ rhs_3 0 0 0 lhs_3 0 0 0 p_0 p_1 p_2 p_3 (lhs_2 * rhs_3)

                            push 0
                            eq
                            assert
                            // _ rhs_3 0 0 0 lhs_3 0 0 0 p_0 p_1 p_2 p_3

                            // check whether lhs_3 * rhs_3 == 0, crash if lhs_3 * rhs_3 != 0, crash
                            push 0 swap 8
                            // _ rhs_3 0 0 0 0 0 0 0 p_0 p_1 p_2 p_3 lhs_3

                            push 0 swap 13 mul 
                            // _ 0 0 0 0 0 0 0 0 p_0 p_1 p_2 p_3 (lhs_3 * rhs_3)

                            push 0
                            eq
                            assert
                            // _ 0 0 0 0 0 0 0 0 p_0 p_1 p_2 p_3

                            swap 11
                            // _ p_3 0 0 0 0 0 0 0 p_0 p_1 p_2 0
                            pop
                            // _ p_3 0 0 0 0 0 0 0 p_0 p_1 p_2

                            swap 9
                            // _ p_3 p_2 0 0 0 0 0 0 p_0 p_1 0
                            pop
                            // _ p_3 p_2 0 0 0 0 0 0 p_0 p_1

                            swap 7
                            // _ p_3 p_2 p_1 0 0 0 0 0 p_0 0
                            pop
                            // _ p_3 p_2 p_1 0 0 0 0 0 p_0

                            swap 5
                            // _ p_3 p_2 p_1 p_0 0 0 0 0 0
                            pop
                            // _ p_3 p_2 p_1 p_0 0 0 0 0

                            pop
                            pop
                            pop
                            pop

                            return

                            "

                )
    }

    fn crash_conditions(&self) -> Vec<String> {
        vec!["Product is greater than u128::MAX".to_string()]
    }

    fn gen_input_states(&self) -> Vec<ExecutionState> {
        let mut rng = rand::thread_rng();

        let mut ret = vec![];

        // u32 * u32
        for _ in 0..10 {
            ret.push(prepare_state(
                rng.next_u32() as u128,
                rng.next_u32() as u128,
            ));
        }

        // u64 * u64
        for _ in 0..10 {
            ret.push(prepare_state(
                rng.next_u64() as u128,
                rng.next_u64() as u128,
            ));
        }

        // u96 * u32
        for _ in 0..10 {
            ret.push(prepare_state(
                ((rng.next_u64() as u128) << 32) + (rng.next_u32() as u128),
                rng.next_u32() as u128,
            ));
            ret.push(prepare_state(
                rng.next_u32() as u128,
                ((rng.next_u64() as u128) << 32) + (rng.next_u32() as u128),
            ));
        }

        // Corner cases
        ret.push(prepare_state(0, 0));
        ret.push(prepare_state(u64::MAX as u128, u64::MAX as u128));
        ret.push(prepare_state(u64::MAX as u128, 1u128 << 64));
        ret.push(prepare_state(1u128 << 64, u64::MAX as u128));
        ret.push(prepare_state((1u128 << 96) - 1, u32::MAX as u128));
        ret.push(prepare_state((1u128 << 96) - 1, 1u128 << 32));

        for i in 0..32 {
            ret.push(prepare_state(
                (1u128 << (96 + i)) - 1,
                (1u128 << (32 - i)) - 1,
            ));
        }

        ret
    }

    fn common_case_input_state(&self) -> ExecutionState {
        prepare_state(1 << 63, (1 << 45) - 1)
    }

    fn worst_case_input_state(&self) -> ExecutionState {
        prepare_state(1 << 63, (1 << 63) - 1)
    }

    fn rust_shadowing(
        &self,
        stack: &mut Vec<BFieldElement>,
        _std_in: Vec<BFieldElement>,
        _secret_in: Vec<BFieldElement>,
        _memory: &mut std::collections::HashMap<BFieldElement, BFieldElement>,
    ) {
        // the initial stack looks like
        // _ b_3 b_2 b_1 b_0 a_3 a_2 a_1 a_0
        let a_0: u32 = stack.pop().unwrap().try_into().unwrap();
        let a_1: u32 = stack.pop().unwrap().try_into().unwrap();
        let a_2: u32 = stack.pop().unwrap().try_into().unwrap();
        let a_3: u32 = stack.pop().unwrap().try_into().unwrap();
        let a: u128 =
            ((a_3 as u128) << 96) + ((a_2 as u128) << 64) + ((a_1 as u128) << 32) + a_0 as u128;

        let b_0: u32 = stack.pop().unwrap().try_into().unwrap();
        let b_1: u32 = stack.pop().unwrap().try_into().unwrap();
        let b_2: u32 = stack.pop().unwrap().try_into().unwrap();
        let b_3: u32 = stack.pop().unwrap().try_into().unwrap();
        let b: u128 =
            ((b_3 as u128) << 96) + ((b_2 as u128) << 64) + ((b_1 as u128) << 32) + b_0 as u128;

        // let prod = a.wrapping_mul(b);
        let (safe_mul_prod, overflow) = a.overflowing_mul(b);
        assert!(!overflow, "u128 mul result overflowed");
        // Pushing the result onto the stack as four u32 limbs
        stack.push(BFieldElement::new((safe_mul_prod >> 96) as u64));
        stack.push(BFieldElement::new(
            ((safe_mul_prod >> 64) & u32::MAX as u128) as u64,
        ));
        stack.push(BFieldElement::new(
            ((safe_mul_prod >> 32) & u32::MAX as u128) as u64,
        ));
        stack.push(BFieldElement::new(
            (safe_mul_prod & u32::MAX as u128) as u64,
        ));
    }
}

fn prepare_state(a: u128, b: u128) -> ExecutionState {
    let a = U32s::<4>::try_from(a).unwrap();
    let b = U32s::<4>::try_from(b).unwrap();
    let mut init_stack = get_init_tvm_stack();
    push_encodable(&mut init_stack, &a);
    push_encodable(&mut init_stack, &b);
    ExecutionState::with_stack(init_stack)
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use twenty_first::shared_math::bfield_codec::BFieldCodec;

    use crate::test_helpers::test_rust_equivalence_multiple_deprecated;

    use super::*;

    #[test]
    fn safe_mul_u128_test() {
        test_rust_equivalence_multiple_deprecated(&SafeMulU128, true);
    }

    #[test]
    fn normal_safe_mul_128_test() {
        // Expect normal behaviour
        let lhs: U32s<4> = U32s::try_from(1u128 << 32).unwrap();
        let rhs: U32s<4> = U32s::try_from(1u128 << 32).unwrap();
        let mut init_stack = get_init_tvm_stack();
        for elem in rhs.encode().into_iter().rev() {
            init_stack.push(elem);
        }
        for elem in lhs.encode().into_iter().rev() {
            init_stack.push(elem);
        }

        SafeMulU128
            .link_and_run_tasm_from_state_for_test(&mut ExecutionState::with_stack(init_stack));
    }

    #[test]
    fn expected_overflow_safe_mul_128_test() {
        for i in 1..128 {
            let lhs: U32s<4> = U32s::try_from(1u128 << i).unwrap();
            let rhs: U32s<4> = U32s::try_from(1u128 << (128 - i)).unwrap();
            let mut init_stack = get_init_tvm_stack();
            for elem in rhs.encode().into_iter().rev() {
                init_stack.push(elem);
            }
            for elem in lhs.encode().into_iter().rev() {
                init_stack.push(elem);
            }

            match SafeMulU128.link_and_run_tasm_for_test(
                &mut init_stack,
                vec![],
                vec![],
                &mut HashMap::default(),
                1,
            ) {
                Ok(_) => panic!("Overflow must result in error"),
                Err(err) => println!("Error: {}", err),
            }
        }
    }

}

#[cfg(test)]
mod benches {
    use super::*;
    use crate::snippet_bencher::bench_and_write;

    #[test]
    fn safe_u64_benchmark() {
        bench_and_write(SafeMulU128);
    }
}
