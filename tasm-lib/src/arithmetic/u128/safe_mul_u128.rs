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
                            
                            // lhs_0 * rhs_1 and add successively to produce `b`
                            dup 3 dup 9 mul add add
                            // _ rhs_3 rhs_2 rhs_1 rhs_0 lhs_3 lhs_2 lhs_1 lhs_0 a_lo (a_hi + (lhs_0 * rhs_1) + (lhs_1 * rhs_0))
                            // or, in terms of b, 
                            // _ rhs_3 rhs_2 rhs_1 rhs_0 lhs_3 lhs_2 lhs_1 lhs_0 a_lo b

                            // split b 
                            split
                            // _ rhs_3 rhs_2 rhs_1 rhs_0 lhs_3 lhs_2 lhs_1 lhs_0 a_lo b_hi b_lo
                        
                            // swap b_hi and b_lo
                            swap 1
                            // _ rhs_3 rhs_2 rhs_1 rhs_0 lhs_3 lhs_2 lhs_1 lhs_0 a_lo b_lo b_hi

                            // lhs_2 * rhs_0
                            dup 5 dup 8 mul 
                            // _ rhs_3 rhs_2 rhs_1 rhs_0 lhs_3 lhs_2 lhs_1 lhs_0 a_lo b_lo b_hi (lhs_2 * rhs_0)

                            // lhs_1 * rhs_1 and add
                            dup 5 dup 10 mul add
                            // _ rhs_3 rhs_2 rhs_1 rhs_0 lhs_3 lhs_2 lhs_1 lhs_0 a_lo b_lo b_hi ((lhs_2 * rhs_0)+(lhs_1 * rhs_1)) 

                            // lhs_0 * rhs_2 and add successively to give `c`
                            dup 4 dup 11 mul add add
                            // _ rhs_3 rhs_2 rhs_1 rhs_0 lhs_3 lhs_2 lhs_1 lhs_0 a_lo b_lo (b_hi+(lhs_2 * rhs_0)+(lhs_1 * rhs_1)+(lhs_0 * rhs_2))

                            // split c
                            split
                            // _ rhs_3 rhs_2 rhs_1 rhs_0 lhs_3 lhs_2 lhs_1 lhs_0 a_lo b_lo c_hi c_lo

                            // swap c_hi and c_lo
                            swap 1
                            // _ rhs_3 rhs_2 rhs_1 rhs_0 lhs_3 lhs_2 lhs_1 lhs_0 a_lo b_lo c_lo c_hi

                            // push 0 and swap with rhs_0 
                            push 0 swap 9
                            // _ rhs_3 rhs_2 rhs_1 0 lhs_3 lhs_2 lhs_1 lhs_0 a_lo b_lo c_lo c_hi rhs_0
                            
                            // lhs_3 * rhs_0, consume rhs_0
                            dup 8 mul
                            // _ rhs_3 rhs_2 rhs_1 0 lhs_3 lhs_2 lhs_1 lhs_0 a_lo b_lo c_lo c_hi (lhs_3 * rhs_0)

                            //lhs_0 * rhs_3, mul and add to consume lhs_0
                            push 0 swap 6 dup 13 mul add 
                            // _ rhs_3 rhs_2 rhs_1 0 lhs_3 lhs_2 lhs_1 0 a_lo b_lo c_lo c_hi ((lhs_3 * rhs_0)+(lhs_0 * rhs_3))

                            //lhs_1 * rhs_2
                            dup 6 dup 12 mul add
                            // _ rhs_3 rhs_2 rhs_1 0 lhs_3 lhs_2 lhs_1 0 a_lo b_lo c_lo c_hi ((lhs_3 * rhs_0)+(lhs_0 * rhs_3)+(lhs_1 * rhs_2))

                            //lhs_2 * rhs_1 and add successively to give `d`
                            dup 7 dup 11 mul add add 
                            //  _ rhs_3 rhs_2 rhs_1 0 lhs_3 lhs_2 lhs_1 0 a_lo b_lo c_lo (c_hi + (lhs_3 * rhs_0)+(lhs_0 * rhs_3)+(lhs_1 * rhs_2)+(lhs_2 * rhs_1))

                            // split d
                            split
                            // _ rhs_3 rhs_2 rhs_1 0 lhs_3 lhs_2 lhs_1 0 a_lo b_lo c_lo d_hi d_lo
                            
                            // swap d_hi and d_lo
                            swap 1
                            // _ rhs_3 rhs_2 rhs_1 0 lhs_3 lhs_2 lhs_1 0 a_lo b_lo c_lo d_lo d_hi
                            
                            // check if d_hi == 0, crash if d_hi!=0
                            push 0
                            eq
                            assert
                            // _ rhs_3 rhs_2 rhs_1 0 lhs_3 lhs_2 lhs_1 0 a_lo b_lo c_lo d_lo

                            // push 0 and swap with lhs_1
                            push 0 swap 6 
                            // _ rhs_3 rhs_2 rhs_1 0 lhs_3 lhs_2 0 0 a_lo b_lo c_lo d_lo lhs_1 

                            // lhs_1 * rhs_3, consume lhs_1
                            dup 12 mul
                            // _ rhs_3 rhs_2 rhs_1 0 lhs_3 lhs_2 0 0 a_lo b_lo c_lo d_lo (lhs_1 * rhs_3)

                            // check if lhs_1 * rhs_3 == 0, crash if lhs_1 * rhs_3 != 0
                            push 0
                            eq
                            assert
                            // _ rhs_3 rhs_2 rhs_1 0 lhs_3 lhs_2 0 0 a_lo b_lo c_lo d_lo

                            // push 0 and swap with lhs_2
                            push 0 swap 7
                            // _ rhs_3 rhs_2 rhs_1 0 lhs_3 0 0 0 a_lo b_lo c_lo d_lo lhs_2

                            // duplicate lhs_2
                            dup 0
                            // _ rhs_3 rhs_2 rhs_1 0 lhs_3 0 0 0 a_lo b_lo c_lo d_lo lhs_2 lhs_2

                            // lhs_2 * rhs_2
                            dup 12 mul
                            // _ rhs_3 rhs_2 rhs_1 0 lhs_3 0 0 0 a_lo b_lo c_lo d_lo lhs_2 (lhs_2 * rhs_2)

                            //check if lhs_2 * rhs_2 == 0, crash if lhs_2 * rhs_2 != 0
                            push 0
                            eq
                            assert
                            // _rhs_3 rhs_2 rhs_1 0 lhs_3 0 0 0 a_lo b_lo c_lo d_lo lhs_2


                            // lhs_2 * rhs_3, consume lhs_2]
                            dup 12 mul
                            // _ rhs_3 rhs_2 rhs_1 0 lhs_3 0 0 0 a_lo b_lo c_lo d_lo (lhs_2 * rhs_3)

                            // check if lhs_2 * rhs_3 == 0, crash if lhs_2 * rhs_3 != 0
                            push 0
                            eq 
                            assert
                            // _ rhs_3 rhs_2 rhs_1 0 lhs_3 0 0 0 a_lo b_lo c_lo d_lo

                            // push 0 and swap with rhs_1
                            push 0 swap 10
                            // _ rhs_3 rhs_2 0 0 lhs_3 0 0 0 a_lo b_lo c_lo d_lo rhs_1
                        
                            // lhs_3 * rhs_1
                            dup 8 mul 
                            // _ rhs_3 rhs_2 0 0 lhs_3 0 0 0 a_lo b_lo c_lo d_lo (lhs_3 * rhs_1)

                            // check if lhs_3 * rhs_1 == 0, crash if lhs_3 * rhs_1 != 0
                            push 0
                            eq
                            assert
                            // _ rhs_3 rhs_2 0 0 lhs_3 0 0 0 a_lo b_lo c_lo d_lo

                            // push 0 and swap with rhs_2
                            push 0 swap 11
                            // _ rhs_3 0 0 0 lhs_3 0 0 0 a_lo b_lo c_lo d_lo rhs_2

                            // lhs_3 * rhs_2
                            dup 8 mul
                            // _ rhs_3 0 0 0 lhs_3 0 0 0 a_lo b_lo c_lo d_lo (lhs_3 * rhs_2)

                            // check if lhs_3 * rhs_2 == 0, crash if lhs_3 * rhs_2 != 0
                            push 0
                            eq
                            assert
                            // _ rhs_3 0 0 0 lhs_3 0 0 0 a_lo b_lo c_lo d_lo

                            // push 0 and swap with rhs_3
                            push 0 swap 12
                            // _ 0 0 0 0 lhs_3 0 0 0 a_lo b_lo c_lo d_lo rhs_3

                            // push 0 and swap with lhs_3
                            push 0 swap 9
                            // _ 0 0 0 0 0 0 0 0 a_lo b_lo c_lo d_lo rhs_3 lhs_3

                            // lhs_3 * rhs_3, consume both
                            mul

                            // check if lhs_3 * rhs_3 == 0, crash if lhs_3 * rhs_3 != 0
                            push 0
                            eq
                            assert
                            // _ 0 0 0 0 0 0 0 0 a_lo b_lo c_lo d_lo

                            // now swap 0s with the product limbs and pop the 0s
                            swap 11 
                            // _ d_lo 0 0 0 0 0 0 0 a_lo b_lo c_lo 0
                            swap 1
                            // _ d_lo 0 0 0 0 0 0 0 a_lo b_lo 0 c_lo
                            swap 10
                            // _ d_lo c_lo 0 0 0 0 0 0 a_lo b_lo 0 0
                            swap 2
                            // _ d_lo c_lo 0 0 0 0 0 0 a_lo 0 0 b_lo
                            swap 9
                            // _ d_lo c_lo b_lo 0 0 0 0 0 a_lo 0 0 0
                            swap 3
                            // _ d_lo c_lo b_lo 0 0 0 0 0 0 0 0 a_lo
                            swap 8
                            // _ d_lo c_lo b_lo a_lo 0 0 0 0 0 0 0 0
        
                            pop
                            pop
                            pop
                            pop
                            pop
                            pop
                            pop
                            pop

                            // _ prod_3 prod_2 prod_1 prod_0
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
        for _ in 0..10 {
            ret.push(prepare_state(
                rng.next_u32() as u128,
                rng.next_u32() as u128,
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

    use num::Zero;
    use twenty_first::shared_math::bfield_codec::BFieldCodec;

    use crate::test_helpers::{
        test_rust_equivalence_given_input_values_deprecated,
        test_rust_equivalence_multiple_deprecated,
    };

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

    #[should_panic]
    #[test]
    fn expected_overflow_safe_mul_128_test() {
        // Expect normal behaviour
        let lhs: U32s<4> = U32s::try_from(1u128 << 64).unwrap();
        let rhs: U32s<4> = U32s::try_from(1u128 << 64).unwrap();
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
