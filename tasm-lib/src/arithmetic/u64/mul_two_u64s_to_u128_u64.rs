use rand::thread_rng;
use rand::RngCore;
use triton_vm::prelude::*;
use triton_vm::twenty_first::prelude::U32s;

use crate::data_type::DataType;
use crate::empty_stack;
use crate::push_encodable;
use crate::traits::deprecated_snippet::DeprecatedSnippet;
use crate::ExecutionState;

#[derive(Clone, Debug)]
pub struct MulTwoU64sToU128;

impl DeprecatedSnippet for MulTwoU64sToU128 {
    fn entrypoint_name(&self) -> String {
        "tasm_arithmetic_u64_mul_two_u64s_to_u128_u64".to_string()
    }

    fn input_field_names(&self) -> Vec<String> {
        vec![
            "rhs_hi".to_string(),
            "rhs_lo".to_string(),
            "lhs_hi".to_string(),
            "lhs_lo".to_string(),
        ]
    }

    fn input_types(&self) -> Vec<DataType> {
        vec![DataType::U64, DataType::U64]
    }

    fn output_field_names(&self) -> Vec<String> {
        vec![
            "prod_3".to_string(),
            "prod_2".to_string(),
            "prod_1".to_string(),
            "prod_0".to_string(),
        ]
    }

    fn output_types(&self) -> Vec<DataType> {
        vec![DataType::U128]
    }

    fn stack_diff(&self) -> isize {
        0
    }

    fn function_code(&self, _library: &mut crate::library::Library) -> String {
        let entrypoint = self.entrypoint_name();
        format!(
            "
                // BEFORE: _ rhs_hi rhs_lo lhs_hi lhs_lo
                // AFTER:  _ prod_3 prod_2 prod_1 prod_0
                {entrypoint}:
                    // 0.
                    // let a = lhs_lo * rhs_lo
                    // - prod_0  = a_lo
                    // - carry_0 = a_hi

                    // 1.
                    // let b = (lhs_lo * rhs_hi)_lo + (lhs_hi * rhs_lo)_lo + carry_0
                    // - prod_1  = b_lo
                    // - carry_1 = b_hi

                    // 2.
                    // let c = (lhs_lo * rhs_hi)_hi + (lhs_hi * rhs_lo)_hi + (lhs_hi * rhs_hi)_lo + carry_1
                    // - prod_2 = c_lo
                    // - carry_2 = c_hi

                    // 3.
                    // let d = (lhs_hi * rhs_hi)_hi + carry_2
                    // - prod_3 = d_lo = d
                    // - carry_3 = d_hi = 0 (because max value of `(lhs_hi * rhs_hi)_hi` is 0xfffffffe)

                    // 0.
                    // _ rhs_hi rhs_lo lhs_hi lhs_lo
                    dup 0 dup 3 mul
                    // _ rhs_hi rhs_lo lhs_hi lhs_lo a

                    split
                    // _ rhs_hi rhs_lo lhs_hi lhs_lo carry_0 prod_0

                    // 1.
                    swap 2
                    // _ rhs_hi rhs_lo lhs_hi prod_0 carry_0 lhs_lo

                    dup 5 mul split
                    // _ rhs_hi rhs_lo lhs_hi prod_0 carry_0 (lhs_lo * rhs_hi)_hi (lhs_lo * rhs_hi)_lo

                    swap 1 swap 5
                    // _ rhs_hi (lhs_lo * rhs_hi)_hi lhs_hi prod_0 carry_0 (lhs_lo * rhs_hi)_lo rhs_lo

                    dup 4 mul split
                    // _ rhs_hi (lhs_lo * rhs_hi)_hi lhs_hi prod_0 carry_0 (lhs_lo * rhs_hi)_lo (rhs_lo * lhs_hi)_hi (rhs_lo * lhs_hi)_lo

                    swap 1 swap 3
                    // _ rhs_hi (lhs_lo * rhs_hi)_hi lhs_hi prod_0 (rhs_lo * lhs_hi)_hi (lhs_lo * rhs_hi)_lo (rhs_lo * lhs_hi)_lo carry_0

                    add add
                    // _ rhs_hi (lhs_lo * rhs_hi)_hi lhs_hi prod_0 (rhs_lo * lhs_hi)_hi ((lhs_lo * rhs_hi)_lo + (rhs_lo * lhs_hi)_lo + carry_0)
                    // _ rhs_hi (lhs_lo * rhs_hi)_hi lhs_hi prod_0 (rhs_lo * lhs_hi)_hi b

                    split
                    // _ rhs_hi (lhs_lo * rhs_hi)_hi lhs_hi prod_0 (rhs_lo * lhs_hi)_hi carry_1 prod_1

                    // 2.
                    swap 4
                    // _ rhs_hi (lhs_lo * rhs_hi)_hi prod_1 prod_0 (rhs_lo * lhs_hi)_hi carry_1 lhs_hi

                    dup 6
                    // _ rhs_hi (lhs_lo * rhs_hi)_hi prod_1 prod_0 (rhs_lo * lhs_hi)_hi carry_1 lhs_hi rhs_hi

                    mul split
                    // _ rhs_hi (lhs_lo * rhs_hi)_hi prod_1 prod_0 (rhs_lo * lhs_hi)_hi carry_1 (lhs_hi * rhs_hi)_hi (lhs_hi * rhs_hi)_lo

                    swap 1 swap 6
                    // _ rhs_hi (lhs_hi * rhs_hi)_hi prod_1 prod_0 (rhs_lo * lhs_hi)_hi carry_1 (lhs_hi * rhs_hi)_lo (lhs_lo * rhs_hi)_hi

                    add add add
                    // _ rhs_hi (lhs_hi * rhs_hi)_hi prod_1 prod_0 c

                    split
                    // _ rhs_hi (lhs_hi * rhs_hi)_hi prod_1 prod_0  c_hi    c_lo
                    // _ rhs_hi (lhs_hi * rhs_hi)_hi prod_1 prod_0 carry_2 prod_2

                    // 3.
                    swap 4
                    // _ rhs_hi prod_2 prod_1 prod_0 carry_2 (lhs_hi * rhs_hi)_hi

                    add
                    // _ rhs_hi prod_2 prod_1 prod_0 d
                    // _ rhs_hi prod_2 prod_1 prod_0 prod_3

                    swap 4 pop 1
                    // _ prod_3 prod_2 prod_1 prod_0

                    return
                "
        )
    }

    fn crash_conditions(&self) -> Vec<String> {
        vec![]
    }

    fn gen_input_states(&self) -> Vec<ExecutionState> {
        let mut ret: Vec<ExecutionState> = vec![
            prepare_state(1, 1),
            prepare_state(1, 2),
            prepare_state(2, 1),
            prepare_state(2, 2),
        ];
        for _ in 0..10 {
            ret.push(prepare_state(
                thread_rng().next_u32() as u64,
                thread_rng().next_u32() as u64,
            ));
            ret.push(prepare_state(
                thread_rng().next_u32() as u64,
                thread_rng().next_u64(),
            ));
            ret.push(prepare_state(
                thread_rng().next_u64(),
                thread_rng().next_u32() as u64,
            ));
            ret.push(prepare_state(
                thread_rng().next_u64(),
                thread_rng().next_u64(),
            ));
        }
        ret.push(prepare_state(u64::MAX, u64::MAX));
        ret.push(prepare_state(u64::MAX - 1, u64::MAX));
        ret.push(prepare_state(u64::MAX, u64::MAX - 1));
        ret.push(prepare_state(u64::MAX - 1, u64::MAX - 1));
        ret.push(prepare_state(u64::MAX, u64::MAX - 2));
        ret.push(prepare_state(u64::MAX - 2, u64::MAX));

        ret.push(prepare_state(u64::MAX, 0));
        ret.push(prepare_state(u64::MAX - 1, 0));
        ret.push(prepare_state(u64::MAX - 2, 0));
        ret.push(prepare_state(u64::MAX, 1));
        ret.push(prepare_state(u64::MAX - 1, 1));
        ret.push(prepare_state(u64::MAX - 2, 1));
        ret.push(prepare_state(0, u64::MAX));
        ret.push(prepare_state(1, u64::MAX));
        ret.push(prepare_state(0, u64::MAX - 1));
        ret.push(prepare_state(1, u64::MAX - 1));
        ret.push(prepare_state(0, u64::MAX - 2));
        ret.push(prepare_state(1, u64::MAX - 2));
        ret.push(prepare_state(0, 0));
        ret.push(prepare_state(0, 1));
        ret.push(prepare_state(1, 0));

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
        let a_lo: u32 = stack.pop().unwrap().try_into().unwrap();
        let a_hi: u32 = stack.pop().unwrap().try_into().unwrap();
        let a: u64 = ((a_hi as u64) << 32) + a_lo as u64;

        let b_lo: u32 = stack.pop().unwrap().try_into().unwrap();
        let b_hi: u32 = stack.pop().unwrap().try_into().unwrap();
        let b: u64 = ((b_hi as u64) << 32) + b_lo as u64;
        let prod = a as u128 * b as u128;

        // Pushing the result onto the stack as four u32 limbs
        stack.push(BFieldElement::new((prod >> 96) as u64));
        stack.push(BFieldElement::new(((prod >> 64) & u32::MAX as u128) as u64));
        stack.push(BFieldElement::new(((prod >> 32) & u32::MAX as u128) as u64));
        stack.push(BFieldElement::new((prod & u32::MAX as u128) as u64));
    }
}

fn prepare_state(a: u64, b: u64) -> ExecutionState {
    let a = U32s::<2>::try_from(a).unwrap();
    let b = U32s::<2>::try_from(b).unwrap();
    let mut init_stack = empty_stack();
    push_encodable(&mut init_stack, &a);
    push_encodable(&mut init_stack, &b);
    ExecutionState::with_stack(init_stack)
}

#[cfg(test)]
mod tests {
    use crate::test_helpers::test_rust_equivalence_multiple_deprecated;

    use super::*;

    #[test]
    fn safe_mul_u64_test() {
        test_rust_equivalence_multiple_deprecated(&MulTwoU64sToU128, true);
    }
}

#[cfg(test)]
mod benches {
    use crate::snippet_bencher::bench_and_write;

    use super::*;

    #[test]
    fn safe_u64_benchmark() {
        bench_and_write(MulTwoU64sToU128);
    }
}
