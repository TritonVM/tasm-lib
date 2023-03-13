use rand::RngCore;
use twenty_first::amount::u32s::U32s;
use twenty_first::shared_math::b_field_element::BFieldElement;

use crate::arithmetic::u64::add_u64::AddU64;
use crate::arithmetic::u64::and_u64::AndU64;
use crate::arithmetic::u64::div2_u64::Div2U64;
use crate::arithmetic::u64::sub_u64::SubU64;
use crate::arithmetic::u64::wrapping_mul_u64::WrappingMulU64;
use crate::library::Library;
use crate::snippet::{DataType, Snippet};
use crate::{get_init_tvm_stack, push_hashable, ExecutionState};

#[derive(Clone)]
pub struct PopCountU64;

impl Snippet for PopCountU64 {
    fn entrypoint(&self) -> String {
        "tasm_arithmetic_u64_popcount".to_string()
    }

    fn inputs(&self) -> Vec<String>
    where
        Self: Sized,
    {
        vec!["value_hi".to_string(), "value_lo".to_string()]
    }

    fn input_types(&self) -> Vec<DataType> {
        vec![DataType::U64]
    }

    fn output_types(&self) -> Vec<DataType> {
        vec![DataType::U32]
    }

    fn outputs(&self) -> Vec<String>
    where
        Self: Sized,
    {
        vec!["popcount".to_string()]
    }

    fn stack_diff(&self) -> isize
    where
        Self: Sized,
    {
        -1
    }

    fn function_body(&self, library: &mut Library) -> String {
        let entrypoint = self.entrypoint();
        let div2_u64 = library.import(Box::new(Div2U64));
        let and_u64 = library.import(Box::new(AndU64));
        let add_u64 = library.import(Box::new(AddU64));
        let sub_u64 = library.import(Box::new(SubU64));
        let wrapping_mul_u64 = library.import(Box::new(WrappingMulU64));
        let m1 = BFieldElement::new(0x5555555555555555u64);
        let m2 = BFieldElement::new(0x3333333333333333u64);
        let m4 = BFieldElement::new(0x0f0f0f0f0f0f0f0fu64);
        let h01 = BFieldElement::new(0x0101010101010101u64);

        // TODO: All right shifting here can be replaced by left shifting and splitting
        format!(
            "
            // Before: _ x_hi x_lo
            // After: _ popcount
            {entrypoint}:
                dup1
                dup1
                call {div2_u64}
                // x_hi x_lo (x >> 1)_hi (x >> 1)_lo

                push {m1}
                split
                call {and_u64}
                // x_hi x_lo ((x >> 1) & m1)_hi ((x >> 1) & m1)_lo

                swap2 swap1 swap3 swap1
                // ((x >> 1) & m1)_hi ((x >> 1) & m1)_lo x_hi x_lo

                call {sub_u64}
                // (x - mask)_hi (x - mask)_lo

                // rename (x - mask) -> x
                // x_hi x_lo
                dup1
                dup1

                call {div2_u64}
                call {div2_u64}
                // _ x_hi x_lo (x >> 2)_hi (x >> 2)_lo

                push {m2}
                split
                call {and_u64}
                // _ x_u64 (x >> 2) & m2)_u64

                swap2 swap1 swap3 swap1
                // _ (x >> 2) & m2)_u64 x_u64

                push {m2}
                split
                call {and_u64}
                // _ (x >> 2) & m2)_u64 (x & m2)_u64

                call {add_u64}
                // _ (x >> 2) & m2)_u64 + (x & m2)_u64

                // rename (x & m2) + ((x >> 2) & m2) -> x
                // _ x_hi x_lo

                dup1 dup1
                // _ x_u64 x_u64

                call {div2_u64}
                call {div2_u64}
                call {div2_u64}
                call {div2_u64}
                // _ x_u64 (x >> 4)_u64

                call {add_u64}
                // _ (x + (x >> 4))_u64

                push {m4}
                split
                call {and_u64}
                // _ ((x + (x >> 4)) & m4)_u64

                push {h01}
                split
                call {wrapping_mul_u64}
                // _ (x * h01)_u64

                // Read top 8 bits from `(x * h01)_u64`
                pop         // _ (x * h01)_hi
                push 256    // _ (x * h01)_hi * (1 << 8)
                mul         // _ (x * h01)_hi << 8
                split       // _ ((x * h01)_hi << 8)_hi ((x * h01)_hi << 8)_lo
                pop         // _ ((x * h01)_hi << 8)_hi

                return

            "
        )
    }

    fn crash_conditions() -> Vec<String>
    where
        Self: Sized,
    {
        vec!["Input are not valid u32s".to_string()]
    }

    fn gen_input_states(&self) -> Vec<ExecutionState>
    where
        Self: Sized,
    {
        let mut rng = rand::thread_rng();

        let mut ret = vec![];
        for _ in 0..100 {
            ret.push(prepare_state(rng.next_u64()));
        }

        ret
    }

    fn common_case_input_state(&self) -> ExecutionState
    where
        Self: Sized,
    {
        prepare_state(1 << 60)
    }

    fn worst_case_input_state(&self) -> ExecutionState
    where
        Self: Sized,
    {
        prepare_state(1 << 60)
    }

    fn rust_shadowing(
        &self,
        stack: &mut Vec<BFieldElement>,
        _std_in: Vec<BFieldElement>,
        _secret_in: Vec<BFieldElement>,
        _memory: &mut std::collections::HashMap<BFieldElement, BFieldElement>,
    ) where
        Self: Sized,
    {
        // top element on stack
        let a_lo: u32 = stack.pop().unwrap().try_into().unwrap();
        let a_hi: u32 = stack.pop().unwrap().try_into().unwrap();
        let a = ((a_hi as u64) << 32) + a_lo as u64;

        let pop_count = a.count_ones();

        stack.push(BFieldElement::new(pop_count as u64));
    }
}

fn prepare_state(a: u64) -> ExecutionState {
    let a = U32s::<2>::try_from(a).unwrap();
    let mut init_stack = get_init_tvm_stack();
    push_hashable(&mut init_stack, &a);
    ExecutionState::with_stack(init_stack)
}

#[cfg(test)]
mod tests {

    use crate::snippet_bencher::bench_and_write;
    use crate::test_helpers::rust_tasm_equivalence_prop_new;

    use super::*;

    #[test]
    fn popcount_u64_test() {
        rust_tasm_equivalence_prop_new(PopCountU64);
    }

    #[test]
    fn popcount_u64_benchmark() {
        bench_and_write(PopCountU64);
    }
}
