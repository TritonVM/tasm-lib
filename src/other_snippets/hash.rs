use num::Zero;

use twenty_first::shared_math::b_field_element::BFieldElement;
use twenty_first::shared_math::rescue_prime_digest::Digest;
use twenty_first::shared_math::rescue_prime_regular::{RescuePrimeRegular, DIGEST_LENGTH};
use twenty_first::util_types::algebraic_hasher::AlgebraicHasher;

use crate::{execute, ExecutionResult};

type _H = RescuePrimeRegular;

fn _top_digests(stack: &[BFieldElement]) -> (Digest, Digest) {
    let height = stack.len();

    let st0 = stack[height - 1];
    let st1 = stack[height - 2];
    let st2 = stack[height - 3];
    let st3 = stack[height - 4];
    let st4 = stack[height - 5];
    let left = Digest::new([st0, st1, st2, st3, st4]);

    let st5 = stack[height - 6];
    let st6 = stack[height - 7];
    let st7 = stack[height - 8];
    let st8 = stack[height - 9];
    let st9 = stack[height - 10];
    let right = Digest::new([st5, st6, st7, st8, st9]);

    (left, right)
}

fn _hash_tasm(stack: &mut Vec<BFieldElement>) -> ExecutionResult {
    let code: &str = "hash";
    execute(code, stack, 0, vec![], vec![])
}

fn _hash_rust(stack: &mut Vec<BFieldElement>) {
    let (left, right) = _top_digests(stack);
    let result = _H::hash_pair(&left, &right);

    let result_values = result.values();

    let height = stack.len();
    for i in 0..DIGEST_LENGTH {
        stack[height - i - 1] = BFieldElement::zero();
        stack[height - i - DIGEST_LENGTH - 1] = result_values[i];
    }
}

#[cfg(test)]
mod tests {
    use crate::get_init_tvm_stack;

    use super::*;

    #[test]
    fn hash_zeros_test_2() {
        let init_stack = get_init_tvm_stack();

        let mut tasm_stack = init_stack.clone();
        let execution_result = _hash_tasm(&mut tasm_stack);
        println!("Cycle count for `hash`: {}", execution_result.cycle_count);
        println!(
            "Hash table height for `hash`: {}",
            execution_result.hash_table_height
        );

        let mut rust_stack = init_stack;
        _hash_rust(&mut rust_stack);

        assert_eq!(
            tasm_stack, rust_stack,
            "Rust code must match TVM for `hash`"
        );
    }
}
