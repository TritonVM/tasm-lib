use std::collections::HashMap;

use twenty_first::shared_math::b_field_element::BFieldElement;
use twenty_first::shared_math::rescue_prime_digest::Digest;

use crate::snippet::Snippet;

pub struct EqDigest();

impl Snippet for EqDigest {
    fn stack_diff() -> isize {
        -9
    }

    fn entrypoint() -> &'static str {
        "eq_digest"
    }

    fn function_body(_library: &mut crate::library::Library) -> String {
        let entrypoint = Self::entrypoint();
        format!(
            "
            // Before: _ b4 b3 b2 b1 b0 a4 a3 a2 a1 a0
            // After: _ (a3 = b3)·(a2 = b2)·(a1 = b1)·(a4 = b4)·(b0 = a0)
            {entrypoint}:
                swap6  // _ b4 b3 b2 a0 b0 a4 a3 a2 a1 b1
                eq     // _ b4 b3 b2 a0 b0 a4 a3 a2 (a1 = b1)
                swap6  // _ b4 b3 (a1 = b1) a0 b0 a4 a3 a2 b2
                eq     // _ b4 b3 (a1 = b1) a0 b0 a4 a3 (a2 = b2)
                swap6  // _ b4 (a2 = b2) (a1 = b1) a0 b0 a4 a3 b3
                eq     // _ b4 (a2 = b2) (a1 = b1) a0 b0 a4 (a3 = b3)
                swap6  // _ (a3 = b3) (a2 = b2) (a1 = b1) a0 b0 a4 b4
                eq     // _ (a3 = b3) (a2 = b2) (a1 = b1) a0 b0 (a4 = b4)
                swap2  // _ (a3 = b3) (a2 = b2) (a1 = b1) (a4 = b4) b0 a0
                eq     // _ (a3 = b3) (a2 = b2) (a1 = b1) (a4 = b4) (b0 = a0)
                
                mul
                mul
                mul
                mul    // (a3 = b3)·(a2 = b2)·(a1 = b1)·(a4 = b4)·(b0 = a0)

                return
            "
        )
    }

    fn rust_shadowing(
        stack: &mut Vec<BFieldElement>,
        _std_in: Vec<BFieldElement>,
        _secret_in: Vec<BFieldElement>,
        _memory: &mut HashMap<BFieldElement, BFieldElement>,
    ) {
        let digest_a = Digest::new([
            stack.pop().unwrap(),
            stack.pop().unwrap(),
            stack.pop().unwrap(),
            stack.pop().unwrap(),
            stack.pop().unwrap(),
        ]);

        let digest_b = Digest::new([
            stack.pop().unwrap(),
            stack.pop().unwrap(),
            stack.pop().unwrap(),
            stack.pop().unwrap(),
            stack.pop().unwrap(),
        ]);

        stack.push(BFieldElement::new((digest_a == digest_b) as u64));
    }
}

#[cfg(test)]
mod tests {
    use rand::Rng;
    use twenty_first::shared_math::rescue_prime_digest::Digest;
    use twenty_first::util_types::algebraic_hasher::Hashable;

    use crate::get_init_tvm_stack;
    use crate::test_helpers::rust_tasm_equivalence_prop;

    use super::*;

    fn eq_digest_prop(digest_a: Digest, digest_b: Digest) {
        let mut stack = get_init_tvm_stack();
        stack.append(&mut digest_b.to_sequence().into_iter().rev().collect());
        stack.append(&mut digest_a.to_sequence().into_iter().rev().collect());

        let stdin = &[];
        let secret_in = &[];
        let mut memory = HashMap::default();
        let words_allocated = 0;
        let expected = None;
        let _execution_result = rust_tasm_equivalence_prop::<EqDigest>(
            &stack,
            stdin,
            secret_in,
            &mut memory,
            words_allocated,
            expected,
        );
    }

    #[test]
    fn eq_digest_test() {
        {
            let digest_a = Digest::new([1, 2, 3, 4, 5].map(BFieldElement::new));
            let digest_b = Digest::new([6, 7, 8, 9, 10].map(BFieldElement::new));

            // Rust and tasm are equivalent when `eq_digest` is true.
            eq_digest_prop(digest_a, digest_a);

            // Rust and tasm are equivalent when `eq_digest` is false.
            eq_digest_prop(digest_a, digest_b);

            let mut rng = rand::thread_rng();
            let digest_c: Digest = rng.gen();
            eq_digest_prop(digest_a, digest_c);
        }
    }
}
