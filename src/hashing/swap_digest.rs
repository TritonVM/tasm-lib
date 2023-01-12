use std::collections::HashMap;

use twenty_first::shared_math::b_field_element::BFieldElement;
use twenty_first::shared_math::rescue_prime_regular::DIGEST_LENGTH;

use crate::snippet::Snippet;

pub struct SwapDigest();

impl Snippet for SwapDigest {
    fn stack_diff() -> isize {
        0
    }

    fn entrypoint() -> &'static str {
        "swap_digest"
    }

    fn function_body(_library: &mut crate::library::Library) -> String {
        let entrypoint = Self::entrypoint();
        format!(
            "
            // Before: _ b4 b3 b2 b1 b0 a4 a3 a2 a1 a0
            // After:  _ a4 a3 a2 a1 a0 b4 b3 b2 b1 b0
            {entrypoint}:
                // a4 <-> b4:
                swap4 // _ b4 b3 b2 b1 b0 a0 a3 a2 a1 a4
                swap9 // _ a4 b3 b2 b1 b0 a0 a3 a2 a1 b4
                swap4 // _ a4 b3 b2 b1 b0 b4 a3 a2 a1 a0

                // a3 <-> b3:
                swap3 // _ a4 b3 b2 b1 b0 b4 a0 a2 a1 a3
                swap8 // _ a4 a3 b2 b1 b0 b4 a0 a2 a1 b3
                swap3 // _ a4 a3 b2 b1 b0 b4 b3 a2 a1 a0

                // a2 <-> b2
                swap2 // _ a4 a3 b2 b1 b0 b4 b3 a0 a1 a2
                swap7 // _ a4 a3 a2 b1 b0 b4 b3 a0 a1 b2
                swap2 // _ a4 a3 a2 b1 b0 b4 b3 b2 a1 a0

                // a1 <-> b1
                swap1 // _ a4 a3 a2 b1 b0 b4 b3 b2 a0 a1
                swap6 // _ a4 a3 a2 a1 b0 b4 b3 b2 a0 b1
                swap1 // _ a4 a3 a2 a1 b0 b4 b3 b2 b1 a0

                // a0 <-> b0
                swap5 // _ a4 a3 a2 a1 a0 b4 b3 b2 b1 b0

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
        let h = stack.len();
        for i in 0..DIGEST_LENGTH {
            stack.swap(h - i - 1, h - i - DIGEST_LENGTH - 1);
        }
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

    fn swap_digest_prop(digest_a: Digest, digest_b: Digest) {
        let mut stack = get_init_tvm_stack();
        stack.append(&mut digest_b.to_sequence().into_iter().rev().collect());
        stack.append(&mut digest_a.to_sequence().into_iter().rev().collect());

        let stdin = &[];
        let secret_in = &[];
        let mut memory = HashMap::default();
        let words_allocated = 0;
        let expected = None;
        let _execution_result = rust_tasm_equivalence_prop::<SwapDigest>(
            &stack,
            stdin,
            secret_in,
            &mut memory,
            words_allocated,
            expected,
        );
    }

    #[test]
    fn swap_digest_test() {
        {
            let digest_a = Digest::new([1, 2, 3, 4, 5].map(BFieldElement::new));
            let digest_b = Digest::new([6, 7, 8, 9, 10].map(BFieldElement::new));
            swap_digest_prop(digest_a, digest_b);
        }

        {
            let mut rng = rand::thread_rng();
            let digest_a: Digest = rng.gen();
            let digest_b: Digest = rng.gen();
            swap_digest_prop(digest_a, digest_b);
        }
    }
}
