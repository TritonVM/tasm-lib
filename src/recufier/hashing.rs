#[cfg(test)]
mod hashing_tests {
    use num::Zero;

    use triton_vm::ord_n::Ord16::*;
    use triton_vm::vm::Program;
    use twenty_first::shared_math::b_field_element::BFieldElement;
    use twenty_first::shared_math::rescue_prime_digest::Digest;
    use twenty_first::shared_math::rescue_prime_regular::{RescuePrimeRegular, DIGEST_LENGTH};
    use twenty_first::util_types::algebraic_hasher::AlgebraicHasher;
    use twenty_first::util_types::emojihash_trait::Emojihash;

    #[test]
    fn hash_zeros_test() {
        type H = RescuePrimeRegular;
        let program = Program::from_code("hash halt").unwrap();
        let (trace, _output, _err) = program.run(vec![], vec![]);
        let last_state = trace.last().unwrap();

        let tasm = Digest::new([ST5, ST6, ST7, ST8, ST9].map(|x| last_state.op_stack.safe_peek(x)));
        let zero = Digest::new([BFieldElement::zero(); DIGEST_LENGTH]);
        let rust = H::hash_pair(&zero, &zero);

        let last_stack = {
            let mut tmp = last_state.op_stack.stack.clone();
            tmp.reverse();
            tmp
        };
        println!("stack = {}", last_stack.emojihash());
        println!("rust = {}", rust.emojihash());
        assert_eq!(tasm, rust);
    }
}
