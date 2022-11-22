#[allow(dead_code)]

pub const SAMPLE_WEIGHTS: &str = "
    sample_weights:
        return
";

#[cfg(test)]
mod sample_weights_tests {
    use itertools::Itertools;
    use num::Zero;
    use rand::Rng;
    use rayon::prelude::{IndexedParallelIterator, IntoParallelIterator, ParallelIterator};

    use triton_vm::vm::Program;
    use twenty_first::shared_math::b_field_element::BFieldElement;
    use twenty_first::shared_math::rescue_prime_digest::Digest;
    use twenty_first::shared_math::rescue_prime_regular::RescuePrimeRegular;
    use twenty_first::shared_math::x_field_element::XFieldElement;
    use twenty_first::util_types::algebraic_hasher::{AlgebraicHasher, Hashable};

    use super::*;

    fn sample_weights<H: AlgebraicHasher>(seed: Digest, num_weights: usize) -> Vec<XFieldElement> {
        let mut digests = Vec::with_capacity(num_weights);
        (0..num_weights)
            .into_par_iter()
            .map(|counter: usize| sample_weight::<H>(&seed, counter))
            .collect_into_vec(&mut digests);
        digests
    }

    fn sample_weight<H: AlgebraicHasher>(seed: &Digest, counter: usize) -> XFieldElement {
        let zero = BFieldElement::zero();
        let counter = BFieldElement::new(counter as u64);
        let left = Digest::new([zero, zero, zero, zero, counter]);
        let [_, _, w0, w1, w2] = H::hash_pair(&left, seed).values();

        XFieldElement::new([w0, w1, w2])
    }

    #[test]
    fn run_sample_weights_equivalency_test() {
        type H = RescuePrimeRegular;
        let mut rng = rand::thread_rng();
        let num_weights = 4;
        let seed: Digest = rng.gen();

        let rust_output: Vec<BFieldElement> = sample_weights::<H>(seed, num_weights)
            .iter()
            .flat_map(|xfe| xfe.coefficients)
            .collect();

        let mut stdin = seed.to_sequence();
        stdin.push(BFieldElement::new(num_weights as u64));

        let test_sample_weights: &str = "
            main:
                read_io
                read_io
                read_io
                read_io
                read_io
                read_io
                call sample_weights
                halt
        ";

        let test_code = format!("{test_sample_weights}{SAMPLE_WEIGHTS}");

        let program = Program::from_code(&test_code).unwrap();

        let (_states, tasm_output, _err) = program.run(stdin, vec![]);

        for state in _states.iter() {
            println!("{}", state);
        }

        let a = rust_output.iter().map(|bfe| bfe.emojihash()).join(", ");
        let b = tasm_output.iter().map(|bfe| bfe.emojihash()).join(", ");
        assert_eq!(
            rust_output, tasm_output,
            "\nrust_output = {}\ntasm_output = {}",
            a, b,
        );

        // tasm:
        // 4. kør programmet halt, navngiv det sample-weights: &str
        // 5. assert at outputtet er identisk til rust-eqv. - dette skal selvfølgelig fejle til at starte med
        // 6. pop string sample-weights??
    }
}
