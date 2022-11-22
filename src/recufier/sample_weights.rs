#[allow(dead_code)]

pub const SAMPLE_WEIGHTS: &str = "
    sample_weights:
        return
";

#[cfg(test)]
mod sample_weights_tests {
    use rand::Rng;
    use triton_vm::{stark::StarkHasher, vm::Program};
    use twenty_first::{
        shared_math::{
            b_field_element::BFieldElement, rescue_prime_digest::Digest,
            x_field_element::XFieldElement,
        },
        util_types::algebraic_hasher::{AlgebraicHasher, Hashable},
    };

    use super::*;

    fn sample_weights(seed: Digest, num_weights: usize) -> Vec<XFieldElement> {
        StarkHasher::get_n_hash_rounds(&seed, num_weights)
            .iter()
            .map(XFieldElement::sample)
            .collect()
    }

    #[test]
    fn run_sample_weights_equivalency_test() {
        let mut rng = rand::thread_rng();
        let num_weights = 5;
        let seed: Digest = rng.gen();

        let rust_output: Vec<BFieldElement> = sample_weights(seed, num_weights)
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

        let (_states, tasm_stdout, _err) = program.run(stdin, vec![]);

        for state in _states.iter() {
            println!("{}", state);
        }

        assert_eq!(rust_output, tasm_stdout)

        // tasm:
        // 4. kør programmet halt, navngiv det sample-weights: &str
        // 5. assert at outputtet er identisk til rust-eqv. - dette skal selvfølgelig fejle til at starte med
        // 6. pop string sample-weights??
    }
}
