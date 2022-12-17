use num::Zero;
use twenty_first::shared_math::b_field_element::BFieldElement;
use twenty_first::shared_math::rescue_prime_digest::Digest;
use twenty_first::shared_math::rescue_prime_regular::DIGEST_LENGTH;
use twenty_first::shared_math::x_field_element::EXTENSION_DEGREE;

use crate::library::Library;
use crate::snippet_trait::Snippet;

pub struct SampleWeights<const N: usize>();

impl<const N: usize> Snippet for SampleWeights<N> {
    fn stack_diff() -> isize {
        -(DIGEST_LENGTH as isize)
    }

    fn entrypoint() -> &'static str {
        "sample_weights"
    }

    fn function_body(library: &mut Library) -> String {
        let entrypoint = Self::entrypoint();
        let addr = library.kmalloc(EXTENSION_DEGREE * N);

        let call_gen_single_code: String = (0..N)
            .map(|i| {
                format!(
                    "
                    push {i}                       // _ s4 s3 s2 s1 s0 addr i
                    call {entrypoint}_gen_single   // _ s4 s3 s2 s1 s0 addr i w2 w1 w0
                    call {entrypoint}_store_weight // _ s4 s3 s2 s1 s0 (addr + 3) i
                    pop                            // _ s4 s3 s2 s1 s0 (addr + 3)
                    "
                )
            })
            .collect();

        format!(
            "
            // Before: _ s4 s3 s2 s1 s0
            // After: _
            {entrypoint}:
                push {addr}                    // _ s4 s3 s2 s1 s0 addr
                {call_gen_single_code}         // _ s4 s3 s2 s1 s0 (addr + 3*N)
                // push 0                         // _ s4 s3 s2 s1 s0 addr i
                // call {entrypoint}_gen_single   // _ s4 s3 s2 s1 s0 addr i w2 w1 w0
                // call {entrypoint}_store_weight // _ s4 s3 s2 s1 s0 (addr + 3) i

                // push 1 add                     // _ s4 s3 s2 s1 s0 (addr + 3) (i + 1)
                // call {entrypoint}_gen_single   // _ s4 s3 s2 s1 s0 addr (i + 1) w2 w1 w0
                // call {entrypoint}_store_weight // _ s4 s3 s2 s1 s0 (addr + 6) (i + 1)

                pop pop pop pop pop pop        // _
                return

            // Before: _ s4 s3 s2 s1 s0 addr i
            // After: _ s4 s3 s2 s1 s0 addr i w2 w1 w0
            {entrypoint}_gen_single:
                dup6 dup6 dup6 dup6 dup6       // _ s4 s3 s2 s1 s0 addr i s4 s3 s2 s1 s0
                push 0 push 0 push 0 push 0    // _ s4 s3 s2 s1 s0 addr i s4 s3 s2 s1 s0 0 0 0 0
                dup9                           // _ s4 s3 s2 s1 s0 addr i s4 s3 s2 s1 s0 0 0 0 0 i
                hash                           // _ s4 s3 s2 s1 s0 addr i _w4 _w3 w2 w1 w0 0 0 0 0 0
                pop pop pop pop pop            // _ s4 s3 s2 s1 s0 addr i _w4 _w3 w2 w1 w0
                swap3 pop                      // _ s4 s3 s2 s1 s0 addr i _w4 w0 w2 w1
                swap3 pop                      // _ s4 s3 s2 s1 s0 addr i w1 w0 w2
                swap2                          // _ s4 s3 s2 s1 s0 addr i w2 w0 w1
                swap1                          // _ s4 s3 s2 s1 s0 addr i w2 w1 w0
                return

            // Before: _ addr i w2 w1 w0
            // After: _ (addr + 3) i
            // Mem:
            //   mem[addr]     := w0
            //   mem[addr + 1] := w1
            //   mem[addr + 2] := w2
            {entrypoint}_store_weight:
                dup4                           // _ addr i w2 w1 w0 addr
                swap1                          // _ addr i w2 w1 addr w0
                write_mem                      // mem[addr] := w0
                pop                            // _ addr i w2 w1 addr

                push 1 add                     // _ addr i w2 w1 (addr + 1)
                swap1                          // _ addr i w2 (addr + 1) w1
                write_mem                      // mem[addr + 1] := w1
                pop                            // _ addr i w2 (addr + 1)

                push 1 add                     // _ addr i w2 (addr + 2)
                swap1                          // _ addr i (addr + 2) w2
                write_mem                      // mem[addr + 2] := w2
                pop                            // _ addr i (addr + 2)
                push 1 add                     // _ addr i (addr + 3)
                swap2                          // _ (addr + 3) i addr
                pop                            // _ (addr + 3) i
                return
            "
        )
    }

    fn rust_shadowing(
        stack: &mut Vec<BFieldElement>,
        _std_in: Vec<BFieldElement>,
        _secret_in: Vec<BFieldElement>,
    ) {
        let _seed = Digest::new(pop_many(stack));
    }
}

pub fn pop_many<const N: usize>(stack: &mut Vec<BFieldElement>) -> [BFieldElement; N] {
    let expectable = format!("At least {N} elements on the stack");
    let mut result = [BFieldElement::zero(); N];

    for i in 0..N {
        result[N - i - 1] = stack.pop().expect(&expectable)
    }

    result
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use itertools::Itertools;
    use triton_vm::table::challenges::AllChallenges;
    use twenty_first::shared_math::rescue_prime_digest::Digest;
    use twenty_first::shared_math::rescue_prime_regular::RescuePrimeRegular;
    use twenty_first::util_types::algebraic_hasher::{AlgebraicHasher, Hashable};

    use crate::get_init_tvm_stack;
    use crate::snippet_trait::rust_tasm_equivalence_prop;

    use super::*;

    fn prop_sample_weights<H: AlgebraicHasher, const N: usize>(seed: &Digest) {
        // FIXME: Move this to `rust_shadowing()` once it supports shadowing memory.
        let rust_sample_weights = H::sample_weights(seed, N)
            .iter()
            .flat_map(|xfe| xfe.coefficients)
            .collect::<Vec<_>>();

        let mut init_stack = get_init_tvm_stack();
        init_stack.append(&mut seed.to_sequence().into_iter().rev().collect());
        let execution_result =
            rust_tasm_equivalence_prop::<SampleWeights<N>>(&mut init_stack, &[], &[], None);

        let tvm_sample_weights = (0..EXTENSION_DEGREE * N)
            .map(|addr| BFieldElement::new(addr as u64))
            .map(|addr| execution_result.final_ram[&addr])
            .collect::<Vec<_>>();

        print_ram(&execution_result.final_ram);

        assert_eq!(
            rust_sample_weights, tvm_sample_weights,
            "sample weights must equal"
        )
    }

    fn print_ram(ram: &HashMap<BFieldElement, BFieldElement>) {
        for addr in ram
            .keys()
            .sorted_by(|a, b| Ord::cmp(&a.value(), &b.value()))
        {
            let value = ram[addr];
            println!("{addr}: {value}");
        }
    }

    #[test]
    fn sample_weights_test() {
        let seed = Digest::new([
            BFieldElement::new(2),
            BFieldElement::new(3),
            BFieldElement::new(5),
            BFieldElement::new(7),
            BFieldElement::new(11),
        ]);

        prop_sample_weights::<RescuePrimeRegular, 1>(&seed);
        prop_sample_weights::<RescuePrimeRegular, { AllChallenges::TOTAL_CHALLENGES }>(&seed);
    }
}
