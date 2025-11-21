use triton_vm::prelude::*;

use crate::arithmetic::bfe::primitive_root_of_unity::PrimitiveRootOfUnity;
use crate::arithmetic::u32::next_power_of_two::NextPowerOfTwo;
use crate::prelude::*;
use crate::verifier::fri::verify::fri_verify_type;

/// Mimics Triton-VM's FRI parameter-derivation method, but doesn't allow for a FRI-domain length
/// of 2^32 bc the domain length is stored in a single word/a `u32`.
#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub struct DeriveFriFromStark {
    pub stark: Stark,
}

impl DeriveFriFromStark {
    fn derive_fri_field_values(&self, library: &mut Library) -> Vec<LabelledInstruction> {
        let next_power_of_two = library.import(Box::new(NextPowerOfTwo));
        let domain_generator = library.import(Box::new(PrimitiveRootOfUnity));

        let num_trace_randomizers = self.stark.num_trace_randomizers;
        let fri_expansion_factor = self.stark.fri_expansion_factor;
        let interpolant_codeword_length_code = triton_asm!(
            // _ padded_height

            push {num_trace_randomizers}
            add
            // _ (padded_height + num_trace_randomizers)

            call {next_power_of_two}
            // _ next_pow2(padded_height + num_trace_randomizers)
            // _ interpolant_codeword_length
        );
        let fri_domain_length = triton_asm!(
            // _ interpolant_codeword_length
            push {fri_expansion_factor}
            mul
            // _ (interpolant_codeword_length * fri_expansion_factor)
            // _ fri_domain_length
        );

        let domain_offset = BFieldElement::generator();
        let num_collinearity_checks = self.stark.num_collinearity_checks;
        let expansion_factor = self.stark.fri_expansion_factor;
        triton_asm!(
            // _ padded_height

            {&interpolant_codeword_length_code}
            {&fri_domain_length}
            // _ fri_domain_length

            push {num_collinearity_checks}
            // _ fri_domain_length num_collinearity_checks

            push {expansion_factor}
            // _ fri_domain_length num_collinearity_checks expansion_factor

            swap 2
            // _ expansion_factor num_collinearity_checks fri_domain_length

            push {domain_offset}
            // _ expansion_factor num_collinearity_checks fri_domain_length domain_offset

            dup 1
            split
            call {domain_generator}
            // _ expansion_factor num_collinearity_checks fri_domain_length domain_offset domain_generator
        )
    }
}

impl BasicSnippet for DeriveFriFromStark {
    fn parameters(&self) -> Vec<(DataType, String)> {
        vec![(DataType::U32, "padded_height".to_owned())]
    }

    fn return_values(&self) -> Vec<(DataType, String)> {
        vec![(
            DataType::StructRef(fri_verify_type()),
            "*fri_verify".to_owned(),
        )]
    }

    fn entrypoint(&self) -> String {
        "tasmlib_verifier_fri_derive_from_stark".to_owned()
    }

    fn code(&self, library: &mut Library) -> Vec<LabelledInstruction> {
        let entrypoint = self.entrypoint();
        let derive_fri_field_values = self.derive_fri_field_values(library);
        let dyn_malloc = library.import(Box::new(DynMalloc));

        triton_asm!(
            {entrypoint}:
                // _ padded_height

                {&derive_fri_field_values}
                // _ fri_domain_length domain_offset domain_generator num_collinearity_checks expansion_factor

                call {dyn_malloc}
                // _ fri_domain_length domain_offset domain_generator num_collinearity_checks expansion_factor *fri_verify

                write_mem 5
                // _ (*fri_verify + 5)

                push -5
                add
                // _ *fri_verify

                return
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::rust_shadowing_helper_functions;
    use crate::test_prelude::*;
    use crate::verifier::fri::verify::FriVerify;

    #[test]
    fn fri_param_derivation_default_stark_pbt() {
        ShadowedFunction::new(DeriveFriFromStark {
            stark: Stark::default(),
        })
        .test();
    }

    #[proptest(cases = 10)]
    fn fri_param_derivation_pbt_pbt(#[strategy(arb())] stark: Stark) {
        ShadowedFunction::new(DeriveFriFromStark { stark }).test();
    }

    impl Function for DeriveFriFromStark {
        fn rust_shadow(
            &self,
            stack: &mut Vec<BFieldElement>,
            memory: &mut HashMap<BFieldElement, BFieldElement>,
        ) {
            let padded_height: u32 = stack.pop().unwrap().try_into().unwrap();
            let fri_from_tvm = self.stark.fri(padded_height.try_into().unwrap()).unwrap();
            let local_fri: FriVerify = fri_from_tvm.into();
            let fri_pointer =
                rust_shadowing_helper_functions::dyn_malloc::dynamic_allocator(memory);
            encode_to_memory(memory, fri_pointer, &local_fri);
            stack.push(fri_pointer)
        }

        fn pseudorandom_initial_state(
            &self,
            seed: [u8; 32],
            bench_case: Option<BenchmarkCase>,
        ) -> FunctionInitialState {
            let padded_height: u32 = match bench_case {
                Some(BenchmarkCase::CommonCase) => 2u32.pow(21),
                Some(BenchmarkCase::WorstCase) => 2u32.pow(23),
                None => {
                    let mut rng = StdRng::from_seed(seed);
                    let mut padded_height = 2u32.pow(rng.random_range(8..=25));

                    // Don't test parameters that result in too big FRI domains, i.e. larger
                    // than 2^32. Note that this also excludes 2^32 as domain length because
                    // the type used to hold this value is a `u32` in this repo. I think such a
                    // large FRI domain is unfeasible anyway, so I'm reasonably comfortable
                    // excluding that option.
                    while self.stark.fri(padded_height as usize * 2).is_err() {
                        padded_height /= 2;
                    }

                    assert!(padded_height >= 2u32.pow(8));

                    padded_height
                }
            };

            FunctionInitialState {
                stack: [
                    self.init_stack_for_isolated_run(),
                    vec![padded_height.into()],
                ]
                .concat(),
                memory: HashMap::default(),
            }
        }
    }
}
