use triton_vm::prelude::*;

use crate::data_type::DataType;
use crate::field;
use crate::library::Library;
use crate::traits::basic_snippet::BasicSnippet;
use crate::verifier::fri::verify::FriVerify;

pub struct NumberOfRounds {}

impl BasicSnippet for NumberOfRounds {
    fn inputs(&self) -> Vec<(DataType, String)> {
        vec![(DataType::VoidPointer, "*fri".to_string())]
    }

    fn outputs(&self) -> Vec<(DataType, String)> {
        vec![(DataType::U32, "num_rounds".to_string())]
    }

    fn entrypoint(&self) -> String {
        "tasmlib_verifier_fri_number_of_rounds".to_string()
    }

    fn code(&self, _library: &mut Library) -> Vec<LabelledInstruction> {
        let entrypoint = self.entrypoint();
        let domain_length = field!(FriVerify::domain_length);
        let expansion_factor = field!(FriVerify::expansion_factor);
        let num_collinearity_checks = field!(FriVerify::num_collinearity_checks);

        triton_asm! {
            // BEFORE: _ *fri_verify
            // AFTER: _  num_rounds
            {entrypoint}:
                // calculate number of rounds
                dup 0 {&domain_length}      // _ *fri_verify *domain_length
                read_mem 1 pop 1            // _ *fri_verify domain_length
                hint domain_length = stack[0]

                dup 1 {&expansion_factor}   // _ *fri_verify domain_length *expansion_factor
                read_mem 1 pop 1            // _ *fri_verify domain_length expansion_factor
                hint expansion_factor = stack[0]

                swap 1 div_mod pop 1        // _ *fri_verify first_round_code_dimension
                log_2_floor                 // _ *fri_verify max_num_rounds
                hint max_num_rounds = stack[0]

                dup 1 {&num_collinearity_checks}
                read_mem 1 pop 1            // _ *fri_verify max_num_rounds num_collinearity_checks
                hint num_collinearity_checks = stack[0]

                log_2_floor push 1 add      // _ *fri_verify max_num_rounds num_rounds_checking_most_locations

                dup 1 dup 1 lt              // _ *fri_verify max_num_rounds num_rounds_checking_most_locations (num_rounds_checking_most_locations<max_num_rounds)
                swap 2 push -1 mul add      // _ *fri_verify (num_rounds_checking_most_locations<max_num_rounds) num_rounds_checking_most_locations-max_num_rounds
                mul push -1 mul             // _ *fri_verify if(num_rounds_checking_most_locations<max_num_rounds){max_num_rounds-num_rounds_checking_most_locations}else{0}
                                            // _ *fri_verify num_rounds
                hint num_rounds = stack[0]

                swap 1 pop 1                // _ num_rounds
                return
        }
    }
}

#[cfg(test)]
mod test {
    use std::collections::HashMap;

    use rand::prelude::*;
    use triton_vm::arithmetic_domain::ArithmeticDomain;
    use triton_vm::fri::Fri;
    use triton_vm::twenty_first::math::traits::PrimitiveRootOfUnity;

    use super::*;
    use crate::memory::dyn_malloc::DYN_MALLOC_ADDRESS;
    use crate::memory::encode_to_memory;
    use crate::prelude::TasmObject;
    use crate::snippet_bencher::BenchmarkCase;
    use crate::traits::function::Function;
    use crate::traits::function::FunctionInitialState;
    use crate::traits::function::ShadowedFunction;
    use crate::traits::rust_shadow::RustShadow;

    impl Function for NumberOfRounds {
        fn rust_shadow(
            &self,
            stack: &mut Vec<BFieldElement>,
            memory: &mut HashMap<BFieldElement, BFieldElement>,
        ) {
            let fri_verify = FriVerify::decode_from_memory(memory, stack.pop().unwrap()).unwrap();
            stack.push(BFieldElement::new(fri_verify.num_rounds() as u64));
        }

        fn pseudorandom_initial_state(
            &self,
            seed: [u8; 32],
            bench_case: Option<BenchmarkCase>,
        ) -> FunctionInitialState {
            let mut rng: StdRng = SeedableRng::from_seed(seed);
            let rate_entropy = rng.gen_range(1..16);
            let num_colinearity_checks = f64::ceil(160.0 / (rate_entropy as f64)) as usize;
            let domain_length = match bench_case {
                Some(BenchmarkCase::CommonCase) => 1 << 17,
                Some(BenchmarkCase::WorstCase) => 1 << 22,
                None => 1 << rng.gen_range(rate_entropy..=22),
            };
            let fri_verify = FriVerify {
                expansion_factor: 1 << rate_entropy,
                num_collinearity_checks: num_colinearity_checks as u32,
                domain_length,
                domain_offset: BFieldElement::new(7),
                domain_generator: BFieldElement::primitive_root_of_unity(domain_length.into())
                    .unwrap(),
            };
            let mut stack = self.init_stack_for_isolated_run();
            let mut memory: HashMap<BFieldElement, BFieldElement> = HashMap::new();

            let address = DYN_MALLOC_ADDRESS;
            encode_to_memory(&mut memory, address, &fri_verify);
            stack.push(address);

            FunctionInitialState { stack, memory }
        }
    }

    #[test]
    fn test_shadow() {
        ShadowedFunction::new(NumberOfRounds {}).test()
    }

    #[test]
    fn shadow_agrees_with_canon() {
        let mut rng = thread_rng();
        let num_trials = 50;
        for _ in 0..num_trials {
            let rate_entropy = rng.gen_range(1..16);
            let expansion_factor = 1 << rate_entropy;
            let num_colinearity_checks = f64::ceil(160.0 / (rate_entropy as f64)) as usize;
            let domain_length = 1 << rng.gen_range(rate_entropy..=22);
            let domain_offset = BFieldElement::new(7);
            let fri_verify = FriVerify {
                expansion_factor: 1 << rate_entropy,
                num_collinearity_checks: num_colinearity_checks as u32,
                domain_length,
                domain_offset,
                domain_generator: BFieldElement::primitive_root_of_unity(domain_length.into())
                    .unwrap(),
            };

            let arithmetic_domain = ArithmeticDomain::of_length(domain_length.try_into().unwrap())
                .unwrap()
                .with_offset(domain_offset);
            let fri =
                Fri::new(arithmetic_domain, expansion_factor, num_colinearity_checks).unwrap();

            assert_eq!(fri.num_rounds(), fri_verify.num_rounds());
        }
    }
}
