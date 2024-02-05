use std::collections::HashMap;

use num_traits::Zero;
use rand::rngs::StdRng;
use rand::Rng;
use rand::SeedableRng;
use triton_vm::prelude::*;

use crate::data_type::DataType;
use crate::empty_stack;
use crate::field;
use crate::memory::encode_to_memory;
use crate::recufier::fri_verify::FriVerify;
use crate::snippet_bencher::BenchmarkCase;
use crate::structure::tasm_object::TasmObject;
use crate::traits::basic_snippet::BasicSnippet;
use crate::traits::function::Function;
use crate::traits::function::FunctionInitialState;

/// Compute domain\[index\]^(1<<round)
#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
pub struct GetColinearityCheckX;

impl BasicSnippet for GetColinearityCheckX {
    fn inputs(&self) -> Vec<(DataType, String)> {
        vec![
            (DataType::VoidPointer, "*fri_verify".to_string()),
            (DataType::U32, "index".to_string()),
            (DataType::U32, "round".to_string()),
        ]
    }

    fn outputs(&self) -> Vec<(DataType, String)> {
        vec![(DataType::Xfe, "evaluation_argument".to_string())]
    }

    fn entrypoint(&self) -> String {
        "tasm_recufier_get_colinearity_check_x".to_string()
    }

    fn code(&self, _library: &mut crate::library::Library) -> Vec<LabelledInstruction> {
        let entrypoint = self.entrypoint();
        let domain_offset = field!(FriVerify::domain_offset);
        let domain_generator = field!(FriVerify::domain_generator);

        triton_asm! {
            // BEFORE: _ *fri_verify index round
            // AFTER:  _ x2 x1 x0
            {entrypoint}:
                dup 2               // _ *fri_verify index round *fri_verify
                {&domain_generator} // _ *fri_verify index round *domain_generator
                read_mem 1 pop 1    // _ *fri_verify index round domain_generator
                dup 2               // _ *fri_verify index round domain_generator index
                swap 1 pow          // _ *fri_verify index round domain_generator^index

                dup 3               // _ *fri_verify index round domain_generator^index *fri_verify
                {&domain_offset}    // _ *fri_verify index round domain_generator^index *domain_offset
                read_mem 1 pop 1    // _ *fri_verify index round domain_generator^index domain_offset
                mul                 // _ *fri_verify index round domain_generator^index*domain_offset

                dup 1 push 2 pow    // _ *fri_verify index round domain_generator^index*domain_offset 2^round

                swap 1 pow          // _ *fri_verify index round (domain_generator^index*domain_offset)^(1<<round)

                swap 3 pop 3        // _ (g^i*o)^(1<<r)
                push 0 push 0 swap 2
                                    // _ 0 0 (g^i*o)^(1<<r)
                return
        }
    }
}

impl Function for GetColinearityCheckX {
    fn rust_shadow(
        &self,
        stack: &mut Vec<BFieldElement>,
        memory: &mut HashMap<BFieldElement, BFieldElement>,
    ) {
        // read stack arguments
        let round = stack.pop().unwrap().value() as usize;
        let index = stack.pop().unwrap().value() as u32;
        let fri_verify_address = stack.pop().unwrap();

        // read fri_verify object from memory
        let fri_verify = FriVerify::decode_from_memory(memory, fri_verify_address).unwrap();

        // invoke actual function
        let x = fri_verify.get_colinearity_check_x(index, round);

        // push to stack
        stack.push(x.coefficients[2]);
        stack.push(x.coefficients[1]);
        stack.push(x.coefficients[0]);
    }

    fn pseudorandom_initial_state(
        &self,
        seed: [u8; 32],
        bench_case: Option<BenchmarkCase>,
    ) -> FunctionInitialState {
        let mut rng: StdRng = SeedableRng::from_seed(seed);
        let round = if let Some(case) = bench_case {
            match case {
                BenchmarkCase::CommonCase => 10,
                BenchmarkCase::WorstCase => 20,
            }
        } else {
            rng.gen_range(0..10)
        };
        let fri_domain_length = if let Some(case) = bench_case {
            match case {
                BenchmarkCase::CommonCase => 1 << 20,
                BenchmarkCase::WorstCase => 1 << 25,
            }
        } else {
            1 << (rng.gen_range(0..5) + round)
        };
        let index = rng.gen_range(0..fri_domain_length);

        let fri_verify = FriVerify::new(rng.gen(), fri_domain_length, 4, 40);

        let mut memory = HashMap::<BFieldElement, BFieldElement>::new();
        let fri_verify_address = BFieldElement::zero();
        encode_to_memory(&mut memory, fri_verify_address, fri_verify);

        let mut stack = empty_stack();
        stack.push(fri_verify_address);
        stack.push(BFieldElement::new(index as u64));
        stack.push(BFieldElement::new(round as u64));

        FunctionInitialState { stack, memory }
    }
}

#[cfg(test)]
mod test {
    use crate::traits::function::ShadowedFunction;
    use crate::traits::rust_shadow::RustShadow;

    use super::GetColinearityCheckX;

    #[test]
    fn test() {
        ShadowedFunction::new(GetColinearityCheckX).test();
    }
}

#[cfg(test)]
mod bench {
    use crate::traits::function::ShadowedFunction;
    use crate::traits::rust_shadow::RustShadow;

    use super::GetColinearityCheckX;

    #[test]
    fn bench() {
        ShadowedFunction::new(GetColinearityCheckX).bench();
    }
}
