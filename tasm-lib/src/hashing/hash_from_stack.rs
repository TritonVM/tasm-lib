use triton_vm::isa::triton_asm;
use triton_vm::prelude::LabelledInstruction;
use triton_vm::prelude::Tip5;
use triton_vm::twenty_first::prelude::Sponge;

use crate::data_type::DataType;
use crate::library::Library;
use crate::prelude::BasicSnippet;

pub struct HashFromStack {
    pub data_type: DataType,
}

impl BasicSnippet for HashFromStack {
    fn inputs(&self) -> Vec<(DataType, String)> {
        vec![(self.data_type.clone(), "preimage".to_string())]
    }

    fn outputs(&self) -> Vec<(DataType, String)> {
        vec![(DataType::Digest, "digest".to_string())]
    }

    fn entrypoint(&self) -> String {
        format!(
            "tasmlib_hashing_hash_from_stack___{}",
            self.data_type.label_friendly_name()
        )
    }

    fn code(&self, _library: &mut Library) -> Vec<LabelledInstruction> {
        let preimage_size = self.data_type.stack_size();
        let size = self
            .data_type
            .static_length()
            .expect("Can only hash static-length data types from stack");
        assert_eq!(
            size, preimage_size,
            "Can only hash types that live on stack"
        );
        assert!(
            preimage_size < Tip5::RATE,
            "This snippet assumes small preimage size"
        );

        let num_zeros_in_pad = Tip5::RATE - preimage_size - 1;
        let zero_padding = triton_asm!(
            push 0
            place {preimage_size}
        );

        // _ val 0

        // _ 0 val
        let zero_padding = vec![zero_padding; num_zeros_in_pad].concat();
        let one_pad = triton_asm!(
            push 1
            place {preimage_size}
        );

        let pad = triton_asm!(
            // _ [preimage: data_type]
            {&zero_padding}
            {&one_pad}

            // _ [0, 0..0] 1 [preimage]
        );

        let entrypoint = self.entrypoint();
        triton_asm!(
            {entrypoint}:

                {&pad}
                // _ [padded-preimage]

                sponge_init
                sponge_absorb
                sponge_squeeze

                pick 5 pop 1
                pick 5 pop 1
                pick 5 pop 1
                pick 5 pop 1
                pick 5 pop 1

                return
        )
    }
}

#[cfg(test)]
mod tests {
    use rand::rngs::StdRng;
    use rand::SeedableRng;
    use triton_vm::prelude::BFieldElement;

    use super::*;
    use crate::push_encodable;
    use crate::snippet_bencher::BenchmarkCase;
    use crate::traits::closure::Closure;
    use crate::traits::closure::ShadowedClosure;
    use crate::traits::rust_shadow::RustShadow;

    impl Closure for HashFromStack {
        fn rust_shadow(&self, stack: &mut Vec<triton_vm::prelude::BFieldElement>) {
            let mut preimage = vec![];
            for _ in 0..self.data_type.stack_size() {
                preimage.push(stack.pop().unwrap());
            }

            let digest = Tip5::hash_varlen(&preimage);

            push_encodable(stack, &digest);
        }

        type Args = Vec<BFieldElement>;

        fn pseudorandom_args(&self, seed: [u8; 32], _: Option<BenchmarkCase>) -> Self::Args {
            let mut rng = StdRng::from_seed(seed);

            self.data_type.seeded_random_element(&mut rng)
        }

        fn set_up_test_stack(&self, args: Self::Args) -> Vec<BFieldElement> {
            let mut stack = self.init_stack_for_isolated_run();
            for b in args.iter().rev() {
                stack.push(*b);
            }
            stack
        }
    }

    #[test]
    fn hash_from_stack_pbt() {
        let primitives = [
            DataType::Bool,
            DataType::U32,
            DataType::U64,
            DataType::U128,
            DataType::Bfe,
            DataType::Xfe,
            DataType::Digest,
        ];
        for data_type in primitives {
            ShadowedClosure::new(HashFromStack { data_type }).test();
        }
    }
}

#[cfg(test)]
mod benches {
    use super::*;
    use crate::traits::closure::ShadowedClosure;
    use crate::traits::rust_shadow::RustShadow;

    #[test]
    fn hash_from_stack_bench_bfe() {
        ShadowedClosure::new(HashFromStack {
            data_type: DataType::Bfe,
        })
        .bench()
    }

    #[test]
    fn hash_from_stack_bench_digest() {
        ShadowedClosure::new(HashFromStack {
            data_type: DataType::Digest,
        })
        .bench()
    }
}
