use triton_vm::prelude::*;
use twenty_first::prelude::*;

use crate::prelude::*;

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct HashFromStack {
    ty: DataType,
    ty_len: usize,
}

impl HashFromStack {
    /// # Panics
    ///
    /// Panics if the argument does not have statically-known length, or if that
    /// length is larger than or equal to [`Tip5::RATE`].
    pub fn new(ty: DataType) -> Self {
        let ty_len = ty
            .static_length()
            .expect("data type to hash should have static length");
        assert!(ty_len < Tip5::RATE, "type length should be small");

        Self { ty, ty_len }
    }
}

impl BasicSnippet for HashFromStack {
    fn parameters(&self) -> Vec<(DataType, String)> {
        vec![(self.ty.clone(), "preimage".to_string())]
    }
    fn return_values(&self) -> Vec<(DataType, String)> {
        vec![(DataType::Digest, "digest".to_string())]
    }

    fn entrypoint(&self) -> String {
        format!(
            "tasmlib_hashing_hash_from_stack___{}",
            self.ty.label_friendly_name()
        )
    }

    fn code(&self, _: &mut Library) -> Vec<LabelledInstruction> {
        let pad_single_zero = triton_asm!(
            push 0
            place {self.ty_len}
        );

        let num_zeros_in_pad = Tip5::RATE - self.ty_len - 1;
        let pad_zeros = vec![pad_single_zero; num_zeros_in_pad].concat();

        let entrypoint = self.entrypoint();
        triton_asm!(
            {entrypoint}:

                {&pad_zeros}
                // _ [0, …, 0] [preimage]

                push 1
                place {self.ty_len}
                // _ [0, …, 0] 1 [preimage]
                // _ [padded-preimage]

                sponge_init
                sponge_absorb
                sponge_squeeze

                pick 9
                pick 9
                pick 9
                pick 9
                pick 9
                pop 5

                return
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_prelude::*;

    impl Closure for HashFromStack {
        type Args = Vec<BFieldElement>;

        fn rust_shadow(&self, stack: &mut Vec<BFieldElement>) {
            let mut preimage = vec![];
            for _ in 0..self.ty_len {
                preimage.push(stack.pop().unwrap());
            }

            push_encodable(stack, &Tip5::hash_varlen(&preimage));
        }

        fn pseudorandom_args(&self, seed: [u8; 32], _: Option<BenchmarkCase>) -> Self::Args {
            self.ty.seeded_random_element(&mut StdRng::from_seed(seed))
        }

        fn set_up_test_stack(&self, args: Self::Args) -> Vec<BFieldElement> {
            let mut stack = self.init_stack_for_isolated_run();
            stack.extend(args.into_iter().rev());

            stack
        }
    }

    #[test]
    fn unit() {
        let types = [
            DataType::Bool,
            DataType::U32,
            DataType::U64,
            DataType::U128,
            DataType::Bfe,
            DataType::Xfe,
            DataType::Digest,
        ];
        for data_type in types {
            ShadowedClosure::new(HashFromStack::new(data_type)).test();
        }
    }
}

#[cfg(test)]
mod benches {
    use super::*;
    use crate::test_prelude::*;

    #[test]
    fn benchmark() {
        let types = [DataType::Bfe, DataType::Digest];
        for data_type in types {
            ShadowedClosure::new(HashFromStack::new(data_type)).bench();
        }
    }
}
