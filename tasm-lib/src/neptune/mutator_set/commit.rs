use std::collections::HashMap;

use triton_vm::prelude::*;

use crate::prelude::*;
use crate::traits::basic_snippet::Reviewer;
use crate::traits::basic_snippet::SignOffFingerprint;

/// Compute the addition record for the mutator set AOCL from an `item`, the
/// sender's randomness, and the receiver's digest.
///
/// ### Behavior
///
/// ```text
/// BEFORE: _ [receiver_digest: Digest] [sender_randomness: Digest] [item: Digest]
/// AFTER:  _ [commitment: Digest]
/// ```
///
/// ### Preconditions
///
/// None.
///
/// ### Postconditions
///
/// None.
#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub struct Commit;

impl BasicSnippet for Commit {
    fn parameters(&self) -> Vec<(DataType, String)> {
        ["receiver_digest", "sender_randomness", "item"]
            .map(|name| (DataType::Digest, name.to_string()))
            .to_vec()
    }

    fn return_values(&self) -> Vec<(DataType, String)> {
        vec![(DataType::Digest, "commitment".to_string())]
    }

    fn entrypoint(&self) -> String {
        "tasmlib_neptune_mutator_set_commit".to_string()
    }

    fn code(&self, _: &mut Library) -> Vec<LabelledInstruction> {
        triton_asm! {
            {self.entrypoint()}:
                hash
                hash
                return
        }
    }

    fn sign_offs(&self) -> HashMap<Reviewer, SignOffFingerprint> {
        let mut sign_offs = HashMap::new();
        sign_offs.insert(Reviewer("ferdinand"), 0xbe1bb66a7035660b.into());
        sign_offs
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_prelude::*;

    impl Closure for Commit {
        type Args = (Digest, Digest, Digest);

        fn rust_shadow(&self, stack: &mut Vec<BFieldElement>) {
            let (receiver_digest, sender_randomness, item) = pop_encodable::<Self::Args>(stack);
            let commitment =
                Tip5::hash_pair(Tip5::hash_pair(item, sender_randomness), receiver_digest);
            push_encodable(stack, &commitment);
        }

        fn pseudorandom_args(&self, seed: [u8; 32], _: Option<BenchmarkCase>) -> Self::Args {
            StdRng::from_seed(seed).random()
        }
    }

    #[test]
    fn rust_shadow() {
        ShadowedClosure::new(Commit).test();
    }
}

#[cfg(test)]
mod benches {
    use super::*;
    use crate::test_prelude::*;

    #[test]
    fn benchmark() {
        ShadowedClosure::new(Commit).bench();
    }
}
