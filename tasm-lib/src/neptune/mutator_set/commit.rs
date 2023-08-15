use std::collections::HashMap;

use rand::random;
use triton_vm::NonDeterminism;
use twenty_first::util_types::algebraic_hasher::AlgebraicHasher;

use crate::{
    get_init_tvm_stack,
    snippet::{DataType, DeprecatedSnippet},
    Digest, ExecutionState, VmHasher,
};

#[derive(Clone, Debug)]
pub struct Commit;

/// Commit computes the addition record for the mutator set AOCL from
///  an item (which is a digest), the sender's randomness (also
/// digest), and the receiver's digest (ditto). Note that the order
/// is:
///  (top of stack -->) - item 0
///                     - item 1
///                     - item 2
///                     - item 3
///                     - item 4
///                     - sender_randomness 0
///                     - sender_randomness 1
///                     - sender_randomness 2
///                     - sender_randomness 3
///                     - sender_randomness 4
///                     - receiver_digest 0
///                     - receiver_digest 1
///                     - receiver_digest 2
///                     - receiver_digest 3
///                     - receiver_digest 4
impl Commit {
    fn test_state() -> ExecutionState {
        let item: Digest = random();
        let sender_randomness: Digest = random();
        let receiver_digest: Digest = random();
        let mut stack = get_init_tvm_stack();
        for d in [
            item.values(),
            sender_randomness.values(),
            receiver_digest.values(),
        ]
        .concat()
        .to_vec()
        .iter()
        .rev()
        {
            stack.push(*d);
        }
        ExecutionState {
            stack,
            std_in: vec![],
            nondeterminism: NonDeterminism::new(vec![]),
            memory: HashMap::new(),
            words_allocated: 1,
        }
    }
}

impl DeprecatedSnippet for Commit {
    fn entrypoint_name(&self) -> String {
        "tasm_neptune_mutator_set_commit".to_string()
    }

    fn input_field_names(&self) -> Vec<String>
    where
        Self: Sized,
    {
        vec![
            "receiver_digest4".to_string(),
            "receiver_digest3".to_string(),
            "receiver_digest2".to_string(),
            "receiver_digest1".to_string(),
            "receiver_digest0".to_string(),
            "sender_randomness4".to_string(),
            "sender_randomness3".to_string(),
            "sender_randomness2".to_string(),
            "sender_randomness1".to_string(),
            "sender_randomness0".to_string(),
            "item4".to_string(),
            "item3".to_string(),
            "item2".to_string(),
            "item1".to_string(),
            "item0".to_string(),
        ]
    }

    fn input_types(&self) -> Vec<crate::snippet::DataType> {
        vec![DataType::Digest, DataType::Digest, DataType::Digest]
    }

    fn output_types(&self) -> Vec<crate::snippet::DataType> {
        vec![DataType::Digest]
    }

    fn output_field_names(&self) -> Vec<String>
    where
        Self: Sized,
    {
        vec![
            "commitment4".to_string(),
            "commitment3".to_string(),
            "commitment2".to_string(),
            "commitment1".to_string(),
            "commitment0".to_string(),
        ]
    }

    fn stack_diff(&self) -> isize
    where
        Self: Sized,
    {
        -10
    }

    fn function_code(&self, _library: &mut crate::library::Library) -> String {
        let entrypoint = self.entrypoint_name();

        format!(
            "
            // BEFORE: _ r4 r3 r2 r1 r0 s4 s3 s2 s1 s0 i4 i3 i2 i1 i0
            // AFTER: _ c4 c3 c2 c1 c0
            {entrypoint}:
                hash
                pop pop pop pop pop
                hash
                pop pop pop pop pop
                return
            "
        )
    }

    fn crash_conditions(&self) -> Vec<String>
    where
        Self: Sized,
    {
        vec!["Stack too shallow.".to_string()]
    }

    fn gen_input_states(&self) -> Vec<crate::ExecutionState>
    where
        Self: Sized,
    {
        vec![Self::test_state()]
    }

    fn common_case_input_state(&self) -> crate::ExecutionState
    where
        Self: Sized,
    {
        Self::test_state()
    }

    fn worst_case_input_state(&self) -> crate::ExecutionState
    where
        Self: Sized,
    {
        Self::test_state()
    }

    fn rust_shadowing(
        &self,
        stack: &mut Vec<triton_vm::BFieldElement>,
        _std_in: Vec<triton_vm::BFieldElement>,
        _secret_in: Vec<triton_vm::BFieldElement>,
        _memory: &mut std::collections::HashMap<triton_vm::BFieldElement, triton_vm::BFieldElement>,
    ) where
        Self: Sized,
    {
        let item = Digest::new([
            stack.pop().unwrap(),
            stack.pop().unwrap(),
            stack.pop().unwrap(),
            stack.pop().unwrap(),
            stack.pop().unwrap(),
        ]);
        let sender_randomness = Digest::new([
            stack.pop().unwrap(),
            stack.pop().unwrap(),
            stack.pop().unwrap(),
            stack.pop().unwrap(),
            stack.pop().unwrap(),
        ]);
        let receiver_digest = Digest::new([
            stack.pop().unwrap(),
            stack.pop().unwrap(),
            stack.pop().unwrap(),
            stack.pop().unwrap(),
            stack.pop().unwrap(),
        ]);
        let commitment = VmHasher::hash_pair(
            &VmHasher::hash_pair(&item, &sender_randomness),
            &receiver_digest,
        );
        stack.push(commitment.values()[4]);
        stack.push(commitment.values()[3]);
        stack.push(commitment.values()[2]);
        stack.push(commitment.values()[1]);
        stack.push(commitment.values()[0]);
    }
}

#[cfg(test)]
mod tests {
    use crate::test_helpers::test_rust_equivalence_multiple_deprecated;

    use super::Commit;

    #[test]
    fn new_prop_test() {
        test_rust_equivalence_multiple_deprecated(&Commit, true);
    }
}

#[cfg(test)]
mod benches {
    use super::*;
    use crate::snippet_bencher::bench_and_write;

    #[test]
    fn commit_benchmark_unsafe() {
        bench_and_write(Commit);
    }
}
