// Contains field-by-field copies of various types defined outside this
// repository to derive `TasmObject` on. These “fake” types are then used
// to implement `TasmObject` for the actual type by forwarding function calls.

use triton_vm::fri::AuthenticationStructure;
use triton_vm::prelude::*;
use triton_vm::proof_item::FriResponse;

use crate::prelude::*;
use crate::twenty_first::prelude::MmrMembershipProof;
use crate::twenty_first::util_types::mmr::mmr_accumulator::MmrAccumulator;
use crate::twenty_first::util_types::mmr::mmr_successor_proof::MmrSuccessorProof;

macro_rules! derive_tasm_object_for {
    ($actual:ident using $fake:ident {$($field:ident: $field_type:ty,)* $(,)?}) => {
        #[derive(BFieldCodec, TasmObject)]
        struct $fake { $($field: $field_type),* }

        impl TasmObject for $actual {
            fn label_friendly_name() -> String {
                stringify!($actual).to_string()
            }

            fn compute_size_and_assert_valid_size_indicator(
                library: &mut Library,
            ) -> Vec<LabelledInstruction> {
                $fake::compute_size_and_assert_valid_size_indicator(library)
            }

            fn decode_iter<Itr: Iterator<Item = BFieldElement>>(
                iterator: &mut Itr,
            ) -> Result<Box<Self>, Box<dyn core::error::Error + Send + Sync>> {
                let $fake { $($field),* } = *$fake::decode_iter(iterator)?;
                Ok(Box::new(Self { $($field),* }))
            }
        }

        impl TasmStruct for $actual {
            fn get_field(field_name: &str) -> Vec<LabelledInstruction> {
                $fake::get_field(field_name)
            }

            fn get_field_with_size(field_name: &str) -> Vec<LabelledInstruction> {
                $fake::get_field_with_size(field_name)
            }

            fn destructure() -> Vec<LabelledInstruction> {
                $fake::destructure()
            }
        }
    };
}

derive_tasm_object_for!(
    Claim using FakeClaim {
        program_digest: Digest,
        version: u32,
        input: Vec<BFieldElement>,
        output: Vec<BFieldElement>,
    }
);

derive_tasm_object_for!(
    FriResponse using FakeFriResponse {
        auth_structure: AuthenticationStructure,
        revealed_leaves: Vec<XFieldElement>,
    }
);

derive_tasm_object_for!(
    MmrMembershipProof using FakeMmrMembershipProof {
        authentication_path: Vec<Digest>,
    }
);

derive_tasm_object_for!(
    MmrSuccessorProof using FakeMmrSuccessorProof {
        paths: Vec<Digest>,
    }
);

// can't use macro: MmrAccumulator has private fields and uses a dedicated
// constructor
#[derive(BFieldCodec, TasmObject)]
struct FakeMmrAccumulator {
    leaf_count: u64,
    peaks: Vec<Digest>,
}

impl TasmObject for MmrAccumulator {
    fn label_friendly_name() -> String {
        "MmrAccumulator".to_string()
    }

    fn compute_size_and_assert_valid_size_indicator(
        library: &mut Library,
    ) -> Vec<LabelledInstruction> {
        FakeMmrAccumulator::compute_size_and_assert_valid_size_indicator(library)
    }

    fn decode_iter<Itr: Iterator<Item = BFieldElement>>(
        iterator: &mut Itr,
    ) -> Result<Box<Self>, Box<dyn core::error::Error + Send + Sync>> {
        let FakeMmrAccumulator { leaf_count, peaks } = *FakeMmrAccumulator::decode_iter(iterator)?;

        Ok(Box::new(Self::init(peaks, leaf_count)))
    }
}

impl TasmStruct for MmrAccumulator {
    fn get_field(field_name: &str) -> Vec<LabelledInstruction> {
        FakeMmrAccumulator::get_field(field_name)
    }

    fn get_field_with_size(field_name: &str) -> Vec<LabelledInstruction> {
        FakeMmrAccumulator::get_field_with_size(field_name)
    }

    fn destructure() -> Vec<LabelledInstruction> {
        FakeMmrAccumulator::destructure()
    }
}
