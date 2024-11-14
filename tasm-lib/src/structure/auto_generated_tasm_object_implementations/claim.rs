use triton_vm::prelude::*;

use crate::library::Library;
use crate::prelude::TasmObject;

/// A field-by-field copy of [`Claim`] to derive [`TasmObject`] on. Helps to
/// implement [`TasmObject`] for [`Claim`] by forwarding function calls.
#[derive(TasmObject)]
struct FakeClaim {
    pub program_digest: Digest,
    pub version: u32,
    pub input: Vec<BFieldElement>,
    pub output: Vec<BFieldElement>,
}

impl TasmObject for Claim {
    fn label_friendly_name() -> String {
        "Claim".to_owned()
    }

    fn get_field(field_name: &str) -> Vec<LabelledInstruction> {
        FakeClaim::get_field(field_name)
    }

    fn get_field_with_size(field_name: &str) -> Vec<LabelledInstruction> {
        FakeClaim::get_field_with_size(field_name)
    }

    fn get_field_start_with_jump_distance(field_name: &str) -> Vec<LabelledInstruction> {
        FakeClaim::get_field_start_with_jump_distance(field_name)
    }

    fn compute_size_and_assert_valid_size_indicator(
        library: &mut Library,
    ) -> Vec<LabelledInstruction> {
        FakeClaim::compute_size_and_assert_valid_size_indicator(library)
    }

    fn decode_iter<Itr: Iterator<Item = BFieldElement>>(
        iterator: &mut Itr,
    ) -> Result<Box<Self>, Box<dyn core::error::Error + Send + Sync>> {
        let FakeClaim {
            program_digest,
            version,
            input,
            output,
        } = *FakeClaim::decode_iter(iterator)?;

        Ok(Box::new(Self {
            program_digest,
            version,
            input,
            output,
        }))
    }
}
