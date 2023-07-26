pub use derive_tasm_object::TasmObject;

use triton_vm::instruction::LabelledInstruction;

/// TasmObject
///
/// This trait defines methods for dealing with custom-defined objects from within the VM,
/// assuming those methods live in memory as they are encoded with BFieldCodec.
///
/// The arguments referring to fields are strings. For structs with unnamed fields, the
/// nth field name is implicitly `field_n`.
pub trait TasmObject {
    /// Returns tasm code that returns a pointer the named field of the object, assuming:
    ///  - that a pointer to the said object lives on top of the stack;
    ///  - said object has a type that implements the TasmObject trait;
    ///  - said object lives in memory endoded as BFieldCodec specifies.
    ///
    /// BEFORE: _ *object
    ///
    /// AFTER: _ *field
    fn get_field(field_name: &str) -> Vec<LabelledInstruction>;

    /// Returns tasm code that returns a pointer the named field of the object, along with
    /// the size of that field in number of BFieldElements, assuming:
    ///  - that a pointer to the said object lives on top of the stack;
    ///  - said object has a type that implements the TasmObject trait;
    ///  - said object lives in memory endoded as BFieldCodec specifies.
    ///
    /// BEFORE: _ *object
    ///
    /// AFTER: _ *field field_size
    ///
    /// See also: `get_field` if you just want the field without the size.
    fn get_field_with_size(field_name: &str) -> Vec<LabelledInstruction>;
}

#[cfg(test)]
mod test {
    use derive_tasm_object::TasmObject;
    use itertools::Itertools;
    use rand::{rngs::StdRng, Rng, SeedableRng};
    use triton_vm::BFieldElement;
    use twenty_first::shared_math::{bfield_codec::BFieldCodec, x_field_element::XFieldElement};

    #[derive(BFieldCodec, TasmObject)]
    struct InnerStruct(XFieldElement, u32);

    #[derive(BFieldCodec, TasmObject)]
    struct OuterStruct {
        a: Vec<Option<bool>>,
        b: InnerStruct,
        c: BFieldElement,
    }

    fn _pseudorandom_object(seed: [u8; 32]) -> OuterStruct {
        let mut rng: StdRng = SeedableRng::from_seed(seed);
        let a = (0..19)
            .map(|_| {
                if rng.gen() {
                    if rng.gen() {
                        Some(true)
                    } else {
                        Some(false)
                    }
                } else {
                    None
                }
            })
            .collect_vec();
        let b0: XFieldElement = rng.gen();
        let b1: u32 = rng.gen();
        let c: BFieldElement = rng.gen();

        OuterStruct {
            a,
            b: InnerStruct(b0, b1),
            c,
        }
    }
}
