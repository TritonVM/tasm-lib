use itertools::Itertools;
use triton_vm::prelude::*;
use twenty_first::error::BFieldCodecError;

use crate::prelude::TasmObject;

use super::tasm_object::Result;

impl<T: BFieldCodec> TasmObject for Vec<T> {
    fn get_field(_field_name: &str) -> Vec<LabelledInstruction> {
        panic!("`Vec` does not have fields; cannot access them")
    }

    fn get_field_with_size(_field_name: &str) -> Vec<LabelledInstruction> {
        panic!("`Vec` does not have fields; cannot access them")
    }

    fn get_field_start_with_jump_distance(_field_name: &str) -> Vec<LabelledInstruction> {
        panic!("`Vec` does not have fields; cannot access them")
    }

    fn decode_iter<Itr: Iterator<Item = BFieldElement>>(iterator: &mut Itr) -> Result<Box<Self>> {
        let length = iterator.next().unwrap().value() as usize;
        let mut vector = vec![];
        for _ in 0..length {
            let sequence_length = if let Some(static_size) = T::static_length() {
                static_size
            } else {
                iterator.next().unwrap().value() as usize
            };
            let sequence = (0..sequence_length)
                .map(|_| iterator.next().unwrap())
                .collect_vec();
            let object = *T::decode(&sequence).map_err(|e| e.into())?;
            vector.push(object);
        }
        Ok(Box::new(vector))
    }

    fn get_encoding_length() -> Vec<LabelledInstruction> {
        todo!()
    }

    fn compute_size_and_assert_valid_size_indicator() -> Vec<LabelledInstruction> {
        if let Some(static_size) = T::static_length() {
            // _ *list_len
            triton_asm!(
                read_mem 1
                pop 1
                // _ list_len

                push {static_size}
                mul
                addi 1
                // _ (list_len * elem_size + 1)
                // _ calculated_size
            )
        } else {
            todo!("integral SI check not implemented for vec of dynamically-sized elements yet")
        }
    }
}

impl TasmObject for BFieldElement {
    fn get_field(_field_name: &str) -> Vec<LabelledInstruction> {
        panic!()
    }

    fn get_field_with_size(_field_name: &str) -> Vec<LabelledInstruction> {
        panic!()
    }

    fn get_field_start_with_jump_distance(_field_name: &str) -> Vec<LabelledInstruction> {
        panic!()
    }

    fn get_encoding_length() -> Vec<LabelledInstruction> {
        panic!()
    }

    fn compute_size_and_assert_valid_size_indicator() -> Vec<LabelledInstruction> {
        panic!("Size is known statically for BFieldElement encoding")
    }

    fn decode_iter<Itr: Iterator<Item = BFieldElement>>(_iterator: &mut Itr) -> Result<Box<Self>> {
        todo!()
    }
}

impl TasmObject for XFieldElement {
    fn get_field(_field_name: &str) -> Vec<LabelledInstruction> {
        panic!()
    }

    fn get_field_with_size(_field_name: &str) -> Vec<LabelledInstruction> {
        panic!()
    }

    fn get_field_start_with_jump_distance(_field_name: &str) -> Vec<LabelledInstruction> {
        panic!()
    }

    fn get_encoding_length() -> Vec<LabelledInstruction> {
        panic!()
    }

    fn compute_size_and_assert_valid_size_indicator() -> Vec<LabelledInstruction> {
        panic!("Size is known statically for XFieldElement encoding")
    }

    fn decode_iter<Itr: Iterator<Item = BFieldElement>>(_iterator: &mut Itr) -> Result<Box<Self>> {
        todo!()
    }
}

impl TasmObject for Digest {
    fn get_field(_field_name: &str) -> Vec<LabelledInstruction> {
        panic!()
    }

    fn get_field_with_size(_field_name: &str) -> Vec<LabelledInstruction> {
        panic!()
    }

    fn get_field_start_with_jump_distance(_field_name: &str) -> Vec<LabelledInstruction> {
        panic!()
    }

    fn get_encoding_length() -> Vec<LabelledInstruction> {
        panic!()
    }

    fn compute_size_and_assert_valid_size_indicator() -> Vec<LabelledInstruction> {
        panic!("Size is known statically for Digest encoding")
    }

    fn decode_iter<Itr: Iterator<Item = BFieldElement>>(_iterator: &mut Itr) -> Result<Box<Self>> {
        todo!()
    }
}

impl TasmObject for u32 {
    fn get_field(_field_name: &str) -> Vec<LabelledInstruction> {
        panic!()
    }

    fn get_field_with_size(_field_name: &str) -> Vec<LabelledInstruction> {
        panic!()
    }

    fn get_field_start_with_jump_distance(_field_name: &str) -> Vec<LabelledInstruction> {
        panic!()
    }

    fn get_encoding_length() -> Vec<LabelledInstruction> {
        panic!()
    }

    fn compute_size_and_assert_valid_size_indicator() -> Vec<LabelledInstruction> {
        panic!("Size is known statically for u32 encoding")
    }

    fn decode_iter<Itr: Iterator<Item = BFieldElement>>(_iterator: &mut Itr) -> Result<Box<Self>> {
        todo!()
    }
}

impl TasmObject for u64 {
    fn get_field(_field_name: &str) -> Vec<LabelledInstruction> {
        panic!()
    }

    fn get_field_with_size(_field_name: &str) -> Vec<LabelledInstruction> {
        panic!()
    }

    fn get_field_start_with_jump_distance(_field_name: &str) -> Vec<LabelledInstruction> {
        panic!()
    }

    fn get_encoding_length() -> Vec<LabelledInstruction> {
        panic!()
    }

    fn compute_size_and_assert_valid_size_indicator() -> Vec<LabelledInstruction> {
        panic!("Size is known statically for u64 encoding")
    }

    fn decode_iter<Itr: Iterator<Item = BFieldElement>>(_iterator: &mut Itr) -> Result<Box<Self>> {
        todo!()
    }
}

impl TasmObject for u128 {
    fn get_field(_field_name: &str) -> Vec<LabelledInstruction> {
        panic!()
    }

    fn get_field_with_size(_field_name: &str) -> Vec<LabelledInstruction> {
        panic!()
    }

    fn get_field_start_with_jump_distance(_field_name: &str) -> Vec<LabelledInstruction> {
        panic!()
    }

    fn get_encoding_length() -> Vec<LabelledInstruction> {
        panic!()
    }

    fn compute_size_and_assert_valid_size_indicator() -> Vec<LabelledInstruction> {
        panic!("Size is known statically for u128 encoding")
    }

    fn decode_iter<Itr: Iterator<Item = BFieldElement>>(_iterator: &mut Itr) -> Result<Box<Self>> {
        todo!()
    }
}

impl TasmObject for Proof {
    fn get_field(_field_name: &str) -> Vec<LabelledInstruction> {
        panic!()
    }

    fn get_field_with_size(_field_name: &str) -> Vec<LabelledInstruction> {
        panic!()
    }

    fn get_field_start_with_jump_distance(_field_name: &str) -> Vec<LabelledInstruction> {
        panic!()
    }

    fn get_encoding_length() -> Vec<LabelledInstruction> {
        panic!()
    }

    fn compute_size_and_assert_valid_size_indicator() -> Vec<LabelledInstruction> {
        panic!()
    }

    fn decode_iter<Itr: Iterator<Item = BFieldElement>>(_iterator: &mut Itr) -> Result<Box<Self>> {
        todo!()
    }
}

impl<T: TasmObject> TasmObject for Option<T> {
    fn get_field(_field_name: &str) -> Vec<LabelledInstruction> {
        unreachable!("cannot get field of an option type");
    }

    fn get_field_with_size(_field_name: &str) -> Vec<LabelledInstruction> {
        unreachable!("cannot get field with size of an option type");
    }

    fn get_field_start_with_jump_distance(_field_name: &str) -> Vec<LabelledInstruction> {
        unreachable!("cannot get field start with jump distance of an option type");
    }

    fn decode_iter<Itr: Iterator<Item = BFieldElement>>(iterator: &mut Itr) -> Result<Box<Self>> {
        match iterator.next() {
            Some(token) => {
                if token == BFieldElement::new(0) {
                    Ok(Box::new(None))
                } else if token == BFieldElement::new(1) {
                    let t: T = *T::decode_iter(iterator)?;
                    Ok(Box::new(Some(t)))
                } else {
                    Err(Box::new(BFieldCodecError::ElementOutOfRange))
                }
            }
            None => Err(Box::new(BFieldCodecError::SequenceTooShort)),
        }
    }

    fn get_encoding_length() -> Vec<LabelledInstruction> {
        todo!()
    }

    fn compute_size_and_assert_valid_size_indicator() -> Vec<LabelledInstruction> {
        todo!()
    }
}
