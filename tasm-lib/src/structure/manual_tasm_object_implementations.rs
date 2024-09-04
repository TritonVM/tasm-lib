use itertools::Itertools;
use triton_vm::prelude::*;
use twenty_first::error::BFieldCodecError;

use crate::{data_type::DataType, prelude::TasmObject};

use super::tasm_object::Result;

impl<T: BFieldCodec + TasmObject> TasmObject for Vec<T> {
    fn label_friendly_name() -> String {
        format!("vec___{}", T::label_friendly_name())
    }

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

    fn compute_size_and_assert_valid_size_indicator(
        library: &mut crate::tasm_lib::Library,
    ) -> Vec<LabelledInstruction> {
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
            // _ *list_len

            let verified_element_size = T::compute_size_and_assert_valid_size_indicator(library);

            let loop_label = format!(
                "tasmlib_structure_tasmobject_verify_size_indicators_dyn_elem_sizes___{}",
                T::label_friendly_name()
            );

            let loop_code = triton_asm!(
                // INVARIANT: _ remaining_elements acc_size *element_si
                {loop_label}:

                    dup 2
                    push 0
                    eq
                    skiz
                        return
                    // _ remaining_elements acc_size *element_si

                    read_mem 1
                    // _ remaining_elements acc_size element_si (*element_si-1)

                    /* Verify that max allowed size is not exceeded */
                    push {T::MAX_OFFSET}
                    dup 2
                    lt
                    assert
                    // _ remaining_elements acc_size element_si (*element_si-1)

                    addi 2
                    // _ remaining_elements acc_size element_si *element

                    dup 0
                    {&verified_element_size}
                    // _ remaining_elements acc_size element_si *element calculated_elem_size

                    dup 2
                    eq
                    assert
                    // _ remaining_elements acc_size element_si *element

                    dup 2
                    dup 2
                    add
                    // _ remaining_elements acc_size element_si *element acc_size'

                    /* Account for element's size indicator, since it's dynamically sized */
                    addi 1
                    // _ remaining_elements acc_size element_si *element acc_size'

                    swap 3
                    pop 1
                    // _ remaining_elements acc_size' element_si *element

                    add
                    // _ remaining_elements acc_size' *next_element

                    swap 2
                    addi -1
                    swap 2
                    // _ (remaining_elements-1) acc_size' *next_element

                    recurse
            );

            library.explicit_import(&loop_label, &loop_code);
            triton_asm!(
                // _ *list_len

                read_mem 1
                // _ list_len (*list_len - 1)

                push 0
                swap 1
                // _ list_len 0 (*list_len - 1)

                addi 2
                hint elem_si_ptr = stack[0]
                hint acc_size = stack[1]
                hint remaining_elements = stack[2]
                // _ list_len 0 (*list_len + 1)
                // _ remaining_elements acc_size *element[0]_si <-- rename

                call {loop_label}
                // _ 0 acc_size *EOF

                pop 1
                swap 1
                pop 1
                // _ acc_size

                /* Add size of (outer) list's length indicator */
                addi 1
            )
        }
    }
}

impl TasmObject for BFieldElement {
    fn label_friendly_name() -> String {
        unreachable!()
    }

    fn get_field(_field_name: &str) -> Vec<LabelledInstruction> {
        unreachable!()
    }

    fn get_field_with_size(_field_name: &str) -> Vec<LabelledInstruction> {
        unreachable!()
    }

    fn get_field_start_with_jump_distance(_field_name: &str) -> Vec<LabelledInstruction> {
        unreachable!()
    }

    fn get_encoding_length() -> Vec<LabelledInstruction> {
        unreachable!()
    }

    fn compute_size_and_assert_valid_size_indicator(
        _library: &mut crate::tasm_lib::Library,
    ) -> Vec<LabelledInstruction> {
        unreachable!("Size is known statically for BFieldElement encoding")
    }

    fn decode_iter<Itr: Iterator<Item = BFieldElement>>(_iterator: &mut Itr) -> Result<Box<Self>> {
        todo!()
    }
}

impl TasmObject for XFieldElement {
    fn label_friendly_name() -> String {
        DataType::Xfe.label_friendly_name()
    }

    fn get_field(_field_name: &str) -> Vec<LabelledInstruction> {
        unreachable!()
    }

    fn get_field_with_size(_field_name: &str) -> Vec<LabelledInstruction> {
        unreachable!()
    }

    fn get_field_start_with_jump_distance(_field_name: &str) -> Vec<LabelledInstruction> {
        unreachable!()
    }

    fn get_encoding_length() -> Vec<LabelledInstruction> {
        unreachable!()
    }

    fn compute_size_and_assert_valid_size_indicator(
        _library: &mut crate::tasm_lib::Library,
    ) -> Vec<LabelledInstruction> {
        unreachable!("Size is known statically for XFieldElement encoding")
    }

    fn decode_iter<Itr: Iterator<Item = BFieldElement>>(_iterator: &mut Itr) -> Result<Box<Self>> {
        todo!()
    }
}

impl TasmObject for Digest {
    fn label_friendly_name() -> String {
        DataType::Digest.label_friendly_name()
    }

    fn get_field(_field_name: &str) -> Vec<LabelledInstruction> {
        unreachable!()
    }

    fn get_field_with_size(_field_name: &str) -> Vec<LabelledInstruction> {
        unreachable!()
    }

    fn get_field_start_with_jump_distance(_field_name: &str) -> Vec<LabelledInstruction> {
        unreachable!()
    }

    fn get_encoding_length() -> Vec<LabelledInstruction> {
        unreachable!()
    }

    fn compute_size_and_assert_valid_size_indicator(
        _library: &mut crate::tasm_lib::Library,
    ) -> Vec<LabelledInstruction> {
        unreachable!("Size is known statically for Digest encoding")
    }

    fn decode_iter<Itr: Iterator<Item = BFieldElement>>(_iterator: &mut Itr) -> Result<Box<Self>> {
        todo!()
    }
}

impl TasmObject for bool {
    fn label_friendly_name() -> String {
        DataType::Bool.label_friendly_name()
    }

    fn get_field(_field_name: &str) -> Vec<LabelledInstruction> {
        unreachable!()
    }

    fn get_field_with_size(_field_name: &str) -> Vec<LabelledInstruction> {
        unreachable!()
    }

    fn get_field_start_with_jump_distance(_field_name: &str) -> Vec<LabelledInstruction> {
        unreachable!()
    }

    fn get_encoding_length() -> Vec<LabelledInstruction> {
        unreachable!()
    }

    fn compute_size_and_assert_valid_size_indicator(
        _library: &mut crate::tasm_lib::Library,
    ) -> Vec<LabelledInstruction> {
        unreachable!("Size is known statically for u32 encoding")
    }

    fn decode_iter<Itr: Iterator<Item = BFieldElement>>(_iterator: &mut Itr) -> Result<Box<Self>> {
        todo!()
    }
}

impl TasmObject for u32 {
    fn label_friendly_name() -> String {
        DataType::U32.label_friendly_name()
    }

    fn get_field(_field_name: &str) -> Vec<LabelledInstruction> {
        unreachable!()
    }

    fn get_field_with_size(_field_name: &str) -> Vec<LabelledInstruction> {
        unreachable!()
    }

    fn get_field_start_with_jump_distance(_field_name: &str) -> Vec<LabelledInstruction> {
        unreachable!()
    }

    fn get_encoding_length() -> Vec<LabelledInstruction> {
        unreachable!()
    }

    fn compute_size_and_assert_valid_size_indicator(
        _library: &mut crate::tasm_lib::Library,
    ) -> Vec<LabelledInstruction> {
        unreachable!("Size is known statically for u32 encoding")
    }

    fn decode_iter<Itr: Iterator<Item = BFieldElement>>(_iterator: &mut Itr) -> Result<Box<Self>> {
        todo!()
    }
}

impl TasmObject for u64 {
    fn label_friendly_name() -> String {
        DataType::U64.label_friendly_name()
    }

    fn get_field(_field_name: &str) -> Vec<LabelledInstruction> {
        unreachable!()
    }

    fn get_field_with_size(_field_name: &str) -> Vec<LabelledInstruction> {
        unreachable!()
    }

    fn get_field_start_with_jump_distance(_field_name: &str) -> Vec<LabelledInstruction> {
        unreachable!()
    }

    fn get_encoding_length() -> Vec<LabelledInstruction> {
        unreachable!()
    }

    fn compute_size_and_assert_valid_size_indicator(
        _library: &mut crate::tasm_lib::Library,
    ) -> Vec<LabelledInstruction> {
        unreachable!("Size is known statically for u64 encoding")
    }

    fn decode_iter<Itr: Iterator<Item = BFieldElement>>(_iterator: &mut Itr) -> Result<Box<Self>> {
        todo!()
    }
}

impl TasmObject for u128 {
    fn label_friendly_name() -> String {
        DataType::U128.label_friendly_name()
    }

    fn get_field(_field_name: &str) -> Vec<LabelledInstruction> {
        unreachable!()
    }

    fn get_field_with_size(_field_name: &str) -> Vec<LabelledInstruction> {
        unreachable!()
    }

    fn get_field_start_with_jump_distance(_field_name: &str) -> Vec<LabelledInstruction> {
        unreachable!()
    }

    fn get_encoding_length() -> Vec<LabelledInstruction> {
        unreachable!()
    }

    fn compute_size_and_assert_valid_size_indicator(
        _library: &mut crate::tasm_lib::Library,
    ) -> Vec<LabelledInstruction> {
        unreachable!("Size is known statically for u128 encoding")
    }

    fn decode_iter<Itr: Iterator<Item = BFieldElement>>(_iterator: &mut Itr) -> Result<Box<Self>> {
        todo!()
    }
}

impl<T: TasmObject, S: TasmObject> TasmObject for (T, S) {
    fn label_friendly_name() -> String {
        format!(
            "tuple_L_{}_{}_R",
            T::label_friendly_name(),
            S::label_friendly_name()
        )
    }

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

    fn compute_size_and_assert_valid_size_indicator(
        _library: &mut crate::tasm_lib::Library,
    ) -> Vec<LabelledInstruction> {
        panic!("Size is known statically for u32 encoding")
    }

    fn decode_iter<Itr: Iterator<Item = BFieldElement>>(_iterator: &mut Itr) -> Result<Box<Self>> {
        todo!()
    }
}

impl TasmObject for Proof {
    fn label_friendly_name() -> String {
        "tvm_proof".to_owned()
    }

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

    fn compute_size_and_assert_valid_size_indicator(
        _library: &mut crate::tasm_lib::Library,
    ) -> Vec<LabelledInstruction> {
        panic!()
    }

    fn decode_iter<Itr: Iterator<Item = BFieldElement>>(_iterator: &mut Itr) -> Result<Box<Self>> {
        todo!()
    }
}

impl<T: TasmObject> TasmObject for Option<T> {
    fn label_friendly_name() -> String {
        format!("option_L_{}_R", T::label_friendly_name())
    }

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

    fn compute_size_and_assert_valid_size_indicator(
        _library: &mut crate::tasm_lib::Library,
    ) -> Vec<LabelledInstruction> {
        todo!()
    }
}
