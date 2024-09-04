use itertools::Itertools;
use triton_vm::prelude::*;
use twenty_first::error::BFieldCodecError;

use crate::{data_type::DataType, prelude::TasmObject};

use super::tasm_object::Result;

impl<const N: usize, T: BFieldCodec + TasmObject> TasmObject for [T; N] {
    fn label_friendly_name() -> String {
        format!("array{}___{}", N, T::label_friendly_name())
    }

    fn get_field(_field_name: &str) -> Vec<LabelledInstruction> {
        todo!()
    }

    fn get_field_with_size(_field_name: &str) -> Vec<LabelledInstruction> {
        todo!()
    }

    fn get_field_start_with_jump_distance(_field_name: &str) -> Vec<LabelledInstruction> {
        todo!()
    }

    fn get_encoding_length() -> Vec<LabelledInstruction> {
        todo!()
    }

    fn compute_size_and_assert_valid_size_indicator(
        _library: &mut crate::prelude::Library,
    ) -> Vec<LabelledInstruction> {
        if let Some(static_size) = T::static_length() {
            let own_size = static_size * N;
            triton_asm!(
                // _ *elem[0]

                pop 1
                push {own_size}
                // _ own_size
            )
        } else {
            todo!()
        }
    }

    fn decode_iter<Itr: Iterator<Item = BFieldElement>>(_iterator: &mut Itr) -> Result<Box<Self>> {
        todo!()
    }
}

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
        DataType::Bfe.label_friendly_name()
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

impl<T: TasmObject + BFieldCodec, S: TasmObject + BFieldCodec> TasmObject for (T, S) {
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
        library: &mut crate::tasm_lib::Library,
    ) -> Vec<LabelledInstruction> {
        let size_left = match T::static_length() {
            Some(static_size) => triton_asm!(
                // _ *left

                pop 1
                push { static_size }
                // _ left_size
            ),
            None => {
                let recursive_call = T::compute_size_and_assert_valid_size_indicator(library);

                triton_asm!(
                    // _ *left_si
                    hint left_si_ptr = stack[0]

                    read_mem 1
                    addi 2
                    // _ left_si *left

                    {&recursive_call}
                    hint calculated_left = stack[0]
                    // _ left_si calculated_left

                    dup 1
                    eq
                    assert
                    // _ left_size

                    addi 1
                    // _ (left_size + 1)
                )
            }
        };
        let size_right = match S::static_length() {
            Some(static_size) => triton_asm!(
                // _ *right

                push { static_size }
                hint right_size = stack[0]
                 // _ *right right_size
            ),
            None => {
                let recursive_call = S::compute_size_and_assert_valid_size_indicator(library);

                triton_asm!(
                    // _ *right_si
                    hint right_si_ptr = stack[0]

                    read_mem 1
                    addi 2
                    hint right_ptr = stack[0]

                    swap 1
                    dup 1
                    // _ *right right_si *right

                    {&recursive_call}
                    hint calculated_right = stack[0]
                    // _ *right right_si calculated_right

                    dup 1
                    eq
                    assert
                    // _ *right right_size

                    // TODO: Can this be made nicer?
                    swap 1
                    addi -1
                    swap 1
                    addi 1
                )
            }
        };

        // panic!("Size is known statically for u32 encoding")
        triton_asm!(
            // _ *tuple

            // TODO: addi 1 here?
            {&size_right}
            hint right_ptr = stack[1]
            hint right_size = stack[0]
            // _ *right right_size

            swap 1
            dup 1
            add
            hint left = stack[0]
            // _ right_size (*right + right_size)
            // _ right_size *left

            {&size_left}
            hint left_size = stack[0]
            // _ right_size left_size

            add
            // _ addi 1 or addi 2 here?
        )
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
        // Proofs are special, as the fields of a proof is only accessed through
        // the [`DequeueNextAs`](crate::verifier::vm_proof_iter::dequeue_next_as)
        // snippet which does some checks itself. So we just report the total size
        // here.

        triton_asm!(
            // _ *proof
            read_mem 1
            // _ field_0_len (*proof - 1)

            pop 1
            // _ field_0_len

            addi 1

            // _ own_size
        )
    }

    fn decode_iter<Itr: Iterator<Item = BFieldElement>>(iterator: &mut Itr) -> Result<Box<Self>> {
        match iterator.next() {
            Some(_field_0_size) => Ok(Box::new(Self(*Vec::<BFieldElement>::decode_iter(
                iterator,
            )?))),
            None => Err(Box::new(BFieldCodecError::SequenceTooShort)),
        }
    }
}

impl<T: TasmObject + BFieldCodec> TasmObject for Option<T> {
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
        library: &mut crate::tasm_lib::Library,
    ) -> Vec<LabelledInstruction> {
        let get_payload_size = match T::static_length() {
            Some(static_size) => triton_asm!(push { static_size }),
            None => T::compute_size_and_assert_valid_size_indicator(library),
        };

        let some_branch_label = format!(
            "tasmlib_tasmobject_size_verifier_option_some_branch___{}",
            T::label_friendly_name()
        );
        let some_branch = triton_asm!(
            {some_branch_label}:

                // _ *value 1
                pop 1

                {&get_payload_size}
                // _ value_size

                /* Push 0 to avoid `None` branch from being taken */
                push 0

                return
        );

        let none_branch_label = "tasmlib_tasmobject_size_verifier_option_none".to_owned();
        let none_branch = triton_asm!(
            {none_branch_label}:
                // _ *ptr

                pop 1
                push 0
                // _ value_size

                return
        );

        library.explicit_import(&some_branch_label, &some_branch);
        library.explicit_import(&none_branch_label, &none_branch);

        triton_asm!(
            // _ *discriminant
            read_mem 1
            addi 2
            // _ discriminant (*discriminant + 1)

            swap 1
            // _ (*discriminant + 1) discriminant

            push 1
            swap 1
            // _ (*discriminant + 1) 1 discriminant

            push 1
            eq
            // _ (*discriminant + 1) 1 (discriminant == 1)

            skiz
                call {some_branch_label}
            skiz
                call {none_branch_label}

            // _ value_size
            addi 1

            // _ total_size
        )
    }
}
