use triton_vm::fri::AuthenticationStructure;
use triton_vm::prelude::*;
use triton_vm::proof_item::FriResponse;
use twenty_first::util_types::mmr::mmr_accumulator::MmrAccumulator;
use twenty_first::util_types::mmr::mmr_membership_proof::MmrMembershipProof;

impl crate::tasm_lib::structure::tasm_object::TasmObject for MmrAccumulator {
    fn label_friendly_name() -> String {
        "MmrAccumulator".to_owned()
    }
    fn get_field(field_name: &str) -> Vec<crate::triton_vm::instruction::LabelledInstruction> {
        let field_getter = match field_name {
            "peaks" => {
                let current = {
                    if let Some(size) = <Vec<
                            Digest,
                        > as crate::twenty_first::math::bfield_codec::BFieldCodec>::static_length() {
                            [
                                crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                    crate::triton_vm::instruction::AnInstruction::Push(
                                        crate::BFieldElement::new(size as u64),
                                    ),
                                ),
                            ]
                                .to_vec()
                        } else {
                            [
                                crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                    crate::triton_vm::instruction::AnInstruction::ReadMem(
                                        crate::triton_vm::op_stack::NumberOfWords::N1,
                                    ),
                                ),
                                crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                    crate::triton_vm::instruction::AnInstruction::Push(
                                        crate::triton_vm::prelude::BFieldElement::new(
                                            Self::MAX_OFFSET.into(),
                                        ),
                                    ),
                                ),
                                crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                    crate::triton_vm::instruction::AnInstruction::Dup(
                                        crate::triton_vm::op_stack::OpStackElement::ST2,
                                    ),
                                ),
                                crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                    crate::triton_vm::instruction::AnInstruction::Lt,
                                ),
                                crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                    crate::triton_vm::instruction::AnInstruction::Assert,
                                ),
                                crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                    crate::triton_vm::instruction::AnInstruction::AddI(
                                        crate::BFieldElement::new(1u64),
                                    ),
                                ),
                                crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                    crate::triton_vm::instruction::AnInstruction::Swap(
                                        crate::triton_vm::op_stack::OpStackElement::ST1,
                                    ),
                                ),
                                crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                    crate::triton_vm::instruction::AnInstruction::AddI(
                                        crate::BFieldElement::new(1u64),
                                    ),
                                ),
                            ]
                                .to_vec()
                        }
                };
                let getter = {
                    if <Vec<
                            Digest,
                        > as crate::twenty_first::math::bfield_codec::BFieldCodec>::static_length()
                            .is_some()
                        {
                            [
                                crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                    crate::triton_vm::instruction::AnInstruction::Pop(
                                        crate::triton_vm::op_stack::NumberOfWords::N1,
                                    ),
                                ),
                            ]
                                .to_vec()
                        } else {
                            [
                                crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                    crate::triton_vm::instruction::AnInstruction::Pop(
                                        crate::triton_vm::op_stack::NumberOfWords::N1,
                                    ),
                                ),
                                crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                    crate::triton_vm::instruction::AnInstruction::Push(
                                        crate::BFieldElement::new(1u64),
                                    ),
                                ),
                                crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                    crate::triton_vm::instruction::AnInstruction::Add,
                                ),
                            ]
                                .to_vec()
                        }
                };
                [current, getter].concat()
            }
            "leaf_count" => {
                let current = {
                    [
                            Self::get_field_start_with_jump_distance("peaks"),
                            [
                                crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                    crate::triton_vm::instruction::AnInstruction::Add,
                                ),
                            ]
                                .to_vec(),
                            {
                                if let Some(size) = <u64 as crate::twenty_first::math::bfield_codec::BFieldCodec>::static_length() {
                                    [
                                        crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                            crate::triton_vm::instruction::AnInstruction::Push(
                                                crate::BFieldElement::new(size as u64),
                                            ),
                                        ),
                                    ]
                                        .to_vec()
                                } else {
                                    [
                                        crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                            crate::triton_vm::instruction::AnInstruction::ReadMem(
                                                crate::triton_vm::op_stack::NumberOfWords::N1,
                                            ),
                                        ),
                                        crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                            crate::triton_vm::instruction::AnInstruction::Push(
                                                crate::triton_vm::prelude::BFieldElement::new(
                                                    Self::MAX_OFFSET.into(),
                                                ),
                                            ),
                                        ),
                                        crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                            crate::triton_vm::instruction::AnInstruction::Dup(
                                                crate::triton_vm::op_stack::OpStackElement::ST2,
                                            ),
                                        ),
                                        crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                            crate::triton_vm::instruction::AnInstruction::Lt,
                                        ),
                                        crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                            crate::triton_vm::instruction::AnInstruction::Assert,
                                        ),
                                        crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                            crate::triton_vm::instruction::AnInstruction::AddI(
                                                crate::BFieldElement::new(1u64),
                                            ),
                                        ),
                                        crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                            crate::triton_vm::instruction::AnInstruction::Swap(
                                                crate::triton_vm::op_stack::OpStackElement::ST1,
                                            ),
                                        ),
                                        crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                            crate::triton_vm::instruction::AnInstruction::AddI(
                                                crate::BFieldElement::new(1u64),
                                            ),
                                        ),
                                    ]
                                        .to_vec()
                                }
                            },
                        ]
                            .concat()
                };
                let getter = {
                    if <u64 as crate::twenty_first::math::bfield_codec::BFieldCodec>::static_length(
                    )
                    .is_some()
                    {
                        [
                            crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                crate::triton_vm::instruction::AnInstruction::Pop(
                                    crate::triton_vm::op_stack::NumberOfWords::N1,
                                ),
                            ),
                        ]
                        .to_vec()
                    } else {
                        [
                            crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                crate::triton_vm::instruction::AnInstruction::Pop(
                                    crate::triton_vm::op_stack::NumberOfWords::N1,
                                ),
                            ),
                            crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                crate::triton_vm::instruction::AnInstruction::Push(
                                    crate::BFieldElement::new(1u64),
                                ),
                            ),
                            crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                crate::triton_vm::instruction::AnInstruction::Add,
                            ),
                        ]
                        .to_vec()
                    }
                };
                [current, getter].concat()
            }
            unknown_field_name => {
                panic!("Cannot match on field name `{0}`.", unknown_field_name,);
            }
        };
        let hint_appendix = [
            crate::triton_vm::instruction::LabelledInstruction::TypeHint(
                crate::triton_vm::instruction::TypeHint {
                    starting_index: 0,
                    length: 1,
                    type_name: std::option::Option::<std::string::String>::None,
                    variable_name: std::string::String::from(field_name),
                },
            ),
        ]
        .to_vec();
        [field_getter, hint_appendix].concat()
    }
    fn get_field_with_size(
        field_name: &str,
    ) -> Vec<crate::triton_vm::instruction::LabelledInstruction> {
        let field_getter = match field_name {
            "peaks" => {
                let current = {
                    if let Some(size) = <Vec<
                            Digest,
                        > as crate::twenty_first::math::bfield_codec::BFieldCodec>::static_length() {
                            [
                                crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                    crate::triton_vm::instruction::AnInstruction::Push(
                                        crate::BFieldElement::new(size as u64),
                                    ),
                                ),
                            ]
                                .to_vec()
                        } else {
                            [
                                crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                    crate::triton_vm::instruction::AnInstruction::ReadMem(
                                        crate::triton_vm::op_stack::NumberOfWords::N1,
                                    ),
                                ),
                                crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                    crate::triton_vm::instruction::AnInstruction::Push(
                                        crate::triton_vm::prelude::BFieldElement::new(
                                            Self::MAX_OFFSET.into(),
                                        ),
                                    ),
                                ),
                                crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                    crate::triton_vm::instruction::AnInstruction::Dup(
                                        crate::triton_vm::op_stack::OpStackElement::ST2,
                                    ),
                                ),
                                crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                    crate::triton_vm::instruction::AnInstruction::Lt,
                                ),
                                crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                    crate::triton_vm::instruction::AnInstruction::Assert,
                                ),
                                crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                    crate::triton_vm::instruction::AnInstruction::AddI(
                                        crate::BFieldElement::new(1u64),
                                    ),
                                ),
                                crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                    crate::triton_vm::instruction::AnInstruction::Swap(
                                        crate::triton_vm::op_stack::OpStackElement::ST1,
                                    ),
                                ),
                                crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                    crate::triton_vm::instruction::AnInstruction::AddI(
                                        crate::BFieldElement::new(1u64),
                                    ),
                                ),
                            ]
                                .to_vec()
                        }
                };
                let getter_sizer = {
                    if <Vec<
                            Digest,
                        > as crate::twenty_first::math::bfield_codec::BFieldCodec>::static_length()
                            .is_some()
                        {
                            ::std::vec::Vec::<
                                crate::triton_vm::instruction::LabelledInstruction,
                            >::new()
                        } else {
                            [
                                crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                    crate::triton_vm::instruction::AnInstruction::Push(
                                        -crate::BFieldElement::new(1u64),
                                    ),
                                ),
                                crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                    crate::triton_vm::instruction::AnInstruction::Add,
                                ),
                                crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                    crate::triton_vm::instruction::AnInstruction::Swap(
                                        crate::triton_vm::op_stack::OpStackElement::ST1,
                                    ),
                                ),
                                crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                    crate::triton_vm::instruction::AnInstruction::Push(
                                        crate::BFieldElement::new(1u64),
                                    ),
                                ),
                                crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                    crate::triton_vm::instruction::AnInstruction::Add,
                                ),
                                crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                    crate::triton_vm::instruction::AnInstruction::Swap(
                                        crate::triton_vm::op_stack::OpStackElement::ST1,
                                    ),
                                ),
                            ]
                                .to_vec()
                        }
                };
                [current, getter_sizer].concat()
            }
            "leaf_count" => {
                let current = {
                    [
                            Self::get_field_start_with_jump_distance("peaks"),
                            [
                                crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                    crate::triton_vm::instruction::AnInstruction::Add,
                                ),
                            ]
                                .to_vec(),
                            {
                                if let Some(size) = <u64 as crate::twenty_first::math::bfield_codec::BFieldCodec>::static_length() {
                                    [
                                        crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                            crate::triton_vm::instruction::AnInstruction::Push(
                                                crate::BFieldElement::new(size as u64),
                                            ),
                                        ),
                                    ]
                                        .to_vec()
                                } else {
                                    [
                                        crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                            crate::triton_vm::instruction::AnInstruction::ReadMem(
                                                crate::triton_vm::op_stack::NumberOfWords::N1,
                                            ),
                                        ),
                                        crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                            crate::triton_vm::instruction::AnInstruction::Push(
                                                crate::triton_vm::prelude::BFieldElement::new(
                                                    Self::MAX_OFFSET.into(),
                                                ),
                                            ),
                                        ),
                                        crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                            crate::triton_vm::instruction::AnInstruction::Dup(
                                                crate::triton_vm::op_stack::OpStackElement::ST2,
                                            ),
                                        ),
                                        crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                            crate::triton_vm::instruction::AnInstruction::Lt,
                                        ),
                                        crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                            crate::triton_vm::instruction::AnInstruction::Assert,
                                        ),
                                        crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                            crate::triton_vm::instruction::AnInstruction::AddI(
                                                crate::BFieldElement::new(1u64),
                                            ),
                                        ),
                                        crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                            crate::triton_vm::instruction::AnInstruction::Swap(
                                                crate::triton_vm::op_stack::OpStackElement::ST1,
                                            ),
                                        ),
                                        crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                            crate::triton_vm::instruction::AnInstruction::AddI(
                                                crate::BFieldElement::new(1u64),
                                            ),
                                        ),
                                    ]
                                        .to_vec()
                                }
                            },
                        ]
                            .concat()
                };
                let getter_sizer = {
                    if <u64 as crate::twenty_first::math::bfield_codec::BFieldCodec>::static_length(
                    )
                    .is_some()
                    {
                        ::std::vec::Vec::<crate::triton_vm::instruction::LabelledInstruction>::new()
                    } else {
                        [
                            crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                crate::triton_vm::instruction::AnInstruction::Push(
                                    -crate::BFieldElement::new(1u64),
                                ),
                            ),
                            crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                crate::triton_vm::instruction::AnInstruction::Add,
                            ),
                            crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                crate::triton_vm::instruction::AnInstruction::Swap(
                                    crate::triton_vm::op_stack::OpStackElement::ST1,
                                ),
                            ),
                            crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                crate::triton_vm::instruction::AnInstruction::Push(
                                    crate::BFieldElement::new(1u64),
                                ),
                            ),
                            crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                crate::triton_vm::instruction::AnInstruction::Add,
                            ),
                            crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                crate::triton_vm::instruction::AnInstruction::Swap(
                                    crate::triton_vm::op_stack::OpStackElement::ST1,
                                ),
                            ),
                        ]
                        .to_vec()
                    }
                };
                [current, getter_sizer].concat()
            }
            unknown_field_name => {
                panic!("Cannot match on field name `{0}`.", unknown_field_name,);
            }
        };
        let hint_appendix = [
            crate::triton_vm::instruction::LabelledInstruction::TypeHint(
                crate::triton_vm::instruction::TypeHint {
                    starting_index: 0,
                    length: 1,
                    type_name: std::option::Option::<std::string::String>::Some(
                        std::string::String::from("u32"),
                    ),
                    variable_name: std::string::String::from("size"),
                },
            ),
            crate::triton_vm::instruction::LabelledInstruction::TypeHint(
                crate::triton_vm::instruction::TypeHint {
                    starting_index: 1,
                    length: 1,
                    type_name: std::option::Option::<std::string::String>::None,
                    variable_name: std::string::String::from(field_name),
                },
            ),
        ]
        .to_vec();
        [field_getter, hint_appendix].concat()
    }
    fn get_field_start_with_jump_distance(
        field_name: &str,
    ) -> Vec<crate::triton_vm::instruction::LabelledInstruction> {
        match field_name {
            "peaks" => {
                if let Some(size) = <Vec<
                        Digest,
                    > as crate::twenty_first::math::bfield_codec::BFieldCodec>::static_length() {
                        [
                            crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                crate::triton_vm::instruction::AnInstruction::Push(
                                    crate::BFieldElement::new(size as u64),
                                ),
                            ),
                        ]
                            .to_vec()
                    } else {
                        [
                            crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                crate::triton_vm::instruction::AnInstruction::ReadMem(
                                    crate::triton_vm::op_stack::NumberOfWords::N1,
                                ),
                            ),
                            crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                crate::triton_vm::instruction::AnInstruction::Push(
                                    crate::triton_vm::prelude::BFieldElement::new(
                                        Self::MAX_OFFSET.into(),
                                    ),
                                ),
                            ),
                            crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                crate::triton_vm::instruction::AnInstruction::Dup(
                                    crate::triton_vm::op_stack::OpStackElement::ST2,
                                ),
                            ),
                            crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                crate::triton_vm::instruction::AnInstruction::Lt,
                            ),
                            crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                crate::triton_vm::instruction::AnInstruction::Assert,
                            ),
                            crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                crate::triton_vm::instruction::AnInstruction::AddI(
                                    crate::BFieldElement::new(1u64),
                                ),
                            ),
                            crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                crate::triton_vm::instruction::AnInstruction::Swap(
                                    crate::triton_vm::op_stack::OpStackElement::ST1,
                                ),
                            ),
                            crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                crate::triton_vm::instruction::AnInstruction::AddI(
                                    crate::BFieldElement::new(1u64),
                                ),
                            ),
                        ]
                            .to_vec()
                    }
            }
            "leaf_count" => {
                let prev = [
                    Self::get_field_start_with_jump_distance("peaks"),
                    [
                        crate::triton_vm::instruction::LabelledInstruction::Instruction(
                            crate::triton_vm::instruction::AnInstruction::Add,
                        ),
                    ]
                    .to_vec(),
                ]
                .concat();
                let jumper = {
                    if let Some(size) =
                        <u64 as crate::twenty_first::math::bfield_codec::BFieldCodec>::static_length(
                        )
                    {
                        [
                            crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                crate::triton_vm::instruction::AnInstruction::Push(
                                    crate::BFieldElement::new(size as u64),
                                ),
                            ),
                        ]
                        .to_vec()
                    } else {
                        [
                            crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                crate::triton_vm::instruction::AnInstruction::ReadMem(
                                    crate::triton_vm::op_stack::NumberOfWords::N1,
                                ),
                            ),
                            crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                crate::triton_vm::instruction::AnInstruction::Push(
                                    crate::triton_vm::prelude::BFieldElement::new(
                                        Self::MAX_OFFSET.into(),
                                    ),
                                ),
                            ),
                            crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                crate::triton_vm::instruction::AnInstruction::Dup(
                                    crate::triton_vm::op_stack::OpStackElement::ST2,
                                ),
                            ),
                            crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                crate::triton_vm::instruction::AnInstruction::Lt,
                            ),
                            crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                crate::triton_vm::instruction::AnInstruction::Assert,
                            ),
                            crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                crate::triton_vm::instruction::AnInstruction::AddI(
                                    crate::BFieldElement::new(1u64),
                                ),
                            ),
                            crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                crate::triton_vm::instruction::AnInstruction::Swap(
                                    crate::triton_vm::op_stack::OpStackElement::ST1,
                                ),
                            ),
                            crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                crate::triton_vm::instruction::AnInstruction::AddI(
                                    crate::BFieldElement::new(1u64),
                                ),
                            ),
                        ]
                        .to_vec()
                    }
                };
                [prev, jumper].concat()
            }
            unknown_field_name => {
                panic!("Cannot match on field name `{0}`.", unknown_field_name,);
            }
        }
    }
    fn get_encoding_length() -> Vec<crate::triton_vm::instruction::LabelledInstruction> {
        let first_field_with_jump_distance = {
            [
                    Self::get_field_start_with_jump_distance("peaks"),
                    [
                        crate::triton_vm::instruction::LabelledInstruction::Instruction(
                            crate::triton_vm::instruction::AnInstruction::Add,
                        ),
                    ]
                        .to_vec(),
                    {
                        if let Some(size) = <u64 as crate::twenty_first::math::bfield_codec::BFieldCodec>::static_length() {
                            [
                                crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                    crate::triton_vm::instruction::AnInstruction::Push(
                                        crate::BFieldElement::new(size as u64),
                                    ),
                                ),
                            ]
                                .to_vec()
                        } else {
                            [
                                crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                    crate::triton_vm::instruction::AnInstruction::ReadMem(
                                        crate::triton_vm::op_stack::NumberOfWords::N1,
                                    ),
                                ),
                                crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                    crate::triton_vm::instruction::AnInstruction::Push(
                                        crate::triton_vm::prelude::BFieldElement::new(
                                            Self::MAX_OFFSET.into(),
                                        ),
                                    ),
                                ),
                                crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                    crate::triton_vm::instruction::AnInstruction::Dup(
                                        crate::triton_vm::op_stack::OpStackElement::ST2,
                                    ),
                                ),
                                crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                    crate::triton_vm::instruction::AnInstruction::Lt,
                                ),
                                crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                    crate::triton_vm::instruction::AnInstruction::Assert,
                                ),
                                crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                    crate::triton_vm::instruction::AnInstruction::AddI(
                                        crate::BFieldElement::new(1u64),
                                    ),
                                ),
                                crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                    crate::triton_vm::instruction::AnInstruction::Swap(
                                        crate::triton_vm::op_stack::OpStackElement::ST1,
                                    ),
                                ),
                                crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                    crate::triton_vm::instruction::AnInstruction::AddI(
                                        crate::BFieldElement::new(1u64),
                                    ),
                                ),
                            ]
                                .to_vec()
                        }
                    },
                ]
                    .concat()
        };
        let add = crate::triton_vm::instruction::LabelledInstruction::Instruction(
            crate::triton_vm::instruction::AnInstruction::Add,
        );
        let dup0 = crate::triton_vm::instruction::LabelledInstruction::Instruction(
            crate::triton_vm::instruction::AnInstruction::Dup(
                crate::triton_vm::op_stack::OpStackElement::ST0,
            ),
        );
        let swap1 = crate::triton_vm::instruction::LabelledInstruction::Instruction(
            crate::triton_vm::instruction::AnInstruction::Swap(
                crate::triton_vm::op_stack::OpStackElement::ST1,
            ),
        );
        let push_neg_1 = crate::triton_vm::instruction::LabelledInstruction::Instruction(
            crate::triton_vm::instruction::AnInstruction::Push(
                crate::triton_vm::prelude::BFieldElement::new(
                    crate::triton_vm::prelude::BFieldElement::P - 1,
                ),
            ),
        );
        let mul = crate::triton_vm::instruction::LabelledInstruction::Instruction(
            crate::triton_vm::instruction::AnInstruction::Mul,
        );
        let extract_encoding_size = [add.clone(), swap1, push_neg_1, mul, add].to_vec();
        [
            [dup0].to_vec(),
            first_field_with_jump_distance,
            extract_encoding_size,
        ]
        .concat()
    }
    fn compute_size_and_assert_valid_size_indicator(
        library: &mut crate::tasm_lib::Library,
    ) -> Vec<crate::triton_vm::instruction::LabelledInstruction> {
        let push0 = crate::triton_vm::instruction::LabelledInstruction::Instruction(
            crate::triton_vm::instruction::AnInstruction::Push(0u64.into()),
        );
        let pushmax = crate::triton_vm::instruction::LabelledInstruction::Instruction(
            crate::triton_vm::instruction::AnInstruction::Push(Self::MAX_OFFSET.into()),
        );
        let dup0 = crate::triton_vm::instruction::LabelledInstruction::Instruction(
            crate::triton_vm::instruction::AnInstruction::Dup(
                crate::triton_vm::op_stack::OpStackElement::ST0,
            ),
        );
        let dup1 = crate::triton_vm::instruction::LabelledInstruction::Instruction(
            crate::triton_vm::instruction::AnInstruction::Dup(
                crate::triton_vm::op_stack::OpStackElement::ST1,
            ),
        );
        let dup2 = crate::triton_vm::instruction::LabelledInstruction::Instruction(
            crate::triton_vm::instruction::AnInstruction::Dup(
                crate::triton_vm::op_stack::OpStackElement::ST2,
            ),
        );
        let swap1 = crate::triton_vm::instruction::LabelledInstruction::Instruction(
            crate::triton_vm::instruction::AnInstruction::Swap(
                crate::triton_vm::op_stack::OpStackElement::ST1,
            ),
        );
        let swap2 = crate::triton_vm::instruction::LabelledInstruction::Instruction(
            crate::triton_vm::instruction::AnInstruction::Swap(
                crate::triton_vm::op_stack::OpStackElement::ST2,
            ),
        );
        let lt = crate::triton_vm::instruction::LabelledInstruction::Instruction(
            crate::triton_vm::instruction::AnInstruction::Lt,
        );
        let assert = crate::triton_vm::instruction::LabelledInstruction::Instruction(
            crate::triton_vm::instruction::AnInstruction::Assert,
        );
        let eq = crate::triton_vm::instruction::LabelledInstruction::Instruction(
            crate::triton_vm::instruction::AnInstruction::Eq,
        );
        let add = crate::triton_vm::instruction::LabelledInstruction::Instruction(
            crate::triton_vm::instruction::AnInstruction::Add,
        );
        let read_mem1 = crate::triton_vm::instruction::LabelledInstruction::Instruction(
            crate::triton_vm::instruction::AnInstruction::ReadMem(
                crate::triton_vm::op_stack::NumberOfWords::N1,
            ),
        );
        let addi1 = crate::triton_vm::instruction::LabelledInstruction::Instruction(
            crate::triton_vm::instruction::AnInstruction::AddI(crate::BFieldElement::new(1u64)),
        );
        let addi2 = crate::triton_vm::instruction::LabelledInstruction::Instruction(
            crate::triton_vm::instruction::AnInstruction::AddI(crate::BFieldElement::new(2u64)),
        );
        let pop1 = crate::triton_vm::instruction::LabelledInstruction::Instruction(
            crate::triton_vm::instruction::AnInstruction::Pop(
                crate::triton_vm::op_stack::NumberOfWords::N1,
            ),
        );
        let hint_acc_size = [
            crate::triton_vm::instruction::LabelledInstruction::TypeHint(
                crate::triton_vm::instruction::TypeHint {
                    starting_index: 1,
                    length: 1,
                    type_name: std::option::Option::<std::string::String>::None,
                    variable_name: std::string::String::from("acc_size"),
                },
            ),
        ]
        .to_vec();
        let hint_field_ptr = [
            crate::triton_vm::instruction::LabelledInstruction::TypeHint(
                crate::triton_vm::instruction::TypeHint {
                    starting_index: 0,
                    length: 1,
                    type_name: std::option::Option::<std::string::String>::None,
                    variable_name: std::string::String::from("field_ptr"),
                },
            ),
        ]
        .to_vec();
        [
                [push0.clone(), swap1.clone()].to_vec(),
                hint_acc_size,
                hint_field_ptr,
                if let Some(static_length) = <Vec<
                    Digest,
                > as crate::triton_vm::twenty_first::math::bfield_codec::BFieldCodec>::static_length() {
                    let addi_len = crate::triton_vm::instruction::LabelledInstruction::Instruction(
                        crate::triton_vm::instruction::AnInstruction::AddI(
                            crate::BFieldElement::new(static_length as u64),
                        ),
                    );
                    [addi_len.clone(), swap1.clone(), addi_len.clone(), swap1.clone()]
                        .to_vec()
                } else {
                    [
                        [
                            read_mem1.clone(),
                            pushmax.clone(),
                            dup2.clone(),
                            lt.clone(),
                            assert.clone(),
                            addi2.clone(),
                            dup0.clone(),
                        ]
                            .to_vec(),
                        <Vec<
                            Digest,
                        > as crate::tasm_lib::structure::tasm_object::TasmObject>::compute_size_and_assert_valid_size_indicator(
                            library,
                        ),
                        [
                            dup2.clone(),
                            eq.clone(),
                            assert.clone(),
                            dup1.clone(),
                            add.clone(),
                            swap2.clone(),
                            add.clone(),
                            addi1.clone(),
                            swap1.clone(),
                        ]
                            .to_vec(),
                    ]
                        .concat()
                },
                if let Some(static_length) = <u64 as crate::triton_vm::twenty_first::math::bfield_codec::BFieldCodec>::static_length() {
                    let addi_len = crate::triton_vm::instruction::LabelledInstruction::Instruction(
                        crate::triton_vm::instruction::AnInstruction::AddI(
                            crate::BFieldElement::new(static_length as u64),
                        ),
                    );
                    [addi_len.clone(), swap1.clone(), addi_len.clone(), swap1.clone()]
                        .to_vec()
                } else {
                    [
                        [
                            read_mem1.clone(),
                            pushmax.clone(),
                            dup2.clone(),
                            lt.clone(),
                            assert.clone(),
                            addi2.clone(),
                            dup0.clone(),
                        ]
                            .to_vec(),
                        <u64 as crate::tasm_lib::structure::tasm_object::TasmObject>::compute_size_and_assert_valid_size_indicator(
                            library,
                        ),
                        [
                            dup2.clone(),
                            eq.clone(),
                            assert.clone(),
                            dup1.clone(),
                            add.clone(),
                            swap2.clone(),
                            add.clone(),
                            addi1.clone(),
                            swap1.clone(),
                        ]
                            .to_vec(),
                    ]
                        .concat()
                },
                [pop1.clone()].to_vec(),
            ]
                .concat()
    }
    fn decode_iter<Itr: Iterator<Item = crate::BFieldElement>>(
        iterator: &mut Itr,
    ) -> ::std::result::Result<
        ::std::boxed::Box<Self>,
        ::std::boxed::Box<dyn ::std::error::Error + ::core::marker::Send + ::core::marker::Sync>,
    > {
        let length: usize = if let Some(static_length) =
            <Vec<Digest> as crate::twenty_first::math::bfield_codec::BFieldCodec>::static_length()
        {
            static_length
        } else {
            iterator.next().unwrap().value() as usize
        };
        let sequence = (0..length)
            .map(|_| iterator.next().unwrap())
            .collect::<Vec<_>>();
        let peaks: Vec<Digest> =
            *crate::twenty_first::math::bfield_codec::BFieldCodec::decode(&sequence)?;
        let length: usize = if let Some(static_length) =
            <u64 as crate::twenty_first::math::bfield_codec::BFieldCodec>::static_length()
        {
            static_length
        } else {
            iterator.next().unwrap().value() as usize
        };
        let sequence = (0..length)
            .map(|_| iterator.next().unwrap())
            .collect::<Vec<_>>();
        let leaf_count: u64 =
            *crate::twenty_first::math::bfield_codec::BFieldCodec::decode(&sequence)?;
        ::std::result::Result::Ok(::std::boxed::Box::new(Self::init(peaks, leaf_count)))
    }
}

impl crate::tasm_lib::structure::tasm_object::TasmObject for MmrMembershipProof {
    fn label_friendly_name() -> String {
        "MmrMembershipProof".to_owned()
    }
    fn get_field(field_name: &str) -> Vec<crate::triton_vm::instruction::LabelledInstruction> {
        let field_getter = match field_name {
            "authentication_path" => {
                let current = {
                    if let Some(size) = <Vec<
                            Digest,
                        > as crate::twenty_first::math::bfield_codec::BFieldCodec>::static_length() {
                            [
                                crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                    crate::triton_vm::instruction::AnInstruction::Push(
                                        crate::BFieldElement::new(size as u64),
                                    ),
                                ),
                            ]
                                .to_vec()
                        } else {
                            [
                                crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                    crate::triton_vm::instruction::AnInstruction::ReadMem(
                                        crate::triton_vm::op_stack::NumberOfWords::N1,
                                    ),
                                ),
                                crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                    crate::triton_vm::instruction::AnInstruction::Push(
                                        crate::triton_vm::prelude::BFieldElement::new(
                                            Self::MAX_OFFSET.into(),
                                        ),
                                    ),
                                ),
                                crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                    crate::triton_vm::instruction::AnInstruction::Dup(
                                        crate::triton_vm::op_stack::OpStackElement::ST2,
                                    ),
                                ),
                                crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                    crate::triton_vm::instruction::AnInstruction::Lt,
                                ),
                                crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                    crate::triton_vm::instruction::AnInstruction::Assert,
                                ),
                                crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                    crate::triton_vm::instruction::AnInstruction::AddI(
                                        crate::BFieldElement::new(1u64),
                                    ),
                                ),
                                crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                    crate::triton_vm::instruction::AnInstruction::Swap(
                                        crate::triton_vm::op_stack::OpStackElement::ST1,
                                    ),
                                ),
                                crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                    crate::triton_vm::instruction::AnInstruction::AddI(
                                        crate::BFieldElement::new(1u64),
                                    ),
                                ),
                            ]
                                .to_vec()
                        }
                };
                let getter = {
                    if <Vec<
                            Digest,
                        > as crate::twenty_first::math::bfield_codec::BFieldCodec>::static_length()
                            .is_some()
                        {
                            [
                                crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                    crate::triton_vm::instruction::AnInstruction::Pop(
                                        crate::triton_vm::op_stack::NumberOfWords::N1,
                                    ),
                                ),
                            ]
                                .to_vec()
                        } else {
                            [
                                crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                    crate::triton_vm::instruction::AnInstruction::Pop(
                                        crate::triton_vm::op_stack::NumberOfWords::N1,
                                    ),
                                ),
                                crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                    crate::triton_vm::instruction::AnInstruction::Push(
                                        crate::BFieldElement::new(1u64),
                                    ),
                                ),
                                crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                    crate::triton_vm::instruction::AnInstruction::Add,
                                ),
                            ]
                                .to_vec()
                        }
                };
                [current, getter].concat()
            }
            unknown_field_name => {
                panic!("Cannot match on field name `{0}`.", unknown_field_name,);
            }
        };
        let hint_appendix = [
            crate::triton_vm::instruction::LabelledInstruction::TypeHint(
                crate::triton_vm::instruction::TypeHint {
                    starting_index: 0,
                    length: 1,
                    type_name: std::option::Option::<std::string::String>::None,
                    variable_name: std::string::String::from(field_name),
                },
            ),
        ]
        .to_vec();
        [field_getter, hint_appendix].concat()
    }
    fn get_field_with_size(
        field_name: &str,
    ) -> Vec<crate::triton_vm::instruction::LabelledInstruction> {
        let field_getter = match field_name {
            "authentication_path" => {
                let current = {
                    if let Some(size) = <Vec<
                            Digest,
                        > as crate::twenty_first::math::bfield_codec::BFieldCodec>::static_length() {
                            [
                                crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                    crate::triton_vm::instruction::AnInstruction::Push(
                                        crate::BFieldElement::new(size as u64),
                                    ),
                                ),
                            ]
                                .to_vec()
                        } else {
                            [
                                crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                    crate::triton_vm::instruction::AnInstruction::ReadMem(
                                        crate::triton_vm::op_stack::NumberOfWords::N1,
                                    ),
                                ),
                                crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                    crate::triton_vm::instruction::AnInstruction::Push(
                                        crate::triton_vm::prelude::BFieldElement::new(
                                            Self::MAX_OFFSET.into(),
                                        ),
                                    ),
                                ),
                                crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                    crate::triton_vm::instruction::AnInstruction::Dup(
                                        crate::triton_vm::op_stack::OpStackElement::ST2,
                                    ),
                                ),
                                crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                    crate::triton_vm::instruction::AnInstruction::Lt,
                                ),
                                crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                    crate::triton_vm::instruction::AnInstruction::Assert,
                                ),
                                crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                    crate::triton_vm::instruction::AnInstruction::AddI(
                                        crate::BFieldElement::new(1u64),
                                    ),
                                ),
                                crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                    crate::triton_vm::instruction::AnInstruction::Swap(
                                        crate::triton_vm::op_stack::OpStackElement::ST1,
                                    ),
                                ),
                                crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                    crate::triton_vm::instruction::AnInstruction::AddI(
                                        crate::BFieldElement::new(1u64),
                                    ),
                                ),
                            ]
                                .to_vec()
                        }
                };
                let getter_sizer = {
                    if <Vec<
                            Digest,
                        > as crate::twenty_first::math::bfield_codec::BFieldCodec>::static_length()
                            .is_some()
                        {
                            ::std::vec::Vec::<
                                crate::triton_vm::instruction::LabelledInstruction,
                            >::new()
                        } else {
                            [
                                crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                    crate::triton_vm::instruction::AnInstruction::Push(
                                        -crate::BFieldElement::new(1u64),
                                    ),
                                ),
                                crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                    crate::triton_vm::instruction::AnInstruction::Add,
                                ),
                                crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                    crate::triton_vm::instruction::AnInstruction::Swap(
                                        crate::triton_vm::op_stack::OpStackElement::ST1,
                                    ),
                                ),
                                crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                    crate::triton_vm::instruction::AnInstruction::Push(
                                        crate::BFieldElement::new(1u64),
                                    ),
                                ),
                                crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                    crate::triton_vm::instruction::AnInstruction::Add,
                                ),
                                crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                    crate::triton_vm::instruction::AnInstruction::Swap(
                                        crate::triton_vm::op_stack::OpStackElement::ST1,
                                    ),
                                ),
                            ]
                                .to_vec()
                        }
                };
                [current, getter_sizer].concat()
            }
            unknown_field_name => {
                panic!("Cannot match on field name `{0}`.", unknown_field_name,);
            }
        };
        let hint_appendix = [
            crate::triton_vm::instruction::LabelledInstruction::TypeHint(
                crate::triton_vm::instruction::TypeHint {
                    starting_index: 0,
                    length: 1,
                    type_name: std::option::Option::<std::string::String>::Some(
                        std::string::String::from("u32"),
                    ),
                    variable_name: std::string::String::from("size"),
                },
            ),
            crate::triton_vm::instruction::LabelledInstruction::TypeHint(
                crate::triton_vm::instruction::TypeHint {
                    starting_index: 1,
                    length: 1,
                    type_name: std::option::Option::<std::string::String>::None,
                    variable_name: std::string::String::from(field_name),
                },
            ),
        ]
        .to_vec();
        [field_getter, hint_appendix].concat()
    }
    fn get_field_start_with_jump_distance(
        field_name: &str,
    ) -> Vec<crate::triton_vm::instruction::LabelledInstruction> {
        match field_name {
            "authentication_path" => {
                if let Some(size) = <Vec<
                        Digest,
                    > as crate::twenty_first::math::bfield_codec::BFieldCodec>::static_length() {
                        [
                            crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                crate::triton_vm::instruction::AnInstruction::Push(
                                    crate::BFieldElement::new(size as u64),
                                ),
                            ),
                        ]
                            .to_vec()
                    } else {
                        [
                            crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                crate::triton_vm::instruction::AnInstruction::ReadMem(
                                    crate::triton_vm::op_stack::NumberOfWords::N1,
                                ),
                            ),
                            crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                crate::triton_vm::instruction::AnInstruction::Push(
                                    crate::triton_vm::prelude::BFieldElement::new(
                                        Self::MAX_OFFSET.into(),
                                    ),
                                ),
                            ),
                            crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                crate::triton_vm::instruction::AnInstruction::Dup(
                                    crate::triton_vm::op_stack::OpStackElement::ST2,
                                ),
                            ),
                            crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                crate::triton_vm::instruction::AnInstruction::Lt,
                            ),
                            crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                crate::triton_vm::instruction::AnInstruction::Assert,
                            ),
                            crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                crate::triton_vm::instruction::AnInstruction::AddI(
                                    crate::BFieldElement::new(1u64),
                                ),
                            ),
                            crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                crate::triton_vm::instruction::AnInstruction::Swap(
                                    crate::triton_vm::op_stack::OpStackElement::ST1,
                                ),
                            ),
                            crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                crate::triton_vm::instruction::AnInstruction::AddI(
                                    crate::BFieldElement::new(1u64),
                                ),
                            ),
                        ]
                            .to_vec()
                    }
            }
            unknown_field_name => {
                panic!("Cannot match on field name `{0}`.", unknown_field_name,);
            }
        }
    }
    fn get_encoding_length() -> Vec<crate::triton_vm::instruction::LabelledInstruction> {
        let first_field_with_jump_distance = {
            if let Some(size) =
                <Vec<Digest> as crate::twenty_first::math::bfield_codec::BFieldCodec>::static_length(
                )
            {
                [
                    crate::triton_vm::instruction::LabelledInstruction::Instruction(
                        crate::triton_vm::instruction::AnInstruction::Push(
                            crate::BFieldElement::new(size as u64),
                        ),
                    ),
                ]
                .to_vec()
            } else {
                [
                    crate::triton_vm::instruction::LabelledInstruction::Instruction(
                        crate::triton_vm::instruction::AnInstruction::ReadMem(
                            crate::triton_vm::op_stack::NumberOfWords::N1,
                        ),
                    ),
                    crate::triton_vm::instruction::LabelledInstruction::Instruction(
                        crate::triton_vm::instruction::AnInstruction::Push(
                            crate::triton_vm::prelude::BFieldElement::new(Self::MAX_OFFSET.into()),
                        ),
                    ),
                    crate::triton_vm::instruction::LabelledInstruction::Instruction(
                        crate::triton_vm::instruction::AnInstruction::Dup(
                            crate::triton_vm::op_stack::OpStackElement::ST2,
                        ),
                    ),
                    crate::triton_vm::instruction::LabelledInstruction::Instruction(
                        crate::triton_vm::instruction::AnInstruction::Lt,
                    ),
                    crate::triton_vm::instruction::LabelledInstruction::Instruction(
                        crate::triton_vm::instruction::AnInstruction::Assert,
                    ),
                    crate::triton_vm::instruction::LabelledInstruction::Instruction(
                        crate::triton_vm::instruction::AnInstruction::AddI(
                            crate::BFieldElement::new(1u64),
                        ),
                    ),
                    crate::triton_vm::instruction::LabelledInstruction::Instruction(
                        crate::triton_vm::instruction::AnInstruction::Swap(
                            crate::triton_vm::op_stack::OpStackElement::ST1,
                        ),
                    ),
                    crate::triton_vm::instruction::LabelledInstruction::Instruction(
                        crate::triton_vm::instruction::AnInstruction::AddI(
                            crate::BFieldElement::new(1u64),
                        ),
                    ),
                ]
                .to_vec()
            }
        };
        let add = crate::triton_vm::instruction::LabelledInstruction::Instruction(
            crate::triton_vm::instruction::AnInstruction::Add,
        );
        let dup0 = crate::triton_vm::instruction::LabelledInstruction::Instruction(
            crate::triton_vm::instruction::AnInstruction::Dup(
                crate::triton_vm::op_stack::OpStackElement::ST0,
            ),
        );
        let swap1 = crate::triton_vm::instruction::LabelledInstruction::Instruction(
            crate::triton_vm::instruction::AnInstruction::Swap(
                crate::triton_vm::op_stack::OpStackElement::ST1,
            ),
        );
        let push_neg_1 = crate::triton_vm::instruction::LabelledInstruction::Instruction(
            crate::triton_vm::instruction::AnInstruction::Push(
                crate::triton_vm::prelude::BFieldElement::new(
                    crate::triton_vm::prelude::BFieldElement::P - 1,
                ),
            ),
        );
        let mul = crate::triton_vm::instruction::LabelledInstruction::Instruction(
            crate::triton_vm::instruction::AnInstruction::Mul,
        );
        let extract_encoding_size = [add.clone(), swap1, push_neg_1, mul, add].to_vec();
        [
            [dup0].to_vec(),
            first_field_with_jump_distance,
            extract_encoding_size,
        ]
        .concat()
    }
    fn compute_size_and_assert_valid_size_indicator(
        library: &mut crate::tasm_lib::Library,
    ) -> Vec<crate::triton_vm::instruction::LabelledInstruction> {
        let push0 = crate::triton_vm::instruction::LabelledInstruction::Instruction(
            crate::triton_vm::instruction::AnInstruction::Push(0u64.into()),
        );
        let pushmax = crate::triton_vm::instruction::LabelledInstruction::Instruction(
            crate::triton_vm::instruction::AnInstruction::Push(Self::MAX_OFFSET.into()),
        );
        let dup0 = crate::triton_vm::instruction::LabelledInstruction::Instruction(
            crate::triton_vm::instruction::AnInstruction::Dup(
                crate::triton_vm::op_stack::OpStackElement::ST0,
            ),
        );
        let dup1 = crate::triton_vm::instruction::LabelledInstruction::Instruction(
            crate::triton_vm::instruction::AnInstruction::Dup(
                crate::triton_vm::op_stack::OpStackElement::ST1,
            ),
        );
        let dup2 = crate::triton_vm::instruction::LabelledInstruction::Instruction(
            crate::triton_vm::instruction::AnInstruction::Dup(
                crate::triton_vm::op_stack::OpStackElement::ST2,
            ),
        );
        let swap1 = crate::triton_vm::instruction::LabelledInstruction::Instruction(
            crate::triton_vm::instruction::AnInstruction::Swap(
                crate::triton_vm::op_stack::OpStackElement::ST1,
            ),
        );
        let swap2 = crate::triton_vm::instruction::LabelledInstruction::Instruction(
            crate::triton_vm::instruction::AnInstruction::Swap(
                crate::triton_vm::op_stack::OpStackElement::ST2,
            ),
        );
        let lt = crate::triton_vm::instruction::LabelledInstruction::Instruction(
            crate::triton_vm::instruction::AnInstruction::Lt,
        );
        let assert = crate::triton_vm::instruction::LabelledInstruction::Instruction(
            crate::triton_vm::instruction::AnInstruction::Assert,
        );
        let eq = crate::triton_vm::instruction::LabelledInstruction::Instruction(
            crate::triton_vm::instruction::AnInstruction::Eq,
        );
        let add = crate::triton_vm::instruction::LabelledInstruction::Instruction(
            crate::triton_vm::instruction::AnInstruction::Add,
        );
        let read_mem1 = crate::triton_vm::instruction::LabelledInstruction::Instruction(
            crate::triton_vm::instruction::AnInstruction::ReadMem(
                crate::triton_vm::op_stack::NumberOfWords::N1,
            ),
        );
        let addi1 = crate::triton_vm::instruction::LabelledInstruction::Instruction(
            crate::triton_vm::instruction::AnInstruction::AddI(crate::BFieldElement::new(1u64)),
        );
        let addi2 = crate::triton_vm::instruction::LabelledInstruction::Instruction(
            crate::triton_vm::instruction::AnInstruction::AddI(crate::BFieldElement::new(2u64)),
        );
        let pop1 = crate::triton_vm::instruction::LabelledInstruction::Instruction(
            crate::triton_vm::instruction::AnInstruction::Pop(
                crate::triton_vm::op_stack::NumberOfWords::N1,
            ),
        );
        let hint_acc_size = [
            crate::triton_vm::instruction::LabelledInstruction::TypeHint(
                crate::triton_vm::instruction::TypeHint {
                    starting_index: 1,
                    length: 1,
                    type_name: std::option::Option::<std::string::String>::None,
                    variable_name: std::string::String::from("acc_size"),
                },
            ),
        ]
        .to_vec();
        let hint_field_ptr = [
            crate::triton_vm::instruction::LabelledInstruction::TypeHint(
                crate::triton_vm::instruction::TypeHint {
                    starting_index: 0,
                    length: 1,
                    type_name: std::option::Option::<std::string::String>::None,
                    variable_name: std::string::String::from("field_ptr"),
                },
            ),
        ]
        .to_vec();
        [
                [push0.clone(), swap1.clone()].to_vec(),
                hint_acc_size,
                hint_field_ptr,
                if let Some(static_length) = <Vec<
                    Digest,
                > as crate::triton_vm::twenty_first::math::bfield_codec::BFieldCodec>::static_length() {
                    let addi_len = crate::triton_vm::instruction::LabelledInstruction::Instruction(
                        crate::triton_vm::instruction::AnInstruction::AddI(
                            crate::BFieldElement::new(static_length as u64),
                        ),
                    );
                    [addi_len.clone(), swap1.clone(), addi_len.clone(), swap1.clone()]
                        .to_vec()
                } else {
                    [
                        [
                            read_mem1.clone(),
                            pushmax.clone(),
                            dup2.clone(),
                            lt.clone(),
                            assert.clone(),
                            addi2.clone(),
                            dup0.clone(),
                        ]
                            .to_vec(),
                        <Vec<
                            Digest,
                        > as crate::tasm_lib::structure::tasm_object::TasmObject>::compute_size_and_assert_valid_size_indicator(
                            library,
                        ),
                        [
                            dup2.clone(),
                            eq.clone(),
                            assert.clone(),
                            dup1.clone(),
                            add.clone(),
                            swap2.clone(),
                            add.clone(),
                            addi1.clone(),
                            swap1.clone(),
                        ]
                            .to_vec(),
                    ]
                        .concat()
                },
                [pop1.clone()].to_vec(),
            ]
                .concat()
    }
    fn decode_iter<Itr: Iterator<Item = crate::BFieldElement>>(
        iterator: &mut Itr,
    ) -> ::std::result::Result<
        ::std::boxed::Box<Self>,
        ::std::boxed::Box<dyn ::std::error::Error + ::core::marker::Send + ::core::marker::Sync>,
    > {
        let length: usize = if let Some(static_length) =
            <Vec<Digest> as crate::twenty_first::math::bfield_codec::BFieldCodec>::static_length()
        {
            static_length
        } else {
            iterator.next().unwrap().value() as usize
        };
        let sequence = (0..length)
            .map(|_| iterator.next().unwrap())
            .collect::<Vec<_>>();
        let authentication_path: Vec<Digest> =
            *crate::twenty_first::math::bfield_codec::BFieldCodec::decode(&sequence)?;
        ::std::result::Result::Ok(::std::boxed::Box::new(Self {
            authentication_path,
        }))
    }
}
impl crate::tasm_lib::structure::tasm_object::TasmObject for FriResponse {
    fn label_friendly_name() -> String {
        "FriResponse".to_owned()
    }
    fn get_field(field_name: &str) -> Vec<crate::triton_vm::instruction::LabelledInstruction> {
        let field_getter = match field_name {
            "revealed_leaves" => {
                let current = {
                    if let Some(size) = <Vec<
                            XFieldElement,
                        > as crate::twenty_first::math::bfield_codec::BFieldCodec>::static_length() {
                            [
                                crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                    crate::triton_vm::instruction::AnInstruction::Push(
                                        crate::BFieldElement::new(size as u64),
                                    ),
                                ),
                            ]
                                .to_vec()
                        } else {
                            [
                                crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                    crate::triton_vm::instruction::AnInstruction::ReadMem(
                                        crate::triton_vm::op_stack::NumberOfWords::N1,
                                    ),
                                ),
                                crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                    crate::triton_vm::instruction::AnInstruction::Push(
                                        crate::triton_vm::prelude::BFieldElement::new(
                                            Self::MAX_OFFSET.into(),
                                        ),
                                    ),
                                ),
                                crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                    crate::triton_vm::instruction::AnInstruction::Dup(
                                        crate::triton_vm::op_stack::OpStackElement::ST2,
                                    ),
                                ),
                                crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                    crate::triton_vm::instruction::AnInstruction::Lt,
                                ),
                                crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                    crate::triton_vm::instruction::AnInstruction::Assert,
                                ),
                                crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                    crate::triton_vm::instruction::AnInstruction::AddI(
                                        crate::BFieldElement::new(1u64),
                                    ),
                                ),
                                crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                    crate::triton_vm::instruction::AnInstruction::Swap(
                                        crate::triton_vm::op_stack::OpStackElement::ST1,
                                    ),
                                ),
                                crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                    crate::triton_vm::instruction::AnInstruction::AddI(
                                        crate::BFieldElement::new(1u64),
                                    ),
                                ),
                            ]
                                .to_vec()
                        }
                };
                let getter = {
                    if <Vec<
                            XFieldElement,
                        > as crate::twenty_first::math::bfield_codec::BFieldCodec>::static_length()
                            .is_some()
                        {
                            [
                                crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                    crate::triton_vm::instruction::AnInstruction::Pop(
                                        crate::triton_vm::op_stack::NumberOfWords::N1,
                                    ),
                                ),
                            ]
                                .to_vec()
                        } else {
                            [
                                crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                    crate::triton_vm::instruction::AnInstruction::Pop(
                                        crate::triton_vm::op_stack::NumberOfWords::N1,
                                    ),
                                ),
                                crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                    crate::triton_vm::instruction::AnInstruction::Push(
                                        crate::BFieldElement::new(1u64),
                                    ),
                                ),
                                crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                    crate::triton_vm::instruction::AnInstruction::Add,
                                ),
                            ]
                                .to_vec()
                        }
                };
                [current, getter].concat()
            }
            "auth_structure" => {
                let current = {
                    [
                            Self::get_field_start_with_jump_distance("revealed_leaves"),
                            [
                                crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                    crate::triton_vm::instruction::AnInstruction::Add,
                                ),
                            ]
                                .to_vec(),
                            {
                                if let Some(size) = <AuthenticationStructure as crate::twenty_first::math::bfield_codec::BFieldCodec>::static_length() {
                                    [
                                        crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                            crate::triton_vm::instruction::AnInstruction::Push(
                                                crate::BFieldElement::new(size as u64),
                                            ),
                                        ),
                                    ]
                                        .to_vec()
                                } else {
                                    [
                                        crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                            crate::triton_vm::instruction::AnInstruction::ReadMem(
                                                crate::triton_vm::op_stack::NumberOfWords::N1,
                                            ),
                                        ),
                                        crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                            crate::triton_vm::instruction::AnInstruction::Push(
                                                crate::triton_vm::prelude::BFieldElement::new(
                                                    Self::MAX_OFFSET.into(),
                                                ),
                                            ),
                                        ),
                                        crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                            crate::triton_vm::instruction::AnInstruction::Dup(
                                                crate::triton_vm::op_stack::OpStackElement::ST2,
                                            ),
                                        ),
                                        crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                            crate::triton_vm::instruction::AnInstruction::Lt,
                                        ),
                                        crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                            crate::triton_vm::instruction::AnInstruction::Assert,
                                        ),
                                        crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                            crate::triton_vm::instruction::AnInstruction::AddI(
                                                crate::BFieldElement::new(1u64),
                                            ),
                                        ),
                                        crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                            crate::triton_vm::instruction::AnInstruction::Swap(
                                                crate::triton_vm::op_stack::OpStackElement::ST1,
                                            ),
                                        ),
                                        crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                            crate::triton_vm::instruction::AnInstruction::AddI(
                                                crate::BFieldElement::new(1u64),
                                            ),
                                        ),
                                    ]
                                        .to_vec()
                                }
                            },
                        ]
                            .concat()
                };
                let getter = {
                    if <AuthenticationStructure as crate::twenty_first::math::bfield_codec::BFieldCodec>::static_length()
                            .is_some()
                        {
                            [
                                crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                    crate::triton_vm::instruction::AnInstruction::Pop(
                                        crate::triton_vm::op_stack::NumberOfWords::N1,
                                    ),
                                ),
                            ]
                                .to_vec()
                        } else {
                            [
                                crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                    crate::triton_vm::instruction::AnInstruction::Pop(
                                        crate::triton_vm::op_stack::NumberOfWords::N1,
                                    ),
                                ),
                                crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                    crate::triton_vm::instruction::AnInstruction::Push(
                                        crate::BFieldElement::new(1u64),
                                    ),
                                ),
                                crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                    crate::triton_vm::instruction::AnInstruction::Add,
                                ),
                            ]
                                .to_vec()
                        }
                };
                [current, getter].concat()
            }
            unknown_field_name => {
                panic!("Cannot match on field name `{0}`.", unknown_field_name,);
            }
        };
        let hint_appendix = [
            crate::triton_vm::instruction::LabelledInstruction::TypeHint(
                crate::triton_vm::instruction::TypeHint {
                    starting_index: 0,
                    length: 1,
                    type_name: std::option::Option::<std::string::String>::None,
                    variable_name: std::string::String::from(field_name),
                },
            ),
        ]
        .to_vec();
        [field_getter, hint_appendix].concat()
    }
    fn get_field_with_size(
        field_name: &str,
    ) -> Vec<crate::triton_vm::instruction::LabelledInstruction> {
        let field_getter = match field_name {
            "revealed_leaves" => {
                let current = {
                    if let Some(size) = <Vec<
                            XFieldElement,
                        > as crate::twenty_first::math::bfield_codec::BFieldCodec>::static_length() {
                            [
                                crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                    crate::triton_vm::instruction::AnInstruction::Push(
                                        crate::BFieldElement::new(size as u64),
                                    ),
                                ),
                            ]
                                .to_vec()
                        } else {
                            [
                                crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                    crate::triton_vm::instruction::AnInstruction::ReadMem(
                                        crate::triton_vm::op_stack::NumberOfWords::N1,
                                    ),
                                ),
                                crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                    crate::triton_vm::instruction::AnInstruction::Push(
                                        crate::triton_vm::prelude::BFieldElement::new(
                                            Self::MAX_OFFSET.into(),
                                        ),
                                    ),
                                ),
                                crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                    crate::triton_vm::instruction::AnInstruction::Dup(
                                        crate::triton_vm::op_stack::OpStackElement::ST2,
                                    ),
                                ),
                                crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                    crate::triton_vm::instruction::AnInstruction::Lt,
                                ),
                                crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                    crate::triton_vm::instruction::AnInstruction::Assert,
                                ),
                                crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                    crate::triton_vm::instruction::AnInstruction::AddI(
                                        crate::BFieldElement::new(1u64),
                                    ),
                                ),
                                crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                    crate::triton_vm::instruction::AnInstruction::Swap(
                                        crate::triton_vm::op_stack::OpStackElement::ST1,
                                    ),
                                ),
                                crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                    crate::triton_vm::instruction::AnInstruction::AddI(
                                        crate::BFieldElement::new(1u64),
                                    ),
                                ),
                            ]
                                .to_vec()
                        }
                };
                let getter_sizer = {
                    if <Vec<
                            XFieldElement,
                        > as crate::twenty_first::math::bfield_codec::BFieldCodec>::static_length()
                            .is_some()
                        {
                            ::std::vec::Vec::<
                                crate::triton_vm::instruction::LabelledInstruction,
                            >::new()
                        } else {
                            [
                                crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                    crate::triton_vm::instruction::AnInstruction::Push(
                                        -crate::BFieldElement::new(1u64),
                                    ),
                                ),
                                crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                    crate::triton_vm::instruction::AnInstruction::Add,
                                ),
                                crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                    crate::triton_vm::instruction::AnInstruction::Swap(
                                        crate::triton_vm::op_stack::OpStackElement::ST1,
                                    ),
                                ),
                                crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                    crate::triton_vm::instruction::AnInstruction::Push(
                                        crate::BFieldElement::new(1u64),
                                    ),
                                ),
                                crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                    crate::triton_vm::instruction::AnInstruction::Add,
                                ),
                                crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                    crate::triton_vm::instruction::AnInstruction::Swap(
                                        crate::triton_vm::op_stack::OpStackElement::ST1,
                                    ),
                                ),
                            ]
                                .to_vec()
                        }
                };
                [current, getter_sizer].concat()
            }
            "auth_structure" => {
                let current = {
                    [
                            Self::get_field_start_with_jump_distance("revealed_leaves"),
                            [
                                crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                    crate::triton_vm::instruction::AnInstruction::Add,
                                ),
                            ]
                                .to_vec(),
                            {
                                if let Some(size) = <AuthenticationStructure as crate::twenty_first::math::bfield_codec::BFieldCodec>::static_length() {
                                    [
                                        crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                            crate::triton_vm::instruction::AnInstruction::Push(
                                                crate::BFieldElement::new(size as u64),
                                            ),
                                        ),
                                    ]
                                        .to_vec()
                                } else {
                                    [
                                        crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                            crate::triton_vm::instruction::AnInstruction::ReadMem(
                                                crate::triton_vm::op_stack::NumberOfWords::N1,
                                            ),
                                        ),
                                        crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                            crate::triton_vm::instruction::AnInstruction::Push(
                                                crate::triton_vm::prelude::BFieldElement::new(
                                                    Self::MAX_OFFSET.into(),
                                                ),
                                            ),
                                        ),
                                        crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                            crate::triton_vm::instruction::AnInstruction::Dup(
                                                crate::triton_vm::op_stack::OpStackElement::ST2,
                                            ),
                                        ),
                                        crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                            crate::triton_vm::instruction::AnInstruction::Lt,
                                        ),
                                        crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                            crate::triton_vm::instruction::AnInstruction::Assert,
                                        ),
                                        crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                            crate::triton_vm::instruction::AnInstruction::AddI(
                                                crate::BFieldElement::new(1u64),
                                            ),
                                        ),
                                        crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                            crate::triton_vm::instruction::AnInstruction::Swap(
                                                crate::triton_vm::op_stack::OpStackElement::ST1,
                                            ),
                                        ),
                                        crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                            crate::triton_vm::instruction::AnInstruction::AddI(
                                                crate::BFieldElement::new(1u64),
                                            ),
                                        ),
                                    ]
                                        .to_vec()
                                }
                            },
                        ]
                            .concat()
                };
                let getter_sizer = {
                    if <AuthenticationStructure as crate::twenty_first::math::bfield_codec::BFieldCodec>::static_length()
                            .is_some()
                        {
                            ::std::vec::Vec::<
                                crate::triton_vm::instruction::LabelledInstruction,
                            >::new()
                        } else {
                            [
                                crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                    crate::triton_vm::instruction::AnInstruction::Push(
                                        -crate::BFieldElement::new(1u64),
                                    ),
                                ),
                                crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                    crate::triton_vm::instruction::AnInstruction::Add,
                                ),
                                crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                    crate::triton_vm::instruction::AnInstruction::Swap(
                                        crate::triton_vm::op_stack::OpStackElement::ST1,
                                    ),
                                ),
                                crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                    crate::triton_vm::instruction::AnInstruction::Push(
                                        crate::BFieldElement::new(1u64),
                                    ),
                                ),
                                crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                    crate::triton_vm::instruction::AnInstruction::Add,
                                ),
                                crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                    crate::triton_vm::instruction::AnInstruction::Swap(
                                        crate::triton_vm::op_stack::OpStackElement::ST1,
                                    ),
                                ),
                            ]
                                .to_vec()
                        }
                };
                [current, getter_sizer].concat()
            }
            unknown_field_name => {
                panic!("Cannot match on field name `{0}`.", unknown_field_name,);
            }
        };
        let hint_appendix = [
            crate::triton_vm::instruction::LabelledInstruction::TypeHint(
                crate::triton_vm::instruction::TypeHint {
                    starting_index: 0,
                    length: 1,
                    type_name: std::option::Option::<std::string::String>::Some(
                        std::string::String::from("u32"),
                    ),
                    variable_name: std::string::String::from("size"),
                },
            ),
            crate::triton_vm::instruction::LabelledInstruction::TypeHint(
                crate::triton_vm::instruction::TypeHint {
                    starting_index: 1,
                    length: 1,
                    type_name: std::option::Option::<std::string::String>::None,
                    variable_name: std::string::String::from(field_name),
                },
            ),
        ]
        .to_vec();
        [field_getter, hint_appendix].concat()
    }
    fn get_field_start_with_jump_distance(
        field_name: &str,
    ) -> Vec<crate::triton_vm::instruction::LabelledInstruction> {
        match field_name {
                "revealed_leaves" => {
                    if let Some(size) = <Vec<
                        XFieldElement,
                    > as crate::twenty_first::math::bfield_codec::BFieldCodec>::static_length() {
                        [
                            crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                crate::triton_vm::instruction::AnInstruction::Push(
                                    crate::BFieldElement::new(size as u64),
                                ),
                            ),
                        ]
                            .to_vec()
                    } else {
                        [
                            crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                crate::triton_vm::instruction::AnInstruction::ReadMem(
                                    crate::triton_vm::op_stack::NumberOfWords::N1,
                                ),
                            ),
                            crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                crate::triton_vm::instruction::AnInstruction::Push(
                                    crate::triton_vm::prelude::BFieldElement::new(
                                        Self::MAX_OFFSET.into(),
                                    ),
                                ),
                            ),
                            crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                crate::triton_vm::instruction::AnInstruction::Dup(
                                    crate::triton_vm::op_stack::OpStackElement::ST2,
                                ),
                            ),
                            crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                crate::triton_vm::instruction::AnInstruction::Lt,
                            ),
                            crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                crate::triton_vm::instruction::AnInstruction::Assert,
                            ),
                            crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                crate::triton_vm::instruction::AnInstruction::AddI(
                                    crate::BFieldElement::new(1u64),
                                ),
                            ),
                            crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                crate::triton_vm::instruction::AnInstruction::Swap(
                                    crate::triton_vm::op_stack::OpStackElement::ST1,
                                ),
                            ),
                            crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                crate::triton_vm::instruction::AnInstruction::AddI(
                                    crate::BFieldElement::new(1u64),
                                ),
                            ),
                        ]
                            .to_vec()
                    }
                }
                "auth_structure" => {
                    let prev = [
                        Self::get_field_start_with_jump_distance("revealed_leaves"),
                        [
                            crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                crate::triton_vm::instruction::AnInstruction::Add,
                            ),
                        ]
                            .to_vec(),
                    ]
                        .concat();
                    let jumper = {
                        if let Some(size) = <AuthenticationStructure as crate::twenty_first::math::bfield_codec::BFieldCodec>::static_length() {
                            [
                                crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                    crate::triton_vm::instruction::AnInstruction::Push(
                                        crate::BFieldElement::new(size as u64),
                                    ),
                                ),
                            ]
                                .to_vec()
                        } else {
                            [
                                crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                    crate::triton_vm::instruction::AnInstruction::ReadMem(
                                        crate::triton_vm::op_stack::NumberOfWords::N1,
                                    ),
                                ),
                                crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                    crate::triton_vm::instruction::AnInstruction::Push(
                                        crate::triton_vm::prelude::BFieldElement::new(
                                            Self::MAX_OFFSET.into(),
                                        ),
                                    ),
                                ),
                                crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                    crate::triton_vm::instruction::AnInstruction::Dup(
                                        crate::triton_vm::op_stack::OpStackElement::ST2,
                                    ),
                                ),
                                crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                    crate::triton_vm::instruction::AnInstruction::Lt,
                                ),
                                crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                    crate::triton_vm::instruction::AnInstruction::Assert,
                                ),
                                crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                    crate::triton_vm::instruction::AnInstruction::AddI(
                                        crate::BFieldElement::new(1u64),
                                    ),
                                ),
                                crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                    crate::triton_vm::instruction::AnInstruction::Swap(
                                        crate::triton_vm::op_stack::OpStackElement::ST1,
                                    ),
                                ),
                                crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                    crate::triton_vm::instruction::AnInstruction::AddI(
                                        crate::BFieldElement::new(1u64),
                                    ),
                                ),
                            ]
                                .to_vec()
                        }
                    };
                    [prev, jumper].concat()
                }
                unknown_field_name => {
                    panic!(
                            "Cannot match on field name `{0}`.",
                            unknown_field_name,
                    );
                }
            }
    }
    fn get_encoding_length() -> Vec<crate::triton_vm::instruction::LabelledInstruction> {
        let first_field_with_jump_distance = {
            [
                    Self::get_field_start_with_jump_distance("revealed_leaves"),
                    [
                        crate::triton_vm::instruction::LabelledInstruction::Instruction(
                            crate::triton_vm::instruction::AnInstruction::Add,
                        ),
                    ]
                        .to_vec(),
                    {
                        if let Some(size) = <AuthenticationStructure as crate::twenty_first::math::bfield_codec::BFieldCodec>::static_length() {
                            [
                                crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                    crate::triton_vm::instruction::AnInstruction::Push(
                                        crate::BFieldElement::new(size as u64),
                                    ),
                                ),
                            ]
                                .to_vec()
                        } else {
                            [
                                crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                    crate::triton_vm::instruction::AnInstruction::ReadMem(
                                        crate::triton_vm::op_stack::NumberOfWords::N1,
                                    ),
                                ),
                                crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                    crate::triton_vm::instruction::AnInstruction::Push(
                                        crate::triton_vm::prelude::BFieldElement::new(
                                            Self::MAX_OFFSET.into(),
                                        ),
                                    ),
                                ),
                                crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                    crate::triton_vm::instruction::AnInstruction::Dup(
                                        crate::triton_vm::op_stack::OpStackElement::ST2,
                                    ),
                                ),
                                crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                    crate::triton_vm::instruction::AnInstruction::Lt,
                                ),
                                crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                    crate::triton_vm::instruction::AnInstruction::Assert,
                                ),
                                crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                    crate::triton_vm::instruction::AnInstruction::AddI(
                                        crate::BFieldElement::new(1u64),
                                    ),
                                ),
                                crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                    crate::triton_vm::instruction::AnInstruction::Swap(
                                        crate::triton_vm::op_stack::OpStackElement::ST1,
                                    ),
                                ),
                                crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                    crate::triton_vm::instruction::AnInstruction::AddI(
                                        crate::BFieldElement::new(1u64),
                                    ),
                                ),
                            ]
                                .to_vec()
                        }
                    },
                ]
                    .concat()
        };
        let add = crate::triton_vm::instruction::LabelledInstruction::Instruction(
            crate::triton_vm::instruction::AnInstruction::Add,
        );
        let dup0 = crate::triton_vm::instruction::LabelledInstruction::Instruction(
            crate::triton_vm::instruction::AnInstruction::Dup(
                crate::triton_vm::op_stack::OpStackElement::ST0,
            ),
        );
        let swap1 = crate::triton_vm::instruction::LabelledInstruction::Instruction(
            crate::triton_vm::instruction::AnInstruction::Swap(
                crate::triton_vm::op_stack::OpStackElement::ST1,
            ),
        );
        let push_neg_1 = crate::triton_vm::instruction::LabelledInstruction::Instruction(
            crate::triton_vm::instruction::AnInstruction::Push(
                crate::triton_vm::prelude::BFieldElement::new(
                    crate::triton_vm::prelude::BFieldElement::P - 1,
                ),
            ),
        );
        let mul = crate::triton_vm::instruction::LabelledInstruction::Instruction(
            crate::triton_vm::instruction::AnInstruction::Mul,
        );
        let extract_encoding_size = [add.clone(), swap1, push_neg_1, mul, add].to_vec();
        [
            [dup0].to_vec(),
            first_field_with_jump_distance,
            extract_encoding_size,
        ]
        .concat()
    }
    fn compute_size_and_assert_valid_size_indicator(
        library: &mut crate::tasm_lib::Library,
    ) -> Vec<crate::triton_vm::instruction::LabelledInstruction> {
        let push0 = crate::triton_vm::instruction::LabelledInstruction::Instruction(
            crate::triton_vm::instruction::AnInstruction::Push(0u64.into()),
        );
        let pushmax = crate::triton_vm::instruction::LabelledInstruction::Instruction(
            crate::triton_vm::instruction::AnInstruction::Push(Self::MAX_OFFSET.into()),
        );
        let dup0 = crate::triton_vm::instruction::LabelledInstruction::Instruction(
            crate::triton_vm::instruction::AnInstruction::Dup(
                crate::triton_vm::op_stack::OpStackElement::ST0,
            ),
        );
        let dup1 = crate::triton_vm::instruction::LabelledInstruction::Instruction(
            crate::triton_vm::instruction::AnInstruction::Dup(
                crate::triton_vm::op_stack::OpStackElement::ST1,
            ),
        );
        let dup2 = crate::triton_vm::instruction::LabelledInstruction::Instruction(
            crate::triton_vm::instruction::AnInstruction::Dup(
                crate::triton_vm::op_stack::OpStackElement::ST2,
            ),
        );
        let swap1 = crate::triton_vm::instruction::LabelledInstruction::Instruction(
            crate::triton_vm::instruction::AnInstruction::Swap(
                crate::triton_vm::op_stack::OpStackElement::ST1,
            ),
        );
        let swap2 = crate::triton_vm::instruction::LabelledInstruction::Instruction(
            crate::triton_vm::instruction::AnInstruction::Swap(
                crate::triton_vm::op_stack::OpStackElement::ST2,
            ),
        );
        let lt = crate::triton_vm::instruction::LabelledInstruction::Instruction(
            crate::triton_vm::instruction::AnInstruction::Lt,
        );
        let assert = crate::triton_vm::instruction::LabelledInstruction::Instruction(
            crate::triton_vm::instruction::AnInstruction::Assert,
        );
        let eq = crate::triton_vm::instruction::LabelledInstruction::Instruction(
            crate::triton_vm::instruction::AnInstruction::Eq,
        );
        let add = crate::triton_vm::instruction::LabelledInstruction::Instruction(
            crate::triton_vm::instruction::AnInstruction::Add,
        );
        let read_mem1 = crate::triton_vm::instruction::LabelledInstruction::Instruction(
            crate::triton_vm::instruction::AnInstruction::ReadMem(
                crate::triton_vm::op_stack::NumberOfWords::N1,
            ),
        );
        let addi1 = crate::triton_vm::instruction::LabelledInstruction::Instruction(
            crate::triton_vm::instruction::AnInstruction::AddI(crate::BFieldElement::new(1u64)),
        );
        let addi2 = crate::triton_vm::instruction::LabelledInstruction::Instruction(
            crate::triton_vm::instruction::AnInstruction::AddI(crate::BFieldElement::new(2u64)),
        );
        let pop1 = crate::triton_vm::instruction::LabelledInstruction::Instruction(
            crate::triton_vm::instruction::AnInstruction::Pop(
                crate::triton_vm::op_stack::NumberOfWords::N1,
            ),
        );
        let hint_acc_size = [
            crate::triton_vm::instruction::LabelledInstruction::TypeHint(
                crate::triton_vm::instruction::TypeHint {
                    starting_index: 1,
                    length: 1,
                    type_name: std::option::Option::<std::string::String>::None,
                    variable_name: std::string::String::from("acc_size"),
                },
            ),
        ]
        .to_vec();
        let hint_field_ptr = [
            crate::triton_vm::instruction::LabelledInstruction::TypeHint(
                crate::triton_vm::instruction::TypeHint {
                    starting_index: 0,
                    length: 1,
                    type_name: std::option::Option::<std::string::String>::None,
                    variable_name: std::string::String::from("field_ptr"),
                },
            ),
        ]
        .to_vec();
        [
                [push0.clone(), swap1.clone()].to_vec(),
                hint_acc_size,
                hint_field_ptr,
                if let Some(static_length) = <Vec<
                    XFieldElement,
                > as crate::triton_vm::twenty_first::math::bfield_codec::BFieldCodec>::static_length() {
                    let addi_len = crate::triton_vm::instruction::LabelledInstruction::Instruction(
                        crate::triton_vm::instruction::AnInstruction::AddI(
                            crate::BFieldElement::new(static_length as u64),
                        ),
                    );
                    [addi_len.clone(), swap1.clone(), addi_len.clone(), swap1.clone()]
                        .to_vec()
                } else {
                    [
                        [
                            read_mem1.clone(),
                            pushmax.clone(),
                            dup2.clone(),
                            lt.clone(),
                            assert.clone(),
                            addi2.clone(),
                            dup0.clone(),
                        ]
                            .to_vec(),
                        <Vec<
                            XFieldElement,
                        > as crate::tasm_lib::structure::tasm_object::TasmObject>::compute_size_and_assert_valid_size_indicator(
                            library,
                        ),
                        [
                            dup2.clone(),
                            eq.clone(),
                            assert.clone(),
                            dup1.clone(),
                            add.clone(),
                            swap2.clone(),
                            add.clone(),
                            addi1.clone(),
                            swap1.clone(),
                        ]
                            .to_vec(),
                    ]
                        .concat()
                },
                if let Some(static_length) = <AuthenticationStructure as crate::triton_vm::twenty_first::math::bfield_codec::BFieldCodec>::static_length() {
                    let addi_len = crate::triton_vm::instruction::LabelledInstruction::Instruction(
                        crate::triton_vm::instruction::AnInstruction::AddI(
                            crate::BFieldElement::new(static_length as u64),
                        ),
                    );
                    [addi_len.clone(), swap1.clone(), addi_len.clone(), swap1.clone()]
                        .to_vec()
                } else {
                    [
                        [
                            read_mem1.clone(),
                            pushmax.clone(),
                            dup2.clone(),
                            lt.clone(),
                            assert.clone(),
                            addi2.clone(),
                            dup0.clone(),
                        ]
                            .to_vec(),
                        <AuthenticationStructure as crate::tasm_lib::structure::tasm_object::TasmObject>::compute_size_and_assert_valid_size_indicator(
                            library,
                        ),
                        [
                            dup2.clone(),
                            eq.clone(),
                            assert.clone(),
                            dup1.clone(),
                            add.clone(),
                            swap2.clone(),
                            add.clone(),
                            addi1.clone(),
                            swap1.clone(),
                        ]
                            .to_vec(),
                    ]
                        .concat()
                },
                [pop1.clone()].to_vec(),
            ]
                .concat()
    }
    fn decode_iter<Itr: Iterator<Item = crate::BFieldElement>>(
        iterator: &mut Itr,
    ) -> ::std::result::Result<
        ::std::boxed::Box<Self>,
        ::std::boxed::Box<dyn ::std::error::Error + ::core::marker::Send + ::core::marker::Sync>,
    > {
        let length: usize = if let Some(static_length) = <Vec<
                XFieldElement,
            > as crate::twenty_first::math::bfield_codec::BFieldCodec>::static_length() {
                static_length
            } else {
                iterator.next().unwrap().value() as usize
            };
        let sequence = (0..length)
            .map(|_| iterator.next().unwrap())
            .collect::<Vec<_>>();
        let revealed_leaves: Vec<XFieldElement> =
            *crate::twenty_first::math::bfield_codec::BFieldCodec::decode(&sequence)?;
        let length: usize = if let Some(static_length) = <AuthenticationStructure as crate::twenty_first::math::bfield_codec::BFieldCodec>::static_length() {
                static_length
            } else {
                iterator.next().unwrap().value() as usize
            };
        let sequence = (0..length)
            .map(|_| iterator.next().unwrap())
            .collect::<Vec<_>>();
        let auth_structure: AuthenticationStructure =
            *crate::twenty_first::math::bfield_codec::BFieldCodec::decode(&sequence)?;
        ::std::result::Result::Ok(::std::boxed::Box::new(Self {
            revealed_leaves,
            auth_structure,
        }))
    }
}

impl crate::tasm_lib::structure::tasm_object::TasmObject for Claim {
    fn label_friendly_name() -> String {
        "Claim".to_owned()
    }
    fn get_field(field_name: &str) -> Vec<crate::triton_vm::instruction::LabelledInstruction> {
        let field_getter = match field_name {
            "output" => {
                let current = {
                    if let Some(size) = <Vec<
                            BFieldElement,
                        > as crate::twenty_first::math::bfield_codec::BFieldCodec>::static_length() {
                            [
                                crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                    crate::triton_vm::instruction::AnInstruction::Push(
                                        crate::BFieldElement::new(size as u64),
                                    ),
                                ),
                            ]
                                .to_vec()
                        } else {
                            [
                                crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                    crate::triton_vm::instruction::AnInstruction::ReadMem(
                                        crate::triton_vm::op_stack::NumberOfWords::N1,
                                    ),
                                ),
                                crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                    crate::triton_vm::instruction::AnInstruction::Push(
                                        crate::triton_vm::prelude::BFieldElement::new(
                                            Self::MAX_OFFSET.into(),
                                        ),
                                    ),
                                ),
                                crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                    crate::triton_vm::instruction::AnInstruction::Dup(
                                        crate::triton_vm::op_stack::OpStackElement::ST2,
                                    ),
                                ),
                                crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                    crate::triton_vm::instruction::AnInstruction::Lt,
                                ),
                                crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                    crate::triton_vm::instruction::AnInstruction::Assert,
                                ),
                                crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                    crate::triton_vm::instruction::AnInstruction::AddI(
                                        crate::BFieldElement::new(1u64),
                                    ),
                                ),
                                crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                    crate::triton_vm::instruction::AnInstruction::Swap(
                                        crate::triton_vm::op_stack::OpStackElement::ST1,
                                    ),
                                ),
                                crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                    crate::triton_vm::instruction::AnInstruction::AddI(
                                        crate::BFieldElement::new(1u64),
                                    ),
                                ),
                            ]
                                .to_vec()
                        }
                };
                let getter = {
                    if <Vec<
                            BFieldElement,
                        > as crate::twenty_first::math::bfield_codec::BFieldCodec>::static_length()
                            .is_some()
                        {
                            [
                                crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                    crate::triton_vm::instruction::AnInstruction::Pop(
                                        crate::triton_vm::op_stack::NumberOfWords::N1,
                                    ),
                                ),
                            ]
                                .to_vec()
                        } else {
                            [
                                crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                    crate::triton_vm::instruction::AnInstruction::Pop(
                                        crate::triton_vm::op_stack::NumberOfWords::N1,
                                    ),
                                ),
                                crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                    crate::triton_vm::instruction::AnInstruction::Push(
                                        crate::BFieldElement::new(1u64),
                                    ),
                                ),
                                crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                    crate::triton_vm::instruction::AnInstruction::Add,
                                ),
                            ]
                                .to_vec()
                        }
                };
                [current, getter].concat()
            }
            "input" => {
                let current = {
                    [
                            Self::get_field_start_with_jump_distance("output"),
                            [
                                crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                    crate::triton_vm::instruction::AnInstruction::Add,
                                ),
                            ]
                                .to_vec(),
                            {
                                if let Some(size) = <Vec<
                                    BFieldElement,
                                > as crate::twenty_first::math::bfield_codec::BFieldCodec>::static_length() {
                                    [
                                        crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                            crate::triton_vm::instruction::AnInstruction::Push(
                                                crate::BFieldElement::new(size as u64),
                                            ),
                                        ),
                                    ]
                                        .to_vec()
                                } else {
                                    [
                                        crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                            crate::triton_vm::instruction::AnInstruction::ReadMem(
                                                crate::triton_vm::op_stack::NumberOfWords::N1,
                                            ),
                                        ),
                                        crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                            crate::triton_vm::instruction::AnInstruction::Push(
                                                crate::triton_vm::prelude::BFieldElement::new(
                                                    Self::MAX_OFFSET.into(),
                                                ),
                                            ),
                                        ),
                                        crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                            crate::triton_vm::instruction::AnInstruction::Dup(
                                                crate::triton_vm::op_stack::OpStackElement::ST2,
                                            ),
                                        ),
                                        crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                            crate::triton_vm::instruction::AnInstruction::Lt,
                                        ),
                                        crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                            crate::triton_vm::instruction::AnInstruction::Assert,
                                        ),
                                        crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                            crate::triton_vm::instruction::AnInstruction::AddI(
                                                crate::BFieldElement::new(1u64),
                                            ),
                                        ),
                                        crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                            crate::triton_vm::instruction::AnInstruction::Swap(
                                                crate::triton_vm::op_stack::OpStackElement::ST1,
                                            ),
                                        ),
                                        crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                            crate::triton_vm::instruction::AnInstruction::AddI(
                                                crate::BFieldElement::new(1u64),
                                            ),
                                        ),
                                    ]
                                        .to_vec()
                                }
                            },
                        ]
                            .concat()
                };
                let getter = {
                    if <Vec<
                            BFieldElement,
                        > as crate::twenty_first::math::bfield_codec::BFieldCodec>::static_length()
                            .is_some()
                        {
                            [
                                crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                    crate::triton_vm::instruction::AnInstruction::Pop(
                                        crate::triton_vm::op_stack::NumberOfWords::N1,
                                    ),
                                ),
                            ]
                                .to_vec()
                        } else {
                            [
                                crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                    crate::triton_vm::instruction::AnInstruction::Pop(
                                        crate::triton_vm::op_stack::NumberOfWords::N1,
                                    ),
                                ),
                                crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                    crate::triton_vm::instruction::AnInstruction::Push(
                                        crate::BFieldElement::new(1u64),
                                    ),
                                ),
                                crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                    crate::triton_vm::instruction::AnInstruction::Add,
                                ),
                            ]
                                .to_vec()
                        }
                };
                [current, getter].concat()
            }
            "program_digest" => {
                let current = {
                    [
                            Self::get_field_start_with_jump_distance("input"),
                            [
                                crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                    crate::triton_vm::instruction::AnInstruction::Add,
                                ),
                            ]
                                .to_vec(),
                            {
                                if let Some(size) = <Digest as crate::twenty_first::math::bfield_codec::BFieldCodec>::static_length() {
                                    [
                                        crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                            crate::triton_vm::instruction::AnInstruction::Push(
                                                crate::BFieldElement::new(size as u64),
                                            ),
                                        ),
                                    ]
                                        .to_vec()
                                } else {
                                    [
                                        crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                            crate::triton_vm::instruction::AnInstruction::ReadMem(
                                                crate::triton_vm::op_stack::NumberOfWords::N1,
                                            ),
                                        ),
                                        crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                            crate::triton_vm::instruction::AnInstruction::Push(
                                                crate::triton_vm::prelude::BFieldElement::new(
                                                    Self::MAX_OFFSET.into(),
                                                ),
                                            ),
                                        ),
                                        crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                            crate::triton_vm::instruction::AnInstruction::Dup(
                                                crate::triton_vm::op_stack::OpStackElement::ST2,
                                            ),
                                        ),
                                        crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                            crate::triton_vm::instruction::AnInstruction::Lt,
                                        ),
                                        crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                            crate::triton_vm::instruction::AnInstruction::Assert,
                                        ),
                                        crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                            crate::triton_vm::instruction::AnInstruction::AddI(
                                                crate::BFieldElement::new(1u64),
                                            ),
                                        ),
                                        crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                            crate::triton_vm::instruction::AnInstruction::Swap(
                                                crate::triton_vm::op_stack::OpStackElement::ST1,
                                            ),
                                        ),
                                        crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                            crate::triton_vm::instruction::AnInstruction::AddI(
                                                crate::BFieldElement::new(1u64),
                                            ),
                                        ),
                                    ]
                                        .to_vec()
                                }
                            },
                        ]
                            .concat()
                };
                let getter = {
                    if <Digest as crate::twenty_first::math::bfield_codec::BFieldCodec>::static_length()
                            .is_some()
                        {
                            [
                                crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                    crate::triton_vm::instruction::AnInstruction::Pop(
                                        crate::triton_vm::op_stack::NumberOfWords::N1,
                                    ),
                                ),
                            ]
                                .to_vec()
                        } else {
                            [
                                crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                    crate::triton_vm::instruction::AnInstruction::Pop(
                                        crate::triton_vm::op_stack::NumberOfWords::N1,
                                    ),
                                ),
                                crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                    crate::triton_vm::instruction::AnInstruction::Push(
                                        crate::BFieldElement::new(1u64),
                                    ),
                                ),
                                crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                    crate::triton_vm::instruction::AnInstruction::Add,
                                ),
                            ]
                                .to_vec()
                        }
                };
                [current, getter].concat()
            }
            unknown_field_name => {
                panic!("Cannot match on field name `{0}`.", unknown_field_name,);
            }
        };
        let hint_appendix = [
            crate::triton_vm::instruction::LabelledInstruction::TypeHint(
                crate::triton_vm::instruction::TypeHint {
                    starting_index: 0,
                    length: 1,
                    type_name: std::option::Option::<std::string::String>::None,
                    variable_name: std::string::String::from(field_name),
                },
            ),
        ]
        .to_vec();
        [field_getter, hint_appendix].concat()
    }
    fn get_field_with_size(
        field_name: &str,
    ) -> Vec<crate::triton_vm::instruction::LabelledInstruction> {
        let field_getter = match field_name {
            "output" => {
                let current = {
                    if let Some(size) = <Vec<
                            BFieldElement,
                        > as crate::twenty_first::math::bfield_codec::BFieldCodec>::static_length() {
                            [
                                crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                    crate::triton_vm::instruction::AnInstruction::Push(
                                        crate::BFieldElement::new(size as u64),
                                    ),
                                ),
                            ]
                                .to_vec()
                        } else {
                            [
                                crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                    crate::triton_vm::instruction::AnInstruction::ReadMem(
                                        crate::triton_vm::op_stack::NumberOfWords::N1,
                                    ),
                                ),
                                crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                    crate::triton_vm::instruction::AnInstruction::Push(
                                        crate::triton_vm::prelude::BFieldElement::new(
                                            Self::MAX_OFFSET.into(),
                                        ),
                                    ),
                                ),
                                crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                    crate::triton_vm::instruction::AnInstruction::Dup(
                                        crate::triton_vm::op_stack::OpStackElement::ST2,
                                    ),
                                ),
                                crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                    crate::triton_vm::instruction::AnInstruction::Lt,
                                ),
                                crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                    crate::triton_vm::instruction::AnInstruction::Assert,
                                ),
                                crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                    crate::triton_vm::instruction::AnInstruction::AddI(
                                        crate::BFieldElement::new(1u64),
                                    ),
                                ),
                                crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                    crate::triton_vm::instruction::AnInstruction::Swap(
                                        crate::triton_vm::op_stack::OpStackElement::ST1,
                                    ),
                                ),
                                crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                    crate::triton_vm::instruction::AnInstruction::AddI(
                                        crate::BFieldElement::new(1u64),
                                    ),
                                ),
                            ]
                                .to_vec()
                        }
                };
                let getter_sizer = {
                    if <Vec<
                            BFieldElement,
                        > as crate::twenty_first::math::bfield_codec::BFieldCodec>::static_length()
                            .is_some()
                        {
                            ::std::vec::Vec::<
                                crate::triton_vm::instruction::LabelledInstruction,
                            >::new()
                        } else {
                            [
                                crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                    crate::triton_vm::instruction::AnInstruction::Push(
                                        -crate::BFieldElement::new(1u64),
                                    ),
                                ),
                                crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                    crate::triton_vm::instruction::AnInstruction::Add,
                                ),
                                crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                    crate::triton_vm::instruction::AnInstruction::Swap(
                                        crate::triton_vm::op_stack::OpStackElement::ST1,
                                    ),
                                ),
                                crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                    crate::triton_vm::instruction::AnInstruction::Push(
                                        crate::BFieldElement::new(1u64),
                                    ),
                                ),
                                crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                    crate::triton_vm::instruction::AnInstruction::Add,
                                ),
                                crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                    crate::triton_vm::instruction::AnInstruction::Swap(
                                        crate::triton_vm::op_stack::OpStackElement::ST1,
                                    ),
                                ),
                            ]
                                .to_vec()
                        }
                };
                [current, getter_sizer].concat()
            }
            "input" => {
                let current = {
                    [
                            Self::get_field_start_with_jump_distance("output"),
                            [
                                crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                    crate::triton_vm::instruction::AnInstruction::Add,
                                ),
                            ]
                                .to_vec(),
                            {
                                if let Some(size) = <Vec<
                                    BFieldElement,
                                > as crate::twenty_first::math::bfield_codec::BFieldCodec>::static_length() {
                                    [
                                        crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                            crate::triton_vm::instruction::AnInstruction::Push(
                                                crate::BFieldElement::new(size as u64),
                                            ),
                                        ),
                                    ]
                                        .to_vec()
                                } else {
                                    [
                                        crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                            crate::triton_vm::instruction::AnInstruction::ReadMem(
                                                crate::triton_vm::op_stack::NumberOfWords::N1,
                                            ),
                                        ),
                                        crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                            crate::triton_vm::instruction::AnInstruction::Push(
                                                crate::triton_vm::prelude::BFieldElement::new(
                                                    Self::MAX_OFFSET.into(),
                                                ),
                                            ),
                                        ),
                                        crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                            crate::triton_vm::instruction::AnInstruction::Dup(
                                                crate::triton_vm::op_stack::OpStackElement::ST2,
                                            ),
                                        ),
                                        crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                            crate::triton_vm::instruction::AnInstruction::Lt,
                                        ),
                                        crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                            crate::triton_vm::instruction::AnInstruction::Assert,
                                        ),
                                        crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                            crate::triton_vm::instruction::AnInstruction::AddI(
                                                crate::BFieldElement::new(1u64),
                                            ),
                                        ),
                                        crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                            crate::triton_vm::instruction::AnInstruction::Swap(
                                                crate::triton_vm::op_stack::OpStackElement::ST1,
                                            ),
                                        ),
                                        crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                            crate::triton_vm::instruction::AnInstruction::AddI(
                                                crate::BFieldElement::new(1u64),
                                            ),
                                        ),
                                    ]
                                        .to_vec()
                                }
                            },
                        ]
                            .concat()
                };
                let getter_sizer = {
                    if <Vec<
                            BFieldElement,
                        > as crate::twenty_first::math::bfield_codec::BFieldCodec>::static_length()
                            .is_some()
                        {
                            ::std::vec::Vec::<
                                crate::triton_vm::instruction::LabelledInstruction,
                            >::new()
                        } else {
                            [
                                crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                    crate::triton_vm::instruction::AnInstruction::Push(
                                        -crate::BFieldElement::new(1u64),
                                    ),
                                ),
                                crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                    crate::triton_vm::instruction::AnInstruction::Add,
                                ),
                                crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                    crate::triton_vm::instruction::AnInstruction::Swap(
                                        crate::triton_vm::op_stack::OpStackElement::ST1,
                                    ),
                                ),
                                crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                    crate::triton_vm::instruction::AnInstruction::Push(
                                        crate::BFieldElement::new(1u64),
                                    ),
                                ),
                                crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                    crate::triton_vm::instruction::AnInstruction::Add,
                                ),
                                crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                    crate::triton_vm::instruction::AnInstruction::Swap(
                                        crate::triton_vm::op_stack::OpStackElement::ST1,
                                    ),
                                ),
                            ]
                                .to_vec()
                        }
                };
                [current, getter_sizer].concat()
            }
            "program_digest" => {
                let current = {
                    [
                            Self::get_field_start_with_jump_distance("input"),
                            [
                                crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                    crate::triton_vm::instruction::AnInstruction::Add,
                                ),
                            ]
                                .to_vec(),
                            {
                                if let Some(size) = <Digest as crate::twenty_first::math::bfield_codec::BFieldCodec>::static_length() {
                                    [
                                        crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                            crate::triton_vm::instruction::AnInstruction::Push(
                                                crate::BFieldElement::new(size as u64),
                                            ),
                                        ),
                                    ]
                                        .to_vec()
                                } else {
                                    [
                                        crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                            crate::triton_vm::instruction::AnInstruction::ReadMem(
                                                crate::triton_vm::op_stack::NumberOfWords::N1,
                                            ),
                                        ),
                                        crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                            crate::triton_vm::instruction::AnInstruction::Push(
                                                crate::triton_vm::prelude::BFieldElement::new(
                                                    Self::MAX_OFFSET.into(),
                                                ),
                                            ),
                                        ),
                                        crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                            crate::triton_vm::instruction::AnInstruction::Dup(
                                                crate::triton_vm::op_stack::OpStackElement::ST2,
                                            ),
                                        ),
                                        crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                            crate::triton_vm::instruction::AnInstruction::Lt,
                                        ),
                                        crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                            crate::triton_vm::instruction::AnInstruction::Assert,
                                        ),
                                        crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                            crate::triton_vm::instruction::AnInstruction::AddI(
                                                crate::BFieldElement::new(1u64),
                                            ),
                                        ),
                                        crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                            crate::triton_vm::instruction::AnInstruction::Swap(
                                                crate::triton_vm::op_stack::OpStackElement::ST1,
                                            ),
                                        ),
                                        crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                            crate::triton_vm::instruction::AnInstruction::AddI(
                                                crate::BFieldElement::new(1u64),
                                            ),
                                        ),
                                    ]
                                        .to_vec()
                                }
                            },
                        ]
                            .concat()
                };
                let getter_sizer = {
                    if <Digest as crate::twenty_first::math::bfield_codec::BFieldCodec>::static_length()
                            .is_some()
                        {
                            ::std::vec::Vec::<
                                crate::triton_vm::instruction::LabelledInstruction,
                            >::new()
                        } else {
                            [
                                crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                    crate::triton_vm::instruction::AnInstruction::Push(
                                        -crate::BFieldElement::new(1u64),
                                    ),
                                ),
                                crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                    crate::triton_vm::instruction::AnInstruction::Add,
                                ),
                                crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                    crate::triton_vm::instruction::AnInstruction::Swap(
                                        crate::triton_vm::op_stack::OpStackElement::ST1,
                                    ),
                                ),
                                crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                    crate::triton_vm::instruction::AnInstruction::Push(
                                        crate::BFieldElement::new(1u64),
                                    ),
                                ),
                                crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                    crate::triton_vm::instruction::AnInstruction::Add,
                                ),
                                crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                    crate::triton_vm::instruction::AnInstruction::Swap(
                                        crate::triton_vm::op_stack::OpStackElement::ST1,
                                    ),
                                ),
                            ]
                                .to_vec()
                        }
                };
                [current, getter_sizer].concat()
            }
            unknown_field_name => {
                panic!("Cannot match on field name `{0}`.", unknown_field_name,);
            }
        };
        let hint_appendix = [
            crate::triton_vm::instruction::LabelledInstruction::TypeHint(
                crate::triton_vm::instruction::TypeHint {
                    starting_index: 0,
                    length: 1,
                    type_name: std::option::Option::<std::string::String>::Some(
                        std::string::String::from("u32"),
                    ),
                    variable_name: std::string::String::from("size"),
                },
            ),
            crate::triton_vm::instruction::LabelledInstruction::TypeHint(
                crate::triton_vm::instruction::TypeHint {
                    starting_index: 1,
                    length: 1,
                    type_name: std::option::Option::<std::string::String>::None,
                    variable_name: std::string::String::from(field_name),
                },
            ),
        ]
        .to_vec();
        [field_getter, hint_appendix].concat()
    }
    fn get_field_start_with_jump_distance(
        field_name: &str,
    ) -> Vec<crate::triton_vm::instruction::LabelledInstruction> {
        match field_name {
                "output" => {
                    if let Some(size) = <Vec<
                        BFieldElement,
                    > as crate::twenty_first::math::bfield_codec::BFieldCodec>::static_length() {
                        [
                            crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                crate::triton_vm::instruction::AnInstruction::Push(
                                    crate::BFieldElement::new(size as u64),
                                ),
                            ),
                        ]
                            .to_vec()
                    } else {
                        [
                            crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                crate::triton_vm::instruction::AnInstruction::ReadMem(
                                    crate::triton_vm::op_stack::NumberOfWords::N1,
                                ),
                            ),
                            crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                crate::triton_vm::instruction::AnInstruction::Push(
                                    crate::triton_vm::prelude::BFieldElement::new(
                                        Self::MAX_OFFSET.into(),
                                    ),
                                ),
                            ),
                            crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                crate::triton_vm::instruction::AnInstruction::Dup(
                                    crate::triton_vm::op_stack::OpStackElement::ST2,
                                ),
                            ),
                            crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                crate::triton_vm::instruction::AnInstruction::Lt,
                            ),
                            crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                crate::triton_vm::instruction::AnInstruction::Assert,
                            ),
                            crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                crate::triton_vm::instruction::AnInstruction::AddI(
                                    crate::BFieldElement::new(1u64),
                                ),
                            ),
                            crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                crate::triton_vm::instruction::AnInstruction::Swap(
                                    crate::triton_vm::op_stack::OpStackElement::ST1,
                                ),
                            ),
                            crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                crate::triton_vm::instruction::AnInstruction::AddI(
                                    crate::BFieldElement::new(1u64),
                                ),
                            ),
                        ]
                            .to_vec()
                    }
                }
                "input" => {
                    let prev = [
                        Self::get_field_start_with_jump_distance("output"),
                        [
                            crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                crate::triton_vm::instruction::AnInstruction::Add,
                            ),
                        ]
                            .to_vec(),
                    ]
                        .concat();
                    let jumper = {
                        if let Some(size) = <Vec<
                            BFieldElement,
                        > as crate::twenty_first::math::bfield_codec::BFieldCodec>::static_length() {
                            [
                                crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                    crate::triton_vm::instruction::AnInstruction::Push(
                                        crate::BFieldElement::new(size as u64),
                                    ),
                                ),
                            ]
                                .to_vec()
                        } else {
                            [
                                crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                    crate::triton_vm::instruction::AnInstruction::ReadMem(
                                        crate::triton_vm::op_stack::NumberOfWords::N1,
                                    ),
                                ),
                                crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                    crate::triton_vm::instruction::AnInstruction::Push(
                                        crate::triton_vm::prelude::BFieldElement::new(
                                            Self::MAX_OFFSET.into(),
                                        ),
                                    ),
                                ),
                                crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                    crate::triton_vm::instruction::AnInstruction::Dup(
                                        crate::triton_vm::op_stack::OpStackElement::ST2,
                                    ),
                                ),
                                crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                    crate::triton_vm::instruction::AnInstruction::Lt,
                                ),
                                crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                    crate::triton_vm::instruction::AnInstruction::Assert,
                                ),
                                crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                    crate::triton_vm::instruction::AnInstruction::AddI(
                                        crate::BFieldElement::new(1u64),
                                    ),
                                ),
                                crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                    crate::triton_vm::instruction::AnInstruction::Swap(
                                        crate::triton_vm::op_stack::OpStackElement::ST1,
                                    ),
                                ),
                                crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                    crate::triton_vm::instruction::AnInstruction::AddI(
                                        crate::BFieldElement::new(1u64),
                                    ),
                                ),
                            ]
                                .to_vec()
                        }
                    };
                    [prev, jumper].concat()
                }
                "program_digest" => {
                    let prev = [
                        Self::get_field_start_with_jump_distance("input"),
                        [
                            crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                crate::triton_vm::instruction::AnInstruction::Add,
                            ),
                        ]
                            .to_vec(),
                    ]
                        .concat();
                    let jumper = {
                        if let Some(size) = <Digest as crate::twenty_first::math::bfield_codec::BFieldCodec>::static_length() {
                            [
                                crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                    crate::triton_vm::instruction::AnInstruction::Push(
                                        crate::BFieldElement::new(size as u64),
                                    ),
                                ),
                            ]
                                .to_vec()
                        } else {
                            [
                                crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                    crate::triton_vm::instruction::AnInstruction::ReadMem(
                                        crate::triton_vm::op_stack::NumberOfWords::N1,
                                    ),
                                ),
                                crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                    crate::triton_vm::instruction::AnInstruction::Push(
                                        crate::triton_vm::prelude::BFieldElement::new(
                                            Self::MAX_OFFSET.into(),
                                        ),
                                    ),
                                ),
                                crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                    crate::triton_vm::instruction::AnInstruction::Dup(
                                        crate::triton_vm::op_stack::OpStackElement::ST2,
                                    ),
                                ),
                                crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                    crate::triton_vm::instruction::AnInstruction::Lt,
                                ),
                                crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                    crate::triton_vm::instruction::AnInstruction::Assert,
                                ),
                                crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                    crate::triton_vm::instruction::AnInstruction::AddI(
                                        crate::BFieldElement::new(1u64),
                                    ),
                                ),
                                crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                    crate::triton_vm::instruction::AnInstruction::Swap(
                                        crate::triton_vm::op_stack::OpStackElement::ST1,
                                    ),
                                ),
                                crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                    crate::triton_vm::instruction::AnInstruction::AddI(
                                        crate::BFieldElement::new(1u64),
                                    ),
                                ),
                            ]
                                .to_vec()
                        }
                    };
                    [prev, jumper].concat()
                }
                unknown_field_name => {
                    panic!(
                            "Cannot match on field name `{0}`.",
                            unknown_field_name,
                    );
                }
            }
    }
    fn get_encoding_length() -> Vec<crate::triton_vm::instruction::LabelledInstruction> {
        let first_field_with_jump_distance = {
            [
                    Self::get_field_start_with_jump_distance("input"),
                    [
                        crate::triton_vm::instruction::LabelledInstruction::Instruction(
                            crate::triton_vm::instruction::AnInstruction::Add,
                        ),
                    ]
                        .to_vec(),
                    {
                        if let Some(size) = <Digest as crate::twenty_first::math::bfield_codec::BFieldCodec>::static_length() {
                            [
                                crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                    crate::triton_vm::instruction::AnInstruction::Push(
                                        crate::BFieldElement::new(size as u64),
                                    ),
                                ),
                            ]
                                .to_vec()
                        } else {
                            [
                                crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                    crate::triton_vm::instruction::AnInstruction::ReadMem(
                                        crate::triton_vm::op_stack::NumberOfWords::N1,
                                    ),
                                ),
                                crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                    crate::triton_vm::instruction::AnInstruction::Push(
                                        crate::triton_vm::prelude::BFieldElement::new(
                                            Self::MAX_OFFSET.into(),
                                        ),
                                    ),
                                ),
                                crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                    crate::triton_vm::instruction::AnInstruction::Dup(
                                        crate::triton_vm::op_stack::OpStackElement::ST2,
                                    ),
                                ),
                                crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                    crate::triton_vm::instruction::AnInstruction::Lt,
                                ),
                                crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                    crate::triton_vm::instruction::AnInstruction::Assert,
                                ),
                                crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                    crate::triton_vm::instruction::AnInstruction::AddI(
                                        crate::BFieldElement::new(1u64),
                                    ),
                                ),
                                crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                    crate::triton_vm::instruction::AnInstruction::Swap(
                                        crate::triton_vm::op_stack::OpStackElement::ST1,
                                    ),
                                ),
                                crate::triton_vm::instruction::LabelledInstruction::Instruction(
                                    crate::triton_vm::instruction::AnInstruction::AddI(
                                        crate::BFieldElement::new(1u64),
                                    ),
                                ),
                            ]
                                .to_vec()
                        }
                    },
                ]
                    .concat()
        };
        let add = crate::triton_vm::instruction::LabelledInstruction::Instruction(
            crate::triton_vm::instruction::AnInstruction::Add,
        );
        let dup0 = crate::triton_vm::instruction::LabelledInstruction::Instruction(
            crate::triton_vm::instruction::AnInstruction::Dup(
                crate::triton_vm::op_stack::OpStackElement::ST0,
            ),
        );
        let swap1 = crate::triton_vm::instruction::LabelledInstruction::Instruction(
            crate::triton_vm::instruction::AnInstruction::Swap(
                crate::triton_vm::op_stack::OpStackElement::ST1,
            ),
        );
        let push_neg_1 = crate::triton_vm::instruction::LabelledInstruction::Instruction(
            crate::triton_vm::instruction::AnInstruction::Push(
                crate::triton_vm::prelude::BFieldElement::new(
                    crate::triton_vm::prelude::BFieldElement::P - 1,
                ),
            ),
        );
        let mul = crate::triton_vm::instruction::LabelledInstruction::Instruction(
            crate::triton_vm::instruction::AnInstruction::Mul,
        );
        let extract_encoding_size = [add.clone(), swap1, push_neg_1, mul, add].to_vec();
        [
            [dup0].to_vec(),
            first_field_with_jump_distance,
            extract_encoding_size,
        ]
        .concat()
    }
    fn compute_size_and_assert_valid_size_indicator(
        library: &mut crate::tasm_lib::Library,
    ) -> Vec<crate::triton_vm::instruction::LabelledInstruction> {
        let push0 = crate::triton_vm::instruction::LabelledInstruction::Instruction(
            crate::triton_vm::instruction::AnInstruction::Push(0u64.into()),
        );
        let pushmax = crate::triton_vm::instruction::LabelledInstruction::Instruction(
            crate::triton_vm::instruction::AnInstruction::Push(Self::MAX_OFFSET.into()),
        );
        let dup0 = crate::triton_vm::instruction::LabelledInstruction::Instruction(
            crate::triton_vm::instruction::AnInstruction::Dup(
                crate::triton_vm::op_stack::OpStackElement::ST0,
            ),
        );
        let dup1 = crate::triton_vm::instruction::LabelledInstruction::Instruction(
            crate::triton_vm::instruction::AnInstruction::Dup(
                crate::triton_vm::op_stack::OpStackElement::ST1,
            ),
        );
        let dup2 = crate::triton_vm::instruction::LabelledInstruction::Instruction(
            crate::triton_vm::instruction::AnInstruction::Dup(
                crate::triton_vm::op_stack::OpStackElement::ST2,
            ),
        );
        let swap1 = crate::triton_vm::instruction::LabelledInstruction::Instruction(
            crate::triton_vm::instruction::AnInstruction::Swap(
                crate::triton_vm::op_stack::OpStackElement::ST1,
            ),
        );
        let swap2 = crate::triton_vm::instruction::LabelledInstruction::Instruction(
            crate::triton_vm::instruction::AnInstruction::Swap(
                crate::triton_vm::op_stack::OpStackElement::ST2,
            ),
        );
        let lt = crate::triton_vm::instruction::LabelledInstruction::Instruction(
            crate::triton_vm::instruction::AnInstruction::Lt,
        );
        let assert = crate::triton_vm::instruction::LabelledInstruction::Instruction(
            crate::triton_vm::instruction::AnInstruction::Assert,
        );
        let eq = crate::triton_vm::instruction::LabelledInstruction::Instruction(
            crate::triton_vm::instruction::AnInstruction::Eq,
        );
        let add = crate::triton_vm::instruction::LabelledInstruction::Instruction(
            crate::triton_vm::instruction::AnInstruction::Add,
        );
        let read_mem1 = crate::triton_vm::instruction::LabelledInstruction::Instruction(
            crate::triton_vm::instruction::AnInstruction::ReadMem(
                crate::triton_vm::op_stack::NumberOfWords::N1,
            ),
        );
        let addi1 = crate::triton_vm::instruction::LabelledInstruction::Instruction(
            crate::triton_vm::instruction::AnInstruction::AddI(crate::BFieldElement::new(1u64)),
        );
        let addi2 = crate::triton_vm::instruction::LabelledInstruction::Instruction(
            crate::triton_vm::instruction::AnInstruction::AddI(crate::BFieldElement::new(2u64)),
        );
        let pop1 = crate::triton_vm::instruction::LabelledInstruction::Instruction(
            crate::triton_vm::instruction::AnInstruction::Pop(
                crate::triton_vm::op_stack::NumberOfWords::N1,
            ),
        );
        let hint_acc_size = [
            crate::triton_vm::instruction::LabelledInstruction::TypeHint(
                crate::triton_vm::instruction::TypeHint {
                    starting_index: 1,
                    length: 1,
                    type_name: std::option::Option::<std::string::String>::None,
                    variable_name: std::string::String::from("acc_size"),
                },
            ),
        ]
        .to_vec();
        let hint_field_ptr = [
            crate::triton_vm::instruction::LabelledInstruction::TypeHint(
                crate::triton_vm::instruction::TypeHint {
                    starting_index: 0,
                    length: 1,
                    type_name: std::option::Option::<std::string::String>::None,
                    variable_name: std::string::String::from("field_ptr"),
                },
            ),
        ]
        .to_vec();
        [
                [push0.clone(), swap1.clone()].to_vec(),
                hint_acc_size,
                hint_field_ptr,
                if let Some(static_length) = <Vec<
                    BFieldElement,
                > as crate::triton_vm::twenty_first::math::bfield_codec::BFieldCodec>::static_length() {
                    let addi_len = crate::triton_vm::instruction::LabelledInstruction::Instruction(
                        crate::triton_vm::instruction::AnInstruction::AddI(
                            crate::BFieldElement::new(static_length as u64),
                        ),
                    );
                    [addi_len.clone(), swap1.clone(), addi_len.clone(), swap1.clone()]
                        .to_vec()
                } else {
                    [
                        [
                            read_mem1.clone(),
                            pushmax.clone(),
                            dup2.clone(),
                            lt.clone(),
                            assert.clone(),
                            addi2.clone(),
                            dup0.clone(),
                        ]
                            .to_vec(),
                        <Vec<
                            BFieldElement,
                        > as crate::tasm_lib::structure::tasm_object::TasmObject>::compute_size_and_assert_valid_size_indicator(
                            library,
                        ),
                        [
                            dup2.clone(),
                            eq.clone(),
                            assert.clone(),
                            dup1.clone(),
                            add.clone(),
                            swap2.clone(),
                            add.clone(),
                            addi1.clone(),
                            swap1.clone(),
                        ]
                            .to_vec(),
                    ]
                        .concat()
                },
                if let Some(static_length) = <Vec<
                    BFieldElement,
                > as crate::triton_vm::twenty_first::math::bfield_codec::BFieldCodec>::static_length() {
                    let addi_len = crate::triton_vm::instruction::LabelledInstruction::Instruction(
                        crate::triton_vm::instruction::AnInstruction::AddI(
                            crate::BFieldElement::new(static_length as u64),
                        ),
                    );
                    [addi_len.clone(), swap1.clone(), addi_len.clone(), swap1.clone()]
                        .to_vec()
                } else {
                    [
                        [
                            read_mem1.clone(),
                            pushmax.clone(),
                            dup2.clone(),
                            lt.clone(),
                            assert.clone(),
                            addi2.clone(),
                            dup0.clone(),
                        ]
                            .to_vec(),
                        <Vec<
                            BFieldElement,
                        > as crate::tasm_lib::structure::tasm_object::TasmObject>::compute_size_and_assert_valid_size_indicator(
                            library,
                        ),
                        [
                            dup2.clone(),
                            eq.clone(),
                            assert.clone(),
                            dup1.clone(),
                            add.clone(),
                            swap2.clone(),
                            add.clone(),
                            addi1.clone(),
                            swap1.clone(),
                        ]
                            .to_vec(),
                    ]
                        .concat()
                },
                if let Some(static_length) = <Digest as crate::triton_vm::twenty_first::math::bfield_codec::BFieldCodec>::static_length() {
                    let addi_len = crate::triton_vm::instruction::LabelledInstruction::Instruction(
                        crate::triton_vm::instruction::AnInstruction::AddI(
                            crate::BFieldElement::new(static_length as u64),
                        ),
                    );
                    [addi_len.clone(), swap1.clone(), addi_len.clone(), swap1.clone()]
                        .to_vec()
                } else {
                    [
                        [
                            read_mem1.clone(),
                            pushmax.clone(),
                            dup2.clone(),
                            lt.clone(),
                            assert.clone(),
                            addi2.clone(),
                            dup0.clone(),
                        ]
                            .to_vec(),
                        <Digest as crate::tasm_lib::structure::tasm_object::TasmObject>::compute_size_and_assert_valid_size_indicator(
                            library,
                        ),
                        [
                            dup2.clone(),
                            eq.clone(),
                            assert.clone(),
                            dup1.clone(),
                            add.clone(),
                            swap2.clone(),
                            add.clone(),
                            addi1.clone(),
                            swap1.clone(),
                        ]
                            .to_vec(),
                    ]
                        .concat()
                },
                [pop1.clone()].to_vec(),
            ]
                .concat()
    }
    fn decode_iter<Itr: Iterator<Item = crate::BFieldElement>>(
        iterator: &mut Itr,
    ) -> ::std::result::Result<
        ::std::boxed::Box<Self>,
        ::std::boxed::Box<dyn ::std::error::Error + ::core::marker::Send + ::core::marker::Sync>,
    > {
        let length: usize = if let Some(static_length) = <Vec<
                BFieldElement,
            > as crate::twenty_first::math::bfield_codec::BFieldCodec>::static_length() {
                static_length
            } else {
                iterator.next().unwrap().value() as usize
            };
        let sequence = (0..length)
            .map(|_| iterator.next().unwrap())
            .collect::<Vec<_>>();
        let output: Vec<BFieldElement> =
            *crate::twenty_first::math::bfield_codec::BFieldCodec::decode(&sequence)?;
        let length: usize = if let Some(static_length) = <Vec<
                BFieldElement,
            > as crate::twenty_first::math::bfield_codec::BFieldCodec>::static_length() {
                static_length
            } else {
                iterator.next().unwrap().value() as usize
            };
        let sequence = (0..length)
            .map(|_| iterator.next().unwrap())
            .collect::<Vec<_>>();
        let input: Vec<BFieldElement> =
            *crate::twenty_first::math::bfield_codec::BFieldCodec::decode(&sequence)?;
        let length: usize = if let Some(static_length) =
            <Digest as crate::twenty_first::math::bfield_codec::BFieldCodec>::static_length()
        {
            static_length
        } else {
            iterator.next().unwrap().value() as usize
        };
        let sequence = (0..length)
            .map(|_| iterator.next().unwrap())
            .collect::<Vec<_>>();
        let program_digest: Digest =
            *crate::twenty_first::math::bfield_codec::BFieldCodec::decode(&sequence)?;
        ::std::result::Result::Ok(::std::boxed::Box::new(Self {
            output,
            input,
            program_digest,
        }))
    }
}
