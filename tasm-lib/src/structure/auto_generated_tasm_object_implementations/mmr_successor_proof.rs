use triton_vm::prelude::*;
use twenty_first::util_types::mmr::mmr_successor_proof::MmrSuccessorProof;

impl crate::tasm_lib::structure::tasm_object::TasmObject for MmrSuccessorProof {
    fn label_friendly_name() -> String {
        "MmrSuccessorProof".to_owned()
    }
    fn get_field(field_name: &str) -> Vec<crate::triton_vm::isa::instruction::LabelledInstruction> {
        let field_getter = match field_name {
            "paths" => {
                let current = {
                    if let Some(size) = <Vec<
                        Digest,
                    > as crate::twenty_first::math::bfield_codec::BFieldCodec>::static_length() {
                        [
                            crate::triton_vm::isa::instruction::LabelledInstruction::Instruction(
                                crate::triton_vm::isa::instruction::AnInstruction::Push(
                                    crate::BFieldElement::new(size as u64),
                                ),
                            ),
                        ]
                            .to_vec()
                    } else {
                        [
                            crate::triton_vm::isa::instruction::LabelledInstruction::Instruction(
                                crate::triton_vm::isa::instruction::AnInstruction::ReadMem(
                        crate::triton_vm::isa::op_stack::NumberOfWords::N1,
                        ),
                        ),
                        crate::triton_vm::isa::instruction::LabelledInstruction::Instruction(
                            crate::triton_vm::isa::instruction::AnInstruction::Push(
                                crate::triton_vm::prelude::BFieldElement::new(
                                    Self::MAX_OFFSET.into(),
                                ),
                            ),
                        ),
                        crate::triton_vm::isa::instruction::LabelledInstruction::Instruction(
                            crate::triton_vm::isa::instruction::AnInstruction::Dup(
                        crate::triton_vm::isa::op_stack::OpStackElement::ST2,
                        ),
                        ),
                        crate::triton_vm::isa::instruction::LabelledInstruction::Instruction(
                            crate::triton_vm::isa::instruction::AnInstruction::Lt,
                        ),
                        crate::triton_vm::isa::instruction::LabelledInstruction::Instruction(
                            crate::triton_vm::isa::instruction::AnInstruction::Assert,
                        ),
                        crate::triton_vm::isa::instruction::LabelledInstruction::Instruction(
                            crate::triton_vm::isa::instruction::AnInstruction::AddI(
                                crate::BFieldElement::new(1u64),
                            ),
                        ),
                        crate::triton_vm::isa::instruction::LabelledInstruction::Instruction(
                            crate::triton_vm::isa::instruction::AnInstruction::Swap(
                        crate::triton_vm::isa::op_stack::OpStackElement::ST1,
                        ),
                        ),
                        crate::triton_vm::isa::instruction::LabelledInstruction::Instruction(
                            crate::triton_vm::isa::instruction::AnInstruction::AddI(
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
                            crate::triton_vm::isa::instruction::LabelledInstruction::Instruction(
                                crate::triton_vm::isa::instruction::AnInstruction::Pop(
                        crate::triton_vm::isa::op_stack::NumberOfWords::N1,
                        ),
                        ),
                        ]
                        .to_vec()
                    } else {
                        [
                            crate::triton_vm::isa::instruction::LabelledInstruction::Instruction(
                                crate::triton_vm::isa::instruction::AnInstruction::Pop(
                        crate::triton_vm::isa::op_stack::NumberOfWords::N1,
                        ),
                        ),
                        crate::triton_vm::isa::instruction::LabelledInstruction::Instruction(
                            crate::triton_vm::isa::instruction::AnInstruction::Push(
                                crate::BFieldElement::new(1u64),
                            ),
                        ),
                        crate::triton_vm::isa::instruction::LabelledInstruction::Instruction(
                            crate::triton_vm::isa::instruction::AnInstruction::Add,
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
            crate::triton_vm::isa::instruction::LabelledInstruction::TypeHint(
                crate::triton_vm::isa::instruction::TypeHint {
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
    ) -> Vec<crate::triton_vm::isa::instruction::LabelledInstruction> {
        let field_getter = match field_name {
            "paths" => {
                let current = {
                    if let Some(size) = <Vec<
                        Digest,
                    > as crate::twenty_first::math::bfield_codec::BFieldCodec>::static_length() {
                        [
                            crate::triton_vm::isa::instruction::LabelledInstruction::Instruction(
                                crate::triton_vm::isa::instruction::AnInstruction::Push(
                                    crate::BFieldElement::new(size as u64),
                                ),
                            ),
                        ]
                            .to_vec()
                    } else {
                        [
                            crate::triton_vm::isa::instruction::LabelledInstruction::Instruction(
                                crate::triton_vm::isa::instruction::AnInstruction::ReadMem(
                        crate::triton_vm::isa::op_stack::NumberOfWords::N1,
                        ),
                        ),
                        crate::triton_vm::isa::instruction::LabelledInstruction::Instruction(
                            crate::triton_vm::isa::instruction::AnInstruction::Push(
                                crate::triton_vm::prelude::BFieldElement::new(
                                    Self::MAX_OFFSET.into(),
                                ),
                            ),
                        ),
                        crate::triton_vm::isa::instruction::LabelledInstruction::Instruction(
                            crate::triton_vm::isa::instruction::AnInstruction::Dup(
                        crate::triton_vm::isa::op_stack::OpStackElement::ST2,
                        ),
                        ),
                        crate::triton_vm::isa::instruction::LabelledInstruction::Instruction(
                            crate::triton_vm::isa::instruction::AnInstruction::Lt,
                        ),
                        crate::triton_vm::isa::instruction::LabelledInstruction::Instruction(
                            crate::triton_vm::isa::instruction::AnInstruction::Assert,
                        ),
                        crate::triton_vm::isa::instruction::LabelledInstruction::Instruction(
                            crate::triton_vm::isa::instruction::AnInstruction::AddI(
                                crate::BFieldElement::new(1u64),
                            ),
                        ),
                        crate::triton_vm::isa::instruction::LabelledInstruction::Instruction(
                            crate::triton_vm::isa::instruction::AnInstruction::Swap(
                        crate::triton_vm::isa::op_stack::OpStackElement::ST1,
                        ),
                        ),
                        crate::triton_vm::isa::instruction::LabelledInstruction::Instruction(
                            crate::triton_vm::isa::instruction::AnInstruction::AddI(
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
                            crate::triton_vm::isa::instruction::LabelledInstruction,
                        >::new()
                    } else {
                        [
                            crate::triton_vm::isa::instruction::LabelledInstruction::Instruction(
                                crate::triton_vm::isa::instruction::AnInstruction::Push(
                                    -crate::BFieldElement::new(1u64),
                                ),
                            ),
                            crate::triton_vm::isa::instruction::LabelledInstruction::Instruction(
                                crate::triton_vm::isa::instruction::AnInstruction::Add,
                            ),
                            crate::triton_vm::isa::instruction::LabelledInstruction::Instruction(
                                crate::triton_vm::isa::instruction::AnInstruction::Swap(
                        crate::triton_vm::isa::op_stack::OpStackElement::ST1,
                        ),
                        ),
                        crate::triton_vm::isa::instruction::LabelledInstruction::Instruction(
                            crate::triton_vm::isa::instruction::AnInstruction::Push(
                                crate::BFieldElement::new(1u64),
                            ),
                        ),
                        crate::triton_vm::isa::instruction::LabelledInstruction::Instruction(
                            crate::triton_vm::isa::instruction::AnInstruction::Add,
                        ),
                        crate::triton_vm::isa::instruction::LabelledInstruction::Instruction(
                            crate::triton_vm::isa::instruction::AnInstruction::Swap(
                        crate::triton_vm::isa::op_stack::OpStackElement::ST1,
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
            crate::triton_vm::isa::instruction::LabelledInstruction::TypeHint(
                crate::triton_vm::isa::instruction::TypeHint {
                    starting_index: 0,
                    length: 1,
                    type_name: std::option::Option::<std::string::String>::Some(
                        std::string::String::from("u32"),
                    ),
                    variable_name: std::string::String::from("size"),
                },
            ),
            crate::triton_vm::isa::instruction::LabelledInstruction::TypeHint(
                crate::triton_vm::isa::instruction::TypeHint {
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
    ) -> Vec<crate::triton_vm::isa::instruction::LabelledInstruction> {
        match field_name {
            "paths" => {
                if let Some(size) = <Vec<
                    Digest,
                > as crate::twenty_first::math::bfield_codec::BFieldCodec>::static_length() {
                    [
                        crate::triton_vm::isa::instruction::LabelledInstruction::Instruction(
                            crate::triton_vm::isa::instruction::AnInstruction::Push(
                                crate::BFieldElement::new(size as u64),
                            ),
                        ),
                    ]
                        .to_vec()
                } else {
                    [
                        crate::triton_vm::isa::instruction::LabelledInstruction::Instruction(
                            crate::triton_vm::isa::instruction::AnInstruction::ReadMem(
                    crate::triton_vm::isa::op_stack::NumberOfWords::N1,
                    ),
                    ),
                    crate::triton_vm::isa::instruction::LabelledInstruction::Instruction(
                        crate::triton_vm::isa::instruction::AnInstruction::Push(
                            crate::triton_vm::prelude::BFieldElement::new(
                                Self::MAX_OFFSET.into(),
                            ),
                        ),
                    ),
                    crate::triton_vm::isa::instruction::LabelledInstruction::Instruction(
                        crate::triton_vm::isa::instruction::AnInstruction::Dup(
                    crate::triton_vm::isa::op_stack::OpStackElement::ST2,
                    ),
                    ),
                    crate::triton_vm::isa::instruction::LabelledInstruction::Instruction(
                        crate::triton_vm::isa::instruction::AnInstruction::Lt,
                    ),
                    crate::triton_vm::isa::instruction::LabelledInstruction::Instruction(
                        crate::triton_vm::isa::instruction::AnInstruction::Assert,
                    ),
                    crate::triton_vm::isa::instruction::LabelledInstruction::Instruction(
                        crate::triton_vm::isa::instruction::AnInstruction::AddI(
                            crate::BFieldElement::new(1u64),
                        ),
                    ),
                    crate::triton_vm::isa::instruction::LabelledInstruction::Instruction(
                        crate::triton_vm::isa::instruction::AnInstruction::Swap(
                    crate::triton_vm::isa::op_stack::OpStackElement::ST1,
                    ),
                    ),
                    crate::triton_vm::isa::instruction::LabelledInstruction::Instruction(
                        crate::triton_vm::isa::instruction::AnInstruction::AddI(
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
    fn compute_size_and_assert_valid_size_indicator(
        library: &mut crate::tasm_lib::Library,
    ) -> Vec<crate::triton_vm::isa::instruction::LabelledInstruction> {
        let push0 = crate::triton_vm::isa::instruction::LabelledInstruction::Instruction(
            crate::triton_vm::isa::instruction::AnInstruction::Push(0u64.into()),
        );
        let pushmax = crate::triton_vm::isa::instruction::LabelledInstruction::Instruction(
            crate::triton_vm::isa::instruction::AnInstruction::Push(Self::MAX_OFFSET.into()),
        );
        let dup0 = crate::triton_vm::isa::instruction::LabelledInstruction::Instruction(
            crate::triton_vm::isa::instruction::AnInstruction::Dup(
                crate::triton_vm::isa::op_stack::OpStackElement::ST0,
            ),
        );
        let dup1 = crate::triton_vm::isa::instruction::LabelledInstruction::Instruction(
            crate::triton_vm::isa::instruction::AnInstruction::Dup(
                crate::triton_vm::isa::op_stack::OpStackElement::ST1,
            ),
        );
        let dup2 = crate::triton_vm::isa::instruction::LabelledInstruction::Instruction(
            crate::triton_vm::isa::instruction::AnInstruction::Dup(
                crate::triton_vm::isa::op_stack::OpStackElement::ST2,
            ),
        );
        let swap1 = crate::triton_vm::isa::instruction::LabelledInstruction::Instruction(
            crate::triton_vm::isa::instruction::AnInstruction::Swap(
                crate::triton_vm::isa::op_stack::OpStackElement::ST1,
            ),
        );
        let swap2 = crate::triton_vm::isa::instruction::LabelledInstruction::Instruction(
            crate::triton_vm::isa::instruction::AnInstruction::Swap(
                crate::triton_vm::isa::op_stack::OpStackElement::ST2,
            ),
        );
        let lt = crate::triton_vm::isa::instruction::LabelledInstruction::Instruction(
            crate::triton_vm::isa::instruction::AnInstruction::Lt,
        );
        let assert = crate::triton_vm::isa::instruction::LabelledInstruction::Instruction(
            crate::triton_vm::isa::instruction::AnInstruction::Assert,
        );
        let eq = crate::triton_vm::isa::instruction::LabelledInstruction::Instruction(
            crate::triton_vm::isa::instruction::AnInstruction::Eq,
        );
        let add = crate::triton_vm::isa::instruction::LabelledInstruction::Instruction(
            crate::triton_vm::isa::instruction::AnInstruction::Add,
        );
        let read_mem1 = crate::triton_vm::isa::instruction::LabelledInstruction::Instruction(
            crate::triton_vm::isa::instruction::AnInstruction::ReadMem(
                crate::triton_vm::isa::op_stack::NumberOfWords::N1,
            ),
        );
        let addi1 = crate::triton_vm::isa::instruction::LabelledInstruction::Instruction(
            crate::triton_vm::isa::instruction::AnInstruction::AddI(crate::BFieldElement::new(
                1u64,
            )),
        );
        let addi2 = crate::triton_vm::isa::instruction::LabelledInstruction::Instruction(
            crate::triton_vm::isa::instruction::AnInstruction::AddI(crate::BFieldElement::new(
                2u64,
            )),
        );
        let pop1 = crate::triton_vm::isa::instruction::LabelledInstruction::Instruction(
            crate::triton_vm::isa::instruction::AnInstruction::Pop(
                crate::triton_vm::isa::op_stack::NumberOfWords::N1,
            ),
        );
        let hint_acc_size = [
            crate::triton_vm::isa::instruction::LabelledInstruction::TypeHint(
                crate::triton_vm::isa::instruction::TypeHint {
                    starting_index: 1,
                    length: 1,
                    type_name: std::option::Option::<std::string::String>::None,
                    variable_name: std::string::String::from("acc_size"),
                },
            ),
        ]
        .to_vec();
        let hint_field_ptr = [
            crate::triton_vm::isa::instruction::LabelledInstruction::TypeHint(
                crate::triton_vm::isa::instruction::TypeHint {
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
                let addi_len = crate::triton_vm::isa::instruction::LabelledInstruction::Instruction(
                    crate::triton_vm::isa::instruction::AnInstruction::AddI(
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
        let paths: Vec<Digest> =
            *crate::twenty_first::math::bfield_codec::BFieldCodec::decode(&sequence)?;
        ::std::result::Result::Ok(::std::boxed::Box::new(Self { paths }))
    }
}
