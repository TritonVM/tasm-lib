//! This crate provides a derive macro for the `BFieldCodec` trait.

extern crate proc_macro;

use proc_macro::TokenStream;
use quote::quote;

/// Derives `TasmObject` for structs.
///
/// ### Known limitations
/// ```
#[proc_macro_derive(TasmObject, attributes(tasm_object))]
pub fn derive_tasm_object(input: TokenStream) -> TokenStream {
    // ...
    // Construct a representation of Rust code as a syntax tree
    // that we can manipulate
    let ast = syn::parse(input).unwrap();

    // Build the trait implementation
    impl_derive_tasm_object_macro(ast)
}

struct ParseResult {
    field_names: Vec<syn::Ident>,
    field_types: Vec<syn::Type>,
    getters: Vec<quote::__private::TokenStream>,
    sizers: Vec<quote::__private::TokenStream>,
    jumpers: Vec<quote::__private::TokenStream>,
}

fn impl_derive_tasm_object_macro(ast: syn::DeriveInput) -> TokenStream {
    let parse_result = match &ast.data {
        syn::Data::Struct(syn::DataStruct {
            fields: syn::Fields::Named(fields),
            ..
        }) => generate_tokens_for_struct_with_named_fields(fields),
        syn::Data::Struct(syn::DataStruct {
            fields: syn::Fields::Unnamed(fields),
            ..
        }) => generate_tokens_for_struct_with_unnamed_fields(fields),
        _ => panic!("expected a struct with named fields, or with unnamed fields"),
    };

    let name = &ast.ident;

    // Extract the generics of the struct/enum.
    let (impl_generics, ty_generics, where_clause) = ast.generics.split_for_impl();

    // generate clauses for match statements
    let get_current_field_start_with_jump = (0..parse_result.field_names.len()).map(|index| {
        let jumper = &parse_result.jumpers[index];
        match index {
            0 => jumper.to_owned(),
            not_zero => {
                let previous_field_name_as_string = &parse_result.field_names[not_zero - 1].to_string();
                quote! {
                    [
                        Self::get_field_start_with_jump_distance(#previous_field_name_as_string),
                            // _ *prev_field_start prev_jump_amount
                        [triton_vm::instruction::LabelledInstruction::Instruction(triton_vm::instruction::AnInstruction::Add)].to_vec(),
                            // _ *current_field_start
                        { #jumper },
                            // _ *current_field_start current_field_jump_amount
                    ].concat()
                }
            }
        }
    });

    let just_field_clauses = parse_result
        .field_names
        .iter()
        .zip(parse_result.getters.iter())
        .zip(get_current_field_start_with_jump.clone())
        .map(|((name, getter), current)| {
            let name_as_string = name.to_string();
            quote! {
                #name_as_string => {
                    let current = { #current }; // _ *current_field_start current_field_jump_amount
                    let getter = { #getter };   // _ *current_field
                    [current, getter].concat()
                }
            }
        });
    let field_with_size_clauses = parse_result
        .field_names
        .iter()
        .zip(parse_result.sizers.iter())
        .zip(get_current_field_start_with_jump.clone())
        .map(|((name, getter_sizer), current)| {
            let name_as_string = name.to_string();
            quote! {
                #name_as_string => {
                    let current = { #current };             // _ *current_field_start current_field_jump_amount
                    let getter_sizer = { #getter_sizer };   // _ *current_field current_field_size
                    [current,  getter_sizer].concat()
                }
            }
        });
    let field_starter_clauses = parse_result.field_names
        .iter()
        .zip(parse_result.jumpers.iter())
        .enumerate()
        .map(|(index,(name, jumper))| {
            let name_as_string = name.to_string();
            match index {
                0 => quote!{
                    #name_as_string => { #jumper }
                },
                not_zero => {
                    let previous_field_name_as_string = parse_result.field_names[not_zero-1].to_string();
                    quote! {
                        #name_as_string => {
                            let prev =
                            [
                                Self::get_field_start_with_jump_distance(#previous_field_name_as_string),
                                    // _ *prev_field_start prev_field_size
                                [triton_vm::instruction::LabelledInstruction::Instruction(triton_vm::instruction::AnInstruction::Add)].to_vec(),
                                    // _ *current_field_start
                            ].concat();
                            let jumper = { #jumper }; // _ *current_field current_field_jump_amount
                            [prev,  jumper].concat()
                        }
                    }
                }
            }
        });

    let field_decoders = parse_result
        .field_names
        .iter()
        .cloned()
        .zip(parse_result.field_types.iter().cloned())
        .map(|(fnm, ftp)| get_field_decoder(fnm, ftp));

    let field_names = parse_result.field_names.clone();

    let self_builder = match &ast.data {
        syn::Data::Struct(syn::DataStruct {
            fields: syn::Fields::Named(_),
            ..
        }) => quote! { Self { #( #field_names ,)* } },
        syn::Data::Struct(syn::DataStruct {
            fields: syn::Fields::Unnamed(_),
            ..
        }) => quote! { Self(#( #field_names ,)*) },
        _ => unreachable!("expected a struct with named fields, or with unnamed fields"),
    };

    let gen = quote! {
        impl #impl_generics ::tasm_lib::structure::tasm_object::TasmObject
        for #name #ty_generics #where_clause {
            fn get_field( field_name : &str ) -> Vec<triton_vm::instruction::LabelledInstruction> {
                match field_name {
                    #( #just_field_clauses ,)*
                    unknown_field_name => panic!("Cannot match on field name `{unknown_field_name}`."),
                }
            }

            fn get_field_with_size( field_name : &str ) -> Vec<triton_vm::instruction::LabelledInstruction> {
                match field_name {
                    #( #field_with_size_clauses ,)*
                    unknown_field_name => panic!("Cannot match on field name `{unknown_field_name}`."),
                }
            }

            fn get_field_start_with_jump_distance( field_name : &str ) -> Vec<triton_vm::instruction::LabelledInstruction> {
                match field_name {
                    #( #field_starter_clauses ,)*
                    unknown_field_name => panic!("Cannot match on field name `{unknown_field_name}`."),
                }
            }

            fn decode_iter<Itr: Iterator<Item=triton_vm::BFieldElement>>( iterator: &mut Itr ) -> anyhow::Result<Box<Self>> {
                #( #field_decoders )*
                anyhow::Result::Ok(Box::new(#self_builder))
            }
        }
    };

    gen.into()
}

fn generate_tokens_for_struct_with_named_fields(fields: &syn::FieldsNamed) -> ParseResult {
    let named_fields = fields.named.iter();

    let field_names = named_fields
        .clone()
        .map(|field| field.ident.as_ref().unwrap().to_owned());
    let field_names_list = field_names.clone().collect::<std::vec::Vec<_>>();

    let getters = named_fields
        .clone()
        .enumerate()
        .map(|(i, _f)| {
            generate_tasm_for_getter_postprocess(
                &named_fields.clone().cloned().collect::<Vec<_>>()[i].ty,
            )
        })
        .collect::<std::vec::Vec<_>>();

    let sizers = named_fields
        .clone()
        .map(|f| generate_tasm_for_sizer_postprocess(&f.ty))
        .collect::<std::vec::Vec<_>>();

    let jumpers = named_fields
        .clone()
        .map(|f| generate_tasm_for_extend_field_start_with_jump_amount(&f.ty))
        .collect::<std::vec::Vec<_>>();

    let field_types = named_fields
        .clone()
        .map(|f| f.ty.clone())
        .collect::<std::vec::Vec<_>>();

    ParseResult {
        field_names: field_names_list,
        field_types,
        getters,
        sizers,
        jumpers,
    }
}

/// This function generates tasm code that
///  - assumes the stack is in the state _ *field_start field_jump_amount
///  - leaves the stack in the state _ *field
///
/// The complication arises from *field_start == *field when the field size is statically
/// known, but otherwise *field_start+1 == *field.
fn generate_tasm_for_getter_postprocess(field_type: &syn::Type) -> quote::__private::TokenStream {
    quote! {
        if <#field_type as twenty_first::shared_math::bfield_codec::BFieldCodec>::static_length().is_some() {
            [
                triton_vm::instruction::LabelledInstruction::Instruction(triton_vm::instruction::AnInstruction::Pop),
            ].to_vec()
        } else {
            [
                triton_vm::instruction::LabelledInstruction::Instruction(triton_vm::instruction::AnInstruction::Pop),
                triton_vm::instruction::LabelledInstruction::Instruction(triton_vm::instruction::AnInstruction::Push(twenty_first::shared_math::b_field_element::BFieldElement::new(1u64))),
                triton_vm::instruction::LabelledInstruction::Instruction(triton_vm::instruction::AnInstruction::Add),
            ].to_vec()
        }
    }
}

/// This function generates tasm code that
///  - assumes the stack is in the state _ *field_start field_jump_amount
///  - leaves the stack in the state _ *field field_size
///
/// The complication arises from *field_start == *field when the field size is statically
/// known, but otherwise *field_start+1 == *field.
fn generate_tasm_for_sizer_postprocess(field_type: &syn::Type) -> quote::__private::TokenStream {
    quote! {
        if <#field_type as twenty_first::shared_math::bfield_codec::BFieldCodec>::static_length().is_some() {
            std::vec::Vec::<triton_vm::instruction::LabelledInstruction>::new()
        } else {
            [
                triton_vm::instruction::LabelledInstruction::Instruction(triton_vm::instruction::AnInstruction::Push(twenty_first::shared_math::b_field_element::BFieldElement::new(18446744069414584320u64))),
                triton_vm::instruction::LabelledInstruction::Instruction(triton_vm::instruction::AnInstruction::Add),
                triton_vm::instruction::LabelledInstruction::Instruction(triton_vm::instruction::AnInstruction::Swap(triton_vm::op_stack::OpStackElement::ST1)),
                triton_vm::instruction::LabelledInstruction::Instruction(triton_vm::instruction::AnInstruction::Push(twenty_first::shared_math::b_field_element::BFieldElement::new(1u64))),
                triton_vm::instruction::LabelledInstruction::Instruction(triton_vm::instruction::AnInstruction::Add),
                triton_vm::instruction::LabelledInstruction::Instruction(triton_vm::instruction::AnInstruction::Swap(triton_vm::op_stack::OpStackElement::ST1)),
            ].to_vec()
        }
    }
}

/// This function generates tasm code that
///  - assumes the stack is in the state _ *field_start
///  - leaves the stack in the state _ *field_start jump_amount
fn generate_tasm_for_extend_field_start_with_jump_amount(
    field_type: &syn::Type,
) -> quote::__private::TokenStream {
    quote! {
        if let Some(size) = <#field_type as twenty_first::shared_math::bfield_codec::BFieldCodec>::static_length() {
            [
                triton_vm::instruction::LabelledInstruction::Instruction(triton_vm::instruction::AnInstruction::Push(twenty_first::shared_math::b_field_element::BFieldElement::new(size as u64)))
            ].to_vec()
        } else {
            [
                triton_vm::instruction::LabelledInstruction::Instruction(triton_vm::instruction::AnInstruction::ReadMem),
                triton_vm::instruction::LabelledInstruction::Instruction(triton_vm::instruction::AnInstruction::Push(twenty_first::shared_math::b_field_element::BFieldElement::new(1u64))),
                triton_vm::instruction::LabelledInstruction::Instruction(triton_vm::instruction::AnInstruction::Add),
            ].to_vec()
        }
    }
}

fn generate_tokens_for_struct_with_unnamed_fields(fields: &syn::FieldsUnnamed) -> ParseResult {
    let field_names = fields
        .unnamed
        .iter()
        .enumerate()
        .map(|(i, _f)| quote::format_ident!("field_{i}"));
    let field_names_list = field_names.clone().collect::<std::vec::Vec<_>>();

    let getters = fields
        .unnamed
        .iter()
        .enumerate()
        .map(|(i, _f)| {
            generate_tasm_for_getter_postprocess(
                &fields.unnamed.iter().cloned().collect::<Vec<_>>()[i].ty,
            )
        })
        .collect::<std::vec::Vec<_>>();

    let sizers = fields
        .unnamed
        .iter()
        .enumerate()
        .map(|(_i, f)| generate_tasm_for_sizer_postprocess(&f.ty))
        .collect::<std::vec::Vec<_>>();

    let jumpers = fields
        .unnamed
        .iter()
        .enumerate()
        .map(|(_i, f)| generate_tasm_for_extend_field_start_with_jump_amount(&f.ty))
        .collect::<std::vec::Vec<_>>();

    let field_types = fields
        .unnamed
        .iter()
        .map(|field| field.ty.clone())
        .collect::<std::vec::Vec<_>>();

    ParseResult {
        field_names: field_names_list,
        field_types,
        getters,
        sizers,
        jumpers,
    }
}

fn get_field_decoder(
    field_name: syn::Ident,
    field_type: syn::Type,
) -> quote::__private::TokenStream {
    quote! {
        let length : usize = if let Some(static_length) = <#field_type as twenty_first::shared_math::bfield_codec::BFieldCodec>::static_length() {
            static_length
        } else {
            iterator.next().unwrap().value() as usize
        };
        let sequence = (0..length).map(|_| iterator.next().unwrap()).collect::<Vec<_>>();
        let #field_name : #field_type = *twenty_first::shared_math::bfield_codec::BFieldCodec::decode(&sequence)?;
    }
}
