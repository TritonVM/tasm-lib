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

/// Add a bound `T: BFieldCodec` to every type parameter T, unless we ignore it.
fn add_trait_bounds(mut generics: syn::Generics) -> syn::Generics {
    for param in &mut generics.params {
        let syn::GenericParam::Type(type_param) = param else {
            continue
        };
        type_param.bounds.push(syn::parse_quote!(
            twenty_first::shared_math::bfield_codec::BFieldCodec
        ));
        type_param.bounds.push(syn::parse_quote!(TasmObject));
    }
    generics
}

fn impl_derive_tasm_object_macro(ast: syn::DeriveInput) -> TokenStream {
    let (names, getters, sizers) = match &ast.data {
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

    // Add a bound `T: BFieldCodec` to every type parameter T.
    let generics = add_trait_bounds(ast.generics);

    // Extract the generics of the struct/enum.
    let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();

    // generate clauses for match statements
    let just_field_clauses = names.iter().zip(getters.iter()).map(|(name, getter)| {
        let name_as_string = name.to_string();
        quote! {
            #name_as_string => { #getter }
        }
    });
    let field_with_size_clauses =
        names
            .iter()
            .zip(getters.iter().zip(sizers.iter()))
            .map(|(name, (getter, sizer))| {
                let name_as_string = name.to_string();
                quote! {
                    #name_as_string => {
                        let getter = #getter;
                        let sizer = #sizer;
                        [getter, sizer].concat()
                    }
                }
            });

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
        }
    };

    gen.into()
}

fn generate_tokens_for_struct_with_named_fields(
    fields: &syn::FieldsNamed,
) -> (
    Vec<syn::Ident>,
    Vec<quote::__private::TokenStream>,
    Vec<quote::__private::TokenStream>,
) {
    let named_fields = fields.named.iter();

    let field_names = named_fields
        .clone()
        .map(|field| field.ident.as_ref().unwrap().to_owned());
    let field_names_list = field_names.clone().collect::<std::vec::Vec<_>>();

    let getters = named_fields
        .clone()
        .enumerate()
        .map(|(i, _f)| generate_tasm_for_getter(i, &field_names_list))
        .collect::<std::vec::Vec<_>>();

    let sizers = named_fields
        .map(|f| generate_tasm_code_for_size(&f.ty))
        .collect::<std::vec::Vec<_>>();

    (field_names_list, getters, sizers)
}

fn generate_tasm_for_getter(index: usize, fields: &[syn::Ident]) -> quote::__private::TokenStream {
    if index == 0 {
        quote! { std::vec::Vec::new() }
    } else {
        let previous_field_as_string = fields[index - 1].to_string();
        quote! {
            [
                Self::get_field_with_size( #previous_field_as_string ),
                [triton_vm::instruction::LabelledInstruction::Instruction(triton_vm::instruction::AnInstruction::Add)].to_vec(),
            ].concat()
        }
    }
}

fn generate_tasm_code_for_size(field_type: &syn::Type) -> quote::__private::TokenStream {
    quote! {
        if let Some(size) = <#field_type as twenty_first::shared_math::bfield_codec::BFieldCodec>::static_length() {
            [
                triton_vm::instruction::LabelledInstruction::Instruction(triton_vm::instruction::AnInstruction::Push(twenty_first::shared_math::b_field_element::BFieldElement::new(size as u64)))
            ].to_vec()
        } else {
            [
                triton_vm::instruction::LabelledInstruction::Instruction(triton_vm::instruction::AnInstruction::ReadIo)
            ].to_vec()
        }
    }
}

fn generate_tokens_for_struct_with_unnamed_fields(
    fields: &syn::FieldsUnnamed,
) -> (
    Vec<syn::Ident>,
    Vec<quote::__private::TokenStream>,
    Vec<quote::__private::TokenStream>,
) {
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
        .map(|(i, _f)| generate_tasm_for_getter(i, &field_names_list))
        .collect::<std::vec::Vec<_>>();

    let sizers = fields
        .unnamed
        .iter()
        .enumerate()
        .map(|(_i, f)| generate_tasm_code_for_size(&f.ty))
        .collect::<std::vec::Vec<_>>();

    (field_names_list, getters, sizers)
}
