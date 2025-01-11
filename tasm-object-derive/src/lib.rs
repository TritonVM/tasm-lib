//! This crate provides a derive macro for the `TasmObject` and `TasmStruct`
//! traits.
//!
//! Example usage:
//! ```no_compile
//! #[derive(BFieldCodec, TasmObject)]
//! struct Foo<T: BFieldCodec> {
//!     t_list: Vec<T>,
//! }
//! ```
//!
//! notes:
//!  1. An implementation of `BFieldCodec` is required, else compilation will
//!     fail. It is recommended to derive `BFieldCodec`.
//!  2. If the target struct has a `where` clause with a trailing comma,
//!     compilation will fail.
//!
//!     See also: <https://github.com/TritonVM/tasm-lib/issues/91>
//!
//!     Example: do not do this.
//! ```no_compile
//! #[derive(BFieldCodec, TasmObject)]
//! struct Foo<T>
//! where
//!     T: BFieldCodec, { … }
//! //                ^
//! ```

extern crate proc_macro;

use proc_macro2::TokenStream;
use quote::quote;
use syn::DeriveInput;

/// Derives both, `TasmObject` and `TasmStruct` for structs.
#[proc_macro_derive(TasmObject, attributes(tasm_object))]
pub fn tasm_object_derive(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let ast = syn::parse(input).unwrap();
    impl_tasm_object_derive_macro(ast).into()
}

#[derive(Clone)]
struct ParseResult {
    field_names: Vec<syn::Ident>,
    field_types: Vec<syn::Type>,
    getters: Vec<TokenStream>,
    sizers: Vec<TokenStream>,
    jumpers: Vec<TokenStream>,
    ignored_fields: Vec<syn::Field>,
}

impl ParseResult {
    /// Generate the tasm code that
    /// - assumes the stack is in the state `_ *struct`
    /// - leaves the stack in the state `_ *field_n *field_(n-1) … *field_0`
    //
    // `self.field_types` is reversed compared to the struct's declaration order.
    // That is, for struct `Foo`:
    //
    // #[derive(BFieldCodec, TasmObject)]
    // struct Foo {
    //     a: Vec<u32>,
    //     b: XFieldElement,
    // }
    //
    // the element `self.field_types[0]` is `XFieldElement`, element
    // `self.field_types[1]` is `Vec<u32>`.
    //
    // An example for you to follow along while reading the function below: Consider
    // struct `Bar`:
    //
    // #[derive(BFieldCodec, TasmObject)]
    // struct Bar {
    //     a: Vec<u32>,
    //     b: XFieldElement,
    //     c: Vec<Digest>,
    // }
    //
    // The encoding of
    // Bar {
    //     a: vec![40, 41, 42],
    //     b: xfe!([43, 44, 45]),
    //     c: vec![Digest::new(bfe_array![46, 47, 48, 49, 50])]
    // }
    // loos like this:
    //
    //     ╭──────── c ────────╮  ╭─── b ──╮     ╭──── a ────╮
    //  6, 1, 46, 47, 48, 49, 50, 43, 44, 45, 4, 3, 40, 41, 42
    //  ↑  ↑                       ↑          ↑  ↑
    //  | *c                      *b          | *a
    // *c_si                                 *a_si
    //
    // A pointer `*bar` is equal to pointer `*c_si`.
    //
    fn generate_tasm_for_destructuring(&self) -> TokenStream {
        debug_assert_eq!(self.field_types.len(), self.field_names.len());

        let mut fields = self.field_types.iter().zip(&self.field_names);
        let Some(first_field) = fields.next_back() else {
            return quote!([
                crate::triton_vm::isa::instruction::LabelledInstruction::Instruction(
                    crate::triton_vm::isa::instruction::AnInstruction::Pop(
                        crate::triton_vm::isa::op_stack::NumberOfWords::N1
                    )
                )
            ]
            .to_vec());
        };

        let mut tasm = quote!(let mut instructions = ::std::vec::Vec::new(););
        for (field_ty, field_name) in fields {
            let field_name = field_name.to_string();
            let field_ptr_hint = quote!(
                crate::triton_vm::isa::instruction::LabelledInstruction::TypeHint(
                    crate::triton_vm::isa::instruction::TypeHint {
                        starting_index: 0,
                        length: 1,
                        type_name: ::std::option::Option::Some("Pointer".to_string()),
                        variable_name: ::std::string::String::from(#field_name),
                    }
                )
            );

            tasm.extend(quote!(
            if let Some(size) = <#field_ty as crate::twenty_first::math::bfield_codec::BFieldCodec>::static_length() {
                instructions.extend([
                    #field_ptr_hint,
                    // _ *field
                    crate::triton_vm::isa::instruction::LabelledInstruction::Instruction(crate::triton_vm::isa::instruction::AnInstruction::Dup(crate::triton_vm::isa::op_stack::OpStackElement::ST0)),
                    // _ *field *field
                    crate::triton_vm::isa::instruction::LabelledInstruction::Instruction(crate::triton_vm::isa::instruction::AnInstruction::AddI(crate::BFieldElement::from(size))),
                    // _ *field *next_field_or_next_field_si
                ]);
            } else {
                instructions.extend([
                    // _ *field_si
                    crate::triton_vm::isa::instruction::LabelledInstruction::Instruction(crate::triton_vm::isa::instruction::AnInstruction::ReadMem(crate::triton_vm::isa::op_stack::NumberOfWords::N1)),
                    // _ field_size (*field_si - 1)
                    crate::triton_vm::isa::instruction::LabelledInstruction::Instruction(crate::triton_vm::isa::instruction::AnInstruction::AddI(crate::BFieldElement::new(2u64))),
                    #field_ptr_hint,
                    // _ field_size *field
                    crate::triton_vm::isa::instruction::LabelledInstruction::Instruction(crate::triton_vm::isa::instruction::AnInstruction::Dup(crate::triton_vm::isa::op_stack::OpStackElement::ST0)),
                    // _ field_size *field *field
                    crate::triton_vm::isa::instruction::LabelledInstruction::Instruction(crate::triton_vm::isa::instruction::AnInstruction::Pick(crate::triton_vm::isa::op_stack::OpStackElement::ST2)),
                    // _ *field *field field_size
                    crate::triton_vm::isa::instruction::LabelledInstruction::Instruction(crate::triton_vm::isa::instruction::AnInstruction::Push(crate::triton_vm::prelude::BFieldElement::new(Self::MAX_OFFSET.into()))),
                    crate::triton_vm::isa::instruction::LabelledInstruction::Instruction(crate::triton_vm::isa::instruction::AnInstruction::Dup(crate::triton_vm::isa::op_stack::OpStackElement::ST1)),
                    crate::triton_vm::isa::instruction::LabelledInstruction::Instruction(crate::triton_vm::isa::instruction::AnInstruction::Lt),
                    crate::triton_vm::isa::instruction::LabelledInstruction::Instruction(crate::triton_vm::isa::instruction::AnInstruction::Assert),
                    crate::triton_vm::isa::instruction::LabelledInstruction::AssertionContext(crate::triton_vm::isa::instruction::AssertionContext::ID(183_i128)),
                    // _ *field *field field_size
                    crate::triton_vm::isa::instruction::LabelledInstruction::Instruction(crate::triton_vm::isa::instruction::AnInstruction::Add),
                    // _ *field *next_field_or_next_field_si
                ]);
            }
        ));
        }

        let (first_field_ty, first_field_name) = first_field;
        let first_field_name = first_field_name.to_string();
        let first_field_ptr_hint = quote!(
            crate::triton_vm::isa::instruction::LabelledInstruction::TypeHint(
                crate::triton_vm::isa::instruction::TypeHint {
                    starting_index: 0,
                    length: 1,
                    type_name: ::std::option::Option::Some("Pointer".to_string()),
                    variable_name: ::std::string::String::from(#first_field_name),
                }
            )
        );

        tasm.extend(quote!(
            if <#first_field_ty as crate::twenty_first::math::bfield_codec::BFieldCodec>::static_length().is_none() {
                // shift final pointer from size indicator to actual field
                instructions.push(
                    crate::triton_vm::isa::instruction::LabelledInstruction::Instruction(crate::triton_vm::isa::instruction::AnInstruction::AddI(crate::BFieldElement::new(1u64))),
                );
            }
            instructions.push(#first_field_ptr_hint);
        ));

        tasm.extend(quote!(instructions));
        tasm
    }
}

fn generate_integral_size_indicators_code(parse_result: &ParseResult) -> TokenStream {
    let mut integral_size_indicators_code = quote! {};
    for field_type in parse_result.field_types.iter() {
        // INVARIANT: accumulated_size *field_start
        integral_size_indicators_code = quote! {
            #integral_size_indicators_code
            // _ accumulated_size *field_start
            if let Some(static_length) = <#field_type as crate::triton_vm::twenty_first::math::bfield_codec::BFieldCodec>::static_length() {
                let addi_len = crate::triton_vm::isa::instruction::LabelledInstruction::Instruction(crate::triton_vm::isa::instruction::AnInstruction::AddI(crate::BFieldElement::from(static_length)));
                [
                    // _ accumulated_size *field
                    addi_len.clone(),
                    pick1.clone(),
                    addi_len.clone(),
                    place1.clone(),
                    // _ accumulated_size' *next_field
                ].to_vec()
            } else {
                [
                    [
                        // _ accumulated_size *indicated_field_size
                        read_mem1.clone(),
                        // _ accumulated_size indicated_field_size (*field-2)

                        pushmax.clone(),
                        dup2.clone(),
                        lt.clone(),
                        // _ accumulated_size indicated_field_size (*field-2) (MAX > indicated_field_size)

                        assert.clone(),
                        assertion_context_field_size_too_big.clone(),
                        // _ accumulated_size indicated_field_size (*field-2)

                        addi2.clone(),
                        // _ accumulated_size indicated_field_size *field

                        dup0.clone(),
                        // _ accumulated_size indicated_field_size *field *field
                    ].to_vec(),
                    <#field_type as crate::tasm_lib::structure::tasm_object::TasmObject>::compute_size_and_assert_valid_size_indicator(library),
                    // _ accumulated_size indicated_field_size *field computed_size
                    [
                        dup2.clone(),
                        // _ accumulated_size indicated_field_size *field computed_size indicated_field_size
                        eq.clone(),
                        // _ accumulated_size indicated_field_size *field (computed_size == indicated_field_size)
                        assert.clone(),
                        assertion_context_field_size_unequal_computed.clone(),
                        // _ accumulated_size indicated_field_size *field

                        dup1.clone(),
                        add.clone(),
                        // _ accumulated_size indicated_field_size *next_field

                        place2.clone(),
                        // _  *next_field accumulated_size indicated_field_size

                        /* Add one for size-indicator on this struct */
                        add.clone(),
                        addi1.clone(),
                        // _ *next_field accumulated_size'

                        pick1.clone(),
                        // _ accumulated_size' *next_field
                    ].to_vec(),
                ].concat()
            },
        };
    }

    integral_size_indicators_code = quote! {
        let push0 = crate::triton_vm::isa::instruction::LabelledInstruction::Instruction(crate::triton_vm::isa::instruction::AnInstruction::Push(0u64.into()));
        let pushmax = crate::triton_vm::isa::instruction::LabelledInstruction::Instruction(crate::triton_vm::isa::instruction::AnInstruction::Push(Self::MAX_OFFSET.into()));
        let dup0 = crate::triton_vm::isa::instruction::LabelledInstruction::Instruction(crate::triton_vm::isa::instruction::AnInstruction::Dup(crate::triton_vm::isa::op_stack::OpStackElement::ST0));
        let dup1 = crate::triton_vm::isa::instruction::LabelledInstruction::Instruction(crate::triton_vm::isa::instruction::AnInstruction::Dup(crate::triton_vm::isa::op_stack::OpStackElement::ST1));
        let dup2 = crate::triton_vm::isa::instruction::LabelledInstruction::Instruction(crate::triton_vm::isa::instruction::AnInstruction::Dup(crate::triton_vm::isa::op_stack::OpStackElement::ST2));
        let pick1 = crate::triton_vm::isa::instruction::LabelledInstruction::Instruction(crate::triton_vm::isa::instruction::AnInstruction::Pick(crate::triton_vm::isa::op_stack::OpStackElement::ST1));
        let place1 = crate::triton_vm::isa::instruction::LabelledInstruction::Instruction(crate::triton_vm::isa::instruction::AnInstruction::Place(crate::triton_vm::isa::op_stack::OpStackElement::ST1));
        let place2 = crate::triton_vm::isa::instruction::LabelledInstruction::Instruction(crate::triton_vm::isa::instruction::AnInstruction::Place(crate::triton_vm::isa::op_stack::OpStackElement::ST2));
        let lt = crate::triton_vm::isa::instruction::LabelledInstruction::Instruction(crate::triton_vm::isa::instruction::AnInstruction::Lt);
        let assert = crate::triton_vm::isa::instruction::LabelledInstruction::Instruction(crate::triton_vm::isa::instruction::AnInstruction::Assert);
        let assertion_context_field_size_too_big = crate::triton_vm::isa::instruction::LabelledInstruction::AssertionContext(crate::triton_vm::isa::instruction::AssertionContext::ID(180_i128));
        let assertion_context_field_size_unequal_computed = crate::triton_vm::isa::instruction::LabelledInstruction::AssertionContext(crate::triton_vm::isa::instruction::AssertionContext::ID(181_i128));
        let eq = crate::triton_vm::isa::instruction::LabelledInstruction::Instruction(crate::triton_vm::isa::instruction::AnInstruction::Eq);
        let add = crate::triton_vm::isa::instruction::LabelledInstruction::Instruction(crate::triton_vm::isa::instruction::AnInstruction::Add);
        let read_mem1 = crate::triton_vm::isa::instruction::LabelledInstruction::Instruction(crate::triton_vm::isa::instruction::AnInstruction::ReadMem(crate::triton_vm::isa::op_stack::NumberOfWords::N1));
        let addi1 = crate::triton_vm::isa::instruction::LabelledInstruction::Instruction(crate::triton_vm::isa::instruction::AnInstruction::AddI(crate::BFieldElement::new(1u64)));
        let addi2 = crate::triton_vm::isa::instruction::LabelledInstruction::Instruction(crate::triton_vm::isa::instruction::AnInstruction::AddI(crate::BFieldElement::new(2u64)));
        let pop1 = crate::triton_vm::isa::instruction::LabelledInstruction::Instruction(crate::triton_vm::isa::instruction::AnInstruction::Pop(crate::triton_vm::isa::op_stack::NumberOfWords::N1));
        let hint_acc_size = [
                    crate::triton_vm::isa::instruction::LabelledInstruction::TypeHint(
                        crate::triton_vm::isa::instruction::TypeHint {
                            starting_index: 1,
                            length: 1,
                            type_name: ::std::option::Option::<::std::string::String>::None,
                            variable_name: ::std::string::String::from("acc_size"),
                        }
                    )
                ].to_vec();
        let hint_field_ptr = [
            crate::triton_vm::isa::instruction::LabelledInstruction::TypeHint(
                crate::triton_vm::isa::instruction::TypeHint {
                    starting_index: 0,
                    length: 1,
                    type_name: ::std::option::Option::<::std::string::String>::None,
                    variable_name: ::std::string::String::from("field_ptr"),
                }
            )
        ].to_vec();
        [
            [
                push0.clone(),
                place1.clone(),
            ].to_vec(),
            hint_acc_size,
            hint_field_ptr,
            #integral_size_indicators_code
            [
                // _ acc_size *EOF
                pop1.clone(),
                // _ acc_size
            ].to_vec(),
        ].concat()
    };

    integral_size_indicators_code
}

fn impl_tasm_object_derive_macro(ast: DeriveInput) -> TokenStream {
    let parse_result = generate_parse_result(&ast);
    let name = &ast.ident;
    let name_as_string = ast.ident.to_string();

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
                        [crate::triton_vm::isa::instruction::LabelledInstruction::Instruction(crate::triton_vm::isa::instruction::AnInstruction::Add)].to_vec(),
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
    let get_field_code = if parse_result.field_names.is_empty() {
        quote!(panic!("{} has no fields", #name_as_string);)
    } else {
        quote!(
            let field_getter = match field_name {
                #( #just_field_clauses ,)*
                unknown_field_name => panic!("Cannot match on field name `{unknown_field_name}`."),
            };
            let hint_appendix = [
                crate::triton_vm::isa::instruction::LabelledInstruction::TypeHint(
                    crate::triton_vm::isa::instruction::TypeHint {
                        starting_index: 0,
                        length: 1,
                        type_name: ::std::option::Option::<::std::string::String>::None,
                        variable_name: ::std::string::String::from(field_name),
                    }
                )
            ].to_vec();
            [field_getter, hint_appendix].concat()
        )
    };

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
    let get_field_with_size_code = if parse_result.field_names.is_empty() {
        quote!(panic!("{} has no fields", #name_as_string);)
    } else {
        quote!(
            let field_getter = match field_name {
                #( #field_with_size_clauses ,)*
                unknown_field_name => panic!("Cannot match on field name `{unknown_field_name}`."),
            };
            let hint_appendix = [
                crate::triton_vm::isa::instruction::LabelledInstruction::TypeHint(
                    crate::triton_vm::isa::instruction::TypeHint {
                        starting_index: 0,
                        length: 1,
                        type_name: ::std::option::Option::Some(::std::string::String::from("u32")),
                        variable_name: ::std::string::String::from("size"),
                    }
                ),
                crate::triton_vm::isa::instruction::LabelledInstruction::TypeHint(
                    crate::triton_vm::isa::instruction::TypeHint {
                        starting_index: 1,
                        length: 1,
                        type_name: ::std::option::Option::<::std::string::String>::None,
                        variable_name: ::std::string::String::from(field_name),
                    }
                )
            ].to_vec();

            [field_getter, hint_appendix].concat()
        )
    };

    let field_starter_clauses = parse_result.field_names
        .iter()
        .zip(parse_result.jumpers.iter())
        .enumerate()
        .map(|(index, (name, jumper))| {
            let field_name = name.to_string();
            match index {
                0 => quote!{
                    #field_name => { #jumper }
                },
                not_zero => {
                    let previous_field_name = parse_result.field_names[not_zero-1].to_string();
                    quote! {
                        #field_name => {
                            let prev =
                            [
                                Self::get_field_start_with_jump_distance(#previous_field_name),
                                    // _ *prev_field_start prev_field_size
                                [crate::triton_vm::isa::instruction::LabelledInstruction::Instruction(crate::triton_vm::isa::instruction::AnInstruction::Add)].to_vec(),
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
    let ignored_field_names = parse_result
        .ignored_fields
        .iter()
        .map(|f| f.ident.clone())
        .collect::<Vec<_>>();

    let self_builder = match &ast.data {
        syn::Data::Struct(syn::DataStruct {
            fields: syn::Fields::Named(_),
            ..
        }) => {
            quote! { Self { #( #field_names ,)* #( #ignored_field_names : Default::default(), )* } }
        }
        syn::Data::Struct(syn::DataStruct {
            fields: syn::Fields::Unnamed(_),
            ..
        }) => {
            let reversed_field_names = parse_result.field_names.iter().rev();
            let defaults = vec![quote! { Default::default() }; ignored_field_names.len()];
            quote! { Self( #( #reversed_field_names ,)* #( #defaults , )* ) }
        }
        _ => unreachable!("expected a struct with named fields, or with unnamed fields"),
    };

    let (impl_generics, ty_generics, where_clause) = ast.generics.split_for_impl();
    let ignored_field_types = parse_result.ignored_fields.iter().map(|f| f.ty.clone());
    let new_where_clause = if let Some(old_where_clause) = where_clause {
        quote! {
            #old_where_clause,
            #(#ignored_field_types : Default ,)*
        }
    } else {
        quote! {
            where #(#ignored_field_types : Default ,)*
        }
    };

    let integral_size_indicators_code = generate_integral_size_indicators_code(&parse_result);
    let destructuring_tasm = parse_result.generate_tasm_for_destructuring();

    quote! {
        impl #impl_generics crate::tasm_lib::structure::tasm_object::TasmObject
        for #name #ty_generics #new_where_clause {
            fn label_friendly_name() -> String {
                #name_as_string.to_owned()
            }

            fn compute_size_and_assert_valid_size_indicator(
                library: &mut crate::tasm_lib::library::Library
            ) -> ::std::vec::Vec<crate::triton_vm::isa::instruction::LabelledInstruction> {
                #integral_size_indicators_code
            }

            fn decode_iter<Itr: Iterator<Item=crate::BFieldElement>>(
                iterator: &mut Itr
            ) -> ::std::result::Result<
                    ::std::boxed::Box<Self>,
                    ::std::boxed::Box<
                        dyn ::std::error::Error
                        + ::core::marker::Send
                        + ::core::marker::Sync
                    >
                >
            {
                #( #field_decoders )*
                ::std::result::Result::Ok(::std::boxed::Box::new(#self_builder))
            }
        }

        impl #impl_generics crate::tasm_lib::structure::tasm_object::TasmStruct
        for #name #ty_generics #new_where_clause {
            fn get_field(
                field_name: &str
            ) -> ::std::vec::Vec<crate::triton_vm::isa::instruction::LabelledInstruction> {
                #get_field_code
            }

            fn get_field_with_size(
                field_name: &str
            ) -> ::std::vec::Vec<crate::triton_vm::isa::instruction::LabelledInstruction> {
                #get_field_with_size_code
            }

            fn get_field_start_with_jump_distance(
                field_name: &str
            ) -> ::std::vec::Vec<crate::triton_vm::isa::instruction::LabelledInstruction> {
                match field_name {
                    #( #field_starter_clauses ,)*
                    unknown_field_name => panic!("Cannot match on field name `{unknown_field_name}`."),
                }
            }

            fn destructure(
            ) -> ::std::vec::Vec<crate::triton_vm::isa::instruction::LabelledInstruction> {
                #destructuring_tasm
            }
        }
    }
}

fn generate_parse_result(ast: &DeriveInput) -> ParseResult {
    match &ast.data {
        syn::Data::Struct(syn::DataStruct {
            fields: syn::Fields::Named(fields),
            ..
        }) => generate_tokens_for_struct_with_named_fields(fields),
        syn::Data::Struct(syn::DataStruct {
            fields: syn::Fields::Unnamed(fields),
            ..
        }) => generate_tokens_for_struct_with_unnamed_fields(fields),
        _ => panic!("expected a struct with named fields, or with unnamed fields"),
    }
}

fn field_is_ignored(field: &syn::Field) -> bool {
    for attribute in field.attrs.iter() {
        if !attribute.path().is_ident("tasm_object") {
            continue;
        }
        attribute
            .parse_nested_meta(|meta| match meta.path.get_ident() {
                Some(ident) if ident == "ignore" => Ok(()),
                Some(ident) => Err(meta.error(format!("Unknown identifier \"{ident}\"."))),
                _ => Err(meta.error("Expected an identifier.")),
            })
            .unwrap();
        return true;
    }
    false
}

fn generate_tokens_for_struct_with_named_fields(fields: &syn::FieldsNamed) -> ParseResult {
    let ignored_fields = fields
        .named
        .iter()
        .rev()
        .filter(|f| field_is_ignored(f))
        .cloned()
        .collect::<Vec<_>>();
    let named_fields = fields.named.iter().rev().filter(|f| !field_is_ignored(f));

    let field_names = named_fields
        .clone()
        .map(|field| field.ident.as_ref().unwrap().to_owned());
    let field_names_list = field_names.clone().collect::<Vec<_>>();

    let getters = named_fields
        .clone()
        .enumerate()
        .map(|(i, _f)| {
            generate_tasm_for_getter_postprocess(
                &named_fields.clone().cloned().collect::<Vec<_>>()[i].ty,
            )
        })
        .collect::<Vec<_>>();

    let sizers = named_fields
        .clone()
        .map(|f| generate_tasm_for_sizer_postprocess(&f.ty))
        .collect::<Vec<_>>();

    let jumpers = named_fields
        .clone()
        .map(|f| generate_tasm_for_extend_field_start_with_jump_amount(&f.ty))
        .collect::<Vec<_>>();

    let field_types = named_fields
        .clone()
        .map(|f| f.ty.clone())
        .collect::<Vec<_>>();

    ParseResult {
        field_names: field_names_list,
        field_types,
        getters,
        sizers,
        jumpers,
        ignored_fields,
    }
}

/// This function generates tasm code that
///  - assumes the stack is in the state _ *field_start field_jump_amount
///  - leaves the stack in the state _ *field
///
/// The complication arises from *field_start == *field when the field size is statically
/// known, but otherwise *field_start+1 == *field.
fn generate_tasm_for_getter_postprocess(field_type: &syn::Type) -> TokenStream {
    quote! {
        if <#field_type as crate::twenty_first::math::bfield_codec::BFieldCodec>::static_length().is_some() {
            [
                crate::triton_vm::isa::instruction::LabelledInstruction::Instruction(crate::triton_vm::isa::instruction::AnInstruction::Pop(crate::triton_vm::isa::op_stack::NumberOfWords::N1)),
            ].to_vec()
        } else {
            [
                crate::triton_vm::isa::instruction::LabelledInstruction::Instruction(crate::triton_vm::isa::instruction::AnInstruction::Pop(crate::triton_vm::isa::op_stack::NumberOfWords::N1)),
                crate::triton_vm::isa::instruction::LabelledInstruction::Instruction(crate::triton_vm::isa::instruction::AnInstruction::AddI(crate::BFieldElement::new(1u64))),
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
fn generate_tasm_for_sizer_postprocess(field_type: &syn::Type) -> TokenStream {
    quote! {
        if <#field_type as crate::twenty_first::math::bfield_codec::BFieldCodec>::static_length().is_some() {
            ::std::vec::Vec::<crate::triton_vm::isa::instruction::LabelledInstruction>::new()
        } else {
            [
                crate::triton_vm::isa::instruction::LabelledInstruction::Instruction(crate::triton_vm::isa::instruction::AnInstruction::AddI(-crate::BFieldElement::new(1u64))),
                crate::triton_vm::isa::instruction::LabelledInstruction::Instruction(crate::triton_vm::isa::instruction::AnInstruction::Pick(crate::triton_vm::isa::op_stack::OpStackElement::ST1)),
                crate::triton_vm::isa::instruction::LabelledInstruction::Instruction(crate::triton_vm::isa::instruction::AnInstruction::AddI(crate::BFieldElement::new(1u64))),
                crate::triton_vm::isa::instruction::LabelledInstruction::Instruction(crate::triton_vm::isa::instruction::AnInstruction::Place(crate::triton_vm::isa::op_stack::OpStackElement::ST1)),
            ].to_vec()
        }
    }
}

/// This function generates tasm code that
///  - assumes the stack is in the state _ *field_start
///  - leaves the stack in the state _ *field_start jump_amount
fn generate_tasm_for_extend_field_start_with_jump_amount(field_type: &syn::Type) -> TokenStream {
    quote! {
        if let Some(size) = <#field_type as crate::twenty_first::math::bfield_codec::BFieldCodec>::static_length() {
            [
                crate::triton_vm::isa::instruction::LabelledInstruction::Instruction(crate::triton_vm::isa::instruction::AnInstruction::Push(crate::BFieldElement::from(size)))
            ].to_vec()
        } else {
            [
                // _ *object
                crate::triton_vm::isa::instruction::LabelledInstruction::Instruction(crate::triton_vm::isa::instruction::AnInstruction::ReadMem(crate::triton_vm::isa::op_stack::NumberOfWords::N1)),
                // _ si (*object-1)
                crate::triton_vm::isa::instruction::LabelledInstruction::Instruction(crate::triton_vm::isa::instruction::AnInstruction::Push(crate::triton_vm::prelude::BFieldElement::new(Self::MAX_OFFSET.into()))),
                // _ si (*object-1) MAX
                crate::triton_vm::isa::instruction::LabelledInstruction::Instruction(crate::triton_vm::isa::instruction::AnInstruction::Dup(crate::triton_vm::isa::op_stack::OpStackElement::ST2)),
                // _ si (*object-1) MAX si
                crate::triton_vm::isa::instruction::LabelledInstruction::Instruction(crate::triton_vm::isa::instruction::AnInstruction::Lt),
                // _ si (*object-1) (si < MAX)
                crate::triton_vm::isa::instruction::LabelledInstruction::Instruction(crate::triton_vm::isa::instruction::AnInstruction::Assert),
                crate::triton_vm::isa::instruction::LabelledInstruction::AssertionContext(crate::triton_vm::isa::instruction::AssertionContext::ID(182_i128)),
                // _ si (*object-1)
                crate::triton_vm::isa::instruction::LabelledInstruction::Instruction(crate::triton_vm::isa::instruction::AnInstruction::AddI(crate::BFieldElement::new(1u64))),
                // _ si *object
                crate::triton_vm::isa::instruction::LabelledInstruction::Instruction(crate::triton_vm::isa::instruction::AnInstruction::Place(crate::triton_vm::isa::op_stack::OpStackElement::ST1)),
                // _ *object si
                crate::triton_vm::isa::instruction::LabelledInstruction::Instruction(crate::triton_vm::isa::instruction::AnInstruction::AddI(crate::BFieldElement::new(1u64))),
                // _ *object (si + 1)
                // _ *object jummp_amount
            ].to_vec()
        }
    }
}

fn generate_tokens_for_struct_with_unnamed_fields(fields: &syn::FieldsUnnamed) -> ParseResult {
    let fields_iterator = fields.unnamed.iter().rev();
    let ignored_fields = fields_iterator
        .clone()
        .filter(|f| field_is_ignored(f))
        .cloned()
        .collect::<Vec<_>>();

    let field_count = fields_iterator.clone().count();
    let field_names = fields_iterator
        .clone()
        .filter(|f| !field_is_ignored(f))
        .enumerate()
        .map(|(i, _f)| quote::format_ident!("field_{}", field_count - 1 - i))
        .collect::<Vec<_>>();

    let getters = fields_iterator
        .clone()
        .enumerate()
        .map(|(i, _f)| {
            generate_tasm_for_getter_postprocess(
                &fields_iterator.clone().nth(i).cloned().unwrap().ty,
            )
        })
        .collect::<Vec<_>>();

    let sizers = fields_iterator
        .clone()
        .map(|f| generate_tasm_for_sizer_postprocess(&f.ty))
        .collect::<Vec<_>>();

    let jumpers = fields_iterator
        .clone()
        .map(|f| generate_tasm_for_extend_field_start_with_jump_amount(&f.ty))
        .collect::<Vec<_>>();

    let field_types = fields_iterator
        .clone()
        .map(|field| field.ty.clone())
        .collect::<Vec<_>>();

    ParseResult {
        field_names,
        field_types,
        getters,
        sizers,
        jumpers,
        ignored_fields,
    }
}

fn get_field_decoder(field_name: syn::Ident, field_type: syn::Type) -> TokenStream {
    quote! {
        let length: usize = if let Some(static_length) = <#field_type as crate::twenty_first::math::bfield_codec::BFieldCodec>::static_length() {
            static_length
        } else {
            iterator.next().ok_or("iterator exhausted")?.try_into()?
        };
        let sequence = (0..length)
            .map(|_| iterator.next())
            .collect::<::std::option::Option<::std::vec::Vec<_>>>()
            .ok_or("iterator exhausted")?;
        let #field_name : #field_type = *crate::twenty_first::math::bfield_codec::BFieldCodec::decode(&sequence)?;
    }
}

#[cfg(test)]
mod tests {
    use syn::parse_quote;

    use super::*;

    #[test]
    #[should_panic] // unit structs are not supported (yet?)
    fn unit_struct() {
        let ast = parse_quote! {
            #[derive(TasmObject)]
            struct UnitStruct;
        };
        let _rust_code = impl_tasm_object_derive_macro(ast);
    }

    #[test]
    fn tuple_struct() {
        let ast = parse_quote! {
            #[derive(TasmObject)]
            struct TupleStruct(u64, u32);
        };
        let _rust_code = impl_tasm_object_derive_macro(ast);
    }

    #[test]
    fn struct_with_named_fields() {
        let ast = parse_quote! {
            #[derive(TasmObject)]
            struct StructWithNamedFields {
                field1: u64,
                field2: u32,
                #[bfield_codec(ignore)]
                ignored_field: bool,
            }
        };
        let _rust_code = impl_tasm_object_derive_macro(ast);
    }

    #[test]
    #[should_panic(expected = "expected a struct")] // enums are not supported (yet?)
    fn enum_with_tuple_variants() {
        let ast = parse_quote! {
            #[derive(TasmObject)]
            enum Enum {
                Variant1,
                Variant2(u64),
                Variant3(u64, u32),
                #[bfield_codec(ignore)]
                IgnoredVariant,
            }
        };
        let _rust_code = impl_tasm_object_derive_macro(ast);
    }

    #[test]
    fn generic_tuple_struct() {
        let ast = parse_quote! {
            #[derive(TasmObject)]
            struct TupleStruct<T>(T, (T, T));
        };
        let _rust_code = impl_tasm_object_derive_macro(ast);
    }

    #[test]
    fn generic_struct_with_named_fields() {
        let ast = parse_quote! {
            #[derive(TasmObject)]
            struct StructWithNamedFields<T> {
                field1: T,
                field2: (T, T),
                #[bfield_codec(ignore)]
                ignored_field: bool,
            }
        };
        let _rust_code = impl_tasm_object_derive_macro(ast);
    }

    #[test]
    #[should_panic(expected = "expected a struct")] // enums are not supported (yet?)
    fn generic_enum() {
        let ast = parse_quote! {
            #[derive(TasmObject)]
            enum Enum<T> {
                Variant1,
                Variant2(T),
                Variant3(T, T),
                #[bfield_codec(ignore)]
                IgnoredVariant,
            }
        };
        let _rust_code = impl_tasm_object_derive_macro(ast);
    }

    #[test]
    fn struct_with_types_from_twenty_first() {
        let ast = parse_quote! {
            #[derive(TasmObject)]
            struct WithComplexFields {
                pub digest: Digest,
                pub my_vec: Vec<BFieldElement>,
            }
        };
        let _rust_code = impl_tasm_object_derive_macro(ast);
    }
}
