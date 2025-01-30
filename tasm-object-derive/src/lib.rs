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
//! Note: An implementation of `BFieldCodec` is required, else compilation will
//! fail. It is recommended to derive `BFieldCodec`.
//!
//! ### Known limitations
//!
//!  - Ignoring fields in tuple structs is currently not supported. Consider
//!    using a struct with named fields instead.
//!
//!    ```no_compile
//!    #[derive(BFieldCodec, TasmObject)]
//!    struct Foo(#[tasm_object(ignore)] u32);
//!    //         ~~~~~~~~~~~~~~~~~~~~~~
//!    //         currently unsupported in tuple structs
//!    ```

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

// To follow along with the more involved functions, consider struct `Foo`:
//
// #[derive(BFieldCodec, TasmObject)]
// struct Foo {
//     a: Vec<u32>,
//     b: XFieldElement,
//     c: Vec<Digest>,
// }
//
// The encoding of
// Foo {
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
// The abbreviation `si` means “size indicator”. Any dynamically-sized field
// is prepended by a size indicator.
//
// A pointer `*foo` is equal to pointer `*c_si`.
//
#[derive(Clone)]
struct ParsedStruct {
    /// The names of all relevant fields. Notably, ignored fields are _not_ included.
    ///
    /// Reversed compared to the field declaration order. That is, for struct `Foo`:
    ///
    /// ```no_compile
    /// #[derive(BFieldCodec, TasmObject)]
    /// struct Foo {
    ///     a: Vec<u32>,
    ///     b: XFieldElement,
    /// }
    /// ```
    ///
    /// element `self.field_names[0]` is `b`, element `self.field_names[1]` is `a`.
    field_names: Vec<syn::Ident>,

    /// The types of all relevant fields. Notably, ignored fields are _not_ included.
    ///
    /// The order of the entries mimics the order of field
    /// [`field_names`](Self::field_names).
    field_types: Vec<syn::Type>,

    ignored_fields: Vec<syn::Field>,

    /// The rust code to assemble the struct that's annotated with the derive macro.
    /// Variables with identifiers equal to the struct field identifiers and of the
    /// appropriate type must be in scope.
    struct_builder: TokenStream,
}

impl ParsedStruct {
    fn new(ast: &DeriveInput) -> Self {
        let syn::Data::Struct(syn::DataStruct { fields, .. }) = &ast.data else {
            panic!("expected a struct")
        };

        match fields {
            syn::Fields::Named(fields) => Self::parse_struct_with_named_fields(fields),
            syn::Fields::Unnamed(fields) => Self::parse_tuple_struct(fields),
            syn::Fields::Unit => Self::unit(),
        }
    }

    fn parse_struct_with_named_fields(fields: &syn::FieldsNamed) -> Self {
        let (ignored_fields, fields): (Vec<_>, Vec<_>) = fields
            .named
            .iter()
            .cloned()
            .rev()
            .partition(Self::field_is_ignored);

        let fields = fields.into_iter();
        let field_names = fields.clone().map(|f| f.ident.unwrap()).collect::<Vec<_>>();
        let field_types = fields.map(|f| f.ty).collect::<Vec<_>>();

        let fields = field_names.iter();
        let ignored = ignored_fields.iter().map(|f| f.ident.clone());
        let struct_builder =
            quote! { Self { #( #fields ,)* #( #ignored : Default::default(), )* } };

        Self {
            field_names,
            field_types,
            ignored_fields,
            struct_builder,
        }
    }

    fn parse_tuple_struct(fields: &syn::FieldsUnnamed) -> Self {
        // for now, ignoring fields in tuple structs is unsupported
        let (field_names, field_types): (Vec<_>, Vec<_>) = fields
            .unnamed
            .iter()
            .cloned()
            .enumerate()
            .map(|(i, f)| (quote::format_ident!("field_{i}"), f.ty))
            .rev()
            .unzip();

        let fields_in_declared_order = field_names.iter().rev();
        let struct_builder = quote! { Self( #( #fields_in_declared_order ),* ) };

        Self {
            field_names,
            field_types,
            ignored_fields: vec![],
            struct_builder,
        }
    }

    fn unit() -> Self {
        Self {
            field_names: vec![],
            field_types: vec![],
            ignored_fields: vec![],
            struct_builder: quote! { Self },
        }
    }

    fn field_is_ignored(field: &syn::Field) -> bool {
        let field_name = field.ident.as_ref().unwrap();
        let mut relevant_attributes = field
            .attrs
            .iter()
            .filter(|attr| attr.path().is_ident("tasm_object"));

        let Some(attribute) = relevant_attributes.next() else {
            return false;
        };
        if relevant_attributes.next().is_some() {
            panic!("field `{field_name}` must have at most 1 `tasm_object` attribute");
        }

        let parse_ignore = attribute.parse_nested_meta(|meta| match meta.path.get_ident() {
            Some(ident) if ident == "ignore" => Ok(()),
            Some(ident) => panic!("unknown identifier `{ident}` for field `{field_name}`"),
            _ => unreachable!(),
        });
        parse_ignore.is_ok()
    }

    /// Allows writing shorter paths while staying hygienic.
    fn type_aliases() -> TokenStream {
        quote! {
            type Instruction = crate::triton_vm::isa::instruction::LabelledInstruction;
            type AssertionContext = crate::triton_vm::isa::instruction::AssertionContext;
            type TypeHint = crate::triton_vm::isa::instruction::TypeHint;

            type AnInstruction =
                crate::triton_vm::isa::instruction::AnInstruction<::std::string::String>;

            type N = crate::triton_vm::isa::op_stack::NumberOfWords;
            type ST = crate::triton_vm::isa::op_stack::OpStackElement;
            type BFE = crate::triton_vm::prelude::BFieldElement;

        }
    }

    fn generate_code_for_fn_compute_size_and_assert_valid_size_indicator(&self) -> TokenStream {
        debug_assert_eq!(self.field_types.len(), self.field_names.len());

        let mut fields = self
            .field_names
            .iter()
            .map(|n| n.to_string())
            .zip(&self.field_types);

        let type_aliases = Self::type_aliases();
        let Some((_, first_field_ty)) = fields.next_back() else {
            return quote! {
                #type_aliases

                [
                    Instruction::Instruction(AnInstruction::Pop(N::N1)),
                    Instruction::Instruction(AnInstruction::Push(BFE::new(0))),
                ]
                .to_vec()
            };
        };

        let accumulator_type_hint = quote! {
            Instruction::TypeHint(
                TypeHint {
                    starting_index: 0,
                    length: 1,
                    type_name: ::std::option::Option::None,
                    variable_name: ::std::string::String::from("size_acc"),
                }
            )
        };
        let mut rust = quote! {
            #type_aliases

            if let Some(size) =
                <Self as crate::twenty_first::math::bfield_codec::BFieldCodec>::static_length()
            {
                return [
                    Instruction::Instruction(AnInstruction::Pop(N::N1)),
                    Instruction::Instruction(AnInstruction::Push(BFE::from(size))),
                ]
                .to_vec();
            }

            // accumulates successive static lengths; minimize number of static-length jumps
            let mut static_jump_accumulator = BFE::new(0);
            let mut instructions = [
                Instruction::Instruction(AnInstruction::Push(BFE::new(0))),
                #accumulator_type_hint,
                Instruction::Instruction(AnInstruction::Place(ST::ST1)),
            ].to_vec();
        };

        for (field_name, field_ty) in fields {
            let field_ptr_hint = Self::top_of_stack_pointer_type_hint(&field_name);
            rust.extend(quote! {
                if let Some(size) =
                    <#field_ty as crate::twenty_first::math::bfield_codec::BFieldCodec>
                        ::static_length()
                {
                    static_jump_accumulator += BFE::from(size);
                } else {
                    if static_jump_accumulator != BFE::new(0) {
                        instructions.extend([
                            // _ acc_up_to_some_field *some_field
                            Instruction::Instruction(AnInstruction::AddI(static_jump_accumulator)),
                            // _ acc_up_to_some_field *field_si
                            Instruction::Instruction(AnInstruction::Pick(ST::ST1)),
                            // _ *field_si acc_up_to_some_field
                            Instruction::Instruction(AnInstruction::AddI(static_jump_accumulator)),
                            // _ *field_si acc
                            Instruction::Instruction(AnInstruction::Place(ST::ST1)),
                            // _ acc *field_si
                        ]);
                        static_jump_accumulator = BFE::new(0);
                    }

                    instructions.extend([
                        // _ acc *field_si
                        Instruction::Instruction(AnInstruction::ReadMem(N::N1)),
                        // _ acc field_size (*field_si - 1)
                        Instruction::Instruction(AnInstruction::AddI(BFE::new(2))),
                        #field_ptr_hint,
                        // _ acc field_size *field
                        Instruction::Instruction(AnInstruction::Push(BFE::from(Self::MAX_OFFSET))),
                        Instruction::Instruction(AnInstruction::Dup(ST::ST2)),
                        Instruction::Instruction(AnInstruction::Lt),
                        Instruction::Instruction(AnInstruction::Assert),
                        Instruction::AssertionContext(AssertionContext::ID(180)),
                        // _ acc field_size *field
                        Instruction::Instruction(AnInstruction::Dup(ST::ST0)),
                        // _ acc field_size *field *field
                    ]);
                    instructions.extend(
                        <#field_ty as crate::tasm_lib::structure::tasm_object::TasmObject>
                            ::compute_size_and_assert_valid_size_indicator(library)
                    );
                    instructions.extend([
                        // _ acc field_size *field computed_field_size
                        Instruction::Instruction(AnInstruction::Dup(ST::ST2)),
                        // _ acc field_size *field computed_field_size field_size
                        Instruction::Instruction(AnInstruction::Eq),
                        // _ acc field_size *field (computed_field_size == field_size)
                        Instruction::Instruction(AnInstruction::Assert),
                        Instruction::AssertionContext(AssertionContext::ID(181)),
                        // _ acc field_size *field
                        Instruction::Instruction(AnInstruction::Dup(ST::ST1)),
                        // _ acc field_size *field field_size
                        Instruction::Instruction(AnInstruction::Add),
                        // _ acc field_size *next_field_or_next_field_si
                        Instruction::Instruction(AnInstruction::Place(ST::ST2)),
                        // _ *next_field_or_next_field_si acc field_size
                        Instruction::Instruction(AnInstruction::Add),
                        // _ *next_field_or_next_field_si (acc + field_size)
                        Instruction::Instruction(AnInstruction::AddI(BFE::new(1))),
                        // _ *next_field_or_next_field_si (acc + field_size + 1)
                        Instruction::Instruction(AnInstruction::Pick(ST::ST1)),
                        // _ (acc + field_size + 1) *next_field_or_next_field_si
                    ]);
                }
            });
        }

        rust.extend(quote!(
            if let Some(size) =
                <#first_field_ty as crate::twenty_first::math::bfield_codec::BFieldCodec>
                    ::static_length()
            {
                static_jump_accumulator += BFE::from(size);
                instructions.extend([
                    // _ acc *some_field
                    Instruction::Instruction(AnInstruction::Pop(N::N1)),
                    // _ acc
                    Instruction::Instruction(AnInstruction::AddI(static_jump_accumulator)),
                    // _ final_size
                ]);
            } else {
                if static_jump_accumulator != BFE::new(0) {
                    instructions.push(Instruction::Instruction(AnInstruction::AddI(
                        static_jump_accumulator
                    )));
                }

                instructions.extend([
                    // _ acc *field_si
                    Instruction::Instruction(AnInstruction::ReadMem(N::N1)),
                    // _ acc field_size (*field_si - 1)
                    Instruction::Instruction(AnInstruction::AddI(BFE::new(2))),
                    // _ acc field_size *field
                    Instruction::Instruction(AnInstruction::Push(BFE::from(Self::MAX_OFFSET))),
                    Instruction::Instruction(AnInstruction::Dup(ST::ST2)),
                    Instruction::Instruction(AnInstruction::Lt),
                    Instruction::Instruction(AnInstruction::Assert),
                    Instruction::AssertionContext(AssertionContext::ID(180)),
                    // _ acc field_size *field
                ]);
                instructions.extend(
                    <#first_field_ty as crate::tasm_lib::structure::tasm_object::TasmObject>
                        ::compute_size_and_assert_valid_size_indicator(library)
                );
                instructions.extend([
                    // _ acc field_size computed_field_size
                    Instruction::Instruction(AnInstruction::Dup(ST::ST1)),
                    // _ acc field_size computed_field_size field_size
                    Instruction::Instruction(AnInstruction::Eq),
                    // _ acc field_size (computed_field_size == field_size)
                    Instruction::Instruction(AnInstruction::Assert),
                    Instruction::AssertionContext(AssertionContext::ID(181)),
                    // _ acc field_size
                    Instruction::Instruction(AnInstruction::Add),
                    Instruction::Instruction(AnInstruction::AddI(BFE::new(1))),
                    // _ acc
                ]);

                if static_jump_accumulator != BFE::new(0) {
                    instructions.push(Instruction::Instruction(AnInstruction::AddI(
                        static_jump_accumulator
                    )));
                }
            }

            instructions
        ));

        rust
    }

    /// Generate the rust code for `TasmStruct::get_field(…)`, which will then
    /// generate tasm code to get a field.
    ///
    /// For example, calling `TasmStruct::get_field("field_i")` will generate
    /// tasm code that
    /// - assumes the stack is in the state `_ *struct`
    /// - leaves the stack in the state `_ *field_i`
    fn generate_code_for_fn_get_field(&self, struct_name: &syn::Ident) -> TokenStream {
        debug_assert_eq!(self.field_types.len(), self.field_names.len());

        let mut fields = self
            .field_names
            .iter()
            .map(|n| n.to_string())
            .zip(&self.field_types);

        let Some(first_field) = fields.next_back() else {
            let struct_name = struct_name.to_string();
            return quote!(panic!("type `{}` has no fields", #struct_name););
        };

        let type_aliases = Self::type_aliases();
        let mut rust = quote! {
            #type_aliases
            // accumulates successive static lengths; minimize number of static-length jumps
            let mut static_jump_accumulator = BFE::new(0);
            let mut instructions = ::std::vec::Vec::new();
        };

        for (field_name, field_ty) in fields {
            let field_ptr_hint = Self::top_of_stack_pointer_type_hint(&field_name);
            rust.extend(quote!(
                if field_name == #field_name {
                    if <#field_ty as crate::twenty_first::math::bfield_codec::BFieldCodec>
                        ::static_length().is_none()
                    {
                        // shift pointer from size indicator to actual field
                        static_jump_accumulator += BFE::new(1);
                    }
                    if static_jump_accumulator != BFE::new(0) {
                        instructions.push(Instruction::Instruction(AnInstruction::AddI(
                            static_jump_accumulator
                        )));
                    }
                    instructions.push(#field_ptr_hint);
                    return instructions;
                }

                if let Some(size) =
                    <#field_ty as crate::twenty_first::math::bfield_codec::BFieldCodec>
                        ::static_length()
                {
                    static_jump_accumulator += BFE::from(size);
                } else {
                    if static_jump_accumulator != BFE::new(0) {
                        instructions.push(Instruction::Instruction(AnInstruction::AddI(
                            static_jump_accumulator
                        )));
                        static_jump_accumulator = BFE::new(0);
                    }

                    instructions.extend([
                        // _ *field_si
                        Instruction::Instruction(AnInstruction::ReadMem(N::N1)),
                        // _ field_size (*field_si - 1)
                        Instruction::Instruction(AnInstruction::AddI(BFE::new(2))),
                        #field_ptr_hint,
                        // _ field_size *field
                        Instruction::Instruction(AnInstruction::Pick(ST::ST1)),
                        // _ *field field_size
                        Instruction::Instruction(AnInstruction::Push(BFE::from(Self::MAX_OFFSET))),
                        Instruction::Instruction(AnInstruction::Dup(ST::ST1)),
                        Instruction::Instruction(AnInstruction::Lt),
                        Instruction::Instruction(AnInstruction::Assert),
                        Instruction::AssertionContext(AssertionContext::ID(184)),
                        // _ *field field_size
                        Instruction::Instruction(AnInstruction::Add),
                        // _ *next_field_or_next_field_si
                    ]);
                }
            ));
        }

        let (first_field_name, first_field_ty) = first_field;
        let struct_name = struct_name.to_string();
        let first_field_type_hint = Self::top_of_stack_pointer_type_hint(&first_field_name);
        rust.extend(quote!(
            if field_name != #first_field_name {
                panic!("unknown field name `{field_name}` for type `{}`", #struct_name);
            }
            if <#first_field_ty as crate::twenty_first::math::bfield_codec::BFieldCodec>
                ::static_length().is_none()
            {
                // shift pointer from size indicator to actual field
                static_jump_accumulator += BFE::new(1);
            }
            if static_jump_accumulator != BFE::new(0) {
                instructions.push(Instruction::Instruction(AnInstruction::AddI(
                    static_jump_accumulator
                )));
            }
            instructions.push(#first_field_type_hint);
            instructions
        ));

        rust
    }

    /// Generate the rust code for `TasmStruct::get_field_with_size(…)`, which will
    /// then generate tasm code to get a field and its size.
    ///
    /// For example, calling `TasmStruct::get_field_with_size("field_i")` will
    /// generate tasm code that
    /// - assumes the stack is in the state `_ *struct`
    /// - leaves the stack in the state `_ *field_i size_of_field_i`
    fn generate_code_for_fn_get_field_with_size(&self, struct_name: &syn::Ident) -> TokenStream {
        debug_assert_eq!(self.field_types.len(), self.field_names.len());

        let mut fields = self
            .field_names
            .iter()
            .map(|n| n.to_string())
            .zip(&self.field_types);
        let Some(first_field) = fields.next_back() else {
            let struct_name = struct_name.to_string();
            return quote!(panic!("type `{}` has no fields", #struct_name););
        };

        let type_aliases = Self::type_aliases();
        let mut rust = quote! {
            #type_aliases
            // accumulates successive static lengths; minimize number of static-length jumps
            let mut static_jump_accumulator = BFE::new(0);
            let mut instructions = ::std::vec::Vec::new();
        };

        for (field_name, field_ty) in fields {
            let field_ptr_hint = Self::top_of_stack_pointer_type_hint(&field_name);
            rust.extend(quote!(
                if field_name == #field_name {
                    if static_jump_accumulator != BFE::new(0) {
                        instructions.push(Instruction::Instruction(AnInstruction::AddI(
                            static_jump_accumulator
                        )));
                    }
                    if let Some(size) =
                        <#field_ty as crate::twenty_first::math::bfield_codec::BFieldCodec>
                            ::static_length()
                    {
                        instructions.extend([
                            #field_ptr_hint,
                            Instruction::Instruction(AnInstruction::Push(BFE::from(size))),
                        ]);
                    } else {
                        let max_offset = BFE::from(Self::MAX_OFFSET);
                        instructions.extend([
                            // _ *field_si
                            Instruction::Instruction(AnInstruction::ReadMem(N::N1)),
                            // _ field_size (*field_si - 1)
                            Instruction::Instruction(AnInstruction::AddI(BFE::new(2))),
                            #field_ptr_hint,
                            // _ field_size *field
                            Instruction::Instruction(AnInstruction::Pick(ST::ST1)),
                            // _ *field field_size
                            Instruction::Instruction(AnInstruction::Push(max_offset)),
                            Instruction::Instruction(AnInstruction::Dup(ST::ST1)),
                            Instruction::Instruction(AnInstruction::Lt),
                            Instruction::Instruction(AnInstruction::Assert),
                            Instruction::AssertionContext(AssertionContext::ID(185)),
                            // _ *field field_size
                        ]);
                    }
                    return instructions;
                }

                if let Some(size) =
                    <#field_ty as crate::twenty_first::math::bfield_codec::BFieldCodec>
                        ::static_length()
                {
                    static_jump_accumulator += BFE::from(size);
                } else {
                    if static_jump_accumulator != BFE::new(0) {
                        instructions.push(Instruction::Instruction(AnInstruction::AddI(
                            static_jump_accumulator
                        )));
                        static_jump_accumulator = BFE::new(0);
                    }

                    instructions.extend([
                        // _ *field_si
                        Instruction::Instruction(AnInstruction::ReadMem(N::N1)),
                        // _ field_size (*field_si - 1)
                        Instruction::Instruction(AnInstruction::AddI(BFE::new(2))),
                        #field_ptr_hint,
                        // _ field_size *field
                        Instruction::Instruction(AnInstruction::Pick(ST::ST1)),
                        // _ *field field_size
                        Instruction::Instruction(AnInstruction::Push(BFE::from(Self::MAX_OFFSET))),
                        Instruction::Instruction(AnInstruction::Dup(ST::ST1)),
                        Instruction::Instruction(AnInstruction::Lt),
                        Instruction::Instruction(AnInstruction::Assert),
                        Instruction::AssertionContext(AssertionContext::ID(185)),
                        // _ *field field_size
                        Instruction::Instruction(AnInstruction::Add),
                        // _ *next_field_or_next_field_si
                    ]);
                }
            ));
        }

        let (first_field_name, first_field_ty) = first_field;
        let struct_name = struct_name.to_string();
        let first_field_type_hint = Self::top_of_stack_pointer_type_hint(&first_field_name);
        rust.extend(quote!(
            if field_name != #first_field_name {
                panic!("unknown field name `{field_name}` for type `{}`", #struct_name);
            }
            if static_jump_accumulator != BFE::new(0) {
                instructions.push(Instruction::Instruction(AnInstruction::AddI(
                    static_jump_accumulator
                )));
            }
            if let Some(size) =
                <#first_field_ty as crate::twenty_first::math::bfield_codec::BFieldCodec>
                    ::static_length()
            {
                instructions.extend([
                    #first_field_type_hint,
                    Instruction::Instruction(AnInstruction::Push(BFE::from(size))),
                ]);
            } else {
                instructions.extend([
                    // _ *field_si
                    Instruction::Instruction(AnInstruction::ReadMem(N::N1)),
                    // _ field_size (*field_si - 1)
                    Instruction::Instruction(AnInstruction::AddI(BFE::new(2))),
                    #first_field_type_hint,
                    // _ field_size *field
                    Instruction::Instruction(AnInstruction::Pick(ST::ST1)),
                    // _ *field field_size
                    Instruction::Instruction(AnInstruction::Push(BFE::from(Self::MAX_OFFSET))),
                    Instruction::Instruction(AnInstruction::Dup(ST::ST1)),
                    Instruction::Instruction(AnInstruction::Lt),
                    Instruction::Instruction(AnInstruction::Assert),
                    Instruction::AssertionContext(AssertionContext::ID(185)),
                    // _ *field field_size
                ]);
            }

            instructions
        ));

        rust
    }

    /// Generate the rust code for `TasmStruct::destructure()`, which will then
    /// generate tasm code that
    /// - assumes the stack is in the state `_ *struct`
    /// - leaves the stack in the state `_ *field_n *field_(n-1) … *field_0`
    fn generate_code_for_fn_destructure(&self) -> TokenStream {
        debug_assert_eq!(self.field_types.len(), self.field_names.len());

        let mut fields = self
            .field_names
            .iter()
            .map(|n| n.to_string())
            .zip(&self.field_types);

        let type_aliases = Self::type_aliases();
        let Some(first_field) = fields.next_back() else {
            return quote! {
                #type_aliases
                [Instruction::Instruction(AnInstruction::Pop(N::N1))].to_vec()
            };
        };

        let mut rust = quote! {
            #type_aliases
            let mut instructions = ::std::vec::Vec::new();
        };

        for (field_name, field_ty) in fields {
            let field_ptr_hint = Self::top_of_stack_pointer_type_hint(&field_name);
            rust.extend(quote!(
            if let Some(size) =
                <#field_ty as crate::twenty_first::math::bfield_codec::BFieldCodec>::static_length()
            {
                instructions.extend([
                    #field_ptr_hint,
                    // _ *field
                    Instruction::Instruction(AnInstruction::Dup(ST::ST0)),
                    // _ *field *field
                    Instruction::Instruction(AnInstruction::AddI(BFE::from(size))),
                    // _ *field *next_field_or_next_field_si
                ]);
            } else {
                instructions.extend([
                    // _ *field_si
                    Instruction::Instruction(AnInstruction::ReadMem(N::N1)),
                    // _ field_size (*field_si - 1)
                    Instruction::Instruction(AnInstruction::AddI(BFE::new(2))),
                    #field_ptr_hint,
                    // _ field_size *field
                    Instruction::Instruction(AnInstruction::Dup(ST::ST0)),
                    // _ field_size *field *field
                    Instruction::Instruction(AnInstruction::Pick(ST::ST2)),
                    // _ *field *field field_size
                    Instruction::Instruction(AnInstruction::Push(BFE::from(Self::MAX_OFFSET))),
                    Instruction::Instruction(AnInstruction::Dup(ST::ST1)),
                    Instruction::Instruction(AnInstruction::Lt),
                    Instruction::Instruction(AnInstruction::Assert),
                    Instruction::AssertionContext(AssertionContext::ID(183)),
                    // _ *field *field field_size
                    Instruction::Instruction(AnInstruction::Add),
                    // _ *field *next_field_or_next_field_si
                ]);
            }
        ));
        }

        let (first_field_name, first_field_ty) = first_field;
        let first_field_type_hint = Self::top_of_stack_pointer_type_hint(&first_field_name);
        rust.extend(quote!(
            if <#first_field_ty as crate::twenty_first::math::bfield_codec::BFieldCodec>
                ::static_length().is_some()
            {
                instructions.push(#first_field_type_hint);
            } else {
                instructions.extend([
                    // _ *field_si
                    Instruction::Instruction(AnInstruction::ReadMem(N::N1)),
                    // _ field_size (*field_si - 1)
                    Instruction::Instruction(AnInstruction::AddI(BFE::new(2))),
                    #first_field_type_hint,
                    // _ field_size *field
                    Instruction::Instruction(AnInstruction::Push(BFE::from(Self::MAX_OFFSET))),
                    // _ field_size *field max_offset
                    Instruction::Instruction(AnInstruction::Pick(ST::ST2)),
                    // _ *field max_offset field_size
                    Instruction::Instruction(AnInstruction::Lt),
                    // _ *field (field_size < max_offset)
                    Instruction::Instruction(AnInstruction::Assert),
                    Instruction::AssertionContext(AssertionContext::ID(183)),
                    // _ *field
                ]);
            }
            instructions
        ));

        rust
    }

    fn top_of_stack_pointer_type_hint(field_name: &str) -> TokenStream {
        quote!(
            Instruction::TypeHint(
                TypeHint {
                    starting_index: 0,
                    length: 1,
                    type_name: ::std::option::Option::Some("Pointer".to_string()),
                    variable_name: ::std::string::String::from(#field_name),
                }
            )
        )
    }
}

fn impl_tasm_object_derive_macro(ast: DeriveInput) -> TokenStream {
    let parsed_struct = ParsedStruct::new(&ast);
    let name = &ast.ident;
    let name_as_string = ast.ident.to_string();

    let (impl_generics, ty_generics, where_clause) = ast.generics.split_for_impl();
    let ignored_field_types = parsed_struct.ignored_fields.iter().map(|f| f.ty.clone());
    let new_where_clause = if let Some(old_where_clause) = where_clause {
        quote! { #old_where_clause #(#ignored_field_types : Default ),* }
    } else {
        quote! { where #(#ignored_field_types : Default ),* }
    };

    let field_decoders = parsed_struct
        .field_names
        .iter()
        .cloned()
        .zip(parsed_struct.field_types.iter().cloned())
        .map(|(field_name, field_ty)| field_decoder(field_name, field_ty));
    let struct_builder = parsed_struct.struct_builder.clone();

    let code_for_fn_compute_size_and_assert_valid_size_indicator =
        parsed_struct.generate_code_for_fn_compute_size_and_assert_valid_size_indicator();
    let code_for_fn_get_field = parsed_struct.generate_code_for_fn_get_field(&ast.ident);
    let code_for_fn_get_field_with_size =
        parsed_struct.generate_code_for_fn_get_field_with_size(&ast.ident);
    let code_for_fn_destructure = parsed_struct.generate_code_for_fn_destructure();

    quote! {
        impl #impl_generics crate::tasm_lib::structure::tasm_object::TasmObject
        for #name #ty_generics #new_where_clause {
            fn label_friendly_name() -> String {
                #name_as_string.to_owned()
            }

            fn compute_size_and_assert_valid_size_indicator(
                library: &mut crate::tasm_lib::library::Library
            ) -> ::std::vec::Vec<crate::triton_vm::isa::instruction::LabelledInstruction> {
                #code_for_fn_compute_size_and_assert_valid_size_indicator
            }

            fn decode_iter<Itr: Iterator<Item = crate::triton_vm::prelude::BFieldElement>>(
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
                ::std::result::Result::Ok(::std::boxed::Box::new(#struct_builder))
            }
        }

        impl #impl_generics crate::tasm_lib::structure::tasm_object::TasmStruct
        for #name #ty_generics #new_where_clause {
            fn get_field(
                field_name: &str
            ) -> ::std::vec::Vec<crate::triton_vm::isa::instruction::LabelledInstruction> {
                #code_for_fn_get_field
            }

            fn get_field_with_size(
                field_name: &str
            ) -> ::std::vec::Vec<crate::triton_vm::isa::instruction::LabelledInstruction> {
                #code_for_fn_get_field_with_size
            }

            fn destructure(
            ) -> ::std::vec::Vec<crate::triton_vm::isa::instruction::LabelledInstruction> {
                #code_for_fn_destructure
            }
        }
    }
}

fn field_decoder(field_name: syn::Ident, field_type: syn::Type) -> TokenStream {
    quote! {
        let length = if let Some(static_length) =
            <#field_type as crate::twenty_first::math::bfield_codec::BFieldCodec>::static_length()
        {
            static_length
        } else {
            iterator.next().ok_or("iterator exhausted")?.try_into()?
        };
        let sequence = (0..length)
            .map(|_| iterator.next())
            .collect::<::std::option::Option<::std::vec::Vec<_>>>()
            .ok_or("iterator exhausted")?;
        let #field_name : #field_type =
            *crate::twenty_first::math::bfield_codec::BFieldCodec::decode(&sequence)?;
    }
}

#[cfg(test)]
mod tests {
    use syn::parse_quote;

    use super::*;

    #[test]
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

    #[test]
    fn where_clause_with_trailing_comma() {
        let ast = parse_quote! {
            #[derive(BFieldCodec, TasmObject)]
            struct Foo<T>
            where
                T: BFieldCodec, { }
            //                ^
            //               this trailing comma
        };
        let rust_code = impl_tasm_object_derive_macro(ast);
        println!("{}", prettyplease::unparse(&parse_quote!(#rust_code)));
    }

    #[test]
    fn where_clause_with_trailing_comma_and_ignored_field() {
        let ast = parse_quote! {
            #[derive(BFieldCodec, TasmObject)]
            struct Foo<S, T>
            where
                T: BFieldCodec, // <- this trailing comma
            {
                #[tasm_object(ignore)]
                s: S,
            }
        };
        let rust_code = impl_tasm_object_derive_macro(ast);
        println!("{}", prettyplease::unparse(&parse_quote!(#rust_code)));
    }
}
