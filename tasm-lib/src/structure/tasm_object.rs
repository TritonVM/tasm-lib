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
    /// Returns tasm code that returns a pointer the field of the object, assuming:
    ///  - that a pointer to the said object lives on top of the stack;
    ///  - said object has a type that implements the TasmObject trait;
    ///  - said object lives in memory endoded as BFieldCodec specifies.
    ///
    /// BEFORE: _ *object
    ///
    /// AFTER: _ *field
    fn get_field(field_name: &str) -> Vec<LabelledInstruction>;

    /// Returns tasm code that returns a pointer the field of the object, along with
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

    /// Returns tasm code that returns a pointer to the start of the field of the object,
    /// along with the jump distance to the next field. Note that:
    ///
    ///  -  *field_start == *field      if the size is statically known, but
    ///  -  *field_start == *field-1    if the size is not statically known.
    ///
    /// BEFORE: _ *object
    ///
    /// AFTER: _ *field_start field_jump_distance
    ///
    /// This function is used internally for the derive macro. You probably want to use
    /// `get_field` or `get_field_with_size` instead.
    fn get_field_start_with_jump_distance(field_name: &str) -> Vec<LabelledInstruction>;
}

pub trait TasmObjectFieldName {
    fn tasm_object_field_name(&self) -> String;
}

impl TasmObjectFieldName for &str {
    fn tasm_object_field_name(&self) -> String {
        self.to_string()
    }
}

impl TasmObjectFieldName for i32 {
    fn tasm_object_field_name(&self) -> String {
        format!("field_{}", self)
    }
}

/// Convenience macro, so that we don't have to write
/// ```
/// let field_f = <StructWithNamedFields as TasmObject>::get_field("f");
/// let field_0 = <StructWithUnnamedFields as TasmObject>::get_field("field_0");
/// ```
/// but instead
/// ```
/// let field_f = tasm_object::field!(StructWithNamedFields::f);
/// let field_0 = tasm_object::field!(StructWithUnnamedFields::0);
/// ```
/// and for numbered fields.
#[macro_export]
macro_rules! field {
    { $o : ident :: $e : ident } => {
        <$o as $crate::structure::tasm_object::TasmObject>
            ::get_field(& $crate::structure::tasm_object::TasmObjectFieldName::tasm_object_field_name(&stringify!($e))
        )
    };
    { $o : ident :: $e : expr } => {
        <$o as $crate::structure::tasm_object::TasmObject>
            ::get_field(& $crate::structure::tasm_object::TasmObjectFieldName::tasm_object_field_name(&$e)
        )
    };
}

/// Convenience macro, so that we don't have to write
/// ```
/// let field_f = <StructWithNamedFields as TasmObject>::get_field_with_size("f");
/// let field_0 = <StructWithUnnamedFields as TasmObject>::get_field_with_size("field_0");
/// ```
/// but instead
/// ```
/// let field_f = tasm_object::field_with_size!(StructWithNamedFields::f);
/// let field_0 = tasm_object::field_with_size!(StructWithUnnamedFields::0);
/// ```
/// and for numbered fields.
#[macro_export]
macro_rules! field_with_size {
    { $o : ident :: $e : ident } => {
        <$o as $crate::structure::tasm_object::TasmObject>
            ::get_field_with_size(
                & $crate::structure::tasm_object::TasmObjectFieldName::tasm_object_field_name(&stringify!($e))
            )
    };
    { $o : ident :: $e : expr } => {
        <$o as $crate::structure::tasm_object::TasmObject>
            ::get_field_with_size(
                & $crate::structure::tasm_object::TasmObjectFieldName::tasm_object_field_name(&$e)
            )
    };
}

#[cfg(test)]
mod test {
    use std::collections::HashMap;

    use itertools::Itertools;
    use rand::{rngs::StdRng, Rng, SeedableRng};
    use triton_vm::{instruction::LabelledInstructions, triton_asm, BFieldElement};
    use twenty_first::shared_math::{bfield_codec::BFieldCodec, x_field_element::XFieldElement};

    use crate::{
        get_init_tvm_stack, io, memory,
        snippet::{DataType, InputSource, Snippet},
        structure::tasm_object::TasmObject,
        test_helpers::test_rust_equivalence_multiple,
        ExecutionState,
    };

    #[derive(BFieldCodec)]
    enum InnerEnum {
        Cow(u32),
        Horse(u128),
        Pig(XFieldElement),
        Sheep([BFieldElement; 13]),
    }

    #[derive(BFieldCodec, TasmObject)]
    struct InnerStruct(XFieldElement, u32);

    #[derive(BFieldCodec, TasmObject)]
    struct OuterStruct {
        o: InnerEnum,
        a: Vec<Option<bool>>,
        b: InnerStruct,
        p: InnerEnum,
        c: BFieldElement,
    }

    fn pseudorandom_object(seed: [u8; 32]) -> OuterStruct {
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
            o: InnerEnum::Pig(XFieldElement::new_const(443u64.into())),
            a,
            b: InnerStruct(b0, b1),
            p: InnerEnum::Cow(999),
            c,
        }
    }

    struct TestObjectFieldGetter;

    impl Snippet for TestObjectFieldGetter {
        fn entrypoint(&self) -> String {
            "tasm_test_object_field_getter".to_string()
        }

        fn inputs(&self) -> Vec<String> {
            vec!["object".to_string()]
        }

        fn input_types(&self) -> Vec<crate::snippet::DataType> {
            vec![DataType::VoidPointer]
        }

        fn output_types(&self) -> Vec<crate::snippet::DataType> {
            vec![]
        }

        fn outputs(&self) -> Vec<String> {
            vec![]
        }

        fn stack_diff(&self) -> isize {
            -1
        }

        fn function_code(&self, library: &mut crate::library::Library) -> String {
            let entrypoint = self.entrypoint();
            let object_to_a_with_size = field_with_size!(OuterStruct::a);
            let object_to_b = field!(OuterStruct::b);
            let object_to_c_with_size = field_with_size!(OuterStruct::c);
            let object_to_p_with_size = field_with_size!(OuterStruct::p);
            let b_to_0_with_size = field_with_size!(InnerStruct::0);
            let b_to_1_with_size = field_with_size!(InnerStruct::1);
            let memcpy = library.import(Box::new(memory::memcpy::MemCpy));
            let load_object_from_stdin =
                library.import(Box::new(io::load_struct_from_input::LoadStructFromInput {
                    input_source: InputSource::StdIn,
                }));

            let code = triton_asm! {
                // BEFORE: 1
                // AFTER:
                {entrypoint}:
                    call {load_object_from_stdin} // *object_length
                    push 1 add // *object

                    dup 0 // *object *object
                    {&object_to_a_with_size} // *object *a a_size
                    push 1337
                    swap 1 // *object *a 1337 a_size
                    call {memcpy} // *object

                    dup 0 // *object *object
                    {&object_to_b.clone()}
                    {&b_to_0_with_size}
                    push 2337
                    swap 1 // *object *b.0 2337 *b.0_size
                    call {memcpy}

                    dup 0
                    {&object_to_b}
                    {&b_to_1_with_size}
                    push 3337
                    swap 1 // *object *b.1 3337 *b.1_size
                    call {memcpy}

                    dup 0
                    {&object_to_c_with_size} // *object *c c_size
                    push 4337
                    swap 1 // *object *c 4337 c_size
                    call {memcpy} // *object

                    dup 0
                    {&object_to_p_with_size} // *object *p p_size
                    push 6337
                    swap 1 // *object *p 6337 p_size
                    call {memcpy} // *object

                    pop
                    return
            };

            LabelledInstructions(code).to_string()
        }

        fn crash_conditions(&self) -> Vec<String> {
            vec![]
        }

        fn gen_input_states(&self) -> Vec<ExecutionState> {
            let mut seed = [0u8; 32];
            seed[0] = 1;
            seed[1] = 2;
            let mut rng: StdRng = SeedableRng::from_seed(seed);
            let mut stack = get_init_tvm_stack();
            stack.push(BFieldElement::new(1u64));
            let memory = HashMap::<BFieldElement, BFieldElement>::new();
            let mut input_states = vec![];
            for _ in 0..1 {
                let object = pseudorandom_object(rng.gen());
                let std_in = object.encode().encode();
                println!("object, encoded twice: {}", std_in.iter().join(","));
                println!("length of object encoding: {}", object.encode().len());
                let secret_in = vec![];
                let input_state = ExecutionState {
                    stack: stack.clone(),
                    std_in,
                    secret_in,
                    memory: memory.clone(),
                    words_allocated: 1,
                };
                input_states.push(input_state);
            }

            input_states
        }

        fn common_case_input_state(&self) -> ExecutionState {
            let mut seed = [0u8; 32];
            seed[0] = 1;
            seed[1] = 2;
            let mut rng: StdRng = SeedableRng::from_seed(seed);
            let mut stack = get_init_tvm_stack();
            stack.push(BFieldElement::new(1u64));
            let memory = HashMap::<BFieldElement, BFieldElement>::new();
            let object = pseudorandom_object(rng.gen());
            let std_in = object.encode().encode();
            let secret_in = vec![];

            ExecutionState {
                stack,
                std_in,
                secret_in,
                memory,
                words_allocated: 1,
            }
        }

        fn worst_case_input_state(&self) -> ExecutionState {
            let mut seed = [0u8; 32];
            seed[0] = 1;
            seed[1] = 2;
            let mut rng: StdRng = SeedableRng::from_seed(seed);
            let mut stack = get_init_tvm_stack();
            stack.push(BFieldElement::new(1u64));
            let memory = HashMap::<BFieldElement, BFieldElement>::new();
            let object = pseudorandom_object(rng.gen());
            let std_in = object.encode().encode();
            let secret_in = vec![];

            ExecutionState {
                stack,
                std_in,
                secret_in,
                memory,
                words_allocated: 1,
            }
        }

        fn rust_shadowing(
            &self,
            stack: &mut Vec<BFieldElement>,
            std_in: Vec<BFieldElement>,
            _secret_in: Vec<BFieldElement>,
            memory: &mut std::collections::HashMap<BFieldElement, BFieldElement>,
        ) {
            println!("standard in: {}", std_in.iter().join(","));
            let _num_fields = stack.pop().unwrap().value() as usize;
            for (i, o) in std_in.iter().enumerate() {
                memory.insert(BFieldElement::new(1u64 + i as u64), *o);
            }

            let object = *OuterStruct::decode(&std_in[1..]).unwrap();

            println!("object a encoding length: {}", object.a.encode().len());

            for (i, o) in object.a.encode().iter().enumerate() {
                memory.insert(BFieldElement::new(1337u64 + i as u64), *o);
            }

            for (i, o) in object.b.0.encode().iter().enumerate() {
                memory.insert(BFieldElement::new(2337u64 + i as u64), *o);
            }

            for (i, o) in object.b.1.encode().iter().enumerate() {
                memory.insert(BFieldElement::new(3337u64 + i as u64), *o);
            }

            for (i, o) in object.c.encode().iter().enumerate() {
                memory.insert(BFieldElement::new(4337u64 + i as u64), *o);
            }

            for (i, p) in object.p.encode().iter().enumerate() {
                memory.insert(BFieldElement::new(6337u64 + i as u64), *p);
            }
        }
    }

    #[test]
    fn test_tasm_object_field_getter() {
        test_rust_equivalence_multiple(&TestObjectFieldGetter, false);
    }
}
