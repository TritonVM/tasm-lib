use std::collections::HashMap;
use std::error::Error;

use itertools::Itertools;
use num_traits::Zero;
use triton_vm::prelude::*;

pub use derive_tasm_object::TasmObject;
use triton_vm::twenty_first::error::BFieldCodecError;

type Result<T> = std::result::Result<T, Box<dyn Error + Send + Sync>>;

pub const DEFAULT_MAX_DYN_FIELD_SIZE: u32 = 1u32 << 28;

/// This trait defines methods for dealing with custom-defined objects from within the VM,
/// assuming those methods live in memory as they are encoded with [`BFieldCodec`].
///
/// The arguments referring to fields are strings. For structs with unnamed fields, the
/// nth field name is implicitly `field_n`.
pub trait TasmObject {
    /// Maximum jump distance for encoded size and length indicators.
    /// The field getters will compare any length or size indicator read
    /// from memory against this value and crash the VM if the indicator
    /// is larger or equal.
    const MAX_OFFSET: u32 = DEFAULT_MAX_DYN_FIELD_SIZE;

    /// Returns tasm code that returns a pointer the field of the object, assuming:
    ///  - that a pointer to the said object lives on top of the stack;
    ///  - said object has a type that implements the TasmObject trait;
    ///  - said object lives in memory encoded as BFieldCodec specifies.
    ///
    /// ```text
    /// BEFORE: _ *object
    /// AFTER:  _ *field
    /// ```
    fn get_field(field_name: &str) -> Vec<LabelledInstruction>;

    /// Returns tasm code that returns a pointer the field of the object, along with
    /// the size of that field in number of BFieldElements, assuming:
    ///  - that a pointer to the said object lives on top of the stack;
    ///  - said object has a type that implements the TasmObject trait;
    ///  - said object lives in memory encoded as [`BFieldCodec`] specifies.
    ///
    /// ```text
    /// BEFORE: _ *object
    /// AFTER:  _ *field field_size
    ///```
    ///
    /// See also: `get_field` if you just want the field without the size.
    fn get_field_with_size(field_name: &str) -> Vec<LabelledInstruction>;

    /// Returns tasm code that returns a pointer to the start of the field of the object,
    /// along with the jump distance to the next field. Note that:
    ///
    ///  -  *field_start == *field      if the size is statically known, but
    ///  -  *field_start == *field-1    if the size is not statically known.
    ///
    /// ```text
    /// BEFORE: _ *object
    /// AFTER:  _ *field_start field_jump_distance
    /// ```
    ///
    /// This function is used internally for the derive macro. You probably want to use
    /// [`get_field`](TasmObject::get_field) or
    /// [`get_field_with_size`](TasmObject::get_field_with_size) instead.
    fn get_field_start_with_jump_distance(field_name: &str) -> Vec<LabelledInstruction>;

    /// Returns tasm code that computes the length of the encoded object given a
    /// pointer to it.
    ///
    /// ```text
    /// BEFORE: _ *object
    /// AFTER: _ size
    /// ```
    fn get_encoding_length() -> Vec<LabelledInstruction>;

    /// Decode as [`Self`].
    fn decode_iter<Itr: Iterator<Item = BFieldElement>>(iterator: &mut Itr) -> Result<Box<Self>>;

    fn decode_from_memory(
        memory: &HashMap<BFieldElement, BFieldElement>,
        address: BFieldElement,
    ) -> Result<Box<Self>> {
        let mut iterator = MemoryIter::new(memory, address);
        Self::decode_iter(&mut iterator)
    }
}

pub fn decode_from_memory_with_size<T: BFieldCodec>(
    memory: &HashMap<BFieldElement, BFieldElement>,
    address: BFieldElement,
    size: usize,
) -> Result<Box<T>> {
    let sequence = (0..size)
        .map(|i| address + BFieldElement::new(i as u64))
        .map(|b| memory.get(&b).copied().unwrap_or(BFieldElement::new(0)))
        .collect_vec();
    T::decode(&sequence).map_err(|e| e.into())
}

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
}

/// Convenience struct for converting between string literals and field name identifiers.
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
/// ```ignore
/// let field_f = <StructWithNamedFields as TasmObject>::get_field!("f");
/// let field_0 = <StructWithUnnamedFields as TasmObject>::get_field!("field_0");
/// ```
/// but instead
/// ```ignore
/// let field_f = field!(StructWithNamedFields::f);
/// let field_0 = field!(StructWithUnnamedFields::0);
/// ```
/// .
///
/// **Limitations** The type descriptor cannot have generic type arguments. To get around
/// this, define a new type via `type Custom = Generic<T>` and use that instead.
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
/// ```ignore
/// let field_f = <StructWithNamedFields as TasmObject>::get_field_with_size!("f");
/// let field_0 = <StructWithUnnamedFields as TasmObject>::get_field_with_size!("field_0");
/// ```
/// but instead
/// ```ignore
/// let field_f = field_with_size!(StructWithNamedFields::f);
/// let field_0 = field_with_size!(StructWithUnnamedFields::0);
/// ```
/// and for numbered fields.
///
/// **Limitations** The type descriptor cannot have generic type arguments. To get around
/// this, define a new type via `type Custom = Generic<T>` and use that instead.
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

/// Turns a memory, represented as a `HashMap` from `BFieldElement`s to `BFieldElement`s,
/// along with a starting address, into an iterator over `BFieldElement`s.
#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub struct MemoryIter<'a> {
    memory: &'a HashMap<BFieldElement, BFieldElement>,
    address: BFieldElement,
}

impl<'a> MemoryIter<'a> {
    pub fn new(memory: &'a HashMap<BFieldElement, BFieldElement>, address: BFieldElement) -> Self {
        Self { memory, address }
    }
}

impl<'a> Iterator for MemoryIter<'a> {
    type Item = BFieldElement;

    fn next(&mut self) -> Option<Self::Item> {
        let element = self
            .memory
            .get(&self.address)
            .copied()
            .unwrap_or(BFieldElement::zero());
        self.address.increment();
        Some(element)
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
}

#[cfg(test)]
mod test {
    use arbitrary::Arbitrary;
    use arbitrary::Unstructured;
    use rand::prelude::*;
    use triton_vm::proof_item::FriResponse;

    use crate::data_type::DataType;
    use crate::empty_stack;
    use crate::execute_with_terminal_state;
    use crate::library::Library;
    use crate::list::length::Length;
    use crate::memory::encode_to_memory;
    use crate::memory::FIRST_NON_DETERMINISTICALLY_INITIALIZED_MEMORY_ADDRESS;
    use crate::structure::tasm_object::TasmObject;
    use crate::Digest;

    use super::*;

    #[test]
    fn test_load_and_decode_from_memory() {
        #[derive(Debug, Clone, PartialEq, Eq, BFieldCodec)]
        enum InnerEnum {
            Cow(u32),
            Horse(u128, u128),
            Pig(XFieldElement),
            Sheep([BFieldElement; 13]),
        }

        #[derive(Debug, Clone, PartialEq, Eq, BFieldCodec, TasmObject)]
        struct InnerStruct(XFieldElement, u32);

        #[derive(Debug, Clone, PartialEq, Eq, BFieldCodec, TasmObject)]
        struct OuterStruct {
            o: InnerEnum,
            a: Vec<Option<bool>>,
            b: InnerStruct,
            p: InnerEnum,
            c: BFieldElement,
            l: InnerEnum,
        }

        fn pseudorandom_object(seed: [u8; 32]) -> OuterStruct {
            let mut rng: StdRng = SeedableRng::from_seed(seed);
            let a = (0..19)
                .map(|_| if rng.gen() { Some(rng.gen()) } else { None })
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
                l: InnerEnum::Horse(1 << 99, 1 << 108),
            }
        }

        let mut rng = thread_rng();
        let mut memory: HashMap<BFieldElement, BFieldElement> = HashMap::new();

        let object = pseudorandom_object(rng.gen());
        let address = rng.gen();
        encode_to_memory(&mut memory, address, &object);
        let object_again: OuterStruct = *OuterStruct::decode_from_memory(&memory, address).unwrap();
        assert_eq!(object, object_again);
    }

    /// Test derivation of field getters and manual derivations of the `field!` macro
    mod derive_tests {
        use num_traits::ConstZero;

        use super::*;

        #[test]
        fn load_and_decode_struct_with_named_fields_from_memory() {
            #[derive(BFieldCodec, TasmObject, PartialEq, Eq, Clone, Debug, Arbitrary)]
            struct NamedFields {
                a: Digest,
                b: BFieldElement,
                c: u128,
                d: Vec<Digest>,
                e: XFieldElement,
                f: Vec<u32>,
            }

            let mut randomness = [0u8; 100000];
            thread_rng().fill_bytes(&mut randomness);
            let mut unstructured = Unstructured::new(&randomness);
            let random_object = NamedFields::arbitrary(&mut unstructured).unwrap();
            let random_address: u64 = thread_rng().gen_range(0..(1 << 30));
            let address = random_address.into();
            let mut memory: HashMap<BFieldElement, BFieldElement> = HashMap::new();

            encode_to_memory(&mut memory, address, &random_object);
            let object_again: NamedFields =
                *NamedFields::decode_from_memory(&memory, address).unwrap();
            assert_eq!(random_object, object_again);

            let mut library = Library::new();
            let length_d = library.import(Box::new(Length {
                element_type: DataType::Digest,
            }));
            let length_f = library.import(Box::new(Length {
                element_type: DataType::U32,
            }));
            let code = triton_asm! {
                    // _ *obj
                    dup 0 {&field!(NamedFields::d)}

                    // _ *obj *d
                    swap 1
                   {&field!(NamedFields::f)}
                    // _ *d *f

                    call {length_f}
                    // _ *d f_length

                    swap 1
                    call {length_d}
                    // _ f_length d_length
            };

            let mut stack = get_final_stack(&random_object, library, code);
            let extracted_d_length = stack.pop().unwrap().value() as usize;
            let extracted_f_length = stack.pop().unwrap().value() as usize;

            assert_eq!(random_object.d.len(), extracted_d_length);
            assert_eq!(random_object.f.len(), extracted_f_length);
        }

        #[derive(BFieldCodec, PartialEq, Eq, Clone, Debug, Arbitrary)]
        enum MyEnum {
            A(u64, Digest),
            B,
            C,
        }

        #[derive(BFieldCodec, TasmObject, PartialEq, Eq, Clone, Debug, Arbitrary)]
        struct TupleStruct(
            Vec<XFieldElement>,
            MyEnum,
            u32,
            Vec<Digest>,
            Digest,
            Vec<BFieldElement>,
            Digest,
        );

        fn prepare_random_object(seed: [u8; 32]) -> TupleStruct {
            let mut rng: StdRng = SeedableRng::from_seed(seed);
            let mut randomness = [0u8; 100000];
            rng.fill_bytes(&mut randomness);
            let mut unstructured = Unstructured::new(&randomness);
            TupleStruct::arbitrary(&mut unstructured).unwrap()
        }

        /// Verify correct field-getter behavior when a size-indicator gets
        /// manipulated to illegal values.
        fn prop_negative_test_messed_up_size_indicators<T: BFieldCodec>(
            program: &Program,
            tuple_struct: &T,
            obj_pointer: BFieldElement,
            offset_for_manipulated_si: BFieldElement,
            expected_stack: &[BFieldElement],
        ) {
            // No-messed works
            let mut no_messed_memory = HashMap::new();
            encode_to_memory(&mut no_messed_memory, obj_pointer, tuple_struct);
            let no_messed_nd = NonDeterminism::default().with_ram(no_messed_memory.clone());
            let mut vm_state_pass =
                VMState::new(program, PublicInput::default(), no_messed_nd.clone());
            vm_state_pass.run().unwrap();

            let expected_output_len = expected_stack.len();
            let actual_stack = (0..expected_output_len)
                .map(|i| vm_state_pass.op_stack[i])
                .collect_vec();
            assert_eq!(expected_stack, actual_stack);

            // Messed-up encoding fails: Too big but still u32
            let mut messed_up_memory = no_messed_memory.clone();
            messed_up_memory.insert(
                obj_pointer + offset_for_manipulated_si,
                bfe!(TupleStruct::MAX_OFFSET + 1),
            );
            let messed_up_nd_0 = NonDeterminism::default().with_ram(messed_up_memory.clone());
            let mut vm_state_fail0 =
                VMState::new(program, PublicInput::default(), messed_up_nd_0.clone());
            let instruction_error = vm_state_fail0.run().unwrap_err();
            assert_eq!(InstructionError::AssertionFailed, instruction_error,);

            // Messed-up encoding fails: Negative sizes banned
            let negative_number = bfe!(-42);
            messed_up_memory = no_messed_memory;
            messed_up_memory.insert(obj_pointer + offset_for_manipulated_si, negative_number);
            let messed_up_nd_1 = NonDeterminism::default().with_ram(messed_up_memory.clone());
            let mut vm_state_fail1 =
                VMState::new(program, PublicInput::default(), messed_up_nd_1.clone());
            let instruction_error = vm_state_fail1.run().unwrap_err();
            assert_eq!(
                InstructionError::FailedU32Conversion(negative_number),
                instruction_error,
            );
        }

        #[test]
        fn mess_with_size_indicator_field_getter_named_fields_negative_test() {
            #[derive(BFieldCodec, TasmObject, PartialEq, Eq, Clone, Debug, Arbitrary)]
            struct WithNamedFields {
                a: Vec<Digest>,
                b: Vec<BFieldElement>,
                c: Digest,
                d: Vec<XFieldElement>,
            }

            fn prepare_random_object(seed: [u8; 32]) -> WithNamedFields {
                let mut rng: StdRng = SeedableRng::from_seed(seed);
                let mut randomness = [0u8; 100000];
                rng.fill_bytes(&mut randomness);
                let mut unstructured = Unstructured::new(&randomness);
                WithNamedFields::arbitrary(&mut unstructured).unwrap()
            }

            const START_OF_OBJ: BFieldElement = BFieldElement::new(800);
            let random_object = prepare_random_object(random());
            let third_to_last_field = field!(WithNamedFields::c);
            let code_using_field_getter = triton_asm!(
                // _

                push {START_OF_OBJ}
                // _ *with_named_fields

                {&third_to_last_field}
                // _ *digest

                addi {Digest::LEN - 1}
                read_mem {Digest::LEN}
                pop 1
                // _ [digest]

                halt
            );

            let program = Program::new(&code_using_field_getter);
            let expected_stack_benign = random_object.c.values();
            let offset_for_manipulated_si = bfe!(0);
            prop_negative_test_messed_up_size_indicators(
                &program,
                &random_object,
                START_OF_OBJ,
                offset_for_manipulated_si,
                &expected_stack_benign,
            );
        }

        #[test]
        fn mess_with_size_indicators_total_size_negative_test() {
            const START_OF_OBJ: BFieldElement = BFieldElement::ZERO;
            let random_object = prepare_random_object(random());
            let get_encoding_length = TupleStruct::get_encoding_length();
            let code_using_total_length_getter = triton_asm!(
                // _
                push { START_OF_OBJ }
                // _ *tuple_struct

                {&get_encoding_length}
                // _ total_len

                halt
            );

            let program = Program::new(&code_using_total_length_getter);
            let expected_stack_benign_nd = [bfe!(random_object.encode().len() as u64)];
            prop_negative_test_messed_up_size_indicators(
                &program,
                &random_object,
                START_OF_OBJ,
                bfe!(Digest::LEN as u64),
                &expected_stack_benign_nd,
            );
        }

        #[test]
        fn mess_with_size_indicators_field_and_size_getter_negative_test() {
            const START_OF_OBJ: BFieldElement = BFieldElement::ZERO;
            let random_object = prepare_random_object(random());
            let fourth_to_last_field = field_with_size!(TupleStruct::field_3);
            let code_using_field_and_size_getter = triton_asm!(
                // _
                push { START_OF_OBJ }
                // _ *tuple_struct

                {&fourth_to_last_field}
                // _ *digests digests_size

                swap 1
                // _ digests_size *digests

                read_mem 1
                pop 1
                // _ digests_size digests_len

                halt
            );

            let program = Program::new(&code_using_field_and_size_getter);
            let expected_field_size = bfe!(random_object.3.len() as u64 * Digest::LEN as u64 + 1);
            let expected_list_len = bfe!(random_object.3.len() as u64);
            let expected_stack_benign_nd = [expected_list_len, expected_field_size];
            prop_negative_test_messed_up_size_indicators(
                &program,
                &random_object,
                START_OF_OBJ,
                bfe!(Digest::LEN as u64),
                &expected_stack_benign_nd,
            );
        }

        #[test]
        fn mess_with_size_indicators_field_getter_negative_test() {
            const START_OF_OBJ: BFieldElement = BFieldElement::ZERO;
            let random_object = prepare_random_object(random());
            let third_to_last_field = field!(TupleStruct::field_4);
            let code_using_field_getter = triton_asm!(
                // _

                push {START_OF_OBJ}
                // _ *tuple_struct

                {&third_to_last_field}
                // _ *digest

                addi {Digest::LEN - 1}
                read_mem {Digest::LEN}
                pop 1
                // _ [digest]

                halt
            );

            let program = Program::new(&code_using_field_getter);
            let expected_output_benign_nd = random_object.4.values();
            prop_negative_test_messed_up_size_indicators(
                &program,
                &random_object,
                START_OF_OBJ,
                bfe!(Digest::LEN as u64),
                &expected_output_benign_nd,
            );
        }

        #[test]
        fn load_and_decode_tuple_struct_containing_enums_from_memory() {
            let random_object = prepare_random_object(random());
            let random_address: u64 = thread_rng().gen_range(0..(1 << 30));
            let address = random_address.into();

            let mut memory: HashMap<BFieldElement, BFieldElement> = HashMap::new();
            encode_to_memory(&mut memory, address, &random_object);
            let object_again: TupleStruct =
                *TupleStruct::decode_from_memory(&memory, address).unwrap();
            assert_eq!(random_object, object_again);

            // code snippet to access object's fields
            let mut library = Library::new();
            let length_digests = library.import(Box::new(Length {
                element_type: DataType::Digest,
            }));
            let length_bfes = library.import(Box::new(Length {
                element_type: DataType::Bfe,
            }));
            let length_xfes = library.import(Box::new(Length {
                element_type: DataType::Xfe,
            }));
            let code_for_list_lengths = triton_asm! {
                // _ *obj

                dup 0
                {&field!(TupleStruct::3)} // _ *obj *digests
                swap 1                    // _ *digests *obj

                dup 0
                {&field!(TupleStruct::5)} // _ *digests *obj *bfes
                swap 1                    // _ *digests *bfes *obj

                {&field!(TupleStruct::0)} // _ *digests *bfes *xfes
                call {length_xfes}     // _ *digests *bfes xfe_count
                swap 2                 // _ xfe_count *bfes *digests
                call {length_digests}  // _ xfe_count *bfes digest_count
                swap 1
                call {length_bfes}     // _ xfe_count digest_count bfe_count
            };

            // extract list lengths
            let mut stack = get_final_stack(&random_object, library, code_for_list_lengths);
            let extracted_bfe_count = stack.pop().unwrap().value() as usize;
            let extracted_digest_count = stack.pop().unwrap().value() as usize;
            let extracted_xfe_count = stack.pop().unwrap().value() as usize;

            // assert correct lengths
            assert_eq!(random_object.3.len(), extracted_digest_count);
            assert_eq!(random_object.5.len(), extracted_bfe_count);
            assert_eq!(random_object.0.len(), extracted_xfe_count);

            // code snippet to get encoding length
            let code_for_encoding_length = triton_asm! {
                // _ *obj
                {&TupleStruct::get_encoding_length()}
                // _ len
            };

            println!(
                "encoding length code:\n{}",
                code_for_encoding_length.iter().join("\n")
            );

            // extract length
            stack = get_final_stack(&random_object, Library::new(), code_for_encoding_length);
            let computed_length = stack.pop().unwrap().value() as usize;
            assert_eq!(random_object.encode().len(), computed_length);
        }

        #[test]
        fn test_fri_response() {
            let mut rng = thread_rng();
            let num_digests = 50;
            let num_leafs = 20;

            // generate object
            let authentication_structure =
                (0..num_digests).map(|_| rng.gen::<Digest>()).collect_vec();
            let revealed_leafs = (0..num_leafs)
                .map(|_| rng.gen::<XFieldElement>())
                .collect_vec();
            let fri_response = FriResponse {
                auth_structure: authentication_structure,
                revealed_leaves: revealed_leafs,
            };

            // code snippet to access object's fields
            let mut library = Library::new();
            let get_authentication_structure = field!(FriResponse::auth_structure);
            let length_digests = library.import(Box::new(Length {
                element_type: DataType::Digest,
            }));
            let get_revealed_leafs = field!(FriResponse::revealed_leaves);
            let length_xfes = library.import(Box::new(Length {
                element_type: DataType::Xfe,
            }));
            let code = triton_asm! {
                // _ *fri_response
                dup 0 // _ *fri_response *fri_response

                {&get_authentication_structure} // _ *fri_response *authentication_structure
                swap 1                          // _ *authentication_structure *fri_response
                {&get_revealed_leafs}           // _ *authentication_structure *revealed_leafs

                swap 1                          // _ *revealed_leafs *authentication_structure
                call {length_digests}           // _ *revealed_leafs num_digests
                swap 1                          // _ num_digests *revealed_leafs
                call {length_xfes}              // _ num_digests num_leafs

            };

            // extract list lengths
            let mut stack = get_final_stack(&fri_response, library, code);
            let extracted_xfes_length = stack.pop().unwrap().value() as usize;
            let extracted_digests_length = stack.pop().unwrap().value() as usize;

            // assert correct lengths
            assert_eq!(num_digests, extracted_digests_length);
            assert_eq!(num_leafs, extracted_xfes_length);
        }

        /// Helper function for testing field getters. Only returns the final stack.
        fn get_final_stack<T: BFieldCodec + Clone>(
            obj: &T,
            library: Library,
            code: Vec<LabelledInstruction>,
        ) -> Vec<BFieldElement> {
            // initialize memory and stack
            let mut memory: HashMap<BFieldElement, BFieldElement> = HashMap::new();
            let random_address: u64 = thread_rng().gen_range(0..(1 << 30));
            let address = random_address.into();

            encode_to_memory(&mut memory, address, obj);
            let stack = [empty_stack(), vec![address]].concat();

            // link by hand
            let entrypoint = "entrypoint";
            let library_code = library.all_imports();
            let instructions = triton_asm!(
                call {entrypoint}
                halt

                {entrypoint}:
                    {&code}
                    return

                {&library_code}
            );

            let program = Program::new(&instructions);
            let nondeterminism = NonDeterminism::new(vec![]).with_ram(memory);
            let final_state =
                execute_with_terminal_state(&program, &[], &stack, &nondeterminism, None).unwrap();
            final_state.op_stack.stack
        }
    }

    #[test]
    fn test_option() {
        let mut rng = thread_rng();
        let n = rng.gen_range(0..5);
        let v = (0..n).map(|_| rng.gen::<Digest>()).collect_vec();
        let mut ram: HashMap<BFieldElement, BFieldElement> = HashMap::new();
        let some_address = FIRST_NON_DETERMINISTICALLY_INITIALIZED_MEMORY_ADDRESS;
        let none_address = encode_to_memory(&mut ram, some_address, &Some(v.clone()));
        encode_to_memory(&mut ram, none_address, &Option::<Vec<Digest>>::None);

        let some_decoded = *Option::<Vec<Digest>>::decode_from_memory(&ram, some_address).unwrap();
        assert!(some_decoded.is_some());
        assert_eq!(some_decoded.unwrap(), v);

        let none_decoded = *Option::<Vec<Digest>>::decode_from_memory(&ram, none_address).unwrap();
        assert!(none_decoded.is_none());
    }
}
