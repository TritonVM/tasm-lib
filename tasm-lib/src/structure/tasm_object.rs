use std::collections::HashMap;
use std::error::Error;

use itertools::Itertools;
use num_traits::ConstZero;
use num_traits::Zero;
pub use tasm_object_derive::TasmObject;
use triton_vm::prelude::*;

use crate::prelude::*;

pub(super) type Result<T> = std::result::Result<T, Box<dyn Error + Send + Sync>>;

pub const DEFAULT_MAX_DYN_FIELD_SIZE: u32 = 1u32 << 28;

/// This trait defines methods for dealing with primitive types and
/// custom-defined struct types from within the VM, assuming they live in memory
/// as they are encoded with [`BFieldCodec`].
///
/// ### Dyn-Compatibility
///
/// This trait is _not_ [dyn-compatible] (previously known as “object safe”).
///
/// [dyn-compatible]: https://doc.rust-lang.org/reference/items/traits.html#dyn-compatibility
pub trait TasmObject: BFieldCodec {
    /// Maximum jump distance for encoded size and length indicators.
    /// The field getters will compare any length or size indicator read
    /// from memory against this value and crash the VM if the indicator
    /// is larger or equal.
    const MAX_OFFSET: u32 = DEFAULT_MAX_DYN_FIELD_SIZE;

    fn label_friendly_name() -> String;

    /// Return the size of `self` and crash if any contained size-indicator
    /// is not valid.
    ///
    /// ```text
    /// BEFORE: _ *object
    /// AFTER:  _ calculated_size
    /// ```
    fn compute_size_and_assert_valid_size_indicator(
        library: &mut Library,
    ) -> Vec<LabelledInstruction>;

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

/// This trait defines methods for dealing with custom-defined struct types from
/// within the VM, assuming they live in memory as they are encoded with
/// [`BFieldCodec`].
///
/// The arguments referring to fields are strings. For structs with unnamed fields, the
/// nth field name is implicitly `field_n`.
///
/// ### Dyn-Compatibility
///
/// This trait is _not_ [dyn-compatible] (previously known as “object safe”).
///
/// [dyn-compatible]: https://doc.rust-lang.org/reference/items/traits.html#dyn-compatibility
pub trait TasmStruct: TasmObject {
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
}

pub fn decode_from_memory_with_size<T: BFieldCodec>(
    memory: &HashMap<BFieldElement, BFieldElement>,
    address: BFieldElement,
    size: usize,
) -> Result<Box<T>> {
    let sequence = (0..size)
        .map(|i| address + bfe!(i as u64))
        .map(|b| memory.get(&b).copied().unwrap_or(BFieldElement::ZERO))
        .collect_vec();
    T::decode(&sequence).map_err(|e| e.into())
}

/// Convenience struct for converting between string literals and field name identifiers.
pub trait TasmStructFieldName {
    fn tasm_struct_field_name(&self) -> String;
}

impl TasmStructFieldName for &str {
    fn tasm_struct_field_name(&self) -> String {
        self.to_string()
    }
}

impl TasmStructFieldName for i32 {
    fn tasm_struct_field_name(&self) -> String {
        format!("field_{}", self)
    }
}

/// Convenience macro, so that we don't have to write
/// ```ignore
/// let field_f = <StructWithNamedFields as TasmStruct>::get_field!("f");
/// let field_0 = <StructWithUnnamedFields as TasmStruct>::get_field!("field_0");
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
    ($o:ident::$e:ident) => {
        <$o as $crate::structure::tasm_object::TasmStruct>::get_field(
            &$crate::structure::tasm_object::TasmStructFieldName::tasm_struct_field_name(
                &stringify!($e),
            ),
        )
    };
    ($o:ident::$e:expr) => {
        <$o as $crate::structure::tasm_object::TasmStruct>::get_field(
            &$crate::structure::tasm_object::TasmStructFieldName::tasm_struct_field_name(&$e),
        )
    };
}

/// Convenience macro, so that we don't have to write
/// ```ignore
/// let field_f = <StructWithNamedFields as TasmStruct>::get_field_with_size!("f");
/// let field_0 = <StructWithUnnamedFields as TasmStruct>::get_field_with_size!("field_0");
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
    ($o:ident::$e:ident) => {
        <$o as $crate::structure::tasm_object::TasmStruct>
            ::get_field_with_size(
                &$crate::structure::tasm_object::TasmStructFieldName::tasm_struct_field_name(
                    &stringify!($e)
                )
            )
    };
    ($o:ident::$e:expr) => {
        <$o as $crate::structure::tasm_object::TasmStruct>
            ::get_field_with_size(
                &$crate::structure::tasm_object::TasmStructFieldName::tasm_object_field_name(&$e)
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

impl Iterator for MemoryIter<'_> {
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

#[cfg(test)]
mod tests {
    use arbitrary::Arbitrary;
    use arbitrary::Unstructured;
    use triton_vm::proof_item::FriResponse;

    use super::*;
    use crate::empty_stack;
    use crate::execute_with_terminal_state;
    use crate::list::length::Length;
    use crate::memory::FIRST_NON_DETERMINISTICALLY_INITIALIZED_MEMORY_ADDRESS;
    use crate::test_prelude::*;

    #[derive(Debug, Clone, PartialEq, Eq, BFieldCodec, TasmObject, Arbitrary)]
    struct InnerStruct(XFieldElement, u32);

    #[test]
    fn test_load_and_decode_from_memory() {
        #[derive(Debug, Clone, PartialEq, Eq, BFieldCodec, TasmObject)]
        struct OuterStruct {
            o: InnerStruct,
            a: Vec<Option<bool>>,
            b: InnerStruct,
            p: Vec<Digest>,
            c: BFieldElement,
            l: Vec<Digest>,
        }

        fn pseudorandom_object(seed: [u8; 32]) -> OuterStruct {
            let mut rng = StdRng::from_seed(seed);
            let a = (0..19)
                .map(|_| if rng.gen() { Some(rng.gen()) } else { None })
                .collect_vec();
            let b0: XFieldElement = rng.gen();
            let b1: u32 = rng.gen();
            let b2: XFieldElement = rng.gen();
            let b3: u32 = rng.gen();
            let c: BFieldElement = rng.gen();
            let digests_len_p = rng.gen_range(0..5);
            let digests_p = (0..digests_len_p).map(|_| rng.gen()).collect_vec();
            let digests_len_l = rng.gen_range(0..5);
            let digests_l = (0..digests_len_l).map(|_| rng.gen()).collect_vec();

            OuterStruct {
                o: InnerStruct(b0, b1),
                a,
                b: InnerStruct(b2, b3),
                p: digests_p,
                c,
                l: digests_l,
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
        use twenty_first::math::x_field_element::EXTENSION_DEGREE;

        use super::*;
        use crate::maybe_write_debuggable_vm_state_to_disk;

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

        #[derive(BFieldCodec, TasmObject, PartialEq, Eq, Clone, Debug, Arbitrary)]
        struct TupleStruct(
            Vec<XFieldElement>,
            InnerStruct,
            u32,
            Vec<Digest>,
            Digest,
            Vec<BFieldElement>,
            Digest,
        );

        fn prepare_random_tuple_struct(seed: [u8; 32]) -> TupleStruct {
            let mut rng = StdRng::from_seed(seed);
            let mut randomness = [0u8; 100000];
            rng.fill_bytes(&mut randomness);
            let mut unstructured = Unstructured::new(&randomness);
            TupleStruct::arbitrary(&mut unstructured).unwrap()
        }

        /// Verify correct field-macro behavior when the size-indicators have
        /// illegal values.
        fn prop_negative_test_messed_up_size_indicators<T: BFieldCodec>(
            program: Program,
            tuple_struct: &T,
            obj_pointer: BFieldElement,
            offset_for_manipulated_si: BFieldElement,
            expected_stack: &[BFieldElement],
            also_run_negative_tests_for_correct_size_indicators: bool,
        ) {
            // No-messed works
            let mut no_messed_memory = HashMap::new();
            encode_to_memory(&mut no_messed_memory, obj_pointer, tuple_struct);
            let no_messed_nd = NonDeterminism::default().with_ram(no_messed_memory.clone());
            let mut vm_state_pass = VMState::new(
                program.clone(),
                PublicInput::default(),
                no_messed_nd.clone(),
            );
            maybe_write_debuggable_vm_state_to_disk(&vm_state_pass);
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
            let mut vm_state_fail0 = VMState::new(
                program.clone(),
                PublicInput::default(),
                messed_up_nd_0.clone(),
            );
            maybe_write_debuggable_vm_state_to_disk(&vm_state_fail0);
            let instruction_error0 = vm_state_fail0.run().unwrap_err();
            assert!(matches!(
                instruction_error0,
                InstructionError::AssertionFailed(_)
            ));

            // Messed-up encoding fails: Negative sizes banned
            let negative_number = bfe!(-42);
            messed_up_memory = no_messed_memory.clone();
            messed_up_memory.insert(obj_pointer + offset_for_manipulated_si, negative_number);
            let messed_up_nd_1 = NonDeterminism::default().with_ram(messed_up_memory.clone());
            let mut vm_state_fail1 = VMState::new(
                program.clone(),
                PublicInput::default(),
                messed_up_nd_1.clone(),
            );
            maybe_write_debuggable_vm_state_to_disk(&vm_state_fail1);
            let instruction_error1 = vm_state_fail1.run().unwrap_err();
            let expected_err =
                InstructionError::OpStackError(OpStackError::FailedU32Conversion(negative_number));
            assert_eq!(expected_err, instruction_error1);

            // Messed-up encoding fails: Size-indicator is within allowed
            // range but does not have the correct value. This is not checked
            // by all the `TasmObject` trait functions, so these checks are run
            // conditionally.
            if also_run_negative_tests_for_correct_size_indicators {
                // Messed-up encoding fails: Size-indicator is *one* too big
                messed_up_memory = no_messed_memory.clone();
                let address_for_manipulated_si = obj_pointer + offset_for_manipulated_si;
                messed_up_memory.insert(
                    address_for_manipulated_si,
                    messed_up_memory[&address_for_manipulated_si] + bfe!(1),
                );
                let messed_up_nd_2 = NonDeterminism::default().with_ram(messed_up_memory.clone());
                let mut vm_state_fail2 = VMState::new(
                    program.clone(),
                    PublicInput::default(),
                    messed_up_nd_2.clone(),
                );
                maybe_write_debuggable_vm_state_to_disk(&vm_state_fail2);
                let instruction_error2 = vm_state_fail2.run().unwrap_err();
                assert!(matches!(
                    instruction_error2,
                    InstructionError::AssertionFailed(_)
                ));

                // Messed-up encoding fails: Size-indicator is *one* too small
                messed_up_memory = no_messed_memory.clone();
                messed_up_memory.insert(
                    address_for_manipulated_si,
                    messed_up_memory[&address_for_manipulated_si] - bfe!(1),
                );
                let messed_up_nd_3 = NonDeterminism::default().with_ram(messed_up_memory.clone());
                let mut vm_state_fail3 =
                    VMState::new(program, PublicInput::default(), messed_up_nd_3.clone());
                maybe_write_debuggable_vm_state_to_disk(&vm_state_fail3);
                let instruction_error3 = vm_state_fail3.run().unwrap_err();
                assert!(matches!(
                    instruction_error3,
                    InstructionError::AssertionFailed(_)
                ));
            }
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
                let mut rng = StdRng::from_seed(seed);
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

            let expected_stack_benign = random_object.c.values();
            let offset_for_manipulated_si = bfe!(0);
            prop_negative_test_messed_up_size_indicators(
                Program::new(&code_using_field_getter),
                &random_object,
                START_OF_OBJ,
                offset_for_manipulated_si,
                &expected_stack_benign,
                false,
            );
        }

        #[test]
        fn mess_with_size_indicators_field_and_size_getter_negative_test() {
            const START_OF_OBJ: BFieldElement = BFieldElement::ZERO;
            let random_object = prepare_random_tuple_struct(random());
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

            let expected_field_size = bfe!(random_object.3.len() as u64 * Digest::LEN as u64 + 1);
            let expected_list_len = bfe!(random_object.3.len() as u64);
            let expected_stack_benign_nd = [expected_list_len, expected_field_size];
            prop_negative_test_messed_up_size_indicators(
                Program::new(&code_using_field_and_size_getter),
                &random_object,
                START_OF_OBJ,
                bfe!(Digest::LEN as u64),
                &expected_stack_benign_nd,
                false,
            );
        }

        #[test]
        fn mess_with_size_indicators_field_getter_negative_test() {
            const START_OF_OBJ: BFieldElement = BFieldElement::ZERO;
            let random_object = prepare_random_tuple_struct(random());
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

            let expected_output_benign_nd = random_object.4.values();
            prop_negative_test_messed_up_size_indicators(
                Program::new(&code_using_field_getter),
                &random_object,
                START_OF_OBJ,
                bfe!(Digest::LEN as u64),
                &expected_output_benign_nd,
                false,
            );
        }

        #[test]
        fn mess_with_size_indicators_size_indicator_validity_check_negative_test() {
            let mut library = Library::default();
            const OBJ_POINTER: BFieldElement = BFieldElement::new(422);
            let random_object = prepare_random_tuple_struct(random());
            let assert_size_indicator_validity =
                TupleStruct::compute_size_and_assert_valid_size_indicator(&mut library);
            let code_using_size_integrity_code = triton_asm!(
                // _

                push {OBJ_POINTER}
                // _ *tuple_struct

                {&assert_size_indicator_validity}
                // _ encoding_length

                halt
            );

            let expected_stack_benign = [bfe!(random_object.encode().len() as u64)];

            // mess up size-indicator of all size-indicators
            const SIZE_OF_SIZE_INDICATOR: usize = 1;
            let size_indicator_for_bfe_vec = Digest::LEN;
            let size_indicator_for_digest_vec_ptr = size_indicator_for_bfe_vec
                + random_object.5.encode().len()
                + SIZE_OF_SIZE_INDICATOR
                + Digest::LEN;
            let size_indicator_for_xfe_vec = size_indicator_for_digest_vec_ptr
                + random_object.3.encode().len()
                + SIZE_OF_SIZE_INDICATOR
                + 1
                + 4;

            for size_indicator_offset in [
                size_indicator_for_bfe_vec,
                size_indicator_for_digest_vec_ptr,
                size_indicator_for_xfe_vec,
            ] {
                prop_negative_test_messed_up_size_indicators(
                    Program::new(&code_using_size_integrity_code),
                    &random_object,
                    OBJ_POINTER,
                    bfe!(size_indicator_offset as u64),
                    &expected_stack_benign,
                    true,
                );
            }
        }

        #[test]
        fn mess_with_size_indicators_checked_size_list_w_dyn_sized_elems_negative_test() {
            #[derive(BFieldCodec, TasmObject, Debug, Clone, Arbitrary)]
            struct ListDynSizedElements {
                a: Vec<Digest>,
                b: Vec<Vec<Digest>>,
                c: Vec<XFieldElement>,
            }

            fn random_struct(seed: [u8; 32]) -> ListDynSizedElements {
                let mut rng = StdRng::from_seed(seed);
                let mut randomness = [0u8; 100000];
                rng.fill_bytes(&mut randomness);
                let mut unstructured = Unstructured::new(&randomness);
                ListDynSizedElements::arbitrary(&mut unstructured).unwrap()
            }

            const OBJ_POINTER: BFieldElement = BFieldElement::new(423);
            let mut library = Library::default();
            let assert_size_indicator_validity =
                ListDynSizedElements::compute_size_and_assert_valid_size_indicator(&mut library);

            let imports = library.all_imports();
            let code_using_size_integrity_code = triton_asm!(
                // _

                push {OBJ_POINTER}
                // _ *list_dyn_sized_elems

                {&assert_size_indicator_validity}
                // _ encoding_length

                halt

                {&imports}
            );

            let random_obj = random_struct(random());
            let expected_stack_benign = [bfe!(random_obj.encode().len() as u64)];

            const SIZE_OF_SIZE_INDICATOR: usize = 1;
            const SIZE_OF_LENGTH_INDICATOR: usize = 1;
            let offset_for_vec_vec_digest_size_indicator = random_obj.c.len() * EXTENSION_DEGREE
                + SIZE_OF_SIZE_INDICATOR
                + SIZE_OF_LENGTH_INDICATOR;
            prop_negative_test_messed_up_size_indicators(
                Program::new(&code_using_size_integrity_code),
                &random_obj,
                OBJ_POINTER,
                bfe!(offset_for_vec_vec_digest_size_indicator as u64),
                &expected_stack_benign,
                true,
            )
        }

        #[test]
        fn validate_total_size_statically_sized_struct() {
            #[derive(BFieldCodec, TasmObject, Debug, Clone, Copy)]
            struct StaticallySizedStruct {
                a: Digest,
                b: Digest,
                c: XFieldElement,
            }
            const OBJ_POINTER: BFieldElement = BFieldElement::new(422);
            let random_object = StaticallySizedStruct {
                a: random(),
                b: random(),
                c: random(),
            };

            let mut library = Library::default();
            let assert_size_indicator_validity =
                StaticallySizedStruct::compute_size_and_assert_valid_size_indicator(&mut library);
            let code_using_size_integrity_code = triton_asm!(
                // _

                push {OBJ_POINTER}
                // _ *tuple_struct

                {&assert_size_indicator_validity}
                // _ encoding_length

                halt
            );

            let program = Program::new(&code_using_size_integrity_code);
            let mut no_messed_memory = HashMap::new();
            encode_to_memory(&mut no_messed_memory, OBJ_POINTER, &random_object);
            let no_messed_nd = NonDeterminism::default().with_ram(no_messed_memory.clone());
            let mut vm_state = VMState::new(program, PublicInput::default(), no_messed_nd.clone());
            maybe_write_debuggable_vm_state_to_disk(&vm_state);
            vm_state.run().unwrap();

            let expected_stack = vec![bfe!(random_object.encode().len() as u64)];
            let actual_stack = vec![vm_state.op_stack[0]];
            assert_eq!(expected_stack, actual_stack);
        }

        #[test]
        fn load_and_decode_tuple_structs_from_memory() {
            let random_object = prepare_random_tuple_struct(random());
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
                execute_with_terminal_state(program, &[], &stack, &nondeterminism, None).unwrap();
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

    #[proptest]
    fn iter_decoding_too_short_sequence_does_not_panic(
        #[strategy(arb())] object: Vec<Vec<Digest>>,
        num_elements_to_drop: usize,
    ) {
        let encoding = object.encode();
        let encoding_len = encoding.len();
        let num_elements_to_drop = num_elements_to_drop % encoding_len;
        prop_assume!(num_elements_to_drop != 0);

        let mut object_iter = encoding.into_iter().dropping_back(num_elements_to_drop);
        prop_assert!(<Vec<Vec<Digest>>>::decode_iter(&mut object_iter).is_err());
    }
}
