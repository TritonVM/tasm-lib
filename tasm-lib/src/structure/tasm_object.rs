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
    /// Tasm code that returns a pointer to the field of the object, assuming:
    ///  - that a pointer to the said object lives on top of the stack;
    ///  - said object has a type that implements the [`TasmObject`] trait;
    ///  - said object lives in memory encoded as [`BFieldCodec`] specifies.
    ///
    /// ```text
    /// BEFORE: _ *object
    /// AFTER:  _ *field
    /// ```
    fn get_field(field_name: &str) -> Vec<LabelledInstruction>;

    /// Tasm code that returns a pointer to the field of the object, along with
    /// the size of that field in number of [`BFieldElement`]s, assuming:
    ///  - that a pointer to the said object lives on top of the stack;
    ///  - said object has a type that implements the [`TasmObject`] trait;
    ///  - said object lives in memory encoded as [`BFieldCodec`] specifies.
    ///
    /// ```text
    /// BEFORE: _ *object
    /// AFTER:  _ *field field_size
    ///```
    ///
    /// See also: `get_field` if you just want the field without the size.
    fn get_field_with_size(field_name: &str) -> Vec<LabelledInstruction>;

    /// Destructure a struct into the pointers to its fields.
    ///
    /// ```text
    /// BEFORE: _ *struct
    /// AFTER:  _ [pointers to all fields]
    /// ```
    ///
    /// # Example
    ///
    /// The example below defines a struct `Foo` and encodes an instance of it into
    /// memory. It then creates a Triton VM program to read and destructure the
    /// `Foo` instance, extracting and outputting the `bar` field. Finally, it runs
    /// the program and asserts that the extracted value matches the original `bar`
    /// value.
    ///
    /// ```no_compile
    /// #  // ^^^^^^^ derive macro `BFieldCodec` does not behave nicely; todo
    /// # use tasm_lib::prelude::*;
    /// # use tasm_lib::triton_vm::prelude::*;
    /// # use tasm_lib::memory::encode_to_memory;
    /// #[derive(BFieldCodec, TasmObject)]
    /// struct Foo {
    ///     bar: u32,
    ///     baz: XFieldElement,
    /// }
    ///
    /// let foo = Foo { bar: 13, baz: xfe!(0) };
    /// let foo_ptr = bfe!(42);
    /// let mut non_determinism = NonDeterminism::default();
    /// encode_to_memory(&mut non_determinism.ram, foo_ptr, &foo);
    ///
    /// let program = triton_program! {
    ///     read_io 1               // _ *foo
    ///     {&Foo::destructure()}   // _ *baz *bar
    ///     read_mem 1              // _ *baz bar (*bar - 1)
    ///     pop 1                   // _ *baz bar
    ///     write_io 1              // _ *baz
    ///     halt
    /// };
    ///
    /// let output = VM::run(program, PublicInput::new(vec![foo_ptr]), non_determinism).unwrap();
    /// let [bar] = output[..] else { panic!() };
    /// assert_eq!(bfe!(foo.bar), bar);
    /// ```
    fn destructure() -> Vec<LabelledInstruction>;
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
        format!("field_{self}")
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
struct MemoryIter<'a> {
    memory: &'a HashMap<BFieldElement, BFieldElement>,
    address: BFieldElement,
}

impl<'a> MemoryIter<'a> {
    fn new(memory: &'a HashMap<BFieldElement, BFieldElement>, address: BFieldElement) -> Self {
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
                .map(|_| {
                    if rng.random() {
                        Some(rng.random())
                    } else {
                        None
                    }
                })
                .collect_vec();
            let b0: XFieldElement = rng.random();
            let b1: u32 = rng.random();
            let b2: XFieldElement = rng.random();
            let b3: u32 = rng.random();
            let c: BFieldElement = rng.random();
            let digests_len_p = rng.random_range(0..5);
            let digests_p = (0..digests_len_p).map(|_| rng.random()).collect_vec();
            let digests_len_l = rng.random_range(0..5);
            let digests_l = (0..digests_len_l).map(|_| rng.random()).collect_vec();

            OuterStruct {
                o: InnerStruct(b0, b1),
                a,
                b: InnerStruct(b2, b3),
                p: digests_p,
                c,
                l: digests_l,
            }
        }

        let mut rng = rand::rng();
        let mut memory: HashMap<BFieldElement, BFieldElement> = HashMap::new();

        let object = pseudorandom_object(rng.random());
        let address = rng.random();
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
            rand::rng().fill_bytes(&mut randomness);
            let mut unstructured = Unstructured::new(&randomness);
            let random_object = NamedFields::arbitrary(&mut unstructured).unwrap();
            let random_address: u64 = rand::rng().random_range(0..(1 << 30));
            let address = random_address.into();
            let mut memory: HashMap<BFieldElement, BFieldElement> = HashMap::new();

            encode_to_memory(&mut memory, address, &random_object);
            let object_again: NamedFields =
                *NamedFields::decode_from_memory(&memory, address).unwrap();
            assert_eq!(random_object, object_again);

            let mut library = Library::new();
            let length_d = library.import(Box::new(Length));
            let length_f = library.import(Box::new(Length));
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
            let random_object = prepare_random_object(rand::random());
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
            let random_object = prepare_random_tuple_struct(rand::random());
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
            let random_object = prepare_random_tuple_struct(rand::random());
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
            let random_object = prepare_random_tuple_struct(rand::random());
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

            let random_obj = random_struct(rand::random());
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
                a: rand::random(),
                b: rand::random(),
                c: rand::random(),
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
            let random_object = prepare_random_tuple_struct(rand::random());
            let random_address: u64 = rand::rng().random_range(0..(1 << 30));
            let address = random_address.into();

            let mut memory: HashMap<BFieldElement, BFieldElement> = HashMap::new();
            encode_to_memory(&mut memory, address, &random_object);
            let object_again: TupleStruct =
                *TupleStruct::decode_from_memory(&memory, address).unwrap();
            assert_eq!(random_object, object_again);

            // code snippet to access object's fields
            let mut library = Library::new();
            let list_length = library.import(Box::new(Length));
            let code_for_list_lengths = triton_asm! {
                // _ *obj

                dup 0
                {&field!(TupleStruct::3)} // _ *obj *digests
                swap 1                    // _ *digests *obj

                dup 0
                {&field!(TupleStruct::5)} // _ *digests *obj *bfes
                swap 1                    // _ *digests *bfes *obj

                {&field!(TupleStruct::0)} // _ *digests *bfes *xfes
                call {list_length}        // _ *digests *bfes xfe_count
                swap 2                    // _ xfe_count *bfes *digests
                call {list_length}        // _ xfe_count *bfes digest_count
                swap 1
                call {list_length}        // _ xfe_count digest_count bfe_count
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
            let mut rng = rand::rng();
            let num_digests = 50;
            let num_leafs = 20;

            // generate object
            let authentication_structure = (0..num_digests)
                .map(|_| rng.random::<Digest>())
                .collect_vec();
            let revealed_leafs = (0..num_leafs)
                .map(|_| rng.random::<XFieldElement>())
                .collect_vec();
            let fri_response = FriResponse {
                auth_structure: authentication_structure,
                revealed_leaves: revealed_leafs,
            };

            // code snippet to access object's fields
            let mut library = Library::new();
            let get_authentication_structure = field!(FriResponse::auth_structure);
            let list_length = library.import(Box::new(Length));
            let get_revealed_leafs = field!(FriResponse::revealed_leaves);
            let code = triton_asm! {
                // _ *fri_response
                dup 0 // _ *fri_response *fri_response

                {&get_authentication_structure} // _ *fri_response *authentication_structure
                swap 1                          // _ *authentication_structure *fri_response
                {&get_revealed_leafs}           // _ *authentication_structure *revealed_leafs

                swap 1                          // _ *revealed_leafs *authentication_structure
                call {list_length}              // _ *revealed_leafs num_digests
                swap 1                          // _ num_digests *revealed_leafs
                call {list_length}              // _ num_digests num_leafs

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
            let random_address: u64 = rand::rng().random_range(0..(1 << 30));
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

    #[cfg(test)]
    mod destructure {
        use super::*;
        use crate::neptune::neptune_like_types_for_tests::MmrSuccessorProofLookalike;
        use crate::neptune::neptune_like_types_for_tests::TransactionKernelLookalike;
        use crate::neptune::neptune_like_types_for_tests::UpdateWitnessLookalike;
        use crate::twenty_first::util_types::mmr::mmr_accumulator::MmrAccumulator;

        #[test]
        fn unit_struct() {
            #[derive(BFieldCodec, TasmObject)]
            struct Empty {}

            let sentinel = bfe!(0xdead_face_u64);
            let program = triton_program! {
                push {sentinel}         // _ s
                push 0                  // _ s 0
                {&Empty::destructure()} // _ s
                push {sentinel}         // _ s s
                eq                      // _ (s == s)
                assert                  // _
                halt
            };
            VM::run(program, PublicInput::default(), NonDeterminism::default()).unwrap();
        }

        mod one_field {
            use super::*;

            #[derive(Debug, Copy, Clone, BFieldCodec, TasmObject, Arbitrary)]
            struct TupleStatic(u32);

            #[derive(Debug, Clone, BFieldCodec, TasmObject, Arbitrary)]
            struct TupleDynamic(Vec<u32>);

            #[derive(Debug, Clone, BFieldCodec, TasmObject, Arbitrary)]
            struct TupleNested(Vec<Vec<u32>>);

            #[derive(Debug, Copy, Clone, BFieldCodec, TasmObject, Arbitrary)]
            struct NamedStatic {
                field: u32,
            }

            #[derive(Debug, Clone, BFieldCodec, TasmObject, Arbitrary)]
            struct NamedDynamic {
                field: Vec<u32>,
            }

            #[derive(Debug, Clone, BFieldCodec, TasmObject, Arbitrary)]
            struct NamedNested {
                field: Vec<Vec<u32>>,
            }

            // This macro is a little bit cursed due to the `$post_process`. Since it is
            // very limited in scope, I say it's better than duplicating essentially the
            // same code six times. If you want to extend the scope of this macro, please
            // re-design it.
            macro_rules! one_field_test_case {
                (fn $test_name:ident for $ty:ident: $f_name:tt $($post_process:tt)*) => {
                    #[proptest]
                    fn $test_name(
                        #[strategy(arb())] foo: $ty,
                        #[strategy(arb())] ptr: BFieldElement,
                    ) {
                        let program = triton_program! {
                            push {ptr}
                            {&$ty::destructure()}
                            read_mem 1 pop 1 write_io 1
                            halt
                        };

                        let mut non_determinism = NonDeterminism::default();
                        encode_to_memory(&mut non_determinism.ram, ptr, &foo);

                        let output = VM::run(program, PublicInput::default(), non_determinism)?;
                        let [output] = output[..] else {
                            return Err(TestCaseError::Fail("unexpected output".into()));
                        };

                        let $ty { $f_name: the_field } = foo;
                        let expected = the_field$($post_process)*;
                        prop_assert_eq!(bfe!(expected), output);
                    }
                };
            }

            one_field_test_case!( fn tuple_static  for TupleStatic:  0 );
            one_field_test_case!( fn tuple_dynamic for TupleDynamic: 0.len() );
            one_field_test_case!( fn tuple_nested  for TupleNested:  0.len() );
            one_field_test_case!( fn named_static  for NamedStatic:  field );
            one_field_test_case!( fn named_dynamic for NamedDynamic: field.len() );
            one_field_test_case!( fn named_nested  for NamedNested:  field.len() );
        }

        mod two_fields {
            use super::*;

            #[derive(Debug, Copy, Clone, BFieldCodec, TasmObject, Arbitrary)]
            struct TupleStatStat(u32, u32);

            #[derive(Debug, Clone, BFieldCodec, TasmObject, Arbitrary)]
            struct TupleStatDyn(u32, Vec<u32>);

            #[derive(Debug, Clone, BFieldCodec, TasmObject, Arbitrary)]
            struct TupleDynStat(Vec<u32>, u32);

            #[derive(Debug, Clone, BFieldCodec, TasmObject, Arbitrary)]
            struct TupleDynDyn(Vec<u32>, Vec<u32>);

            #[derive(Debug, Clone, BFieldCodec, TasmObject, Arbitrary)]
            struct TupleStatNest(u32, Vec<Vec<u32>>);

            #[derive(Debug, Clone, BFieldCodec, TasmObject, Arbitrary)]
            struct TupleNestStat(Vec<Vec<u32>>, u32);

            #[derive(Debug, Clone, BFieldCodec, TasmObject, Arbitrary)]
            struct TupleNestNest(Vec<Vec<u32>>, Vec<Vec<u32>>);

            #[derive(Debug, Copy, Clone, BFieldCodec, TasmObject, Arbitrary)]
            struct NamedStatStat {
                a: u32,
                b: u32,
            }

            #[derive(Debug, Clone, BFieldCodec, TasmObject, Arbitrary)]
            struct NamedStatDyn {
                a: u32,
                b: Vec<u32>,
            }

            #[derive(Debug, Clone, BFieldCodec, TasmObject, Arbitrary)]
            struct NamedDynStat {
                a: Vec<u32>,
                b: u32,
            }

            #[derive(Debug, Clone, BFieldCodec, TasmObject, Arbitrary)]
            struct NamedDynDyn {
                a: Vec<u32>,
                b: Vec<u32>,
            }

            #[derive(Debug, Clone, BFieldCodec, TasmObject, Arbitrary)]
            struct NamedStatNest {
                a: u32,
                b: Vec<Vec<u32>>,
            }

            #[derive(Debug, Clone, BFieldCodec, TasmObject, Arbitrary)]
            struct NamedNestStat {
                a: Vec<Vec<u32>>,
                b: u32,
            }

            #[derive(Debug, Clone, BFieldCodec, TasmObject, Arbitrary)]
            struct NamedNestNest {
                a: Vec<Vec<u32>>,
                b: Vec<Vec<u32>>,
            }

            // This macro is a little bit cursed due to the `$post_process`es. Since it is
            // very limited in scope, I say it's better than duplicating essentially the
            // same code 14 times. If you want to extend the scope of this macro, please
            // re-design it.
            macro_rules! two_fields_test_case {
                (fn $test_name:ident for $ty:ident:
                    ($f_name_0:tt $($post_process_0:tt)*)
                    ($f_name_1:tt $($post_process_1:tt)*)
                ) => {
                    #[proptest]
                    fn $test_name(
                        #[strategy(arb())] foo: $ty,
                        #[strategy(arb())] ptr: BFieldElement,
                    ) {
                        let program = triton_program! {
                            push {ptr}
                            {&$ty::destructure()}
                            read_mem 1 pop 1 write_io 1
                            read_mem 1 pop 1 write_io 1
                            halt
                        };

                        let mut non_determinism = NonDeterminism::default();
                        encode_to_memory(&mut non_determinism.ram, ptr, &foo);

                        let output = VM::run(program, PublicInput::default(), non_determinism)?;
                        let [output_0, output_1] = output[..] else {
                            return Err(TestCaseError::Fail("unexpected output".into()));
                        };

                        let $ty { $f_name_0: field_0, $f_name_1: field_1 } = foo;
                        let expected_0 = field_0$($post_process_0)*;
                        let expected_1 = field_1$($post_process_1)*;
                        prop_assert_eq!(bfe!(expected_0), output_0);
                        prop_assert_eq!(bfe!(expected_1), output_1);
                    }
                };
            }

            two_fields_test_case!( fn tuple_stat_stat for TupleStatStat: (0) (1) );
            two_fields_test_case!( fn tuple_stat_dyn  for TupleStatDyn:  (0) (1.len()) );
            two_fields_test_case!( fn tuple_dyn_stat  for TupleDynStat:  (0.len()) (1) );
            two_fields_test_case!( fn tuple_dyn_dyn   for TupleDynDyn:   (0.len()) (1.len()) );
            two_fields_test_case!( fn tuple_stat_nest for TupleStatNest: (0) (1.len()) );
            two_fields_test_case!( fn tuple_nest_stat for TupleNestStat: (0.len()) (1) );
            two_fields_test_case!( fn tuple_nest_nest for TupleNestNest: (0.len()) (1.len()) );
            two_fields_test_case!( fn named_stat_stat for NamedStatStat: (a) (b) );
            two_fields_test_case!( fn named_stat_dyn  for NamedStatDyn:  (a) (b.len()) );
            two_fields_test_case!( fn named_dyn_stat  for NamedDynStat:  (a.len()) (b) );
            two_fields_test_case!( fn named_dyn_dyn   for NamedDynDyn:   (a.len()) (b.len()) );
            two_fields_test_case!( fn named_stat_nest for NamedStatNest: (a) (b.len()) );
            two_fields_test_case!( fn named_nest_stat for NamedNestStat: (a.len()) (b) );
            two_fields_test_case!( fn named_nest_nest for NamedNestNest: (a.len()) (b.len()) );
        }

        #[test]
        fn all_static_dynamic_neighbor_combinations() {
            /// A struct where all neighbor combinations of fields with
            /// {static, dynamic}×{static, dynamic} sizes occur.
            #[derive(Debug, BFieldCodec, TasmObject, Eq, PartialEq)]
            struct Foo {
                a: XFieldElement,
                b: Vec<Digest>,
                c: Vec<Vec<XFieldElement>>,
                d: u128,
                e: u64,
            }

            let foo = Foo {
                a: xfe!([42, 43, 44]),
                b: vec![Digest::new(bfe_array![45, 46, 47, 48, 49])],
                c: vec![vec![], xfe_vec![[50, 51, 52]]],
                d: 53 + (54 << 32) + (55 << 64) + (56 << 96),
                e: 57 + (58 << 32),
            };

            let foo_encoding = bfe_vec![
                /* e: 00..=01 */ 57, 58, //
                /* d: 02..=05 */ 53, 54, 55, 56, //
                /* c: 06..=14 */ 8, 2, 1, 0, 4, 1, 50, 51, 52, //
                /* b: 15..=21 */ 6, 1, 45, 46, 47, 48, 49, //
                /* a: 22..=24 */ 42, 43, 44 //
            ];
            debug_assert_eq!(foo_encoding, foo.encode(),);

            let foo_ptr = bfe!(100);
            let mut non_determinism = NonDeterminism::default();
            encode_to_memory(&mut non_determinism.ram, foo_ptr, &foo);

            let program = triton_program! {
                read_io 1               // _ *foo
                {&Foo::destructure()}   // _ *e *d *c *b *a
                write_io 5              // _
                halt
            };

            let input = PublicInput::new(vec![foo_ptr]);
            let output = VM::run(program, input, non_determinism.clone()).unwrap();
            let [a_ptr, b_ptr, c_ptr, d_ptr, e_ptr] = output[..] else {
                panic!("expected 5 pointers");
            };

            assert_eq!(foo_ptr + bfe!(22), a_ptr);
            assert_eq!(foo_ptr + bfe!(16), b_ptr);
            assert_eq!(foo_ptr + bfe!(7), c_ptr);
            assert_eq!(foo_ptr + bfe!(2), d_ptr);
            assert_eq!(foo_ptr + bfe!(0), e_ptr);

            let a = *XFieldElement::decode_from_memory(&non_determinism.ram, a_ptr).unwrap();
            let b = *Vec::decode_from_memory(&non_determinism.ram, b_ptr).unwrap();
            let c = *Vec::decode_from_memory(&non_determinism.ram, c_ptr).unwrap();
            let d = *u128::decode_from_memory(&non_determinism.ram, d_ptr).unwrap();
            let e = *u64::decode_from_memory(&non_determinism.ram, e_ptr).unwrap();
            let foo_again = Foo { a, b, c, d, e };
            assert_eq!(foo, foo_again);
        }

        #[proptest]
        fn destructure_update_witness(
            #[strategy(arb())] witness: UpdateWitnessLookalike,
            #[strategy(arb())] witness_ptr: BFieldElement,
        ) {
            let mut non_determinism = NonDeterminism::default();
            encode_to_memory(&mut non_determinism.ram, witness_ptr, &witness);

            let program = triton_program! {
                read_io 1
                {&UpdateWitnessLookalike::destructure()}
                write_io 5 write_io 5 write_io 4
                halt
            };

            let input = PublicInput::new(vec![witness_ptr]);
            let output = VM::run(program, input, non_determinism.clone())?;
            let mut output = output.into_iter();
            let mut next_ptr = || output.next().unwrap();
            let ram = &non_determinism.ram;

            let old_kernel =
                *TransactionKernelLookalike::decode_from_memory(ram, next_ptr()).unwrap();
            let new_kernel =
                *TransactionKernelLookalike::decode_from_memory(ram, next_ptr()).unwrap();
            let old_kernel_mast_hash = *Digest::decode_from_memory(ram, next_ptr()).unwrap();
            let new_kernel_mast_hash = *Digest::decode_from_memory(ram, next_ptr()).unwrap();
            let old_proof = *Proof::decode_from_memory(ram, next_ptr()).unwrap();
            let new_swbfi_bagged = *Digest::decode_from_memory(ram, next_ptr()).unwrap();
            let new_aocl = *MmrAccumulator::decode_from_memory(ram, next_ptr()).unwrap();
            let new_swbfa_hash = *Digest::decode_from_memory(ram, next_ptr()).unwrap();
            let old_swbfi_bagged = *Digest::decode_from_memory(ram, next_ptr()).unwrap();
            let old_aocl = *MmrAccumulator::decode_from_memory(ram, next_ptr()).unwrap();
            let old_swbfa_hash = *Digest::decode_from_memory(ram, next_ptr()).unwrap();
            let aocl_successor_proof =
                *MmrSuccessorProofLookalike::decode_from_memory(ram, next_ptr()).unwrap();
            let outputs_hash = *Digest::decode_from_memory(ram, next_ptr()).unwrap();
            let public_announcements_hash = *Digest::decode_from_memory(ram, next_ptr()).unwrap();

            let witness_again = UpdateWitnessLookalike {
                old_kernel,
                new_kernel,
                old_kernel_mast_hash,
                new_kernel_mast_hash,
                old_proof,
                new_swbfi_bagged,
                new_aocl,
                new_swbfa_hash,
                old_swbfi_bagged,
                old_aocl,
                old_swbfa_hash,
                aocl_successor_proof,
                outputs_hash,
                public_announcements_hash,
            };
            prop_assert_eq!(witness, witness_again);
        }
    }

    #[test]
    fn test_option() {
        let mut rng = rand::rng();
        let n = rng.random_range(0..5);
        let v = (0..n).map(|_| rng.random::<Digest>()).collect_vec();
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
