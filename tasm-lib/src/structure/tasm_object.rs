use std::collections::HashMap;

use anyhow::Result;
pub use derive_tasm_object::TasmObject;

use itertools::Itertools;
use num_traits::Zero;
use triton_vm::{instruction::LabelledInstruction, triton_asm, BFieldElement};
use twenty_first::{
    shared_math::bfield_codec::BFieldCodec,
    util_types::{
        algebraic_hasher::AlgebraicHasher,
        mmr::{mmr_accumulator::MmrAccumulator, mmr_membership_proof::MmrMembershipProof},
    },
};

use crate::{memory::dyn_malloc::DYN_MALLOC_ADDRESS, Digest};

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

    /// Given an iterator over `BFieldElement`s, decode it as a Self object.
    fn decode_iter<Itr: Iterator<Item = BFieldElement>>(iterator: &mut Itr) -> Result<Box<Self>>;

    /// Given a memory object (as HashMap of BFE->BFE) and and address (BFE), decode the
    /// object located there.
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
    T::decode(&sequence)
}

/// Stores the encoding of the given object into memory at the given address, and returns
/// the address of the first untouched memory cell after.
pub fn encode_to_memory<T: BFieldCodec>(
    memory: &mut HashMap<BFieldElement, BFieldElement>,
    address: BFieldElement,
    object: T,
) -> BFieldElement {
    let encoding = object.encode();
    for (i, e) in encoding.iter().enumerate() {
        memory.insert(address + BFieldElement::new(i as u64), *e);
    }
    address + BFieldElement::new(encoding.len() as u64)
}

/// Loads the `BFieldCodec`-encodable object into memory at the first free location, and
/// updates the allocator accordingly. Return the address where the object is stored. This
/// method can be chained together in order to load multiple objects into memory without
/// overlaps.
pub fn load_to_memory<T: BFieldCodec>(
    memory: &mut HashMap<BFieldElement, BFieldElement>,
    object: T,
) -> BFieldElement {
    let address = memory
        .get(&BFieldElement::new(DYN_MALLOC_ADDRESS as u64))
        .copied()
        .unwrap_or(BFieldElement::new(1));
    let new_alloc = encode_to_memory(memory, address, object);
    memory.insert(BFieldElement::new(DYN_MALLOC_ADDRESS as u64), new_alloc);
    address
}

impl<T: BFieldCodec> TasmObject for Vec<T> {
<<<<<<< HEAD
<<<<<<< HEAD
    fn get_field(_field_name: &str) -> Vec<LabelledInstruction> {
        panic!("`Vec` does not have fields; cannot access them")
    }

    fn get_field_with_size(_field_name: &str) -> Vec<LabelledInstruction> {
        panic!("`Vec` does not have fields; cannot access them")
    }

    fn get_field_start_with_jump_distance(_field_name: &str) -> Vec<LabelledInstruction> {
=======
    fn get_field(field_name: &str) -> Vec<LabelledInstruction> {
=======
    fn get_field(_field_name: &str) -> Vec<LabelledInstruction> {
>>>>>>> 90e580d (integrate merkle root snippet into fri stub)
        panic!("`Vec` does not have fields; cannot access them")
    }

    fn get_field_with_size(_field_name: &str) -> Vec<LabelledInstruction> {
        panic!("`Vec` does not have fields; cannot access them")
    }

<<<<<<< HEAD
    fn get_field_start_with_jump_distance(field_name: &str) -> Vec<LabelledInstruction> {
>>>>>>> c02a808 (start fri verifier (wip))
=======
    fn get_field_start_with_jump_distance(_field_name: &str) -> Vec<LabelledInstruction> {
>>>>>>> 90e580d (integrate merkle root snippet into fri stub)
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
            let object = *T::decode(&sequence)?;
            vector.push(object);
        }
        Ok(Box::new(vector))
    }
}

impl<H: AlgebraicHasher> TasmObject for MmrMembershipProof<H> {
    fn get_field(field_name: &str) -> Vec<LabelledInstruction> {
        match field_name {
            "leaf_index" => triton_asm! {},
            "authentication_path" => triton_asm! { push 3 add },
            unknown => panic!("cannot match on field {unknown}"),
        }
    }

    fn get_field_with_size(field_name: &str) -> Vec<LabelledInstruction> {
        match field_name {
            "leaf_index" => triton_asm! { push 2 },
            "authentication_path" => triton_asm! { push 2 add read_mem swap 1 push 1 add swap 1 },
            unknown => panic!("cannot match on field {unknown}"),
        }
    }

    fn get_field_start_with_jump_distance(field_name: &str) -> Vec<LabelledInstruction> {
        match field_name {
            "leaf_index" => triton_asm! { push 2 },
            "authentication_path" => triton_asm! { push 2 add read_mem push 1 add },
            unknown => panic!("cannot match on field {unknown}"),
        }
    }

    fn decode_iter<Itr: Iterator<Item = BFieldElement>>(iterator: &mut Itr) -> Result<Box<Self>> {
        // leaf index is encoded as two `BFieldElement`s
        let leaf_index: u64 =
            *BFieldCodec::decode(&[iterator.next().unwrap(), iterator.next().unwrap()])?;
        // authentication path is length-prepended
        let length = iterator.next().unwrap().value() as usize;
        let auth_path_sequence = (0..length).map(|_| iterator.next().unwrap()).collect_vec();
        let authentication_path: Vec<Digest> = *BFieldCodec::decode(&auth_path_sequence)?;
        let object = MmrMembershipProof::new(leaf_index, authentication_path);
        Ok(Box::new(object))
    }
}

impl<H: AlgebraicHasher> TasmObject for MmrAccumulator<H> {
    fn get_field(field_name: &str) -> Vec<LabelledInstruction> {
        match field_name {
            "leaf_count" => triton_asm! {},
            "peaks" => triton_asm! { push 3 add },
            unknown => panic!("cannot match on field {unknown}"),
        }
    }

    fn get_field_with_size(field_name: &str) -> Vec<LabelledInstruction> {
        match field_name {
            "leaf_count" => triton_asm! { push 2 },
            "peaks" => triton_asm! { push 2 add read_mem swap 1 push 1 add swap 1 },
            unknown => panic!("cannot match on field {unknown}"),
        }
    }

    fn get_field_start_with_jump_distance(field_name: &str) -> Vec<LabelledInstruction> {
        match field_name {
            "leaf_count" => triton_asm! { push 2 },
            "peaks" => triton_asm! { push 2 add read_mem push 1 add },
            unknown => panic!("cannot match on field {unknown}"),
        }
    }

    fn decode_iter<Itr: Iterator<Item = BFieldElement>>(iterator: &mut Itr) -> Result<Box<Self>> {
        // the `digests` field is length-prepended
        let length = iterator.next().unwrap().value() as usize;
        let digests_sequence = (0..length).map(|_| iterator.next().unwrap()).collect_vec();
        let digests: Vec<Digest> = *BFieldCodec::decode(&digests_sequence)?;
        let object = MmrAccumulator::new(digests);
        Ok(Box::new(object))
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

#[cfg(test)]
mod test {
    use std::collections::HashMap;

    use itertools::Itertools;
    use rand::{rngs::StdRng, thread_rng, Rng, SeedableRng};
    use triton_vm::{triton_asm, BFieldElement, NonDeterminism};
    use twenty_first::shared_math::{bfield_codec::BFieldCodec, x_field_element::XFieldElement};

    use crate::{
        get_init_tvm_stack, io, memory,
        snippet::{DataType, DeprecatedSnippet, InputSource},
        structure::tasm_object::TasmObject,
        test_helpers::test_rust_equivalence_multiple_deprecated,
        ExecutionState,
    };

    #[derive(Debug, PartialEq, Eq, BFieldCodec)]
    enum InnerEnum {
        Cow(u32),
        Horse(u128),
        Pig(XFieldElement),
        Sheep([BFieldElement; 13]),
    }

    #[derive(Debug, PartialEq, Eq, BFieldCodec, TasmObject)]
    struct InnerStruct(XFieldElement, u32);

    #[derive(Debug, PartialEq, Eq, BFieldCodec, TasmObject)]
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

    impl DeprecatedSnippet for TestObjectFieldGetter {
        fn entrypoint_name(&self) -> String {
            "tasm_test_object_field_getter".to_string()
        }

        fn input_field_names(&self) -> Vec<String> {
            vec!["object".to_string()]
        }

        fn input_types(&self) -> Vec<crate::snippet::DataType> {
            vec![DataType::VoidPointer]
        }

        fn output_types(&self) -> Vec<crate::snippet::DataType> {
            vec![]
        }

        fn output_field_names(&self) -> Vec<String> {
            vec![]
        }

        fn stack_diff(&self) -> isize {
            -1
        }

        fn function_code(&self, library: &mut crate::library::Library) -> String {
            let entrypoint = self.entrypoint_name();
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
                    {&object_to_b}
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

            format!("{}\n", code.iter().join("\n"))
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
                let input_state = ExecutionState {
                    stack: stack.clone(),
                    std_in,
                    nondeterminism: NonDeterminism::new(vec![]),
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

            ExecutionState {
                stack,
                std_in,
                nondeterminism: NonDeterminism::new(vec![]),
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

            ExecutionState {
                stack,
                std_in,
                nondeterminism: NonDeterminism::new(vec![]),
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
        test_rust_equivalence_multiple_deprecated(&TestObjectFieldGetter, false);
    }

    #[test]
    fn test_decode_from_memory() {
        let mut rng = thread_rng();
        let mut memory: HashMap<BFieldElement, BFieldElement> = HashMap::new();
        let address: BFieldElement = rng.gen();

        // generate random object
        let object = pseudorandom_object(rng.gen());

        // write encoding to memory
        for (i, o) in object.encode().into_iter().enumerate() {
            memory.insert(address + BFieldElement::new(i as u64), o);
        }

        // decode from memory
        let object_again: OuterStruct = *OuterStruct::decode_from_memory(&memory, address).unwrap();

        // assert equal
        assert_eq!(object, object_again);
    }
}
