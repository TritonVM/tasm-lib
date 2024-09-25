use std::fmt::Display;
use std::fmt::Formatter;
use std::str::FromStr;

use itertools::Itertools;
use num::One;
use num::Zero;
use rand::prelude::*;
use triton_vm::isa::op_stack::NUM_OP_STACK_REGISTERS;
use triton_vm::prelude::*;

use crate::io::InputSource;
use crate::memory::load_words_from_memory_leave_pointer;
use crate::memory::load_words_from_memory_pop_pointer;
use crate::memory::write_words_to_memory_leave_pointer;
use crate::memory::write_words_to_memory_pop_pointer;

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum DataType {
    Bool,
    U32,
    U64,
    U128,
    Bfe,
    Xfe,
    Digest,
    List(Box<DataType>),
    Array(Box<ArrayType>),
    Tuple(Vec<DataType>),
    VoidPointer,
    StructRef(StructType),
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct ArrayType {
    pub element_type: DataType,
    pub length: usize,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct StructType {
    pub name: String,
    pub fields: Vec<(String, DataType)>,
}

impl Display for StructType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name)
    }
}

impl DataType {
    /// See [`BFieldCodec::static_length`].
    pub(crate) fn static_length(&self) -> Option<usize> {
        match self {
            DataType::Bool => bool::static_length(),
            DataType::U32 => u32::static_length(),
            DataType::U64 => u64::static_length(),
            DataType::U128 => u128::static_length(),
            DataType::Bfe => BFieldElement::static_length(),
            DataType::Xfe => XFieldElement::static_length(),
            DataType::Digest => Digest::static_length(),
            DataType::List(_) => None,
            DataType::Array(a) => Some(a.length * a.element_type.static_length()?),
            DataType::Tuple(t) => t.iter().map(|dt| dt.static_length()).sum(),
            DataType::VoidPointer => None,
            DataType::StructRef(s) => s.fields.iter().map(|(_, dt)| dt.static_length()).sum(),
        }
    }

    /// Return a string which can be used as part of function labels in Triton-VM
    pub fn label_friendly_name(&self) -> String {
        match self {
            DataType::List(inner_type) => format!("list_L{}R", inner_type.label_friendly_name()),
            DataType::Tuple(inner_types) => {
                format!(
                    "tuple_L{}R",
                    inner_types
                        .iter()
                        .map(|x| x.label_friendly_name())
                        .join("___")
                )
            }
            DataType::VoidPointer => "void_pointer".to_string(),
            DataType::Bool => "bool".to_string(),
            DataType::U32 => "u32".to_string(),
            DataType::U64 => "u64".to_string(),
            DataType::U128 => "u128".to_string(),
            DataType::Bfe => "bfe".to_string(),
            DataType::Xfe => "xfe".to_string(),
            DataType::Digest => "digest".to_string(),
            DataType::Array(array_type) => format!(
                "array{}___{}",
                array_type.length,
                array_type.element_type.label_friendly_name()
            ),
            DataType::StructRef(struct_type) => format!("{struct_type}"),
        }
    }

    /// Return the size that the data type takes up on stack
    pub fn stack_size(&self) -> usize {
        match self {
            DataType::Bool => 1,
            DataType::U32 => 1,
            DataType::U64 => 2,
            DataType::U128 => 4,
            DataType::Bfe => 1,
            DataType::Xfe => 3,
            DataType::Digest => Digest::LEN,
            DataType::List(_) => 1,
            DataType::Array(_) => 1,
            DataType::Tuple(t) => t.iter().map(|dt| dt.stack_size()).sum(),
            DataType::VoidPointer => 1,
            DataType::StructRef(_) => 1,
        }
    }

    /// Return the code to read a value of this type from memory.
    /// Leaves mutated point on top of stack.
    ///
    /// ```text
    /// BEFORE: _ (*address + self.stack_size() - 1)
    /// AFTER:  _ [value] (*address - 1)
    /// ```
    pub fn read_value_from_memory_leave_pointer(&self) -> Vec<LabelledInstruction> {
        load_words_from_memory_leave_pointer(self.stack_size())
    }

    /// Return the code to read a value of this type from memory.
    /// Pops pointer from stack.
    ///
    /// ```text
    /// BEFORE: _ (*address + self.stack_size() - 1)
    /// AFTER:  _ [value]
    /// ```
    pub fn read_value_from_memory_pop_pointer(&self) -> Vec<LabelledInstruction> {
        load_words_from_memory_pop_pointer(self.stack_size())
    }

    /// Return the code to write a value of this type to memory
    ///
    /// ```text
    /// BEFORE: _ [value] *address
    /// AFTER:  _ (*address + self.stack_size())
    /// ```
    pub fn write_value_to_memory_leave_pointer(&self) -> Vec<LabelledInstruction> {
        write_words_to_memory_leave_pointer(self.stack_size())
    }

    /// Return the code to write a value of this type to memory
    ///
    /// ```text
    /// BEFORE: _ [value] *address
    /// AFTER:  _
    /// ```
    pub fn write_value_to_memory_pop_pointer(&self) -> Vec<LabelledInstruction> {
        write_words_to_memory_pop_pointer(self.stack_size())
    }

    /// Return the code to read a value of this type from the specified input source
    ///
    /// ```text
    /// BEFORE: _
    /// AFTER:  _ [value]
    /// ```
    pub fn read_value_from_input(&self, input_source: InputSource) -> Vec<LabelledInstruction> {
        input_source.read_words(self.stack_size())
    }

    /// Return the code to write a value of this type to standard output
    ///
    /// ```text
    /// BEFORE: _ [value]
    /// AFTER:  _
    /// ```
    pub fn write_value_to_stdout(&self) -> Vec<LabelledInstruction> {
        crate::io::write_words(self.stack_size())
    }

    /// Return the code that compares two elements of this stack-size.
    ///
    /// ```text
    /// BEFORE: _ [self] [other]
    /// AFTER: _ (self == other)
    /// ```
    pub fn compare_elem_of_stack_size(stack_size: usize) -> Vec<LabelledInstruction> {
        match stack_size {
            0 => triton_asm!(push 1),
            1 => triton_asm!(eq),
            n => {
                let size_plus_one = n + 1;
                let size_minus_one = n - 1;

                assert!(size_plus_one < NUM_OP_STACK_REGISTERS);

                let first_cmps =
                    vec![triton_asm!(swap {size_plus_one} eq ); size_minus_one].concat();
                let last_cmp = triton_asm!(swap 2 eq);
                let boolean_ands = triton_asm![mul; size_minus_one];

                [first_cmps, last_cmp, boolean_ands].concat()
            }
        }
    }

    /// Return the code that compares two elements of this type.
    ///
    /// ```text
    /// BEFORE: _ [self] [other]
    /// AFTER: _ (self == other)
    /// ```
    pub fn compare(&self) -> Vec<LabelledInstruction> {
        DataType::compare_elem_of_stack_size(self.stack_size())
    }

    /// Return a string matching how the variant looks in source code
    pub fn variant_name(&self) -> String {
        // This function is used to autogenerate snippets in the tasm-lang compiler
        match self {
            DataType::Bool => "DataType::Bool".to_owned(),
            DataType::U32 => "DataType::U32".to_owned(),
            DataType::U64 => "DataType::U64".to_owned(),
            DataType::U128 => "DataType::U128".to_owned(),
            DataType::Bfe => "DataType::BFE".to_owned(),
            DataType::Xfe => "DataType::XFE".to_owned(),
            DataType::Digest => "DataType::Digest".to_owned(),
            DataType::List(elem_type) => {
                format!("DataType::List(Box::new({}))", elem_type.variant_name())
            }
            DataType::VoidPointer => "DataType::VoidPointer".to_owned(),
            DataType::Tuple(elements) => {
                let elements_as_variant_names =
                    elements.iter().map(|x| x.variant_name()).collect_vec();
                format!(
                    "DataType::Tuple(vec![{}])",
                    elements_as_variant_names.join(", ")
                )
            }
            DataType::Array(array_type) => format!(
                "[{}; {}]",
                array_type.element_type.variant_name(),
                array_type.length
            ),
            DataType::StructRef(struct_type) => format!("Box<{struct_type}>"),
        }
    }

    /// Return a collection of different data types, used for testing
    #[cfg(test)]
    pub fn big_random_generatable_type_collection() -> Vec<DataType> {
        vec![
            DataType::Bool,
            DataType::U32,
            DataType::U64,
            DataType::U128,
            DataType::Bfe,
            DataType::Xfe,
            DataType::Digest,
            DataType::VoidPointer,
            DataType::Tuple(vec![DataType::Bool]),
            DataType::Tuple(vec![DataType::Xfe, DataType::Bool]),
            DataType::Tuple(vec![DataType::Xfe, DataType::Digest]),
            DataType::Tuple(vec![DataType::Bool, DataType::Bool]),
            DataType::Tuple(vec![DataType::Digest, DataType::Xfe]),
            DataType::Tuple(vec![DataType::Bfe, DataType::Xfe, DataType::Digest]),
            DataType::Tuple(vec![DataType::Xfe, DataType::Bfe, DataType::Digest]),
            DataType::Tuple(vec![
                DataType::U64,
                DataType::Digest,
                DataType::Digest,
                DataType::Digest,
            ]),
            DataType::Tuple(vec![
                DataType::Digest,
                DataType::Digest,
                DataType::Digest,
                DataType::U64,
            ]),
            DataType::Tuple(vec![
                DataType::Digest,
                DataType::Xfe,
                DataType::U128,
                DataType::Bool,
            ]),
        ]
    }

    pub fn random_elements(&self, count: usize) -> Vec<Vec<BFieldElement>> {
        (0..count)
            .map(|_| self.seeded_random_element(&mut thread_rng()))
            .collect()
    }

    pub fn seeded_random_element(&self, rng: &mut impl Rng) -> Vec<BFieldElement> {
        match self {
            DataType::Bool => rng.gen::<bool>().encode(),
            DataType::U32 => rng.gen::<u32>().encode(),
            DataType::U64 => rng.gen::<u64>().encode(),
            DataType::U128 => rng.gen::<u128>().encode(),
            DataType::Bfe => rng.gen::<BFieldElement>().encode(),
            DataType::Xfe => rng.gen::<XFieldElement>().encode(),
            DataType::Digest => rng.gen::<Digest>().encode(),
            DataType::List(e) => {
                let len = rng.gen_range(0..20);
                e.random_list(rng, len)
            }
            DataType::Array(a) => Self::random_array(rng, a),
            DataType::Tuple(tys) => tys
                .iter()
                .flat_map(|ty| ty.seeded_random_element(rng))
                .collect(),
            DataType::VoidPointer => vec![rng.gen()],
            DataType::StructRef(_) => panic!("Random generation of structs is not supported"),
        }
    }

    /// A list of given length with random elements of type `self`.
    pub(crate) fn random_list(&self, rng: &mut impl Rng, len: usize) -> Vec<BFieldElement> {
        let maybe_prepend_elem_len = |elem: Vec<_>| {
            if self.static_length().is_some() {
                elem
            } else {
                [bfe_vec![elem.len() as u64], elem].concat()
            }
        };

        let elements = (0..len)
            .map(|_| self.seeded_random_element(rng))
            .flat_map(maybe_prepend_elem_len)
            .collect();

        [bfe_vec![len as u64], elements].concat()
    }

    pub(crate) fn random_array(rng: &mut impl Rng, array_ty: &ArrayType) -> Vec<BFieldElement> {
        (0..array_ty.length)
            .flat_map(|_| array_ty.element_type.seeded_random_element(rng))
            .collect()
    }
}

impl FromStr for DataType {
    type Err = anyhow::Error;

    // This implementation must be the inverse of `label_friendly_name`
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let res = if s.starts_with("list_L") && s.ends_with('R') {
            let inner = &s[6..s.len() - 1];
            let inner = FromStr::from_str(inner)?;
            Self::List(Box::new(inner))
        } else if s.starts_with("tuple_L") && s.ends_with('R') {
            let inner = &s[7..s.len() - 1];
            let inners = inner.split("___");
            let mut inners_resolved: Vec<Self> = vec![];
            for inner_elem in inners {
                inners_resolved.push(FromStr::from_str(inner_elem)?);
            }

            Self::Tuple(inners_resolved)
        } else {
            match s {
                "void_pointer" => Self::VoidPointer,
                "bool" => Self::Bool,
                "u32" => Self::U32,
                "u64" => Self::U64,
                "u128" => Self::U128,
                "bfe" => Self::Bfe,
                "xfe" => Self::Xfe,
                "digest" => Self::Digest,
                _ => anyhow::bail!("Could not parse {s} as a data type"),
            }
        };

        Ok(res)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, arbitrary::Arbitrary)]
pub enum Literal {
    Bool(bool),
    U32(u32),
    U64(u64),
    U128(u128),
    Bfe(BFieldElement),
    Xfe(XFieldElement),
    Digest(Digest),
}

impl Literal {
    pub fn data_type(&self) -> DataType {
        match self {
            Literal::Bool(_) => DataType::Bool,
            Literal::U32(_) => DataType::U32,
            Literal::U64(_) => DataType::U64,
            Literal::U128(_) => DataType::U128,
            Literal::Bfe(_) => DataType::Bfe,
            Literal::Xfe(_) => DataType::Xfe,
            Literal::Digest(_) => DataType::Digest,
        }
    }

    pub fn as_xfe(&self) -> XFieldElement {
        match self {
            Literal::Xfe(xfe) => *xfe,
            n => panic!("Expected XFE, got {n:?}"),
        }
    }

    /// Return the code to push the literal to the stack
    pub fn push_to_stack_code(&self) -> Vec<LabelledInstruction> {
        match self {
            Literal::Bool(bool) => triton_asm!(push {*bool as u32}),
            Literal::U32(val) => triton_asm!(push {*val}),
            Literal::U64(val) => triton_asm!(
                push {*val >> 32}
                push {*val & u32::MAX as u64}
            ),
            Literal::U128(val) => {
                triton_asm!(
                    push {(*val >> 96) & u32::MAX as u128}
                    push {(*val >> 64) & u32::MAX as u128}
                    push {(*val >> 32) & u32::MAX as u128}
                    push {(*val) & u32::MAX as u128}
                )
            }
            Literal::Bfe(bfe) => triton_asm!(push { bfe }),
            Literal::Xfe(xfe) => {
                let x0 = xfe.coefficients[0];
                let x1 = xfe.coefficients[1];
                let x2 = xfe.coefficients[2];
                triton_asm!(
                    push { x2 }
                    push { x1 }
                    push { x0 }
                )
            }
            Literal::Digest(digest) => {
                let x0 = digest.0[0];
                let x1 = digest.0[1];
                let x2 = digest.0[2];
                let x3 = digest.0[3];
                let x4 = digest.0[4];
                triton_asm!(
                    push { x4 }
                    push { x3 }
                    push { x2 }
                    push { x1 }
                    push { x0 }
                )
            }
        }
    }

    pub fn pop_from_stack(data_type: DataType, stack: &mut Vec<BFieldElement>) -> Self {
        let mut words = vec![];
        (0..data_type.stack_size()).for_each(|_| words.push(stack.pop().unwrap()));

        fn is_u32_based(words: &[BFieldElement]) -> bool {
            words.iter().all(|w| w.value() <= u32::MAX as u64)
        }

        match data_type {
            DataType::Bool => {
                assert!(words[0].is_one() || words[0].is_zero());
                Literal::Bool(words[0].value() != 0)
            }
            DataType::U32 => {
                assert!(is_u32_based(&words));
                Literal::U32(words[0].value().try_into().unwrap())
            }
            DataType::U64 => {
                assert!(is_u32_based(&words));
                Literal::U64(words[0].value() + (words[1].value() << 32))
            }
            DataType::U128 => {
                assert!(is_u32_based(&words));
                Literal::U128(
                    words[0].value() as u128
                        + ((words[1].value() as u128) << 32)
                        + ((words[2].value() as u128) << 64)
                        + ((words[3].value() as u128) << 96),
                )
            }
            DataType::Bfe => Literal::Bfe(words[0]),
            DataType::Xfe => Literal::Xfe(XFieldElement::new([words[0], words[1], words[2]])),
            DataType::Digest => Literal::Digest(Digest::new([
                words[0], words[1], words[2], words[3], words[4],
            ])),
            DataType::List(_) => unimplemented!(),
            DataType::Array(_) => unimplemented!(),
            DataType::Tuple(_) => unimplemented!(),
            DataType::VoidPointer => unimplemented!(),
            DataType::StructRef(_) => unimplemented!(),
        }
    }
}

#[cfg(test)]
mod tests {
    use proptest::prop_assert;
    use proptest_arbitrary_interop::arb;
    use test_strategy::proptest;

    use super::*;

    #[proptest]
    fn push_to_stack_leaves_value_on_top_of_stack(#[strategy(arb())] literal: Literal) {
        let code = triton_asm!(
            {&literal.push_to_stack_code()}
            halt
        );

        let mut vm_state = VMState::new(
            &Program::new(&code),
            PublicInput::default(),
            NonDeterminism::default(),
        );
        vm_state.run().unwrap();
        let read = Literal::pop_from_stack(literal.data_type(), &mut vm_state.op_stack.stack);
        assert_eq!(literal, read);
    }

    #[test]
    fn static_lengths_match_up_for_vectors() {
        assert_eq!(
            <Vec<XFieldElement>>::static_length(),
            DataType::List(Box::new(DataType::Xfe)).static_length()
        );
    }

    #[test]
    fn static_lengths_match_up_for_array_with_static_length_data_type() {
        assert_eq!(
            <[BFieldElement; 42]>::static_length(),
            DataType::Array(Box::new(ArrayType {
                element_type: DataType::Bfe,
                length: 42
            }))
            .static_length()
        );
    }

    #[test]
    fn static_lengths_match_up_for_array_with_dynamic_length_data_type() {
        assert_eq!(
            <[Vec<BFieldElement>; 42]>::static_length(),
            DataType::Array(Box::new(ArrayType {
                element_type: DataType::List(Box::new(DataType::Bfe)),
                length: 42
            }))
            .static_length()
        );
    }

    #[test]
    fn static_lengths_match_up_for_tuple_with_only_static_length_types() {
        assert_eq!(
            <(XFieldElement, BFieldElement)>::static_length(),
            DataType::Tuple(vec![DataType::Xfe, DataType::Bfe]).static_length()
        );
    }

    #[test]
    fn static_lengths_match_up_for_tuple_with_dynamic_length_types() {
        assert_eq!(
            <(XFieldElement, Vec<BFieldElement>)>::static_length(),
            DataType::Tuple(vec![DataType::Xfe, DataType::List(Box::new(DataType::Bfe))])
                .static_length()
        );
    }

    #[test]
    fn static_length_of_void_pointer_is_unknown() {
        assert!(DataType::VoidPointer.static_length().is_none());
    }

    #[test]
    fn static_lengths_match_up_for_struct_with_only_static_length_types() {
        #[derive(Debug, Clone, BFieldCodec)]
        struct StructTyStatic {
            u32: u32,
            u64: u64,
        }

        let struct_ty_static = StructType {
            name: "struct".to_owned(),
            fields: vec![
                ("u32".to_owned(), DataType::U32),
                ("u64".to_owned(), DataType::U64),
            ],
        };
        assert_eq!(
            StructTyStatic::static_length(),
            DataType::StructRef(struct_ty_static).static_length()
        );
    }

    #[test]
    fn static_lengths_match_up_for_struct_with_dynamic_length_types() {
        #[derive(Debug, Clone, BFieldCodec)]
        struct StructTyDyn {
            digest: Digest,
            list: Vec<BFieldElement>,
        }

        let struct_ty_dyn = StructType {
            name: "struct".to_owned(),
            fields: vec![
                ("digest".to_owned(), DataType::Digest),
                ("list".to_owned(), DataType::List(Box::new(DataType::Bfe))),
            ],
        };
        assert_eq!(
            StructTyDyn::static_length(),
            DataType::StructRef(struct_ty_dyn).static_length()
        );
    }

    #[test]
    fn random_list_of_lists_can_be_generated() {
        let mut rng = StdRng::seed_from_u64(5950175350772851878);
        let element_type = DataType::List(Box::new(DataType::Digest));
        let _list = element_type.random_list(&mut rng, 10);
    }

    #[proptest]
    fn random_list_conforms_to_bfield_codec(#[strategy(..255_usize)] len: usize, seed: u64) {
        let mut rng = StdRng::seed_from_u64(seed);
        let element_type = DataType::Digest;
        let list = element_type.random_list(&mut rng, len);
        prop_assert!(<Vec<Digest>>::decode(&list).is_ok());
    }

    #[proptest]
    fn random_list_of_lists_conforms_to_bfield_codec(
        #[strategy(..255_usize)] len: usize,
        seed: u64,
    ) {
        let mut rng = StdRng::seed_from_u64(seed);
        let element_type = DataType::List(Box::new(DataType::Digest));
        let list = element_type.random_list(&mut rng, len);
        prop_assert!(<Vec<Vec<Digest>>>::decode(&list).is_ok());
    }

    #[proptest]
    fn random_array_conforms_to_bfield_codec(seed: u64) {
        const LEN: usize = 42;

        let mut rng = StdRng::seed_from_u64(seed);
        let array_type = ArrayType {
            element_type: DataType::Digest,
            length: LEN,
        };
        let array = DataType::random_array(&mut rng, &array_type);
        prop_assert!(<[Digest; LEN]>::decode(&array).is_ok());
    }

    #[proptest]
    fn random_array_of_arrays_conforms_to_bfield_codec(seed: u64) {
        const INNER_LEN: usize = 42;
        const OUTER_LEN: usize = 13;

        let mut rng = StdRng::seed_from_u64(seed);
        let inner_type = ArrayType {
            element_type: DataType::Digest,
            length: INNER_LEN,
        };
        let outer_type = ArrayType {
            element_type: DataType::Array(Box::new(inner_type)),
            length: OUTER_LEN,
        };
        let array = DataType::random_array(&mut rng, &outer_type);
        prop_assert!(<[[Digest; INNER_LEN]; OUTER_LEN]>::decode(&array).is_ok());
    }
}
