use std::fmt::Display;
use std::fmt::Formatter;
use std::str::FromStr;

use itertools::Itertools;
use rand::prelude::*;
use triton_vm::isa::op_stack::NUM_OP_STACK_REGISTERS;
use triton_vm::prelude::*;

use crate::io::InputSource;
use crate::memory::load_words_from_memory_leave_pointer;
use crate::memory::load_words_from_memory_pop_pointer;
use crate::memory::write_words_to_memory_leave_pointer;
use crate::memory::write_words_to_memory_pop_pointer;
use crate::pop_encodable;

/// A type hint for developers of Triton Assembly.
///
/// Note that _no_ type checking is performed.
#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum DataType {
    Bool,
    U32,
    U64,
    U128,
    U160,
    U192,
    I128,
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
            Self::Bool => bool::static_length(),
            Self::U32 => u32::static_length(),
            Self::U64 => u64::static_length(),
            Self::U128 => u128::static_length(),
            Self::U160 => Some(5),
            Self::U192 => Some(6),
            Self::I128 => i128::static_length(),
            Self::Bfe => BFieldElement::static_length(),
            Self::Xfe => XFieldElement::static_length(),
            Self::Digest => Digest::static_length(),
            Self::List(_) => None,
            Self::Array(a) => Some(a.length * a.element_type.static_length()?),
            Self::Tuple(t) => t.iter().map(|dt| dt.static_length()).sum(),
            Self::VoidPointer => None,
            Self::StructRef(s) => s.fields.iter().map(|(_, dt)| dt.static_length()).sum(),
        }
    }

    /// A string which can be used as part of function labels in Triton-VM.
    pub fn label_friendly_name(&self) -> String {
        match self {
            Self::List(inner_type) => format!("list_L{}R", inner_type.label_friendly_name()),
            Self::Tuple(inner_types) => {
                format!(
                    "tuple_L{}R",
                    inner_types
                        .iter()
                        .map(|x| x.label_friendly_name())
                        .join("___")
                )
            }
            Self::VoidPointer => "void_pointer".to_string(),
            Self::Bool => "bool".to_string(),
            Self::U32 => "u32".to_string(),
            Self::U64 => "u64".to_string(),
            Self::U128 => "u128".to_string(),
            Self::U160 => "u160".to_string(),
            Self::U192 => "u192".to_string(),
            Self::I128 => "i128".to_string(),
            Self::Bfe => "bfe".to_string(),
            Self::Xfe => "xfe".to_string(),
            Self::Digest => "digest".to_string(),
            Self::Array(array_type) => format!(
                "array{}___{}",
                array_type.length,
                array_type.element_type.label_friendly_name()
            ),
            Self::StructRef(struct_type) => format!("{struct_type}"),
        }
    }

    /// The size that the data type takes up on stack
    pub fn stack_size(&self) -> usize {
        match self {
            Self::Bool
            | Self::U32
            | Self::U64
            | Self::U128
            | Self::U160
            | Self::U192
            | Self::I128
            | Self::Bfe
            | Self::Xfe
            | Self::Digest => self.static_length().unwrap(),
            Self::List(_) => 1,
            Self::Array(_) => 1,
            Self::Tuple(t) => t.iter().map(|dt| dt.stack_size()).sum(),
            Self::VoidPointer => 1,
            Self::StructRef(_) => 1,
        }
    }

    /// The code to read a value of this type from memory.
    /// Leaves mutated point on top of stack.
    ///
    /// ```text
    /// BEFORE: _ (*address + self.stack_size() - 1)
    /// AFTER:  _ [value] (*address - 1)
    /// ```
    pub fn read_value_from_memory_leave_pointer(&self) -> Vec<LabelledInstruction> {
        load_words_from_memory_leave_pointer(self.stack_size())
    }

    /// The code to read a value of this type from memory.
    /// Pops pointer from stack.
    ///
    /// ```text
    /// BEFORE: _ (*address + self.stack_size() - 1)
    /// AFTER:  _ [value]
    /// ```
    pub fn read_value_from_memory_pop_pointer(&self) -> Vec<LabelledInstruction> {
        load_words_from_memory_pop_pointer(self.stack_size())
    }

    /// The code to write a value of this type to memory
    ///
    /// ```text
    /// BEFORE: _ [value] *address
    /// AFTER:  _ (*address + self.stack_size())
    /// ```
    pub fn write_value_to_memory_leave_pointer(&self) -> Vec<LabelledInstruction> {
        write_words_to_memory_leave_pointer(self.stack_size())
    }

    /// The code to write a value of this type to memory
    ///
    /// ```text
    /// BEFORE: _ [value] *address
    /// AFTER:  _
    /// ```
    pub fn write_value_to_memory_pop_pointer(&self) -> Vec<LabelledInstruction> {
        write_words_to_memory_pop_pointer(self.stack_size())
    }

    /// The code to read a value of this type from the specified input source
    ///
    /// ```text
    /// BEFORE: _
    /// AFTER:  _ [value]
    /// ```
    pub fn read_value_from_input(&self, input_source: InputSource) -> Vec<LabelledInstruction> {
        input_source.read_words(self.stack_size())
    }

    /// The code to write a value of this type to standard output
    ///
    /// ```text
    /// BEFORE: _ [value]
    /// AFTER:  _
    /// ```
    pub fn write_value_to_stdout(&self) -> Vec<LabelledInstruction> {
        crate::io::write_words(self.stack_size())
    }

    /// The code that compares two elements of this stack-size.
    ///
    /// ```text
    /// BEFORE: _ [self] [other]
    /// AFTER:  _ (self == other)
    /// ```
    pub fn compare_elem_of_stack_size(stack_size: usize) -> Vec<LabelledInstruction> {
        if stack_size == 0 {
            return triton_asm!(push 1);
        } else if stack_size == 1 {
            return triton_asm!(eq);
        }

        assert!(stack_size + 1 < NUM_OP_STACK_REGISTERS);
        let first_cmps = vec![triton_asm!(swap {stack_size + 1} eq); stack_size - 1].concat();
        let last_cmp = triton_asm!(swap 2 eq);
        let boolean_ands = triton_asm![mul; stack_size - 1];

        [first_cmps, last_cmp, boolean_ands].concat()
    }

    /// The code that compares two elements of this type.
    ///
    /// ```text
    /// BEFORE: _ [self] [other]
    /// AFTER:  _ (self == other)
    /// ```
    pub fn compare(&self) -> Vec<LabelledInstruction> {
        Self::compare_elem_of_stack_size(self.stack_size())
    }

    /// A string matching how the variant looks in source code.
    pub fn variant_name(&self) -> String {
        // This function is used to autogenerate snippets in the tasm-lang compiler
        match self {
            Self::Bool => "DataType::Bool".to_owned(),
            Self::U32 => "DataType::U32".to_owned(),
            Self::U64 => "DataType::U64".to_owned(),
            Self::U128 => "DataType::U128".to_owned(),
            Self::U160 => "DataType::U160".to_owned(),
            Self::U192 => "DataType::U192".to_owned(),
            Self::I128 => "DataType::I128".to_owned(),
            Self::Bfe => "DataType::BFE".to_owned(),
            Self::Xfe => "DataType::XFE".to_owned(),
            Self::Digest => "DataType::Digest".to_owned(),
            Self::List(elem_type) => {
                format!("DataType::List(Box::new({}))", elem_type.variant_name())
            }
            Self::VoidPointer => "DataType::VoidPointer".to_owned(),
            Self::Tuple(elements) => {
                let elements_as_variant_names =
                    elements.iter().map(|x| x.variant_name()).collect_vec();
                format!(
                    "DataType::Tuple(vec![{}])",
                    elements_as_variant_names.join(", ")
                )
            }
            Self::Array(array_type) => format!(
                "[{}; {}]",
                array_type.element_type.variant_name(),
                array_type.length
            ),
            Self::StructRef(struct_type) => format!("Box<{struct_type}>"),
        }
    }

    /// A collection of different data types, used for testing.
    #[cfg(test)]
    pub fn big_random_generatable_type_collection() -> Vec<Self> {
        vec![
            Self::Bool,
            Self::U32,
            Self::U64,
            Self::U128,
            Self::U160,
            Self::U192,
            Self::Bfe,
            Self::Xfe,
            Self::Digest,
            Self::VoidPointer,
            Self::Tuple(vec![Self::Bool]),
            Self::Tuple(vec![Self::Xfe, Self::Bool]),
            Self::Tuple(vec![Self::Xfe, Self::Digest]),
            Self::Tuple(vec![Self::Bool, Self::Bool]),
            Self::Tuple(vec![Self::Digest, Self::Xfe]),
            Self::Tuple(vec![Self::Bfe, Self::Xfe, Self::Digest]),
            Self::Tuple(vec![Self::Xfe, Self::Bfe, Self::Digest]),
            Self::Tuple(vec![Self::U64, Self::Digest, Self::Digest, Self::Digest]),
            Self::Tuple(vec![Self::Digest, Self::Digest, Self::Digest, Self::U64]),
            Self::Tuple(vec![Self::Digest, Self::Xfe, Self::U128, Self::Bool]),
        ]
    }

    pub fn random_elements(&self, count: usize) -> Vec<Vec<BFieldElement>> {
        (0..count)
            .map(|_| self.seeded_random_element(&mut rand::rng()))
            .collect()
    }

    pub fn seeded_random_element(&self, rng: &mut impl Rng) -> Vec<BFieldElement> {
        match self {
            Self::Bool => rng.random::<bool>().encode(),
            Self::U32 => rng.random::<u32>().encode(),
            Self::U64 => rng.random::<u64>().encode(),
            Self::U128 => rng.random::<u128>().encode(),
            Self::U160 => rng.random::<[u32; 5]>().encode(),
            Self::U192 => rng.random::<[u32; 6]>().encode(),
            Self::I128 => rng.random::<[u32; 4]>().encode(),
            Self::Bfe => rng.random::<BFieldElement>().encode(),
            Self::Xfe => rng.random::<XFieldElement>().encode(),
            Self::Digest => rng.random::<Digest>().encode(),
            Self::List(e) => {
                let len = rng.random_range(0..20);
                e.random_list(rng, len)
            }
            Self::Array(a) => Self::random_array(rng, a),
            Self::Tuple(tys) => tys
                .iter()
                .flat_map(|ty| ty.seeded_random_element(rng))
                .collect(),
            Self::VoidPointer => vec![rng.random()],
            Self::StructRef(_) => panic!("Random generation of structs is not supported"),
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
                "u160" => Self::U160,
                "u192" => Self::U192,
                "i128" => Self::I128,
                "bfe" => Self::Bfe,
                "xfe" => Self::Xfe,
                "digest" => Self::Digest,
                _ => anyhow::bail!("Could not parse {s} as a data type"),
            }
        };

        Ok(res)
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, arbitrary::Arbitrary)]
pub enum Literal {
    Bool(bool),
    U32(u32),
    U64(u64),
    U128(u128),
    I128(i128),
    Bfe(BFieldElement),
    Xfe(XFieldElement),
    Digest(Digest),
}

impl Literal {
    pub fn data_type(&self) -> DataType {
        match self {
            Self::Bool(_) => DataType::Bool,
            Self::U32(_) => DataType::U32,
            Self::U64(_) => DataType::U64,
            Self::U128(_) => DataType::U128,
            Self::I128(_) => DataType::I128,
            Self::Bfe(_) => DataType::Bfe,
            Self::Xfe(_) => DataType::Xfe,
            Self::Digest(_) => DataType::Digest,
        }
    }

    /// # Panics
    ///
    /// Panics if `self` is anything but an [extension field element](Self::Xfe).
    pub fn as_xfe(&self) -> XFieldElement {
        match self {
            Self::Xfe(xfe) => *xfe,
            _ => panic!("Expected XFE, got {self:?}"),
        }
    }

    /// The code to push the literal to the stack.
    pub fn push_to_stack_code(&self) -> Vec<LabelledInstruction> {
        let encoding = match self {
            Literal::Bool(x) => x.encode(),
            Literal::U32(x) => x.encode(),
            Literal::U64(x) => x.encode(),
            Literal::U128(x) => x.encode(),
            Literal::I128(x) => x.encode(),
            Literal::Bfe(x) => x.encode(),
            Literal::Xfe(x) => x.encode(),
            Literal::Digest(x) => x.encode(),
        };

        encoding
            .into_iter()
            .rev()
            .flat_map(|b| triton_asm!(push { b }))
            .collect()
    }

    /// # Panics
    ///
    /// - if the stack is too shallow
    /// - if the top of the stack does not contain an element of the requested type
    /// - if the element is incorrectly [`BFieldCodec`] encoded
    pub fn pop_from_stack(data_type: DataType, stack: &mut Vec<BFieldElement>) -> Self {
        match data_type {
            DataType::Bool => Self::Bool(pop_encodable(stack)),
            DataType::U32 => Self::U32(pop_encodable(stack)),
            DataType::U64 => Self::U64(pop_encodable(stack)),
            DataType::U128 => Self::U128(pop_encodable(stack)),
            DataType::I128 => Self::I128(pop_encodable(stack)),
            DataType::Bfe => Self::Bfe(pop_encodable(stack)),
            DataType::Xfe => Self::Xfe(pop_encodable(stack)),
            DataType::Digest => Self::Digest(pop_encodable(stack)),
            DataType::List(_)
            | DataType::Array(_)
            | DataType::Tuple(_)
            | DataType::VoidPointer
            | DataType::StructRef(_)
            | DataType::U160
            | DataType::U192 => unimplemented!(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_prelude::*;

    impl Literal {
        fn as_i128(&self) -> i128 {
            let Self::I128(val) = self else {
                panic!("Expected i128, got: {self:?}");
            };

            *val
        }
    }

    #[proptest]
    fn push_to_stack_leaves_value_on_top_of_stack(#[strategy(arb())] literal: Literal) {
        let code = triton_asm!(
            {&literal.push_to_stack_code()}
            halt
        );

        let mut vm_state = VMState::new(
            Program::new(&code),
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

    #[test]
    fn i128_sizes() {
        assert_eq!(4, DataType::I128.stack_size());
        assert_eq!(Some(4), DataType::I128.static_length());
    }

    #[proptest]
    fn non_negative_i128s_encode_like_u128s_prop(
        #[strategy(arb())]
        #[filter(#as_i128 >= 0i128)]
        as_i128: i128,
    ) {
        let as_u128: u128 = as_i128.try_into().unwrap();
        assert_eq!(
            Literal::U128(as_u128).push_to_stack_code(),
            Literal::I128(as_i128).push_to_stack_code()
        );
    }

    #[proptest]
    fn i128_literals_prop(val: i128) {
        let program = Literal::I128(val).push_to_stack_code();
        let program = triton_program!(
            {&program}
            halt
        );

        let mut vm_state = VMState::new(program, [].into(), [].into());
        vm_state.run().unwrap();
        let mut final_stack = vm_state.op_stack.stack;
        let popped = Literal::pop_from_stack(DataType::I128, &mut final_stack).as_i128();
        assert_eq!(val, popped);
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

/// Test [`DataType::compare`] by wrapping it in [`BasicSnippet`] and
/// implementing [`RustShadow`] for it.
#[cfg(test)]
mod compare_literals {
    use super::*;
    use crate::prelude::*;
    use crate::test_prelude::*;

    macro_rules! comparison_snippet {
        ($name:ident for tasm_ty $tasm_ty:ident and rust_ty $rust_ty:ident) => {
            #[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
            struct $name;

            impl BasicSnippet for $name {
                fn parameters(&self) -> Vec<(DataType, String)> {
                    ["left", "right"]
                        .map(|s| (DataType::$tasm_ty, s.to_string()))
                        .to_vec()
                }

                fn return_values(&self) -> Vec<(DataType, String)> {
                    vec![(DataType::Bool, "are_eq".to_string())]
                }

                fn entrypoint(&self) -> String {
                    let ty = stringify!($tasm_ty);
                    format!("tasmlib_test_compare_{ty}")
                }

                fn code(&self, _: &mut Library) -> Vec<LabelledInstruction> {
                    triton_asm!({self.entrypoint()}: {&DataType::$tasm_ty.compare()} return)
                }
            }

            impl Closure for $name {
                type Args = ($rust_ty, $rust_ty);

                fn rust_shadow(&self, stack: &mut Vec<BFieldElement>) {
                    let (right, left) = pop_encodable::<Self::Args>(stack);
                    push_encodable(stack, &(left == right));
                }

                fn pseudorandom_args(
                    &self,
                    seed: [u8; 32],
                    _: Option<BenchmarkCase>
                ) -> Self::Args {
                    // almost certainly different arguments, comparison gives `false`
                    StdRng::from_seed(seed).random()
                }

                fn corner_case_args(&self) -> Vec<Self::Args> {
                    // identical arguments, comparison gives `true`
                    vec![Self::Args::default()]
                }
            }
        };
    }

    // stack size == 1
    comparison_snippet!(CompareBfes for tasm_ty Bfe and rust_ty BFieldElement);

    // stack size > 1
    comparison_snippet!(CompareDigests for tasm_ty Digest and rust_ty Digest);

    #[test]
    fn test() {
        ShadowedClosure::new(CompareBfes).test();
        ShadowedClosure::new(CompareDigests).test();
    }

    #[test]
    fn bench() {
        ShadowedClosure::new(CompareBfes).bench();
        ShadowedClosure::new(CompareDigests).bench();
    }
}
