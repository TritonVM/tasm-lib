use std::fmt::Display;
use std::fmt::Formatter;
use std::str::FromStr;

use itertools::Itertools;
use num::One;
use num::Zero;
use rand::random;
use rand::thread_rng;
use rand::Rng;
use triton_vm::prelude::*;

use crate::io::InputSource;
use crate::memory::load_words_from_memory_leave_pointer;
use crate::memory::load_words_from_memory_pop_pointer;
use crate::memory::write_words_to_memory_leave_pointer;
use crate::memory::write_words_to_memory_pop_pointer;
use crate::DIGEST_LENGTH;

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
            DataType::Digest => DIGEST_LENGTH,
            DataType::List(_) => 1,
            DataType::Array(_) => 1,
            DataType::VoidPointer => 1,
            DataType::Tuple(t) => t.iter().map(|dt| dt.stack_size()).sum(),
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

    pub fn seeded_random_elements(
        &self,
        count: usize,
        rng: &mut impl Rng,
    ) -> Vec<Vec<BFieldElement>> {
        match self {
            DataType::Bool => {
                let bools: Vec<bool> = (0..count).map(|_| rng.gen_bool(0.5)).collect();
                bools
                    .iter()
                    .map(|x| vec![BFieldElement::new(*x as u64)])
                    .collect_vec()
            }
            DataType::U32 => (0..count)
                .map(|_| vec![BFieldElement::new(rng.gen_range(0..=u32::MAX as u64))])
                .collect_vec(),
            DataType::U64 => (0..2 * count)
                .map(|_| BFieldElement::new(rng.gen_range(0..=u32::MAX as u64)))
                .tuples()
                .map(|(a, b)| vec![a, b])
                .collect_vec(),
            DataType::U128 => (0..4 * count)
                .map(|_| BFieldElement::new(rng.gen_range(0..=u32::MAX as u64)))
                .tuples()
                .map(|(a, b, c, d)| vec![a, b, c, d])
                .collect_vec(),
            DataType::Bfe => (0..count)
                .map(|_| vec![BFieldElement::new(rng.gen_range(0..=BFieldElement::MAX))])
                .collect_vec(),
            DataType::Xfe => (0..count)
                .map(|_| vec![random(), random(), random()])
                .collect_vec(),
            DataType::Digest => (0..DIGEST_LENGTH * count)
                .map(|_| BFieldElement::new(rng.gen_range(0..=BFieldElement::MAX)))
                .tuples()
                .map(|(a, b, c, d, e)| vec![a, b, c, d, e])
                .collect_vec(),
            DataType::List(_) => panic!("Random generation of lists is not supported"),
            DataType::Array(_) => panic!("Random generation of arrays is not supported"),
            DataType::VoidPointer => (0..count)
                .map(|_| vec![random::<BFieldElement>()])
                .collect_vec(),
            DataType::Tuple(v) => (0..count)
                .map(|_| v.iter().flat_map(|dt| dt.random_elements(1)).concat())
                .collect(),
            DataType::StructRef(_) => panic!("Random generation of structs is not supported"),
        }
    }

    pub fn random_elements(&self, count: usize) -> Vec<Vec<BFieldElement>> {
        let mut rng = thread_rng();
        self.seeded_random_elements(count, &mut rng)
    }
}

impl FromStr for DataType {
    type Err = anyhow::Error;

    // This implementation must be the inverse of `label_friendly_name`
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        use DataType::*;

        let res = if s.starts_with("list_L") && s.ends_with('R') {
            let inner = &s[6..s.len() - 1];
            let inner = FromStr::from_str(inner)?;
            DataType::List(Box::new(inner))
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
                "void_pointer" => VoidPointer,
                "bool" => Bool,
                "u32" => U32,
                "u64" => U64,
                "u128" => U128,
                "bfe" => Bfe,
                "xfe" => Xfe,
                "digest" => Digest,
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
}
