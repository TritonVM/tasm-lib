use triton_vm::prelude::*;

use crate::data_type::ArrayType;
use crate::data_type::DataType;
use crate::data_type::StructType;

pub(super) fn challenges_data_type(total_number_of_challenges: usize) -> DataType {
    DataType::StructRef(StructType {
        name: "Challenges".to_owned(),
        fields: vec![(
            "challenges".to_owned(),
            DataType::Array(Box::new(ArrayType {
                element_type: DataType::Xfe,
                length: total_number_of_challenges,
            })),
        )],
    })
}

pub(crate) fn conventional_challenges_pointer() -> BFieldElement {
    // By convention, the challenges are stored in the fourth-to-last
    // memory page
    BFieldElement::new(((1 << 32) - 4) * (1 << 32))
}
