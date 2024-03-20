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
