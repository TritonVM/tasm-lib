use crate::data_type::DataType;
use crate::data_type::StructType;

// TODO: This *must* match the type of `Claim` used in Triton VM. It would be preferable if
// it could be derived/generated from that type at the source.
// The tests below *should* verify that this type is consistent with the one used in TVM.
pub(crate) fn claim_type() -> StructType {
    StructType {
        name: "Claim".to_owned(),
        fields: vec![
            ("program_digest".to_owned(), DataType::Digest),
            ("input".to_owned(), DataType::List(Box::new(DataType::Bfe))),
            ("output".to_owned(), DataType::List(Box::new(DataType::Bfe))),
        ],
    }
}
