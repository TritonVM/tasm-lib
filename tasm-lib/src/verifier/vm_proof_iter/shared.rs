use crate::data_type::DataType;
use crate::data_type::StructType;

pub fn vm_proof_iter_type() -> StructType {
    let name = "VmProofIter".to_string();
    let fields = vec![("current_item_pointer".to_string(), DataType::Bfe)];

    StructType { name, fields }
}
