/// The number of VM words required to store the metadata / bookkeeping data of a list.
pub const LIST_METADATA_SIZE: usize = 1;

pub mod contiguous_list;
pub mod get;
pub mod higher_order;
pub mod horner_evaluation_dynamic_length;
pub mod length;
pub mod multiset_equality;
pub mod new;
pub mod pop;
pub mod push;
pub mod range;
pub mod set;
pub mod set_length;
pub mod split_off;
pub mod sum_bfes;
pub mod sum_xfes;
pub mod swap_unchecked;
