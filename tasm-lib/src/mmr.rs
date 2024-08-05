pub mod bag_peaks;
pub mod calculate_new_peaks_from_append;
pub mod calculate_new_peaks_from_leaf_mutation;
pub mod leaf_index_to_mt_index_and_peak_index;
pub mod verify_from_memory;
pub mod verify_from_secret_in_leaf_index_on_stack;
pub mod verify_from_secret_in_secret_leaf_index;
pub mod verify_mmr_successor;

pub const MAX_MMR_HEIGHT: usize = 64;
