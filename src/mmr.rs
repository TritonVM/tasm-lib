pub mod bag_peaks;
pub mod calculate_new_peaks_from_append;
pub mod calculate_new_peaks_from_leaf_mutation;
pub mod data_index_to_node_index;
pub mod get_height_from_data_index;
pub mod leaf_index_to_mt_index;
pub mod left_child;
pub mod leftmost_ancestor;
pub mod load_from_secret_in_then_verify;
pub mod non_leaf_nodes_left;
pub mod right_child;
pub mod right_child_and_height;
pub mod right_lineage_count_and_own_height;
pub mod right_lineage_length;
pub mod verify_from_memory;
pub mod verify_from_secret_in;

pub const MAX_MMR_HEIGHT: usize = 64;
