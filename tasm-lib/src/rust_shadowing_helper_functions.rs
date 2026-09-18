pub mod array;
pub mod claim;
pub mod dyn_malloc;
pub mod list;

/// Count the number of non-leaf nodes that were inserted *prior* to
/// the insertion of this leaf.
pub fn non_leaf_nodes_left(leaf_index: u64) -> u64 {
    // This formula is derived as follows:
    // To get the heights of peaks before this leaf index was inserted, bit-decompose
    // the number of leaves before it was inserted.
    // Number of leaves in tree of height h = 2^h
    // Number of nodes in tree of height h = 2^(h + 1) - 1
    // Number of non-leaves is `#(nodes) - #(leaves)`.
    // Thus: f(x) = sum_{h}(2^h - 1)

    // An upper limit for the loop iterator is the log_2_floor(leaf_index)
    let log_2_floor_plus_one = u64::BITS - leaf_index.leading_zeros();
    let mut h = 0;
    let mut ret = 0;
    while h != log_2_floor_plus_one {
        let pow = (1 << h) & leaf_index;
        if pow != 0 {
            ret += pow - 1;
        }
        h += 1;
    }

    ret
}
