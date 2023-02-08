pub mod input;
pub mod safe_list;
pub mod unsafe_list;

// TODO: Remove this when twenty-first gets a new version with this function in it
pub fn leaf_index_to_mt_index_and_peak_index(leaf_index: u128, leaf_count: u128) -> (u128, u32) {
    // This assert also guarantees that leaf_count is never zero
    assert!(
        leaf_index < leaf_count,
        "Leaf index must be stricly smaller than leaf count. Got leaf_index = {leaf_index}, leaf_count = {leaf_count}"
    );

    let max_tree_height = u128::BITS - leaf_count.leading_zeros() - 1;
    let mut h = max_tree_height;
    let mut ret = leaf_index;
    let mut pow;
    let mut peak_index: u32 = 0;
    loop {
        pow = 1 << h;
        let maybe_pow = pow & leaf_count;
        if h == 0 || (ret < maybe_pow) {
            break;
        }
        ret -= maybe_pow;
        peak_index += (maybe_pow != 0) as u32;
        h -= 1;
    }

    ret += pow;

    (ret, peak_index)
}
