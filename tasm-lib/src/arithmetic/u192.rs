pub mod overflowing_add;
pub mod safe_add;

#[cfg(test)]
pub(crate) type U192 = [u32; 6];

/// Convert a `u128` to a u192 representation with the same value.
#[cfg(test)]
pub(super) fn u128_to_u192(x: u128) -> U192 {
    [
        (x & (u32::MAX as u128)) as u32,
        ((x >> 32) & (u32::MAX as u128)) as u32,
        ((x >> 64) & (u32::MAX as u128)) as u32,
        ((x >> 96) & (u32::MAX as u128)) as u32,
        0,
        0,
    ]
}

/// Multiply input value with $2^{64}$ and return result as a u192
#[cfg(test)]
pub(super) fn u128_to_u192_shl64(x: u128) -> U192 {
    [
        0,
        0,
        (x & (u32::MAX as u128)) as u32,
        ((x >> 32) & (u32::MAX as u128)) as u32,
        ((x >> 64) & (u32::MAX as u128)) as u32,
        ((x >> 96) & (u32::MAX as u128)) as u32,
    ]
}

#[cfg(test)]
pub(super) fn to_u192(hi: u128, lo: u64) -> U192 {
    [
        (lo & u32::MAX as u64) as u32,
        (lo >> 32) as u32,
        (hi & (u32::MAX as u128)) as u32,
        ((hi >> 32) & (u32::MAX as u128)) as u32,
        ((hi >> 64) & (u32::MAX as u128)) as u32,
        (hi >> 96) as u32,
    ]
}
