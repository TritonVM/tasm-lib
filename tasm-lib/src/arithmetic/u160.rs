pub mod div_mod;
pub mod lt;
pub mod overflowing_add;
pub mod safe_add;
pub mod safe_mul;

/// Convert a `u128` to a u160 representation with the same value.
#[cfg(test)]
pub(super) fn u128_to_u160(x: u128) -> [u32; 5] {
    [
        (x & (u32::MAX as u128)) as u32,
        ((x >> 32) & (u32::MAX as u128)) as u32,
        ((x >> 64) & (u32::MAX as u128)) as u32,
        ((x >> 96) & (u32::MAX as u128)) as u32,
        0,
    ]
}

/// Convert a `u128` to a u160 representation of $2^{32}$ times the input.
#[cfg(test)]
pub(super) fn u128_to_u160_shl_32(x: u128) -> [u32; 5] {
    [
        0,
        (x & (u32::MAX as u128)) as u32,
        ((x >> 32) & (u32::MAX as u128)) as u32,
        ((x >> 64) & (u32::MAX as u128)) as u32,
        ((x >> 96) & (u32::MAX as u128)) as u32,
    ]
}

/// Convert a `u128` to a u160 representation of $x * 2^{32} + 2^{32} - 1$.
#[cfg(test)]
pub(super) fn u128_to_u160_shl_32_lower_limb_filled(x: u128) -> [u32; 5] {
    [
        u32::MAX,
        (x & (u32::MAX as u128)) as u32,
        ((x >> 32) & (u32::MAX as u128)) as u32,
        ((x >> 64) & (u32::MAX as u128)) as u32,
        ((x >> 96) & (u32::MAX as u128)) as u32,
    ]
}
