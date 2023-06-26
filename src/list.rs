use std::fmt::Display;

pub mod higher_order;
pub mod multiset_equality;
pub mod range;
pub mod safe_u32;
pub mod unsafe_u32;

pub mod contiguous_list;

#[derive(Clone, Debug)]
pub enum ListType {
    Safe,
    Unsafe,
}

impl Display for ListType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ListType::Safe => write!(f, "safe"),
            ListType::Unsafe => write!(f, "unsafe"),
        }
    }
}
