use std::fmt::Display;

use crate::data_type::DataType;
use crate::list::safeimplu32::get::SafeGet;
use crate::list::safeimplu32::length::Length as SafeLength;
use crate::list::safeimplu32::new::SafeNew;
use crate::list::safeimplu32::pop::SafePop;
use crate::list::safeimplu32::push::SafePush;
use crate::list::safeimplu32::set::SafeSet;
use crate::list::safeimplu32::set_length::SafeSetLength;
use crate::list::unsafeimplu32::get::UnsafeGet;
use crate::list::unsafeimplu32::length::Length as UnsafeLength;
use crate::list::unsafeimplu32::new::UnsafeNew;
use crate::list::unsafeimplu32::pop::UnsafePop;
use crate::list::unsafeimplu32::push::UnsafePush;
use crate::list::unsafeimplu32::set::UnsafeSet;
use crate::list::unsafeimplu32::set_length::UnsafeSetLength;
use crate::snippet::BasicSnippet;

pub mod contiguous_list;
pub mod higher_order;
pub mod multiset_equality;
pub mod range;
pub mod safeimplu32;
pub mod unsafeimplu32;

#[derive(Clone, Debug)]
pub enum ListType {
    Safe,
    Unsafe,
}

impl ListType {
    /// the number of words this list type uses for bookkeeping
    pub fn safety_offset(&self) -> usize {
        match self {
            ListType::Safe => 2,
            ListType::Unsafe => 1,
        }
    }

    pub fn new_list(&self, data_type: DataType) -> Box<dyn BasicSnippet> {
        match self {
            ListType::Safe => Box::new(SafeNew { data_type }),
            ListType::Unsafe => Box::new(UnsafeNew { data_type }),
        }
    }

    pub fn push(&self, data_type: DataType) -> Box<dyn BasicSnippet> {
        match self {
            ListType::Safe => Box::new(SafePush { data_type }),
            ListType::Unsafe => Box::new(UnsafePush { data_type }),
        }
    }

    pub fn pop(&self, data_type: DataType) -> Box<dyn BasicSnippet> {
        match self {
            ListType::Safe => Box::new(SafePop { data_type }),
            ListType::Unsafe => Box::new(UnsafePop { data_type }),
        }
    }

    pub fn get(&self, data_type: DataType) -> Box<dyn BasicSnippet> {
        match self {
            ListType::Safe => Box::new(SafeGet { data_type }),
            ListType::Unsafe => Box::new(UnsafeGet { data_type }),
        }
    }

    pub fn set(&self, data_type: DataType) -> Box<dyn BasicSnippet> {
        match self {
            ListType::Safe => Box::new(SafeSet { data_type }),
            ListType::Unsafe => Box::new(UnsafeSet { data_type }),
        }
    }

    pub fn length(&self, data_type: DataType) -> Box<dyn BasicSnippet> {
        match self {
            ListType::Safe => Box::new(SafeLength { data_type }),
            ListType::Unsafe => Box::new(UnsafeLength { data_type }),
        }
    }

    pub fn set_length(&self, data_type: DataType) -> Box<dyn BasicSnippet> {
        match self {
            ListType::Safe => Box::new(SafeSetLength { data_type }),
            ListType::Unsafe => Box::new(UnsafeSetLength { data_type }),
        }
    }

    /* Rust-shadowing helper functions */
    pub fn rust_shadowing_get(
        &self,
        list_pointer: BFieldElement,
        index: usize,
        memory: &HashMap<BFieldElement, BFieldElement>,
        element_size: usize,
    ) -> Vec<BFieldElement> {
        match self {
            ListType::Safe => rust_shadowing_helper_functions::safe_list::safe_list_get(
                list_pointer,
                index,
                memory,
                element_size,
            ),
            ListType::Unsafe => rust_shadowing_helper_functions::unsafe_list::unsafe_list_get(
                list_pointer,
                index,
                memory,
                element_size,
            ),
        }
    }

    pub fn rust_shadowing_set(
        &self,
        list_pointer: BFieldElement,
        index: usize,
        value: Vec<BFieldElement>,
        memory: &mut HashMap<BFieldElement, BFieldElement>,
    ) {
        match self {
            ListType::Safe => rust_shadowing_helper_functions::safe_list::safe_list_set(
                list_pointer,
                index,
                value,
                memory,
            ),
            ListType::Unsafe => rust_shadowing_helper_functions::unsafe_list::unsafe_list_set(
                list_pointer,
                index,
                value,
                memory,
            ),
        }
    }
}

impl Display for ListType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ListType::Safe => write!(f, "safeimplu32"),
            ListType::Unsafe => write!(f, "unsafeimplu32"),
        }
    }
}
