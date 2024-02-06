use std::collections::HashMap;
use std::fmt::Display;

use triton_vm::prelude::BFieldElement;

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
use crate::rust_shadowing_helper_functions::{self, safe_list, unsafe_list};
use crate::traits::basic_snippet::BasicSnippet;

pub mod contiguous_list;
pub mod higher_order;
pub mod multiset_equality;
pub mod range;
pub mod safeimplu32;
pub mod sum_bfes;
pub mod swap_unchecked;
pub mod unsafeimplu32;

#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
pub enum ListType {
    Safe,
    Unsafe,
}

impl ListType {
    /// Return the number of words this list type uses for bookkeeping
    pub fn metadata_size(&self) -> usize {
        match self {
            ListType::Safe => 2,
            ListType::Unsafe => 1,
        }
    }

    /* Get snippets */
    pub fn new_list_snippet(&self, data_type: DataType) -> Box<dyn BasicSnippet> {
        match self {
            ListType::Safe => Box::new(SafeNew { data_type }),
            ListType::Unsafe => Box::new(UnsafeNew { data_type }),
        }
    }

    pub fn push_snippet(&self, data_type: DataType) -> Box<dyn BasicSnippet> {
        match self {
            ListType::Safe => Box::new(SafePush { data_type }),
            ListType::Unsafe => Box::new(UnsafePush { data_type }),
        }
    }

    pub fn pop_snippet(&self, data_type: DataType) -> Box<dyn BasicSnippet> {
        match self {
            ListType::Safe => Box::new(SafePop { data_type }),
            ListType::Unsafe => Box::new(UnsafePop { data_type }),
        }
    }

    pub fn get_snippet(&self, data_type: DataType) -> Box<dyn BasicSnippet> {
        match self {
            ListType::Safe => Box::new(SafeGet { data_type }),
            ListType::Unsafe => Box::new(UnsafeGet { data_type }),
        }
    }

    pub fn set_snippet(&self, data_type: DataType) -> Box<dyn BasicSnippet> {
        match self {
            ListType::Safe => Box::new(SafeSet { data_type }),
            ListType::Unsafe => Box::new(UnsafeSet { data_type }),
        }
    }

    pub fn length_snippet(&self, data_type: DataType) -> Box<dyn BasicSnippet> {
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

    pub fn rust_shadowing_load_list_with_copy_element<const ELEMENT_SIZE: usize>(
        &self,
        list_pointer: BFieldElement,
        memory: &HashMap<BFieldElement, BFieldElement>,
    ) -> Vec<[BFieldElement; ELEMENT_SIZE]> {
        match self {
            ListType::Safe => safe_list::load_safe_list_with_copy_elements(list_pointer, memory),
            ListType::Unsafe => {
                unsafe_list::load_unsafe_list_with_copy_elements(list_pointer, memory)
            }
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

    pub fn rust_shadowing_insert_random_list(
        &self,
        element_type: &DataType,
        list_pointer: BFieldElement,
        list_length: usize,
        memory: &mut HashMap<BFieldElement, BFieldElement>,
    ) {
        match self {
            ListType::Safe => rust_shadowing_helper_functions::safe_list::safe_insert_random_list(
                element_type,
                list_pointer,
                list_length as u32,
                list_length,
                memory,
            ),
            ListType::Unsafe => {
                rust_shadowing_helper_functions::unsafe_list::unsafe_insert_random_list(
                    element_type,
                    list_pointer,
                    list_length,
                    memory,
                )
            }
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
