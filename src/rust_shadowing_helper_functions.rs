use std::collections::HashMap;

use num::{One, Zero};
use twenty_first::shared_math::b_field_element::BFieldElement;

pub fn list_pop<const N: usize>(
    list_pointer: BFieldElement,
    memory: &mut HashMap<BFieldElement, BFieldElement>,
) -> [BFieldElement; N] {
    let init_list_length = memory[&list_pointer];
    assert!(!init_list_length.is_zero(), "Stack underflow");
    memory.get_mut(&list_pointer).unwrap().decrement();

    let mut ret = [BFieldElement::zero(); N];
    for i in 0..N {
        let key = list_pointer
            + BFieldElement::one()
            + BFieldElement::new(N as u64) * (init_list_length - BFieldElement::one())
            + BFieldElement::new(i as u64);
        println!("indexing into {key}");
        ret[i] = memory[&key];
    }

    ret
}

pub fn list_push<const N: usize>(
    list_pointer: BFieldElement,
    value: [BFieldElement; N],
    memory: &mut HashMap<BFieldElement, BFieldElement>,
) {
    let init_list_length = memory[&list_pointer];
    memory.get_mut(&list_pointer).unwrap().increment();

    for i in 0..N {
        memory.insert(
            list_pointer
                + BFieldElement::one()
                + BFieldElement::new(N as u64) * init_list_length
                + BFieldElement::new(i as u64),
            value[i],
        );
    }
}

pub fn list_new(list_pointer: BFieldElement, memory: &mut HashMap<BFieldElement, BFieldElement>) {
    memory.insert(list_pointer, BFieldElement::zero());
}

pub fn list_set_length(
    list_pointer: BFieldElement,
    new_length: usize,
    memory: &mut HashMap<BFieldElement, BFieldElement>,
) {
    memory.insert(list_pointer, BFieldElement::new(new_length as u64));
}
