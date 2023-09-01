use std::collections::HashMap;

use triton_vm::BFieldElement;
use twenty_first::shared_math::bfield_codec::BFieldCodec;

/// A RamBuilder faciliates preparing RAM into a good state.
pub struct RamBuilder {
    memory: HashMap<BFieldElement, BFieldElement>,
}

impl RamBuilder {
    /// Return a fresh RamBuilder object.
    pub fn start() -> Self {
        Self {
            memory: HashMap::new(),
        }
    }

    /// Load the given object into memory and increment the memory allocator accordlingly.
    /// Return the address of the loaded object.
    pub fn load<T: BFieldCodec>(&mut self, object: &T) -> BFieldElement {
        // find first free location by reading the memory allocator
        let memory_allocator = BFieldElement::new(0);
        let start_address = *self
            .memory
            .get(&memory_allocator)
            .unwrap_or(&BFieldElement::new(1));
        let mut working_address = start_address;

        // load the struct as it is encoded to this location
        for bfe in object.encode() {
            self.memory.insert(working_address, bfe);
            working_address += BFieldElement::new(1);
        }

        // increase the memory allocator
        self.memory.insert(memory_allocator, working_address);

        // return a pointer to the inserted object
        start_address
    }

    /// Return the prepared memory object.
    pub fn finish(self) -> HashMap<BFieldElement, BFieldElement> {
        self.memory
    }
}
