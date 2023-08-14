/// A Function can modify the top of the stack, and can read and
/// extend memory. Specifically: any memory writes have to happen
/// to addresses larger than the dynamic memory allocator and the
/// dynamic memory allocator value has to be updated accordingly.
pub trait Function : SnippetBase {
    fn rust_shadow(stack: &mut Vec<BFieldElement>,
        memory: &mut HashMap<BFieldElement,BFieldElement>)
}