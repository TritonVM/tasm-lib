/// A Closure modifies the top of the stack without reading
/// memory.
trait Closure: SnippetBase {
    fn rust_shadow(&mut stack: Vec<BFieldElement>);
}
