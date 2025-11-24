//! Contains an interface for defining composable snippets of Triton assembly.

use std::collections::HashMap;
use std::fmt::Display;
use std::fmt::Formatter;
use std::hash::Hash;
use std::hash::Hasher;

use num_traits::ConstZero;
use num_traits::Zero;
use triton_vm::isa::instruction::AnInstruction;
use triton_vm::isa::op_stack::NUM_OP_STACK_REGISTERS;
use triton_vm::prelude::*;

use crate::prelude::*;
use crate::push_encodable;

/// A (basic) snippet represents a piece of code that can be run in
/// [Triton VM](triton_vm).
///
/// Generally speaking, it’s not possible to run a snippet stand-alone. Rather,
/// snippets are (generally) intended to be used in combination with other
/// snippets. Together, they can make up a full Triton VM program.
///
/// ### Example
///
/// ```
/// # use tasm_lib::prelude::*;
/// # use tasm_lib::triton_vm::prelude::*;
/// /// Checks whether some u32 is odd.
/// struct IsOdd;
///
/// impl BasicSnippet for IsOdd {
///     fn parameters(&self) -> Vec<(DataType, String)> {
///         vec![(DataType::U32, "x".to_string())]
///     }
///
///     fn return_values(&self) -> Vec<(DataType, String)> {
///         vec![(DataType::Bool, "x % 2".to_string())]
///     }
///
///     fn entrypoint(&self) -> String {
///         "is_odd".to_string()
///     }
///
///     fn code(&self, _: &mut Library) -> Vec<LabelledInstruction> {
///         triton_asm!(
///             // BEFORE: _ x
///             // AFTER:  _ (x%2)
///             {self.entrypoint()}:
///                 push 2   // _ x 2
///                 pick 1   // _ 2 x
///                 div_mod  // _ (x//2) (x%2)
///                 pick 1   // _ (x%2) (x//2)
///                 pop 1    // _ (x%2)
///                 return
///         )
///     }
/// }
/// ```
///
/// ### A Caveat Regarding Type Safety
///
/// The methods [`parameters`](Self::parameters) and
/// [`return_values`](Self::return_values) define some expected and returned
/// [`DataType`]s, respectively. Note that there is no mechanism that enforces
/// any guarantees about these types. Notably:
/// - tasm-lib never checks any kind of type consistency or correctness,
///   neither at run- nor compile-time
/// - [Triton VM](triton_vm) does not know anything about types
///
/// While the types defined in these respective methods streamline a snippet’s
/// testability and simplify the required reasoning when composing them, they
/// are _only_ used as a development aid.
///
/// ### Dyn-Compatibility
///
/// This trait is [dyn-compatible] (previously known as “object safe”).
///
/// [dyn-compatible]: https://doc.rust-lang.org/reference/items/traits.html#dyn-compatibility
pub trait BasicSnippet {
    /// The parameters expected by this snippet.
    ///
    /// The parameters are expected to be on top of Triton VM’s stack when
    /// the snippet is called. If this is not the case, behavior of the snippet
    /// is generally undefined.
    ///
    /// See also the
    /// [caveat regarding type safety](Self#a-caveat-regarding-type-safety).
    fn parameters(&self) -> Vec<(DataType, String)>;

    /// The (types of the) values this snippet computes.
    ///
    /// The return values are at the top of the stack when the snippet returns.
    ///
    /// See also the
    /// [caveat regarding type safety](Self#a-caveat-regarding-type-safety).
    fn return_values(&self) -> Vec<(DataType, String)>;

    /// The name of the snippet as a possible target for Triton VM’s
    /// instruction `call`.
    fn entrypoint(&self) -> String;

    /// The Triton Assembly that defines this snippet.
    ///
    /// Snippet authors are responsible for the following:
    /// 1. Only use the pre-conditions
    ///     - listed explicitly in the snippet’s documentation, and
    ///     - implied by [`parameters`](Self::parameters).
    /// 1. Uphold all post-conditions
    ///     - listed explicitly in the snippet’s documentation, and
    ///     - implied by [`return_values`](Self::return_values).
    ///
    /// Notably, no type checking of any kind is performed. See also the
    /// [caveat regarding type safety](Self#a-caveat-regarding-type-safety).
    fn code(&self, library: &mut Library) -> Vec<LabelledInstruction>;

    /// Adds “type hints” to the [code](Self::code).
    ///
    /// This method should _not_ be overwritten by implementors of the trait.
    ///
    /// Note that type hints do not change the behavior of Triton VM in any
    /// way. They are only useful in debuggers, like the
    /// [Triton TUI](https://crates.io/crates/triton-tui).
    fn annotated_code(&self, library: &mut Library) -> Vec<LabelledInstruction> {
        fn generate_hints_for_input_values(inputs: Vec<(DataType, String)>) -> Vec<String> {
            let mut input_hints = vec![];
            let mut stack_depth = 0;
            for (data_type, name) in inputs.into_iter().rev() {
                let stack_size = data_type.stack_size();
                if stack_size.is_zero() {
                    continue;
                }

                let data_name = data_type.label_friendly_name();

                // TODO: Remove this once. the Triton-VM parser becomes more
                // permissive WRT variable names
                let name = name
                    .replace(|c: char| !c.is_alphanumeric(), "_")
                    .to_ascii_lowercase();

                input_hints.push(format!(
                    "hint {name}: {data_name} = stack[{stack_depth}..{}]",
                    stack_depth + stack_size
                ));
                stack_depth += stack_size;
            }

            input_hints
        }

        let code = self.code(library);
        let Some((entrypoint, snippet_body)) = code.split_first() else {
            return code;
        };
        let entrypoint = entrypoint.to_string();
        let observed_entrypoint = entrypoint.trim_end_matches(':');
        if *observed_entrypoint != self.entrypoint() {
            return code;
        }

        let input_hints = generate_hints_for_input_values(self.parameters());

        triton_asm! {
            {observed_entrypoint}:
                {&input_hints}
                {&snippet_body}
        }
    }

    #[cfg(test)]
    fn link_for_isolated_run_populated_static_memory(
        &self,
        words_statically_allocated: u32,
    ) -> Vec<LabelledInstruction> {
        let mut library = Library::with_preallocated_memory(words_statically_allocated);
        let entrypoint = self.entrypoint();
        let function_body = self.annotated_code(&mut library);
        let library_code = library.all_imports();

        // The TASM code is always run through a function call, so the 1st instruction is a call to
        // the function in question.
        let code = triton_asm!(
            call {entrypoint}
            halt

            {&function_body}
            {&library_code}
        );

        code
    }

    #[doc(hidden)]
    fn link_for_isolated_run(&self) -> Vec<LabelledInstruction> {
        let mut library = Library::empty();
        let entrypoint = self.entrypoint();
        let function_body = self.annotated_code(&mut library);
        let library_code = library.all_imports();

        // The TASM code is always run through a function call, so the 1st instruction is a call to
        // the function in question.
        let code = triton_asm!(
            call {entrypoint}
            halt

            {&function_body}
            {&library_code}
        );

        code
    }

    /// Initial stack on program start, when the snippet runs in isolation.
    #[doc(hidden)]
    fn init_stack_for_isolated_run(&self) -> Vec<BFieldElement> {
        let code = self.link_for_isolated_run();
        let program = Program::new(&code);

        let mut stack = vec![];
        push_encodable(&mut stack, &program.hash());
        stack.resize(NUM_OP_STACK_REGISTERS, BFieldElement::ZERO);

        stack
    }

    /// The size difference of the stack as a result of executing this snippet.
    fn stack_diff(&self) -> isize {
        let io_size = |io: Vec<(DataType, _)>| -> isize {
            let size = io.into_iter().map(|(ty, _)| ty.stack_size()).sum::<usize>();
            size.try_into().unwrap()
        };

        io_size(self.return_values()) - io_size(self.parameters())
    }

    /// Contains an entry for every sign off.
    ///
    /// Many of the snippets defined in this TASM library are critical for the
    /// consensus logic of the blockchain [Neptune Cash](https://neptune.cash).
    /// Therefore, it is paramount that the snippets are free of errors. In order
    /// to catch as many errors as possible, the snippets are reviewed by as many
    /// developers as possible. The requirements of such a review are listed here.
    ///
    /// A reviewer can (and should) sign off on any snippet they have reviewed and
    /// for which they found no defects. This is done by adding that snippet's
    /// [fingerprint] (at the time) to the overriding implementation of this method
    /// on that snippet.
    ///
    /// Together with the tools [`git blame`][blame] and cryptographic
    /// [signing] of commits, this makes sign-offs traceable. It also guarantees
    /// that changes to snippets that have already been signed-off are easy to
    /// detect.
    ///
    /// # For Reviewers
    ///
    /// ## Modifying snippets
    ///
    /// While the primary intention of the review process is to _review_ a snippet,
    /// there are circumstances under which modifying it is acceptable.
    ///
    /// Modifying a snippet to simplify reviewing that snippet is fair game. A
    /// common example of this case is replacing a `swap`-juggle chain with a few
    /// `pick`s & `place`s.
    ///
    /// Modifying a snippet in order to improve performance should only happen if
    /// the performance impact is meaningful. The currently agreed-upon threshold
    /// is 0.5% of at least one consensus program.
    ///
    /// It is acceptable, and can be desired, to modify a snippet by including
    /// assumption checks. For example, if the snippet's pre-conditions require
    /// some input to fall within a certain range, it is fine to add a corresponding
    /// range check to the snippet.
    /// Removing existing checks of such nature is considered bad practice.
    ///
    /// In either case, modifying a snippet that has already been reviewed and
    /// signed off by someone else in a way that alters its [fingerprint] requires
    /// their consent.
    ///
    /// ## Checklist
    ///
    /// Use the following checklist to guide your review. Signing off on a snippet
    /// means that in your eyes, all points on this checklist are true.
    ///
    /// - the snippet's documentation lists pre- and post-conditions
    /// - the snippet makes no assumptions outside the stated pre-conditions
    /// - given all pre-conditions, all post-conditions are met
    /// - whenever this snippet calls another snippet, all of that other snippet's
    ///   pre-conditions are met
    /// - all dynamic memory offsets are range-checked before they are used
    /// - each field accessor is used at most once per struct instance, or
    ///   range-checked before each use
    /// - reading from non-deterministically initialized memory only happens from
    ///   the region specified in the [memory convention]
    /// - memory-writes only happen outside of page 0 (see [memory convention])
    ///
    /// ## Documentation Template
    ///
    /// If a snippet you are reviewing is not (properly) documented yet, you can use
    /// the following template to document the type implementing [`BasicSnippet`].
    ///
    /// ````text
    /// /// ### Behavior
    /// ///
    /// /// ```text
    /// /// BEFORE: _
    /// /// AFTER:  _
    /// /// ```
    /// ///
    /// /// ### Preconditions
    /// ///
    /// /// - condition
    /// ///
    /// /// ### Postconditions
    /// ///
    /// /// - condition
    /// ````
    ///
    /// ## Non-Unit Structs
    ///
    /// Most, but not all types implementing [`BasicSnippet`] are unit structs.
    /// [Fingerprinting][fingerprint] gets more difficult for non-unit structs.
    /// In such cases, a default instantiation should be selected and signed off.
    ///
    /// ## Overriding this Method
    ///
    /// This default implementation _is_ intended to be overridden for any snippet
    /// that has been signed off, but _should not_ call the [fingerprint] method.
    ///
    /// [fingerprint]: SignedOffSnippet::fingerprint
    /// [memory convention]: crate::memory
    /// [blame]: https://git-scm.com/docs/git-blame
    /// [signing]: https://git-scm.com/book/en/v2/Git-Tools-Signing-Your-Work
    fn sign_offs(&self) -> HashMap<Reviewer, SignOffFingerprint> {
        HashMap::default()
    }
}

/// Extension trait for [`BasicSnippet`] related to
/// [signing off](BasicSnippet::sign_offs). Contains methods that are callable,
/// but for which the provided default implementation cannot be overridden.
///
/// ### Dyn-Compatibility
///
/// This trait is [dyn-compatible] (previously known as “object safe”).
///
/// [dyn-compatible]: https://doc.rust-lang.org/reference/items/traits.html#object-safety
//
// Because `#[final]` trait methods are in pre-RFC phase [0], and trait
// sealing [1] would be clumsy, use this workaround.
//
// [0]: https://internals.rust-lang.org/t/pre-rfc-final-trait-methods/18407
// [1]: https://predr.ag/blog/definitive-guide-to-sealed-traits-in-rust
pub trait SignedOffSnippet: BasicSnippet {
    /// The unique fingerprint as used for [signing off][BasicSnippet::sign_offs] on
    /// this snippet.
    fn fingerprint(&self) -> SignOffFingerprint {
        let mut hasher = std::hash::DefaultHasher::new();
        triton_vm::proof::CURRENT_VERSION.hash(&mut hasher);

        for instruction in self.code(&mut Library::new()) {
            let LabelledInstruction::Instruction(instruction) = instruction else {
                continue;
            };

            if let AnInstruction::Call(_) = instruction {
                AnInstruction::Call("").opcode().hash(&mut hasher);
            } else {
                instruction.hash(&mut hasher)
            }
        }

        SignOffFingerprint(hasher.finish())
    }

    /// Panics if any [sign-offs](BasicSnippet::sign_offs) disagree with the actual
    /// [fingerprint](Self::fingerprint).
    fn assert_all_sign_offs_are_up_to_date(&self) {
        let fingerprint = self.fingerprint();
        let mut out_of_date_sign_offs = self
            .sign_offs()
            .into_iter()
            .filter(|(_, fp)| fp != &fingerprint)
            .peekable();

        if out_of_date_sign_offs.peek().is_none() {
            return;
        }

        let name = self.entrypoint();
        for (reviewer, fp) in out_of_date_sign_offs {
            eprintln!("reviewer {reviewer} of snippet “{name}” has signed off on fingerprint {fp}")
        }
        panic!("A sign-off is out of date. Current fingerprint of “{name}”: {fingerprint}");
    }
}

// Blanket implementation conflicts with any other implementation, making the
// provided defaults final.
impl<T: BasicSnippet + ?Sized> SignedOffSnippet for T {}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub struct Reviewer(pub &'static str);

impl Display for Reviewer {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

/// A fingerprint as used for [signing off][BasicSnippet::sign_offs] snippets.
///
/// While this fingerprint can be used to distinguish [`BasicSnippet`]s, it is
/// not cryptographically secure.
#[derive(Debug, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub struct SignOffFingerprint(pub(crate) u64);

impl Display for SignOffFingerprint {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "0x{:x}", self.0)
    }
}

impl From<u64> for SignOffFingerprint {
    fn from(value: u64) -> Self {
        Self(value)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    macro_rules! dummy_snippet {
        ($name:ident: $($instr:tt)+) => {
            #[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
            struct $name;

            impl BasicSnippet for $name {
                fn parameters(&self) -> Vec<(DataType, String)> { vec![] }
                fn return_values(&self) -> Vec<(DataType, String)> { vec![] }
                fn entrypoint(&self) -> String {
                    stringify!($name).to_ascii_lowercase()
                }

                fn code(&self, _: &mut Library) -> Vec<LabelledInstruction> {
                    triton_asm!($($instr)+)
                }
            }
        };
    }

    dummy_snippet!(DummySnippet: dummysnippet: push 14 push 14 pop 2 return);

    #[test]
    fn init_stack_agrees_with_tvm() {
        // Verify that our assumptions about the initial stack at program start
        // agrees with Triton VM.
        let calculated_init_stack = DummySnippet.init_stack_for_isolated_run();
        let program = DummySnippet.link_for_isolated_run();
        let program = Program::new(&program);
        let init_vm_state = VMState::new(program, Default::default(), Default::default());

        assert_eq!(init_vm_state.op_stack.stack, calculated_init_stack);
    }

    #[test]
    fn defined_traits_are_dyn_compatible() {
        fn basic_snippet_is_dyn_compatible(snippet: Box<dyn BasicSnippet>) {
            snippet.fingerprint();
        }

        fn signed_off_snippet_is_dyn_compatible(snippet: Box<dyn SignedOffSnippet>) {
            snippet.fingerprint();
        }

        basic_snippet_is_dyn_compatible(Box::new(DummySnippet));
        signed_off_snippet_is_dyn_compatible(Box::new(DummySnippet));
    }

    #[test]
    fn call_targets_dont_influence_snippet_fingerprints() {
        dummy_snippet!(SomeLabel: call some_label);
        dummy_snippet!(OtherLabel: call other_label);

        assert_eq!(SomeLabel.fingerprint(), OtherLabel.fingerprint());
    }

    #[test]
    fn instruction_arguments_do_influence_snippet_fingerprints() {
        dummy_snippet!(Push20: push 20);
        dummy_snippet!(Push42: push 42);

        assert_ne!(Push20.fingerprint(), Push42.fingerprint());
    }
}
