use triton_vm::{instruction::LabelledInstruction, triton_asm, BFieldElement};
use twenty_first::shared_math::bfield_codec::BFieldCodec;

use crate::{
    algorithm::Algorithm,
    field,
    library::Library,
    snippet::{BasicSnippet, DataType},
    structure::tasm_object::TasmObject,
};

#[derive(Debug, Clone, BFieldCodec, TasmObject)]
struct VmProofStream {
    pub data: Vec<BFieldElement>,
    pub word_index: u32,
}

/// Dequeue reads the next object from the `ProofStream`
pub struct Dequeue {}

impl BasicSnippet for Dequeue {
    fn inputs(&self) -> Vec<(DataType, String)> {
        vec![(DataType::VoidPointer, "*proof_stream".to_string())]
    }

    fn outputs(&self) -> Vec<(DataType, String)> {
        vec![
            (DataType::VoidPointer, "*proof_stream".to_string()),
            (DataType::VoidPointer, "*object".to_string()),
        ]
    }

    fn entrypoint(&self) -> String {
        "tasm_recufier_proof_stream_dequeue".to_string()
    }

    fn code(&self, _library: &mut Library) -> Vec<LabelledInstruction> {
        let entrypoint = self.entrypoint();
        let field_word_index = field!(VmProofStream::word_index);
        let field_data = field!(VmProofStream::data);
        triton_asm!(
            // BEFORE: _ *proof_stream
            // AFTER: _ *proof_stream *object
            {entrypoint}:

                dup 0               // _ *proof_stream *proof_stream
                {&field_word_index} // _ *proof_stream *word_index
                read_mem            // _ *proof_stream *word_index word_index
                dup 0               // _ *proof_stream *word_index word_index word_index

                dup 3               // _ *proof_stream *word_index word_index word_index *proof_stream
                {&field_data}       // _ *proof_stream *word_index word_index word_index *data
                add                 // _ *proof_stream *word_index word_index *object_si

                read_mem            // _ *proof_stream *word_index word_index *object_si object_size
                push 1 add          // _ *proof_stream *word_index word_index *object_si object_size+1
                dup 2               // _ *proof_stream *word_index word_index *object_si object_size+1 word_index
                add                 // _ *proof_stream *word_index  word_index *object_si word_index'

                swap 1              // _ *proof_stream *word_index word_index word_index' *object_si
                push 1 add          // _ *proof_stream *word_index word_index word_index' *object
                swap 3              // _ *proof_stream *object word_index word_index' *word_index
                swap 1              // _ *proof_stream *object word_index *word_index word_index'
                write_mem           // _ *proof_stream *object word_index *word_index

                pop pop             // _ *proof_stream *object

        )
    }
}

impl Algorithm for Dequeue {
    fn rust_shadow(
        &self,
        stack: &mut Vec<BFieldElement>,
        memory: &mut std::collections::HashMap<BFieldElement, BFieldElement>,
        _nondeterminism: &triton_vm::NonDeterminism<BFieldElement>,
    ) {
        // read proof stream pointer from stack
        let proof_stream_pointer = stack.pop().unwrap();

        // decode from memory
        let _proof_stream =
            *VmProofStream::decode_from_memory(memory, proof_stream_pointer).unwrap();
    }

    fn pseudorandom_initial_state(
        &self,
        _seed: [u8; 32],
        _bench_case: Option<crate::snippet_bencher::BenchmarkCase>,
    ) -> (
        Vec<BFieldElement>,
        std::collections::HashMap<BFieldElement, BFieldElement>,
        triton_vm::NonDeterminism<BFieldElement>,
    ) {
        todo!()
    }
}
