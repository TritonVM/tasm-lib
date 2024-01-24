use triton_vm::instruction::LabelledInstruction;

use crate::data_type::DataType;
use crate::library::Library;

pub trait BasicSnippet {
    fn inputs(&self) -> Vec<(DataType, String)>;
    fn outputs(&self) -> Vec<(DataType, String)>;
    fn entrypoint(&self) -> String;
    fn code(&self, library: &mut Library) -> Vec<LabelledInstruction>;

    fn stack_diff(&self) -> isize {
        let mut diff = 0isize;
        for (dt, _name) in self.inputs() {
            diff -= dt.stack_size() as isize;
        }
        for (dt, _name) in self.outputs() {
            diff += dt.stack_size() as isize;
        }
        diff
    }
}
