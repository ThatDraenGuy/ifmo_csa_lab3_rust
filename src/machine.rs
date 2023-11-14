use std::path::Path;

use thiserror::Error;

use crate::isa::MachineWord;

#[derive(Error, Debug)]
pub enum MachineError {
    // todo
}

struct DataPath {
    memory: [MachineWord; u32::MAX as usize + 1], // машинная память - ячейки машинных слов с адресами от 0 до u32::MAX
}

impl DataPath {
    // todo
}

struct ControlUnit {
    // todo
}

impl ControlUnit {
    // todo
}

pub fn main(code_path: &Path, input_path: &Path) -> Result<(), MachineError> {
    Ok(()) //TODO
}
