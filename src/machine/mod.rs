use std::path::Path;

use log::debug;
use thiserror::Error;

use self::{
    control_unit::ControlUnit,
    datapath::{ports::*, DataPath},
};
use crate::isa::{ISAError, MachineWord, PortId, Program};

mod control_unit;
mod datapath;

#[derive(Error, Debug)]
pub enum MachineError {
    #[error(transparent)]
    IoError(#[from] std::io::Error),
    #[error("Buffer error encountered")]
    BufferError,
    #[error(transparent)]
    ISAError(#[from] ISAError),
    #[error("Encountered invalid call to decoder")]
    InvalidDecoderCall,
    #[error("No device connected to port {0} found")]
    NonexistentPort(PortId),
    #[error("Device doesn't support specified operation")]
    NotSupportedDeviceOperation,
    #[error("Expected start of an instruction; instead got this: {0:?}")]
    InvalidInstruction(MachineWord),
    #[error("Reached tick limit ({0})")]
    TickLimitReached(usize),
    #[error("Exit command issued")]
    Exit,
}

pub fn main(code_path: &Path, input_path: &Path) -> Result<(), MachineError> {
    debug!("Starting virtual machine");

    let program = Program::read_from_file(code_path)?;

    let mut ports = PortSet::new();
    ports.add_device(0, PortDevice::InputOnly(InputOnlyDevice::from_file(input_path)?));
    ports.add_device(1, PortDevice::OutputOnly(OutputOnlyDevice::to_stdout()));

    let datapath = DataPath::new(program, ports);
    let mut control_unit: ControlUnit<4096, 4096> = ControlUnit::new(datapath);

    loop {
        if let Err(e) = control_unit.tick() {
            if matches!(e, MachineError::Exit) {
                break;
            } else {
                return Err(e);
            }
        }
    }

    Ok(())
}
