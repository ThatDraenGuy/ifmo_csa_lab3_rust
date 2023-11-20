use itertools::Itertools;
use lab3_rust::machine;
use std::{env, path::Path};

fn main() -> Result<(), machine::MachineError> {
    env_logger::init();
    if let Some((_, code_file, input_file)) = env::args().collect_tuple() {
        machine::main(Path::new(code_file.as_str()), Path::new(input_file.as_str()))
    } else {
        panic!("Wrong arguments: machine <code_file> <input_file>")
    }
}
