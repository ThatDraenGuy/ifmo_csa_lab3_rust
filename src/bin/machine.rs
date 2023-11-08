use itertools::Itertools;
use lab3_rust::machine;
use std::env;

fn main() {
    if let Some((_, code_file, input_file)) = env::args().collect_tuple() {
        machine::main(code_file, input_file)
    } else {
        panic!("Wrong arguments: machine <code_file> <input_file>")
    }
}
