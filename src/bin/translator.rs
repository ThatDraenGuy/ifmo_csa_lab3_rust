use itertools::Itertools;
use lab3_rust::translator;
use std::env;

fn main() {
    if let Some((_, source_file, target_file)) = env::args().collect_tuple() {
        translator::main(source_file, target_file)
    } else {
        panic!("Wrong arguments: translator <source_file> <target_file>")
    }
}
