use itertools::Itertools;
use lab3_rust::translator;
use std::{env, path::Path};

fn main() -> Result<(), translator::TranslatorError> {
    if let Some((_, source_file, target_file)) = env::args().collect_tuple() {
        translator::main(Path::new(source_file.as_str()), Path::new(target_file.as_str()))
    } else {
        panic!("Wrong arguments: translator <source_file> <target_file>")
    }
}
