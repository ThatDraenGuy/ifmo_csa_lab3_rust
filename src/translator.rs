use std::path::Path;

use thiserror::Error;

#[derive(Error, Debug)]
pub enum TranslatorError {
    // todo
}

pub fn main(source_path: &Path, target_path: &Path) -> Result<(), TranslatorError> {
    todo!()
}
