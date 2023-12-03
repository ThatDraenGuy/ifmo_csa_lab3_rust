mod tokens;

use self::tokens::*;
use crate::isa::{
    self, DebugInfo, DebuggedWord, Immed, Instruction, InstructionTrait, MachineWord,
    MemoryAddress, OpArg, OpArgBatch, OpArgNum, Program, SrcInfo,
};
use itertools::Itertools;
use log::debug;
use std::{collections::HashMap, fs, marker::PhantomData, path::Path};
use thiserror::Error;

#[derive(Error, Debug)]
pub enum TranslatorError {
    #[error(transparent)]
    IoError(#[from] std::io::Error),
    #[error(transparent)]
    ISAError(#[from] isa::ISAError),
    #[error("End of input reached unexpectedly")]
    EndOfInput,
    #[error("Encuntered unrecognizable token: {0}")]
    UnknownToken(String),
    #[error("Encountered unexpected token; expected {expected:?}, found {found:?}")]
    UnexpectedToken { expected: String, found: String },
    #[error("Encountered duplicate label")]
    DuplicateLabel,
    #[error("Encountered invalid label")]
    InvalidLabel,
    #[error("Encountered unresolved label")]
    UnresolvedLabel,
    #[error("Entrypoint is missing")]
    EntrypointMissing,
    #[error("Encountered invalid args combination")]
    InvalidArgsCombination,
    #[error("Encountered too long of a string")]
    StringIsTooLong,
}

// ProgramBuilder state space START
#[derive(Default)]
struct LabelsUnresolved;
struct LabelsResolved;
// ProgramBuilder state space END

#[derive(Default)]
struct ProgramBuilder<T> {
    labels: HashMap<Label, MemoryAddress>,
    current_address: MemoryAddress,
    machine_code: Vec<DebuggedWord>,
    entrypoint: Option<MemoryAddress>,
    phantom: PhantomData<T>,
}

impl ProgramBuilder<LabelsUnresolved> {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn move_address(&mut self, count: u32) {
        self.current_address.add(count);
    }

    pub fn add_label(&mut self, label: &Label) -> Result<(), TranslatorError> {
        if self.labels.contains_key(label) {
            Err(TranslatorError::DuplicateLabel)
        } else {
            let mut label_address = self.current_address;
            label_address.add(1);
            self.labels.insert(label.to_owned(), label_address);
            if label == "start" {
                self.entrypoint = Some(label_address);
            }
            Ok(())
        }
    }

    pub fn finalize_labels(self) -> Result<ProgramBuilder<LabelsResolved>, TranslatorError> {
        if self.entrypoint.is_none() {
            return Err(TranslatorError::EntrypointMissing);
        }

        debug!("Resolved {count} labels", count = self.labels.len());

        Ok(ProgramBuilder {
            labels: self.labels,
            current_address: MemoryAddress::null(),
            machine_code: self.machine_code,
            entrypoint: self.entrypoint,
            phantom: PhantomData,
        })
    }
}

impl ProgramBuilder<LabelsResolved> {
    pub fn add_word(&mut self, word: MachineWord, src_info: SrcInfo) {
        self.current_address.add(1);
        self.machine_code.push(DebuggedWord {
            debug_info: DebugInfo { src_info, addr: self.current_address },
            word,
        });
    }

    pub fn add_words(&mut self, words: (MachineWord, Option<MachineWord>), src_info: SrcInfo) {
        self.add_word(words.0, src_info.clone());

        if let Some(word) = words.1 {
            self.add_word(word, src_info);
        }
    }

    pub fn get_label(&self, label: &Label) -> Result<MemoryAddress, TranslatorError> {
        self.labels.get(label).cloned().ok_or(TranslatorError::UnresolvedLabel)
    }

    pub fn build(self) -> Program {
        Program::new(self.machine_code, self.entrypoint.unwrap_or(MemoryAddress::null()))
    }
}

struct Translator;
impl Translator {
    pub fn translate(source_path: &Path, target_path: &Path) -> Result<(), TranslatorError> {
        let builder = ProgramBuilder::new();
        let code = fs::read_to_string(source_path)?;

        debug!("Starting first pass");
        let mut builder = Self::resolve_labels(builder, &mut TokenIterator::new(&code))?;
        debug!("Starting second pass");
        Self::perform_translation(&mut builder, &mut TokenIterator::new(&code))?;
        let program = builder.build();
        debug!("Built a program: from {src_line_count} lines of source code to {code_word_count} machine words ({code_byte_count} bytes)", 
            src_line_count=code.lines().count(),
            code_word_count=program.len(),
            code_byte_count=program.len() * 4
        );

        Ok(program.write_to_file(target_path)?)
    }

    fn resolve_labels(
        mut builder: ProgramBuilder<LabelsUnresolved>,
        iter: &mut TokenIterator,
    ) -> Result<ProgramBuilder<LabelsResolved>, TranslatorError> {
        while let Ok((raw_token, _)) = iter.next_string() {
            match raw_token.parse()? {
                Token::TranslatorDirective(directive) => {
                    Self::calculate_directive(&mut builder, directive, iter)
                },
                Token::Instruction(instruction) => {
                    Self::calculate_instruction(&mut builder, instruction, iter)
                },
                Token::LabelDef(label) => builder.add_label(&label),
                Token::Operand(operand) => Err(TranslatorError::UnexpectedToken {
                    expected: "Directive, label def or instruction".to_owned(),
                    found: format!("Operand: {:?}", operand),
                }),
                Token::Literal(literal) => Err(TranslatorError::UnexpectedToken {
                    expected: "Directive, label def or instruction".to_owned(),
                    found: format!("Literal: {}", literal),
                }),
            }?;
        }
        builder.finalize_labels()
    }

    fn perform_translation(
        builder: &mut ProgramBuilder<LabelsResolved>,
        iter: &mut TokenIterator,
    ) -> Result<(), TranslatorError> {
        while let Ok((raw_token, info)) = iter.next_string() {
            match raw_token.parse()? {
                Token::TranslatorDirective(directive) => {
                    Self::handle_directive(builder, directive, iter)
                },
                Token::Instruction(instruction) => {
                    Self::handle_instruction(builder, instruction, info, iter)
                },
                Token::LabelDef(_) => Ok(()),
                Token::Operand(operand) => Err(TranslatorError::UnexpectedToken {
                    expected: "Directive, label def or instruction".to_owned(),
                    found: format!("Operand: {:?}", operand),
                }),
                Token::Literal(literal) => Err(TranslatorError::UnexpectedToken {
                    expected: "Directive, label def or instruction".to_owned(),
                    found: format!("Literal: {}", literal),
                }),
            }?;
        }
        Ok(())
    }

    fn calculate_directive(
        builder: &mut ProgramBuilder<LabelsUnresolved>,
        directive: TranslatorDirective,
        iter: &mut TokenIterator,
    ) -> Result<(), TranslatorError> {
        match directive {
            TranslatorDirective::Word => match iter.next_token()? {
                (Token::Operand(value), _) => match value.try_into_op_arg()? {
                    OpArg::Immed(_) => {
                        builder.move_address(1);
                        Ok(())
                    },
                    arg => Err(TranslatorError::UnexpectedToken {
                        expected: "Word".to_owned(),
                        found: format!("{:?}", arg),
                    }),
                },
                (Token::Literal(value), _) => {
                    let length: u32 =
                        value.len().try_into().map_err(|_| TranslatorError::StringIsTooLong)?;
                    builder.move_address(1 + 1 + length / 4);
                    Ok(())
                },
                token => Err(TranslatorError::UnexpectedToken {
                    expected: "Word".to_owned(),
                    found: format!("{:?}", token),
                }),
            },
        }
    }

    fn handle_directive(
        builder: &mut ProgramBuilder<LabelsResolved>,
        directive: TranslatorDirective,
        iter: &mut TokenIterator,
    ) -> Result<(), TranslatorError> {
        match directive {
            TranslatorDirective::Word => match iter.next_token()? {
                (Token::Operand(value), info) => match value.try_into_op_arg()? {
                    OpArg::Immed(value) => {
                        builder.add_word(MachineWord::Data(value), info);
                        Ok(())
                    },
                    arg => Err(TranslatorError::UnexpectedToken {
                        expected: "Word".to_owned(),
                        found: format!("{:?}", arg),
                    }),
                },
                (Token::Literal(value), info) => {
                    let length: u32 =
                        value.len().try_into().map_err(|_| TranslatorError::StringIsTooLong)?;
                    builder.add_word(MachineWord::Data(Immed::from(length)), info.clone());
                    for chunk in &value.chars().chunks(4) {
                        let (res, _) = chunk.fold((0, 0), |acc, c| {
                            let res = acc.0 + ((c as u32) << acc.1);
                            (res, acc.1 + 8)
                        });
                        builder.add_word(MachineWord::Data(Immed::from(res)), info.clone());
                    }
                    Ok(())
                },
                token => Err(TranslatorError::UnexpectedToken {
                    expected: "Word".to_owned(),
                    found: format!("{:?}", token),
                }),
            },
        }
    }

    fn calculate_instruction(
        builder: &mut ProgramBuilder<LabelsUnresolved>,
        instruction: Instruction,
        iter: &mut TokenIterator,
    ) -> Result<(), TranslatorError> {
        let words_num = match instruction.args_num() {
            OpArgNum::Zero => Ok(instruction.words_num(OpArgBatch::Zero)?),
            OpArgNum::One => match iter.next_token()? {
                (Token::Operand(operand), _) => {
                    Ok(instruction.words_num(OpArgBatch::One(operand.into_placeholder_arg()))?)
                },
                token => Err(TranslatorError::UnexpectedToken {
                    expected: "Operand".to_owned(),
                    found: format!("{:?}", token),
                }),
            },
            OpArgNum::Two => match (iter.next_token_with_suffix(',')?.0, iter.next_token()?.0) {
                (Token::Operand(first), Token::Operand(second)) => Ok(instruction.words_num(
                    OpArgBatch::Two(first.into_placeholder_arg(), second.into_placeholder_arg()),
                )?),
                (fst, snd) => Err(TranslatorError::UnexpectedToken {
                    expected: "Operand pair".to_owned(),
                    found: format!("{:?}, {:?}", fst, snd),
                }),
            },
        }?;
        builder.move_address(words_num);
        Ok(())
    }

    fn handle_instruction(
        builder: &mut ProgramBuilder<LabelsResolved>,
        instruction: Instruction,
        info: SrcInfo,
        iter: &mut TokenIterator,
    ) -> Result<(), TranslatorError> {
        match instruction.args_num() {
            OpArgNum::Zero => {
                builder.add_words(instruction.into_words(OpArgBatch::Zero)?, info);
                Ok(())
            },
            OpArgNum::One => match iter.next_token()? {
                (Token::Operand(operand), info) => {
                    builder.add_words(
                        instruction.into_words(OpArgBatch::One(operand.into_arg(builder)?))?,
                        info,
                    );
                    Ok(())
                },
                token => Err(TranslatorError::UnexpectedToken {
                    expected: "Operand".to_owned(),
                    found: format!("{:?}", token),
                }),
            },
            OpArgNum::Two => match (iter.next_token_with_suffix(',')?, iter.next_token()?) {
                ((Token::Operand(first), info_first), (Token::Operand(second), _)) => {
                    builder.add_words(
                        instruction.into_words(OpArgBatch::Two(
                            first.into_arg(builder)?,
                            second.into_arg(builder)?,
                        ))?,
                        info_first,
                    );
                    Ok(())
                },
                (fst, snd) => Err(TranslatorError::UnexpectedToken {
                    expected: "Operand pair".to_owned(),
                    found: format!("{:?}, {:?}", fst, snd),
                }),
            },
        }
    }
}

pub fn main(source_path: &Path, target_path: &Path) -> Result<(), TranslatorError> {
    debug!("Starting translator");
    Translator::translate(source_path, target_path)?;
    debug!("Successfully finished translation process");
    Ok(())
}
