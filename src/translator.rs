use std::{collections::HashMap, fs, marker::PhantomData, path::Path, str::FromStr};

use log::debug;
use thiserror::Error;

use crate::isa::{
    self, Immed, Instruction, InstructionTrait, MachineWord, MemoryAddress, OpArg, OpArgBatch,
    OpArgNum, Program,
};

#[derive(Error, Debug)]
pub enum TranslatorError {
    #[error(transparent)]
    IoError(#[from] std::io::Error),
    #[error(transparent)]
    ISAError(#[from] isa::ISAError),
    #[error("End of input reached unexpectedly")]
    EndOfInput,
    #[error("Encuntered unrecognizable token")]
    UnknownToken,
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

// Ключевые слова, не транслирующиеся в машинные команды
#[derive(Debug)]
enum TranslatorDirective {
    Word, // Директива для записи литералов и значений в память
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
struct Label(String);

impl FromStr for Label {
    type Err = TranslatorError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if s.chars().all(|c| c.is_ascii_lowercase()) {
            Ok(Self(s.to_owned()))
        } else {
            Err(TranslatorError::InvalidLabel)
        }
    }
}

impl PartialEq<str> for Label {
    fn eq(&self, other: &str) -> bool {
        self.0 == other
    }
}

// Токен транслятора
#[derive(Debug)]
enum Token {
    TranslatorDirective(TranslatorDirective),
    Instruction(Instruction),
    Operand(Operand),
    LabelDef(Label),
    Literal(String),
}

impl FromStr for Token {
    type Err = TranslatorError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if s == "word" {
            Ok(Self::TranslatorDirective(TranslatorDirective::Word))
        } else if let Ok(instruction) = s.parse() {
            Ok(Self::Instruction(instruction))
        } else if let Ok(operand) = s.parse() {
            Ok(Self::Operand(operand))
        } else if let Some(label) = s.strip_suffix(':').map(Label::from_str).and_then(Result::ok) {
            Ok(Self::LabelDef(label))
        } else if s.starts_with('"') && s.ends_with('"') {
            Ok(Self::Literal(s[1..s.len() - 1].to_owned()))
        } else {
            Err(TranslatorError::UnknownToken)
        }
    }
}

#[derive(Debug)]
enum Operand {
    OpArg(OpArg),
    LabelRef(Label),
    MemLabelRef(Label),
}

impl Operand {
    pub fn into_placeholder_arg(self) -> OpArg {
        match self {
            Operand::OpArg(arg) => arg,
            Operand::LabelRef(_) => OpArg::Immed(Immed::null()),
            Operand::MemLabelRef(_) => OpArg::Mem(MemoryAddress::null()),
        }
    }

    pub fn into_arg(
        self,
        builder: &ProgramBuilder<LabelsResolved>,
    ) -> Result<OpArg, TranslatorError> {
        match self {
            Operand::OpArg(arg) => Ok(arg),
            Operand::LabelRef(label) => {
                Ok(OpArg::Immed(builder.get_label(&label).map(MemoryAddress::into)?))
            },
            Operand::MemLabelRef(label) => Ok(OpArg::Mem(builder.get_label(&label)?)),
        }
    }

    fn try_into_op_arg(self) -> Result<OpArg, TranslatorError> {
        match self {
            Operand::OpArg(arg) => Ok(arg),
            Operand::LabelRef(label) | Operand::MemLabelRef(label) => {
                Err(TranslatorError::UnexpectedToken {
                    expected: "OpArg".to_owned(),
                    found: format!("Label \"{:?}\"", label),
                })
            },
        }
    }
}

impl FromStr for Operand {
    type Err = TranslatorError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if let Ok(arg) = s.parse() {
            Ok(Self::OpArg(arg))
        } else if let Ok(label) = s.parse() {
            Ok(Self::LabelRef(label))
        } else if let Some(label) = s
            .strip_prefix('[')
            .and_then(|value| value.strip_suffix(']'))
            .map(Label::from_str)
            .and_then(Result::ok)
        {
            Ok(Self::MemLabelRef(label))
        } else {
            Err(TranslatorError::UnknownToken)
        }
    }
}

// Тип для удобства итерации по коду

struct TokenIterator<'a> {
    inner: core::slice::Iter<'a, String>,
}

impl<'a> TokenIterator<'a> {
    pub fn new(tokens: &'a [String]) -> Self {
        Self { inner: tokens.iter() }
    }

    pub fn next_str(&mut self) -> Result<&'a str, TranslatorError> {
        Ok(self.inner.next().ok_or(TranslatorError::EndOfInput)?.as_str())
    }

    pub fn next_token(&mut self) -> Result<Token, TranslatorError> {
        self.inner.next().ok_or(TranslatorError::EndOfInput)?.as_str().parse()
    }

    pub fn next_token_with_suffix(&mut self, suffix: char) -> Result<Token, TranslatorError> {
        let str = self.inner.next().ok_or(TranslatorError::EndOfInput)?.as_str();
        if str.ends_with(suffix) {
            str[0..str.len() - 1].parse()
        } else {
            Err(TranslatorError::UnexpectedToken {
                expected: format!("Token with suffix {}", suffix),
                found: str.to_owned(),
            })
        }
    }
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
    machine_code: Vec<MachineWord>,
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
    pub fn add_word(&mut self, word: MachineWord) {
        self.machine_code.push(word);
        self.current_address.add(1);
    }

    pub fn add_words(&mut self, words: (MachineWord, Option<MachineWord>)) {
        self.add_word(words.0);

        if let Some(word) = words.1 {
            self.add_word(word);
        }
    }

    pub fn get_label(&self, label: &Label) -> Result<MemoryAddress, TranslatorError> {
        self.labels.get(label).cloned().ok_or(TranslatorError::UnresolvedLabel)
    }

    pub fn build(self) -> Result<Program, TranslatorError> {
        Ok(Program::new(self.machine_code, self.entrypoint.unwrap_or(MemoryAddress::null())))
    }
}

struct Translator;
impl Translator {
    pub fn translate(source_path: &Path, target_path: &Path) -> Result<(), TranslatorError> {
        let builder = ProgramBuilder::new();
        let tokens = Self::read_tokens(source_path)?;

        let mut builder = Self::resolve_labels(builder, &mut TokenIterator::new(&tokens))?;
        Self::perform_translation(&mut builder, &mut TokenIterator::new(&tokens))?;
        let program = builder.build()?;

        Ok(program.write_to_file(target_path)?)
    }

    fn read_tokens(source_path: &Path) -> Result<Vec<String>, TranslatorError> {
        let code = fs::read_to_string(source_path)?;
        let tokens = code.split_whitespace().map(|token| token.to_string()).collect();

        Ok(tokens)
    }

    fn resolve_labels(
        mut builder: ProgramBuilder<LabelsUnresolved>,
        iter: &mut TokenIterator,
    ) -> Result<ProgramBuilder<LabelsResolved>, TranslatorError> {
        while let Ok(raw_token) = iter.next_str() {
            match Token::from_str(raw_token)? {
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
        while let Ok(raw_token) = iter.next_str() {
            match Token::from_str(raw_token)? {
                Token::TranslatorDirective(directive) => {
                    Self::handle_directive(builder, directive, iter)
                },
                Token::Instruction(instruction) => {
                    Self::handle_instruction(builder, instruction, iter)
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
                Token::Operand(value) => match value.try_into_op_arg()? {
                    OpArg::Immed(_) => {
                        builder.move_address(1);
                        Ok(())
                    },
                    arg => Err(TranslatorError::UnexpectedToken {
                        expected: "Word".to_owned(),
                        found: format!("{:?}", arg),
                    }),
                },
                Token::Literal(value) => {
                    let length: u32 =
                        value.len().try_into().map_err(|_| TranslatorError::StringIsTooLong)?;
                    builder.move_address(1 + length);
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
                Token::Operand(value) => match value.try_into_op_arg()? {
                    OpArg::Immed(value) => {
                        builder.add_word(MachineWord::Data(value));
                        Ok(())
                    },
                    arg => Err(TranslatorError::UnexpectedToken {
                        expected: "Word".to_owned(),
                        found: format!("{:?}", arg),
                    }),
                },
                Token::Literal(value) => {
                    let length: u32 =
                        value.len().try_into().map_err(|_| TranslatorError::StringIsTooLong)?;
                    builder.add_word(MachineWord::Data(Immed::from(length)));
                    for char in value.chars() {
                        builder.add_word(MachineWord::Data(Immed::from(char)))
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
                Token::Operand(operand) => {
                    Ok(instruction.words_num(OpArgBatch::One(operand.into_placeholder_arg()))?)
                },
                token => Err(TranslatorError::UnexpectedToken {
                    expected: "Operand".to_owned(),
                    found: format!("{:?}", token),
                }),
            },
            OpArgNum::Two => match (iter.next_token_with_suffix(',')?, iter.next_token()?) {
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
        iter: &mut TokenIterator,
    ) -> Result<(), TranslatorError> {
        match instruction.args_num() {
            OpArgNum::Zero => {
                builder.add_words(instruction.into_words(OpArgBatch::Zero)?);
                Ok(())
            },
            OpArgNum::One => match iter.next_token()? {
                Token::Operand(operand) => {
                    builder.add_words(
                        instruction.into_words(OpArgBatch::One(operand.into_arg(builder)?))?,
                    );
                    Ok(())
                },
                token => Err(TranslatorError::UnexpectedToken {
                    expected: "Operand".to_owned(),
                    found: format!("{:?}", token),
                }),
            },
            OpArgNum::Two => match (iter.next_token_with_suffix(',')?, iter.next_token()?) {
                (Token::Operand(first), Token::Operand(second)) => {
                    builder.add_words(instruction.into_words(OpArgBatch::Two(
                        first.into_arg(builder)?,
                        second.into_arg(builder)?,
                    ))?);
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
    debug!("Starting translation process");
    Translator::translate(source_path, target_path)?;
    debug!("Successfully finished translation process");
    Ok(())
}
