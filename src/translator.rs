use std::{collections::HashMap, fs, path::Path};

use thiserror::Error;

use crate::isa::{self, Data, MachineWord, MemoryAddress, OpArg, OpArgNum, OpCode, Program};

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
    #[error("Encountered unexpected token")]
    UnexpectedToken,
    #[error("Encountered duplicate label")]
    DuplicateLabel,
    #[error("Encountered unresolved label")]
    UnresolvedLabel,
    #[error("Entrypoint is missing")]
    EntrypointMissing,
    #[error("Encountered invalid args combination")]
    InvalidArgsCombination,
}

// Ключевые слова, не транслирующиеся в машинные команды
enum TranslatorDirective {
    Word, // Директива для записи литералов в память
}

type Label = String;

// Токен транслятора
enum Token {
    TranslatorDirective(TranslatorDirective),
    OpCode(OpCode),
    OpArg(OpArg),
    LabelDef(Label),
    LabelRef(Label),
    Literal(String),
}

impl Token {
    fn try_into_literal(self) -> Result<String, TranslatorError> {
        if let Self::Literal(v) = self {
            Ok(v)
        } else {
            Err(TranslatorError::UnexpectedToken)
        }
    }
}

impl TryFrom<&str> for Token {
    type Error = TranslatorError;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        if value == "word" {
            Ok(Self::TranslatorDirective(TranslatorDirective::Word))
        } else if let Ok(operation) = OpCode::try_from(value) {
            Ok(Self::OpCode(operation))
        } else if let Ok(argument) = OpArg::try_from(value) {
            Ok(Self::OpArg(argument))
        } else if let Some(label) = value.strip_suffix(':') {
            Ok(Self::LabelDef(label.to_owned()))
        } else if value.starts_with('"') && value.ends_with('"') {
            Ok(Self::Literal(value[1..value.len() - 1].to_owned()))
        } else if value.chars().all(|c| c.is_ascii_lowercase()) {
            Ok(Self::LabelRef(value.to_owned()))
        } else {
            Err(TranslatorError::UnknownToken)
        }
    }
}

// Тип для хранения машинного слова или плэйсхолдера с меткой
enum TranslatorWord {
    MachineWord(MachineWord),
    LabelPlaceholder(Label),
}

impl From<MachineWord> for TranslatorWord {
    fn from(value: MachineWord) -> Self {
        Self::MachineWord(value)
    }
}

impl From<Label> for TranslatorWord {
    fn from(value: Label) -> Self {
        Self::LabelPlaceholder(value)
    }
}

impl TryFrom<TranslatorWord> for MachineWord {
    type Error = TranslatorError;

    fn try_from(value: TranslatorWord) -> Result<Self, Self::Error> {
        match value {
            TranslatorWord::MachineWord(word) => Ok(word),
            TranslatorWord::LabelPlaceholder(_) => Err(TranslatorError::UnresolvedLabel),
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
        self.inner.next().ok_or(TranslatorError::EndOfInput)?.as_str().try_into()
    }

    pub fn next_token_with_suffix(&mut self, suffix: char) -> Result<Token, TranslatorError> {
        let str = self.inner.next().ok_or(TranslatorError::EndOfInput)?.as_str();
        if str.ends_with(suffix) {
            str[0..str.len() - 1].try_into()
        } else {
            Err(TranslatorError::UnexpectedToken)
        }
    }
}

#[derive(Default)]
struct ProgramBuilder {
    labels: HashMap<Label, MemoryAddress>,
    current_address: MemoryAddress,
    machine_code: Vec<TranslatorWord>,
    entrypoint: Option<MemoryAddress>,
}

impl ProgramBuilder {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn add_word<T: Into<TranslatorWord>>(&mut self, word: T) {
        self.machine_code.push(word.into());
        self.current_address += 1;
    }

    pub fn add_label(&mut self, label: &str) -> Result<(), TranslatorError> {
        if self.labels.contains_key(label) {
            Err(TranslatorError::DuplicateLabel)
        } else {
            self.labels.insert(label.to_owned(), self.current_address + 1);
            if label == "start" {
                self.entrypoint = Some(self.current_address + 1);
            }
            Ok(())
        }
    }

    pub fn build(self) -> Result<Program, TranslatorError> {
        let code = self
            .machine_code
            .into_iter()
            .map(|word| match word {
                TranslatorWord::MachineWord(word) => Ok(word),
                TranslatorWord::LabelPlaceholder(placeholder) => {
                    if let Some(address) = self.labels.get(&placeholder) {
                        Ok(MachineWord::OpArg(OpArg::MemoryAddress(*address)))
                    } else {
                        Err(TranslatorError::UnresolvedLabel)
                    }
                },
            })
            .collect::<Result<Vec<MachineWord>, TranslatorError>>()?;

        let entrypoint = self.entrypoint.ok_or(TranslatorError::EntrypointMissing)?;
        Ok(Program::new(code, entrypoint))
    }
}

struct Translator;
impl Translator {
    pub fn translate(source_path: &Path, target_path: &Path) -> Result<(), TranslatorError> {
        let mut builder = ProgramBuilder::new();
        let tokens = Self::read_tokens(source_path)?;
        let mut iter = TokenIterator::new(&tokens);

        Self::perform_translation(&mut builder, &mut iter)?;
        let program = builder.build()?;

        Ok(program.write_to_file(target_path)?)
    }

    fn read_tokens(source_path: &Path) -> Result<Vec<String>, TranslatorError> {
        let code = fs::read_to_string(source_path)?;
        let tokens = code.split_whitespace().map(|token| token.to_string()).collect();

        Ok(tokens)
    }

    fn perform_translation(
        builder: &mut ProgramBuilder,
        iter: &mut TokenIterator,
    ) -> Result<(), TranslatorError> {
        while let Ok(raw_token) = iter.next_str() {
            match Token::try_from(raw_token)? {
                Token::TranslatorDirective(directive) => {
                    Self::resolve_directive(builder, directive, iter)
                },
                Token::OpCode(operation) => Self::resolve_operation(builder, operation, iter),
                Token::LabelDef(label) => Self::resolve_label(builder, label.as_str()),
                _ => Err(TranslatorError::UnexpectedToken),
            }?;
        }
        Ok(())
    }

    fn resolve_label(builder: &mut ProgramBuilder, label: &str) -> Result<(), TranslatorError> {
        builder.add_label(label)
    }

    fn resolve_directive(
        builder: &mut ProgramBuilder,
        directive: TranslatorDirective,
        iter: &mut TokenIterator,
    ) -> Result<(), TranslatorError> {
        match directive {
            TranslatorDirective::Word => {
                let value = iter.next_token()?.try_into_literal()?;
                builder.add_word(MachineWord::Data(Data::U32(value.len().try_into().unwrap())));
                for char in value.chars() {
                    builder.add_word(MachineWord::Data(Data::Char(char)))
                }
                Ok(())
            },
        }
    }

    fn resolve_operation(
        builder: &mut ProgramBuilder,
        operation: OpCode,
        iter: &mut TokenIterator,
    ) -> Result<(), TranslatorError> {
        // Добавляем код операции
        builder.add_word(MachineWord::OpCode(operation.clone()));

        // Добавляем аргументы операции (если они есть)
        match operation.args_num() {
            OpArgNum::Zero => Ok(()),
            OpArgNum::One => match iter.next_token()? {
                Token::OpArg(arg) => match arg {
                    OpArg::RegisterId(_) => {
                        builder.add_word(MachineWord::OpArg(arg));
                        Ok(())
                    },
                    _ => Err(TranslatorError::UnexpectedToken),
                },
                Token::LabelRef(label) => {
                    builder.add_word(label);
                    Ok(())
                },
                _ => Err(TranslatorError::UnexpectedToken),
            },
            OpArgNum::Two => {
                let first = iter.next_token_with_suffix(',')?;
                let second = iter.next_token()?;

                match (first, second) {
                    (Token::OpArg(first), Token::OpArg(second)) => {
                        // невозможно записать результат в литерал
                        // не поддерживаем операции с двумя обращениями к памяти
                        if first.is_arg_literal()
                            || first.is_memory_dependent() && second.is_memory_dependent()
                        {
                            Err(TranslatorError::InvalidArgsCombination)
                        } else {
                            builder.add_word(MachineWord::OpArg(first));
                            builder.add_word(MachineWord::OpArg(second));
                            Ok(())
                        }
                    },
                    (Token::OpArg(arg), Token::LabelRef(label)) => {
                        // невозможно записать результат в литерал
                        if arg.is_arg_literal() {
                            Err(TranslatorError::InvalidArgsCombination)
                        } else {
                            builder.add_word(MachineWord::OpArg(arg));
                            builder.add_word(label);
                            Ok(())
                        }
                    },
                    // (Token::LabelRef(label), Token::OpArg(arg)) => {
                    //     // не поддерживаем операции с двумя обращениями к памяти
                    //     if arg.is_memory_dependent() {
                    //         Err(TranslatorError::InvalidArgsCombination)
                    //     } else {
                    //         builder.add_word(label);
                    //         builder.add_word(MachineWord::OpArg(arg));
                    //         Ok(())
                    //     }
                    // },
                    // (Token::LabelRef(_), Token::LabelRef(_)) => {
                    //     // не поддерживаем операции с двумя обращениями к памяти
                    //     Err(TranslatorError::InvalidArgsCombination)
                    // },
                    _ => Err(TranslatorError::UnexpectedToken),
                }
            },
        }
    }
}

pub fn main(source_path: &Path, target_path: &Path) -> Result<(), TranslatorError> {
    Translator::translate(source_path, target_path)
}
