use std::str::FromStr;

use crate::isa::{Immed, Instruction, MemoryAddress, OpArg, SrcInfo};

use super::{LabelsResolved, ProgramBuilder, TranslatorError};

/// Ключевые слова, не транслирующиеся в машинные команды
#[derive(Debug)]
pub enum TranslatorDirective {
    /// Директива для записи литералов и значений в память
    Word,
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct Label(String);

impl FromStr for Label {
    type Err = TranslatorError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if s.chars().all(|c| c.is_ascii_lowercase() || c.is_ascii_digit() || c == '_') {
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

/// Токен транслятора
#[derive(Debug)]
pub enum Token {
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
            Err(TranslatorError::UnknownToken(s.to_owned()))
        }
    }
}

#[derive(Debug)]
pub enum Operand {
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

    pub fn try_into_op_arg(self) -> Result<OpArg, TranslatorError> {
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
            Err(TranslatorError::UnknownToken(s.to_owned()))
        }
    }
}

/// Тип для удобства итерации по коду
pub struct TokenIterator<'a> {
    current_src_info: SrcInfo,
    state: TokenIteratorState,
    chars: std::str::Chars<'a>,
}
enum TokenIteratorState {
    /// whitespace-символы
    Delimiter,
    /// После обратного слэша, до последующего символа
    Backslash,
    /// Слово без кавычек
    Unquoted,
    /// После обратного слэша в слове без кавычек
    UnquotedBackslash,
    /// Слово в одинарных кавычках
    SingleQuoted,
    /// Слово в двойных кавычках
    DoubleQuoted,
    /// После обратного слэша в слове в двойных кавычках
    DoubleQuotedBackslash,
    /// Комментарий
    Comment,
}

impl<'a> TokenIterator<'a> {
    pub fn new(code: &'a str) -> Self {
        Self {
            state: TokenIteratorState::Delimiter,
            chars: code.chars(),
            current_src_info: Default::default(),
        }
    }
    pub fn next_string(&mut self) -> Result<(String, SrcInfo), TranslatorError> {
        use TokenIteratorState::*;

        let mut word = String::new();
        let mut word_src_info = self.current_src_info.clone();
        let chars = &mut self.chars;
        let state = &mut self.state;

        loop {
            let c = chars.next();
            self.current_src_info.src_symb += 1;
            if let Some('\n') = c {
                self.current_src_info.src_line += 1;
                self.current_src_info.src_symb = 0;
            }

            *state = match state {
                Delimiter => match c {
                    None => return Err(TranslatorError::EndOfInput),
                    Some(c @ '\'') => {
                        word.push(c);
                        word_src_info = self.current_src_info.clone();
                        SingleQuoted
                    },
                    Some(c @ '\"') => {
                        word.push(c);
                        word_src_info = self.current_src_info.clone();
                        DoubleQuoted
                    },
                    Some('\\') => Backslash,
                    Some('\t') | Some(' ') | Some('\n') => Delimiter,
                    Some(';') => Comment,
                    Some(c) => {
                        word.push(c);
                        word_src_info = self.current_src_info.clone();
                        Unquoted
                    },
                },
                Backslash => match c {
                    None => {
                        word.push('\\');
                        *state = Delimiter;
                        break;
                    },
                    Some('\n') => Delimiter,
                    Some(c) => {
                        word.push(c);
                        word_src_info = self.current_src_info.clone();
                        Unquoted
                    },
                },
                Unquoted => match c {
                    None => {
                        *state = Delimiter;
                        break;
                    },
                    Some(c @ '\'') => {
                        word.push(c);
                        SingleQuoted
                    },
                    Some(c @ '\"') => {
                        word.push(c);
                        DoubleQuoted
                    },
                    Some('\\') => UnquotedBackslash,
                    Some('\t') | Some(' ') => {
                        *state = Delimiter;
                        break;
                    },
                    Some('\n') => {
                        *state = Delimiter;
                        break;
                    },
                    Some(c) => {
                        word.push(c);
                        Unquoted
                    },
                },
                UnquotedBackslash => match c {
                    None => {
                        word.push('\\');
                        *state = Delimiter;
                        break;
                    },
                    Some('\n') => Unquoted,
                    Some(c) => {
                        word.push(c);
                        Unquoted
                    },
                },
                SingleQuoted => match c {
                    None => return Err(TranslatorError::EndOfInput),
                    Some(c @ '\'') => {
                        word.push(c);
                        word_src_info = self.current_src_info.clone();
                        Unquoted
                    },
                    Some(c) => {
                        word.push(c);
                        SingleQuoted
                    },
                },
                DoubleQuoted => match c {
                    None => return Err(TranslatorError::EndOfInput),
                    Some(c @ '\"') => {
                        word.push(c);
                        word_src_info = self.current_src_info.clone();
                        Unquoted
                    },
                    Some('\\') => DoubleQuotedBackslash,
                    Some(c) => {
                        word.push(c);
                        DoubleQuoted
                    },
                },
                DoubleQuotedBackslash => match c {
                    None => return Err(TranslatorError::EndOfInput),
                    Some('\n') => DoubleQuoted,
                    Some(c @ '$') | Some(c @ '`') | Some(c @ '"') | Some(c @ '\\') => {
                        word.push(c);
                        DoubleQuoted
                    },
                    Some(c) => {
                        word.push('\\');
                        word.push(c);
                        DoubleQuoted
                    },
                },
                Comment => match c {
                    None => return Err(TranslatorError::EndOfInput),
                    Some('\n') => Delimiter,
                    Some(_) => Comment,
                },
            }
        }
        Ok((word, word_src_info))
    }

    pub fn next_token(&mut self) -> Result<(Token, SrcInfo), TranslatorError> {
        let (s, i) = self.next_string()?;
        Ok((s.parse()?, i))
    }
    pub fn next_token_with_suffix(
        &mut self,
        suffix: char,
    ) -> Result<(Token, SrcInfo), TranslatorError> {
        let (string, info) = self.next_string()?;
        if string.ends_with(suffix) {
            Ok((string[0..string.len() - 1].parse()?, info))
        } else {
            Err(TranslatorError::UnexpectedToken {
                expected: format!("Token with suffix {}", suffix),
                found: string.to_owned(),
            })
        }
    }
}
