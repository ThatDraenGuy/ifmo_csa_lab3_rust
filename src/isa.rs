use std::{fs::File, path::Path};

use phf::phf_map;
use ron::ser::PrettyConfig;
use serde::{Deserialize, Serialize};
use thiserror::Error;

#[derive(Error, Debug)]
pub enum ISAError {
    #[error(transparent)]
    IoError(#[from] std::io::Error),
    #[error(transparent)]
    RonError(#[from] ron::Error),
    #[error("Invalid opcode")]
    InvalidOpCode,
    #[error("Invalid register id")]
    InvalidRegisterId,
    #[error("Invalid oparg")]
    InvalidOpArg,
}

#[derive(Debug, Deserialize, Serialize)]
pub struct Program {
    code: Vec<MachineWord>,
    entrypoint: MemoryAddress,
}

impl Program {
    pub fn new(code: Vec<MachineWord>, entrypoint: MemoryAddress) -> Self {
        Self { code, entrypoint }
    }

    pub fn read_from_file(path: &Path) -> Result<Self, ISAError> {
        let file = File::open(path)?;
        Ok(ron::de::from_reader(file).map_err(ron::Error::from)?)
    }

    pub fn write_to_file(&self, path: &Path) -> Result<(), ISAError> {
        let file = File::create(path)?;
        Ok(ron::ser::to_writer_pretty(file, self, PrettyConfig::default())?)
    }
}

// Машинное слово представляется несколькими типами
#[derive(Debug, Default, Deserialize, Serialize)]
pub enum MachineWord {
    OpCode(OpCode), // Машинное слово представляет код операции
    OpArg(OpArg),   // Машинное слово представляет аргумент операции
    Data(Data),     // Машинное слово представляет значение (не операцию)
    #[default]
    Empty, // Пустое машинное слово
}

#[derive(Clone, Debug, Deserialize, Serialize)]
pub enum OpCode {
    Move,
    Jump,
    JumpIfZero,
    JumpIfNotZero,
    Add,
    Subtract,
    Increment,
    Decrement,
    SysWrite,
    SysRead,
}

pub enum OpArgNum {
    Zero,
    One,
    Two,
}

impl OpCode {
    pub fn args_num(&self) -> OpArgNum {
        match self {
            OpCode::Move => OpArgNum::Two,
            OpCode::Jump => OpArgNum::One,
            OpCode::JumpIfZero => OpArgNum::One,
            OpCode::JumpIfNotZero => OpArgNum::One,
            OpCode::Add => OpArgNum::Two,
            OpCode::Subtract => OpArgNum::Two,
            OpCode::Increment => OpArgNum::One,
            OpCode::Decrement => OpArgNum::One,
            OpCode::SysWrite => OpArgNum::Zero,
            OpCode::SysRead => OpArgNum::Zero,
        }
    }
}

impl From<OpCode> for &'static str {
    fn from(val: OpCode) -> Self {
        match val {
            OpCode::Move => "mov",
            OpCode::Jump => "jump",
            OpCode::JumpIfZero => "jz",
            OpCode::JumpIfNotZero => "jnz",
            OpCode::Add => "add",
            OpCode::Subtract => "sub",
            OpCode::Increment => "inc",
            OpCode::Decrement => "dec",
            OpCode::SysWrite => "syswrite",
            OpCode::SysRead => "sysread",
        }
    }
}

static OPCODE_KEYWORDS: phf::Map<&'static str, OpCode> = phf_map! {
    "mov" => OpCode::Move,
    "jump" => OpCode::Jump,
    "jz" => OpCode::JumpIfZero,
    "jnz" => OpCode::JumpIfNotZero,
    "add" => OpCode::Add,
    "sub" => OpCode::Subtract,
    "inc" => OpCode::Increment,
    "dec" => OpCode::Decrement,
    "syswrite" => OpCode::SysWrite,
    "sysread" => OpCode::SysRead,
};

impl TryFrom<&str> for OpCode {
    type Error = ISAError;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        OPCODE_KEYWORDS.get(value).cloned().ok_or(ISAError::InvalidOpCode)
    }
}

pub type MemoryAddress = u32;

// Аргумент операции представляется несколькими типами
#[derive(Debug, Deserialize, Serialize)]
pub enum OpArg {
    ArgLiteral(ArgLiteral),       // литерал
    RegisterId(RegisterId),       // идентификатор регистра
    MemoryByRegister(RegisterId), // доступ к памяти по значению в регистре
    MemoryAddress(MemoryAddress), // адрес в памяти
                                  // StringLiteral(String),        // литерал строки
}

impl OpArg {
    pub fn is_memory_dependent(&self) -> bool {
        matches!(self, Self::MemoryByRegister(..)) || matches!(self, Self::MemoryAddress(..))
    }

    pub fn is_arg_literal(&self) -> bool {
        matches!(self, Self::ArgLiteral(..))
    }
}

impl TryFrom<&str> for OpArg {
    type Error = ISAError;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        if let Ok(literal) = value.try_into() {
            Ok(Self::ArgLiteral(literal))
        } else if let Ok(register) = value.try_into() {
            Ok(Self::RegisterId(register))
        } else if value.starts_with('[') && value.ends_with(']') {
            Ok(Self::MemoryByRegister(value[1..value.len() - 1].try_into()?))
        } else {
            Err(ISAError::InvalidOpArg)
        }
        // } else if let Ok(register) = value.strip_prefix('[')
        //  else {
        //     Err(ISAError::InvalidOpArg)
        // }
    }
}

#[derive(Debug, Deserialize, Serialize)]
pub enum ArgLiteral {
    U32(u32), // просто число u32
    I32(i32), // просто число i32
}

impl TryFrom<&str> for ArgLiteral {
    type Error = ISAError;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        if let Ok(num) = value.parse::<u32>() {
            Ok(Self::U32(num))
        } else if let Ok(num) = value.parse::<i32>() {
            Ok(Self::I32(num))
        } else {
            Err(ISAError::InvalidOpArg)
        }
    }
}

#[derive(Clone, Debug, Deserialize, Serialize)]
pub enum RegisterId {
    Accumulator,
    Count,
    Data,
    InstructionPointer,
    StackPointer,
}

static REGISTER_ID_KEYWORDS: phf::Map<&'static str, RegisterId> = phf_map! {
    "eax" => RegisterId::Accumulator,
    "ecx" => RegisterId::Count,
    "edx" => RegisterId::Data,
    "eip" => RegisterId::InstructionPointer,
    "esp" => RegisterId::StackPointer,
};

impl TryFrom<&str> for RegisterId {
    type Error = ISAError;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        REGISTER_ID_KEYWORDS.get(value).cloned().ok_or(ISAError::InvalidRegisterId)
    }
}

#[derive(Debug, Deserialize, Serialize)]
pub enum Data {
    Char(char),
    U32(u32),
}
