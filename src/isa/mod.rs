pub mod instructions;

use self::instructions::*;
use enum_dispatch::enum_dispatch;
use phf::phf_map;
use ron::ser::PrettyConfig;
use serde::{Deserialize, Serialize};
use std::{fs::File, path::Path, str::FromStr};
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
    #[error("Invalid args num")]
    InvalidArgNum,
    #[error("Invalid machine word to compose")]
    InvalidComposeAttempt,
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

    pub fn len(&self) -> u32 {
        self.code.len() as u32
    }

    pub fn entrypoint(&self) -> MemoryAddress {
        self.entrypoint
    }
}

impl From<Program> for Vec<MachineWord> {
    fn from(value: Program) -> Self {
        value.code
    }
}

#[enum_dispatch(Instruction)]
pub trait InstructionTrait: Clone {
    fn into_words(self, args: OpArgBatch) -> Result<(MachineWord, Option<MachineWord>), ISAError>;

    fn args_num(&self) -> OpArgNum;

    fn words_num(&self, args: OpArgBatch) -> Result<u32, ISAError> {
        if self.clone().into_words(args)?.1.is_some() {
            Ok(2)
        } else {
            Ok(1)
        }
    }
}

#[enum_dispatch]
#[derive(Debug, Clone, PartialEq)]
pub enum Instruction {
    Math(MathOp),
    Branch(BranchOp),
    Alter(AlterOp),
    Io(IoOp),
    Control(ControlOp),
    Stack(StackOp),
}

pub const EMPTY_WORD: MachineWord = MachineWord::Empty;
#[derive(Debug, Default, Deserialize, Serialize, Clone)]
pub enum MachineWord {
    /// Верхняя часть инструкции
    OpHigher(OpHigher),
    /// Нижняя часть инструкции
    OpLower(OpLower),
    /// "Чистые" данные
    Data(Immed),
    /// Пустое слово
    #[default]
    Empty,
}

impl MachineWord {
    pub fn as_number(&self) -> u32 {
        match self {
            MachineWord::OpHigher(op) => match op {
                OpHigher::Math(math) => match math.args {
                    MathOpHigherArgs::RegToReg(_, _)
                    | MathOpHigherArgs::RegToRegMem(_, _)
                    | MathOpHigherArgs::RegMemToReg(_, _) => 0,
                    MathOpHigherArgs::RegImmed(_, val)
                    | MathOpHigherArgs::MemToReg(_, val)
                    | MathOpHigherArgs::RegToMem(_, val) => val as u32,
                },
                OpHigher::Branch(branch) => branch.arg_higher as u32,
                OpHigher::Io(io) => io.arg as u32,
                OpHigher::Alter(_) | OpHigher::Control(_) => 0,
                OpHigher::Stack(stack) => match stack.args {
                    StackOpHigherArgs::Register(_) | StackOpHigherArgs::None => 0,
                    StackOpHigherArgs::Immed(val) => val as u32,
                },
            },
            MachineWord::OpLower(op) => match op {
                OpLower::Math(math) => match math {
                    MathOpLower::MemToReg(val)
                    | MathOpLower::RegToMem(val)
                    | MathOpLower::RegImmed(val) => *val as u32,
                },
                OpLower::Branch(branch) => branch.arg_lower as u32,
                OpLower::Stack(stack) => match stack {
                    StackOpLower::Immed(val) => *val as u32,
                },
            },
            MachineWord::Data(data) => data.clone().into(),
            MachineWord::Empty => 0,
        }
    }
}

/// Верхняя часть кода инструкции
/// В реализации на уровне структуры часть слова, определяющая конкретный тип инструкции,
/// задана неявно через тип enum'а.
/// Для расчёта размера будем считать, что такая часть слова занимает 1 байт,
#[derive(Debug, Deserialize, Serialize, Clone)]
pub enum OpHigher {
    Math(MathOpHigher),
    Branch(BranchOpHigher),
    Alter(AlterOpWord),
    Io(IoOpWord),
    Control(ControlOpWord),
    Stack(StackOpHigher),
}

/// Нижняя часть кода инструкции
/// Считаем, что "лишних" затрат памяти на повторный идентификатор инструкции нет
#[derive(Debug, Deserialize, Serialize, Clone)]
pub enum OpLower {
    Math(MathOpLower),
    Branch(BranchOpLower),
    Stack(StackOpLower),
}

static OPCODE_KEYWORDS: phf::Map<&str, Instruction> = phf_map! {
// Математические инструкции
    "mov" => Instruction::Math(MathOp::Mov),
    "add" => Instruction::Math(MathOp::Add),
    "sub" => Instruction::Math(MathOp::Sub),
    "cmp" => Instruction::Math(MathOp::Cmp),
    "shl" => Instruction::Math(MathOp::Shl),
    "shr" => Instruction::Math(MathOp::Shr),
// Инструкции ветвления
    "jmp" => Instruction::Branch(BranchOp::Jmp),
    "jz" => Instruction::Branch(BranchOp::Jz),
    "jnz" => Instruction::Branch(BranchOp::Jnz),
    "js" => Instruction::Branch(BranchOp::Js),
    "jns" => Instruction::Branch(BranchOp::Jns),
// Инструкции альтерации
    "inc" => Instruction::Alter(AlterOp::Inc),
    "dec" => Instruction::Alter(AlterOp::Dec),
// Инструкции ввода-вывода
    "in" => Instruction::Io(IoOp::In),
    "out" => Instruction::Io(IoOp::Out),
// Инструкции управления
    "div" => Instruction::Control(ControlOp::Div),
    "exit" => Instruction::Control(ControlOp::Exit),
// Интсрукции стэка
    "push" => Instruction::Stack(StackOp::Push),
    "pop" => Instruction::Stack(StackOp::Pop),
    "call" => Instruction::Stack(StackOp::Call),
    "ret" => Instruction::Stack(StackOp::Ret),
};

impl FromStr for Instruction {
    type Err = ISAError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        OPCODE_KEYWORDS.get(s).cloned().ok_or(ISAError::InvalidOpCode)
    }
}

#[derive(Debug, Deserialize, Serialize, Default, Clone, Copy, PartialEq)]
pub struct MemoryAddress(u32);
pub type MemoryAddressHigher = u16;
pub type MemoryAddressLower = u16;

impl MemoryAddress {
    pub fn higher(&self) -> ImmedHigher {
        (self.0 >> 16) as u16
    }
    pub fn lower(&self) -> ImmedLower {
        self.0 as u16
    }
    pub fn null() -> Self {
        Self(0)
    }
    pub fn add(&mut self, value: u32) {
        self.0 += value;
    }
}

impl From<Immed> for MemoryAddress {
    fn from(value: Immed) -> Self {
        Self(value.0)
    }
}

impl From<MemoryAddress> for u32 {
    fn from(value: MemoryAddress) -> Self {
        value.0
    }
}

impl From<u32> for MemoryAddress {
    fn from(value: u32) -> Self {
        Self(value)
    }
}

#[derive(Debug, Deserialize, Serialize, Clone, PartialEq)]
pub struct Immed(u32);
pub type ImmedHigher = u16;
pub type ImmedLower = u16;

impl Immed {
    pub fn new(val: u32) -> Self {
        Self(val)
    }
    pub fn of(higher: u16, lower: u16) -> Self {
        Self(((higher as u32) << 16) + lower as u32)
    }
    pub fn higher(&self) -> ImmedHigher {
        (self.0 >> 16) as u16
    }
    pub fn lower(&self) -> ImmedLower {
        self.0 as u16
    }
    pub fn null() -> Self {
        Self(0)
    }
}

impl FromStr for Immed {
    type Err = ISAError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if let Ok(num) = s.parse::<u32>() {
            Ok(Self(num))
        } else if let Ok(num) = s.parse::<i32>() {
            Ok(Self(u32::from_be_bytes(num.to_be_bytes())))
        } else if s.starts_with('\'') && s.ends_with('\'') && s.len() == 3 {
            let c = s.chars().nth(1).unwrap();
            Ok(Self(c as u32))
        } else {
            Err(ISAError::InvalidOpArg)
        }
    }
}

impl From<u32> for Immed {
    fn from(value: u32) -> Self {
        Self(value)
    }
}

impl From<Immed> for u32 {
    fn from(value: Immed) -> Self {
        value.0
    }
}

impl From<char> for Immed {
    fn from(value: char) -> Self {
        Self(value as u32)
    }
}

impl From<MemoryAddress> for Immed {
    fn from(value: MemoryAddress) -> Self {
        Self(value.0)
    }
}

/// Аргумент операции представляется несколькими типами
#[derive(Debug, Deserialize, Serialize, PartialEq)]
pub enum OpArg {
    /// литерал
    Immed(Immed),
    /// идентификатор регистра
    Reg(RegisterId),
    /// доступ к памяти по значению в регистре
    RegMem(RegisterId),
    /// адрес в памяти
    Mem(MemoryAddress),
}

impl FromStr for OpArg {
    type Err = ISAError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if let Ok(literal) = s.parse() {
            Ok(Self::Immed(literal))
        } else if let Ok(register) = s.parse() {
            Ok(Self::Reg(register))
        } else if s.starts_with('[') && s.ends_with(']') {
            Ok(Self::RegMem(s[1..s.len() - 1].parse()?))
        } else {
            Err(ISAError::InvalidOpArg)
        }
    }
}

pub enum OpArgBatch {
    Zero,
    One(OpArg),
    Two(OpArg, OpArg),
}

pub enum OpArgNum {
    Zero,
    One,
    Two,
}

/// Уникально определяет регистр.
/// В реализации на уровне структуры часть слова, определяющая id регистра
/// задана явно через тип enum'а.
/// Для расчёта размера будем считать, что такая часть слова занимает 4 бита
#[derive(Clone, Copy, Debug, Deserialize, Serialize, PartialEq)]
pub enum RegisterId {
    Accumulator,
    Base,
    Count,
    Data,
    InstructionPointer,
    StackPointer,
}

static REGISTER_ID_KEYWORDS: phf::Map<&'static str, RegisterId> = phf_map! {
    "eax" => RegisterId::Accumulator,
    "ebx" => RegisterId::Base,
    "ecx" => RegisterId::Count,
    "edx" => RegisterId::Data,
    "eip" => RegisterId::InstructionPointer,
    "esp" => RegisterId::StackPointer,
};

impl FromStr for RegisterId {
    type Err = ISAError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        REGISTER_ID_KEYWORDS.get(s).cloned().ok_or(ISAError::InvalidRegisterId)
    }
}

/// Уникально определяет порт.
pub type PortId = u16;

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_register() -> Result<(), ISAError> {
        assert_eq!(RegisterId::from_str("eax")?, RegisterId::Accumulator);
        assert_eq!(RegisterId::from_str("eip")?, RegisterId::InstructionPointer);
        assert!(RegisterId::from_str("rax").is_err());
        Ok(())
    }

    #[test]
    fn test_parse_immed() -> Result<(), ISAError> {
        assert_eq!(Immed::from_str("'c'")?, Immed(99));
        assert_eq!(Immed::from_str("32")?, Immed(32));
        assert_eq!(Immed::from_str("-1")?, Immed(u32::MAX));
        assert!(Immed::from_str("bruh").is_err());
        Ok(())
    }

    #[test]
    fn test_parse_op_arg() -> Result<(), ISAError> {
        assert_eq!(OpArg::from_str("[eax]")?, OpArg::RegMem(RegisterId::Accumulator));
        assert_eq!(OpArg::from_str("edx")?, OpArg::Reg(RegisterId::Data));
        assert_eq!(OpArg::from_str("156")?, OpArg::Immed(Immed(156)));
        assert!(OpArg::from_str("[ecx").is_err());
        Ok(())
    }

    #[test]
    fn test_instruction() -> Result<(), ISAError> {
        assert_eq!(Instruction::from_str("mov")?, Instruction::Math(MathOp::Mov));
        assert_eq!(Instruction::from_str("in")?, Instruction::Io(IoOp::In));
        assert_eq!(Instruction::from_str("call")?, Instruction::Stack(StackOp::Call));
        assert!(Instruction::from_str("mul").is_err());
        Ok(())
    }
}
