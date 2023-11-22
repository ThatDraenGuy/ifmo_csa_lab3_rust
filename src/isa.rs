use std::{fs::File, path::Path, str::FromStr};

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

#[derive(Debug, Clone)]
pub enum Instruction {
    Math(MathOp),
    Branch(BranchOp),
    Alter(AlterOp),
    Io(IoOp),
    Control(ControlOp),
    Stack(StackOp),
}

impl InstructionTrait for Instruction {
    fn into_words(self, args: OpArgBatch) -> Result<(MachineWord, Option<MachineWord>), ISAError> {
        match self {
            Instruction::Math(op) => op.into_words(args),
            Instruction::Branch(op) => op.into_words(args),
            Instruction::Alter(op) => op.into_words(args),
            Instruction::Io(op) => op.into_words(args),
            Instruction::Control(op) => op.into_words(args),
            Instruction::Stack(op) => op.into_words(args),
        }
    }

    fn args_num(&self) -> OpArgNum {
        match self {
            Instruction::Math(op) => op.args_num(),
            Instruction::Branch(op) => op.args_num(),
            Instruction::Alter(op) => op.args_num(),
            Instruction::Io(op) => op.args_num(),
            Instruction::Control(op) => op.args_num(),
            Instruction::Stack(op) => op.args_num(),
        }
    }
}

pub const EMPTY_WORD: MachineWord = MachineWord::Empty;
#[derive(Debug, Default, Deserialize, Serialize, Clone)]
pub enum MachineWord {
    OpHigher(OpHigher), // Верхняя часть инструкции
    OpLower(OpLower),   // Нижняя часть инструкции
    Data(Immed),        // "Чистые" данные
    #[default]
    Empty, // Пустое слово
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

// Верхняя часть кода инструкции
// В реализации на уровне структуры часть слова, определяющая конкретный тип инструкции,
// задана неявно через тип enum'а.
// Для расчёта размера будем считать, что такая часть слова занимает 1 байт,
#[derive(Debug, Deserialize, Serialize, Clone)]
pub enum OpHigher {
    Math(MathOpHigher),
    Branch(BranchOpHigher),
    Alter(AlterOpWord),
    Io(IoOpWord),
    Control(ControlOpWord),
    Stack(StackOpHigher),
}

// Нижняя часть кода инструкции
// Считаем, что "лишних" затрат памяти на повторный идентификатор инструкции нет
#[derive(Debug, Deserialize, Serialize, Clone)]
pub enum OpLower {
    Math(MathOpLower),
    Branch(BranchOpLower),
    Stack(StackOpLower),
}

// ================= MATH OP - START =================

// Математические инструкции. Принимают на вход 2 аргумента - dest и src и как-то модифицируют dest на основе src
#[derive(Debug, Deserialize, Serialize, Clone)]
pub enum MathOp {
    Mov, // MOV - записывает значение src в dest
    Add, // ADD - прибавляет значение src к dest
    Sub, // SUB - вычитает значение src из dest
    Cmp, // CMP - вычитает значение src из dest, но не изменяет его, а только выставляет флаги
    Shl, // SHL - арифметический сдвиг src на dest бит влево
    Shr, // SHR - арифметический сдвиг src на dest бит вправо
}

// Верхняя часть математических инструкций
// В реализации на уровне структуры часть слова, определяющая тип аргументов,
// задана неявно через тип enum'а.
// Для расчёта размера будем считать, что такая часть слова занимает 4 бита
#[derive(Debug, Deserialize, Serialize, Clone)]
pub struct MathOpHigher {
    pub opcode: MathOp,
    pub args: MathOpHigherArgs,
}

// Нижняя часть математический инструкций
#[derive(Debug, Deserialize, Serialize, Clone)]
pub enum MathOpLower {
    MemToReg(MemoryAddressLower), // MOV из памяти в регистр, для инструкции нужно 2 слова - оставшаяся часть адреса
    RegToMem(MemoryAddressLower), // MOV из регистра в память, для инструкции нужно 2 слова - оставшаяся часть адреса
    RegImmed(ImmedLower), // MOV литерала в регистр, для инструкции нужно 2 слова - оставшаяся часть литерала
}

impl InstructionTrait for MathOp {
    fn into_words(self, args: OpArgBatch) -> Result<(MachineWord, Option<MachineWord>), ISAError> {
        if let OpArgBatch::Two(fst, snd) = args {
            let (higher, lower) = self.as_args(fst, snd)?;
            Ok((MathOpHigher { opcode: self, args: higher }.into(), lower.map(|l| l.into())))
        } else {
            Err(ISAError::InvalidArgNum)
        }
    }

    fn args_num(&self) -> OpArgNum {
        OpArgNum::Two
    }
}

impl MathOp {
    fn as_args(
        &self,
        fst: OpArg,
        snd: OpArg,
    ) -> Result<(MathOpHigherArgs, Option<MathOpLower>), ISAError> {
        match (fst, snd) {
            (OpArg::Reg(register), OpArg::Immed(immed)) => Ok((
                MathOpHigherArgs::RegImmed(register, immed.higher()),
                Some(MathOpLower::RegImmed(immed.lower())),
            )),
            (OpArg::Reg(fst), OpArg::Reg(snd)) => Ok((MathOpHigherArgs::RegToReg(fst, snd), None)),
            (OpArg::Reg(reg), OpArg::RegMem(reg_mem)) => {
                Ok((MathOpHigherArgs::RegMemToReg(reg, reg_mem), None))
            },
            (OpArg::Reg(reg), OpArg::Mem(mem)) => Ok((
                MathOpHigherArgs::MemToReg(reg, mem.higher()),
                Some(MathOpLower::MemToReg(mem.lower())),
            )),
            (OpArg::RegMem(reg_mem), OpArg::Reg(reg)) => {
                Ok((MathOpHigherArgs::RegToRegMem(reg_mem, reg), None))
            },
            (OpArg::Mem(mem), OpArg::Reg(reg)) => Ok((
                MathOpHigherArgs::RegToMem(reg, mem.higher()),
                Some(MathOpLower::RegToMem(mem.lower())),
            )),
            _ => Err(ISAError::InvalidOpArg),
        }
    }
}

impl From<MathOpHigher> for MachineWord {
    fn from(value: MathOpHigher) -> Self {
        Self::OpHigher(OpHigher::Math(value))
    }
}

#[derive(Debug, Deserialize, Serialize, Clone)]
pub enum MathOpHigherArgs {
    RegToReg(RegisterId, RegisterId), // из регистра в регистр, для инструкции хватает одного слова
    RegToRegMem(RegisterId, RegisterId), // из регистра в память по второму регистру, для инструкции хватает одного слова
    RegMemToReg(RegisterId, RegisterId), // из памяти по регистру во второй регистр, для инструкции хватает одного слова
    MemToReg(RegisterId, MemoryAddressHigher), // из памяти в регистр, для инструкции нужно 2 слова - весь адрес не помещается
    RegToMem(RegisterId, MemoryAddressHigher), // из регистра в память, для инструкции нужно 2 слова - весь адрес не помещается
    RegImmed(RegisterId, ImmedHigher), // из литерала в регистр, для инструкции нужно 2 слова - весь литерал не помещается
}

impl From<MathOpLower> for MachineWord {
    fn from(value: MathOpLower) -> Self {
        Self::OpLower(OpLower::Math(value))
    }
}

// ================= MATH OP - END =================

// ================= BRANCH OP - START =================

// Инструкции ветвления. Принимают на вход один аргумент (адрес) и переходят по нему при выполнении некоторого условия
#[derive(Debug, Deserialize, Serialize, Clone)]
pub enum BranchOp {
    Jmp, // JMP - безусловный переход
    Jz,  // JZ - переход, если ZF=1
    Jnz, // JNZ - переход, если ZF=0
    Js,  // JS - переход, если SF=1
    Jns, // JNS - переход, если SF=0
}

#[derive(Debug, Deserialize, Serialize, Clone)]
pub struct BranchOpHigher {
    pub opcode: BranchOp,
    pub arg_higher: ImmedHigher,
}

#[derive(Debug, Deserialize, Serialize, Clone)]
pub struct BranchOpLower {
    arg_lower: ImmedLower,
}

impl InstructionTrait for BranchOp {
    fn into_words(self, args: OpArgBatch) -> Result<(MachineWord, Option<MachineWord>), ISAError> {
        if let OpArgBatch::One(arg) = args {
            if let OpArg::Immed(value) = arg {
                Ok((
                    BranchOpHigher { opcode: self, arg_higher: value.higher() }.into(),
                    Some(BranchOpLower { arg_lower: value.lower() }.into()),
                ))
            } else {
                Err(ISAError::InvalidOpArg)
            }
        } else {
            Err(ISAError::InvalidArgNum)
        }
    }

    fn args_num(&self) -> OpArgNum {
        OpArgNum::One
    }
}

impl From<BranchOpHigher> for MachineWord {
    fn from(value: BranchOpHigher) -> Self {
        Self::OpHigher(OpHigher::Branch(value))
    }
}

impl From<BranchOpLower> for MachineWord {
    fn from(value: BranchOpLower) -> Self {
        Self::OpLower(OpLower::Branch(value))
    }
}

// ================= BRANCH OP - END =================

// ================= ALTER OP - START =================

// Инструкции альтерации. Выполняют действие над регистром
#[derive(Debug, Deserialize, Serialize, Clone)]
pub enum AlterOp {
    Inc, // INC - инкремент регистра
    Dec, // DEC - декремент регистра
}

#[derive(Debug, Deserialize, Serialize, Clone)]
pub struct AlterOpWord {
    pub opcode: AlterOp,
    pub arg: RegisterId,
}

impl InstructionTrait for AlterOp {
    fn into_words(self, args: OpArgBatch) -> Result<(MachineWord, Option<MachineWord>), ISAError> {
        if let OpArgBatch::One(arg) = args {
            if let OpArg::Reg(register) = arg {
                Ok((AlterOpWord { opcode: self, arg: register }.into(), None))
            } else {
                Err(ISAError::InvalidOpArg)
            }
        } else {
            Err(ISAError::InvalidArgNum)
        }
    }

    fn args_num(&self) -> OpArgNum {
        OpArgNum::One
    }
}

impl From<AlterOpWord> for MachineWord {
    fn from(value: AlterOpWord) -> Self {
        Self::OpHigher(OpHigher::Alter(value))
    }
}

// ================= ALTER OP - END =================

// ================= IO OP - START =================

// Инструкции ввода-вывода. Выполняют операцию ввода или вывода над указанным портом
#[derive(Debug, Deserialize, Serialize, Clone)]
pub enum IoOp {
    In,
    Out,
}
#[derive(Debug, Deserialize, Serialize, Clone)]
pub struct IoOpWord {
    pub opcode: IoOp,
    pub arg: PortId,
}

impl InstructionTrait for IoOp {
    fn into_words(self, args: OpArgBatch) -> Result<(MachineWord, Option<MachineWord>), ISAError> {
        if let OpArgBatch::One(arg) = args {
            if let OpArg::Immed(value) = arg {
                Ok((
                    IoOpWord {
                        opcode: self,
                        // ID портов ограничены 2-мя байтами, значение литерала - 4-мя
                        arg: value.0.try_into().map_err(|_| ISAError::InvalidOpArg)?,
                    }
                    .into(),
                    None,
                ))
            } else {
                Err(ISAError::InvalidOpArg)
            }
        } else {
            Err(ISAError::InvalidArgNum)
        }
    }

    fn args_num(&self) -> OpArgNum {
        OpArgNum::One
    }
}

impl From<IoOpWord> for MachineWord {
    fn from(value: IoOpWord) -> Self {
        Self::OpHigher(OpHigher::Io(value))
    }
}

// ================= IO OP - END =================

// ================= STACK OP - START =================

// Инструкции для работы со стэком.
#[derive(Debug, Deserialize, Serialize, Clone)]
pub enum StackOp {
    Push,
    Pop,
    Call,
    Ret,
}

#[derive(Debug, Deserialize, Serialize, Clone)]
pub struct StackOpHigher {
    pub opcode: StackOp,
    pub args: StackOpHigherArgs,
}

#[derive(Debug, Deserialize, Serialize, Clone)]
pub enum StackOpHigherArgs {
    Register(RegisterId),
    Immed(ImmedHigher),
    None,
}

#[derive(Debug, Deserialize, Serialize, Clone)]
pub enum StackOpLower {
    Immed(ImmedLower),
}

impl InstructionTrait for StackOp {
    fn into_words(self, args: OpArgBatch) -> Result<(MachineWord, Option<MachineWord>), ISAError> {
        match self {
            StackOp::Push => {
                if let OpArgBatch::One(arg) = args {
                    match arg {
                        OpArg::Immed(value) => Ok((
                            StackOpHigher {
                                opcode: self,
                                args: StackOpHigherArgs::Immed(value.higher()),
                            }
                            .into(),
                            Some(StackOpLower::Immed(value.lower()).into()),
                        )),
                        OpArg::Reg(id) => Ok((
                            StackOpHigher { opcode: self, args: StackOpHigherArgs::Register(id) }
                                .into(),
                            None,
                        )),
                        _ => Err(ISAError::InvalidOpArg),
                    }
                } else {
                    Err(ISAError::InvalidArgNum)
                }
            },
            StackOp::Pop => {
                if let OpArgBatch::One(arg) = args {
                    if let OpArg::Reg(id) = arg {
                        Ok((
                            StackOpHigher { opcode: self, args: StackOpHigherArgs::Register(id) }
                                .into(),
                            None,
                        ))
                    } else {
                        Err(ISAError::InvalidOpArg)
                    }
                } else {
                    Err(ISAError::InvalidArgNum)
                }
            },
            StackOp::Call => {
                if let OpArgBatch::One(arg) = args {
                    if let OpArg::Immed(value) = arg {
                        Ok((
                            StackOpHigher {
                                opcode: self,
                                args: StackOpHigherArgs::Immed(value.higher()),
                            }
                            .into(),
                            Some(StackOpLower::Immed(value.lower()).into()),
                        ))
                    } else {
                        Err(ISAError::InvalidOpArg)
                    }
                } else {
                    Err(ISAError::InvalidArgNum)
                }
            },
            StackOp::Ret => {
                if let OpArgBatch::Zero = args {
                    Ok((StackOpHigher { opcode: self, args: StackOpHigherArgs::None }.into(), None))
                } else {
                    Err(ISAError::InvalidArgNum)
                }
            },
        }
    }

    fn args_num(&self) -> OpArgNum {
        match self {
            StackOp::Push | StackOp::Pop | StackOp::Call => OpArgNum::One,
            StackOp::Ret => OpArgNum::Zero,
        }
    }
}

impl From<StackOpHigher> for MachineWord {
    fn from(value: StackOpHigher) -> Self {
        Self::OpHigher(OpHigher::Stack(value))
    }
}
impl From<StackOpLower> for MachineWord {
    fn from(value: StackOpLower) -> Self {
        Self::OpLower(OpLower::Stack(value))
    }
}
// ================= STACK OP - END =================

// ================= CONTROL OP - START =================

// Инструкции управления. Не принимают аргументов и как-либо влияют на работу процессора
#[derive(Debug, Deserialize, Serialize, Clone)]
pub enum ControlOp {
    Exit, // выполняет завершение работы
}

#[derive(Debug, Deserialize, Serialize, Clone)]
pub struct ControlOpWord(pub ControlOp);

impl InstructionTrait for ControlOp {
    fn into_words(self, args: OpArgBatch) -> Result<(MachineWord, Option<MachineWord>), ISAError> {
        if let OpArgBatch::Zero = args {
            Ok((ControlOpWord(self).into(), None))
        } else {
            Err(ISAError::InvalidArgNum)
        }
    }

    fn args_num(&self) -> OpArgNum {
        OpArgNum::Zero
    }
}

impl From<ControlOpWord> for MachineWord {
    fn from(value: ControlOpWord) -> Self {
        Self::OpHigher(OpHigher::Control(value))
    }
}

// ================= CONTROL OP - END =================

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

#[derive(Debug, Deserialize, Serialize, Clone)]
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

// Аргумент операции представляется несколькими типами
#[derive(Debug, Deserialize, Serialize)]
pub enum OpArg {
    Immed(Immed),       // литерал
    Reg(RegisterId),    // идентификатор регистра
    RegMem(RegisterId), // доступ к памяти по значению в регистре
    Mem(MemoryAddress), // адрес в памяти
                        // StringLiteral(String),        // литерал строки
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

// Уникально определяет регистр.
// В реализации на уровне структуры часть слова, определяющая id регистра
// задана явно через тип enum'а
// Для расчёта размера будем считать, что такая часть слова занимает 4 бита
#[derive(Clone, Copy, Debug, Deserialize, Serialize)]
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

// Уникально определяет порт.
pub type PortId = u16;
