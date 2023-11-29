// ================= MATH OP - START =================

use serde::{Deserialize, Serialize};

use super::*;

/// Математические инструкции. Принимают на вход 2 аргумента - dest и src и как-то модифицируют dest на основе src
#[derive(Debug, Deserialize, Serialize, Clone, PartialEq)]
pub enum MathOp {
    /// MOV - записывает значение src в dest
    Mov,
    /// ADD - прибавляет значение src к dest
    Add,
    /// SUB - вычитает значение src из dest
    Sub,
    /// CMP - вычитает значение src из dest, но не изменяет его, а только выставляет флаги
    Cmp,
    /// SHL - арифметический сдвиг src на dest бит влево
    Shl,
    /// SHR - арифметический сдвиг src на dest бит вправо
    Shr,
}

/// Верхняя часть математических инструкций
/// В реализации на уровне структуры часть слова, определяющая тип аргументов,
/// задана неявно через тип enum'а.
/// Для расчёта размера будем считать, что такая часть слова занимает 4 бита
#[derive(Debug, Deserialize, Serialize, Clone)]
pub struct MathOpHigher {
    pub opcode: MathOp,
    pub args: MathOpHigherArgs,
}

/// Нижняя часть математический инструкций
#[derive(Debug, Deserialize, Serialize, Clone)]
pub enum MathOpLower {
    /// OP из памяти в регистр, для инструкции нужно 2 слова - оставшаяся часть адреса
    MemToReg(MemoryAddressLower),
    /// OP из регистра в память, для инструкции нужно 2 слова - оставшаяся часть адреса
    RegToMem(MemoryAddressLower),
    /// OP литерала в регистр, для инструкции нужно 2 слова - оставшаяся часть литерала
    RegImmed(ImmedLower),
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
    /// из регистра в регистр, для инструкции хватает одного слова
    RegToReg(RegisterId, RegisterId),
    /// из регистра в память по второму регистру, для инструкции хватает одного слова
    RegToRegMem(RegisterId, RegisterId),
    /// из памяти по регистру во второй регистр, для инструкции хватает одного слова
    RegMemToReg(RegisterId, RegisterId),
    /// из памяти в регистр, для инструкции нужно 2 слова - весь адрес не помещается
    MemToReg(RegisterId, MemoryAddressHigher),
    /// из регистра в память, для инструкции нужно 2 слова - весь адрес не помещается
    RegToMem(RegisterId, MemoryAddressHigher),
    /// из литерала в регистр, для инструкции нужно 2 слова - весь литерал не помещается
    RegImmed(RegisterId, ImmedHigher),
}

impl From<MathOpLower> for MachineWord {
    fn from(value: MathOpLower) -> Self {
        Self::OpLower(OpLower::Math(value))
    }
}

// ================= MATH OP - END =================

// ================= BRANCH OP - START =================

/// Инструкции ветвления. Принимают на вход один аргумент (адрес) и переходят по нему при выполнении некоторого условия
#[derive(Debug, Deserialize, Serialize, Clone, PartialEq)]
pub enum BranchOp {
    /// JMP - безусловный переход
    Jmp,
    /// JZ - переход, если ZF=1
    Jz,
    /// JNZ - переход, если ZF=0
    Jnz,
    /// JS - переход, если SF=1
    Js,
    /// JNS - переход, если SF=0
    Jns,
}

#[derive(Debug, Deserialize, Serialize, Clone)]
pub struct BranchOpHigher {
    pub opcode: BranchOp,
    pub arg_higher: ImmedHigher,
}

#[derive(Debug, Deserialize, Serialize, Clone)]
pub struct BranchOpLower {
    pub arg_lower: ImmedLower,
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

/// Инструкции альтерации. Выполняют действие над регистром
#[derive(Debug, Deserialize, Serialize, Clone, PartialEq)]
pub enum AlterOp {
    /// INC - инкремент регистра
    Inc,
    /// DEC - декремент регистра
    Dec,
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

/// Инструкции ввода-вывода. Выполняют операцию ввода или вывода над указанным портом
#[derive(Debug, Deserialize, Serialize, Clone, PartialEq)]
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

/// Инструкции для работы со стэком.
#[derive(Debug, Deserialize, Serialize, Clone, PartialEq)]
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

/// Инструкции управления. Не принимают аргументов и как-либо влияют на работу процессора
#[derive(Debug, Deserialize, Serialize, Clone, PartialEq)]
pub enum ControlOp {
    /// EXIT - выполняет завершение работы
    Exit,
    /// DIV - деление значения в eax на значение из ebx. В eax кладётся частное, в edx - остаток
    Div,
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
