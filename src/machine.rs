use core::panic;
use std::{
    collections::HashMap,
    fmt::{Debug, Display},
    fs::File,
    io::{Read, Write},
    ops::{Index, IndexMut},
    path::Path,
};

use log::debug;
use thiserror::Error;

use crate::isa::*;

#[derive(Error, Debug)]
pub enum MachineError {
    #[error(transparent)]
    IoError(#[from] std::io::Error),
    #[error("Buffer error encountered")]
    BufferError,
    #[error(transparent)]
    ISAError(#[from] ISAError),
    #[error("No device connected to port {0} found")]
    NonexistentPort(PortId),
    #[error("Device doesn't support specified operation")]
    NotSupportedDeviceOperation,
    #[error("Expected start of an instruction; instead got this: {0:?}")]
    InvalidInstruction(MachineWord),
    #[error("Reached tick limit ({0})")]
    TickLimitReached(usize),
    #[error("Exit command issued")]
    Exit,
}

trait PortInterface: Debug {
    fn read(&mut self) -> Result<u8, MachineError>;
    fn write(&mut self, value: u8) -> Result<(), MachineError>;
}
struct InputOnlyDevice(Box<dyn Read>);
impl InputOnlyDevice {
    pub fn from_file(input_path: &Path) -> Result<Self, MachineError> {
        let file = File::open(input_path)?;
        Ok(Self(Box::new(file)))
    }
    #[allow(unused)]
    pub fn from_stdin() -> Result<Self, MachineError> {
        Ok(Self(Box::new(std::io::stdin())))
    }
}
impl Debug for InputOnlyDevice {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("InputOnlyDevice")
    }
}
impl PortInterface for InputOnlyDevice {
    fn read(&mut self) -> Result<u8, MachineError> {
        let mut buf = [0; 1];
        let _ = self.0.read(&mut buf[..])?;
        Ok(buf[0])
    }

    fn write(&mut self, _: u8) -> Result<(), MachineError> {
        Err(MachineError::NotSupportedDeviceOperation)
    }
}

struct OutputOnlyDevice(Box<dyn Write>);
impl OutputOnlyDevice {
    #[allow(unused)]
    pub fn from_file(output_path: &Path) -> Result<Self, MachineError> {
        let file = File::create(output_path)?;
        Ok(Self(Box::new(file)))
    }
    pub fn from_stdout() -> Result<Self, MachineError> {
        Ok(Self(Box::new(std::io::stdout())))
    }
}
impl Debug for OutputOnlyDevice {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("OutputOnlyDevice")
    }
}
impl PortInterface for OutputOnlyDevice {
    fn read(&mut self) -> Result<u8, MachineError> {
        Err(MachineError::NotSupportedDeviceOperation)
    }

    fn write(&mut self, value: u8) -> Result<(), MachineError> {
        let buf = [value; 1];
        let res = self.0.write(&buf)?;
        if res != 1 {
            return Err(MachineError::BufferError);
        }
        Ok(())
    }
}
impl Drop for OutputOnlyDevice {
    fn drop(&mut self) {
        let _ = self.0.flush();
    }
}

struct Memory<const SIZE: usize>(Box<[MachineWord; SIZE]>);
impl<const S: usize> Default for Memory<S> {
    fn default() -> Self {
        Self(Box::new([EMPTY_WORD; S]))
    }
}
impl<const S: usize> Index<u32> for Memory<S> {
    type Output = MachineWord;

    fn index(&self, index: u32) -> &Self::Output {
        &self.0[index as usize]
    }
}
impl<const S: usize> IndexMut<u32> for Memory<S> {
    fn index_mut(&mut self, index: u32) -> &mut Self::Output {
        &mut self.0[index as usize]
    }
}
impl<const S: usize> Debug for Memory<S> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("Memory of size {}", S))
    }
}

#[derive(Default, Debug)]
struct RegisterSet {
    acc: u32,   //eax
    count: u32, //ecx
    data: u32,  //edx
    stack: u32, //esp
    instr: u32, //eip
}

impl Index<RegisterId> for RegisterSet {
    type Output = u32;

    fn index(&self, index: RegisterId) -> &Self::Output {
        match index {
            RegisterId::Accumulator => &self.acc,
            RegisterId::Count => &self.count,
            RegisterId::Data => &self.data,
            RegisterId::InstructionPointer => &self.instr,
            RegisterId::StackPointer => &self.stack,
        }
    }
}
impl IndexMut<RegisterId> for RegisterSet {
    fn index_mut(&mut self, index: RegisterId) -> &mut Self::Output {
        match index {
            RegisterId::Accumulator => &mut self.acc,
            RegisterId::Count => &mut self.count,
            RegisterId::Data => &mut self.data,
            RegisterId::InstructionPointer => &mut self.instr,
            RegisterId::StackPointer => &mut self.stack,
        }
    }
}

#[derive(Default, Debug)]
struct PortSet {
    ports: HashMap<PortId, Box<dyn PortInterface>>,
}
impl PortSet {
    pub fn new() -> Self {
        Self::default()
    }
    pub fn add_device(&mut self, id: PortId, device: Box<dyn PortInterface>) {
        self.ports.insert(id, device);
    }
    pub fn get_device(&mut self, id: PortId) -> Result<&mut Box<dyn PortInterface>, MachineError> {
        self.ports.get_mut(&id).ok_or(MachineError::NonexistentPort(id))
    }
}

#[derive(Default, Debug)]
struct FlagSet {
    zero: bool, // ZF
    sign: bool, // SF
}

#[derive(Default, Debug)]
struct Alu {
    left: u32,
    right: u32,
    invert_left: bool,
    invert_right: bool,
}

enum AluOpCode {
    Add,
    Sub,
    Cmp,
    Mov,
    Shl,
}

impl Alu {
    pub fn set_left(&mut self, left: u32) {
        self.left = left;
    }
    pub fn reset_left(&mut self) {
        self.left = 0;
    }
    pub fn set_invert_left(&mut self) {
        self.invert_left = true;
    }

    pub fn set_right(&mut self, right: u32) {
        self.right = right;
    }
    pub fn reset_right(&mut self) {
        self.right = 0;
    }
    pub fn set_invert_right(&mut self) {
        self.invert_right = true;
    }
    pub fn set_right_one(&mut self) {
        self.right = 1;
    }

    pub fn perform(&mut self, opcode: AluOpCode, flags: Option<&mut FlagSet>) -> u32 {
        let left = if self.invert_left { !self.left } else { self.left };
        let right = if self.invert_right { !self.right } else { self.right };

        let num = match opcode {
            AluOpCode::Add => left.wrapping_add(right),
            AluOpCode::Sub => left.wrapping_sub(right),
            AluOpCode::Cmp => left.wrapping_sub(right),
            AluOpCode::Mov => right,
            AluOpCode::Shl => left << right,
        };
        if let Some(flags) = flags {
            flags.zero = num == 0;
            flags.sign = num >> 31 == 1;
        }

        self.invert_left = false;
        self.invert_right = false;
        if let AluOpCode::Cmp = opcode {
            left
        } else {
            num
        }
    }
}

#[derive(Default, Debug)]
struct DataPath<const MEMORY_SIZE: usize> {
    memory: Memory<MEMORY_SIZE>, // машинная память - ячейки машинных слов с адресами от 0 до u32::MAX
    regs: RegisterSet,           // Набор регистров
    alu: Alu,                    // Арифметико-логическое устройство
    flags: FlagSet,              // Набор флагов
    ports: PortSet,              // Порты внешних устройств
}

impl<const M: usize> DataPath<M> {
    pub fn new(program: Program, ports: PortSet) -> Self {
        let mut datapath = Self { ports, ..Default::default() };
        let entrypoint = program.entrypoint();
        datapath.regs.instr = entrypoint.into();

        let code: Vec<MachineWord> = program.into();

        for (index, word) in code.into_iter().enumerate() {
            datapath.memory[index as u32 + 1] = word;
        }
        datapath
    }
}

#[derive(Default, Debug)]
struct InstructionDecoder<const M: usize> {
    instruction: MachineWord,
    operand_buffer: MachineWord,
}
impl<const M: usize> InstructionDecoder<M> {
    pub fn decode(&mut self, alu: &mut Alu) -> Result<ControlUnitState, MachineError> {
        if let MachineWord::OpHigher(op) = &self.instruction {
            Ok(match op {
                OpHigher::Math(math) => match math.args {
                    MathOpHigherArgs::MemToReg(_, higher)
                    | MathOpHigherArgs::RegToMem(_, higher)
                    | MathOpHigherArgs::RegImmed(_, higher) => {
                        self.prepare_higher_operand(alu, higher);
                        ControlUnitState::OperandPreFetch
                    },
                    _ => ControlUnitState::InstructionExecution,
                },
                OpHigher::Branch(branch) => {
                    self.prepare_higher_operand(alu, branch.arg_higher);
                    ControlUnitState::OperandPreFetch
                },
                OpHigher::Alter(_) => ControlUnitState::InstructionExecution,
                OpHigher::Io(_) => ControlUnitState::InstructionExecution,
                OpHigher::Control(_) => ControlUnitState::InstructionExecution,
            })
        } else {
            Err(MachineError::InvalidInstruction(self.instruction.clone()))
        }
    }

    fn prepare_higher_operand(&mut self, alu: &mut Alu, higher: u16) {
        alu.set_left(higher as u32);
        alu.set_right(16); //TODO think!!
        self.operand_buffer = MachineWord::Data(Immed::new(alu.perform(AluOpCode::Shl, None)));
    }

    pub fn execute(&self, datapath: &mut DataPath<M>) -> Result<ControlUnitState, MachineError> {
        match &self.instruction {
            MachineWord::OpHigher(op) => match op {
                OpHigher::Math(math) => self.execute_math(math, datapath),
                OpHigher::Branch(branch) => self.execute_branch(branch, datapath),
                OpHigher::Alter(alter) => self.execute_alter(alter, datapath),
                OpHigher::Io(io) => self.execute_io(io, datapath),
                OpHigher::Control(control) => self.execute_control(control, datapath),
            },
            _ => Err(MachineError::InvalidInstruction(self.instruction.clone())),
        }
    }

    fn execute_math(
        &self,
        math: &MathOpHigher,
        datapath: &mut DataPath<M>,
    ) -> Result<ControlUnitState, MachineError> {
        let regs = &mut datapath.regs;

        let (left, right) = match math.args {
            MathOpHigherArgs::RegToReg(dest, src) => (regs[dest], regs[src]),
            MathOpHigherArgs::RegToRegMem(dest, src) => {
                let addr = regs[dest];
                let word = &datapath.memory[addr];
                (word.as_number(), regs[src])
            },
            MathOpHigherArgs::RegMemToReg(dest, src) => {
                let addr = regs[src];
                let word = &datapath.memory[addr];
                (regs[dest], word.as_number())
            },
            MathOpHigherArgs::MemToReg(dest, _) => {
                let number = datapath.memory[self.operand_buffer.as_number()].as_number();
                (regs[dest], number)
            },
            MathOpHigherArgs::RegToMem(src, _) => {
                let number = datapath.memory[self.operand_buffer.as_number()].as_number();
                (number, regs[src])
            },
            MathOpHigherArgs::RegImmed(dest, _) => (regs[dest], self.operand_buffer.as_number()),
        };
        datapath.alu.set_left(left);
        datapath.alu.set_right(right);

        let flags = Some(&mut datapath.flags);
        let res = match math.opcode {
            MathOp::Mov => datapath.alu.perform(AluOpCode::Mov, flags),
            MathOp::Add => datapath.alu.perform(AluOpCode::Add, flags),
            MathOp::Sub => datapath.alu.perform(AluOpCode::Sub, flags),
            MathOp::Cmp => datapath.alu.perform(AluOpCode::Cmp, flags),
        };

        match math.args {
            MathOpHigherArgs::RegToReg(dest, _) => regs[dest] = res,
            MathOpHigherArgs::RegToRegMem(dest, _) => {
                datapath.memory[regs[dest]] = MachineWord::Data(Immed::new(res))
            },
            MathOpHigherArgs::RegMemToReg(dest, _) => regs[dest] = res,
            MathOpHigherArgs::MemToReg(dest, _) => regs[dest] = res,
            MathOpHigherArgs::RegToMem(_, _) => {
                datapath.memory[self.operand_buffer.as_number()] =
                    MachineWord::Data(Immed::new(res))
            },
            MathOpHigherArgs::RegImmed(dest, _) => regs[dest] = res,
        };

        Ok(ControlUnitState::InstructionPreFetch)
    }

    fn execute_branch(
        &self,
        branch: &BranchOpHigher,
        datapath: &mut DataPath<M>,
    ) -> Result<ControlUnitState, MachineError> {
        let addr = self.operand_buffer.as_number();

        let regs = &mut datapath.regs;
        let flags = &datapath.flags;
        let do_branch = match branch.opcode {
            BranchOp::Jmp => true,
            BranchOp::Jz => flags.zero,
            BranchOp::Jnz => !flags.zero,
            BranchOp::Js => flags.sign,
            BranchOp::Jns => !flags.sign,
        };
        if do_branch {
            regs[RegisterId::InstructionPointer] = addr;
            Ok(ControlUnitState::InstructionFetch)
        } else {
            Ok(ControlUnitState::InstructionPreFetch)
        }
    }

    fn execute_alter(
        &self,
        alter: &AlterOpWord,
        datapath: &mut DataPath<M>,
    ) -> Result<ControlUnitState, MachineError> {
        let register = alter.arg;
        datapath.alu.set_left(datapath.regs[register]);
        match alter.opcode {
            AlterOp::Inc => {
                datapath.alu.set_right_one();
            },
            AlterOp::Dec => {
                datapath.alu.reset_right();
                datapath.alu.set_invert_right();
            },
        }
        datapath.regs[register] = datapath.alu.perform(AluOpCode::Add, Some(&mut datapath.flags));
        Ok(ControlUnitState::InstructionPreFetch)
    }

    fn execute_io(
        &self,
        io: &IoOpWord,
        datapath: &mut DataPath<M>,
    ) -> Result<ControlUnitState, MachineError> {
        let device = datapath.ports.get_device(io.arg)?;
        let acc = &mut datapath.regs[RegisterId::Accumulator];
        match io.opcode {
            IoOp::In => {
                *acc = device.read()?.into();
            },
            IoOp::Out => device.write(*acc as u8)?,
        };
        Ok(ControlUnitState::InstructionPreFetch)
    }

    fn execute_control(
        &self,
        control: &ControlOpWord,
        _: &mut DataPath<M>,
    ) -> Result<ControlUnitState, MachineError> {
        match control.0 {
            ControlOp::Exit => Err(MachineError::Exit),
        }
    }
}

#[derive(Default, Debug)]
enum ControlUnitState {
    InstructionPreFetch,
    #[default]
    InstructionFetch,
    InstructionDecode,
    OperandPreFetch,
    OperandFetch,
    InstructionExecution,
}
impl Display for ControlUnitState {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let val = match self {
            ControlUnitState::InstructionPreFetch => "IPF",
            ControlUnitState::InstructionFetch => "InF",
            ControlUnitState::InstructionDecode => "InD",
            ControlUnitState::OperandPreFetch => "OPF",
            ControlUnitState::OperandFetch => "OnF",
            ControlUnitState::InstructionExecution => "InE",
        };
        f.write_str(val)
    }
}

#[derive(Default, Debug)]
struct ControlUnit<const MEM_SIZE: usize, const TICK_LIMIT: usize> {
    datapath: DataPath<MEM_SIZE>,          // Блок обработки данных
    decoder: InstructionDecoder<MEM_SIZE>, // Декодер инструкций
    tick: usize,                           // Номер текущего такта
    state: ControlUnitState,               // Текущее состояние
}

impl<const MEM_SIZE: usize, const TICK_LIMIT: usize> ControlUnit<MEM_SIZE, TICK_LIMIT> {
    pub fn new(datapath: DataPath<MEM_SIZE>) -> Self {
        Self { datapath, ..Default::default() }
    }

    pub fn tick(&mut self) -> Result<(), MachineError> {
        debug!("{:#}", self);

        self.tick += 1;
        if self.tick > TICK_LIMIT {
            return Err(MachineError::TickLimitReached(TICK_LIMIT));
        }

        self.state = match self.state {
            ControlUnitState::InstructionPreFetch => {
                self.inc_instruction_pointer()?;
                Ok(ControlUnitState::InstructionFetch)
            },
            ControlUnitState::OperandPreFetch => {
                self.inc_instruction_pointer()?;
                Ok(ControlUnitState::OperandFetch)
            },
            ControlUnitState::InstructionFetch => self.fetch_instruction(),
            ControlUnitState::InstructionDecode => self.decode_instruction(),
            ControlUnitState::OperandFetch => self.fetch_operand(),
            ControlUnitState::InstructionExecution => self.execute_instruction(),
        }?;

        Ok(())
    }

    fn inc_instruction_pointer(&mut self) -> Result<(), MachineError> {
        let datapath = &mut self.datapath;
        let alu = &mut datapath.alu;
        let regs = &mut datapath.regs;

        alu.set_left(regs[RegisterId::InstructionPointer]);
        alu.set_right_one();
        regs[RegisterId::InstructionPointer] = alu.perform(AluOpCode::Add, None);
        Ok(())
    }

    fn fetch_instruction(&mut self) -> Result<ControlUnitState, MachineError> {
        let datapath = &mut self.datapath;
        let regs = &mut datapath.regs;
        let memory = &datapath.memory;

        self.decoder.instruction = memory[regs[RegisterId::InstructionPointer]].clone();
        Ok(ControlUnitState::InstructionDecode)
    }

    fn decode_instruction(&mut self) -> Result<ControlUnitState, MachineError> {
        self.decoder.decode(&mut self.datapath.alu)
    }

    fn fetch_operand(&mut self) -> Result<ControlUnitState, MachineError> {
        let datapath = &mut self.datapath;
        let regs = &mut datapath.regs;
        let memory = &datapath.memory;

        self.decoder
            .operand_buffer
            .compose(memory[regs[RegisterId::InstructionPointer]].clone())?;
        Ok(ControlUnitState::InstructionExecution)
    }

    fn execute_instruction(&mut self) -> Result<ControlUnitState, MachineError> {
        self.decoder.execute(&mut self.datapath)
    }
}
impl<const MEM_SIZE: usize, const TICK_LIMIT: usize> Display for ControlUnit<MEM_SIZE, TICK_LIMIT> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!(
            "TICK: {tick:4}; REGS: eax:{eax:4}, ecx:{ecx:4}, edx:{edx:4}, eip:{eip:4}, esp:{esp:4}; STATE: {state}; INSTR: {instr:?}",
            tick = self.tick,
            eax = self.datapath.regs.acc,
            ecx = self.datapath.regs.count,
            edx = self.datapath.regs.data,
            eip = self.datapath.regs.instr,
            esp = self.datapath.regs.stack,
            instr = self.decoder.instruction,
            state = self.state
        ))
    }
}

pub fn main(code_path: &Path, input_path: &Path) -> Result<(), MachineError> {
    debug!("Starting virtual machine");

    let program = Program::read_from_file(code_path)?;

    let mut ports = PortSet::new();
    ports.add_device(0, Box::new(InputOnlyDevice::from_file(input_path)?));
    ports.add_device(1, Box::new(OutputOnlyDevice::from_stdout()?));

    let datapath = DataPath::new(program, ports);
    let mut control_unit: ControlUnit<4096, 2048> = ControlUnit::new(datapath);

    loop {
        if let Err(e) = control_unit.tick() {
            if matches!(e, MachineError::Exit) {
                break;
            } else {
                return Err(e);
            }
        }
    }

    Ok(())
}
