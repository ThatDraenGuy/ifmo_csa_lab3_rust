use std::fmt::Display;

use crate::isa::*;

// ------------------------- MEMORY -------------------------
mod memory {
    use crate::isa::{MachineWord, EMPTY_WORD};
    use std::{
        fmt::Debug,
        ops::{Index, IndexMut},
    };

    pub struct Memory<const SIZE: usize>(Box<[MachineWord; SIZE]>);
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
}
use self::memory::Memory;

pub mod ports {
    use std::{
        collections::HashMap,
        fmt::Debug,
        fs::File,
        io::{Read, Write},
        path::Path,
    };

    use crate::{isa::PortId, machine::MachineError};
    #[derive(Debug)]
    pub enum PortDevice {
        InputOnly(InputOnlyDevice),
        OutputOnly(OutputOnlyDevice),
    }
    impl PortDevice {
        pub fn read(&mut self) -> Result<u8, MachineError> {
            match self {
                PortDevice::InputOnly(device) => device.read(),
                PortDevice::OutputOnly(_) => Err(MachineError::NotSupportedDeviceOperation),
            }
        }
        pub fn write(&mut self, value: u8) -> Result<(), MachineError> {
            match self {
                PortDevice::InputOnly(_) => Err(MachineError::NotSupportedDeviceOperation),
                PortDevice::OutputOnly(device) => device.write(value),
            }
        }
    }

    #[allow(unused)] // Разрешаем неиспользованные варианты
    #[derive(Debug)]
    pub enum InputOnlyDevice {
        FromFile(File),
        FromStdin,
    }

    #[allow(unused)] // Разрешаем неиспользованные варианты
    impl InputOnlyDevice {
        pub fn from_file(path: &Path) -> Result<Self, MachineError> {
            let file = File::open(path)?;
            Ok(Self::FromFile(file))
        }
        pub fn from_stdin() -> Self {
            Self::FromStdin
        }
        pub fn read(&mut self) -> Result<u8, MachineError> {
            let mut buf = [0; 1];
            let res = match self {
                InputOnlyDevice::FromFile(file) => file.read(&mut buf),
                InputOnlyDevice::FromStdin => std::io::stdin().read(&mut buf),
            }?;
            if res != 1 {
                Ok(0)
            } else {
                Ok(buf[0])
            }
        }
    }

    #[derive(Debug)]
    #[allow(unused)] // Разрешаем неиспользованные варианты
    pub enum OutputOnlyDevice {
        ToFile(File),
        ToStdout,
    }

    #[allow(unused)] // Разрешаем неиспользованные варианты
    impl OutputOnlyDevice {
        pub fn to_stdout() -> Self {
            Self::ToStdout
        }

        pub fn to_file(path: &Path) -> Result<Self, MachineError> {
            let file = File::create(path)?;
            Ok(Self::ToFile(file))
        }
        pub fn write(&mut self, value: u8) -> Result<(), MachineError> {
            let buf = [value; 1];
            let res = match self {
                OutputOnlyDevice::ToFile(file) => file.write(&buf),
                // OutputOnlyDevice::ToStdout => std::io::stdout().lock().write(&buf),
                OutputOnlyDevice::ToStdout => {
                    let mut handle = std::io::stdout().lock();
                    let res = handle.write(&buf)?;
                    handle.flush()?;
                    Ok(res)
                },
            }?;
            if res != 1 {
                return Err(MachineError::BufferError);
            }
            Ok(())
        }
    }

    #[derive(Default, Debug)]
    pub struct PortSet {
        ports: HashMap<PortId, PortDevice>,
    }
    impl PortSet {
        pub fn new() -> Self {
            Self::default()
        }
        pub fn add_device(&mut self, id: PortId, device: PortDevice) {
            self.ports.insert(id, device);
        }
        pub fn get_device(&mut self, id: PortId) -> Result<&mut PortDevice, MachineError> {
            self.ports.get_mut(&id).ok_or(MachineError::NonexistentPort(id))
        }
    }
}
use self::ports::PortSet;

// ------------------------- FLAG SET -------------------------
#[derive(Default, Debug)]
pub struct FlagSet {
    pub zero: bool, // ZF
    pub sign: bool, // SF
}

// ------------------------- REGISTER SET -------------------------
mod register_set {
    use std::ops::{Index, IndexMut};

    use crate::isa::RegisterId;

    #[derive(Default, Debug)]
    pub struct RegisterSet {
        pub acc: u32,   //eax
        pub base: u32,  //ebx
        pub count: u32, //ecx
        pub data: u32,  //edx
        pub stack: u32, //esp
        pub instr: u32, //eip
    }

    impl Index<RegisterId> for RegisterSet {
        type Output = u32;

        fn index(&self, index: RegisterId) -> &Self::Output {
            match index {
                RegisterId::Accumulator => &self.acc,
                RegisterId::Base => &self.base,
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
                RegisterId::Base => &mut self.base,
                RegisterId::Count => &mut self.count,
                RegisterId::Data => &mut self.data,
                RegisterId::InstructionPointer => &mut self.instr,
                RegisterId::StackPointer => &mut self.stack,
            }
        }
    }
}
use self::register_set::RegisterSet;

// ------------------------- ALU -------------------------
mod alu {
    use super::FlagSet;

    #[derive(Default, Debug)]
    pub struct Alu {
        left: u32,
        right: u32,
        invert_left: bool,
        invert_right: bool,
        add_right_one: bool,
    }

    pub enum AluOpCode {
        Add,
        Cmp,
        Mov,
        Stc,
        Shl,
        Shr,
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
        pub fn set_add_right_one(&mut self) {
            self.add_right_one = true;
        }

        pub(super) fn perform(&mut self, opcode: AluOpCode, flags: Option<&mut FlagSet>) -> u32 {
            let left = if self.invert_left { !self.left } else { self.left };
            let right = if self.invert_right { !self.right } else { self.right };
            let right = if self.add_right_one { right + 1 } else { right };

            let num = match opcode {
                AluOpCode::Add => left.wrapping_add(right),
                AluOpCode::Cmp => left.wrapping_sub(right),
                AluOpCode::Mov => right,
                AluOpCode::Stc => (right & 0xFFFF0000) + (left & 0x0000FFFF),
                AluOpCode::Shl => left << right,
                AluOpCode::Shr => left >> right,
            };

            if let Some(flags) = flags {
                flags.zero = num == 0;
                flags.sign = num >> 31 == 1;
            }

            self.invert_left = false;
            self.invert_right = false;
            self.add_right_one = false;
            if let AluOpCode::Cmp = opcode {
                left
            } else {
                num
            }
        }
    }
}
use self::alu::Alu;
pub use self::alu::AluOpCode;

use super::MachineError;

// ------------------------- DATAPATH -------------------------
/// ```ignore
///                     to Decoder
///                   ▲     ▲     ▲                       │from Decoder               │from Decoder    ▲to Decoder
///                   │     │     │                       │                           │                │
///                   │     │     │                       │                           │                │
///                   │     │     │                       │                           │                │
///                   │     │     │                       │                           │                │
///                   │     │     │                       │                           │                │
///                   │     │     │                       │ ┌────────────────┐        │                │
///                   │     │     │          ┌────────────┼─┤     Memory     │    ┌───▼────┐           │
///                   │     │     │          │            │ │                ◄────┤Mem Addr◄──┐        │
///                   │     │     │   sel ┌──▼──┐         │ │                │    └────────┘  │        │
///                   │     │     │   ────►DEMUX│         │ │                │                │        │
///                   │     │     │       └┬─┬─┬┘         │ │                │RD SIG          │        │
///                   │     │     │        │ │ │          │ │                ◄────            │        │
///                   │     │     └────────┘ │ │          │ │                │                │        │
///                   │     │                │ │          │ │                │                │        │
///                   │     │     ┌──────────┘ │          │ │                │WR SIG          │        │
///                   │     │     │            │          │ │                ◄───             │        │
///                   │  ┌──┴─────▼────┐       │          │ │                │                │        │
/// to/from Ports     │  │  Registers  │       │          │ │                │   ┌──────────┐ │        │
/// ◄─────────────────┼──►             │   ┌───▼───────┐  │ │                ◄───┤Mem In Buf│ │        │
///                   │  │             │   │Mem Out Buf│  │ │                │   └─▲────────┘ │        │
///                   │  └─▲─────┬─────┘   └────┬──────┘  │ └────────────────┘     │          │        │
///                   │    │     │              │         │                        │          │        │
///                   │    │     │ ┌────────────┤         │                        │          │        │
///                   │    │     │ │            │         │                        │          │        │
///                   │    │     ├─┼──────────┐ │ ┌───────┘                        │          │        │
///                   │    │     │ │          │ │ │                                │          │        │
///                   │    │ sel┌▼─▼┐        ┌▼─▼─▼┐sel                            │          │        │
///                   │    │ ───►MUX│        │ MUX ◄───                            │          │        │
///                   │    │    └─┬─┘        └─┬───┘                               │          │        │
///                   │    │      │            │                                   │          │        │
///                   │    │    ┌─▼────────────▼─┐                                 │          │        │
///                   │    │    │      ALU       │                                 │          │        │
///                   │    │    │                │ALU signals                      │          │        │
///                   │    │    │                ◄───────                          │          │        │
///                   │    │    │                │                                 │          │        │
///                   │    │    │                │                                 │          │        │
///                   │    │    └───────┬────────┘                                 │          │        │
///                   │    │            │                        ┌─────┐           │          │        │
///                   │    │            ├────────────────────────►flags├───────────┼──────────┼────────┘
///                   │    │            │                        └─────┘           │          │
///                   │    │       ┌────▼────┐sel                                  │          │
///                   │    │       │  DEMUX  ◄───                                  │          │
///                   │    │       └─┬─┬──┬─┬┘                                     │          │
///                   │    │         │ │  │ │                                      │          │
///                   │    └─────────┘ │  │ └──────────────────────────────────────┘          │
///                   │                │  │                                                   │
///                   └────────────────┘  └───────────────────────────────────────────────────┘
/// ```
#[derive(Default, Debug)]
pub struct DataPath<const MEMORY_SIZE: usize> {
    memory: Memory<MEMORY_SIZE>,
    regs: RegisterSet,
    flags: FlagSet,
    ports: PortSet,
    alu: Alu,
    mem_addr: MemoryAddress,
    mem_in_buf: MachineWord,
    mem_out_buf: MachineWord,
}

pub enum ReadDemuxSel<'a> {
    MemOutBuf,
    Registers(RegisterId),
    Decoder { dest: &'a mut MachineWord },
}
pub enum AluLeftMuxSel {
    MemOutBuf,
    Registers(RegisterId),
}
pub enum AluRightMuxSel<'a> {
    MemOutBuf,
    Registers(RegisterId),
    Decoder { src: &'a MachineWord },
}
pub enum AluOutDemuxSel<'a> {
    Decoder { dest: &'a mut MachineWord },
    Registers(RegisterId),
    MemAddr,
    MemInBuf,
}
#[allow(unused)] // Инверсия левого входа АЛУ не пригодилась для реализованного сабсета комманд
pub enum AluSignal {
    ResetLeft,
    ResetRight,
    InverseLeft,
    InverseRight,
    AddRightOne,
}
pub enum PortsMuxSel {
    In(PortId),
    Out(PortId),
}

impl<const MEMORY_SIZE: usize> DataPath<MEMORY_SIZE> {
    pub fn new(program: Program, ports: PortSet) -> Self {
        let mut datapath = Self { ports, ..Default::default() };
        let entrypoint = program.entrypoint();
        datapath.regs.instr = entrypoint.into();
        datapath.regs.stack = MEMORY_SIZE as u32;

        let code: Vec<MachineWord> = program.into();

        for (index, word) in code.into_iter().enumerate() {
            datapath.memory[index as u32 + 1] = word;
        }
        datapath
    }
    pub fn signal_read(&mut self, sel: ReadDemuxSel) {
        let val = self.memory[self.mem_addr.into()].clone();
        match sel {
            ReadDemuxSel::MemOutBuf => self.mem_out_buf = val,
            ReadDemuxSel::Registers(id) => self.regs[id] = val.as_number(),
            ReadDemuxSel::Decoder { dest } => *dest = val,
        }
    }
    pub fn signal_write(&mut self) {
        self.memory[self.mem_addr.into()] = self.mem_in_buf.clone();
    }
    pub fn signal_left_alu(&mut self, sel: AluLeftMuxSel) {
        self.alu.set_left(match sel {
            AluLeftMuxSel::MemOutBuf => self.mem_out_buf.as_number(),
            AluLeftMuxSel::Registers(id) => self.regs[id],
        });
    }
    pub fn signal_right_alu(&mut self, sel: AluRightMuxSel) {
        self.alu.set_right(match sel {
            AluRightMuxSel::MemOutBuf => self.mem_out_buf.as_number(),
            AluRightMuxSel::Registers(id) => self.regs[id],
            AluRightMuxSel::Decoder { src } => src.clone().as_number(),
        });
    }
    pub fn signal_alu(&mut self, signal: AluSignal) {
        match signal {
            AluSignal::ResetLeft => self.alu.reset_left(),
            AluSignal::ResetRight => self.alu.reset_right(),
            AluSignal::InverseLeft => self.alu.set_invert_left(),
            AluSignal::InverseRight => self.alu.set_invert_right(),
            AluSignal::AddRightOne => self.alu.set_add_right_one(),
        }
    }
    pub fn signal_alu_perform(&mut self, op: AluOpCode, set_flags: bool, sel: AluOutDemuxSel) {
        let res = self.alu.perform(op, if set_flags { Some(&mut self.flags) } else { None });
        match sel {
            AluOutDemuxSel::Decoder { dest } => *dest = MachineWord::Data(Immed::new(res)),
            AluOutDemuxSel::Registers(id) => self.regs[id] = res,
            AluOutDemuxSel::MemAddr => self.mem_addr = res.into(),
            AluOutDemuxSel::MemInBuf => self.mem_in_buf = MachineWord::Data(Immed::new(res)),
        }
    }
    pub fn signal_ports(&mut self, sel: PortsMuxSel) -> Result<(), MachineError> {
        match sel {
            PortsMuxSel::In(id) => {
                self.regs[RegisterId::Accumulator] = self.ports.get_device(id)?.read()? as u32
            },
            PortsMuxSel::Out(id) => {
                self.ports.get_device(id)?.write(self.regs[RegisterId::Accumulator] as u8)?
            },
        };
        Ok(())
    }
    pub fn flags(&self) -> &FlagSet {
        &self.flags
    }
}
impl<const M: usize> Display for DataPath<M> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!(
            "REGS: eax:{eax:#010x}, ebx:{ebx:#010x}, ecx:{ecx:#010x}, edx:{edx:#010x}, eip:{eip:#010x}, esp:{esp:#010x}",
            eax = self.regs.acc,
            ebx = self.regs.base,
            ecx = self.regs.count,
            edx = self.regs.data,
            eip = self.regs.instr,
            esp = self.regs.stack,
        ))
    }
}
