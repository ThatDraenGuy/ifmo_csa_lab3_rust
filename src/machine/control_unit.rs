use super::{datapath::*, MachineError};
use crate::isa::{RegisterId::*, *};
use log::debug;
use std::fmt::Display;

type CUResult = Result<ControlUnitState, MachineError>;

#[derive(Default, Debug)]
struct InstructionDecoder<const M: usize> {
    pub instruction: MachineWord,
    pub operand_buf: MachineWord,
}
impl<const M: usize> InstructionDecoder<M> {
    pub fn decode(&self) -> CUResult {
        match &self.instruction {
            MachineWord::OpHigher(op) => Ok(match op {
                OpHigher::Math(math) => match math.args {
                    MathOpHigherArgs::MemToReg(_, _)
                    | MathOpHigherArgs::RegToMem(_, _)
                    | MathOpHigherArgs::RegImmed(_, _) => {
                        ControlUnitState::FetchOperand { tick_count: 1 }
                    },
                    _ => ControlUnitState::ExecuteInstruction { tick_count: 1 },
                },
                OpHigher::Branch(_) => ControlUnitState::FetchOperand { tick_count: 1 },
                OpHigher::Alter(_) => ControlUnitState::ExecuteInstruction { tick_count: 1 },
                OpHigher::Io(_) => ControlUnitState::ExecuteInstruction { tick_count: 1 },
                OpHigher::Control(_) => ControlUnitState::ExecuteInstruction { tick_count: 1 },
            }),
            _ => Err(MachineError::InvalidInstruction(self.instruction.clone())),
        }
    }

    pub fn fetch_operand(&mut self, tick_count: u8, datapath: &mut DataPath<M>) -> CUResult {
        // 1 tick - instruction(lower) -> operand_buf(higher)
        // 2 tick - regs[eip] ADD 1 -> regs[eip]
        // 3 tick - regs[eip] ADD 0 -> mem_addr
        // 4 tick - memory[mem_addr] -> mem_out_buf
        // 5 tick - mem_out_buf STITCH operand_buf -> operand_buf
        match tick_count {
            1 => {
                let higher = self.instruction.as_number() as ImmedHigher;
                self.operand_buf = MachineWord::Data(Immed::of(higher, 0));
            },
            2 => {
                datapath.signal_left_alu(AluLeftMuxSel::Registers(InstructionPointer));
                datapath.signal_alu(AluSignal::SetRightOne);
                datapath.signal_alu_perform(
                    AluOpCode::Add,
                    false,
                    AluOutDemuxSel::Registers(InstructionPointer),
                );
            },
            3 => {
                datapath.signal_left_alu(AluLeftMuxSel::Registers(InstructionPointer));
                datapath.signal_alu(AluSignal::ResetRight);
                datapath.signal_alu_perform(AluOpCode::Add, false, AluOutDemuxSel::MemAddr);
            },
            4 => {
                datapath.signal_read(ReadDemuxSel::MemOutBuf);
            },
            5 => {
                datapath.signal_left_alu(AluLeftMuxSel::MemOutBuf);
                datapath.signal_right_alu(AluRightMuxSel::Decoder { src: &self.operand_buf });
                datapath.signal_alu_perform(
                    AluOpCode::Stc,
                    false,
                    AluOutDemuxSel::Decoder { dest: &mut self.operand_buf },
                );
                return Ok(ControlUnitState::ExecuteInstruction { tick_count: 1 });
            },
            _ => return Err(MachineError::InvalidDecoderCall),
        };
        Ok(ControlUnitState::FetchOperand { tick_count: tick_count + 1 })
    }

    pub fn execute(&mut self, tick_count: u8, datapath: &mut DataPath<M>) -> CUResult {
        match &self.instruction {
            MachineWord::OpHigher(op) => match op {
                OpHigher::Math(math) => self.execute_math(math, tick_count, datapath),
                OpHigher::Branch(branch) => self.execute_branch(branch, tick_count, datapath),
                OpHigher::Alter(alter) => self.execute_alter(alter, tick_count, datapath),
                OpHigher::Io(io) => self.execute_io(io, tick_count, datapath),
                OpHigher::Control(control) => self.execute_control(control, tick_count, datapath),
            },
            _ => Err(MachineError::InvalidInstruction(self.instruction.clone())),
        }
    }

    fn execute_math(
        &self,
        math: &MathOpHigher,
        tick_count: u8,
        datapath: &mut DataPath<M>,
    ) -> CUResult {
        let alu_opcode = match math.opcode {
            MathOp::Mov => AluOpCode::Mov,
            MathOp::Add => AluOpCode::Add,
            MathOp::Sub => AluOpCode::Sub,
            MathOp::Cmp => AluOpCode::Cmp,
        };
        match math.args {
            MathOpHigherArgs::RegToReg(dest, src) => {
                // 1 tick regs[dest] OP regs[src] -> regs[dest]
                match tick_count {
                    1 => {
                        datapath.signal_left_alu(AluLeftMuxSel::Registers(dest));
                        datapath.signal_right_alu(AluRightMuxSel::Registers(src));
                        datapath.signal_alu_perform(
                            alu_opcode,
                            true,
                            AluOutDemuxSel::Registers(dest),
                        );
                        return Ok(ControlUnitState::cycle_start());
                    },
                    _ => return Err(MachineError::InvalidDecoderCall),
                }
            },
            MathOpHigherArgs::RegToRegMem(dest, src) => {
                // 1 tick - regs[dest] -> mem_addr
                // 2 tick - memory[mem_addr] -> mem_out_buf
                // 3 tick - mem_out_buf OP src -> mem_in_buf
                // 4 tick - mem_in_buf -> memory[mem_addr]
                match tick_count {
                    1 => {
                        datapath.signal_left_alu(AluLeftMuxSel::Registers(dest));
                        datapath.signal_alu(AluSignal::ResetRight);
                        datapath.signal_alu_perform(AluOpCode::Add, false, AluOutDemuxSel::MemAddr);
                    },
                    2 => {
                        datapath.signal_read(ReadDemuxSel::MemOutBuf);
                    },
                    3 => {
                        datapath.signal_left_alu(AluLeftMuxSel::MemOutBuf);
                        datapath.signal_right_alu(AluRightMuxSel::Registers(src));
                        datapath.signal_alu_perform(alu_opcode, true, AluOutDemuxSel::MemInBuf);
                    },
                    4 => {
                        datapath.signal_write();
                        return Ok(ControlUnitState::cycle_start());
                    },
                    _ => return Err(MachineError::InvalidDecoderCall),
                }
            },
            MathOpHigherArgs::RegMemToReg(dest, src) => {
                // 1 tick - regs[src] -> mem_addr
                // 2 tick - memory[mem_addr] -> mem_out_buf
                // 3 tick - dest OP mem_out_buf -> regs[dest]
                match tick_count {
                    1 => {
                        datapath.signal_left_alu(AluLeftMuxSel::Registers(src));
                        datapath.signal_alu(AluSignal::ResetRight);
                        datapath.signal_alu_perform(AluOpCode::Add, false, AluOutDemuxSel::MemAddr);
                    },
                    2 => {
                        datapath.signal_read(ReadDemuxSel::MemOutBuf);
                    },
                    3 => {
                        datapath.signal_left_alu(AluLeftMuxSel::Registers(dest));
                        datapath.signal_right_alu(AluRightMuxSel::MemOutBuf);
                        datapath.signal_alu_perform(
                            alu_opcode,
                            true,
                            AluOutDemuxSel::Registers(dest),
                        );
                        return Ok(ControlUnitState::cycle_start());
                    },
                    _ => return Err(MachineError::InvalidDecoderCall),
                }
            },
            MathOpHigherArgs::MemToReg(dest, _) => {
                // 1 tick - operand_buf -> mem_addr
                // 2 tick - memory[mem_addr] -> mem_out_buf
                // 3 tick - dest OP mem_out_buf -> regs[dest]
                match tick_count {
                    1 => {
                        datapath.signal_alu(AluSignal::ResetLeft);
                        datapath
                            .signal_right_alu(AluRightMuxSel::Decoder { src: &self.operand_buf });
                        datapath.signal_alu_perform(AluOpCode::Add, false, AluOutDemuxSel::MemAddr);
                    },
                    2 => {
                        datapath.signal_read(ReadDemuxSel::MemOutBuf);
                    },
                    3 => {
                        datapath.signal_left_alu(AluLeftMuxSel::Registers(dest));
                        datapath.signal_right_alu(AluRightMuxSel::MemOutBuf);
                        datapath.signal_alu_perform(
                            alu_opcode,
                            true,
                            AluOutDemuxSel::Registers(dest),
                        );
                        return Ok(ControlUnitState::cycle_start());
                    },
                    _ => return Err(MachineError::InvalidDecoderCall),
                }
            },
            MathOpHigherArgs::RegToMem(src, _) => {
                // 1 tick - operand_buf -> mem_addr
                // 2 tick - memory[mem_addr] -> mem_out_buf
                // 3 tick - mem_out_buf OP regs[src] -> mem_in_buf
                // 4 tick - mem_in_buf -> memory[mem_addr]
                match tick_count {
                    1 => {
                        datapath.signal_alu(AluSignal::ResetLeft);
                        datapath
                            .signal_right_alu(AluRightMuxSel::Decoder { src: &self.operand_buf });
                        datapath.signal_alu_perform(AluOpCode::Add, false, AluOutDemuxSel::MemAddr);
                    },
                    2 => {
                        datapath.signal_read(ReadDemuxSel::MemOutBuf);
                    },
                    3 => {
                        datapath.signal_left_alu(AluLeftMuxSel::MemOutBuf);
                        datapath.signal_right_alu(AluRightMuxSel::Registers(src));
                        datapath.signal_alu_perform(alu_opcode, true, AluOutDemuxSel::MemInBuf);
                    },
                    4 => {
                        datapath.signal_write();
                        return Ok(ControlUnitState::cycle_start());
                    },
                    _ => return Err(MachineError::InvalidDecoderCall),
                }
            },
            MathOpHigherArgs::RegImmed(dest, _) => {
                // 1 tick regs[dest] OP operand_buf -> regs[dest]
                match tick_count {
                    1 => {
                        datapath.signal_left_alu(AluLeftMuxSel::Registers(dest));
                        datapath
                            .signal_right_alu(AluRightMuxSel::Decoder { src: &self.operand_buf });
                        datapath.signal_alu_perform(
                            alu_opcode,
                            true,
                            AluOutDemuxSel::Registers(dest),
                        );
                        return Ok(ControlUnitState::cycle_start());
                    },
                    _ => return Err(MachineError::InvalidDecoderCall),
                }
            },
        };
        Ok(ControlUnitState::ExecuteInstruction { tick_count: tick_count + 1 })
    }

    fn execute_branch(
        &self,
        branch: &BranchOpHigher,
        tick_count: u8,
        datapath: &mut DataPath<M>,
    ) -> CUResult {
        match tick_count {
            1 => {
                let do_jump = match branch.opcode {
                    BranchOp::Jmp => true,
                    BranchOp::Jz => datapath.flags().zero,
                    BranchOp::Jnz => !datapath.flags().zero,
                    BranchOp::Js => datapath.flags().sign,
                    BranchOp::Jns => !datapath.flags().sign,
                };

                if do_jump {
                    datapath.signal_alu(AluSignal::ResetLeft);
                    datapath.signal_right_alu(AluRightMuxSel::Decoder { src: &self.operand_buf });
                    datapath.signal_alu_perform(
                        AluOpCode::Mov,
                        false,
                        AluOutDemuxSel::Registers(InstructionPointer),
                    );
                    Ok(ControlUnitState::default())
                } else {
                    Ok(ControlUnitState::cycle_start())
                }
            },
            _ => Err(MachineError::InvalidDecoderCall),
        }
    }

    fn execute_alter(
        &self,
        alter: &AlterOpWord,
        tick_count: u8,
        datapath: &mut DataPath<M>,
    ) -> CUResult {
        match tick_count {
            1 => {
                datapath.signal_left_alu(AluLeftMuxSel::Registers(alter.arg));
                match alter.opcode {
                    AlterOp::Inc => datapath.signal_alu(AluSignal::SetRightOne),
                    AlterOp::Dec => {
                        datapath.signal_alu(AluSignal::ResetRight);
                        datapath.signal_alu(AluSignal::InverseRight);
                    },
                }
                datapath.signal_alu_perform(
                    AluOpCode::Add,
                    true,
                    AluOutDemuxSel::Registers(alter.arg),
                );
                Ok(ControlUnitState::cycle_start())
            },
            _ => Err(MachineError::InvalidDecoderCall),
        }
    }

    fn execute_io(&self, io: &IoOpWord, tick_count: u8, datapath: &mut DataPath<M>) -> CUResult {
        match tick_count {
            1 => {
                let port_id = io.arg;
                match io.opcode {
                    IoOp::In => datapath.signal_ports(PortsMuxSel::In(port_id))?,
                    IoOp::Out => datapath.signal_ports(PortsMuxSel::Out(port_id))?,
                };
                Ok(ControlUnitState::cycle_start())
            },
            _ => Err(MachineError::InvalidDecoderCall),
        }
    }

    fn execute_control(
        &self,
        control: &ControlOpWord,
        tick_count: u8,
        _: &mut DataPath<M>,
    ) -> CUResult {
        match control.0 {
            ControlOp::Exit => match tick_count {
                1 => Err(MachineError::Exit),
                _ => Err(MachineError::InvalidDecoderCall),
            },
        }
    }
}

#[derive(Default, Debug)]
enum ControlUnitState {
    IncIP,
    #[default]
    MovIPToMemAddr,
    FetchInstruction,
    DecodeInstruction,
    FetchOperand {
        tick_count: u8,
    },
    ExecuteInstruction {
        tick_count: u8,
    },
}
impl ControlUnitState {
    pub fn cycle_start() -> Self {
        Self::IncIP
    }
}

/// ```ignore
///                                                                          ┌───────────┐
///                                                                          │ microcode │
///                                                                          │ decoder   │
///                                                                          └───┬──────┬┘
///                                                                              │      │
///                                                                              │      │
///                                                                              │      │
///                                                                              │      │
/// ┌───────────────────────────────────────────────────────────────────┐        │      │
/// │                            Decoder                                │        │      │
/// │                                                                   │        │      │
/// │                                                                   │        │      │
/// │                                                                   │        │      │
/// │                                                                   │Signals │      │
/// │                        *чёрная магия*                             ◄────────┘      │
/// │                                                                   │               │
/// │                                                                   │               │
/// │                           operand pipe                            │               │
/// │                        ┌─────────────────┐                        │               │
/// │                        │                 │                        │               │
/// │        ┌───────────────┴──┐            ┌─▼────────────┐           │               │
/// │        │Instruction buffer│            │Operand buffer│           │               │
/// │        └─▲────────────────┘            └─▲──────────┬─┘           │               │
/// │          │                               │          │             │               │
/// │          │                               │          │             │               │
/// │          │                               │          │             ├───────────────┤
/// │          │                               │          │             │               │
/// │          │  ┌────────────────────────────┘          │             │               │
/// │          │  │                                       │             │               │
/// │         ┌┴──┴─┐                                     │             │               │
/// │         │DEMUX│                                     │             │ flags         │
/// │         └─▲───┘                                     │             ◄─────────┐     │
/// │           │                                         │             │         │     │
/// └───────────┼─────────────────────────────────────────┼─────────────┘         │     │
///             │                                         │                       │     │
///             │                                         │                       │     │
///             │                                         │                       │     │
///             │                                         │                       │     │
///             │                                         │                       │     │
///             │                                         │                       │     │
///             │from ALU DEMUX                           ▼to right ALU MUX       │     │
/// ┌───────────┴───────────────────────────────────────────────────────┐         │     │
/// │                              Datapath                             │         │     │
/// │                                                                   ├─────────┘     │
/// │                                                                   │               │
/// │                                                                   │               │
/// │                                                                   ◄───────────────┘
/// │                                                                   │ Signals
/// │                                                                   │
/// │                                                                   │
/// └───────────────────────────────────────────────────────────────────┘
/// ```
#[derive(Default, Debug)]
pub struct ControlUnit<const MEM_SIZE: usize, const TICK_LIMIT: usize> {
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
        self.tick += 1;
        if self.tick > TICK_LIMIT {
            return Err(MachineError::TickLimitReached(TICK_LIMIT));
        }
        self.state = match self.state {
            ControlUnitState::IncIP => self.inc_eip(),
            ControlUnitState::MovIPToMemAddr => self.eip_to_mem_addr(),
            ControlUnitState::FetchInstruction => self.fetch_instruction(),
            ControlUnitState::DecodeInstruction => self.decoder.decode(),
            ControlUnitState::FetchOperand { tick_count } => {
                self.decoder.fetch_operand(tick_count, &mut self.datapath)
            },
            ControlUnitState::ExecuteInstruction { tick_count } => {
                if tick_count == 1 {
                    debug!("{:#}", self);
                }
                self.decoder.execute(tick_count, &mut self.datapath)
            },
        }?;
        Ok(())
    }

    fn inc_eip(&mut self) -> CUResult {
        self.datapath.signal_left_alu(AluLeftMuxSel::Registers(InstructionPointer));
        self.datapath.signal_alu(AluSignal::SetRightOne);
        self.datapath.signal_alu_perform(
            AluOpCode::Add,
            false,
            AluOutDemuxSel::Registers(InstructionPointer),
        );
        Ok(ControlUnitState::MovIPToMemAddr)
    }

    fn eip_to_mem_addr(&mut self) -> CUResult {
        self.datapath.signal_left_alu(AluLeftMuxSel::Registers(InstructionPointer));
        self.datapath.signal_alu(AluSignal::ResetRight);
        self.datapath.signal_alu_perform(AluOpCode::Add, false, AluOutDemuxSel::MemAddr);
        Ok(ControlUnitState::FetchInstruction)
    }

    fn fetch_instruction(&mut self) -> CUResult {
        self.datapath.signal_read(ReadDemuxSel::Decoder { dest: &mut self.decoder.instruction });
        Ok(ControlUnitState::DecodeInstruction)
    }
}
impl<const MEM_SIZE: usize, const TICK_LIMIT: usize> Display for ControlUnit<MEM_SIZE, TICK_LIMIT> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!(
            "TICK: {tick:4}; {datapath}; INSTR: {instr:?}",
            tick = self.tick,
            datapath = self.datapath,
            instr = self.decoder.instruction,
        ))
    }
}
